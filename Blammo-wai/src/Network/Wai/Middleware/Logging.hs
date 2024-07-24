module Network.Wai.Middleware.Logging
  ( addThreadContext
  , addThreadContextFromRequest
  , requestLogger
  , requestLoggerWith

    -- * Configuration
  , Config
  , defaultConfig
  , setConfigLogSource
  , setConfigGetClientIp
  , setConfigGetDestinationIp
  ) where

import Prelude

import Blammo.Logging
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger.Aeson (SeriesElem)
import Data.Aeson
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.HTTP.Types.Status (Status (..))
import Network.Wai
  ( Middleware
  , Request
  , rawPathInfo
  , rawQueryString
  , remoteHost
  , requestHeaders
  , requestMethod
  , responseHeaders
  , responseStatus
  )
import Network.Wai.Internal (Response (..))
import qualified System.Clock as Clock

-- | Add context to any logging done from the request-handling thread
addThreadContext :: [Pair] -> Middleware
addThreadContext = addThreadContextFromRequest . const

-- | 'addThreadContext', but have the 'Request' available
addThreadContextFromRequest :: (Request -> [Pair]) -> Middleware
addThreadContextFromRequest toContext app request respond = do
  withThreadContext (toContext request) $ do
    app request respond

-- | Log requests (more accurately, responses) as they happen
--
-- In JSON format, logged messages look like:
--
-- @
-- {
--   ...
--   message: {
--     text: "GET /foo/bar => 200 OK",
--     meta: {
--       method: "GET",
--       path: "/foo/bar",
--       query: "?baz=bat&quix=quo",
--       status: {
--         code: 200,
--         message: "OK"
--       },
--       durationMs: 1322.2,
--       requestHeaders: {
--         Authorization: "***",
--         Accept: "text/html",
--         Cookie: "***"
--       },
--       responseHeaders: {
--         Set-Cookie: "***",
--         Expires: "never"
--       }
--     }
--   }
-- }
-- @
requestLogger :: HasLogger env => env -> Middleware
requestLogger = requestLoggerWith defaultConfig

data Config = Config
  { cLogSource :: LogSource
  , cGetClientIp :: Request -> Text
  , cGetDestinationIp :: Request -> Maybe Text
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cLogSource = "requestLogger"
    , cGetClientIp = \req ->
        fromMaybe (pack $ show $ remoteHost req) $
          (firstValue =<< lookupRequestHeader "x-forwarded-for" req)
            <|> lookupRequestHeader "x-real-ip" req
    , cGetDestinationIp = lookupRequestHeader "x-real-ip"
    }
 where
  firstValue = find (not . T.null) . map T.strip . T.splitOn ","

lookupRequestHeader :: HeaderName -> Request -> Maybe Text
lookupRequestHeader h = fmap decodeUtf8 . lookup h . requestHeaders

-- | Change the source used for log messages
--
-- Default is @requestLogger@.
setConfigLogSource :: LogSource -> Config -> Config
setConfigLogSource x c = c {cLogSource = x}

-- | Change how the @clientIp@ field is determined
--
-- Default is looking up the first value in @x-forwarded-for@, then the
-- @x-real-ip@ header, then finally falling back to 'Network.Wai.remoteHost'.
setConfigGetClientIp :: (Request -> Text) -> Config -> Config
setConfigGetClientIp x c = c {cGetClientIp = x}

-- | Change how the @destinationIp@ field is determined
--
-- Default is looking up the @x-real-ip@ header.
--
-- __NOTE__: Our default uses a somewhat loose definition of /destination/. It
-- would be more accurate to report the resolved IP address of the @Host@
-- header, but we don't have that available. Our default of @x-real-ip@ favors
-- containerized Warp on AWS/ECS, where this value holds the ECS target
-- container's IP address. This is valuable debugging information and could, if
-- you squint, be considered a /destination/.
setConfigGetDestinationIp :: (Request -> Maybe Text) -> Config -> Config
setConfigGetDestinationIp x c = c {cGetDestinationIp = x}

requestLoggerWith :: HasLogger env => Config -> env -> Middleware
requestLoggerWith config env app req respond =
  withRunInIO $ \runInIO -> do
    begin <- getTime
    app req $ \resp -> do
      recvd <- respond resp
      duration <- toMillis . subtract begin <$> getTime
      runInIO $
        runWithLogger env $
          if isRaw resp
            then logRawResponse config duration req
            else logResponse config duration req resp
      pure recvd
 where
  getTime = Clock.getTime Clock.Monotonic
  toMillis x = fromIntegral (Clock.toNanoSecs x) / nsPerMs
  isRaw = \case
    ResponseRaw {} -> True
    _ -> False

logRawResponse :: MonadLogger m => Config -> Double -> Request -> m ()
logRawResponse config@Config {..} duration req =
  logDebugNS cLogSource $ message :# details
 where
  message = requestMessage req "<raw response>"
  details = requestDetails config req <> ["durationMs" .= duration]

logResponse :: MonadLogger m => Config -> Double -> Request -> Response -> m ()
logResponse config@Config {..} duration req resp
  | statusCode status >= 500 = logErrorNS cLogSource $ message :# details
  | statusCode status == 404 = logDebugNS cLogSource $ message :# details
  | statusCode status >= 400 = logWarnNS cLogSource $ message :# details
  | otherwise = logDebugNS cLogSource $ message :# details
 where
  message = requestMessage req $ decodeUtf8 (statusMessage status)
  details =
    requestDetails config req
      <> responseDetails resp
      <> ["durationMs" .= duration]

  status = responseStatus resp

requestMessage :: Request -> Text -> Text
requestMessage req suffix =
  decodeUtf8 (requestMethod req)
    <> " "
    <> decodeUtf8 (rawPathInfo req)
    <> " => "
    <> suffix

requestDetails :: Config -> Request -> [SeriesElem]
requestDetails Config {..} req =
  [ "method" .= decodeUtf8 (requestMethod req)
  , "path" .= decodeUtf8 (rawPathInfo req)
  , "query" .= decodeUtf8 (rawQueryString req)
  , "clientIp" .= cGetClientIp req
  , "destinationIp" .= cGetDestinationIp req
  , "requestHeaders"
      .= headerObject ["authorization", "cookie"] (requestHeaders req)
  ]

responseDetails :: Response -> [SeriesElem]
responseDetails resp =
  [ "status"
      .= object
        [ "code" .= statusCode status
        , "message" .= decodeUtf8 (statusMessage status)
        ]
  , "responseHeaders" .= headerObject ["set-cookie"] (responseHeaders resp)
  ]
 where
  status = responseStatus resp

headerObject :: [HeaderName] -> [Header] -> Value
headerObject redact = Object . KeyMap.fromList . map (mung . hide)
 where
  mung = Key.fromText . decodeUtf8 . CI.foldedCase *** String . decodeUtf8
  hide (k, v)
    | k `elem` redact = (k, "***")
    | otherwise = (k, v)

nsPerMs :: Double
nsPerMs = 1000000

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
