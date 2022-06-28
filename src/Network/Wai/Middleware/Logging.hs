module Network.Wai.Middleware.Logging
  ( addThreadContext
  , addThreadContextFromRequest
  , requestLogger
  , requestLoggerWith

  -- * Configuration
  , Config
  , defaultConfig
  , setConfigLogSource
  , setConfigGetDestinationIp
  ) where

import Prelude

import Blammo.Logging
import Control.Arrow ((***))
import Control.Monad.IO.Unlift (withRunInIO)
import Data.Aeson
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.HTTP.Types.Status (Status(..))
import Network.Wai
  ( Middleware
  , Request
  , Response
  , rawPathInfo
  , rawQueryString
  , remoteHost
  , requestHeaders
  , requestMethod
  , responseHeaders
  , responseStatus
  )
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
--
requestLogger :: HasLogger env => env -> Middleware
requestLogger = requestLoggerWith defaultConfig

data Config = Config
  { cLogSource :: LogSource
  , cGetDestinationIp :: Request -> Maybe Text
  }

defaultConfig :: Config
defaultConfig = Config
  { cLogSource = "requestLogger"
  , cGetDestinationIp = fmap decodeUtf8 . lookupRequestHeader "x-real-ip"
  }

lookupRequestHeader :: HeaderName -> Request -> Maybe ByteString
lookupRequestHeader h = lookup h . requestHeaders

-- | Change the source used for log messages
--
-- Default is @requestLogger@.
--
setConfigLogSource :: LogSource -> Config -> Config
setConfigLogSource x c = c { cLogSource = x }

-- | Change how the @destinationIp@ field is determined
--
-- Default is looking up the @x-real-ip@ header.
--
setConfigGetDestinationIp :: (Request -> Maybe Text) -> Config -> Config
setConfigGetDestinationIp x c = c { cGetDestinationIp = x }

requestLoggerWith :: HasLogger env => Config -> env -> Middleware
requestLoggerWith config env app req respond =
  runLoggerLoggingT env $ withRunInIO $ \runInIO -> do
    begin <- getTime
    app req $ \resp -> do
      recvd <- respond resp
      duration <- toMillis . subtract begin <$> getTime
      recvd <$ runInIO (logResponse config duration req resp)
 where
  getTime = Clock.getTime Clock.Monotonic

  toMillis x = fromIntegral (Clock.toNanoSecs x) / nsPerMs

logResponse :: MonadLogger m => Config -> Double -> Request -> Response -> m ()
logResponse Config {..} duration req resp
  | statusCode status >= 500 = logErrorNS cLogSource $ message :# details
  | statusCode status == 404 = logDebugNS cLogSource $ message :# details
  | statusCode status >= 400 = logWarnNS cLogSource $ message :# details
  | otherwise = logDebugNS cLogSource $ message :# details
 where
  message =
    decodeUtf8 (requestMethod req)
      <> " "
      <> decodeUtf8 (rawPathInfo req)
      <> " => "
      <> pack (show $ statusCode status)
      <> " "
      <> decodeUtf8 (statusMessage status)

  details =
    [ "method" .= decodeUtf8 (requestMethod req)
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "query" .= decodeUtf8 (rawQueryString req)
    , "status" .= object
      [ "code" .= statusCode status
      , "message" .= decodeUtf8 (statusMessage status)
      ]
    , "clientIp" .= show (remoteHost req)
    , "destinationIp" .= cGetDestinationIp req
    , "durationMs" .= duration
    , "requestHeaders"
      .= headerObject ["authorization", "cookie"] (requestHeaders req)
    , "responseHeaders" .= headerObject ["set-cookie"] (responseHeaders resp)
    ]

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
