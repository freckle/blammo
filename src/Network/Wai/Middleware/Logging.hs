module Network.Wai.Middleware.Logging
  ( addThreadContext
  , requestLogger
  ) where

import Prelude

import Blammo.Logging
import Control.Arrow ((***))
import Control.Monad.IO.Unlift (withRunInIO)
import Data.Aeson
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import qualified Data.CaseInsensitive as CI
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.HTTP.Types.Status (Status(..))
import Network.Wai
  ( Middleware
  , Request
  , Response
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , requestMethod
  , responseHeaders
  , responseStatus
  )
import qualified System.Clock as Clock

-- | Add context to any logging done from the request-handling thread
addThreadContext :: [Pair] -> Middleware
addThreadContext context app request respond = do
  withThreadContext context $ do
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
requestLogger env app req respond =
  runLoggerLoggingT env $ withRunInIO $ \runInIO -> do
    begin <- getTime
    app req $ \resp -> do
      recvd <- respond resp
      duration <- toMillis . subtract begin <$> getTime
      recvd <$ runInIO (logResponse duration req resp)
 where
  getTime = Clock.getTime Clock.Monotonic

  toMillis x = fromIntegral (Clock.toNanoSecs x) / nsPerMs

logResponse :: MonadLogger m => Double -> Request -> Response -> m ()
logResponse duration req resp
  | statusCode status >= 500 = logError $ message :# details
  | statusCode status == 404 = logDebug $ message :# details
  | statusCode status >= 400 = logWarn $ message :# details
  | otherwise = logDebug $ message :# details
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
