module Blammo.Logging.Terminal.LogPiece
  ( LogPiece
  , logPiece
  , render
  , visibleLength
  , bytestring

  -- * Built-in pieces
  , offset
  ) where

import Prelude

import Data.ByteString (ByteString)
import Data.Semigroup (Sum(..))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

data LogPiece = LogPiece
  { lpRendered :: Text
  , lpVisibleLength :: Sum Int
  }
  -- TODO: When we drop support for ghc-8.6:
  -- deriving stock Generic
  -- deriving (Semigroup, Monoid) via (GenericSemigroupMonoid LogPiece)

instance Semigroup LogPiece where
  a <> b = LogPiece
    { lpRendered = lpRendered a <> lpRendered b
    , lpVisibleLength = lpVisibleLength a <> lpVisibleLength b
    }

instance Monoid LogPiece where
  mempty = LogPiece mempty mempty

instance IsString LogPiece where
  fromString = logPiece id . pack

logPiece
  :: (Text -> Text)
  -- ^ Non-visible decoration, such as color escapes
  -> Text
  -- ^ Raw
  -> LogPiece
logPiece f t =
  LogPiece { lpRendered = f t, lpVisibleLength = Sum $ T.length t }

render :: LogPiece -> Text
render = lpRendered

bytestring :: LogPiece -> ByteString
bytestring = encodeUtf8 . render

visibleLength :: LogPiece -> Int
visibleLength = getSum . lpVisibleLength

offset :: Int -> LogPiece
offset n = LogPiece { lpRendered = T.replicate n " ", lpVisibleLength = Sum n }
