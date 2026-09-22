module Blammo.Logging.Terminal.Doc
  ( Ann (..)
  , annToAnsi

    -- * Rendering
  , RenderSettings (..)
  , renderDoc

    -- * "Prettyprinter" re-exports
  , Doc

    -- * "Prettyprinter.Render.Terminal" re-exports
  , AnsiStyle
  , Color (..)
  , color
  , colorDull
  , bold
  , italicized
  , underlined
  ) where

import Prelude

import Control.Monad.Logger.Aeson (LogLevel (..))
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Terminal hiding (renderStrict)
import Prettyprinter.Render.Terminal as Terminal
import Prettyprinter.Render.Text as Text

data Ann
  = AnnTimestamp
  | AnnByLevel LogLevel
  | AnnKey
  | AnnValue
  | AnnString
  | AnnNumber
  | AnnBoolean
  | AnnNull
  | AnnPunctuation
  | -- | backdoor for external use-cases
    AnnAnsi AnsiStyle

annToAnsi :: Ann -> AnsiStyle
annToAnsi = \case
  AnnTimestamp -> colorDull White -- TODO: faint, once supported
  AnnByLevel l -> case l of
    LevelDebug -> colorDull White -- TODO: faint, once supported
    LevelInfo -> colorDull Green
    LevelWarn -> colorDull Yellow
    LevelError -> colorDull Red
    LevelOther _ -> colorDull Blue
  AnnKey -> colorDull Cyan
  AnnValue -> colorDull Magenta
  AnnString -> colorDull Green
  AnnNumber -> colorDull Green
  AnnBoolean -> colorDull Red
  AnnNull -> colorDull Black
  AnnPunctuation -> colorDull Black
  AnnAnsi style -> style

data RenderSettings = RenderSettings
  { rsUseColor :: Bool
  , rsPageWidth :: Int
  , rsAnnToAnsi :: Ann -> AnsiStyle
  }

renderDoc :: RenderSettings -> Doc Ann -> Text
renderDoc RenderSettings {..} =
  render
    . layoutPretty layoutOptions
    . reAnnotate rsAnnToAnsi
 where
  render
    | rsUseColor = Terminal.renderStrict
    | otherwise = Text.renderStrict

  layoutOptions =
    defaultLayoutOptions
      { layoutPageWidth = AvailablePerLine rsPageWidth 1.0
      }
