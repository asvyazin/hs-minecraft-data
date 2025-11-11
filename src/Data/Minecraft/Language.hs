module Data.Minecraft.Language where

import Data.Aeson (FromJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

newtype Language = Language { languageMap :: HashMap.HashMap Text Text }
  deriving (Show, FromJSON)