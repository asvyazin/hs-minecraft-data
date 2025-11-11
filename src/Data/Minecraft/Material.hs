module Data.Minecraft.Material where

import Data.Aeson (FromJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

-- | A material represents how different tools interact with blocks
-- Each material has a name and a mapping of tool IDs to mining speeds

newtype Materials = Materials { materialsToolsSpeedMap :: HashMap.HashMap Text (HashMap.HashMap Text Float) }
  deriving (Show, FromJSON)
