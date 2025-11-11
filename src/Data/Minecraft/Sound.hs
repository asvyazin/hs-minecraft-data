{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Sound where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Sound = Sound
  { soundId :: Int,
    soundName :: Text
  }
  deriving (Show)

instance FromJSON Sound where
  parseJSON = withObject "Sound" $ \v ->
    Sound
      <$> v .: "id"
      <*> v .: "name"