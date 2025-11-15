{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Version where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Version = Version
  { version :: Int,
    minecraftVersion :: Text,
    majorVersion :: Text,
    releaseType :: Text
  }
  deriving (Show)

instance FromJSON Version where
  parseJSON = withObject "Version" $ \v ->
    Version
      <$> v .: "version"
      <*> v .: "minecraftVersion"
      <*> v .: "majorVersion"
      <*> v .: "releaseType"