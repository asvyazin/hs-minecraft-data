{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Attribute where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Attribute = Attribute
  { attrName :: Text,
    attrResource :: Text,
    attrMin :: Float,
    attrMax :: Float,
    attrDefault :: Float
  }
  deriving (Show)

instance FromJSON Attribute where
  parseJSON = withObject "Attribute" $ \v ->
    Attribute
      <$> v .: "name"
      <*> v .: "resource"
      <*> v .: "min"
      <*> v .: "max"
      <*> v .: "default"