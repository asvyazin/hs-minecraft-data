{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Item where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Item = Item
  { itemId :: Int,
    itemName :: Text,
    itemDisplayName :: Text,
    itemStackSize :: Int
  }
  deriving (Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v ->
    Item
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "stackSize"