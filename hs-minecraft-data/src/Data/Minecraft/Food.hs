{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Food where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Food = Food
  { foodId :: Int,
    foodName :: Text,
    foodStackSize :: Int,
    foodDisplayName :: Text,
    foodFoodPoints :: Float,
    foodSaturation :: Float,
    foodEffectiveQuality :: Float,
    foodSaturationRatio :: Float
  }
  deriving (Show)

instance FromJSON Food where
  parseJSON = withObject "Food" $ \v ->
    Food
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "stackSize"
      <*> v .: "displayName"
      <*> v .: "foodPoints"
      <*> v .: "saturation"
      <*> v .: "effectiveQuality"
      <*> v .: "saturationRatio"