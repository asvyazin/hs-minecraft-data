{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Biome where

import Data.Aeson (FromJSON (parseJSON), Value (String), withObject, withText, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

data BiomeCategory
  = BiomeCategoryMesa
  | BiomeCategoryJungle
  | BiomeCategoryNether
  | BiomeCategoryBeach
  | BiomeCategoryForest
  | BiomeCategoryOcean
  | BiomeCategoryUnderground
  | BiomeCategoryDesert
  | BiomeCategoryTheEnd
  | BiomeCategoryIce
  | BiomeCategoryMountain
  | BiomeCategoryMushroom
  | BiomeCategoryTaiga
  | BiomeCategoryPlains
  | BiomeCategoryRiver
  | BiomeCategorySavanna
  | BiomeCategorySwamp
  | BiomeCategoryExtremeHills
  deriving (Show)

instance FromJSON BiomeCategory where
  parseJSON = withText "BiomeCategory" $ \t ->
    case t of
      "mesa" -> pure BiomeCategoryMesa
      "jungle" -> pure BiomeCategoryJungle
      "nether" -> pure BiomeCategoryNether
      "beach" -> pure BiomeCategoryBeach
      "forest" -> pure BiomeCategoryForest
      "ocean" -> pure BiomeCategoryOcean
      "underground" -> pure BiomeCategoryUnderground
      "desert" -> pure BiomeCategoryDesert
      "the_end" -> pure BiomeCategoryTheEnd
      "ice" -> pure BiomeCategoryIce
      "mountain" -> pure BiomeCategoryMountain
      "mushroom" -> pure BiomeCategoryMushroom
      "taiga" -> pure BiomeCategoryTaiga
      "plains" -> pure BiomeCategoryPlains
      "river" -> pure BiomeCategoryRiver
      "savanna" -> pure BiomeCategorySavanna
      "swamp" -> pure BiomeCategorySwamp
      "extreme_hills" -> pure BiomeCategoryExtremeHills
      _ -> fail $ "Unknown BiomeCategory: " ++ show t

data Dimension
  = DimensionOverworld
  | DimensionNether
  | DimensionEnd
  deriving (Show)

instance FromJSON Dimension where
  parseJSON = withText "Dimension" $ \t ->
    case t of
      "overworld" -> pure DimensionOverworld
      "nether" -> pure DimensionNether
      "end" -> pure DimensionEnd
      _ -> fail $ "Unknown Dimension: " ++ show t

data Biome = Biome
  { biomeId :: Int,
    biomeName :: Text,
    biomeCategory :: Maybe BiomeCategory,
    biomeTemperature :: Float,
    biomeHasPrecipitation :: Bool,
    biomeDimension :: Dimension,
    biomeDisplayName :: Text,
    biomeColor :: Int
  }
  deriving (Show)

noneOrJust :: (FromJSON a) => Text -> Parser Value -> Parser (Maybe a)
noneOrJust name parser = do
  value <- parser
  case value of
    String s ->
      if s == name
        then pure Nothing
        else Just <$> parseJSON value
    _ -> fail $ "Expected String value for field, but got: " ++ show value

instance FromJSON Biome where
  parseJSON = withObject "Biome" $ \v ->
    Biome
      <$> v .: "id"
      <*> v .: "name"
      <*> noneOrJust "none" (v .: "category")
      <*> v .: "temperature"
      <*> v .: "has_precipitation"
      <*> v .: "dimension"
      <*> v .: "displayName"
      <*> v .: "color"