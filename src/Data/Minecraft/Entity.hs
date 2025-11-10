{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Entity where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.Text (Text)

data EntityType
  = EntityTypeMob
  | EntityTypeAnimal
  | EntityTypeHostile
  | EntityTypeLiving
  | EntityTypeOther
  | EntityTypeProjectile
  | EntityTypeWaterCreature
  | EntityTypeAmbient
  | EntityTypePlayer
  | EntityTypePassive
  deriving (Show, Eq)

instance FromJSON EntityType where
  parseJSON = withText "EntityType" $ \typeStr ->
    case typeStr of
      "mob" -> pure EntityTypeMob
      "animal" -> pure EntityTypeAnimal
      "hostile" -> pure EntityTypeHostile
      "living" -> pure EntityTypeLiving
      "other" -> pure EntityTypeOther
      "projectile" -> pure EntityTypeProjectile
      "water_creature" -> pure EntityTypeWaterCreature
      "ambient" -> pure EntityTypeAmbient
      "player" -> pure EntityTypePlayer
      "passive" -> pure EntityTypePassive
      _ -> fail $ "Unknown EntityType: " ++ show typeStr

data Entity = Entity
  { entityId :: Int,
    entityInternalId :: Int,
    entityName :: Text,
    entityDisplayName :: Text,
    entityWidth :: Float,
    entityHeight :: Float,
    entityType :: EntityType,
    entityCategory :: Text,
    entityMetadataKeys :: [Text]
  }
  deriving (Show)

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v ->
    Entity
      <$> v .: "id"
      <*> v .: "internalId"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "width"
      <*> v .: "height"
      <*> v .: "type"
      <*> v .: "category"
      <*> v .: "metadataKeys"
