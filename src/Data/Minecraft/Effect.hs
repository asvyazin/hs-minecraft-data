{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Effect where

import Data.Aeson (FromJSON (parseJSON), Value (String), withObject, withText, (.:))
import Data.Text (Text)

data EffectType
  = EffectTypeGood
  | EffectTypeBad
  deriving (Show)

instance FromJSON EffectType where
  parseJSON = withText "EffectType" $ \t ->
    case t of
      "good" -> pure EffectTypeGood
      "bad" -> pure EffectTypeBad
      _ -> fail $ "Unknown EffectType: " ++ show t

data Effect = Effect
  { effectId :: Int,
    effectName :: Text,
    effectDisplayName :: Text,
    effectType :: EffectType
  }
  deriving (Show)

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \v ->
    Effect
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "type"