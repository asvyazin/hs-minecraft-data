{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Enchantment where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.Text (Text)

data EnchantmentCategory
  = EnchantmentCategoryHeadArmor
  | EnchantmentCategoryWeapon
  | EnchantmentCategoryEquippable
  | EnchantmentCategoryArmor
  | EnchantmentCategoryFootArmor
  | EnchantmentCategoryMining
  | EnchantmentCategoryFireAspect
  | EnchantmentCategoryMiningLoot
  | EnchantmentCategoryTrident
  | EnchantmentCategoryMace
  | EnchantmentCategoryBow
  | EnchantmentCategorySword
  | EnchantmentCategoryFishing
  | EnchantmentCategoryDurability
  | EnchantmentCategoryCrossbow
  | EnchantmentCategorySharpWeapon
  | EnchantmentCategoryLegArmor
  | EnchantmentCategoryVanishing
  deriving (Show, Eq, Ord)

instance FromJSON EnchantmentCategory where
  parseJSON = withText "EnchantmentCategory" $ \t ->
    case t of
      "head_armor" -> pure EnchantmentCategoryHeadArmor
      "weapon" -> pure EnchantmentCategoryWeapon
      "equippable" -> pure EnchantmentCategoryEquippable
      "armor" -> pure EnchantmentCategoryArmor
      "foot_armor" -> pure EnchantmentCategoryFootArmor
      "mining" -> pure EnchantmentCategoryMining
      "fire_aspect" -> pure EnchantmentCategoryFireAspect
      "mining_loot" -> pure EnchantmentCategoryMiningLoot
      "trident" -> pure EnchantmentCategoryTrident
      "mace" -> pure EnchantmentCategoryMace
      "bow" -> pure EnchantmentCategoryBow
      "sword" -> pure EnchantmentCategorySword
      "fishing" -> pure EnchantmentCategoryFishing
      "durability" -> pure EnchantmentCategoryDurability
      "crossbow" -> pure EnchantmentCategoryCrossbow
      "sharp_weapon" -> pure EnchantmentCategorySharpWeapon
      "leg_armor" -> pure EnchantmentCategoryLegArmor
      "vanishing" -> pure EnchantmentCategoryVanishing
      _ -> fail $ "Unknown EnchantmentCategory: " ++ show t

data CostFormula = CostFormula
  { costA :: Int,
    costB :: Int
  }
  deriving (Show)

instance FromJSON CostFormula where
  parseJSON = withObject "CostFormula" $ \v ->
    CostFormula
      <$> v .: "a"
      <*> v .: "b"

data Enchantment = Enchantment
  { enchantmentId :: Int,
    enchantmentName :: Text,
    enchantmentDisplayName :: Text,
    enchantmentMaxLevel :: Int,
    enchantmentMinCost :: CostFormula,
    enchantmentMaxCost :: CostFormula,
    enchantmentTreasureOnly :: Bool,
    enchantmentCurse :: Bool,
    enchantmentExclude :: [Text],
    enchantmentCategory :: EnchantmentCategory,
    enchantmentWeight :: Int,
    enchantmentTradeable :: Bool,
    enchantmentDiscoverable :: Bool
  }
  deriving (Show)

instance FromJSON Enchantment where
  parseJSON = withObject "Enchantment" $ \v ->
    Enchantment
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "maxLevel"
      <*> v .: "minCost"
      <*> v .: "maxCost"
      <*> v .: "treasureOnly"
      <*> v .: "curse"
      <*> v .: "exclude"
      <*> v .: "category"
      <*> v .: "weight"
      <*> v .: "tradeable"
      <*> v .: "discoverable"
