{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Block where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:), (.:?))
import Data.HashMap.Strict qualified as M
import Data.Text (Text)

data BlockBoundingBox
  = BlockBoundingBoxEmpty
  | BlockBoundingBoxBlock
  deriving (Show)

instance FromJSON BlockBoundingBox where
  parseJSON = withText "BlockBoundingBox" $ \t ->
    case t of
      "empty" -> pure BlockBoundingBoxEmpty
      "block" -> pure BlockBoundingBoxBlock
      _ -> fail $ "Unknown BlockBoundingBox: " ++ show t

type BlockHarvestTools = M.HashMap Text Bool

data BlockStateType
  = BlockStateTypeBool
  | BlockStateTypeInt
  deriving (Show)

instance FromJSON BlockStateType where
  parseJSON = withText "BlockStateType" $ \t ->
    case t of
      "bool" -> pure BlockStateTypeBool
      "int" -> pure BlockStateTypeInt
      _ -> fail $ "Unknown BlockStateType: " ++ show t

data BlockState = BlockState
  { blockStateName :: Text,
    blockStateType :: BlockStateType,
    blockStateNumValues :: Int,
    blockStateValues :: Maybe [Text]
  }
  deriving (Show)

instance FromJSON BlockState where
  parseJSON = withObject "BlockState" $ \v ->
    BlockState
      <$> v .: "name"
      <*> v .: "type"
      <*> v .: "num_values"
      <*> v .:? "values"

data Block = Block
  { blockId :: Int,
    blockName :: Text,
    blockDisplayName :: Text,
    blockHardness :: Float,
    blockResistance :: Float,
    blockStackSize :: Int,
    blockDiggable :: Bool,
    blockMaterial :: Text,
    blockTransparent :: Bool,
    blockEmitLight :: Int,
    blockFilterLight :: Int,
    blockDefaultState :: Int,
    blockMinStateId :: Int,
    blockMaxStateId :: Int,
    blockStates :: [BlockState],
    blockHarvestTools :: BlockHarvestTools,
    blockDrops :: [Int],
    blockBoundingBox :: BlockBoundingBox
  }
  deriving (Show)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v ->
    Block
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "displayName"
      <*> v .: "hardness"
      <*> v .: "resistance"
      <*> v .: "stackSize"
      <*> v .: "diggable"
      <*> v .: "material"
      <*> v .: "transparent"
      <*> v .: "emitLight"
      <*> v .: "filterLight"
      <*> v .: "defaultState"
      <*> v .: "minStateId"
      <*> v .: "maxStateId"
      <*> v .: "states"
      <*> v .: "harvestTools"
      <*> v .: "drops"
      <*> v .: "boundingBox"