{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Tint where

import Data.Aeson (FromJSON (parseJSON), Value (String, Number), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

data TintKey =
  TintKeyText Text
  | TintKeyInt Int
  deriving (Show)

data TintData = TintData
  { tintDataKeys :: [TintKey],
    tintDataColor :: Int
  }
  deriving (Show)

newtype Tint = Tint { tintData :: [TintData] }
  deriving (Show)

newtype Tints = Tints (HashMap.HashMap Text Tint)
  deriving (Show, FromJSON)

instance FromJSON TintData where
  parseJSON = withObject "TintData" $ \v -> do
    keys <- v .: "keys"
    color <- v .: "color"
    -- Convert both string and numeric keys to TintKey
    tintKeys <- mapM convertToTintKey keys
    pure $ TintData tintKeys color
    where
      convertToTintKey :: Value -> Parser TintKey
      convertToTintKey (String s) = pure $ TintKeyText s
      convertToTintKey (Number n) = pure $ TintKeyInt (floor n)
      convertToTintKey _ = fail "Key must be either String or Number"

instance FromJSON Tint where
  parseJSON = withObject "Tint" $ \v -> do
    dataList <- v .: "data"
    pure $ Tint dataList
