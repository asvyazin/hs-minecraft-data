{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Instrument where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Instrument = Instrument
  { instrumentId :: Int,
    instrumentName :: Text
  }
  deriving (Show)

instance FromJSON Instrument where
  parseJSON = withObject "Instrument" $ \v ->
    Instrument
      <$> v .: "id"
      <*> v .: "name"