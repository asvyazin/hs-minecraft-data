{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Particle where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

data Particle = Particle
  { particleId :: Int,
    particleName :: Text
  }
  deriving (Show)

instance FromJSON Particle where
  parseJSON = withObject "Particle" $ \v ->
    Particle
      <$> v .: "id"
      <*> v .: "name"