{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Either (isRight)
import Data.Maybe (isJust)
-- Import modules to test

import Data.Minecraft.Attribute
import Data.Minecraft.Biome
import Data.Minecraft.Block
import Data.Minecraft.Effect
import Data.Minecraft.Enchantment
import Data.Minecraft.Entity
import Data.Minecraft.Food
import Data.Minecraft.Instrument
import Data.Minecraft.Item
import Data.Minecraft.Material
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

dataDir :: FilePath
dataDir = "minecraft-data/data/pc/1.21.8"

spec :: Spec
spec = do
  describe "Block parsing" $ do
    it "parses block JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/blocks.json"
      let blocks = eitherDecode content :: Either String [Block]
      blocks `shouldSatisfy` isRight

  describe "Biome parsing" $ do
    it "parses biome JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/biomes.json"
      let biomes = eitherDecode content :: Either String [Biome]
      biomes `shouldSatisfy` isRight

  describe "Attribute parsing" $ do
    it "parses attribute JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/attributes.json"
      let attributes = eitherDecode content :: Either String [Attribute]
      attributes `shouldSatisfy` isRight

  describe "Effect parsing" $ do
    it "parses effect JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/effects.json"
      let effects = eitherDecode content :: Either String [Effect]
      effects `shouldSatisfy` isRight

  describe "Enchantment parsing" $ do
    it "parses enchantment JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/enchantments.json"
      let enchantments = eitherDecode content :: Either String [Enchantment]
      enchantments `shouldSatisfy` isRight

  describe "Entity parsing" $ do
    it "parses entity JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/entities.json"
      let entities = eitherDecode content :: Either String [Entity]
      entities `shouldSatisfy` isRight

  describe "Food parsing" $ do
    it "parses food JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/foods.json"
      let foods = eitherDecode content :: Either String [Food]
      foods `shouldSatisfy` isRight

  describe "Instrument parsing" $ do
    it "parses instrument JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/instruments.json"
      let instruments = eitherDecode content :: Either String [Instrument]
      instruments `shouldSatisfy` isRight

  describe "Item parsing" $ do
    it "parses item JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/items.json"
      let items = eitherDecode content :: Either String [Item]
      items `shouldSatisfy` isRight

  describe "Material parsing" $ do
    it "parses materials JSON correctly" $ do
      content <- L8.readFile $ dataDir ++ "/materials.json"
      let materials = eitherDecode content :: Either String Materials
      materials `shouldSatisfy` isRight
