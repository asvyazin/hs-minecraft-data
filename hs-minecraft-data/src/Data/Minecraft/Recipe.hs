{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Recipe where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?))

data RecipeResult = RecipeResult {
    recipeResultId :: Int,
    recipeResultCount :: Int
} deriving (Show)

data Recipe = Recipe {
    recipeShape :: Maybe [[Maybe Int]],
    recipeIngredients :: Maybe [Int],
    recipeResult :: RecipeResult
} deriving (Show)

newtype Recipes = Recipes (HashMap.HashMap Int [Recipe])
    deriving (Show, FromJSON)

instance FromJSON RecipeResult where
    parseJSON = withObject "RecipeResult" $ \v ->
        RecipeResult <$> v .: "id" <*> v .: "count"

instance FromJSON Recipe where
    parseJSON = withObject "Recipe" $ \v ->
        Recipe <$> v .:? "inShape" <*> v .:? "ingredients" <*> v .: "result"