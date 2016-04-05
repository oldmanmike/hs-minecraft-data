{-# LANGUAGE OverloadedStrings #-}
module Generator.Parser.Recipes
  ( parseRecipes
  ) where

import qualified  Data.ByteString as B

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.8/recipes.json"
  , "minecraft-data/data/1.9/recipes.json"
  ]

parseRecipes :: IO ()
parseRecipes = do
  putStrLn "Generating Recipe data..."
  rawRecipesList <- mapM (\x -> B.readFile x) dataPaths
  return ()
