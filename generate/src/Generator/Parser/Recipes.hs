{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module       : Generator.Parser.Blocks
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
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
