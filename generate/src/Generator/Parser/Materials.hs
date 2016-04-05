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
module Generator.Parser.Materials
  ( parseMaterials
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseMaterials :: IO ()
parseMaterials = do
  putStrLn "Generating Material data..."
  rawMaterialsList <- mapM (\x -> B.readFile (x ++ "/materials.json")) dataPaths
  return ()
