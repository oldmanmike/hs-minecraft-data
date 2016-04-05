{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Generator.Parser.Items
  ( ExtractedItems (..)
  , ExtractedItem (..)
  , ExtractedItemVariations (..)
  , ExtractedItemVariation (..)
  , parseItems
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)

data ExtractedItems = ExtractedItems [ExtractedItem]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedItems

data ExtractedItem = ExtractedItem
  { itemId          :: Int
  , itemDisplayName :: String
  , itemStackSize   :: Int
  , itemName        :: String
  , itemVariations  :: Maybe ExtractedItemVariations
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedItem where
  parseJSON (Object a) = ExtractedItem
    <$> a .: "id"
    <*> a .: "displayName"
    <*> a .: "stackSize"
    <*> a .: "name"
    <*> a .:? "variations"
  parseJSON _ = mzero

data ExtractedItemVariations = ExtractedItemVariations [ExtractedItemVariation]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedItemVariations

data ExtractedItemVariation = ExtractedItemVariation
  { variationItemMetadata     :: Int
  , variationItemDisplayName  :: String
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedItemVariation where
  parseJSON (Object a) = ExtractedItemVariation
    <$> a .: "metadata"
    <*> a .: "displayName"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseItems :: IO ()
parseItems = do
  putStrLn "Generating Item data..."
  rawItemsList <- mapM (\x -> B.readFile (x ++ "/items.json")) dataPaths
  let possibleItemsList = mapM eitherDecodeStrict' rawItemsList :: Either String [ExtractedItems]
  case possibleItemsList of
    Right itemsList -> return ()
    Left err -> putStrLn err
