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
module Generator.Parser.Blocks
  ( ExtractedBlocks (..)
  , ExtractedBlock (..)
  , ExtractedBlockVariations (..)
  , ExtractedBlockVariation (..)
  , ExtractedDrops (..)
  , ExtractedDrop (..)
  , parseBlocks
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)

data ExtractedBlocks = ExtractedBlocks [ExtractedBlock]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedBlocks

data ExtractedBlock = ExtractedBlock
  { blockId           :: Int
  , blockDisplayName  :: String
  , blockName         :: String
  , blockHardness     :: Int
  , blockStackSize    :: Int
  , blockDiggable     :: Bool
  , blockBoundedBox   :: Maybe String
  , blockMaterial     :: Maybe String
  , blockHarvestTools :: Maybe [(ExtractedBlock,Bool)]
  , blockVariations   :: Maybe ExtractedBlockVariations
  , blockDrops        :: Maybe ExtractedDrops
  , blockTransparent  :: Bool
  , blockEmitLight    :: Int
  , blockFilterLight  :: Int
  } deriving (Show,Eq,Generic)


{-
instance ToJSON ExtractedBlock where
  toJSON (ExtractedBlock a b c d e f g h i j k l m n) = object
    [ "id"            .= a
    , "displayName"   .= b
    , "name"          .= c
    , "hardness"      .= d
    , "stackSize"     .= e
    , "diggable"      .= f
    , "boundedBox"    .= g
    , "material"      .= h
    , "harvestTools"  .= i
    , "variations"    .= j
    , "drops"         .= k
    , "transparent"   .= l
    , "emitLight"     .= m
    , "filterLight"   .= n
    ]
-}

instance FromJSON ExtractedBlock where
  parseJSON (Object a) = ExtractedBlock
    <$> a .: "id"
    <*> a .: "displayName"
    <*> a .: "name"
    <*> a .: "hardness"
    <*> a .: "stackSize"
    <*> a .: "diggable"
    <*> a .:? "boundedBox"
    <*> a .:? "material"
    <*> a .:? "harvestTools"
    <*> a .:? "variations"
    <*> a .:? "drops"
    <*> a .: "transparent"
    <*> a .: "emitLight"
    <*> a .: "filterLight"
  parseJSON _ = mzero


data ExtractedBlockVariations = ExtractedBlockVariations [ExtractedBlockVariation]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedBlockVariations

data ExtractedBlockVariation = ExtractedBlockVariation
  { variationBlockMetadata     :: Int
  , variationBlockDisplayName  :: String
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedBlockVariation where
  parseJSON (Object a) = ExtractedBlockVariation
    <$> a .: "metadata"
    <*> a .: "displayName"
  parseJSON _ = mzero

data ExtractedDrops = ExtractedDrops [ExtractedDrop]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedDrops

data ExtractedDrop = ExtractedDrop
  { dropDrop  :: Int
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedDrop where
  parseJSON (Object a) = ExtractedDrop
    <$> a .: "drop"
  parseJSON _ = mzero


dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]


parseBlocks :: IO ()
parseBlocks = do
  putStrLn "Generating Block data..."
  rawBlocksList <- mapM (\x -> B.readFile (x ++ "/blocks.json")) dataPaths
  let possibleBlocksList = mapM eitherDecodeStrict' rawBlocksList :: Either String [ExtractedBlocks]
  case possibleBlocksList of
    Right blocksList -> return ()
    Left err -> putStrLn err
