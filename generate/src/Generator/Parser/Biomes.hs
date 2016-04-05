{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Generator.Parser.Biomes
  ( ExtractedBiomes (..)
  , ExtractedBiome (..)
  , parseBiomes
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics

data ExtractedBiomes = ExtractedBiomes [ExtractedBiome]
  deriving (Show,Eq,Generic)

instance ToJSON ExtractedBiomes
instance FromJSON ExtractedBiomes


data ExtractedBiome = ExtractedBiome
  { biomeId           :: Int
  , biomeColor        :: Int
  , biomeName         :: String
  , biomeRainfall     :: Double
  , biomeTemperature  :: Double
  } deriving (Show,Eq,Generic)

instance ToJSON ExtractedBiome where
  toJSON (ExtractedBiome a b c d e) = object
    [ "id"            .= a
    , "color"         .= b
    , "name"          .= c
    , "rainfall"      .= d
    , "temperature"   .= e
    ]

instance FromJSON ExtractedBiome where
  parseJSON (Object a) = ExtractedBiome
    <$> a .: "id"
    <*> a .: "color"
    <*> a .: "name"
    <*> a .: "rainfall"
    <*> a .: "temperature"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseBiomes :: IO ()
parseBiomes = do
  putStrLn "Generating Biome data..."
  rawBiomesList <- mapM (\x -> B.readFile (x ++ "/biomes.json")) dataPaths
  let possibleBiomesList = mapM eitherDecodeStrict' rawBiomesList :: Either String [ExtractedBiomes]
  case possibleBiomesList of
    Right biomesList -> return ()
    Left err -> putStrLn err
