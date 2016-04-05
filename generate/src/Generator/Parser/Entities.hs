{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Generator.Parser.Entities
  ( ExtractedEntities (..)
  , ExtractedEntity
  , parseEntities
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)


data ExtractedEntities = ExtractedEntities [ExtractedEntity]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedEntities

data ExtractedEntity = ExtractedEntity
  { entityId            :: Int
  , entityInternalId    :: Maybe Int
  , entityName          :: String
  , entityDisplayName   :: String
  , entityType          :: String
  , entityWidth         :: Double
  , entityHeight        :: Double
  , entityCategory      :: Maybe String
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedEntity where
  parseJSON (Object a) = ExtractedEntity
    <$> a .: "id"
    <*> a .:? "internalId"
    <*> a .: "name"
    <*> a .: "displayName"
    <*> a .: "type"
    <*> a .: "width"
    <*> a .: "height"
    <*> a .:? "category"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseEntities :: IO ()
parseEntities = do
  putStrLn "Generating Entity data..."
  rawEntitiesList <- mapM (\x -> B.readFile (x ++ "/entities.json")) dataPaths
  let possibleEntitiesList = mapM eitherDecodeStrict' rawEntitiesList :: Either String [ExtractedEntities]
  case possibleEntitiesList of
    Right entitiesList -> return ()
    Left err -> putStrLn err
