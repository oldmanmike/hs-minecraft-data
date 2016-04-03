{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            Data.Char
import            Data.Data
import            Data.List
import            Data.List.Split
import qualified  Data.Map as Map
import            Data.Maybe
import            GHC.Generics hiding (moduleName)

type ExtractedVersionList = [String]

data ExtractedBiomes = ExtractedBiomes [ExtractedBiome]
  deriving (Show,Eq,Generic,Data,Typeable)

instance ToJSON ExtractedBiomes
instance FromJSON ExtractedBiomes


data ExtractedBiome = ExtractedBiome 
  { biomeId           :: Int
  , biomeColor        :: Int
  , biomeName         :: String
  , biomeRainfall     :: Double
  , biomeTemperature  :: Double
  } deriving (Show,Eq,Generic,Data,Typeable)

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

data ExtractedBlocks = ExtractedBlocks [ExtractedBlock]
  deriving (Show,Eq,Generic,Data,Typeable)

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
  , blockVariations   :: Maybe ExtractedVariations
  , blockDrops        :: Maybe ExtractedDrops
  , blockTransparent  :: Bool
  , blockEmitLight    :: Int
  , blockFilterLight  :: Int
  } deriving (Show,Eq,Generic,Data,Typeable)


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

data ExtractedVariations = ExtractedVariations [ExtractedVariation]
  deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedVariations

data ExtractedVariation = ExtractedVariation
  { variationMetadata     :: Int
  , variationDisplayName  :: String
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedVariation where
  parseJSON (Object a) = ExtractedVariation
    <$> a .: "metadata"
    <*> a .: "displayName"
  parseJSON _ = mzero

data ExtractedDrops = ExtractedDrops [ExtractedDrop]
  deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedDrops

data ExtractedDrop = ExtractedDrop
  { dropDrop  :: Int
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedDrop where
  parseJSON (Object a) = ExtractedDrop
    <$> a .: "drop"
  parseJSON _ = mzero

data ExtractedEffects = ExtractedEffects [ExtractedEffect]
  deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedEffects

data ExtractedEffect = ExtractedEffect
  { effectId          :: Int
  , effectName        :: String
  , effectDisplayName :: String
  , effectType        :: String
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedEffect where
  parseJSON (Object a) = ExtractedEffect
    <$> a .: "id"
    <*> a .: "name"
    <*> a .: "displayName"
    <*> a .: "type"
  parseJSON _ = mzero

data ExtractedEntities = ExtractedEntities [ExtractedEntity]
  deriving (Show,Eq,Generic,Data,Typeable)

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
  } deriving (Show,Eq,Generic,Data,Typeable)

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

data ExtractedVersion = ExtractedVersion
  { version :: Int
  , minecraftVersion :: String
  , majorVersion :: String
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedVersion
instance ToJSON ExtractedVersion

type FuncString = (String,String)

versionPathMap :: Map.Map String String
versionPathMap = Map.fromList
  [ ("minecraft-data/data/0.30c/version.json","Data/Minecraft/Classic/Version")
  , ("minecraft-data/data/common/versions.json","Data/Minecraft/Common/Versions")
  , ("minecraft-data/data/1.7/version.json","Data/Minecraft/Release17/Version")
  , ("minecraft-data/data/1.8/version.json","Data/Minecraft/Release18/Version")
  , ("minecraft-data/data/1.9/version.json","Data/Minecraft/Release19/Version")
  , ("minecraft-data/data/1.9.1-pre2/version.json","Data/Minecraft/LatestSnapshot/Version")
  ]

mkModule :: FilePath -> [FuncString] -> String
mkModule path ((f,v):fs) = do
  let maybeModuleName = Map.lookup path versionPathMap
  case maybeModuleName of
    Just actualModuleName -> concat $
      [ "--------------------------------------------------------------------\n"
      , "-- |\n"
      , "-- Module        : " ++ fmtModuleName actualModuleName ++ "\n"
      , "-- Copyright     : (c) 2016 Michael Carpenter\n"
      , "-- License       : BSD3\n"
      , "-- Maintainer    : Michael Carpenter <oldmanmike.dev@gmail.com>\n"
      , "-- Stability     : experimental\n"
      , "-- Portability   : portable\n"
      , "--\n"
      , "--------------------------------------------------------------------\n"
      , "module " ++ fmtModuleName actualModuleName ++ " (\n"
      , exports functionDeclarations
      , "  ) where\n"
      , "\n"
      , concatMap (\(f,v) -> f ++ " = " ++ v ++ "\n\n") $ ((f,v):fs)
      ]
    Nothing -> ""
  where
  functionDeclarations = f : map fst fs

extractedValues :: [(ExtractedVersion -> String)]
extractedValues = [(show . version),(show . minecraftVersion),(show . majorVersion)]

fmtModuleName :: FilePath -> String
fmtModuleName path = concat $ (intersperse ".") $ map capitalize $ splitOn "/" path

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail

exports :: [String] -> String
exports [] = ""
exports (x:xs) = ("  " ++ x ++ ",\n") ++ exports xs

writeModule :: FilePath -> [FuncString] -> IO ()
writeModule path flst = do
    let maybeModuleName = Map.lookup path versionPathMap
    case maybeModuleName of
      Just moduleName -> do
        let doc = mkModule path flst
        writeFile ("../src/"++moduleName++".hs") doc
      Nothing -> return ()

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

genBiomes :: IO ()
genBiomes = do
  putStrLn "Generating Biome data..."
  rawBiomesList <- mapM (\x -> B.readFile (x ++ "/biomes.json")) dataPaths
  let possibleBiomesList = mapM eitherDecodeStrict' rawBiomesList :: Either String [ExtractedBiomes]
  return ()

genBlocks :: IO ()
genBlocks = do
  putStrLn "Generating Block data..."
  rawBlocksList <- mapM (\x -> B.readFile (x ++ "/blocks.json")) dataPaths
  let possibleBlocksList = mapM eitherDecodeStrict' rawBlocksList :: Either String [ExtractedBlocks]
  print possibleBlocksList
  return ()

genEffects :: IO ()
genEffects = do
  putStrLn "Generating Effect data..."
  rawEffectsList <- mapM (\x -> B.readFile (x ++ "/effects.json")) dataPaths
  let possibleEffectsList = mapM eitherDecodeStrict' rawEffectsList :: Either String [ExtractedEffects]
  print possibleEffectsList
  return ()

genEntities :: IO ()
genEntities = do
  putStrLn "Generating Entity data..."
  rawEntitiesList <- mapM (\x -> B.readFile (x ++ "/entities.json")) dataPaths
  let possibleEntitiesList = mapM eitherDecodeStrict' rawEntitiesList :: Either String [ExtractedEntities]
  print possibleEntitiesList

  return ()

genInstruments :: IO ()
genInstruments = do
  putStrLn "Generating Instrument data..."
  rawInstrumentsList <- mapM (\x -> B.readFile (x ++ "/instruments.json")) dataPaths
  return ()

genItems :: IO ()
genItems = do
  putStrLn "Generating Item data..."
  rawItemsList <- mapM (\x -> B.readFile (x ++ "/items.json")) dataPaths
  return ()

genMaterials :: IO ()
genMaterials = do
  putStrLn "Generating Material data..."
  rawMaterialsList <- mapM (\x -> B.readFile (x ++ "/materials.json")) dataPaths
  return ()

genVersions :: IO ()
genVersions = do
  rawVersionList <- B.readFile "minecraft-data/data/common/versions.json"
  let possiblyVersionList = eitherDecodeStrict' rawVersionList :: Either String [String] 
  case possiblyVersionList of
    Right versionList -> do
      let versionPaths =
            map (\s -> "minecraft-data/data/"++s++"/version.json") versionList
      rawList <- mapM B.readFile versionPaths
      let possiblyExtractedList = (mapM eitherDecodeStrict' rawList) :: Either String [ExtractedVersion]
      case possiblyExtractedList of
        Right extractedList -> do
          let fields = fmap (\x -> constrFields (toConstr x)) extractedList
          let values = fmap (\x -> fmap (\f -> f x) extractedValues) extractedList
          mapM_ (\(a,b) -> writeModule a b) $ zip versionPaths $ zipWith zip fields values
        Left err -> print err
    Left err -> print err

genWindows :: IO ()
genWindows = putStrLn "Generating Windows data..."

main :: IO ()
main = do
  putStrLn "Generating hs-minecraft-data from minecraft-data..."
  genBiomes
  genBlocks
  genEffects
  genEntities
  genInstruments
  genItems
  genMaterials
  genVersions
  genWindows
  putStrLn "Done!"
