{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
    print "Got here"
    let maybeModuleName = Map.lookup path versionPathMap
    case maybeModuleName of
      Just moduleName -> do
        let doc = mkModule path flst
        print doc
        writeFile ("src/"++moduleName++".hs") doc
      Nothing -> print "something"

main :: IO ()
main = do
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
