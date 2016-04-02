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

topCommentBlock :: FilePath -> String
topCommentBlock path = concat
  [ "------------------------------------------------------------------------\n"
  , "-- |\n"
  , "-- Module        : " ++ moduleName path ++ "\n"
  , "-- Copyright     : (c) 2016 Michael Carpenter\n"
  , "-- License       : BSD3\n"
  , "-- Maintainer    : Michael Carpenter <oldmanmike.dev@gmail.com>\n"
  , "-- Stability     : experimental\n"
  , "-- Portability   : portable\n"
  , "--\n"
  , "------------------------------------------------------------------------\n"
  ]

extractedValues :: [(ExtractedVersion -> String)]
extractedValues = [(show . version),(show . minecraftVersion),(show . majorVersion)]


fmtVersion :: FilePath -> [String] -> [String] -> String
fmtVersion path fields values = concat $ 
  [header path [] fields]
  ++ zipWith (\f v -> f ++ " = " ++ v ++ "\n\n") fields values


fmtVersions :: [(FilePath,([String],[String]))] -> [(FilePath,String)]
fmtVersions [] = []
fmtVersions ((path,(fields,values)):xs)
  = (path,fmtVersion path fields values) : (fmtVersions xs)


header :: FilePath -> [String] -> [String] -> String
header path dats funcs = do
  let maybeModule = Map.lookup path versionPathMap
  case maybeModule of
    Just validModule -> concat $
      [ topCommentBlock path
      , "module " ++ validModule ++ "\n"
      , "  (\n"
      , exports dats
      , exports funcs
      , "  ) where\n\n"
      ]
    Nothing -> ""


moduleName :: FilePath -> String
moduleName path = concat $ (intersperse ".") $ map capitalize $ splitOn "/" path


capitalize :: String -> String
capitalize (head:tail) = toUpper head : lowered tail
  where lowered [] = []
        lowered (head:tail) = toLower head : lowered tail


exports :: [String] -> String
exports [] = ""
exports (x:xs) = ("  , " ++ x ++ "\n") ++ exports xs


versionPathMap :: Map.Map String String
versionPathMap = Map.fromList
  [ ("minecraft-data/data/0.30c/version.json","Data/Minecraft/Classic/Version")
  , ("minecraft-data/data/common/versions.json","Data/Minecraft/Common/Versions")
  , ("minecraft-data/data/1.7/version.json","Data/Minecraft/Release17/Version")
  , ("minecraft-data/data/1.8/version.json","Data/Minecraft/Release18/Version")
  , ("minecraft-data/data/1.9/version.json","Data/Minecraft/Release19/Version")
  , ("minecraft-data/data/1.9.1-pre2/version.json","Data/Minecraft/LatestSnapshot/Version")
  ]


writeVersions :: [(FilePath,([String],[String]))] -> IO ()
writeVersions metadataLst = do
    mapM_ (\(k,s) -> do
          let maybeModuleName = Map.lookup k versionPathMap
          case maybeModuleName of
            Just moduleName -> do
              let dat = fmtVersions metadataLst
              mapM_ (\(_,s) -> writeFile ("src/"++moduleName++".hs") s) dat
            Nothing -> return ()
          ) metadataLst


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
          let maybeMetadataLst = zip versionPaths $ zip fields values
          writeVersions maybeMetadataLst
        Left err -> print err
    Left err -> print err
