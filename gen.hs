{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import            Data.Aeson
import qualified  Data.ByteString as B
import            Data.Data
import            GHC.Generics

type ExtractedVersionList = [String]

data ExtractedVersion = ExtractedVersion
  { version :: Int
  , minecraftVersion :: String
  , majorVersion :: String
  } deriving (Show,Eq,Generic,Data,Typeable)

instance FromJSON ExtractedVersion
instance ToJSON ExtractedVersion

main :: IO ()
main = do
  rawVersionList <- B.readFile "minecraft-data/data/common/versions.json"
  let versionList = eitherDecodeStrict' rawVersionList :: Either String [String] 
  case versionList of
    Right list -> do
      let versionPaths =
            map (\s -> "minecraft-data/data/"++s++"/version.json") list
      rawList <- mapM B.readFile versionPaths
      let x = (mapM eitherDecodeStrict' rawList) :: Either String [ExtractedVersion]
      case x of
        Right lst -> print lst
        Left err -> print err
    Left err -> print err
