{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Generator.Parser.Effects
  ( ExtractedEffects (..)
  , ExtractedEffect (..)
  , parseEffects
  ) where
import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)


data ExtractedEffects = ExtractedEffects [ExtractedEffect]
  deriving (Show,Eq,Generic)


instance FromJSON ExtractedEffects


data ExtractedEffect = ExtractedEffect
  { effectId          :: Int
  , effectName        :: String
  , effectDisplayName :: String
  , effectType        :: String
  } deriving (Show,Eq,Generic)


instance FromJSON ExtractedEffect where
  parseJSON (Object a) = ExtractedEffect
    <$> a .: "id"
    <*> a .: "name"
    <*> a .: "displayName"
    <*> a .: "type"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseEffects :: IO ()
parseEffects = do
  putStrLn "Generating Effect data..."
  rawEffectsList <- mapM (\x -> B.readFile (x ++ "/effects.json")) dataPaths
  let possibleEffectsList = mapM eitherDecodeStrict' rawEffectsList :: Either String [ExtractedEffects]
  case possibleEffectsList of
    Right effectsList -> return ()
    Left err -> putStrLn err
