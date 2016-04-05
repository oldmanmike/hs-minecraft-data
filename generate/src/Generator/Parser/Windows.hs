{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Generator.Parser.Windows
  ( ExtractedWindows (..)
  , ExtractedWindow (..)
  , ExtractedSlot (..)
  , ExtractedOpenWith (..)
  , parseWindows
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)


data ExtractedWindows = ExtractedWindows [ExtractedWindow]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedWindows

data ExtractedWindow = ExtractedWindow
  { windowId          :: String
  , windowName        :: String
  , windowSlots       :: Maybe [ExtractedSlot]
  , windowProperties  :: Maybe [String]
  , windowOpenWith    :: Maybe [ExtractedOpenWith]
  } deriving (Show,Eq)

instance FromJSON ExtractedWindow where
  parseJSON (Object a) = ExtractedWindow
    <$> a .: "id"
    <*> a .: "name"
    <*> a .:? "slot"
    <*> a .:? "properties"
    <*> a .:? "openedWith"
  parseJSON _ = mzero

data ExtractedSlot = ExtractedSlot
  { slotName    :: String
  , slotIndex   :: Int
  , slotSize    :: Maybe Int
  } deriving (Show,Eq)

instance FromJSON ExtractedSlot where
  parseJSON (Object a) = ExtractedSlot
    <$> a .: "name"
    <*> a .: "index"
    <*> a .:? "size"
  parseJSON _ = mzero

data ExtractedOpenWith = ExtractedOpenWith
  { openWithType    :: String
  , openWithId      :: Int
  } deriving (Show,Eq)

instance FromJSON ExtractedOpenWith where
  parseJSON (Object a) = ExtractedOpenWith
    <$> a .: "type"
    <*> a .: "id"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseWindows :: IO ()
parseWindows = do
    putStrLn "Generating Window data..."
    rawWindowsList <- mapM (\x -> B.readFile (x ++ "/windows.json")) dataPaths
    let possibleWindowsList = mapM eitherDecodeStrict' rawWindowsList :: Either String [ExtractedWindows]
    case possibleWindowsList of
      Right windowsList -> return ()
      Left err -> putStrLn err
