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
module Generator.Parser.Instruments
  ( ExtractedInstruments (..)
  , ExtractedInstrument (..)
  , parseInstruments
  ) where

import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B
import            GHC.Generics hiding (moduleName)

data ExtractedInstruments = ExtractedInstruments [ExtractedInstrument]
  deriving (Show,Eq,Generic)

instance FromJSON ExtractedInstruments

data ExtractedInstrument = ExtractedInstrument
  { instrumentId    :: Int
  , instrumentName  :: String
  } deriving (Show,Eq,Generic)

instance FromJSON ExtractedInstrument where
  parseJSON (Object a) = ExtractedInstrument
    <$> a .: "id"
    <*> a .: "name"
  parseJSON _ = mzero

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseInstruments :: IO ()
parseInstruments = do
  putStrLn "Generating Instrument data..."
  rawInstrumentsList <- mapM (\x -> B.readFile (x ++ "/instruments.json")) dataPaths
  let possibleInstrumentsList = mapM eitherDecodeStrict' rawInstrumentsList :: Either String [ExtractedInstruments]
  case possibleInstrumentsList of
    Right instrumentsList -> return ()
    Left err -> putStrLn err
