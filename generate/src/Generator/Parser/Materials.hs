{-# LANGUAGE OverloadedStrings #-}
module Generator.Parser.Materials
  ( parseMaterials
  ) where
import            Control.Monad
import            Data.Aeson
import qualified  Data.ByteString as B

dataPaths :: [FilePath]
dataPaths =
  [ "minecraft-data/data/1.7"
  , "minecraft-data/data/1.8"
  , "minecraft-data/data/1.9"
  ]

parseMaterials :: IO ()
parseMaterials = do
  putStrLn "Generating Material data..."
  rawMaterialsList <- mapM (\x -> B.readFile (x ++ "/materials.json")) dataPaths
  return ()
