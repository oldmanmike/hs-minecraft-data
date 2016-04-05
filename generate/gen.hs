{-# LANGUAGE OverloadedStrings #-}

import            Generator.Parser

main :: IO ()
main = do
  putStrLn "Generating hs-minecraft-data from minecraft-data..."
  parseBiomes
  parseBlocks
  parseEffects
  parseEntities
  parseInstruments
  parseItems
  parseMaterials
  genVersions
  parseWindows
  putStrLn "Done!"
