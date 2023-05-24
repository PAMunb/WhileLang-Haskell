module CFGtoDOT.GenerateDOT where

import Syntax
import CFG

import Prelude hiding (init)
import Data.Set 
import Data.Foldable
import Data.Map
import Data.Text hiding (init)
import Data.Text.IO
import Data.List hiding (init)
import System.Process

writePairsToFile :: (Show k, Show v) => [(k, v)] -> IO ()
writePairsToFile pairs = do
  let content = generateDotFileContent pairs
  Data.Text.IO.writeFile "src/CFGtoDOT/cfg.dot" content
  executeDotCommand

generateDotFileContent :: (Show k, Show v) => [(k, v)] -> Data.Text.Text
generateDotFileContent pairs =
  Data.Text.unlines $
    [ Data.Text.pack "digraph {"
    , Data.Text.pack "\tlabel = <<font color='red'><b>CFG</b></font>>;"
    , Data.Text.pack "\tlabelloc = \"t\";"
    ] ++
    Prelude.map (\(key, value) -> Data.Text.concat [Data.Text.pack "\t", Data.Text.pack $ show key, Data.Text.pack " -> ", Data.Text.pack $ show value]) pairs ++
    [ Data.Text.pack "}"
    ]

executeDotCommand :: IO ()
executeDotCommand = do
  _ <- system "dot -Tpng src/CFGtoDOT/cfg.dot -o src/CFGtoDOT/cfg.png"
  pure ()
  
callWritePairsToFile :: Stmt -> IO ()
callWritePairsToFile cfgprogram = do
  let myPairs = Data.Set.toList(flow cfgprogram)
  writePairsToFile myPairs
