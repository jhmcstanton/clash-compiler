module PortNames where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

{-# ANN topEntity
  (Synthesize
    { t_name     = "PortNames_topEntity"
    , t_inputs   = [
        ]
    , t_output   = PortProduct "top" [
            PortName "zero",
            PortProduct "sub" [
              PortName "one",
              PortName "two"
            ]
        ]
    }) #-}
topEntity :: (Signal System Int, (Signal System Int, Signal System Int))
topEntity = (pure 0, (pure 1, pure 2))

{-# ANN bitEntity
  (Synthesize
    { t_name     = "PortNames_bitEntity"
    , t_inputs   =
        [ PortName "inp0"
        , PortName "inp1"
        , PortName "inp2"
        , PortName "inp3"
        , PortName "inp4"
        , PortName "inp5"
        , PortName "inp6"
        ]
    , t_output = PortName "outp0"
    }) #-}
bitEntity
  :: Bit
  -> BitVector 1
  -> Unsigned 1
  -> Vec 1 Bit
  -> Vec 1 (BitVector 1)
  -> Vec 1 (Unsigned 1)
  -> Vec 1 (Signed 1)
  -> Bool
bitEntity _ _ _ _ _ _ _ = True

-- Simulation test
{-# ANN testBench
  (Synthesize
    { t_name     = "PortNames_testBench"
    , t_inputs   = [ ]
    , t_output   = PortName "result"
    }) #-}
testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0, (1, 2)) :> (0, (1, 2)) :> Nil)
    done           = expectedOutput (bundle $ bundle <$> topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "PortNames_topEntity" </> "PortNames_topEntity.v")

  assertIn "top_zero" content
  assertIn "top_sub_one" content
  assertIn "top_sub_two" content

vec1bitVerilog :: IO ()
vec1bitVerilog = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> "PortNames_bitEntity" </> "PortNames_topEntity.v")

  mapM_ (`assertIn` content)
    [ "input inp0"
    , "input [0:0] inp1"
    , "input [0:0] inp2"
    , "input [0:0] inp3"
    , "input [0:0] inp4"
    , "input [0:0] inp5"
    , "input [0:0] inp6"
    ]
