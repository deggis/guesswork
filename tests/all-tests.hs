module Main where

import Control.Monad
import System.Exit (exitFailure,exitSuccess)
import Guesswork
import Text.Printf

import RegressorTests
import IOTests

testSets = [
    ioTests
   ,allRegressorTests
   ]

type Test = (IO (), String)
type TestSet = ([Test],String)

runner :: [TestSet] -> IO ()
runner = mapM_ run
  where run (set,name) = do
        putStrLn $ "TEST SET: " ++ name
        runSet set
        putStrLn ""

runSet :: [Test] -> IO ()
runSet tests = do
    let
        xs     = zip ([1..]::[Int]) tests
        n      = length tests
        line (i,s) = printf "Test %02d of %02d: %s" i n s
    mapM_ (\(i,(t,s)) -> (putStrLn $ line (i,s)) >> t) xs
    return ()

main = runner testSets 
