module Main where

import Test.HUnit
import CodeGen

import System.Exit
import Prelude hiding (take, drop, head, tail)
import Data.Text hiding (reverse, zip, filter)
import Data.Monoid
import System.Environment
import Language.Haskell.Exts as HS hiding (prettyPrint)

main = do
  expected1 <- readFile "tests/ex1.swift"
  program1 <- fromParseResult `fmap` parseFile "tests/ex1.swift.gen"
  swift1 <- return $ unpack $ transform program1
  let test1 = TestCase (assertEqual "code gen works" swift1 expected1)

  expected2 <- readFile "tests/ex2.swift"
  program2 <- fromParseResult `fmap` parseFile "tests/ex2.swift.gen"
  swift2 <- return $ unpack $ transform program2
  let test2 = TestCase (assertEqual "code gen works" swift2 expected2)
  
  (Counts c t e f) <- runTestTT (TestList [TestLabel "ex1" test1, TestLabel "ex2" test2])
  if e > 0 || f > 0
    then exitWith (ExitFailure 1)
    else return ()
