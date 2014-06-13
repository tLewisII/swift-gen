module Main where

import Test.HUnit
import CodeGen

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
  runTestTT test1

  expected2 <- readFile "tests/ex2.swift"
  program2 <- fromParseResult `fmap` parseFile "tests/ex2.swift.gen"
  swift2 <- return $ unpack $ transform program2
  let test2 = TestCase (assertEqual "code gen works" swift2 expected2)
  runTestTT test2
