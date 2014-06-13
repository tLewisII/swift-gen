module Main where

import Test.HUnit
import CodeGen

import Prelude hiding (take, drop, head, tail)
import Data.Text hiding (reverse, zip, filter)
import Data.Monoid
import System.Environment
import Language.Haskell.Exts as HS hiding (prettyPrint)

main = do
  expected <- readFile "tests/ex1.swift"
  program <- fromParseResult `fmap` parseFile "tests/ex1.swift.gen"
  swift <- return $ unpack $ transform program
  let tests = TestCase (assertEqual "code gen works" swift expected)
  runTestTT tests
