{-# LANGUAGE OverloadedStrings #-}
module Main where

import CodeGen
import Prelude hiding (take, drop, head, tail)
import Data.Text hiding (reverse, zip, filter)
import Data.Monoid
import System.Environment
import Language.Haskell.Exts as HS hiding (prettyPrint)

-- main
main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      program <- fromParseResult `fmap` parseFile x
      swift <- return $ transform program
      putStrLn $ unpack swift
      -- TODO: check file exits before writing

    _ -> putStrLn "USAGE: file.swift.gen"
