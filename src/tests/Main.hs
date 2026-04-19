module Main where

import Test.Hspec
import ValidatorTests (spec)

main :: IO ()
main = hspec spec
