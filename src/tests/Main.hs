module Main where

import Test.HUnit
import ValidatorTests (idCategorizedAsUnique)

validationTests :: Test
validationTests = TestList [idCategorizedAsUnique]

main :: IO ()
main = runTestTTAndExit validationTests
