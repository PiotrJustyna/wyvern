module Main where

import Content
import Data.Map
import Diagrams.Prelude
import ID
import SkewerBlock
import Test.HUnit

helloWorldTest :: Test
helloWorldTest = TestCase (assertEqual "hello world test" 0 0)

sanityCheckTests :: Test
sanityCheckTests = TestList [helloWorldTest]

main :: IO ()
main = runTestTTAndExit sanityCheckTests
