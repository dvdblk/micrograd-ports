import TestEngineReverse (testsEngineReverse)

import Test.HUnit

main :: IO Counts
main = do runTestTT testsEngineReverse