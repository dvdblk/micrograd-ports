import TestEngine (testsEngine)

import Test.HUnit

main :: IO Counts
main = do runTestTT testsEngine