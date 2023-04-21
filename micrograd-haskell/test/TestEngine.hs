module TestEngine (
    testsEngine,
) where

import Engine
import Test.HUnit

testValueInit :: Test
testValueInit =
    TestCase
        ( do
            assertEqual "Default values should be equal" (defaultValue (3 :: Integer)) (defaultValue 3)
            assertBool "Default values should be equal" $ defaultValue (3.0 :: Double) == defaultValue 3
            assertBool "Default values should not be equal" $ defaultValue (3.0 :: Double) /= defaultValue 2
        )

testChangeValueOperation :: Test
testChangeValueOperation =
    TestCase
        ( do
            assertEqual "Operation should be default" (_op $ defaultValue (3 :: Integer)) ""
            assertEqual "Operation should be changed" (_op $ changeValueOperation "test" $ defaultValue (3 :: Integer)) "test"
        )

testAdditionMultiplication :: Test
testAdditionMultiplication =
    TestCase
        ( do
            assertEqual "Addition should be correct" (_data $ defaultValue (-12.0 :: Double) + defaultValue 24.0) 12
            assertEqual "Addition should be correct" (_data $ defaultValue (-4.0 :: Double) + defaultValue (-2.0)) (-6)
            assertEqual "Addition should be correct" (_data $ defaultValue (3 :: Integer) + defaultValue 2) (_data $ defaultValue 5)
            assertEqual "Multiplication should be correct" (_data $ defaultValue (-12.0 :: Double) * defaultValue 24.0) (-288)
            assertEqual "Multiplication should be correct" (_data $ defaultValue (-4.0 :: Double) * defaultValue (-2.0)) 8
            assertEqual "Multiplication should be correct" (_data $ defaultValue (3 :: Integer) * defaultValue 2) (_data $ defaultValue 6)
            assertEqual "Multiplication and addition should be correct" (_data $ defaultValue (-12.0 :: Double) * defaultValue 24.0 + defaultValue 3.0) (-285)
        )

testValueSubtraction :: Test
testValueSubtraction =
    TestCase
        ( do
            assertEqual "Subtraction should be correct" (_data $ defaultValue (-12.0 :: Double) - defaultValue 24.0) (-36)
            assertEqual "Subtraction should be correct" (_data $ defaultValue (-4.0 :: Double) - defaultValue (-2.0)) (-2)
            assertEqual "Subtraction should be correct" (_data $ defaultValue (3 :: Integer) - defaultValue 2) (_data $ defaultValue 1)
        )

testValueBonusFunctions :: Test
testValueBonusFunctions =
    TestCase
        ( do
            assertEqual "Abs should be correct" (_data $ abs $ defaultValue (-12.0 :: Double)) 12
            assertEqual "Signum should be negative" (_data $ signum $ defaultValue (-12.0 :: Double)) (-1)
            assertEqual "Signum should be positive" (_data $ signum $ defaultValue (12.0 :: Double)) 1
            assertEqual "Signum should be zero" (_data $ signum $ defaultValue (0.0 :: Double)) 0
            assertEqual "fromInteger should be correct" (_data $ fromInteger (3 :: Integer)) (3 :: Integer)
        )

testsEngine :: Test
testsEngine =
    test
        [ TestLabel "testValueInit" testValueInit
        , TestLabel "testChangeValueOperation" testChangeValueOperation
        , TestLabel "testAdditionMultiplication" testAdditionMultiplication
        , TestLabel "testValueSubtraction" testValueSubtraction
        , TestLabel "testValueBonusFunctions" testValueBonusFunctions
        ]
