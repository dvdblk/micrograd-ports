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

testValueDivision :: Test
testValueDivision =
    TestCase
        ( do
            assertEqual "Division should be correct" (_data $ defaultValue (-12.0 :: Double) / defaultValue 24.0) (-0.5)
            assertEqual "Division should be correct" (_data $ defaultValue (-4.0 :: Double) / defaultValue (-2.0)) 2
            assertEqual "Division should be correct" (_data $ defaultValue (3 :: Double) / defaultValue 2) (_data $ defaultValue 1.5)
        )

testValueRecip :: Test
testValueRecip =
    TestCase
        ( do
            assertEqual "Recip should be correct" (_data $ recip $ defaultValue (-12.0 :: Double)) (-0.08333333333333333)
            assertEqual "Recip should be correct" (_data $ recip $ defaultValue (-4.0 :: Double)) (-0.25)
            assertEqual "Recip should be correct" (_data $ recip $ defaultValue (3 :: Double)) (_data $ defaultValue 0.3333333333333333)
        )

testValueExponentiation :: Test
testValueExponentiation =
    TestCase
        ( do
            assertEqual "Exponentiation should be correct" (_data $ defaultValue (-12.0 :: Double) ** defaultValue 2) 144.0
            assertEqual "Exponentiation should be correct" (_data $ defaultValue (-4.0 :: Double) ** defaultValue (-2.0)) 0.0625
            assertEqual "Exponentiation should be correct" (_data $ defaultValue (3.0 :: Double) ** defaultValue 2) (_data $ defaultValue 9)
        )

testValueNegate :: Test
testValueNegate =
    TestCase
        ( do
            assertEqual "Negate value should be positive" (_data $ negate $ defaultValue (-12.0 :: Double)) 12
            assertEqual "Negate value should be positive" (_data $ negate $ defaultValue (-4.0 :: Double)) 4
            assertEqual "Negate value should be negative" (_data $ negate $ defaultValue (3 :: Double)) (-3)
        )

testValueRelu :: Test
testValueRelu =
    TestCase
        ( do
            assertEqual "Relu should be 0" (_data . relu . defaultValue $ (-12.0 :: Double)) 0
            assertEqual "Relu should be linear" (_data . relu . defaultValue $ 3 :: Integer) 3
        )

testValueExponential :: Test
testValueExponential =
    TestCase
        ( do
            assertEqual "Exponential should be correct" (_data . Engine.exp . defaultValue $ (-2.0 :: Double)) 0.1353352832366127
            assertEqual "Exponential should be correct" (_data . Engine.exp . defaultValue $ (3 :: Double)) 20.085536923187668
        )

testValueExponentiationGradient :: Test
testValueExponentiationGradient =
    TestCase
        ( do
            assertEqual "Exponentiation gradient should be correct" 32 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (2 :: Double) ** defaultValue 4)
        )

testsEngine :: Test
testsEngine =
    test
        [ TestLabel "testValueInit" testValueInit
        , TestLabel "testChangeValueOperation" testChangeValueOperation
        , TestLabel "testAdditionMultiplication" testAdditionMultiplication
        , TestLabel "testValueSubtraction" testValueSubtraction
        , TestLabel "testValueBonusFunctions" testValueBonusFunctions
        , TestLabel "testValueDivision" testValueDivision
        , TestLabel "testValueRecip" testValueRecip
        , TestLabel "testValueExponentiation" testValueExponentiation
        , TestLabel "testValueNegate" testValueNegate
        , TestLabel "testValueRelu" testValueRelu
        , TestLabel "testValueExponential" testValueExponential
        , TestLabel "testValueExponentiationGradient" testValueExponentiationGradient
        ]
