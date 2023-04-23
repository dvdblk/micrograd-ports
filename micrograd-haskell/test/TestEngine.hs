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
            assertEqual "Operation should be default" "" (_op $ defaultValue (3 :: Integer))
            assertEqual "Operation should be changed" "test" (_op $ changeValueOperation "test" $ defaultValue (3 :: Integer))
        )

testAdditionMultiplication :: Test
testAdditionMultiplication =
    TestCase
        ( do
            assertEqual "Addition should be correct" 12 (_data $ defaultValue (-12.0 :: Double) + defaultValue 24.0)
            assertEqual "Addition should be correct" (-6) (_data $ defaultValue (-4.0 :: Double) + defaultValue (-2.0))
            assertEqual "Addition should be correct" 5 (_data $ defaultValue (3 :: Integer) + defaultValue 2)
            assertEqual "Multiplication should be correct" (-288) (_data $ defaultValue (-12.0 :: Double) * defaultValue 24.0)
            assertEqual "Multiplication should be correct" 8 (_data $ defaultValue (-4.0 :: Double) * defaultValue (-2.0))
            assertEqual "Multiplication should be correct" 6 (_data $ defaultValue (3 :: Integer) * defaultValue 2)
            assertEqual "Multiplication and addition should be correct" (-285) (_data $ defaultValue (-12.0 :: Double) * defaultValue 24.0 + defaultValue 3.0)
        )

testValueSubtraction :: Test
testValueSubtraction =
    TestCase
        ( do
            assertEqual "Subtraction should be correct" (-36) (_data $ defaultValue (-12.0 :: Double) - defaultValue 24.0)
            assertEqual "Subtraction should be correct" (-2) (_data $ defaultValue (-4.0 :: Double) - defaultValue (-2.0))
            assertEqual "Subtraction should be correct" 1 (_data $ defaultValue (3 :: Integer) - defaultValue 2)
        )

testValueBonusFunctions :: Test
testValueBonusFunctions =
    TestCase
        ( do
            assertEqual "Abs should be correct" 12 (_data $ abs $ defaultValue (-12.0 :: Double))
            assertEqual "Signum should be negative" (-1) (_data $ signum $ defaultValue (-12.0 :: Double))
            assertEqual "Signum should be positive" 1 (_data $ signum $ defaultValue (12.0 :: Double))
            assertEqual "Signum should be zero" 0 (_data $ signum $ defaultValue (0.0 :: Double))
            assertEqual "fromInteger should be correct" (3 :: Integer) (_data $ fromInteger (3 :: Integer))
        )

testValueDivision :: Test
testValueDivision =
    TestCase
        ( do
            assertEqual "Division should be correct" (-0.5) (_data $ defaultValue (-12.0 :: Double) / defaultValue 24.0)
            assertEqual "Division should be correct" 2 (_data $ defaultValue (-4.0 :: Double) / defaultValue (-2.0))
            assertEqual "Division should be correct" 1.5 (_data $ defaultValue (3 :: Double) / defaultValue 2)
        )

testValueRecip :: Test
testValueRecip =
    TestCase
        ( do
            assertEqual "Recip should be correct" (-0.08333333333333333) (_data $ recip $ defaultValue (-12.0 :: Double))
            assertEqual "Recip should be correct" (-0.25) (_data $ recip $ defaultValue (-4.0 :: Double))
            assertEqual "Recip should be correct" 0.3333333333333333 (_data $ recip $ defaultValue (3 :: Double))
        )

testValueExponentiation :: Test
testValueExponentiation =
    TestCase
        ( do
            assertEqual "Exponentiation should be correct" 144.0 (_data $ defaultValue (-12.0 :: Double) ** defaultValue 2)
            assertEqual "Exponentiation should be correct" 0.0625 (_data $ defaultValue (-4.0 :: Double) ** defaultValue (-2.0))
            assertEqual "Exponentiation should be correct" 9 (_data $ defaultValue (3.0 :: Double) ** defaultValue 2)
        )

testValueNegate :: Test
testValueNegate =
    TestCase
        ( do
            assertEqual "Negate value should be positive" 12 (_data $ negate $ defaultValue (-12.0 :: Double))
            assertEqual "Negate value should be positive" 4 (_data $ negate $ defaultValue (-4.0 :: Double))
            assertEqual "Negate value should be negative" (-3) (_data $ negate $ defaultValue (3 :: Double))
        )

testValueRelu :: Test
testValueRelu =
    TestCase
        ( do
            assertEqual "Relu should be 0" 0 (_data . relu . defaultValue $ (-12.0 :: Double))
            assertEqual "Relu should be linear" 3 (_data . relu . defaultValue $ 3 :: Integer)
        )

testValueExponential :: Test
testValueExponential =
    TestCase
        ( do
            assertEqual "Exponential should be correct" 0.1353352832366127 (_data . exp . defaultValue $ (-2.0 :: Double))
            assertEqual "Exponential should be correct" 20.085536923187668 (_data . exp . defaultValue $ (3 :: Double))
        )

testValueExponentiationGradient :: Test
testValueExponentiationGradient =
    TestCase
        ( do
            assertEqual "Exponentiation gradient should be correct" 32 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (2 :: Double) ** defaultValue 4)
            assertEqual "Exponentiation gradient should be correct" 4 (grad . head . _prev . backward $ defaultValue (2 :: Double) ** defaultValue 2)
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
