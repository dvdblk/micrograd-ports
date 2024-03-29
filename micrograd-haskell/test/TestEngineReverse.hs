module TestEngineReverse (
    testsEngineReverse,
) where

import EngineReverse
import Test.HUnit


testValueInit :: Test
testValueInit =
    TestCase
        ( do
            assertEqual "Default values should be equal" (defaultValue (3 :: Integer)) (defaultValue 3)
            assertBool "Default values should be equal" $ defaultValue (3.0 :: Double) == defaultValue 3
            assertBool "Default values should not be equal" $ defaultValue (3.0 :: Double) /= defaultValue 2
            assertBool "initValue should set operation" $ _op (valueInit (3 :: Integer) "test" []) == "test"
            assertBool "initValue should set previous values" $ _prev (valueInit (3 :: Integer) "test" [defaultValue 1, defaultValue 2]) == [defaultValue 1, defaultValue 2]
            assertBool "initValue shouldn't set equal previous values" $ _prev (valueInit (3 :: Integer) "test" [defaultValue 1, defaultValue 1]) == [defaultValue 1]
            assertBool "initValue should set grad to 0" $ grad (valueInit (3 :: Integer) "test" []) == 0
            assertBool "initValue should set data" $ _data (valueInit (3 :: Integer) "test" []) == 3
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
            assertEqual "Exponentiation gradient should be correct" 12 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (2 :: Double) ** defaultValue 3)
            assertEqual "Exponentiation gradient should be correct" 48 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (-4 :: Double) ** defaultValue 3)
        )

testValueReluGrad :: Test
testValueReluGrad =
    TestCase
        ( do
            assertEqual "Relu gradient should be 1" 1 (grad . head . _prev . _backward . incrementGrad 1 . relu $ defaultValue (2.0 :: Double))
            assertEqual "Relu gradient should be 0" 0 (grad . head . _prev . _backward . incrementGrad 1 . relu $ defaultValue (0 :: Double))
            assertEqual "Relu gradient should be 0" 0 (grad . head . _prev . _backward . incrementGrad 1 . relu $ defaultValue (-2 :: Double))
        )

testValueDuplicitChildren :: Test
testValueDuplicitChildren =
    TestCase
        ( do
            assertEqual "Multiplication with equal values should have one child" 1 (length . _prev . _backward . incrementGrad 1 $ defaultValue (2 :: Double) * defaultValue 2)
            assertEqual "Multiplication with equal values should have correct gradient" 8 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (4 :: Double) * defaultValue 4)
            assertEqual "Addition with equal values should have one child" 1 (length $ _prev $ _backward . incrementGrad 1 $ defaultValue (2 :: Double) + defaultValue 2)
            assertEqual "Addition with equal values should have correct gradient" 2 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) + defaultValue 3)
            assertEqual "Subtraction with equal values should have two children" 2 (length $ _prev $ _backward . incrementGrad 1 $ defaultValue (2 :: Double) - defaultValue 2)
            assertEqual "Subtraction with equal values should have correct gradient" 0 (grad . last . _prev . last . _prev . _backward . incrementGrad 1 $ defaultValue (2 :: Double) - defaultValue 2)
            assertEqual "Division with equal values should have two children" 2 (length $ _prev $ _backward . incrementGrad 1 $ defaultValue (2 :: Double) / defaultValue 2)
            assertEqual "Division with equal values should have correct gradient" 0 (grad . head . _prev . last . _prev . _backward . incrementGrad 1 $ defaultValue (5 :: Double) / defaultValue 5)
        )

testValueBackwardDefaults :: Test
testValueBackwardDefaults =
    TestCase
        ( do
            assertEqual "Backward should have correct gradient" 1 (grad . backward $ defaultValue (2 :: Double))
            assertEqual "Default value should have zero gradient" 0 (grad $ defaultValue (2 :: Double))
        )

testValueBackwardChildrenGrad :: Test
testValueBackwardChildrenGrad =
    TestCase
        ( do
            assertEqual "Backward first child should have gradient 1 for addition" 1 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) + defaultValue 4)
            assertEqual "Backward second child should have gradient 1 for addition" 1 (grad . last . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) + defaultValue 4)
            assertEqual "Backward first child should have correct gradient for multiplication" 4 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4)
            assertEqual "Backward second child should have correct gradient for multiplication" 3 (grad . last . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4)
        )

testValueGrad :: Test
testValueGrad =
    TestCase
        ( do
            assertEqual "Multiplication grad should be correct" 4 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4)
            assertEqual "Multiplication grad should be correct" 3 (grad . last . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4)
            assertEqual "Multiplication grad should be correct" 1 (grad . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4)
            assertEqual "Addition grad should be correct" 1 (grad . head . _prev . _backward . incrementGrad 1 $ defaultValue (-5 :: Double) + defaultValue 16)
            assertEqual "Addition grad should be correct" 1 (grad . last . _prev . _backward . incrementGrad 1 $ defaultValue (-5 :: Double) + defaultValue 16)
            assertEqual "Addition grad should be correct" 1 (grad . _backward . incrementGrad 1 $ defaultValue (-5 :: Double) + defaultValue 16)
            assertEqual "Chain rule grad should be correct" 44 (grad . head . _prev . head . _prev . _backward . incrementGrad 1 $ defaultValue (3 :: Double) * defaultValue 4 * (defaultValue (-5) + defaultValue 16))
        )

testValueTanh :: Test
testValueTanh =
    TestCase
        ( do
            assertEqual "Tanh should be correct" 0 (_data . EngineReverse.tanh . defaultValue $ 0 :: Double)
            assertEqual "Tanh should be correct" 1 (_data . EngineReverse.tanh . defaultValue $ 20 :: Double)
            assertEqual "Tanh should be correct" (-1) (_data . EngineReverse.tanh . defaultValue $ (-20) :: Double)
        )

testsEngineReverse :: Test
testsEngineReverse =
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
        , TestLabel "testValueReluGrad" testValueReluGrad
        , TestLabel "testValueDuplicitChildren" testValueDuplicitChildren
        , TestLabel "testValueBackwardDefaults" testValueBackwardDefaults
        , TestLabel "testValueBackwardChildrenGrad" testValueBackwardChildrenGrad
        , TestLabel "testValueGrad" testValueGrad
        , TestLabel "testValueTanh" testValueTanh
        ]
