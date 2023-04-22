{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use -" #-}
module Engine (
  Value (..),
  defaultValue,
  changeValueOperation,
  relu,
  Engine.exp,
  Engine.tanh,
  _backward
)
where

import Data.Set qualified as Set
import Data.Graph qualified as G

data Value a = Value
  { -- | Numerical data for this `Value`.
    -- Need to use `_data` because 'data' is a reserved keyword
    _data :: a
    -- | Gradient of the `Value`
  , grad :: a
    -- | Operation that created this `Value`
  , _op :: String
    -- | Previous **unique** values that created this `Value`
  , _prev :: Set.Set (Value a)
  }
  deriving (Show, Eq, Ord)

-- | defaultValue creates a new `Value` with the given data and no gradient
defaultValue :: Num a => a -> Value a
defaultValue v = Value v 0 "" Set.empty

-- | changeValueOperation changes the operation of a `Value` to the given operation.
-- It takes one argument of type 'String' and one argument of type `Value`.
changeValueOperation :: String -> Value a -> Value a
changeValueOperation op v = v{_op = op}

instance (Num a, Ord a) => Num (Value a) where
  (+) :: Value a -> Value a -> Value a
  v1@(Value x _ _ _) + v2@(Value y _ _ _) = Value (x + y) 0 "+" (Set.fromList [v1, v2])
  (*) :: Value a -> Value a -> Value a
  v1@(Value x _ _ _) * v2@(Value y _ _ _) = Value (x * y) 0 "*" (Set.fromList [v1, v2])
  (-) :: Value a -> Value a -> Value a
  v1 - v2 = (changeValueOperation "-" . (v1 +) . negate) v2
  negate :: Value a -> Value a
  negate v = changeValueOperation "neg" $ defaultValue (-1) * v

  -- | abs is not supported by OG micrograd
  abs :: Value a -> Value a
  abs v@(Value x _ _ _) = changeValueOperation "abs" $ if x >= 0 then v else negate v

  -- | signum is not supported by OG micrograd
  signum :: Value a -> Value a
  signum v@(Value x _ _ _) = Value (signum x) 0 "signum" (Set.fromList [v])
  fromInteger :: Integer -> Value a
  fromInteger = defaultValue . fromInteger

instance (Floating a, Ord a) => Fractional (Value a) where
  (/) :: Value a -> Value a -> Value a
  v1 / v2 = v1 * (v2 ** (-1))

  -- | recip is not supported by OG micrograd
  recip :: Value a -> Value a
  recip v = changeValueOperation "recip" $ defaultValue 1 / v
  fromRational :: Rational -> Value a
  fromRational = defaultValue . fromRational

instance (Floating a, Ord a) => Floating (Value a) where
  (**) :: Value a -> Value a -> Value a
  v1@(Value x _ _ _) ** v2@(Value y _ _ _) = Value (x ** y) 0 "**" (Set.fromList [v1, v2])

-- | Applies the rectified linear unit function to the given `Value`.
relu :: (Num a, Ord a) => Value a -> Value a
relu v@(Value x _ _ _) = Value (max 0 x) 0 "relu" (Set.fromList [v])

-- | Applies the exponential function to the given `Value`.
exp :: (Floating a, Ord a) => Value a -> Value a
exp v@(Value x _ _ _) = Value (Prelude.exp x) 0 "exp" (Set.fromList [v])

-- | Applies the hyperbolic tangent function to the given `Value`.
tanh :: (Floating a, Ord a) => Value a -> Value a
tanh v@(Value x _ _ _) = Value (Prelude.tanh x) 0 "tanh" (Set.fromList [v])

-- | incrementGrad increments the gradient of a `Value` by the given amount.
incrementGrad :: Num a => a -> Value a -> Value a
incrementGrad x v@(Value _ g _ _) = v { grad = g + x }

-- | _backward updates gradients of the children Values based on the operation
_backward :: (Floating a, Ord a, Show a) => Value a -> Value a
_backward v@(Value _ g op prev) = applyBackward v $ Set.toList prev
  where applyBackward (Value _ _ "+" _) [v1, v2] = v { _prev = Set.fromList [incrementGrad g v1, incrementGrad g v2] }
        applyBackward (Value _ _ "*" _) [v1, v2] = v { _prev = Set.fromList [incrementGrad (g * _data v2) v1, incrementGrad (g * _data v1) v2] }
        applyBackward (Value _ _ "**" _) [v1, v2] = v { _prev = Set.fromList [incrementGrad (_data v2 * _data v1 ** (_data v2 - 1) * g) v1, v2] }
        applyBackward (Value _ _ "relu" _) [v1] = v { _prev = Set.fromList [incrementGrad (g * if _data v1 > 0 then 1 else 0) v1] }
        applyBackward (Value _ _ "exp" _) [v1] = v { _prev = Set.fromList [incrementGrad (_data v1 * g) v1] }
        applyBackward (Value _ _ "tanh" _) [v1] = v { _prev = Set.fromList [incrementGrad ((1 - _data v1 ** 2) * g) v1] }
        applyBackward _ _ = error $ "Invalid operation (" ++ op ++ ") in _backward for prev: " ++ show prev

prevToEdges :: Value a -> [(Value a, Int, [Int])]
prevToEdges val@(Value _ _ _ prev) = traverseComputationGraph 0 prev v
  where traverseComputationGraph :: Int -> [Value a] -> Value a -> [(Value a, Int, [Int])]
        traverseComputationGraph i [] v = [(v, i, [])]
        traverseComputationGraph i [v1] v = (v, i, [i + 1]) : traverseComputationGraph (i + 1) v1
        traverseComputationGraph i [v1, v2] v = (v, i, [i + 1, i + 2]) : (traverseComputationGraph (i + 1) v1) ++ (traverseComputationGraph (i + 2) v2)

-- | backward computes the gradient of every Value in the computation graph in a topological order.
-- It also sets the gradient of the topmost node to 1.
--backward :: (Num a, Ord a) => Value a -> Value a
--backward v@(Value _ _ _ prev) = G.graphFromEdges . prevToEdges $ topNode where topNode = v { grad = 1 }
