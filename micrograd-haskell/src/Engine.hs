{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use -" #-}
module Engine (
  Value (..),
  defaultValue,
  changeValueOperation,
)
where

import Data.Set qualified as Set

data Value a = Value
  { -- Need to use _data because data is a reserved keyword
    _data :: a
  , grad :: a
  , _op :: String
  , _prev :: Set.Set (Value a)
  }
  deriving (Show, Eq, Ord)

defaultValue :: Num a => a -> Value a
defaultValue v = Value v 0 "" Set.empty

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

  -- \| abs is not supported by OG micrograd
  abs :: Value a -> Value a
  abs v@(Value x _ _ _) = changeValueOperation "abs" $ if x >= 0 then v else negate v

  -- \| signum is not supported by OG micrograd
  signum :: Value a -> Value a
  signum v@(Value x g _ _) = Value (signum x) g "signum" (Set.fromList [v])
  fromInteger :: Integer -> Value a
  fromInteger = defaultValue . fromInteger
