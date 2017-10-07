{- | Provide the basic MEP data structures
 -}
{-# LANGUAGE GADTs #-}
module AI.MEP.Types where

import qualified Data.Vector as V


type Population a = [Chromosome a]

type Phenotype a = (Double, Chromosome a, V.Vector Int)

-- | A chromosome is a vector of genes
type Chromosome a = V.Vector (Gene a Int)

-- | Either a terminal symbol or a three-address code (a function
-- and two pointers)
data Gene a i where
  -- Terminal symbol: constant
  C :: a -> Gene a i
  -- Terminal symbol: variable
  Var :: Int -> Gene a i
  -- Operation
  Op :: F a -> i -> i -> Gene a i

instance (Show a, Show i) => Show (Gene a i) where
  show (C c) = show c
  show (Var n) = "v" ++ show n
  show (Op (s, _) i1 i2) = show s ++ " " ++ show i1 ++ " " ++ show i2

-- | A function and its symbolic representation
type F a = (Char, a -> a -> a)
