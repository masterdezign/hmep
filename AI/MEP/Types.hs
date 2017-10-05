module AI.MEP.Types where

import qualified Data.Vector as V


-- | A cromosome is a vector of symbols
type Chromosome a = V.Vector (Symbol a Char Int)

-- | Either a terminal symbol or a three-address code (a function
-- and two pointers)
data Symbol a n i =
  C a             -- ^ Terminal symbol: constant
  | Var n         -- ^ Terminal symbol: variable
  | Op (F a) i i  -- ^ Operation

-- | A function
type F a = (Char, a -> a -> a)
