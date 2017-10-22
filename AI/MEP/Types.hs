{- |
   = Core MEP data structures
 -}
{-# LANGUAGE GADTs #-}
module AI.MEP.Types where

import qualified Data.Vector as V
import           Control.Monad.Primitive ( PrimMonad )

import           AI.MEP.Random ( RandT )


-- | MEP configuration
data Config a = Config
  {
    p'const :: Double        -- ^ Probability of constant generation
    , p'var :: Double        -- ^ Probability of variable generation.
                             -- The probability of operator generation is inferred
                             -- automatically as @1 - p'const - p'var@.
    , p'mutation :: Double   -- ^ Mutation probability
    , p'crossover :: Double  -- ^ Crossover probability

    , c'length :: Int        -- ^ The chromosome length
    , c'popSize :: Int       -- ^ A (sub)population size
    , c'popN :: Int          -- ^ Number of subpopulations (1 or more)  [not implemented]
    , c'ops :: V.Vector (F a)  -- ^ Functions pool with their symbolic
                             -- representations
    , c'vars :: Int          -- ^ The input dimensionality
  }

-- |
-- @
-- defaultConfig = Config
--   {
--     p'const = 0.1
--   , p'var = 0.4
--   , p'mutation = 0.1
--   , p'crossover = 0.9
--
--   , c'length = 50
--   , c'popSize = 100
--   , c'popN = 1
--   , c'ops = V.empty  -- <-- To be overridden
--   , c'vars = 1
--   }
-- @
defaultConfig :: Config Double
defaultConfig = Config
  {
    p'const = 0.1
  , p'var = 0.4
  , p'mutation = 0.1
  , p'crossover = 0.9

  , c'length = 50
  , c'popSize = 100
  , c'popN = 1
  , c'ops = V.empty
  , c'vars = 1
  }

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

-- | 'Eq' instance for 'Gene'
instance (Eq a, Eq i) => Eq (Gene a i) where
  (C a) == (C b) = a == b
  (Var a) == (Var b) = a == b
  (Op (s1,_) i1 i2) == (Op (s2,_) j1 j2) = s1 == s2 && i1 == j1 && i2 == j2
  _ == _ = False

-- | 'Show' instance for 'Gene'
instance (Show a, Show i) => Show (Gene a i) where
  show (C c) = show c
  show (Var n) = "v" ++ show n
  show (Op (s, _) i1 i2) = show s ++ " " ++ show i1 ++ " " ++ show i2

-- | A function and its symbolic representation
type F a = (Char, a -> a -> a)

-- Working with lists is not optimal.
-- For instance, a random selection operator
-- such as binaryTournament may look for last
-- elements in the list quite long for big
-- populations.
-- | List of chromosomes
type Population a = [Chromosome a]

-- | Evaluated population
type Generation a = [Phenotype a]

-- | Loss value, chromosome, and the best expression indices vector
type Phenotype a = (Double, Chromosome a, V.Vector Int)

-- | Abstract class ensuring generation of new random genes
class Genetic a where
  newGene :: (PrimMonad m) => Config a -> Int -> RandT m (Gene a Int)
  -- Remark: the following typeclass would be preferable:
  --   newGene :: (PrimMonad m) => Int -> RandT m (Gene a Int)
  -- Breaking changes?

  -- | Randomly initialize a new chromosome.
  -- By definition, the first gene is terminal (a constant
  -- or a variable).
  newChromosome :: PrimMonad m =>
    Config a  -- ^ Common configuration
    -> RandT m (Chromosome a)
  newChromosome c =
    V.mapM (newGene c) $ V.enumFromN 0 (c'length c)
