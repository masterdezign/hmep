-- |
-- = Genetic operators

module AI.MEP.Operators (
  Config (..)
  , defaultConfig
  , LossFunction
  -- * Genetic operators
  , initialize
  , evaluatePopulation
  , best
  , worst
  , evolve
  , phenotype
  , binaryTournament
  , crossover
  , mutation3
  , smoothMutation
  , newChromosome
  ) where

import           Data.Vector ( Vector )
import qualified Data.Vector as V
import           Data.List
                 ( nub
                 , sortBy
                 )
import           Data.Ord ( comparing )
import qualified Control.Monad as CM
import           Control.Monad.Primitive ( PrimMonad )

import           AI.MEP.Random
import           AI.MEP.Types
import           AI.MEP.Run ( evaluateChromosome )

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
    , c'ops :: Vector (F a)  -- ^ Functions pool with their symbolic
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

-- | A function to minimize.
--
-- The argument is a vector evaluation function whose input
-- is a vector (length @c'vars@) and ouput is
-- a vector with a different length @c'length@.
--
-- The result is a vector of the best indices
-- and a scalar loss value.
type LossFunction a =
  ((V.Vector a -> V.Vector a) -> (V.Vector Int, Double))

-- | Evaluates a chromosome according to the given
-- loss function.
phenotype
  :: Num a =>
     LossFunction a
     -> Chromosome a
     -> Phenotype a
phenotype loss chr = let (is, val) = loss (evaluateChromosome chr)
                     in (val, chr, is)

-- | Randomly generate a new population
initialize :: PrimMonad m => Config Double -> RandT m (Population Double)
initialize c@Config { c'popSize = size } = mapM (\_ -> newChromosome c) [1..size]

-- | Using 'LossFunction', find how fit is each chromosome in the population
evaluatePopulation
  :: Num a =>
     LossFunction a
     -> Population a
     -> Generation a
evaluatePopulation loss = map (phenotype loss)

-- | The best phenotype in the generation
best :: Generation a -> Phenotype a
best = last

-- | The worst phenotype in the generation
worst :: Generation a -> Phenotype a
worst = head

-- | Selection operator that produces the next evaluated population.
--
-- Standard algorithm: the best offspring O replaces the worst
-- individual W in the current population if O is better than W.
evolve
  :: PrimMonad m =>
     Config Double
     -- ^ Common configuration
     -> LossFunction Double
     -- ^ Custom loss function
     -> (Chromosome Double -> RandT m (Chromosome Double))
     -- ^ Mutation
     -> (Chromosome Double -> Chromosome Double -> RandT m (Chromosome Double, Chromosome Double))
     -- ^ Crossover
     -> (Generation Double -> RandT m (Chromosome Double))
     -- ^ A chromosome selection algorithm. Does not need to be random, but may be.
     -> Generation Double
     -- ^ Evaluated population
     -> RandT m (Generation Double)
     -- ^ New generation
evolve c loss mut cross select phenotypes = do
  let pc = p'crossover c
      pm = p'mutation c
      -- Sort in decreasing @val@ order so that
      -- the worst (with the biggest loss) is in the head
      sort' = sortBy (comparing (\(val, _, _) -> negate val))

      ev phen0 _ = do
        chr1 <- select phen0
        chr2 <- select phen0
        (of1, of2) <- withProbability pc (uncurry cross) (chr1, chr2)
        of1' <- withProbability pm mut of1
        of2' <- withProbability pm mut of2
        let r1@(val1, _, _) = phenotype loss of1'
            r2@(val2, _, _) = phenotype loss of2'
            (worstVal, _, _) = head phen0
            phen' | val1 < worstVal = r1 : tail phen0
                  | val2 < worstVal = r2 : tail phen0
                  -- No change
                  | otherwise = phen0
        let phen1 = sort' phen'
        return phen1

  CM.foldM ev (sort' phenotypes) [1..c'popSize c `div` 2]

-- | Binary tournament selection
binaryTournament :: (PrimMonad m, Ord a) => Generation a -> RandT m (Chromosome a)
binaryTournament phen = do
  (val1, cand1, _) <- drawFrom $ V.fromList phen
  (val2, cand2, _) <- drawFrom $ V.fromList phen
  if val1 < val2
    then return cand1
    else return cand2

-- | Uniform crossover operator
crossover :: PrimMonad m =>
  Chromosome a
  -> Chromosome a
  -> RandT m (Chromosome a, Chromosome a)
crossover ca cb = do
  r <- V.zipWithM (curry (swap 0.5)) ca cb
  return $ V.unzip r

swap :: PrimMonad m => Double -> (t, t) -> RandT m (t, t)
swap p = withProbability p (\(a, b) -> return (b, a))

replaceAt :: Int -> a -> Vector a -> Vector a
replaceAt i gene chr0 =
  let (c1, c2) = V.splitAt i chr0
  in c1 V.++ V.singleton gene V.++ V.tail c2

-- | Mutation operator with up to three mutations per chromosome
mutation3 :: PrimMonad m =>
  Config Double
  -- ^ Common configuration
  -> Chromosome Double
  -> RandT m (Chromosome Double)
mutation3 c chr = do
                                      -- Subtract 1 to get a non-zero head to
                                      -- replace
  is <- nub <$> CM.replicateM k (uniformIn_ (0, chrLen - 1))
  genes <- mapM new' is
  let chr' = foldr (uncurry replaceAt)
                   chr
                   (zip is genes)
  return chr'
    where chrLen = V.length chr
          k = 3
          new' = new (p'const c) (p'var c) (c'vars c) (c'ops c)

-- | Mutation operator with a fixed mutation probability
-- of each gene
smoothMutation
  :: PrimMonad m =>
     Double
     -- ^ Probability of gene mutation
     -> Config Double
     -- ^ Common configuration
     -> Chromosome Double
     -> RandT m (Chromosome Double)
smoothMutation p c chr =
  let new' = new (p'const c) (p'var c) (c'vars c) (c'ops c)
      mutate i = withProbability p (\_ -> new' i)
  in V.zipWithM mutate (V.enumFromN 0 (V.length chr)) chr

-- | Randomly initialize a new chromosome.
-- By definition, the first gene is terminal (a constant
-- or a variable).
newChromosome :: PrimMonad m =>
  Config Double          -- ^ Common configuration
  -> RandT m (Chromosome Double)
newChromosome c =
  V.mapM new' $ V.enumFromN 0 (c'length c)
    where new' = new (p'const c) (p'var c) (c'vars c) (c'ops c)

-- | Produce a new random gene
new :: PrimMonad m =>
  Double     -- ^ Probability to produce a constant
  -> Double  -- ^ Probability to produce a variable
  -> Int     -- ^ Number of input variables
  -> Vector (F Double)   -- ^ Operations vector
  -> Int                 -- ^ Maximal operation index
  -> RandT m (Gene Double Int)
new p1 p2 vars ops maxIndex = if maxIndex == 0
  -- The head must be a terminal
  -- p1' = p1 + (1 - p1 - p2) / 2 = 1/2 + p1/2 - p2/2
  then let p1' = 0.5 * (1 + p1 - p2)
       in newTerminal p1' vars
  else do
    p' <- double
    let sel | p' < p1 = newC
            | p' < (p1 + p2) = newVar vars
            | otherwise = newOp ops maxIndex
    sel

newTerminal :: (PrimMonad m, Floating a, Variate a) =>
  Double           -- ^ Probability @p@ of a constant generation.
                   -- @1-p@ will be the probability of a variable generation.
  -> Int           -- ^ Number of input variables
  -> RandT m (Gene a i)
newTerminal p vars = do
  p' <- double
  if p' < p
    then newC
    else newVar vars

-- | A randomly generated variable identifier
newVar :: PrimMonad m => Int -> RandT m (Gene a i)
newVar vars = Var <$> uniformIn_ (0, vars)

-- | A random operation from the operations vector
newOp :: PrimMonad m =>
  Vector (F a)
  -> Int
  -> RandT m (Gene a Int)
newOp ops maxIndex = do
  op <- drawFrom ops
  i1 <- uniformIn_ (0, maxIndex)
  i2 <- uniformIn_ (0, maxIndex)
  return $ Op op i1 i2

-- | Draw a constant from the uniform distribution within @(-0.5, 0.5]@
newC :: (PrimMonad m, Floating a, Variate a) => RandT m (Gene a i)
newC = C <$> uniformIn (-0.5, 0.5)
