-- |
-- = Genetic operators

module AI.MEP.Operators (
  LossFunction
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
initialize :: (PrimMonad m, Genetic a) => Config a -> RandT m (Population a)
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
  :: (PrimMonad m, Genetic a, Num a) =>
     Config a
     -- ^ Common configuration
     -> LossFunction a
     -- ^ Custom loss function
     -> (Chromosome a -> RandT m (Chromosome a))
     -- ^ Mutation
     -> (Chromosome a -> Chromosome a -> RandT m (Chromosome a, Chromosome a))
     -- ^ Crossover
     -> (Generation a -> RandT m (Chromosome a))
     -- ^ A chromosome selection algorithm. Does not need to be random, but may be.
     -> Generation a
     -- ^ Evaluated population
     -> RandT m (Generation a)
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
mutation3
  :: (PrimMonad m, Genetic a) =>
     Config a
     -> Chromosome a
     -- ^ Common configuration
     -> RandT m (Chromosome a)
mutation3 c chr = do
                                      -- Subtract 1 to get a non-zero head to
                                      -- replace
  is <- nub <$> CM.replicateM k (uniformIn_ (0, chrLen - 1))
  genes <- mapM (newGene c) is
  let chr' = foldr (uncurry replaceAt)
                   chr
                   (zip is genes)
  return chr'
    where chrLen = V.length chr
          k = 3

-- | Mutation operator with a fixed mutation probability
-- of each gene
smoothMutation
  :: (PrimMonad m, Genetic a) =>
     Double
     -- ^ Probability of gene mutation
     -> Config a
     -- ^ Common configuration
     -> Chromosome a
     -> RandT m (Chromosome a)
smoothMutation p c chr =
  let mutate i = withProbability p (\_ -> newGene c i)
  in V.zipWithM mutate (V.enumFromN 0 (V.length chr)) chr
