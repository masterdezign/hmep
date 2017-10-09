module Main where

{-
  | = Sine approximation

  Generates an expression approximating sin(x) within the
  interval of [-pi;pi].

  Note: works the best when there is no division operation
  involved.
-}

import qualified Data.Vector as V
import           Data.List ( foldl' )
import           Control.Monad ( foldM )
import           Math.Probable.Random  -- From `probable` package
                 ( vectorOf
                 , double
                 )

import           AI.MEP

config = defaultConfig {
  -- Functions available to genetically produced programs
  c'ops = V.fromList [
       ('*', (*)),
       ('+', (+))
     ]
  -- Chromosome length
  , c'length = 50
  , p'mutation = 0.02
  -- Probability to generate a new variable gene
  , p'var = 0.1
  -- Probability to generate a new constant gene
  , p'const = 0.05
  -- Probability to generate a new operator is
  -- inferred as 1 - 0.1 - 0.05 = 0.85
  }

-- | Absolute value distance between two scalar values
dist :: Double -> Double -> Double
dist x y = abs $ x - y

-- Could be optimized
sum' :: Num a => [V.Vector a] -> V.Vector a
sum' xss = foldl' (V.zipWith (+)) base xss
  where
    len = V.length $ head xss
    base = V.replicate len 0

main :: IO ()
main = do
  -- A vector of 50 random numbers between 0 and 1 (including 1)
  xs <- runRandIO (vectorOf 50 double)

  -- Scale the values to the interval of (-pi, pi]
  let xs' = V.map ((2*pi *). subtract 0.5) xs
      -- Target function f to approximate
      function = sin
      -- Pairs (x, f(x))
      dataset = V.map (\x -> (x, function x)) xs'

  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  let -- The loss function which depends on the dataset
      loss evalf = (V.singleton i', loss')
        where
          (xs, ys) = unzip $ V.toList dataset
          -- Distances resulting from multiple expression evaluation
          dss = zipWith (\x y -> V.map (dist y). evalf. V.singleton $ x) xs ys
          -- Cumulative distances for each index
          dcumul = sum' dss
          -- Select index minimizing cumulative distances
          i' = V.minIndex dcumul
          -- The loss value with respect to the index of the best expression
          loss' = dcumul V.! i'

  -- Evaluate the initial population
  let popEvaluated = evaluateGeneration loss pop

      norm = fromIntegral $ V.length dataset

  putStrLn $ "Average loss in the initial population " ++ show (avgLoss popEvaluated / norm)

  -- Declare how to produce the new generation
  let nextGeneration = evolve config loss (mutation3 config) crossover binaryTournament

  -- Specify the I/O loop, which logs every 5 generation
      runIO pop i = do
        newPop <- runRandIO $ foldM (\xg _ -> nextGeneration xg) pop [1..generations]
        putStrLn $ "Population " ++ show (i * generations) ++ ": average loss " ++ show (avgLoss newPop / norm)
        return newPop
          where generations = 5

  -- The final population
  final <- foldM runIO popEvaluated [1..300]
  let best = last final

  putStrLn "Interpreted expression:"
  putStrLn $ generateCode best
