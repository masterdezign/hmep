module Main where

{-
  | = Example application: trigonometry cheating

  Find the trigonometric expression of cos(x) through sin(x)
  using our automatic programming method.
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
       ('+', (+)),
       -- Avoid division by zero
       ('/', \x y -> if y < 1e-6 then 1 else x / y),
       ('-', (-)),
       ('s', \x _ -> sin x)
     ]
  -- Chromosome length
  , c'length = 50
  -- Probability to generate a new variable gene
  , p'var = 0.1
  -- Probability to generate a new constant gene
  , p'const = 0.05
  -- Probability to generate a new operator is
  -- inferred as 1 - 0.1 - 0.5 = 0.85
  }

-- | Absolute value distance between two scalar values
dist :: Double -> Double -> Double
dist x y = abs $ x - y

main :: IO ()
main = do
  -- A vector of 50 random numbers between 0 and 1 (including 1)
  let datasetSize = 50
  xs <- runRandIO (vectorOf datasetSize double)

  -- Scale the values to the interval of (-pi, pi]
  let xs' = V.map ((2*pi *). subtract 0.5) xs
      -- Target function f to approximate
      function x = (cos x)^2
      -- Pairs (x, f(x))
      dataset = map (\x -> (x, function x)) $ V.toList xs'

  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  let loss = regressionLoss1 dist dataset

  -- Evaluate the initial population
  let popEvaluated = evaluatePopulation loss pop
      norm = fromIntegral datasetSize

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
  final <- foldM runIO popEvaluated [1..20]

  putStrLn "Interpreted expression:"
  putStrLn $ generateCode (best final)
