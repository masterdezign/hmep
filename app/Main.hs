module Main where

import qualified Data.Vector as V
import           Data.Ord ( comparing )
import           Data.List
                 ( group
                 , sort
                 , sortBy
                 , foldl'
                 )
import           Numeric.LinearAlgebra
                 ( randomVector
                 , RandDist( Uniform )
                 , toList
                 )

import AI.MEP

ops = V.fromList [('*', (*)), ('+', (+)), ('/', (/)), ('-', (-))]

config = defaultConfig { c'ops = ops }

seed :: Int
seed = 3

randDomain :: Int -> [Double]
randDomain = map (subtract pi. (2*pi *)). toList. randomVector seed Uniform

dataset1 :: V.Vector (Double, Double)
dataset1 = V.map (\x -> (x, sin x)) $ V.fromList $ randDomain nSamples
  where nSamples = 50

dist x y = abs $ x - y

loss :: LossFunction Double
loss evalf = (V.singleton i', loss')
  where
    (xs, ys) = unzip $ V.toList dataset1
    -- Distances resulting from multiple expression evaluation
    dss = zipWith (\x y -> V.map (dist y). evalf. V.singleton $ x) xs ys
    -- Cumulative distances for each index
    dcumul = sum' dss
    -- Select index minimizing cumulative distances
    i' = V.minIndex dcumul
    -- The loss value with respect to the index of the best expression
    loss' = dcumul V.! i'

-- Could be optimized
sum' :: Num a => [V.Vector a] -> V.Vector a
sum' xss = foldl' (V.zipWith (+)) base xss
  where
    len = V.length $ head xss
    base = V.replicate len 0

nextGeneration
  :: [Phenotype Double] -> Rand [Phenotype Double]
nextGeneration = evolve config loss (mutation3 config) crossover binaryTournament

avgLoss :: [Phenotype Double] -> Double
avgLoss xs =
  let (r, len) = foldl' (\(c, i) (val, _, _) -> (c + val, i + 1)) (0, 0) xs
  in r / (fromIntegral len)

generations :: Int
generations = 50

main :: IO ()
main = do
  g <- newPureMT
  let (pop, g') = runRandom (initialize config) g
      popEvaluated = evaluateGeneration loss pop
  putStrLn $ "Average loss: " ++ show (avgLoss popEvaluated)

  let run (x, g) = runRandom (nextGeneration x) g
      (newPop, _) = foldr (\_ xg -> run xg) (popEvaluated, g') [1..generations]

  putStrLn $ "Average loss: " ++ show (avgLoss newPop)
