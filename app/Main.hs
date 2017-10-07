module Main where

{-
  | = Example application: trigonometry cheating

  Find the trigonometric expression of cos(x) through sin(x)
  using our automatic programming method.
-}

import qualified Data.Vector as V
import           Data.List ( foldl' )
import           Control.Monad ( foldM )
import           Numeric.LinearAlgebra
                 ( randomVector
                 , RandDist( Uniform )
                 , toList
                 )

import           AI.MEP

ops = V.fromList [('*', (*)), ('+', (+)), ('/', (/)), ('-', (-)),
  ('s', (\x _ -> sin x))]

config = defaultConfig {
  c'ops = ops
  , c'length = 50
  }

-- Feel free to change the random number generation seed
seed :: Int
seed = 3

randDomain :: Int -> [Double]
randDomain = map (subtract pi. (2*pi *)). toList. randomVector seed Uniform

dataset1 :: V.Vector (Double, Double)
dataset1 = V.map (\x -> (x, function x)) $ V.fromList $ randDomain nSamples
  where nSamples = 50
        function x = (cos x)^2

-- | Absolute value distance between two scalar values
dist :: Double -> Double -> Double
dist x y = if isNaN x || isNaN y
  -- Large distance
  then 10000
  else abs $ x - y

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

runIO (pop, g') i = do
  let (newPop, g2) = foldr (\_ xg -> run xg) (pop, g') [1..generations]
  putStrLn $ "Population " ++ show (i * generations) ++ ": average loss " ++ show (avgLoss newPop)
  return (newPop, g2)
    where
      run (x, g) = runRandom (nextGeneration x) g
      generations = 5

main :: IO ()
main = do
  g <- newPureMT
  let (pop, g') = runRandom (initialize config) g
      popEvaluated = evaluateGeneration loss pop
  putStrLn $ "Average loss in the initial population " ++ show (avgLoss popEvaluated)

  (final, _) <- foldM runIO (popEvaluated, g') [1..20]
  let best = last final
  print best
  putStrLn "Interpreted expression:"
  putStrLn $ generateCode best
