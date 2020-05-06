{-
  | = A simple CLI application

  Generates an expression approximating an unknown
  one-dimensional function f(x) using data from an external file.

  File format: two comma-separated columns, x and f(x).

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

-- CLI options from `optparse-applicative`
import           Options.Applicative as Op
import           Data.Semigroup ( (<>) )
import qualified Data.Vector as V
-- CSV files from `cassava`
import           Data.Csv ( decode
                          , HasHeader (..)
                          )
import           Data.List ( foldl' )
import qualified Data.ByteString.Lazy as BS
import           Control.Monad ( foldM )
import           Text.Printf ( printf )
-- Random operations from `probable`
import           Math.Probable.Random ( double )

import           AI.MEP

-- | Absolute value distance between two scalar values
dist :: Double -> Double -> Double
dist x y = abs $ x - y

-- Functions available to genetically produced programs.
-- Modify this to your needs.
ops = V.fromList [ ('*', (*))
                 , ('+', (+))
                 ]

-- | CLI options are parsed to this data structure
data ProgOptions = ProgOptions
  { _inputFile :: FilePath
  , _chromosomeLength :: Int
  , _mutationProb :: Double
  , _varProb :: Double
  , _constProb :: Double
  , _popSize :: Int
  , _maxIter :: Int
  }

-- | CLI options parser
progOptions :: Parser ProgOptions
progOptions = ProgOptions
  -- Mandatory field (no default value)
  <$> strOption
      ( short 'f'
      <> metavar "<input file>"
      <> help "Input file path. Format: comma-separated, two columns." )
  <*> option auto
      ( long "length"
      <> short 'l'
      <> metavar "30"
      <> value 30
      <> help "Chromosome length" )
  <*> option auto
      ( long "mutation"
      <> short 'm'
      <> metavar "0.05"
      <> value 0.05
      <> help "Mutation probability" )
  <*> option auto
      ( long "var"
      <> short 'r'
      <> metavar "0.1"
      <> value 0.1
      <> help "Probability to generate a new variable gene" )
  <*> option auto
      ( long "const"
      <> short 'c'
      <> metavar "0.05"
      <> value 0.05
      <> help "Probability to generate a new constant gene" )
  <*> option auto
      ( long "population"
      <> short 'p'
      <> metavar "200"
      <> value 200
      <> help "Population size" )
  <*> option auto
      ( long "total"
      <> short 't'
      <> metavar "200"
      <> value 200
      <> help "Total number of iterations" )

main :: IO ()
main = Op.execParser opts >>= run
  where
    opts = Op.info (Op.helper <*> progOptions)
      ( fullDesc
     <> header "A CLI interface to Haskell multi expression programming" )

run :: ProgOptions -> IO ()
run arg = do
  let pVar = _varProb arg
      pConst = _constProb arg
      pMut = _mutationProb arg
      config = defaultConfig
        { c'ops = ops
        , c'length = _chromosomeLength arg
        , p'mutation = pMut
        , p'var = pVar
        , p'const = pConst
        , c'popSize = _popSize arg
        }
      maxIter = _maxIter arg

  let f = _inputFile arg

  putStrLn $ printf "Chromosome length: %d" (_chromosomeLength arg)
  putStrLn $ printf "Population size: %d" (_popSize arg)
  putStrLn $ "Mutation probability: " ++ show pMut
  putStrLn $ "Probability to generate a new variable gene: " ++ show pVar
  putStrLn $ "Probability to generate a new constant gene: " ++ show pConst
  let pNew = 1 - pConst - pVar
  putStrLn $ "Probability to generate a new operator: " ++ show pNew
  putStrLn ""

  putStrLn $ "Reading file " ++ f
  bs <- BS.readFile f
  let result = decode NoHeader bs :: Either String (V.Vector (Double, Double))
  case result of
    Left err -> error err
    Right dataset -> do
      let datasetSize = V.length dataset
      putStrLn $ printf "Fetched %d records\n" datasetSize

      -- Randomly create a population of chromosomes
      pop <- runRandIO $ initialize config

      let loss = regressionLoss1 dist (V.toList dataset)

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
      final <- foldM runIO popEvaluated [1..(maxIter `div` 5)]

      putStrLn "Interpreted expression:"
      putStrLn $ generateCode (best final)
