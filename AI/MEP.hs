{- |
Copyright Bogdan Penkovsky (c) 2017

= Multiple Expression Programming

-}

module AI.MEP (
  Chromosome (..)
  , Gene
  , Population
  , Phenotype
  , Config (..)
  , defaultConfig
  , LossFunction

  -- * Genetic algorithm
  , initialize
  , evaluateGeneration
  , evolve
  , binaryTournament
  , crossover
  , mutation3
  , smoothMutation
  , newChromosome

  -- * Random
  , Rand
  , newPureMT
  , runRandom
  , evalRandom
  ) where

import System.Random.Mersenne.Pure64 ( newPureMT )

import AI.MEP.Types
import AI.MEP.Operators
import AI.MEP.Run
import AI.MEP.Random
