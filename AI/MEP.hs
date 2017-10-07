{- |
Copyright Bogdan Penkovsky (c) 2017

= Multiple Expression Programming

== Example application: trigonometry cheating

  Suppose, you forgot certain trigonometric identities.
  For instance, you want to express cos^2(x) using sin(x).
  No problem, set the target function cos^2(x) in the dataset
  and add sin to the arithmetic set of operators @{+,-,*,/}@.
  See app/Main.hs.

  After running

  @
  $ stack build && stack exec hmep-demo
  @

  We obtain

  @
  Average loss in the initial population 15.268705681244962
  Population 10: average loss 14.709728527360586
  Population 20: average loss 13.497114190675477
  Population 30: average loss 8.953185872653737
  Population 40: average loss 8.953185872653737
  Population 50: average loss 3.3219954564955856e-15
  @

  The value of 3.3e-15 is zero with respect to the
  rounding errors. It means that the exact expression was found!

  The produced output was:

  @
  Interpreted expression:
  v1 = sin x0
  v2 = v1 * v1
  result = 1 - v2
  @

  From here we can infer that
  @
  cos^2(x) = 1 - v2 = 1 - v1 * v1 = 1 - sin^2(x)
  @

  Sweet!

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
  , avgLoss
  , evolve
  , binaryTournament
  , crossover
  , mutation3
  , smoothMutation
  , newChromosome

  -- * Expression interpretation
  , generateCode

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
