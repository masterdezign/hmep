{- |
   This example looks for an expression
   to play the tic-tac-toe game.
 -}


import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           Data.Vector ( Vector )
import           Control.Monad
import           Data.Foldable ( toList )

-- The module used by the loss function
import           Tictactoe

import           AI.MEP


config = defaultConfig
  {
    p'const = 0.04
  , p'var = 0.15
  , p'mutation = 0.03
  , p'crossover = 0.9

  , c'length = 50
  , c'popSize = 100

  , c'ops = V.fromList [
       ('*', (*)),
       ('+', (+)),

       ('m', min),
       ('x', max)
     ]

  -- 9 input variables
  , c'vars = 9
  }

-- | Encode board as a vector of numbers
boardToVec :: Board -> Vector Double
boardToVec b = V.fromList $ Seq.foldlWithIndex f [] b
  where
    f xs _ X = 1 : xs
    f xs _ O = (-1) : xs
    f xs _ Neither = 0 : xs

boardToList :: Board -> [Player]
boardToList = toList

-- | All valid boards with at least one free position
allBoards :: [Board]
allBoards = map Seq.fromList. filter f $ all'
  where f xs = (count1 + count2) < 9 && (abs (count1 - count2) < 2)
          where count x = length $ filter (==x) xs
                count1 = count X
                count2 = count O
        all' = [x| x <- mapM (const [O, X, Neither]) [1..9]]

allBoardsN :: Int
allBoardsN = length allBoards

outputs :: Int
outputs = 9

-- | The loss function computes
-- differences between the moves
-- provided by MEP expressions and actual moves.
-- Here we use nine output variables.
--
-- For simplicity, fix the last 9 expressions
-- as target outputs.
loss :: [Board] -> (Vector Double -> Vector Double) -> (Vector Int, Double)
loss boards evalf = (V.enumFromN start outputs, loss')
  where
    evaluated = map (V.toList. V.slice start outputs. evalf. boardToVec) boards
    targets = map (V.toList. boardToVec. nextMove) allBoards

    loss' = sum $ zipWith3 err (map boardToList boards) evaluated targets
    start = c'length config - outputs

err :: [Player] -> [Double] -> [Double] -> Double
err players evals tgts = sum $ zipWith3 err' players evals tgts

-- | Evaluate only the outputs occupying previously empty squares
-- (not only valid moves because we do not check whose turn)
err' :: Player -> Double -> Double -> Double
err' Neither out target = if out == target then 0 else 1
err' _ out target = 0

-- Play using the obtained expression
-- nextMove' :: Phenotype Double -> Board -> Board

main :: IO ()
main = do
  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  let loss' = loss allBoards

  putStrLn $ "The number of considered boards " ++ show allBoardsN

  -- Evaluate the initial population
  let popEvaluated = evaluateGeneration loss' pop

  putStrLn $ "Average loss in the initial population " ++ show (avgLoss popEvaluated / fromIntegral allBoardsN)

  -- Declare how to produce the new generation
  let nextGeneration = evolve config loss' (mutation3 config) crossover binaryTournament

  -- Specify the I/O loop, which logs every generation
  let runIO pop i = do
        newPop <- runRandIO $ nextGeneration pop
        putStrLn $ "Population " ++ show i ++ ": average loss " ++ show (avgLoss newPop / fromIntegral allBoardsN)
        return newPop

  -- The final population
  final <- foldM runIO popEvaluated [1..1000]
  let best = last final
  print best
