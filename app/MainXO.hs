{- |
   This example looks for an expression
   to play the tic-tac-toe game.
 -}

 {-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           Data.Vector ( Vector )
import           Control.Monad
import           Data.Foldable ( toList )
import           Control.Monad.Primitive ( PrimMonad )

-- The module used by the loss function
import           Tictactoe

import           AI.MEP
import           AI.MEP.Types ( Generation )
import           AI.MEP.Run ( evaluate )


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
boardToVec b = V.fromList $ Seq.foldrWithIndex f [] b
  where
    f _ X xs = 1 : xs
    f _ O  xs = (-1) : xs
    f _ Neither xs = 0 : xs

boardToList :: Board -> [Player]
boardToList = toList

-- | All valid boards with at least one free position
allBoards :: [Board]
allBoards = map Seq.fromList. filter f $ all'
  where f xs = (count1 + count2) < 9 && (abs (count1 - count2) < 2)
          where count1 = count X xs
                count2 = count O xs
        all' = [x| x <- mapM (const [O, X, Neither]) [1..9]]

allBoardsN :: Int
allBoardsN = length allBoards

count x = length. filter (x ==)

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
err' Neither out target = abs (out - target)
err' _ out target = 0

-- | Play using expressions encoded in chromosomes
nextMove' :: Phenotype Double -> Phenotype Double -> Board -> Board
nextMove' (_, chrX, isX) (_, chrO, isO) board = Seq.update idx' player' board
  where
    xMove = (count X $ boardToList board) == (count O $ boardToList board)

    (idx', player') | xMove = (V.maxIndex xIndices, X)
                    -- Assuming a valid board
                    | otherwise = (V.minIndex oIndices, O)

    xIndices = nextMoveIndices chrX isX (-10)
    oIndices = nextMoveIndices chrO isO 10

    nextMoveIndices chr' is' bias = moveCandidates'
      where
        prev = boardToVec board
        evaluated = evaluate chr' (boardToVec board)
        moveCandidates = V.map (evaluated V.!) is'
        -- Consider only valid moves (unoccupied squares):
        -- Use a bias to penalize selection of invalid moves.
        moveCandidates' = V.zipWith (\x y -> if x == 0 then y else bias) prev moveCandidates

-- | A ticktactoe tournament selection
tictacTournament :: PrimMonad m => Generation Double -> RandT m (Chromosome Double)
tictacTournament phens = do
  phen1@(_, cand1, _) <- drawFrom $ V.fromList phens
  phen2@(_, cand2, _) <- drawFrom $ V.fromList phens
  let compete Nothing board = let !board' = nextMove' phen1 phen2 board
                              in compete (boardWinner board') board'
      compete (Just player) _ = player
  case compete Nothing emptyBoard of
    X -> return cand1
    O -> return cand2
    Neither -> withProbability 0.5 (\_ -> return cand1) cand2

main :: IO ()
main = mainTest

mainTest = do
  pop <- runRandIO $ initialize config
  let chr1 = pop !! 0
  let chr2 = pop !! 15
  let phenotype loss chr = let (is, val) = loss (evaluate chr)
                           in (val, chr, is)
  let phen1 = phenotype (loss allBoards) chr1
  let phen2 = phenotype (loss allBoards) chr2
  let play = nextMove' phen1 phen2
  let boards = take 9 $ drop 1 $ iterate play emptyBoard
      s = unlines $ map (\b -> show b  ++ " winner: " ++ (show $ boardWinner b)) boards
  putStrLn s

mainMEP = do
  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  let loss' = loss allBoards

  putStrLn $ "The number of considered boards " ++ show allBoardsN

  -- Evaluate the initial population
  let popEvaluated = evaluateGeneration loss' pop

  putStrLn $ "Average loss in the initial population " ++ show (avgLoss popEvaluated / fromIntegral allBoardsN)

  -- Declare how to produce the new generation
  let nextGeneration = evolve config loss' (mutation3 config) crossover tictacTournament

  -- Specify the I/O loop, which logs every generation
  let runIO pop i = do
        newPop <- runRandIO $ nextGeneration pop
        putStrLn $ "Population " ++ show i ++ ": average loss " ++ show (avgLoss newPop / fromIntegral allBoardsN)
        return newPop

  -- The final population
  final <- foldM runIO popEvaluated [1..1000]
  let best = last final
  print best
