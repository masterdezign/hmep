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
-- the total number of steps needed to win over N completely random players.
-- Lost games result in higher losses.
loss :: [Chromosome Double] -> (Vector Double -> Vector Double) -> (Vector Int, Double)
loss randomChrs evalf = (is, loss')
  where
    loss' = 0  -- sum $ ...
    nextMoveX = nextMove2 (evalf, is) (evaluate (head randomChrs), is)
    nextMoveO = nextMove2 (evaluate (head randomChrs), is) (evalf, is)

-- For simplicity, fix the last 9 expressions as target outputs
-- (can be or needs to be optimized).
is = V.enumFromN start outputs
  where start = c'length config - outputs

-- | Play using expressions encoded in chromosomes
nextMove2 :: (Vector Double -> Vector Double, Vector Int)
  -> (Vector Double -> Vector Double, Vector Int)
  -> Board
  -> Board
nextMove2 (evalfX, isX) (evalfO, isO) board = Seq.update idx' player' board
  where
    xMove = (count X $ boardToList board) == (count O $ boardToList board)

    (idx', player') | xMove = (V.maxIndex xIndices, X)
                    -- Assuming a valid board
                    | otherwise = (V.minIndex oIndices, O)

    xIndices = nextMoveIndices evalfX isX (-10)
    oIndices = nextMoveIndices evalfO isO 10

    nextMoveIndices evalf is' bias = moveCandidates'
      where
        prev = boardToVec board
        evaluated = evalf (boardToVec board)
        moveCandidates = V.map (evaluated V.!) is'
        -- Consider only valid moves (unoccupied squares):
        -- Use a bias to penalize selection of invalid moves.
        moveCandidates' = V.zipWith (\x y -> if x == 0 then y else bias) prev moveCandidates

-- | A ticktactoe tournament selection
tictacTournament :: PrimMonad m => Generation Double -> RandT m (Chromosome Double)
tictacTournament phens = do
  (_, cand1, is1) <- drawFrom $ V.fromList phens
  (_, cand2, is2) <- drawFrom $ V.fromList phens
  let compete Nothing board = let !board' = nextMove2 (evaluate cand1, is1) (evaluate cand2, is2) board
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

  let play = nextMove2 (evaluate chr1, is) (evaluate chr2, is)

  let boards = take 9 $ drop 1 $ iterate play emptyBoard
      s = unlines $ map (\b -> show b  ++ " winner: " ++ (show $ boardWinner b)) boards
  putStrLn s

mainMEP = do
  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  -- A constant population of dummies against which calculate loss
  popConst <- runRandIO $ replicateM 10 (newChromosome config)
  let loss' = loss popConst

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
