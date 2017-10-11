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
  , p'mutation = 0.01
  , p'crossover = 0.9

  , c'length = 45
  , c'popSize = 20

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
boardToVec !b = let !r = V.fromList $ Seq.foldrWithIndex f [] b
                in r
  where
    f _ X xs = 1 : xs
    f _ O  xs = (-1) : xs
    f _ Neither xs = 0 : xs

boardToList :: Board -> [Player]
boardToList = toList

count x = length. filter (x ==)

outputs :: Int
outputs = 9

-- | Loss function computes
-- the total number of steps needed to win over N completely random players.
-- Lost games result in higher losses ^_^
loss :: [AIPlayer]  -- ^ Players to compete with
     -> (Vector Double -> Vector Double)   -- ^ Evaluation function
     -> (Vector Int, Double)  -- ^ The best indices and loss value
loss otherPlayers evalf = (is, loss')
  where
    loss' = fromIntegral $ round1 + round2

    winner' = winner emptyBoard
    player = (evalf, is)

    -- Player is X, odd player, + 1
    round1 = sum $ map (f X 1. winner' player) otherPlayers
    -- Player is O, even player, + 0
    round2 = sum $ map (\otherp -> f O 0 $ winner' otherp player) otherPlayers

    f me privilege (who, score) | who == me = score `div` 2 + privilege
                                -- Not too bad, but slightly worse
                                | who == Neither = 4 + privilege + 2
                                -- Bigger loss, nothing to say
                                | otherwise = 12

-- For simplicity, fix the last 9 expressions as target outputs
-- (can be or needs to be optimized).
is = V.enumFromN start outputs
  where start = c'length config - outputs

-- | Play using expressions encoded in chromosomes
nextMove' :: AIPlayer
  -> AIPlayer
  -> Board
  -> Board
nextMove' (evalfX, isX) (evalfO, isO) board = Seq.update idx' player' board
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
  case winner emptyBoard (evaluate cand1, is1) (evaluate cand2, is2) of
    (X, _) -> return cand1
    (O, _) -> return cand2
    -- Draw
    _ -> withProbability 0.5 (\_ -> return cand1) cand2

winner
  :: Board
     -> AIPlayer
     -> AIPlayer
     -> (Player, Int)
winner = gameRound (Nothing, 0)
  where
    gameRound !(Nothing, !cnt) board ev1 ev2 = let !board' = nextMove' ev1 ev2 board
                                               in gameRound (boardWinner board', cnt + 1) board' ev1 ev2
    gameRound !(Just !player, !cnt) _ _ _ = (player, cnt)

main :: IO ()
main = do
  -- Randomly create a population of chromosomes
  pop <- runRandIO $ initialize config

  -- A population of dummies against which loss is calculated.
  -- Later, this could be replaced with the best player to
  -- evaluate the future generations
  let testPlayersN = 5
  popTest <- runRandIO $ replicateM testPlayersN (newChromosome config)

  let loss' = loss (map (\chr -> (evaluate chr, is)) popTest)

  -- Evaluate the initial population
  let popEvaluated = evaluateGeneration loss' pop

  putStrLn $ "Average loss in the initial population " ++ show (avgLoss popEvaluated / fromIntegral testPlayersN)

  -- Declare how to produce the new generation
  let nextGeneration = evolve config loss' (mutation3 config) crossover tictacTournament

  -- Specify the I/O loop, which logs every generation
  let runIO pop i = do
        newPop <- runRandIO $ nextGeneration pop
        putStrLn $ "Population " ++ show i ++ ": average loss " ++ show (avgLoss newPop / fromIntegral testPlayersN)
        return newPop

  -- The final population
  final <- foldM runIO popEvaluated [1..2000]
  let best@(_, chr, is') = last final
  print best

  let aiPlayer = (evaluate chr, is')

  -- Play against itself :)
  let play = nextMove' aiPlayer aiPlayer

  print $ winner emptyBoard aiPlayer aiPlayer

  let boards = take 9 $ drop 1 $ iterate play emptyBoard
      s = unlines $ map (\b -> show b  ++ " winner: " ++ (show $ boardWinner b)) boards
  putStrLn s

  -- Play with you
  gameLoop' aiPlayer

type AIPlayer = (Vector Double -> Vector Double, Vector Int)

gameLoop' :: AIPlayer -> IO ()
gameLoop' aiPlayer = do
  boardWinner <- movesLoop' aiPlayer emptyBoard X
  putStrLn $ case boardWinner of
    X -> "X won!"
    O -> "O won!"
    Neither -> "It was a draw!"
  putStrLn "Play again y/n"
  answer <- getLine
  case answer of
    "y" -> gameLoop' aiPlayer
    _ -> return ()

movesLoop' :: AIPlayer -> Board -> Player -> IO Player
movesLoop' aiPlayer board currentPlayer = do
  board' <- case currentPlayer of
    X -> do putStrLn "\n==============="
            putStrLn "X's move:"
            let board' = nextMove' aiPlayer aiPlayer board
            putStr $ boardString board'
            return board'
    O -> do board' <- humanMove board
            putStrLn "The result of your move:"
            putStr $ boardString board'
            putStrLn ""
            return board'
  case boardWinner board' of
    Nothing -> movesLoop' aiPlayer board' $ oppositePlayer currentPlayer
    Just player -> return player
