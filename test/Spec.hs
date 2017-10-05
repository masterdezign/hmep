import           Test.HUnit
import           System.Exit ( exitSuccess
                             , exitFailure )
import qualified Data.Vector as V
import qualified Data.Map as M

import           AI.MEP.Types
import           AI.MEP.Run

o'mult :: Num a => F a
o'mult = ('*', (*))
{-# SPECIALIZE o'mult :: F Double #-}

-- Encodes x^8
pow8Int :: Chromosome Int
pow8Int = V.fromList [Var 'X', Op o'mult 0 0, Op o'mult 1 1, Op o'mult 2 2]

pow8 :: Chromosome Double
pow8 = V.fromList [Var 'X', Op o'mult 0 0, Op o'mult 1 1, Op o'mult 2 2]

testEvaluate = test ["test1" ~: "2^8" ~: 256 ~=? evaluate pow8Int (M.fromList [('X', 2 :: Int)]),
                     "test2" ~: "2.5^8" ~: 1525.87890625 ~=? evaluate pow8 (M.fromList [('X', 2.5 :: Double)])
                    ]

allTests = TestList [ testEvaluate ]

main :: IO ()
main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
