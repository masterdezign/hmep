import           Test.HUnit
import           System.Exit ( exitSuccess
                             , exitFailure )
import qualified Data.Vector as V

import           AI.MEP.Types
import           AI.MEP.Run

o'mult :: Num a => F a
o'mult = ('*', (*))
{-# SPECIALIZE o'mult :: F Double #-}

-- Encodes x, x^2, x^4, x^8
pow8Int :: Chromosome Int
pow8Int = V.fromList [Var 0, Op o'mult 0 0, Op o'mult 1 1, Op o'mult 2 2]

pow8 :: Chromosome Double
pow8 = V.fromList [Var 0, Op o'mult 0 0, Op o'mult 1 1, Op o'mult 2 2]

testEvaluate = test [
  "2^8" ~: V.fromList [2, 4, 16, 256] ~=? evaluate pow8Int (V.singleton (2 :: Int)),

  "2.5^8" ~: V.fromList [2.5,6.25,39.0625,1525.87890625] ~=? evaluate pow8 (V.singleton (2.5 :: Double))
  ]

allTests = TestList [ testEvaluate ]

main :: IO ()
main = do
  result <- runTestTT allTests
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
