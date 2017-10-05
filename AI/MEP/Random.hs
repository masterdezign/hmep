module AI.MEP.Random
    (
    -- * Utilities
    draw
    , getNormal
    , getMaxInt
    , withProbability

    -- * Re-exports
    , getBool, getInt, getWord, getDouble
    , runRandom, evalRandom
    , Rand, Random
    ) where

import Control.Monad.Mersenne.Random
import Data.Complex (Complex (..))
import System.Random
import Data.Vector as V

-- | Randomly draw an element from a vector
draw :: Vector a -> Rand a
draw xs =
  Rand $ \g -> let (n, g') = randomR (0, V.length xs - 1) g
                   r = xs V.! n
               in R r g'

-- | Modify value with probability @p@
withProbability
  :: Double         -- ^ The probability @p@
  -> (a -> Rand a)  -- ^ Modification function
  -> (a -> Rand a)
withProbability p modify x = do
  t <- getDouble
  if t < p
     then modify x
     else return x

-- | Randomly generate Int between 0 and @n@.
-- Should be strictly less than n if n > 1
-- or zero otherwise. Therefore, getMaxInt 1
-- should be always 0.
getMaxInt :: Int  -- ^ @n@
  -> Rand Int
getMaxInt n = do
  r <- getDouble
  return $ floor (r * fromIntegral n)

getNormal :: Rand Double
getNormal = do
  -- Box-Muller method
  u <- getDouble
  v <- getDouble
  let (c :+ s) = exp (0 :+ (2*pi*v))
  let r = sqrt $ (-2) * log u
  return $ r*c
