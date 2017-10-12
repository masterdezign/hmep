-- |
-- = Random helpers

module AI.MEP.Random
    (
    -- * Utilities
    drawFrom
    , double_
    , uniformIn_
    , withProbability
    , runRandIO

    -- * Re-exports from Math.Probable.Random
    , RandT
    , double
    , vectorOf
    , vectorOfVariate
    , uniformIn

    -- * Re-exports from System.Random.MWC
    , Variate
    ) where

import Math.Probable.Random
import System.Random.MWC ( Variate )
import Control.Monad.Primitive ( PrimMonad )
import Data.Vector as V

-- | Alias for @mwc@:
-- Take a 'RandT' value and run it in 'IO', generating all the random values described by the 'RandT'.
--
-- It initializes the random number generator. For performance reasons, it is
-- recommended to minimize the number of calls to 'runRandIO'.
runRandIO :: RandT IO a -> IO a
runRandIO = mwc

-- | Randomly draw an element from a vector
drawFrom :: PrimMonad m => Vector a -> RandT m a
drawFrom vec = do
  n <- uniformIn (0, V.length vec - 1)
  return $ vec V.! n

-- | Similar to uniformIn, but using range
-- @[a, b)@ instead of @[a, b]@ and only for integral types.
uniformIn_ :: (PrimMonad m, Variate a, Integral a) => (a, a) -> RandT m a
uniformIn_ (a, b) = uniformIn (a, b - 1)

-- | Returns a double value from the range of @[0, 1)@.
-- If there is no specific reason, then prefer double @(0, 1]@.
double_ :: PrimMonad m => RandT m Double
double_ = subtract magicC <$> double
  where
    -- Change the range (0, 1] to [0, 1).
    -- http://hackage.haskell.org/package/mwc-random-0.13.6.0/docs/System-Random-MWC.html#v:uniform
    magicC = 2**(-53)

-- | Modify a value with the probability @p@
withProbability :: PrimMonad m =>
  Double               -- ^ The probability @p@
  -> (a -> RandT m a)  -- ^ Modification function
  -> (a -> RandT m a)
withProbability p modify x = do
  t <- double
  if t < p
     then modify x
     else return x
