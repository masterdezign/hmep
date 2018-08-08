-- |
-- Here live the most common Genetic instances

module AI.MEP.Instances where

import           Data.Vector
import           Control.Monad.Primitive ( PrimMonad )

import           AI.MEP.Types
import           AI.MEP.Random


instance Genetic Double where
  newGene c = new (p'const c) (p'var c) (c'vars c) (c'ops c)

instance Genetic Float where
  newGene c = new (p'const c) (p'var c) (c'vars c) (c'ops c)

-- | Produce a new random gene
new :: (PrimMonad m, Floating a, Variate a) =>
  Double     -- ^ Probability to produce a constant
  -> Double  -- ^ Probability to produce a variable
  -> Int     -- ^ Number of input variables
  -> Vector (F a)   -- ^ Operations vector
  -> Int                 -- ^ Maximal operation index
  -> RandT m (Gene a Int)
new p1 p2 vars ops maxIndex = if maxIndex == 0
  -- The head must be a terminal
  -- p1' = p1 + (1 - p1 - p2) / 2 = 1/2 + p1/2 - p2/2
  then let p1' = 0.5 * (1 + p1 - p2)
       in newTerminal p1' vars
  else do
    p' <- double
    let sel | p' < p1 = newC
            | p' < (p1 + p2) = newVar vars
            | otherwise = newOp ops maxIndex
    sel

newTerminal :: (PrimMonad m, Floating a, Variate a) =>
  Double           -- ^ Probability @p@ of a constant generation.
                   -- @1-p@ will be the probability of a variable generation.
  -> Int           -- ^ Number of input variables
  -> RandT m (Gene a i)
newTerminal p vars = do
  p' <- double
  if p' < p
    then newC
    else newVar vars

-- | A randomly generated variable identifier
newVar :: PrimMonad m => Int -> RandT m (Gene a i)
newVar vars = Var <$> uniformIn_ (0, vars)

-- | A random operation from the operations vector
newOp :: PrimMonad m =>
  Vector (F a)
  -> Int
  -> RandT m (Gene a Int)
newOp ops maxIndex = do
  op <- drawFrom ops
  i1 <- uniformIn_ (0, maxIndex)
  i2 <- uniformIn_ (0, maxIndex)
  return $ Op op i1 i2

-- | Draw a constant from the uniform distribution within @(-0.5, 0.5]@
newC :: (PrimMonad m, Floating a, Variate a) => RandT m (Gene a i)
newC = C <$> uniformIn (-0.5, 0.5)
