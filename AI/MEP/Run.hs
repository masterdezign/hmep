{-# LANGUAGE BangPatterns #-}

module AI.MEP.Run where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           System.IO.Unsafe ( unsafePerformIO )

import           AI.MEP.Types

evaluate :: Num a => Chromosome a -> M.Map Char a -> a
evaluate chr vmap = unsafePerformIO $ do
  let chrLen = V.length chr
  -- Use dynamic programming to evaluate the chromosome
  v <- VM.new chrLen

  let -- Gene evaluation function
      _f (C c) _ = return c
      _f (Var n) _ = return $ vmap M.! n
      _f (Op (_, f) i1 i2) v' = do
        !r1 <- v' `VM.read` i1
        !r2 <- v' `VM.read` i2
        let !r = f r1 r2
        return r

      -- Chromosome evaluation
      go !v' !j =
        if j == chrLen
           then return ()
           else do
             val <- _f (chr V.! j) v'
             VM.write v' j val
             go v' (j + 1)

  go v 0

  r <- v `VM.read` (chrLen - 1)
  return r
{-# SPECIALIZE
  evaluate :: Chromosome Double
           -> M.Map Char Double
           -> Double #-}
