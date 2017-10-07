{-# LANGUAGE BangPatterns #-}

module AI.MEP.Run where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           System.IO.Unsafe ( unsafePerformIO )
import           Text.Printf

import           AI.MEP.Types

-- | Evaluate each subexpression in a chromosome
evaluate :: Num a
         => Chromosome a  -- ^ Chromosome to evaluate
         -> V.Vector a    -- ^ Variable values
         -> V.Vector a    -- ^ Resulting vector of multiple evaluations
evaluate chr vmap = unsafePerformIO $ do
  -- Use dynamic programming to evaluate the chromosome
  v <- VM.new chrLen

  let -- Gene evaluation function
      _f (C c) _ = return c
      _f (Var n) _ = return $ vmap V.! n
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

  V.unsafeFreeze v
    where chrLen = V.length chr
{-# SPECIALIZE
  evaluate :: Chromosome Double
           -> V.Vector Double
           -> V.Vector Double #-}

-- | Generate code for the functions with a single output
generateCode :: Phenotype Double -> String
generateCode (_, chr, i) = concat expr1 ++ expr2
  where
    finalI = V.head i
    expr1 = map (\k -> _f (chr V.! k) k) [0..finalI - 1]
    expr2 = printf "result = %s\n" $ _h (chr V.! finalI)

    _f (Var i) _ = ""
    _f (C c) _ = ""
    _f op k = printf "v%d = %s\n" k (_h op)

    _h (C c) = show c
    _h (Var i) = printf "x%d" i
    _h (Op (s, _) i1 i2) = printf "%s %c %s" (_g (chr V.! i1) i1) s (_g (chr V.! i2) i2)

    _g (C c) _ = show c
    _g (Var i) _ = printf "x%d" i
    _g (Op _ _ _) k = printf "v%d" k
