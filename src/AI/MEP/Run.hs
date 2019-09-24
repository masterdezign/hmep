-- |
-- = Various utilities for running MEP algorithm

{-# LANGUAGE BangPatterns #-}

module AI.MEP.Run (
      generateCode
    , evaluateChromosome
    , regressionLoss1
    , avgLoss
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.List (
                   foldl'
                 , nub
                 , sort
                 )
import           System.IO.Unsafe ( unsafePerformIO )
import           Text.Printf

import           AI.MEP.Types

-- | Evaluate each subexpression in a chromosome
evaluateChromosome ::
         Chromosome a  -- ^ Chromosome to evaluate
         -> V.Vector a    -- ^ Variable values
         -> V.Vector a    -- ^ Resulting vector of multiple evaluations
evaluateChromosome chr vmap = unsafePerformIO $ do
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

-- | Generate code for the functions with a single output
generateCode :: Show a => Phenotype a -> String
generateCode (_, chr, i) = concat expr1 ++ expr2
  where
    -- A part of chromosome that is used (all genes ahead the `finalI`
    -- and the gene pointed by the `finalI`)
    chr' = V.slice 0 (finalI + 1) chr
    last' = chr' V.! finalI

    finalI = V.head i
    expr1 = map (\k -> _f (chr' V.! k) k). sort. nub $ _usedGeneIx chr'
    expr2 = printf "result = %s\n" $ _h last'

    _f (C c) _ = ""
    _f (Var i) _ = ""
    _f op k = printf "v%d = %s\n" k (_h op)

    _h (C c) = show c
    _h (Var i) = printf "x%d" i
    _h (Op (s, _) i1 i2) = if isInfix s
      then printf "%s %c %s" g1 s g2
      else printf "%c %s %s" s g1 g2
        where g1 = _g (chr' V.! i1) i1
              g2 = _g (chr' V.! i2) i2

    _g (C c) _ = show c
    _g (Var i) _ = printf "x%d" i
    _g Op {} k = printf "v%d" k

    -- Very naive infix operator check. No problem for single-character
    -- ASCII operator representations. Otherwise, please improve.
    isInfix x = x `notElem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- Active genes in case of a chromosome representing a single-output function.
-- Can be generalized to multiple outputs by several calls
-- changing `lastPos` as an argument.
_usedGeneIx :: Chromosome a -> [Int]
_usedGeneIx chr = foldl' _g base $ zip pos $ map (chr V.!) pos
  where
    -- Position indices
    pos = [lastPos - 1,lastPos - 2..0]

    _g xs (i, Op _ i1 i2) = if i `elem` xs
                           -- Next expressions depend on these
                           then i1: i2: xs
                           -- Dead gene, skip
                           else xs
    _g xs _ = xs  -- Terminal symbol, already counted

    base = case last' of
      (Op _ i1 i2) -> [i1, i2]
      _ -> []  -- Sadly, a terminal symbol

    last' = chr V.! lastPos
    lastPos = V.length chr - 1

-- | Loss function for regression problems with
-- one input and one output.
-- Not normalized with respect to the dataset size.
regressionLoss1
  :: (Num result, Ord result) =>
     (b -> b -> result)  -- ^ Distance function
     -> [(a, b)]         -- ^ Dataset
     -> (V.Vector a -> V.Vector b)
     -- ^ Chromosome evaluation function (partially applied 'evaluate')
     -> (V.Vector Int, result)
regressionLoss1 dist dataset evalf = (V.singleton i', loss')
  where
    (xs, ys) = unzip dataset
    -- Distances resulting from multiple expression evaluation
    dss = zipWith (\x y -> V.map (dist y). evalf. V.singleton $ x) xs ys
    -- Cumulative distances for each index
    dcumul = sum' dss
    -- Select index minimizing cumulative distances
    i' = V.minIndex dcumul
    -- The loss value with respect to the index of the best expression
    loss' = dcumul V.! i'
{-# SPECIALIZE
  regressionLoss1
    ::
      (Double -> Double -> Double)
      -> [(Double, Double)]
      -> (V.Vector Double -> V.Vector Double)
      -> (V.Vector Int, Double)
  #-}

-- Could be optimized
sum' :: Num a => [V.Vector a] -> V.Vector a
sum' xss = foldl' (V.zipWith (+)) base xss
  where
    len = V.length $ head xss
    base = V.replicate len 0
{-# SPECIALIZE sum' :: [V.Vector Double] -> V.Vector Double #-}

-- | Average population loss
avgLoss :: Generation Double -> Double
avgLoss = uncurry (/). foldl' (\(c, i) (val, _, _) -> (c + val, i + 1)) (0, 0)
