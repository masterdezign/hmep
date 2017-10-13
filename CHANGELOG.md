# Changelog for [`hmep` package](http://hackage.haskell.org/package/hmep)

## 0.1.1 *October 13th 2017*
  * Improve code generation
  * Add `regressionLoss1` for 1D functions to the library
  * Add helpers `best`, `worst` working on Generation
  * Improve examples and documentation

## 0.1.0 *October 8th 2017*
  * Breaking changes:
    drop [monad-mersenne-random](http://hackage.haskell.org/package/monad-mersenne-random)
    which doesn't build with newer version of GHC. Instead, depend on the
    [probable](http://hackage.haskell.org/package/probable) package.

## 0.0.1 *October 7th 2017*
  * Improved demo: trigonometric identities solving example
  * Add `avgLoss` to the library
  * Fixes:
    * Change the crossover probability using Config.p'crossover parameter

## 0.0.0 *October 6th 2017*
  * Initial release
