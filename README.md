# Multi Expression Programming

[![Build Status](https://travis-ci.org/masterdezign/hmep.svg?branch=master)](https://travis-ci.org/masterdezign/hmep)

You say, not enough Haskell machine learning libraries?

Here is yet another one!


## History

There exist many other Genetic Algorithm (GA) Haskell packages.
Personally I have used
[simple genetic algorithm](http://hackage.haskell.org/package/simple-genetic-algorithm-mr),
[GA](http://hackage.haskell.org/package/GA),
and [moo](http://hackage.haskell.org/package/moo) for quite a long time.
The last package was the most preferred, but the other two are
also great.

However, when I came up with this
[MEP paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.5.4352&rep=rep1&type=pdf),
to my surprise there was no MEP implementation in Haskell.
Soon I realized that existing GA packages are limited,
and it would be more efficient to implement MEP from scratch.

That is how this package was started. I also wish to say thank you
to the authors of the [moo](http://hackage.haskell.org/package/moo)
GA library, which inspired the present
[hmep](http://github.com/masterdezign/hmep) package.


## About MEP

Multi Expression Programming is a genetic programming variant encoding multiple
solutions in the same chromosome. A chromosome is a computer program.
Each gene is featuring [code reuse](https://en.wikipedia.org/wiki/Code_reuse).

How **MEP is different** from other genetic programming (GP) methods?
Consider a classical example of tree-based GP.
The number of nodes to encode `x^N`
using a binary tree is `2N-1`.
With MEP encoding, however, redundancies can be dramatically
diminished so that the
[shortest chromosome](https://github.com/masterdezign/hmep/blob/cd7b4976800d6c23ce5ebbe67f5ab5c9076229b9/test/Spec.hs#L18) 
that encodes the same expression has only `N/2` nodes!
That often results in significantly reduced computational costs
when evaluating MEP chromosomes. Moreover, **all** the intermediate
solutions such as `x^(N/2)`, `x^(N/4)`, etc. are provided by the
chromosome as well.

For more details, please check http://mepx.org/papers.html and
https://en.wikipedia.org/wiki/Multi_expression_programming.

### MEP in open source

  * By [Mihai Oltean](http://github.com/mepx), C++
  * By [Mark Chenoweth](https://github.com/markcheno/go-mep), Go
  * [Current project](https://github.com/masterdezign/hmep), Haskell

### The `hmep` Features

  * **Works out of the box**. You may use one of the elaborated
    [examples](https://github.com/masterdezign/hmep/blob/master/app/)
    to quickly tailor to your needs.
  * **Flexibility**. The [`hmep` package](https://github.com/masterdezign/hmep/)
    provides adjustable and composable building blocks such as
    [selection](https://hackage.haskell.org/package/hmep-0.1.0/docs/src/AI-MEP-Operators.html#binaryTournament),
    [mutation](https://hackage.haskell.org/package/hmep-0.1.0/docs/src/AI-MEP-Operators.html#smoothMutation)
    and [crossover](https://hackage.haskell.org/package/hmep-0.1.0/docs/src/AI-MEP-Operators.html#crossover)
    [operators](https://hackage.haskell.org/package/hmep-0.1.0/docs/AI-MEP.html).
    One is also free to use their own operators.
  * **Versatility**. `hmep` can be applied to solve regression problems with 
    one or multiple outputs. It means, you can approximate unknown functions
    or solve classification tasks. The only requirement is a custom
    [loss function](https://github.com/masterdezign/hmep/blob/b006eb8e0ca7c0540de979631423753bf0b66750/app/Main.hs#L67).


## Getting Started

Use [Stack](http://haskellstack.org).

     $ git clone https://github.com/masterdezign/hmep.git && cd hmep
     $ stack build --install-ghc

### CLI interface

```
A CLI interface to Haskell multi expression programming

Usage: hmep -f <input file> [-l|--length 30] [-m|--mutation 0.05] [-r|--var 0.1]
            [-c|--const 0.05] [-p|--population 200]

Available options:
  -h,--help                Show this help text
  -f <input file>          Input file path. Format: comma-separated, two
                           columns.
  -l,--length 30           Chromosome length
  -m,--mutation 0.05       Mutation probability
  -r,--var 0.1             Probability to generate a new variable gene
  -c,--const 0.05          Probability to generate a new constant gene
  -p,--population 200      Population size
```

Example: run for total of 200 algorithm iterations

     $ stack exec hmep -- -f data/sine.txt -t 200

```
Chromosome length: 30
Population size: 200
Mutation probability: 5.0e-2
Probability to generate a new variable gene: 0.1
Probability to generate a new constant gene: 5.0e-2
Probability to generate a new operator: 0.85

Reading file data/sine.txt
Fetched 50 records

Average loss in the initial population 0.6164572493880963
Population 5: average loss 0.36179141986463337
Population 10: average loss 0.35977590095295237
Population 15: average loss 0.3592976870934518
Population 20: average loss 0.35839623098861284
Population 25: average loss 0.35424451881439295
Population 30: average loss 0.31573374522629394
Population 35: average loss 0.1864152668405434
Population 40: average loss 8.966643495391169e-2
Population 45: average loss 8.522968243289145e-2
Population 50: average loss 8.522968243289145e-2
...
Population 200: average loss 5.51041829148264e-2
Interpreted expression:
v2 = x0 * x0
v4 = -0.12453785273085771 * x0
v5 = -0.12453785273085771 + v4
v6 = x0 * v4
v7 = v4 * -0.12453785273085771
v8 = v4 * v2
v12 = x0 + v5
v14 = v8 + x0
v19 = v8 * v7
v22 = v19 * v6
v23 = v22 * v12
result = v14 + v23
```

CLI application source is [here](app/CLI/Main.hs).


### Library Example 1

Now that the package is built, run the first demo to
express `cos^2(x)` through `sin(x)`:

     $ stack exec hmep-demo

     Average loss in the initial population 15.268705681244962
     Population 10: average loss 14.709728527360586
     Population 20: average loss 13.497114190675477
     Population 30: average loss 8.953185872653737
     Population 40: average loss 8.953185872653737
     Population 50: average loss 3.3219954564955856e-15

     Interpreted expression:
     v1 = sin x0
     v2 = v1 * v1
     result = 1 - v2

Effectively, the solution `cos^2(x) = 1 - sin^2(x)` was found.
Of course, MEP is a stochastic method, meaning that there is
no guarantee to find the globally optimal solution.

The unknown function approximation problem can be illustrated
by the following suboptimal solution for a given set of random
data points (blue crosses). This example was produced by another run of
the [same demo](app/Demo1/Main.hs), after 100 generations of 100 chromosomes
in each. The following expression was obtained
`y(x) = 3*0.31248786462471034 - sin(sin^2(x))`.
Interestingly, the approximating function lies symmetrically
in-between the extrema of the unknown function, approximately 
described by the blue crosses.

![Figure](https://github.com/masterdezign/hmep/blob/bbc2bdbac4fa3269c506455a473dddfa0e95231c/doc/Figures/cos2_approx.png)

### Library Example 2

A similar example is to approximate `sin(x)` using only
addition and multiplication operators, i.e. with polynomials.

     $ stack exec hmep-sin-approximation

The algorithm is able to automatically figure out the
powers of `x`. That is where MEP really shines. We [calculate](app/Demo2/Main.hs)
`c'length = 30` expressions represented by each chromosome gene practically with no
additional computational penalty. We choose the best expression among those 30
in _each_ chromosome of the population `c'popSize = 200`.
In this run, we have automatically obtained a
[seventh degree polynomial](https://github.com/masterdezign/hmep/blob/master/doc/sin_approx.py#L12)
coded by 14 genes. Pretty cool, huh?

```
    v1 = -5.936286355387799e-2 + -5.936286355387799e-2
    v4 = x0 + x0
    v5 = v1 * x0
    v7 = v4 * x0
    v8 = v1 * v5
    v9 = x0 * x0
    v10 = v8 * v9
    v11 = x0 * v10
    v15 = -5.936286355387799e-2 * x0
    v18 = v10 * v11
    v20 = v7 * v15
    v21 = v15 + x0
    v25 = v21 + v20
    result = v18 + v25
```

Which is `0.940637136446122*x - 0.118725727107756*x**3 + 0.000198691529073357*x**7`,
can be regarded as a hand-tuned version of `x - x^3/3! + x^7/7!` (the analytic expression
is `x - x^3/3! + x^5/5! - x^7/7!`).
That is impressive given that this is computed in fourteen steps!
Interestingly, we also observe that roughly half of expressions remain unused (e.g. v2, v3, v12...).


The result of approximation is [visualized](doc/sin_approx.py) below:

![Figure](https://github.com/masterdezign/hmep/blob/d173e96acf72e482474e657880f8bd28c40694e7/doc/Figures/sin_approx.png)

From the log below, one can also infer that obtained
approximation is better than analytical Taylor
sine expansions of 3rd and 5th orders. And naturally, is worse than the 7th order Taylor expansion:

     MEP expression: Average distance for 300 points: 0.0303
     3rd-order Taylor sine expansion: Average distance for 300 points: 0.3633
     5rd-order Taylor sine expansion: Average distance for 300 points: 0.0688
     7rd-order Taylor sine expansion: Average distance for 300 points: 0.0079


## Authors

This library is written and maintained by [Bogdan Penkovsky](http://penkovsky.com)
