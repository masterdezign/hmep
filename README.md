# Multi Expression Programming

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
to my surprise there was no MEP realization in Haskell.
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
For more details, please check http://mepx.org/papers.html and
https://en.wikipedia.org/wiki/Multi_expression_programming.


## How to build

Use [Stack](http://haskellstack.org).

     $ git clone https://github.com/masterdezign/hmep.git && cd hmep
     $ stack build --install-ghc

Now, run the demo to calculate cos^2(x) through sin(x):

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


## Authors

This library is written and maintained by [Bogdan Penkovsky](http://penkovsky.com)
