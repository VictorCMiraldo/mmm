MMM
===

Prelude
-------

This work is funded by project [QAIS](http://wiki.di.uminho.pt/twiki/bin/view/Research/QAIS/WebHome).
Detailed information can be found in the Tech. Report.

Contents
========

MMM library
-----------

Here we provide a library for experimenting (and running) Monadic Mealy Machines in Haskell.
The generic library is inside the `MMM/Core` folder, with the following structure:

- `FuncComb`: Provides point-free combinators for functions.
- `CombComb`: Provides Components (MMMs) Combinators and a few auxiliary lemmas.
- `Probability`: Taken from 
  *Erwig, M., Kollmansberger, S.: Functional pearls: Probabilistic functional programming in Haskell. J. Funct. Program. 16, 21–34 (January 2006)*, provides (sub-)distribution functionality.
- `Distributive`: Has the encoding of a monad distributivity law (some authors refer to it as commutativity). We also prove that having a monad and a distributive law is the same as having a monad transformer. It is very usefull to be able to run components with nested monads.

The other two modules `Show` and `ListUtils` are pretty straight forward and provide just minor
haskell housekeeping code.

Some more theoretically inclined examples of the library are in `Examples/Theoretical`.

The files `HsMMM` and `Qais` are for inernal use only.

`oop2mmm` compiler
------------------

Following [this](https://github.com/VictorCMiraldo/mmm/blob/master/haslabtr201403.pdf) report,
we provide the code for our proof-of-concept compiler from a toy Object Oriented Language
to Monadic Mealy Machines. The resulting code requires the MMM library to run. The code
lives under `MMM/OOP`, and the cabal file on the repository root contains the compilation
instructions (for cabal!).

A few examples can be found under `Examples`. The `*.mmm` files are in the toy OO language
where the resulting components can be found under `Examples/Results`. 

Disclaimer
----------

This code is provided **as is**, and without any warranty whatsoever. Bug reports,
sugestions and forks are more than welcome, however. Forward your comments to
the author's [email](mailto:victor.cacciari@gmail.com).


