# Prim-Spoon

[![Build Status](https://travis-ci.org/michaeljklein/prim-spoon.png)](https://travis-ci.org/michaeljklein/prim-spoon)

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/ed4eb647c76f41f7bd004572502ebeda)](https://www.codacy.com/app/michaeljklein/prim-spoon?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=michaeljklein/prim-spoon&amp;utm_campaign=Badge_Grade)


On hackage as [prim-spoon](https://hackage.haskell.org/package/prim-spoon-0.1.0)

This is a microproject for the sole purpose of having a high-performance 
version of `teaspoon` from [Control.Spoon](http://hackage.haskell.org/package/spoon-0.3.1), using primops.
While it is unsafe, it is as safe as `catch#` and `seq#` from [GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.4.0.0/candidate/docs/GHC-Prim.html).
(I'd look into how safe those are, but they're hidden in GHC's source and that _really_ seems like overkill.)

Current [benchmarks](https://rawgit.com/michaeljklein/prim-spoon/master/benchmarks.html)
suggest that `primspoon` takes about 3x as long as a function call on non-error 
throwing values and about 5x as long to catch an error and return `Nothing`. 
This is an improvment over `teaspoon`, which takes about 4x as long as a
function call on a non-error value and about 21x to catch an error.

Why bother? Three reasons: 

1. This is my first experience with digging into primops in Haskell for performance, and it was a good exercise.
2. I had begun to roll my own solution before I found `Control.Spoon` and wanted to benchmark several different solutions (for example, `makeStableName`).
3. I was curious how `unsafePerformIO` affects performance. It turns out that it takes longer than I expected, but it makes some sense as it's not the best thing to spend time optimizing.
