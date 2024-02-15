# Advent of code 2022

The problems are solved in Haskell. Each has a main
function so that they can be compiled. Input is read
via stdin. All solutions start with a shebang so that
they can be executed directly on Unix based systems.

Interpreter executed example:
```
cat input.txt | ./01.hs
```

Compiled and then executed:
```
ghc 01.hs
cat input.txt | ./01
```

## Prerequisites

Please install the following packages. They help to
keep the code concise.
```
cabal install --lib lens regex-pcre containers \
                    bifunctors-5.6.1 aeson ranges
```

## Notes

* Day 16: Runs in 10 seconds compiled. Can be made faster
still by threading caching through the code at the cost
of complexity.

* Day 19: Runs in 20 seconds compiled. Pruning heuristic
is too pessimistic -- could be faster.

* Day 22: Just part 1 completed, part 2 is coming later.


## To do

* Day 22: I'm struggling to implement part 2 in < 100 LOC.
I'll come back to it at the end, once I figure out a way to 
implement it more elegantly. Meanwhile, part 1 is available.
