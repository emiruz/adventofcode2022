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
                    bifunctors-5.6.1 aeson ranges \
                    heaps
```

## Notes

* Day 16: Runs in 10s compiled. Can be made faster still 
by threading caching through the code at the cost of
complexity.

* Day 19: Runs in 20 seconds compiled. Pruning heuristic
is too pessimistic -- could be faster.

* Day 22: General implementation (no hard-coding). Runs in
2.5s. Orientation discovery assumes last instruction is a
movement and last two movements do not wrap: easy to
handle edge cases but tedious. This day was exceptionally
fiddly.
