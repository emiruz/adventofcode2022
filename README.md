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
                    bifunctors-5.6.1
```

## Notes

* Day 12: Lots of good options Dijkstra, A*, Bellman-Ford,
Floyd Warshall, and so on, but they are boring. I went for
a custom dynamic programming solution as a code golfing
exercise. Its slow (2.2 sec compiled, ~12 sec interpreted)
but mostly because it needs a mutable array since it does
a lot of updating.
