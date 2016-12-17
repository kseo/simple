simple
=======
An implementation of simply typed lambda calculus in Haskell

# Overview

`simple` is an implementation of simply typed lambda calculus presented by Benjamin C. Pierce in his [Types and Programming Languages][tapl]. I transliterated the original source code written in OCaml into Haskell except for the parser which I rewrote in [Parsec][parsec].

# Syntax

* Variables: x
* Applications: x x
* Lambda abstractions: \x.x
* Conditional: if x then y else z
* Booleans: true, false

# Implementation Details

* The representation of a variable is a number - its De Brujin index
* Evaluation performs substitution

# REPL

`simplei` is a REPL where you can input a lambda calculus term.

```
% \x:Bool.x
(\x:Bool.x)
$ (\x:Bool->Bool.x) true
parameter type mismatch at (1,19)
```

[parsec]: https://hackage.haskell.org/package/parsec
[tapl]: https://www.google.co.kr/search?q=tapl&oq=tapl&aqs=chrome..69i57j69i60l3j0l2.776j0j7&sourceid=chrome&ie=UTF-8#q=tapl+benjamin
