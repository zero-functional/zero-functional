# speed

WORK IN PROGRESS JUST A BACKUP

```bash
nim -d:release --out:test_nim c test.nim
./test_nim

python3 test.py

rustc -O -o test_rust test.rs
./test_rust

ruby test.rb

mcs /optimize+ test.cs
mono test.exe

crystal build --release --no-debug -o test_crystal test.cr
./test_crystal

go run test.go

g++ -O3 -o test_cpp test.cpp -Iinclude
./test_cpp

node test.js
```


When only language name: then probably standard lib is used

lang | example0 | example1
------|-----|-----
Nim zero_functional | 12 | 1
Nim sequtils | 41 | 18
Python | 675 | 685
Ruby | 632 | 340
Rust | 10 | 3
C# Windows | 71 | 64
C# Mono | 335 | 268
Crystal | 31 | 4
Go | 85 | 26
C++ FunctionalPlus lib | 14 | 6
Node lodash lib | 4241 | 502

example0:

pos | lang | time
-----|-----|-----
1 | Rust                        | 10 ms
2 | Nim zero_functional lib     | 12 ms
3 | C++ FunctionalPlus lib      | 14 ms
4 | Crystal                     | 31 ms
5 | Nim standard lib            | 41 ms
6 | C# Windows                  | 71 ms
7 | Go                          | 85 ms
8 | C# Mono                     | 335 ms
9 | Ruby                        | 632 ms 
10 | Python3                    | 675 ms
11 | Node lodash lib            | 4241 ms

example1:

pos | lang | time
-----|-----|-----
1 | Nim zero_functional lib  | 1 ms
2 | Rust                     | 3 ms
3 | Crystal                  | 4 ms
4 | C++ FunctionalPlus lib   | 6 ms
5 | Nim standard lib         | 18 ms
6 | Go                       | 26 ms
7 | C# Windows               | 64 ms
8 | C# Mono                  | 268 ms
9 | Ruby                     | 340 ms
10 | Node lodash lib         | 502 ms
11 | Python3                 | 685 ms

Next:

Swift
Clojure
Elixir

Haskell
Julia
Java
Lua
F#


