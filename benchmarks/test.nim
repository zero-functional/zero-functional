import zero_functional, strutils, bench, times, os

let a = readFile("data.txt").split(" ") --> map(uint8(parseInt(it))) --> to(seq[uint8])
let b = 0'u32..<2_000_000'u32 --> to(seq[uint32])

proc f(a: uint, b: uint): uint =
  result = a + b

proc example0: bool =
  result = zip(a, b) -->
              map(f(it[0], it[1])).
              filter((it mod 4'u) > 1'u).
              map(it * 2'u8).
              all(it > 4'u)
callBench(example0)

proc example1: int =
  result = a -->
            map(f(it, it)).
            map(int(it) - 7).
            fold(0, it + a)
callBench(example1)
