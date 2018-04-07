import strutils, sequtils, bench, times, os

var data = readFile("data.txt")

var a = data.split(" ").mapIt(uint8(parseInt(it)))

var b: seq[uint32] = @[]
for z in 0'u32..<2_000_000'u32:
  b.add(z)


proc f(a: uint, b: uint): uint =
  a + b


proc example0: bool =
  var n = zip(a, b).
                mapIt(f(it[0], it[1])).
                filterIt(it mod 4 > 1'u).
                mapIt(it * 2'u).
                allIt(it > 4'u)
  result = n
callBench(example0)

proc example1: int =
  var o = a.
            mapIt(f(it, it)).
            mapIt(int(it) - 7).
            foldl(b + a, 0)
  result = o
callBench(example1)
