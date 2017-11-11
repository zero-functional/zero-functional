import strutils, sequtils, bench, times, os

var data = readFile("data.txt")

var a = data.split(" ").mapIt(parseInt(it))

var b: seq[int] = @[]
for z in 0..<2_000_000:
  b.add(z)


proc f(a: int, b: int): int =
  a + b


proc example0: bool =
  var n = zip(a, b).
                mapIt(f(it[0], it[1])).
                filterIt(it mod 4 > 1).
                mapIt(it * 2).
                allIt(it > 4)
  result = n

proc example1: bool =
  var o = a.
            mapIt(f(it, it)).
            mapIt(it - 7).
            foldl(b + a, 0)
  result = o > 0 # otherwise optimized

var hack = false

benchmark "example0":
  hack = example0()
    
echo hack

benchmark "example1":
  hack = example1()

echo hack
