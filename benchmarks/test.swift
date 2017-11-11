import Foundation

var error:NSError?

let a = [-2, 7, 83, 2938]
let b = [0, 1, 2, 3]

func f(a: Int, b: Int) -> Int {
    return a + b
}

let n = zip(a, b).map { f(a: $0.0, b: $0.1) }.filter { $0 % 4 > 1 }.map { $0 * 2 }.every { $0 > 4 }
print("\(n)")

// proc example0: bool =
//   var n = zip(a, b) -->
//                 map(f(it[0], it[1])).
//                 filter(it mod 4 > 1).
//                 map(it * 2).
//                 all(it > 4)
//   result = n

// proc example1: bool =
//   var o = a -->
//             map(f(it, it)).
//             map(it - 7).
//             fold(0, it + a)
//   result = o > 0 # otherwise optimized


// var hack = false

// benchmark "example0":
//   hack = example0()
    
// echo hack

// benchmark "example1":
//   hack = example1()

// echo hack
// import zero_functional, strutils, sequtils, bench, times, os

// var data = readFile("data.txt")

// var a = data.split(" ").mapIt(parseInt(it))

// var b: seq[int] = @[]
// for z in 0..<2_000_000:
//   b.add(z)


// proc f(a: int, b: int): int =
//   a + b


// proc example0: bool =
//   var n = zip(a, b) -->
//                 map(f(it[0], it[1])).
//                 filter(it mod 4 > 1).
//                 map(it * 2).
//                 all(it > 4)
//   result = n

// proc example1: bool =
//   var o = a -->
//             map(f(it, it)).
//             map(it - 7).
//             fold(0, it + a)
//   result = o > 0 # otherwise optimized


// var hack = false

// benchmark "example0":
//   hack = example0()
    
// echo hack

// benchmark "example1":
//   hack = example1()

// echo hack
