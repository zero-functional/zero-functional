import unittest, zero_functional, options

let a = @[2, 8, -4]
let b = @[0, 1, 2]
let c = @["zero", "one", "two"]

let aArray = [2, 8, -4]
let bArray = [0, 1, 2]
let cArray = ["zero", "one", "two"]

type 
  Suit {.pure.} = enum
    diamonds = (0, "D"),
    hearts = (1, "H"), 
    spades = (2, "S"), 
    clubs = (3, "C")
     
proc f(a: int, b: int): int =
  a + b

proc g(it: int): int =
  if it == 2:
    result = it + 2
  else:
    result = it + 1

suite "valid chains":
  test "basic filter":
    check((a --> filter(it > 0)) == @[2, 8])

  test "basic zip":
    check((zip(a, b, c) --> filter(it[0] > 0 and it[2] == "one")) == @[(8, 1, "one")])

  test "map":
    check((a --> map(it - 1)) == @[1, 7, -5])

  test "filter":
    check((a --> filter(it > 2)) == @[8])

  test "exists":
    check((a --> exists(it > 0)))

  test "all":
    check(not (a --> all(it > 0)))

  test "index":
    check((a --> index(it > 4)) == 1)

  test "find":
    check((a --> find(it > 2)) == some(8))
    check((a --> find(it mod 5 == 0)) == none(int))

  test "indexedMap":
    check((a --> indexedMap(it)) == @[(0, 2), (1, 8), (2, -4)])

  test "fold":
    check((a --> fold(0, a + it)) == 6)

  test "map with filter":
    check((a --> map(it + 2) --> filter(it mod 4 == 0)) == @[4])

  test "map with exists":
    check((a --> map(it + 2) --> exists(it mod 4 == 0)))

  test "map with all":
    check(not (a --> map(it + 2) --> all(it mod 4 == 0)))

  test "map with fold":
    check((a --> map(g(it)) --> fold(0, a + it)) == 10)

  test "filter with exists":
    check(not (a --> filter(it > 2) --> exists(it == 4)))

  test "filter with index":
    check((a --> filter(it mod 2 == 0).index(it < 0)) == 2)

  test "foreach":
    var sum = 0
    a --> foreach(sum += it)
    check(sum == 6)

  test "foreach with index":
    var sum_until_it_gt_2 = 0
    check((a --> foreach(sum_until_it_gt_2 += it).index(it > 2)) == 1)
    check(sum_until_it_gt_2 == 10) # loop breaks when condition in index is true

  test "foreach change in-place":
    var my_list = @[2,3,4]
    my_list --> foreach(it = idx * it)
    check(my_list == @[0,3,8])

  test "multiple methods":
    let n = zip(a, b) -->
      map(f(it[0], it[1])).
      filter(it mod 4 > 1).
      map(it * 2).
      all(it > 4)
    check(not n)

  test "zip with array":
    check((zip(aArray, bArray) --> map(it[0] + it[1])) == [2, 9, -2])

  test "array basic filter":
    check((aArray --> filter(it > 0)) == [2, 8, 0])

  test "array basic zip":
    check((zip(aArray, bArray, cArray) --> filter(it[0] > 0 and it[2] == "one")) == [(0, 0, nil), (8, 1, "one"), (0, 0, nil)])

  test "array map":
    check((aArray --> map(it - 1)) == [1, 7, -5])

  test "array filter":
    check((aArray --> filter(it > 2)) == [0, 8, 0])

  test "array filterSeq":
    check((aArray --> filterSeq(it > 2)) == @[8])

  test "array exists":
    check((aArray --> exists(it > 0)))

  test "array all":
    check(not (aArray --> all(it > 0)))

  test "array index":
    check((aArray --> index(it > 4)) == 1)

  test "array find":
    check((aArray --> find(it < 0)) == some(-4))
    check((aArray --> find(it mod 3 == 0)) == none(int))

  test "array indexedMap":
    check((aArray --> indexedMap(it)) == [(0, 2), (1, 8), (2, -4)])
  
  test "array fold":
    check((aArray --> fold(0, a + it)) == 6)

  test "array map with filter":
    check((aArray --> map(it + 2) --> filter(it mod 4 == 0)) == [4, 0, 0])

  test "array map with exists":
    check((aArray --> map(it + 2) --> exists(it mod 4 == 0)))

  test "array map with all":
    check(not (aArray --> map(it + 2) --> all(it mod 4 == 0)))

  test "array map with fold":
    check((aArray --> map(g(it)) --> fold(0, a + it)) == 10)

  test "array filter with exists":
    check(not (aArray --> filter(it > 2) --> exists(it == 4)))

  test "array filter with index":
    check((aArray --> filter(it mod 2 == 0).index(it < 0)) == 2)

  test "array foreach":
    var sum = 0
    aArray --> foreach(sum += it)
    check(sum == 6)

  test "array foreach with index":
    var sum_until_it_gt_2 = 0
    check((aArray --> foreach(sum_until_it_gt_2 += it).index(it > 2)) == 1)
    check(sum_until_it_gt_2 == 10) # loop breaks when condition in index is true
  
  test "array with foreach change in-place":
    var my_array = [2,3,4]
    my_array --> foreach(it = idx * it)
    check(my_array == [0,3,8])

  test "array":
    let n = zip(aArray, bArray) -->
      map(f(it[0], it[1])).
      filter(it mod 4 > 1).
      map(it * 2).
      all(it > 4)
    check(not n)

  test "array filterSeq":
    check((aArray --> map(it * 2).filterSeq(it > 0)) == @[4, 16])

  test "array mapSeq":
    check((aArray --> map(it + 2).mapSeq(it * 2)) == @[8, 20, -4])

  test "array indexedMapSeq":
    check((aArray --> map(it + 2).indexedMapSeq(it).map(it[0] + it[1])) == @[4, 11, 0])

  test "seq filterSeq":
    check((a --> filterSeq(it > 0)) == @[2, 8])

  test "seq mapSeq":
    check((a --> mapSeq(it * 2)) == @[4, 16 , -8])

  test "seq indexedMapSeq":
    check((a --> indexedMapSeq(it).map(it[0] + it[1])) == @[2, 9, -2])

  test "enum mapSeq":
    check((Suit --> mapSeq($it)) == @["D", "H", "S", "C"])

  test "enum find":
    check ((Suit --> find($it == "H")) == some(Suit.hearts))
    check ((Suit --> find($it == "X")) == none(Suit))
