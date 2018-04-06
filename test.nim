import unittest, zero_functional, options, lists, macros, strutils

# different sequences
let a = @[2, 8, -4]
let b = @[0, 1, 2]
let c = @["zero", "one", "two"]

# different arrays
let aArray = [2, 8, -4]
let bArray = [0, 1, 2]
let cArray = ["zero", "one", "two"]


type 
  # Check working with enum type
  Suit {.pure.} = enum
    diamonds = (0, "D"),
    hearts = (1, "H"), 
    spades = (2, "S"), 
    clubs = (3, "C")

  ## User-defined that supports Iterator and random access. 
  Pack = ref object
    rows: seq[int]

  UsePack = ref object
    packs: seq[Pack]

  ShowPack = ref object
  
  ## same as Pack but without the `add` function
  PackWoAdd = ref object
    rows: seq[int]

  SimpleIter = ref object
    items: seq[int]
  
proc len(pack: Pack) : int = 
  pack.rows.len()
proc `[]`(pack: Pack, idx: int) : int  = 
  pack.rows[idx]
proc add(pack: Pack, t: int) = 
  pack.rows.add(t)
proc len(up: UsePack) : int = 
  up.packs.len()
proc `[]`(up: UsePack, idx: int) : Pack  = 
  up.packs[idx]
proc show(sp: ShowPack, pack: Pack): string = 
  $pack.rows
proc len(pack: PackWoAdd) : int = 
  pack.rows.len()
proc `[]`(pack: PackWoAdd, idx: int) : int  = 
  pack.rows[idx]
  
## init_zf is used to create the user-defined Pack item
proc init_zf(a: Pack): Pack =
  Pack(rows: @[])
proc init_zf(a: PackWoAdd): PackWoAdd =
  PackWoAdd(rows: @[])
proc initSimpleIter(): SimpleIter =
  SimpleIter(items: @[1,2,3])

proc len(si: SimpleIter) : int = 
  si.items.len()

iterator items(si: SimpleIter): int =
  for i in 0..<len(si.items):
    yield(si.items[i])

proc f(a: int, b: int): int =
  a + b

proc g(it: int): int =
  if it == 2:
    result = it + 2
  else:
    result = it + 1

## Macro that checks that the expression compiles
## Calls "check"
macro accept*(e: untyped): untyped =
  static: 
    assert(compiles(e))
  result = quote:
    if compiles(check(`e`)):
      check(`e`)
    else:
      discard

## Checks that the given expression is rejected by the compiler.
template reject*(e) =
  static: assert(not compiles(e))

## This is kind of "TODO" - when an expression does not compile due to a bug
## and it actually should compile, the expression may be surrounded with 
## `check_if_compile'. This macro will complain to use `check` when the expression
## actually gets compilable. 
macro check_if_compiles*(e: untyped): untyped =
  let content = repr(e)
  let msg = "[WARN]: Expression compiles. Use 'check' around '" & `content` & "'"

  result = quote:
    when compiles(check(`e`)):
      stderr.writeLine(`msg`)
      check(`e`)
    else:
      discard


suite "valid chains":

  test "basic filter":
    check(a --> filter(it > 0) == @[2, 8])

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

  test "map with changed type":
    check((a --> mapSeq($it)) == @["2", "8", "-4"])
  
  test "filter with exists":
    check(not (a --> filter(it > 2) --> exists(it == 4)))

  test "filter with index":
    check((a --> filter(it mod 2 == 0) --> index(it < 0)) == 2)

  test "foreach":
    var sum = 0
    a --> foreach(sum += it)
    check(sum == 6)

  test "foreach with index":
    var sum_until_it_gt_2 = 0
    check((a --> foreach(sum_until_it_gt_2 += it).index(it > 2)) == 1)
    check(sum_until_it_gt_2 == 10) # loop breaks when condition in index is true

  test "foreach change in-place":
    var my_seq = @[2,3,4]
    my_seq --> foreach(it = idx * it)
    check(my_seq == @[0,3,8])

  test "multiple methods":
    let n = zip(a, b) -->
      map(f(it[0], it[1])).
      filter(it mod 4 > 1).
      map(it * 2).
      all(it > 4)
    check(not n)

  test "zip with index":
    let n2 = zip(a, b) -->
      map(f(it[0], it[1])).
      filter(it mod 4 > 1).
      map(it * 2).
      index(it == 4)
    check(n2 == 0)

  test "zip with array":
    check((zip(aArray, bArray) --> map(it[0] + it[1])) == @[2, 9, -2])

  test "array basic filter":
    check((aArray --> filter(it > 0)) == [2, 8, 0])

  test "array basic zip":
    check((zip(aArray, bArray, cArray) --> filter(it[0] > 0 and it[2] == "one")) == @[(8, 1, "one")])

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
    check((aArray --> indexedMap(it)) == @[(0, 2), (1, 8), (2, -4)])
    check((aArray --> map(it + 2) --> indexedMap(it) --> map(it[0] + it[1])) == @[4, 11, 0])
  
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
    check((aArray --> filter(it mod 2 == 0) --> index(it < 0)) == 2)

  test "array foreach":
    var sum = 0
    aArray --> foreach(sum += it)
    check(sum == 6)

  test "array foreach with index":
    var sum_until_it_gt_2 = 0
    check((aArray --> foreach(sum_until_it_gt_2 += it) --> index(it > 2)) == 1)
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
    check((aArray --> map(it * 2) --> filterSeq(it > 0)) == @[4, 16])
    check((aArray --> map(it * 2) --> filter(it > 0)) == [4, 16, 0])

  test "array mapSeq":
    check((aArray --> map(it + 2) --> mapSeq(it * 2)) == @[8, 20, -4])

  test "array sub":
    check((aArray--> sub(1)) == [0, 8, -4])
    check((aArray --> sub(1,1)) == [0, 8, 0])
    check((aArray --> sub(1,^2)) == [0, 8, 0])

  test "array subSeq":
    check((aArray --> subSeq(1)) == @[8, -4])
    check((aArray --> subSeq(1,1)) == @[8])
    check((aArray --> subSeq(1,^2)) == @[8])
    
  test "array indexedMap":
    check((aArray --> map(it + 2) --> indexedMap(it) --> map(it[0] + it[1])) == @[4, 11, 0])

  test "seq filterSeq":
    check((a --> filterSeq(it > 0)) == @[2, 8])
    check((a --> filter(it > 0)) == @[2, 8])

  test "seq mapSeq":
    check((a --> mapSeq(it * 2)) == @[4, 16 , -8])

  test "seq indexedMap":
    check((a --> indexedMap(it) --> map(it[0] + it[1])) == @[2, 9, -2])

  test "seq sub":
    check((a --> filter(idx >= 1)) == @[8, -4])
    check((a --> sub(1)) == @[8, -4])
    check((a --> sub(1,1)) == @[8])
    check((a --> sub(1,^2)) == @[8])

  test "enum map":
    check((Suit --> map($it)) == @["D", "H", "S", "C"])

  test "enum filter":
    check((Suit --> filter($it == "H")) == @[Suit.hearts])

  test "enum find":
    check ((Suit --> find($it == "H")) == some(Suit.hearts))
    check ((Suit --> find($it == "X")) == none(Suit))

  test "multi ascending":
    template ascending(s: untyped) : bool = # check if the elements in seq or array are in ascending order
      s --> sub(1) --> all(s[idx]-s[idx-1] > 0)
      # alternative implementation: 
      # s --> all(idx == 0 or s[idx]-s[idx-1] > 0)
    check(ascending(a) == false)
    check(ascending(b) == true)
    check(ascending(aArray) == false)
    check(ascending(bArray) == true)

  test "filter template":
    let stuttered = @[0,1,1,2,2,2,3,3]
    let stutteredArr = [0,0,1,2,3,3]
    template destutter(s: untyped) : untyped =
      s --> filterSeq(idx == 0 or s[idx] != s[idx-1])
    check(destutter(stuttered) == @[0,1,2,3])
    check(destutter(stutteredArr) == @[0,1,2,3])

  test "generic filter":
    let p = Pack(rows: @[0,1,2,3])
    check((p --> filterSeq(it != 0)) == @[1,2,3]) 
    check((p --> filter(it != 0)).rows ==  @[1,2,3])
    
  test "empty":
    let e : seq[int] = @[]
    let res : seq[int] = @[]
    check((e --> all(false)) == true)
    check((e --> exists(false)) == false)
    check((e --> map(it * 2)) == res)
    check((e --> filter(it > 0) --> map(it * 2)) == res)

  test "flatten":
    let f = @[@[1,2,3],@[4,5],@[6]]
    check(f --> flatten() == @[1,2,3,4,5,6])
    let f2 = @[@["1","2","3"],@["4","5"],@["6"]]
    check((f2 --> flatten()) == @["1","2","3","4","5","6"])
    # indexedFlatten attaches the index of the element within the sub-list - that now has been flattened
    check(f --> indexedFlatten()            == @[(0,1),(1,2),(2,3),(0,4),(1,5),(0,6)])
    # this is not the same as:
    check(f --> flatten() --> map((idx,it)) == @[(0,1),(1,2),(2,3),(3,4),(4,5),(5,6)])

  test "flatten sum":
    check((@[a,b] --> flatten() --> fold(0, a + it)) == 9)

  test "zip flatten":
    check((zip(a,b) --> flatten()) == @[2,0,8,1,-4,2])

  test "change DoublyLinkedList in-place":
    var d = initDoublyLinkedList[int]()
    d.append(1)
    d.append(2)
    d.append(3)
    d --> foreach(it = it * 2) # change d in-place
    check((d --> filterSeq(it > 2)) == @[4, 6]) 
    check((d --> mapSeq(float(it) * 1.5)) == @[3.0, 6.0, 9.0])
  
  test "create DoublyLinkedList":
    var d = initDoublyLinkedList[int]()
    d.append(1)
    d.append(2)
    d.append(3)
    let e : DoublyLinkedList[string] = (d --> map(float(it) * 2.4) --> filter(it < 6.0) --> map($it))
    check((e --> map(it) --> to(seq[string])) == @["2.4", "4.8"])
    check((e --> map($it) --> to(seq)) == @["2.4", "4.8"])
    check((e --> map(it) --> to(seq)) == @["2.4", "4.8"])

  test "combinations":
    ## get indices of items where the difference of the elements is 1
    let items = @[1,5,2,9,8,3,11]
    # ----------- 0 1 2 3 4 5 6
    proc abs1(a: int, b: int) : bool = abs(a-b) == 1 
    let b = items -->
      combinations().
      filter(abs1(c.it[0], c.it[1])).
      map(c.idx) 
    check(b == @[[0, 2], [2, 5], [3, 4]])
    check(b --> all(abs1(items[it[0]], items[it[1]])))

    # the same again, but store it to a new list
    let c = items -->
      combinations().
      filter(abs1(c.it[0], c.it[1])).
      map(c.idx).
      to(list)
    
    # check that all items in the list and the seq are the same
    check(c --> all(it == b[idx]))

  test "rejected flatten":
    # some things are not possible or won't compile
    let fArray = [[1,2,3], [4,5,6]]
    let fList = fArray --> map(it) --> to(list)
    let fListFlattened = fList --> flatten() --> to(list)
    let fSeq = @[1,2,3,4,5,6]
    
    # flatten defaults to seq output if not explicitly set to the output format (except DoublyLinkedList)
    reject((fArray --> flatten()) == [1,2,3,4,5,6])
    accept((fArray --> flatten()) == fSeq)
    # array dimensions must be explicitly given
    reject((fArray --> flatten() --> to(array)) == [1,2,3,4,5,6]) 
    accept((fArray --> flatten() --> to(array[6,int])) == [1,2,3,4,5,6])
    accept((fArray --> flatten() --> to(array[8,int])) == [1,2,3,4,5,6,0,0]) # if array is too big, the array is filled with default zero

    reject((fList --> flatten()) == fSeq)
    # list is flattened to list by default
    # comparison of DoublyLinkedList does not seem to work directly...
    accept($(fList --> flatten()) == $fListFlattened)

  test "rejected missing add function":
    let p2 = PackWoAdd(rows: @[0,1,2,3])
    # PackWoAdd as iterable does not define the add method (or append) - hence this won't compile
    reject((p2 --> filter(it != 0)).rows ==  @[1,2,3]) 
    # forced to seq -> compiles
    accept((p2 --> filter(it != 0) --> to(seq)) == @[1,2,3])
    # also when using map which will lead to seq output
    accept((p2 --> filter(it != 0) --> map($it)) == @["1","2","3"])
    accept((p2 --> filter(it != 0) --> map(it)) == @[1,2,3])

  test "rejected wrong result type":
    # a contains int and cannot be mapped to seq[string] without $ operator
    reject((a --> filter(it > 2) --> to(seq[string])) == @["8"])

  test "rejected 'to' with an integral result type":
    reject(a --> exists(it < 0) --> to(list))
    accept(a --> map(it) --> to(seq) == a)

  test "SinglyLinkedList reversing elements":
    var l = a --> map(it) --> to(SinglyLinkedList)
    check(l --> map(it).to(seq) == @[-4,8,2])

  test "map with operator access":
    proc gg(): seq[string] =
      @["1","2","3"]
    
    check(gg() --> map(parseInt(it))[0] == 1)
    check(gg() --> map(parseInt(it)) --> map(1.5 * float(it))[2] == 4.5)
    check(a --> index(it == -4) + 1 == 3)
    check(@[@[1,2], @[3,4]] --> flatten() == @[1,2,3,4])
    check(@[11,2,7,3,4] --> combinations() --> filter(abs(c.it[1]-c.it[0]) == 1) --> map(c.idx) == @[[1,3],[3,4]])
    check(@[1,2,3] --> map($it) --> to(list) is DoublyLinkedList[string])
  
  test "simple iterator":
    # the type SimpleIter is restricted 
    # it does not define init_zf to initialize the type nor add (or append) to add elements
    # also the `[]=` operator is missing
    let si = initSimpleIter()
    let si2 = initSimpleIter()
    accept(si --> filter(it > 2) is seq[int]) 
    accept(si --> filter(it > 2) --> to(seq) == @[3])
    accept(si --> map($it) is seq[string]) # transformed to seq[string]
    accept(si --> map(it) == @[1,2,3])

    reject(si --> foreach(it = it * 2)) # foreach needs [] when changing elements
    var sum = 0
    si --> foreach(sum += it) # foreach without changing the content works however
    check(si --> reduce(it[0] + it[1]) == sum)
    accept(si --> fold(0, a + it) == 6)

    # on the other hand when converted to list or seq (or something with []) the list can be changed
    var d = si --> to(list)
    d --> foreach(it = it * 2)
    let e: DoublyLinkedList[int] = d
    discard(e) # just check it can be assigned
    accept(d --> to(seq) == @[2,4,6])

    reject(zip(si,si2) --> map($it)) # zip also needs the [] operator
    reject(si --> combinations() --> all(c.it[0] < c.it[1]))
    accept(d --> combinations() --> map(c.it) --> all(it[0] < it[1]))

  test "zip with simpleIter":
    let si = initSimpleIter()
    reject(zip(si, a) --> map(it[0]+it[1]) == @[3,10,-1]) # si needs an index
    accept(si --> map((it, a[idx])) -->  map(it[0]+it[1]) == @[3,10,-1]) # this will work
    reject(zip(a,si) --> map(it)) # si needs `[]` and high - we do that now...
    proc `[]`(si: SimpleIter, idx: int) : int = si.items[idx]
    proc `high`(si: SimpleIter) : int = si.items.high()
    check(zip(a,si) --> map(it[0]+it[1]) == @[3,10,-1])
    # when zipping with a shorter list, the result should also be a shorter list (that is where `high` is used)
    check(zip(@[3,2],si) --> map(it[0]+it[1]) == @[4,4])
    # same for a longer list
    check(zip([3,2,1,0],si) --> map(it[0]+it[1]) == @[4,4,4])
    

  test "foreach rejects":
    # changing elements in foreach will not work after the commands
    # map, indexedMap, combinations, flatten and zip
    # as they already create different collections
    var my_seq = @[2,3,5,7]
    var my_seq2 = @[1,2,3,4]
    my_seq --> foreach(it = it + 1)
    check(my_seq == @[3,4,6,8])
    reject(my_seq --> map(it) --> foreach(it = it + 1))
    reject(my_seq --> indexedMap(it) --> foreach(it[1] = it[1] + 1))
    reject(my_seq --> combinations() --> foreach(it = it + 1))
    reject(my_seq --> flatten() --> foreach(it = it + 1))
    reject(zip(my_seq,my_seq2) --> foreach(it[0] = it[0] + 1))
    check(my_seq == @[3,4,6,8])
    discard my_seq2

  test "closure parameters":
    # x is an illegal capture - so this will be rejected
    reject:
      proc chkVarError(x: var seq[int], y: int): seq[int] =
        result = x --> filter(it != y) 

    proc chkVar(x: var seq[int], y: int): seq[int] =
      let x = x # assigning x to a constant will work
      result = x --> filter(it != y)

    proc chkVarFor(x: var seq[int]) : int =
      var sum = 0  
      # foreach works here because it will not create an inner function 
      # only an if true: ... expression (that will create a new context for the variables)
      x --> foreach(sum += it) 
      return sum

    var s = @[1,2,3]
    check(chkVar(s, 2) == @[1,3])
    check(chkVarFor(s) == 6)

  test "complex type call":
    let sp = ShowPack()
    let p1 = Pack(rows: @[1,2,3])
    let p2 = Pack(rows: @[2,4,6])
    let up = UsePack(packs: @[p1, p2])
    accept(up --> map(sp.show(it)) is seq[string])
    # internally asserted, but initially a compiler problem
    reject(up --> map(sp.show(it)) --> to(list) is DoublyLinkedList[string])
    accept(up --> map(sp.show(it)) --> to(seq) is seq[string])
    accept(up --> map(sp.show(it)) --> to(list[string]) is DoublyLinkedList[string])
    accept(up --> map(sp.show(it)) --> to(seq[string]) is seq[string])

  test "dotExpr and function call on left side":
    proc testfun(res: seq[int],something:bool): seq[int] = 
      if something:
        return res
      return @[11]
    check(@[0,1,2].testfun(true) --> reduce(it[0]+it[1]) == 3)
    check(@[0,1,2].testfun(false) --> reduce(it[0]+it[1]) == 11)
    check(testfun(@[0,1,2],true) --> reduce(it[0]+it[1]) == 3)
    check(testfun(@[0,1,2],false) --> reduce(it[0]+it[1]) == 11)

  test "slice as input":
    check(0..<3 --> map($(it*it)) == @["0","1","4"])

  test "zip as first and in-between command":
    # there are a few combinations for zip, map and filter
    let a1 = @[1,-2,3,-4,5]
    let a2 = @[1,4,-2,-3,6]
    # first zip, then multiply with each other @[1,-8,6,-12,30], then filter > 0, then sum up
    check(zip(a1,a2) --> map(it[0]*it[1]) --> filter(it > 0) --> fold(0, a + it)         == 43)
    # internally zip(a1,a2) --> ... is already translated to a1 --> map((a1[idx],a2[idx])) which is roughly the same as  
    check(a1 --> map((a1[idx], a2[idx])) --> map(it[0]*it[1]) --> filter(it > 0) --> fold(0, a + it) == 43)
    # this is not the same - filtering the input seq for positive values only
    check(a1 --> filter(it > 0) --> zip(a2) --> map(it[0]*it[1]) --> fold(0, a + it)  == 25)
    
    # the right hand side of zip is more flexible - you could also use expressions with `it`:
    check(a1 --> filter(it > 0).
                zip(-1*it, a2).
                map(it[1]*it[2]). # it[0] is the list itself
                fold(0, a+it) == -25)
  
  test "subcommands of reduce":
    let arr = [3,11,2,9,1,8,7]
    # find (idx,min) value
    check(arr --> indexedMin() == (4,1))  
    check(arr --> sum() == 41)
    check(arr --> filter(it < 10) --> max() == 9)
    check(arr --> filter(it < 7) --> indexedMax() == (0,3))
    # sumIdx does not make much sense - here the index of the last added element 8 is 5, the sum is 28 
    check(arr --> filter(it > 7) --> indexedSum()  == (5,28))
    check(arr --> filter(it > 7) --> product() == 792)
  
  test "drop, take, dropWhile, takeWhile":
    # filter it > 15 => 16,17,..., sub(3) = 19,20,...
    check((11..222) --> filter(it > 15) --> sub(3,9) == @[19, 20, 21, 22, 23, 24, 25])
    # take 2 after take 10 - is actually the same as take 2 on the whole 
    check((11..222) --> take(10) --> take(2) --> sum() == 23)
    # here the filter does actually not count
    check((11..222) --> filter(it > 4) --> take(10) == @[11, 12, 13, 14, 15, 16, 17, 18, 19, 20])
    # drop 11,12 and take the next 5
    check((11..222) --> drop(2) --> take(5) == @[13, 14, 15, 16, 17])
    # drop 11..13, then drop the 1=14, then take until 26
    check((11..222) --> dropWhile((it mod 7) > 0).drop(1).takeWhile((it mod 13) != 1) == @[15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26])
    