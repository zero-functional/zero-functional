import unittest, zero_functional, options, lists, macros, strutils, tables

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

proc len(pack: Pack): int =
  pack.rows.len()
proc `[]`(pack: Pack, idx: int): int =
  pack.rows[idx]
proc add(pack: Pack, t: int) =
  pack.rows.add(t)
proc len(up: UsePack): int =
  up.packs.len()
proc `[]`(up: UsePack, idx: int): Pack =
  up.packs[idx]
proc show(sp: ShowPack, pack: Pack): string =
  $pack.rows
proc len(pack: PackWoAdd): int =
  pack.rows.len()
proc `[]`(pack: PackWoAdd, idx: int): int =
  pack.rows[idx]

## zfInit is used to create the user-defined Pack item
proc zfInit(a: Pack): Pack =
  Pack(rows: @[])
proc zfInit(a: PackWoAdd): PackWoAdd =
  PackWoAdd(rows: @[])
proc initSimpleIter(): SimpleIter =
  SimpleIter(items: @[1, 2, 3])

proc len(si: SimpleIter): int =
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

## Own implementation of inc(num=1) command which adds num to the iterated.
## This could actually easily be done using `map(it+num)` but this shows an easy example of doing an own mapping.
zfInline inc(add = 1):
  loop: # code that is added inside the loop
    let it = it + add # create new iterator with `let it` and use the previous iterator `it` for it.

## Own implementation of filterNot(cond) command which is basically the opposite of filter.
## We try to implement it not refering to filter - just to show how an if condition is handled.
## The resulting generated node contains a yield statement which is used by the caller to insert
## the next commands in the chain.
zfInline filterNot(condition: bool):
  loop:
    if not condition:
      yield it
  # the yield statement marks the position where the next commands' code will be inserted.

## Own implementation of average command which calculates the arithmetic mean of
## sum / count - where count is the number of items and sum is the sum of all items.
zfInline average():
  # define used variables (and initialize the result) in the init section
  init:
    var countIdx = 0
    var sum = 0.0
  # executed for each item: increment count and calculate the sum
  loop:
    once:
      result = 0.0 # initialize the result... or infinity maybe? (0 / 0)
    countIdx += 1
    sum += float(it)
  # after the loop: calculate the result
  final:
    if countIdx > 0:
      result = sum / float(countIdx)

zfInline intersect(_):
# get all elements that are contained in all collections given as parameters
# this function is built similar to the test case below:
#   combinations(b,squaresPlusOne()).  
  pre:
    # first build the it[0] == it[1] and ... chain
    var chain = quote: true
    for i in (1..<ext.node.len):
      # ... and it[0] == it[i]
      chain = quote:
        `chain` and (it[0] == it[`i`]) # compare first element to all others

  delegate:
    # all arguments of intersect are delegated to combinations
    combinations(_)
    filter(chain)
    map(it[0])

zfInline intersectFast(_):
  # Faster implementation of intersect that only uses the `pre` section.
  # In the `pre` section the `ext.node` is filled with the generated code.
  # All necessary registrations will be done automatically.
  pre:
    var itIdent = ext.prevItNode()
    let lists = ext.node
    ext.node = nnkStmtList.newTree()
    var codePtr = ext.node

    # iterate over all collections (first collection already iterated)
    for idx in 1..lists.len - 1:
      let listRef = lists[idx]
      var prev = itIdent
      itIdent = ext.nextItNode()
      codePtr = codePtr.getStmtList().add quote do:
        for `itIdent` in `listRef`:
          if `itIdent` == `prev`:
            block: # must use block here because we need a statement list that replaces nil
              nil
            break

zfInline removeDoubles():
# remove double elements. Code taken from example "remove doublettes" below
  pre:
    let listRef = ext.listRef
  delegate:
    # this actually only works only on the original list / iterator
    indexedCombinations(listRef) # combine with itself - all elements
    # this is the tricky one: remove later elements that already are in the list
    # this actually translates in the inner for loop of combinations as:
    # if idx[0] > idx[1] and it[0] == it[1]: break
    takeWhile(not(it.elem[0] == it.elem[1] and it.idx[0] > it.idx[1]))
    # go back to the original elements
    filter(it.idx[0] == it.idx[1])
    map(it.elem[0])

proc inlineRemove*(ext: ExtNimNode) {.compileTime.} =
  ## remove function that removes elements from a collection
  ## two implementations - one for linked lists and one for indexable lists
  ## remove elements when the given condition is true - default value is true (removes all (filtered) elements)
  if ext.isListType():
    zfInlineCall remove(cond = true):
      pre:
        let listRef = ext.listRef
        let itList = newIdentNode(zfListIteratorName)
      loop:
        once:
          result = false
        if cond:
          listRef.remove(itList)
        result = true
  else:
    zfInlineCall remove(cond = true):
      pre:
        let listRef = ext.listRef
        ext.forceIndexLoop = true # delete is not supported when iterating over elements -> use index loop (seq modified while iterating over it)
        static:
          when not (listRef is FiniteIndexableLenIter):
            zfFail("Only index with len types supported for remove")
      loop:
        once:
          result = false
        if cond:
          result = true
          listRef.delete(idx)
          idx -= 1

## Registers the extensions for the user commands during compile time
macro registerExtension(): untyped =
  # register all extensions that have been defined with the zfInline macro
  zfCreateExtension()

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

template checkSame*(a: untyped, b: untyped): untyped =
  var idx = 0;
  for it in a:
    check(idx < len(b))
    check(abs(it - b[idx]) <= 1.192092895507812e-07)
    idx += 1

macro compilesMsg(e: untyped): untyped =
  let r = e.repr
  result = quote:
    warning("expression '$1' marked as `reject` did compile!" % `r`)

macro checkRejectMsg(e: untyped, msg: static[string], cmp: static[
    string]): untyped =
  result = quote:
    if `cmp` != `msg`:
      let errMsg = "reject check failed:\n expected: '$1',\n      got: '$2'" % [`msg`, `cmp`]
      warning(errMsg)
    else:
      discard

## Checks that the given expression is rejected by the compiler.
## When an assert (with msg) happens: the msg has to be the same.
template reject*(e: untyped, msg: static[string] = "") =
  static: # [sic!] - need several static sections here [why?!]
    when (compiles(e)):
      compilesMsg(e)
  static:
    checkRejectMsg(e, msg, zfGetLastFailure())
  discard

template reject2*(e: untyped, msg: static[string] = "") =
  # checking compiles(e) currently does not always work with nim!
  # so reject(...) cannot be used under some circumstances
  discard


## This is kind of "TODO" - when an expression does not compile due to a bug
## and it actually should compile, the expression may be surrounded with
## `checkIfCompiles'. This macro will complain to use `check` when the expression
## actually gets compilable.
macro checkIfCompiles*(e: untyped): untyped =
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

  test "map with different proc syntaxes":
    proc sub1(i: int): int = i - 1
    check((a --> map(proc(i: int): int = i - 1)) == @[1, 7, -5])
    check((a --> map(sub1)) == @[1, 7, -5])
    check((a --> map(sub1 it)) == @[1, 7, -5])
    check((a --> map(it.sub1)) == @[1, 7, -5])

  test "filter":
    check((a --> filter(it > 2)) == @[8])

  test "exists":
    check((a --> exists(it > 0)))

  test "exists with changed type":
    check((a --> map($it) --> exists(it.len > 0)))

  test "all":
    check(not (a --> all(it > 0)))

  test "all with changed type":
    check(a --> map(`$`) --> all(it.len > 0)) 

  test "index":
    check((a --> index(it > 4)) == 1)

  test "index with changed type":
    let d = @[1,22,333,444]
    check((d --> map($it) --> index(it.len > 2)) == 2)

  test "find":
    check((a --> find(it > 2)) == some(8))
    check((a --> find(it mod 5 == 0)) == none(int))

  test "indexedMap":
    check((a --> indexedMap(it)) == @[(0, 2), (1, 8), (2, -4)])

  test "fold":
    check((a --> fold(0, a + it)) == 6)

  test "fold with changed type":
    check((a --> map($it) --> fold("", a & it)) == "28-4")

  test "map with filter":
    check((a --> map(it + 2) --> filter(it mod 4 == 0)) == @[4])

  test "map with exists":
    check((a --> map(it + 2) --> exists(it mod 4 == 0)))

  test "map with all":
    check(not (a --> map(it + 2) --> all(it mod 4 == 0)))

  test "map with fold":
    check((a --> map(g(it)) --> fold(0, a + it)) == 10)
    check((a --> map(`$`) --> fold("", a & it)) == "28-4")    

  test "map with changed type":
    check((a --> mapSeq($it)) == @["2", "8", "-4"])

  test "find with changed type":
    check((a --> map(`$`) --> find(it == "-4")) == some("-4"))

  test "filter with exists":
    check(not (a --> filter(it > 2) --> exists(it == 4)))

  test "filter with index":
    check((a --> filter(it mod 2 == 0) --> index(it < 0)) == 2)

  test "foreach":
    var sum = 0
    a --> foreach(sum += it)
    check(sum == 6)

  test "foreach with index":
    var sumUntilItGt2 = 0
    check((a --> foreach(sumUntilItGt2 += it).index(it > 2)) == 1)
    check(sumUntilItGt2 == 10) # loop breaks when condition in index is true

  test "foreach change in-place":
    var mySeq = @[2, 3, 4]
    mySeq --> foreach(it = idx * it)
    check(mySeq == @[0, 3, 8])

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
    check((zip(aArray, bArray, cArray) --> filter(it[0] > 0 and it[2] ==
        "one")) == @[(8, 1, "one")])

  test "array map":
    # map creates a new iterator hence array cannot be used as output
    check((aArray --> map(it - 1)) == @[1, 7, -5])

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
    check((aArray --> indexedMap(it + 2) --> map(it.idx + it.elem)) ==
        @[4, 11, 0])
        
  test "array enumerate":
    check((aArray --> enumerate()) == @[(0, 2), (1, 8), (2, -4)])
    check((aArray --> enumerate() --> map(it.idx + it.elem + 2)) ==
        @[4, 11, 0])

  test "array fold":
    check((aArray --> fold(0, a + it)) == 6)

  test "array map with filter":
    # map forces seq output
    check((aArray --> map(it + 2) --> filter(it mod 4 == 0)) == @[4])

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
    var sumUntilItGt2 = 0
    check((aArray --> foreach(sumUntilItGt2 += it) --> index(it > 2)) == 1)
    check(sumUntilItGt2 == 10) # loop breaks when condition in index is true

  test "array with foreach change in-place":
    var myArray = [2, 3, 4]
    myArray --> foreach(it = idx * it)
    check(myArray == [0, 3, 8])

  test "array":
    let n = zip(aArray, bArray) -->
      map(f(it[0], it[1])).
      filter(it mod 4 > 1).
      map(it * 2).
      all(it > 4)
    check(not n)

  test "array filterSeq":
    check((aArray --> map(it * 2) --> filterSeq(it > 0)) == @[4, 16])
    check((aArray --> map(it * 2) --> filter(it > 0)) == @[4, 16])

  test "array mapSeq":
    check((aArray --> map(it + 2) --> mapSeq(it * 2)) == @[8, 20, -4])

  test "array sub":
    check((aArray --> sub(1)) == [0, 8, -4])
    check((aArray --> sub(1, 1)) == [0, 8, 0])
    check((aArray --> sub(1, ^2)) == [0, 8, 0])

  test "array subSeq":
    check((aArray --> subSeq(1)) == @[8, -4])
    check((aArray --> subSeq(1, 1)) == @[8])
    check((aArray --> subSeq(1, ^2)) == @[8])

  test "array indexedMap":
    check((aArray --> map(it + 2) --> indexedMap(it) -->
        map(it.idx + it.elem)) == @[4, 11, 0])

  test "seq filterSeq":
    check((a --> filterSeq(it > 0)) == @[2, 8])
    check((a --> filter(it > 0)) == @[2, 8])

  test "seq mapSeq":
    check((a --> mapSeq(it * 2)) == @[4, 16, -8])

  test "seq indexedMap":
    check((a --> indexedMap(it) --> map(it.idx + it.elem)) == @[2, 9, -2])

  test "seq sub":
    check((a --> filter(idx >= 1)) == @[8, -4])
    check((a --> sub(1)) == @[8, -4])
    check((a --> sub(1, 1)) == @[8])
    check((a --> sub(1, ^2)) == @[8])

  test "enum map":
    check((Suit --> map($it)) == @["D", "H", "S", "C"])

  test "enum filter":
    check((Suit --> filter($it == "H")) == @[Suit.hearts])

  test "enum find":
    check ((Suit --> find($it == "H")) == some(Suit.hearts))
    check ((Suit --> find($it == "X")) == none(Suit))

  test "multi ascending":
    template ascending(s: untyped): bool = # check if the elements in seq or array are in ascending order
      s --> sub(1) --> all(s[idx]-s[idx-1] > 0)
      # alternative implementation:
      # s --> all(idx == 0 or s[idx]-s[idx-1] > 0)
    check(ascending(a) == false)
    check(ascending(b) == true)
    check(ascending(aArray) == false)
    check(ascending(bArray) == true)

  test "filter template":
    let stuttered = @[0, 1, 1, 2, 2, 2, 3, 3]
    let stutteredArr = [0, 0, 1, 2, 3, 3]
    template destutter(s: untyped): untyped =
      s --> filterSeq(idx == 0 or s[idx] != s[idx-1])
    check(destutter(stuttered) == @[0, 1, 2, 3])
    check(destutter(stutteredArr) == @[0, 1, 2, 3])

  test "generic filter":
    let p = Pack(rows: @[0, 1, 2, 3])
    check((p --> filterSeq(it != 0)) == @[1, 2, 3])
    check((p --> filter(it != 0)).rows == @[1, 2, 3])

  test "empty":
    let e: seq[int] = @[]
    let res: seq[int] = @[]
    check((e --> all(false)) == true)
    check((e --> all(true)) == true)
    check((e --> exists(false)) == false)
    check((e --> exists(true)) == false)
    check((e --> find(false)) == none(int))
    check((e --> find(true)) == none(int))
    check((e --> index(false)) == -1)
    check((e --> index(true)) == -1)    
    check((e --> map(it * 2)) == res)
    check((e --> filter(it > 0) --> map(it * 2)) == res)

  test "empty with changed type":
    let e: seq[int] = @[]
    let res2: seq[string] = @[]
    check((e --> map($it) --> all(it == "123")) == true)
    check((e --> map($it) --> exists(it == "0")) == false)
    check((e --> map($it) --> find(it == "")) == none(string))
    check((e --> map($it) --> index(it != "123")) == -1)
    check((e --> map($it)) == res2)
    check((e --> map($it) --> filter(it.len > 0) --> map(it & it)) == res2)
 
  test "flatten":
    let f = @[@[1, 2, 3], @[4, 5], @[6]]
    check(f --> flatten() == @[1, 2, 3, 4, 5, 6])
    let f2 = @[@["1", "2", "3"], @["4", "5"], @["6"]]
    check((f2 --> flatten()) == @["1", "2", "3", "4", "5", "6"])
    # indexedFlatten attaches the index of the element within the sub-list - that now has been flattened
    check(f --> indexedFlatten() == @[(0, 1), (1, 2), (2, 3), (0, 4), (1, 5), (0, 6)])
    # this is not the same as:
    check(f --> flatten() --> map((idx, it)) == @[(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6)])

  test "flatten sum":
    check((@[a, b] --> flatten() --> fold(0, a + it)) == 9)

  test "zip flatten":
    check((zip(a, b) --> flatten()) == @[2, 0, 8, 1, -4, 2])

  test "change DoublyLinkedList in-place":
    var d = initDoublyLinkedList[int]()
    d.append(1)
    d.append(2)
    d.append(3)
    d --> foreach(it = it * 2) # change d in-place
    check((d --> filterSeq(it > 2)) == @[4, 6])
    checkSame((d --> mapSeq(float(it) * 1.5)), @[3.0f, 6.0f, 9.0f])

  test "create DoublyLinkedList":
    var d = initDoublyLinkedList[int]()
    d.append(1)
    d.append(2)
    d.append(3)
    let e: DoublyLinkedList[float] = (d --> map(float(it) * 2.5) --> filter(
        it < 6.0) --> to(list))
    checkSame(e, @[2.5, 5.0])

  test "combinations":
    ## get indices of items where the difference of the elements is 1
    let items = @[1, 5, 2, 9, 8, 3, 11]
    # ----------- 0..1..2..3..4..5..6
    #------------ ^.....^........^
    proc abs1(a: int, b: int): bool = abs(a-b) == 1
    let b = items -->
      indexedCombinations().
      filter(abs1(it.elem[0], it.elem[1])).
      map(it.idx)
    check(b == @[[0, 2], [2, 5], [3, 4]])
    check(b --> all(abs1(items[it[0]], items[it[1]])))

    # the same again, but store it to a new list
    let c = items -->
      indexedCombinations().
      filter(abs1(it.elem[0], it.elem[1])).
      map(it.idx).
      to(list)

    # check that all items in the list and the seq are the same
    check(c --> all(it == b[idx]))

    # check that only `2` is in both `a` and `b`
    let both = aArray -->
      combinations(bArray).  # build combinations with b
      map((itA, itB) = it).  # define the iterators a itA (from a) and itB
      filter(itA == itB).    # restrict to elements in both a and b
      map(itA)               # it is still ((itA, itB)) => restrict to itA
    check (both == @[2])

    # check indexedCombinations with 2 different collections
    check([11, 22] --> indexedCombinations([33, 44]) ==
      @[(idx: [0, 0], elem: [11, 33]), (idx: [0, 1], elem: [11, 44]), (idx: [1,
          0], elem: [22, 33]), (idx: [1, 1], elem: [22, 44])])

  test "rejected flatten":
    # some things are not possible or won't compile
    let fArray = [[1, 2, 3], [4, 5, 6]]
    let fList = fArray --> map(it) --> to(list)
    let fSeq = @[1, 2, 3, 4, 5, 6]

    # flatten defaults to seq output if not explicitly set to the output format (except DoublyLinkedList)
    let fRes: seq[int] = fArray --> flatten()
    accept(fRes == fSeq)
    # array dimensions must be explicitly given
    # comparison seq to array works now - but automatically converting to an array
    # needs the array size to prevent a runtime error overwriting the bounds.
    # reject(fArray --> flatten() --> to(array) == [1,2,3,4,5,6])
    check((fArray --> flatten() --> to(array[6, int])) == [1, 2, 3, 4, 5, 6])
    # if the array is too big, the array is filled with default zero
    check((fArray --> flatten() --> to(array[8, int])) == [1, 2, 3, 4, 5, 6, 0, 0])
    # if the array is too small we get a runtime error

    # list is flattened to seq by default
    accept((fList --> flatten()) == fSeq)

  test "rejected missing add function":
    let p2 = PackWoAdd(rows: @[0, 1, 2, 3])
    # PackWoAdd as iterable does not define the add method (or append) - hence this won't compile
    reject((p2 --> filter(it != 0)).rows == @[1, 2, 3],
            "Need either 'add' or 'append' implemented in 'PackWoAdd' to add elements")
    # forced to seq -> compiles
    accept((p2 --> filter(it != 0) --> to(seq)) == @[1, 2, 3])
    # also when using map will lead to seq output
    accept((p2 --> filter(it != 0) --> map($it)) == @["1", "2", "3"])
    accept((p2 --> filter(it != 0) --> map(it)) == @[1, 2, 3])

  test "rejected wrong result type":
    # a contains int and cannot be mapped to seq[string] without $ operator
    reject((a --> filter(it > 2) --> to(seq[string])) == @["8"],
            "Result type 'seq[string]' and added item of type 'int' do not match!")

  test "rejected 'to' with an integral result type":
    reject(a --> exists(it < 0) --> to(list), "'to' can only be used with list results - last arg is 'exists'")
    accept(a --> map(it) --> to(seq) == a)

  test "SinglyLinkedList reversing elements":
    var l = a --> map(it) --> to(SinglyLinkedList)
    check(l --> map(it).to(seq) == @[-4, 8, 2])
  
  test "map with operator access":
    proc gg(): seq[string] =
      @["1", "2", "3"]

    check(gg() --> map(parseInt(it))[0] == 1)
    check(gg() --> map(parseInt(it)) --> map(1.5 * float(it))[2] == 4.5)
    check(a --> index(it == -4) + 1 == 3)
    check(@[@[1, 2], @[3, 4]] --> flatten() == @[1, 2, 3, 4])
    check(@[11, 2, 7, 3, 4] --> indexedCombinations() -->
      map((index, item) = it) --> filter(abs(item[1]-item[0]) == 1) --> map(
          index) == @[[1, 3], [3, 4]])
    check(@[1, 2, 3] --> map($it) --> to(list) is DoublyLinkedList[string])

  test "simple iterator":
    # the type SimpleIter is restricted
    # it does not define zfInit to initialize the type nor add (or append) to add elements
    # also the `[]=` operator is missing
    let si = initSimpleIter()
    accept(si --> filter(it > 2) is seq[int])
    accept(si --> filter(it > 2) --> to(seq) == @[3])
    accept(si --> map($it) is seq[string]) # transformed to seq[string]
    accept(si --> map(it) == @[1, 2, 3])

    reject(si --> foreach(it = it * 2)) # foreach needs [] when changing elements
    var sum = 0
    si --> foreach(sum += it) # foreach without changing the content works however
    check(si --> reduce(it.accu + it.elem) == sum)
    accept(si --> fold(0, a + it) == 6)

    # on the other hand when converted to list or seq (or something with []) the list can be changed
    var d = si --> to(list)
    d --> foreach(it = it * 2)
    let e: DoublyLinkedList[int] = d
    discard(e) # just check it can be assigned
    accept(d --> to(seq) == @[2, 4, 6])

    reject(zip(si, si2) --> map($it) != nil,
        "need to provide an own implementation for mkIndexable(SimpleIter)") # zip also needs the [] operator
    reject(si --> combinations() --> all(it[0] < it[1]), "Only index with len types supported for combinations")
    accept(d --> combinations() --> all(it[0] < it[1]))

  test "zip with simpleIter":
    let si = initSimpleIter()
    # si needs access with []
    reject(zip(si, a) --> map(it[0]+it[1]) == @[3, 10, -1],
            "need to provide an own implementation for mkIndexable(SimpleIter)")
    accept(si --> map((it, a[idx])) --> map(it[0]+it[1]) == @[3, 10, -1]) # this will work
    # si needs `[]` and high - we do that now...
    reject(zip(a, si) --> map(it), "need to provide an own implementation for mkIndexable(SimpleIter)")
    proc `[]`(si: SimpleIter, idx: int): int = si.items[idx]
    proc `high`(si: SimpleIter): int = si.items.high()
    check(zip(a, si) --> map(it[0]+it[1]) == @[3, 10, -1])
    # when zipping with a shorter list, the result should also be a shorter list (that is where `high` is used)
    check(zip(@[3, 2], si) --> map(it[0]+it[1]) == @[4, 4])
    # same for a longer list
    check(zip([3, 2, 1, 0], si) --> map(it[0]+it[1]) == @[4, 4, 4])

  test "foreach rejects":
    # changing elements in foreach will not work after the commands
    # map, indexedMap, combinations, flatten and zip
    # as they already create different collections
    var mySeq = @[2, 3, 5, 7]
    var mySeq2 = @[1, 2, 3, 4]
    mySeq --> foreach(it = it + 1)
    check(mySeq == @[3, 4, 6, 8])
    const errMsg = "Adapted list cannot be changed in-place!"
    reject(mySeq --> map(it) --> foreach(it = it + 1), errMsg)
    reject(mySeq --> indexedMap(it) --> foreach(it[1] = it[1] + 1), errMsg)
    reject(mySeq --> flatten() --> foreach(it = it + 1), errMsg)
    reject(zip(mySeq, mySeq2) --> foreach(it[0] = it[0] + 1), errMsg)
    check(mySeq == @[3, 4, 6, 8])
    discard mySeq2

  test "closure parameters":
    # x is an illegal capture - so this will be rejected
    # currently this is not detected as a `reject` - but still is rejected by the compiler
    reject2:
      proc chkVarError(x: var seq[int], y: int): seq[int] =
        result = x --> filter(it != y)

    proc chkVar(x: var seq[int], y: int): seq[int] =
      let x = x # assigning x to a constant will work
      result = x --> filter(it != y)

    proc chkVarFor(x: var seq[int]): int =
      var sum = 0
      # foreach works here because it will not create an inner function
      # only an if true: ... expression (that will create a new context for the variables)
      x --> foreach(sum += it)
      return sum

    proc chkConversion(x: seq[int]): seq[string] =
      result = x --> map($it)

    var s = @[1, 2, 3]
    check(chkVar(s, 2) == @[1, 3])
    check(chkVarFor(s) == 6)
    check(chkConversion(s) == @["1", "2", "3"])

  test "complex type call":
    let sp = ShowPack()
    let p1 = Pack(rows: @[1, 2, 3])
    let p2 = Pack(rows: @[2, 4, 6])
    let up = UsePack(packs: @[p1, p2])
    accept(up --> map(sp.show(it)) is seq[string])
    accept(up --> map(sp.show(it)) --> to(list) is DoublyLinkedList[string])
    accept(up --> map(sp.show(it)) --> to(seq) is seq[string])
    accept(up --> map(sp.show(it)) --> to(list[string]) is DoublyLinkedList[string])
    accept(up --> map(sp.show(it)) --> to(seq[string]) is seq[string])

  test "dotExpr and function call on left side":
    proc testfun(res: seq[int], something: bool): seq[int] =
      if something:
        return res
      return @[11]
    check(@[0, 1, 2].testfun(true) --> reduce(it.accu + it.elem) == 3)
    check(@[0, 1, 2].testfun(false) --> reduce(it[0]+it[1]) == 11)
    check(testfun(@[0, 1, 2], true) --> reduce(it[0]+it[1]) == 3)
    check(testfun(@[0, 1, 2], false) --> reduce(it[0]+it[1]) == 11)

  test "slice as input":
    check(0..<3 --> map($(it*it)) == @["0", "1", "4"])

  test "zip as first and in-between command":
    # there are a few combinations for zip, map and filter
    let a1 = @[1, -2, 3, -4, 5]
    let a2 = @[1, 4, -2, -3, 6]
    # first zip, then multiply with each other @[1,-8,6,-12,30], then filter > 0, then sum up
    check(zip(a1, a2) --> map(it[0]*it[1]) --> filter(it > 0) --> fold(0, a +
        it) == 43)
    # internally zip(a1,a2) --> ... is already translated to a1 --> map((a1[idx],a2[idx])) which is roughly the same as
    check(a1 --> map((a1[idx], a2[idx])) --> map(it[0]*it[1]) --> filter(it >
        0) --> fold(0, a + it) == 43)
    # this is not the same - filtering the input seq for positive values only
    check(a1 --> filter(it > 0) --> zip(a2) --> map(it[0]*it[1]) --> fold(0, a +
        it) == 25)

    # the right hand side of zip is more flexible - you could also use expressions with `it`:
    check(a1 --> filter(it > 0).
      zip(-1*it, a2).
      map(it[1]*it[2]). # it[0] is the list itself
      fold(0, a+it) == -25)

  test "subcommands of reduce":
    let arr = [3, 11, 2, 9, 1, 8, 7]
    # find (idx,min) value
    check(arr --> indexedMin() == (4, 1))
    check(arr --> sum() == 41)
    check(arr --> filter(it < 10) --> max() == 9)
    check(arr --> filter(it < 7) --> indexedMax() == (0, 3))
    check(arr --> filter(it < -1) --> indexedMax() == (-1, 0))
    # sumIdx does not make much sense - here the index of the last added element 8 is 5, the sum is 28
    check(arr --> filter(it > 7) --> indexedSum() == (5, 28))
    check(arr --> filter(it > 7) --> product() == 792)

  test "drop, take, dropWhile, takeWhile":
    # filter it > 15 => 16,17,..., sub(3) = 19,20,...
    check((11..222) --> filter(it > 15) --> sub(3, 9) == @[19, 20, 21, 22, 23,
        24, 25])
    # take 2 after take 10 - is actually the same as take 2 on the whole
    check((11..222) --> take(10) --> take(2) --> sum() == 23)
    # here the filter does actually not count
    check((11..222) --> filter(it > 4) --> take(10) == @[11, 12, 13, 14, 15, 16,
        17, 18, 19, 20])
    # drop 11,12 and take the next 5
    check((11..222) --> drop(2) --> take(5) == @[13, 14, 15, 16, 17])
    # drop 11..13, then drop the 1=14, then take until 26
    check((11..222) --> dropWhile((it mod 7) > 0).drop(1).takeWhile((
        it mod 13) != 1) == @[15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26])

  ## Creates an inline iterator as in-between result.
  ## This iterator cannot be moved around, but is useful to save intermediate results.
  test "create iterator function":
    type Person = ref object
      name: string
      height: int
    let inputData = [{"name": "Mary", "height": "162"},
                      {"name": "Hans", "height": "184"},
                      {"name": "Jonas", "height": "0"},
                      {"name": "Wanda", "height": "136"},
                      {"name": "Joey", "height": "158"}]
    inputData -->
      map(it.newTable).
      map(Person(name: it["name"], height: parseInt(it["height"]))).
      createIter(persons) # creates an iterator function named persons
    # now create the heights which is needed twice to calculate its average value
    persons() -->
      map(it.height).
      filter(it > 0).
      createIter(heights)
    # the heights() can now be accessed several times (to determine the count and the sum of heights)
    let count = heights() --> count()
    let averageHeight = if (count == 0): float(0) else: float((heights() -->
        sum()) / count)
    check(count == 4) # only 4 valid height values
    check(averageHeight == 160.0)

    (1..3) --> map(it) --> createIter(a)
    check(a() --> map(it) == @[1, 2, 3])
    # the result type cannot be guessed automatically - so set it explicitly
    accept(a() --> to(seq[int]) == @[1, 2, 3])

  ## Combines several collections to build an intersection.
  test "combinations of several collections":
    (1..10) --> map(2*it-1) --> createIter(a) # seq of odd numbers 1..19
    let b = @[1, 2, 3, 5, 7, 11, 13, 17, 19]         # some prime numbers
    (1..10) --> map(it*it+1) --> createIter(squaresPlusOne)
    # create an iterfunction squaresPlusOne - don't forget to add () !

    let intersect =
      a() --> combinations(b, squaresPlusOne()). # combine all elements of a,b and squarePlusOne
        # get all combinations where the elements of each collection are equal
        filter(it[0] == it[1] and it[0] == it[2]).  
        map(it[0]).  # and use the first element
        to(seq[int]) # output type with a() on left side has to be supplied
    check(intersect == @[5, 17])

  ## Remove double entries from a collection. Could be used to extend `intersect`.
  test "complex function remove doublettes":
    let a = @[1, 2, 1, 1, 3, 2, 1, 1, 4]
    check(a --> indexedCombinations(a). # combine with itself - all elements
              # this is the tricky one: remove later elements that already are in the list
              # this actually translates in the inner for loop of combinations as:
              # if idx[0] > idx[1] and it[0] == it[1]: break
      takeWhile(not(it.idx[0] > it.idx[1] and it.elem[0] == it.elem[1])).
      # go back to the original elements
      filter(it.idx[0] == it.idx[1]).
      map(it.elem[0]) ==
                       @[1, 2, 3, 4])
  ## Example that uses own extensions: `average`, `intersect`, `inc` and `filterNot`.
  ## `intersect` uses the same implementation as in the previous test.
  test "register own extension":
    let a = @[1, 4, 3, 2, 5, 9]
    let b = @[7, 1, 8, 9, 4]
    let c = @[9, 4, 2, 3]
    # the own extensions are rejected when they have not been registered yet
    const errorMsg = " is unknown, you could provide your own implementation! See `zfCreateExtension`!"
    reject(a --> average() == 4.0, "average" & errorMsg)
    reject(a --> intersect(b) == @[1, 4, 9], "intersect" & errorMsg)
    reject(a --> inc(2) == @[3, 6, 5, 4, 7, 11], "inc" & errorMsg)
    reject(a --> filterNot(it mod 4 == 1) == @[4, 3, 2], "filterNot" & errorMsg)

    # now register the extension that supports the above functions (also set the new sequence handlers)
    registerExtension()

    # build the average value of a
    check(a --> average() == 4.0)
    # get all elements that are both in a and b
    check(a --> intersect(b) == @[1, 4, 9])
    check(a --> intersect(b, b) == @[1, 4, 9])
    check(a --> intersect(b, c) == @[4, 9])
    check(a --> intersectFast(b, c) == @[4, 9])

    # increment a by 1 and by 2
    check(a --> inc() == @[2, 5, 4, 3, 6, 10])
    check(a --> inc(2) == @[3, 6, 5, 4, 7, 11])
    # get all elements that are not 1 when modulo 4 is applied
    check(a --> filterNot(it mod 4 == 1) == @[4, 3, 2])

    # check own errors
    reject(a --> filterNot(it != 0, it != 1), "too many arguments in 'filterNot(it != 0, it != 1)', got 2 but expected only 1")
    reject(a --> inc(1, 2, 3), "too many arguments in 'inc(1, 2, 3)', got 3 but expected only 1")
    reject(a --> intersect(), "'intersect' needs at least 1 parameter!")

    check(@[1, 2, 1, 1, 3, 2, 1, 1, 4] --> removeDoubles() == @[1, 2, 3, 4])

  test "zip with other list":
    let a = @[1, 2, 3]
    let b = @[4, 5, 6]
    var res: seq[int] = @[]
    reject(zip(a, b) --> foreach(it[0] = it[0] + it[1]), "Adapted list cannot be changed in-place!")
    zip(a, b) --> foreach(res.add(it[0] + it[1]))
    check(res == @[5, 7, 9])

  test "reject wrong type arguments":
    reject(a --> exists(2) == true, "Function 'exists': param 'search', expected type 'bool'!")
    reject(a --> exists(1) == false, "Function 'exists': param 'search', expected type 'bool'!")
    reject(a --> index(3) == -1, "Function 'index': param 'cond', expected type 'bool'!")
    reject(a --> all(0) == false, "Function 'all': param 'test', expected type 'bool'!")
    reject(a --> drop(true) == @[], "Function 'drop': param 'count', expected type 'int'!")

  test "remove":
    var a = @[-1, 2, 3, -4, 5, -6]
    var b = @[-1, 2, 3, -4, -5] --> to(list)
    var c = @[-1, 2, 3, -4, 5, -6]
    check(a --> remove(it < 0) == true)
    check(a == @[2, 3, 5])
    check(b --> remove(it < 0) == true)
    check(b --> to(seq) == @[2, 3])
    check(c --> filter(it < 0) --> remove() == true)
    check(c == @[2, 3, 5])
    reject(a --> remove(0) == false, "Function 'remove': param 'cond', expected type 'bool'!")

  test "define variables in map":
    check(a --> map(item = it) --> filter(item > 0) == @[2, 8])
    check(a --> indexedMap(it) --> map((index, item) = it) --> filter(item >
        0) --> map(index) == @[0, 1])

  test "tuple conversion":
    let t = (1, 2, 3)
    check(t --> map(float) == (1.0, 2.0, 3.0))
    let (x, y) = (1, 2) --> map(float)
    check(x == 1.0 and y == 2.0)

  test "assignment in map":
    var cnt = 0
    proc countFun(i: int): int =
      cnt += 1
      result = i

    let res = a.zfun:
      map:
        row = countFun(it)
      filter:
        row > 0
      all:
        row mod 2 == 0

    check(res)
    check(cnt == a.len)

  test "split":
    check(@[(1, "one"), (2, "two"), (3, "three")] --> split() == (@[1, 2, 3], @[
        "one", "two", "three"]))

  test "iterator":
    a --> map($it) --> createIter(s)
    check(s() --> to(seq) == @["2", "8", "-4"])
    a --> map($it) --> createIter(s2, false)
    check(s2() --> to(seq) == @["2", "8", "-4"])
    a --> map($it) --> createIter(s3, closure = false)
    check(s3() --> to(seq) == @["2", "8", "-4"])

    proc convertToSeqString(cl: auto): auto =
      cl() --> map($it)
    check(convertToSeqString(a --> map(it) --> to(iter)) == @["2", "8", "-4"])

    when not defined(js):
      # createIter with closure does not work with JS backend
      proc checkIt() = # add another proc level (just for fun)
        a --> map(it) --> createIter(x, closure = true)
        check(convertToSeqString(x) == @["2", "8", "-4"])
      checkIt()

  test "convert uint to int results":
    let au = @[1u8, 2u8, 3u8]
    check(au --> to(seq[int], true) == @[1, 2, 3])

    let bu = @[@[1u8, 2u8], @[3u8]]
    check(bu --> map(it --> to(seq[int], true)) --> to(seq[seq[int]]) == @[@[1,
        2], @[3]])
    # the final conversion is not actually needed
    check(bu --> map(it --> to(seq[int], true)) == @[@[1, 2], @[3]])

  test "zip creates named tuple":
    # using identifiers in zip yields named tuples
    # in "simple" cases no indices are added
    check($(a --> zip(b, c)) == """@[(a: 2, b: 0, c: "zero"), (a: 8, b: 1, c: "one"), (a: -4, b: 2, c: "two")]""")
    # if any labels are the same _all_ labels are indexed
    check($(zip(a, a) --> map(it)) == "@[(a0: 2, a1: 2), (a0: 8, a1: 8), (a0: -4, a1: -4)]")
    let a1 = @[1, 2, 3]
    check($(a --> zip(a, a1)) == "@[(a0: 2, a1: 2, a12: 1), (a0: 8, a1: 8, a12: 2), (a0: -4, a1: -4, a12: 3)]")
    # using other expressions does not yield named tuples however
    check($(a --> zip(@[1, 2])) == "@[(2, 1), (8, 2)]")
    check($(@[1, 2] --> zip(a)) == "@[(1, 2), (2, 8)]")

  test "uniq":
    check(@[1, 2, 2, 2, 2, 3, 4, 4, 4, 5] --> uniq() == @[1, 2, 3, 4, 5])
    # uniqueness is only determined consecutively
    check(@[1, 1, 2, 1, 1, 3, 1] --> uniq() == @[1, 2, 1, 3, 1])

  test "uniq after changed type":
    check(@[1, 2, 2, 2, 2, 3, 4, 4, 4, 5] --> map($it) --> uniq() == @["1", "2", "3", "4", "5"])
    # uniqueness is only determined consecutively
    check(@[1, 1, 2, 1, 1, 3, 1] --> map($it) --> uniq() == @["1", "2", "1", "3", "1"])

  test "inner exists":
    let x = @[1, 2, 3]
    let y = @[2, 5, 6]
    let z = @[6, 7, 8]
    check(x --> map(a = it) --> exists(y --> exists(a == it)))
    check(x --> (a) --> exists(y --> exists(a == it)))
    check(not (x --> map(a = it) --> exists(z --> exists(a == it))))
    check(x --> (itX) --> exists(y --> (itY) --> exists(itX == itY)))
    # alternative (maybe nicer to read) bracketing
    check((x --> itX) --> exists((y --> itY) --> exists(itX == itY)))
    check((a --> x) --> exists((b --> y) --> exists(x == y)))

    let b = x.zfun(a):
      exists:
        z.zfun(b):
          exists(a == b)
    check(not b)

  test "concat":
    proc concatToSeq(): auto =
      concat(a, b, [7]) --> to(seq)
    check(concatToSeq() == @[2, 8, -4, 0, 1, 2, 7])
    check(concat([1], @[2], (3, 4)) --> map($it) == @["1", "2", "3", "4"])
    zfConcat(con, @[1], (2, 3, 4))
    check(con() --> to(seq) == @[1, 2, 3, 4])

  test "partition":
    proc isEven(i: int): bool = (i and 1) == 0
    let a = @[1, 2, 3, 4, 5, 6]
    let p = a --> partition(it.isEven())
    check(p.yes == @[2, 4, 6])
    check(p.no == @[1, 3, 5])

  test "partition after changed type":
    let a = @[111, 2, 33, 4, 5555, 6]
    let p = a --> map($it) --> partition(it.len() == 1)
    check(p.yes == @["2", "4", "6"])
    check(p.no == @["111", "33", "5555"])

  test "grouping":
    proc isEven(i: int): bool = (i and 1) == 0
    let a = @[1, 2, 3, 4, 5, 6]
    let p = a --> group(it.isEven())
    check(p[true] == @[2, 4, 6])
    check(p[false] == @[1, 3, 5])

    # group by last character
    let m = @[(1, "one"), (2, "two"), (3, "three")] --> group(it[1][^1])
    check(m['e'] == @[(1, "one"), (3, "three")])
    check(m['o'] == @[(2, "two")])

  test "grouping after changed type":
    proc isEvenLen(i: string): bool = (i.len() and 1) == 0
    let a = @[11, 2, 33, 4, 5555, 6]
    let p = a --> map($it) --> group(it.isEvenLen())
    check(p[false] == @["2", "4", "6"])
    check(p[true] == @["11", "33", "5555"])

    # group by last character
    proc stringyifyFirst(t: (int, string)): (string, string) = ($t[0], t[1])
    let m = @[(1, "one"), (2, "two"), (3, "three")] --> map(stringyifyFirst) --> group(it[1][^1])
    check(m['e'] == @[("1", "one"), ("3", "three")])
    check(m['o'] == @[("2", "two")])


