# zero-functional

[![Build Status](https://travis-ci.org/alehander42/zero-functional.svg?branch=master)](https://travis-ci.org/alehander42/zero-functional)

A library providing (almost) zero-cost chaining for functional abstractions in Nim.

```nim
var n = zip(a, b) -->
            map(f(it[0], it[1])).
            filter(it mod 4 > 1).
            map(it * 2).
            all(it > 4)
```

is expanded on compile time to the equivalent of

```nim
(proc (): auto =
  var minHigh134598 = min([high(a), high(b)])
  var empty = true
  for z in low(a) .. minHigh134598:
    var it0 = (a[z], b[z])
    var it1 = f(it0[0], it0[1])
    if it1 mod 4 > 1:
      var it2 = it1
      var it3 = it2 * 2
      result = true
      if not(it3 > 4):
        return false)()
```

compared to 

```nim
import sequtils

var n = zip(a, b).
            mapIt(f(it[0], it[1])).
            filterIt(it mod 4 > 1).
            mapIt(it * 2).
            allIt(it > 4)
```

which is roughly equivalent to

```nim
var m = min(a.len, b.len)
var result0: seq[(int, int)]
newSeq(result0, m)
for i in 0 .. <m:
  result0[i] = (m[i], m[i])
var result1: seq[int]
let t0 = result0
var i0 = 0
result1 = newSeq[int](result0.len)
for it in t0:
  result1[i0] = f(it[0], it[1])
  i0 += 1
var result2 = newSeq[int]()
for it in items(result1):
  if it mod 4 > 1:
    result.add(it)
var result3: seq[int]
let t1 = result1
var i1 = 0
result3 = newSeq[int](result2.len)
for it in t1:
  result3[i1] = it * 2
result = true
for it in items(result3):
  if not (it > 4):
    return false
```


## Installation

```
nimble install zero_functional
```

Note: the correct name is `zero_functional` (with underscore).


## Rationale

Functional style handling of sequences is awesome, and nim is supposed to be fast and smart.
Allocating new sequences on each method in chains can be extremely wasteful and there are not a lot
of technical reasons to punish functional style like that.

This library can expand functional chains to simple loops fusing the method bodies one after another.
it's still very experimental, but it shows such an purely metaprogramming approach can be used to optimize functional Nim code

## Variable names

The supported variable names (can be changed at the beginning of the zero_functional.nim file) are:

* `it` is used for the iterator variable
* `idx` is used as integer index of current iteration
* `a` is used as the accumulator in `fold`
* `c` is used as combination element in `combinations`

## Seq and arrays

All supported methods work on finite indexable types and arrays.
If a handler returns a collection, it will be the same shape as the input one for seq-s, arrays and DoublyLinkedList-s.
Other collections are mapped to seq if it cannot be automatically converted. (e.g. array.map returns an array). 
You can always get a seq if you use `<handler>Seq` e.g. `mapSeq` - or `to(seq)`.
Some of the supported methods default to seq-output, e.g. map when changing the result type, flatten and indexedMap.

We can describe the supported types as

```nim
type
  FiniteIndexable[T] = concept a
    a.low is int
    a.high is int
    a[int] is T
```

## Other types

Enums are supported and mapped to `seq[enumtype]`.
Generic objects are supported if they are of any type:
 + FiniteIndexable - contains `high`/`low` and []-access (see above)
 + FiniteIndexableLen - contains `len` and `[]`

Collection types that will be generated as as result type need to implement either one of 
 + Appendable (contains the `append` function as in DoublyLinkedList)
 + Addable (contains the `add` function as in `seq`)
 + `[]=` operator
Some of the supported methods will only work when the `[]=` operator is defined - except when using DoublyLinkedList or SinglyLinkedList types. This is needed for `zip`, `combinations` and `foreach` when changing elements.
 
For the creation of a generic type as result, the type needs to implement
```nim
proc init_zf(a: MyType): MyType =
  MyType(...)
  # the `a` is not actually used but is needed for overloading.
```
 
## Supported methods

These are not exactly the functions from sequtils, they have the same naming and almost the same behavior

The macro works `-->` or `connect`. Multiple `-->` may be used or `.`.

```nim
sequence --> map(..) --> all(..)
```

or 

```nim
zip(a, b, c) --> map(..).
                 all(..)
```

You can also use 

```nim
connect(collection, map(..), all(..))
```

The methods work with auto it variable

### map

```nim
collection --> map(op)
```
Map each item in the collection to a new value. 
Example:
```nim
let x = [1,2,3] --> map(it * 2)
check(x == [2,4,6])
```
Map also supports converting the type of iterator item and thus of the collection.

### filter

```nim
sequence --> filter(cond)
```
Filter the collection items with the given condition.
Example:
```nim
let x = @[-1,2,-3] --> filter(it > 0)
check(x == @[2])
```

### zip

`zip` can work with n sequences. `zip` is (roughly) internally translated to:
```nim
zip(a,b,c) <=> 
a --> zip(b,c) <~> 
let minHigh = min([a.high(), b.high(), c.high()])
a --> filter(idx <= minHigh) --> map(a[idx], b[idx], c[idx])
```
On the right side of `-->` (or as 2nd and later command) the left side of `-->` is added to the zip result.
For zip in order to work properly all arguments have to support access with `[]` and the `high` procedure.
If those procedures are not available the macro tries to call the procedure `mkIndexable` on that parameter. Using this helper the parameter can be wrapped with a new type that supports `[]` and `high`.

### exists

Check if the given condition is true for at least one element of the collection.

`exists` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..) --> exists(cond): bool
```

### all

Check if the given condition is true for all elements of the collection.

`all` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..) --> all(cond): bool
```

### index

Get the first index of the item in the collection, where the given condition is true.

`index` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..) --> index(cond): int
```


### indexedMap

Generates a tuple (index, it) for each element in the collection

```nim
var n = zip(a, b, c) -->
            indexedMap(f(it[0], it[1])).
            filter(it[0] < 10 and it[1] mod 4 > 1).
            map(it[1] * 2).
            all(it > 4)
```

### fold

Currently a left fold (as easier to combine with the implementation)

the sequtils `a` is `a`, `b` is `it`

```nim
var n = zip(a, b) --> map(it[0] + it[1]) --> fold(0, a + it)
```

### reduce

Same as fold, but with the iterator converted to a tuple where
`it[0]` is the current result and `it[1]` the actual iterator on the collection.
The first item of the collection is used as initial value - the other items
are then accumulated to it. This also useful when a type does not define the neutral element for the given operation. E.g. for integers and `+` the neutral element is 0 but for user defined types the neutral element might not exist.

```nim
var n = a --> reduce(it[0] + it[1])
```

There are a few commands that are simply mapped to reduce 

#### max
Return the maximum value in the collection (`>` is needed)
#### min
Return the minimum value in the collection (`<` is needed)
#### product
Return the product of the (filtered) elements (`*`)
#### sum
Return the sum of the (filtered) elements (`+`).

### indexedReduce

By adding the `indexed` prefix to `reduce` or to the reduce commands above, the index of the last value that was used for the `result` and the actual result of the operation are returned.
For `sum` and `product` this is not actually helpful but it can be used to find the indices of the `min` and `max` elements.

```nim
check(@[11,2,0,-2,1,3,-1] --> indexedMin() == (3,-2))
check(@[11,2,0,-2,1,3,-1] --> indexedMax() == (11,0))
```

### foreach

Can only be used with functions that have side effects.
When used ad last command in the chain the result is void. 
As in-between element the code is simply executed on each element.

#### changing in-place
The iterator content may be changed in `foreach` resulting in changing
the original collection.
However there are a few restrictions (see test.nim](test.nim#L455)):
+ the `[]=` operator has to be available for the underlying collection type (exception: the std LinkedList types)
+ functions that alter the collection elements may not be used in the chain before (e.g. `map` is not allowed, but `filter` is).

```nim
@[1,2,3] --> 
    foreach(echo($it))
    
var a = @[1,2,3]
a --> foreach(it = it * 2)
check (a == @[2,4,6]
```

### sub

Works on a part of the input collection - `sub`(`fromIndex`,`toIndex`) or drop(`fromIndex`) - similarly to ranges, starting with `fromIndex`  and ending (inclusive) with `endIndex` or runs til the end, when `endIndex` is not given.
```nim
check((1..10) --> sub(2,5) --> to(list) == @[2,3,4,5])
```
The `endIndex` may be a `BackwardsIndex` like `^1`, but then the collection has to have a `len`.
`sub` is similar to the `filter` function working on the `idx` variable - however `sub` uses an internal index that is not affected by the outcome of preceeding filtering functions.
```nim
# in filter `idx` counts the iterated items
check(@[-1,2,-3,4,-5,6,-7,8] --> filter(it > 0) --> filter(idx >= 3) == @[4,6,8])
# sub increments its own index when `it > 0`
check(@[-1,2,-3,4,-5,6,-7,8] --> filter(it > 0) --> sub(3) == @[8])
```

Similar commands like `sub` that result in parts of the lists being iterated on or generated are: `drop`, `dropWhile`, `take` and `takeWhile`.

#### drop
`drop`(`n`) drops n items before working on the collection. This is equivalent to `sub`(`n`).
#### dropWhile
`drop`(`cond`) drops the items as long as the condition in `cond` is met - so starts working on the collection when the condition is not fulfilled any more.
As opposed to `filter` the condition in `drop` is ignored, once it was not true any more.
```nim
check(@[-1,2,-3,4,-5] --> dropWhile(it < 0) --> sum() == -2)
check(@[-1,2,-3,4,-5] --> filter(it >= 0)   --> sum() == 6)
```
#### take
`take`(`n`) works on n items of the collection and then breaking. This is useful for very large (infinite) collections or iterators - the same for `takeWhile`.
#### takeWhile`
`takeWhile`(`cond`) works on the collection as long as the condition in `cond` is met. Otherwise it breaks the processing.


### flatten

Working on a collection of iterable items, the flatten function flattens out the elements of the collection.

```nim
check(@[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6])
```

#### indexedFlatten

Is similar to `flatten`, except that it returns the index inside original sub-lists with the actual content.
```nim
check(@[@[1,2],@[3],@[4,5,6]] --> indexedFlatten()            == @[(0,1),(1,2),(0,3),(0,4),(1,5),(2,6)])
check(@[@[1,2],@[3],@[4,5,6]] --> flatten() --> map((idx,it)) == @[(0,1),(1,2),(2,3),(3,4),(4,5),(5,6)])
```

### combinations

Combines each item of the original collection with each other - the resulting variable is `c` with `c.it` as array of 2 containing the combined
iterator values and `c.idx` containg their indices.
`combinations` is not allowed as last argument.

```nim
# find the indices of the elements in the collection, where the diff to the other element is 1
check(@[11,2,7,3,4] --> combinations() --> filter(abs(c.it[1]-c.it[0]) == 1) --> map(c.idx) == @[[1,3],[3,4]])
#          ^   ^ ^
```

### to

Finally it is possible to force the result type to the type given in `to` - which is only allowed as last argument when generating 
collection results (e.g. `map` or `filter` are the last arguments before `to`).
This method is handled differently from the others and removed internally so the command before `to` is the actual last command. 

When the result type is given as `seq`, `array` or `list` (the latter is mapped to `DoublyLinkedList`) then the template argument
can be determined automatically.
However when all auto detection fails, the result type may be given explicitly here - the resulting code is also a bit more efficient.

```nim
check([1,2,3]) --> to(seq) == @[1,2,3])
var l = @[1,2,3] --> map($it) --> to(list)
let l2: DoublyLinkedList[string] = l
echo(l2)
```

## Overview Table

The result type depends on the function used as last parameter.

| Command       | 1st Param | in-between | Last Param | Result Type                 |
| ------------- | --------- | ---------- | ---------- | --------------------------- |
|all            |           |            |     +      | bool                        |
|combinations   |   +       |            |            | map(it): coll[Combination]  |
|exists         |           |            |     +      | bool                        |
|filter         |   +       |     +      |     +      | part coll / zeroed array    |
|find           |           |            |     +      | int                         |
|flatten        |   +       |     +      |     +      | coll                        |
|fold           |           |            |     +      | *                           |
|foreach        |   +       |     +      |     +      | void                        |
|index          |           |            |     +      | int                         |
|indexedMap     |   +       |     +      |     +      | seq[(int,*)]                |
|map            |   +       |     +      |     +      | collType[*]                 |
|reduce         |           |            |     +      | *                           |
|sub            |   +       |     +      |     +      | part coll / zeroed array    |
|zip            |   +       |     +      |            | map(it): seq[(*,..,*)]      |
|to             |           |            |   virtual  | given type                  |

+ *: any type depending on given function parameters
+ coll: is the input collection
+ collType is the input collection type (without template argument)
+ map(it): some functions are not allowed as last parameter but using map(it) will yield the given output
+ to: is a "virtual" function, can only be given as last argument, but does not count as last argument.

## Debugging using `-->>`

As `zero_functional` is still work in progress and macros are still kind of experimental in Nim, it can happen, that the compiler
crashes or that compile errors are hard to understand. To see the actual code that is generated (provided the generation itself does not crash)
you can use the `-->>` operator which prints the representation `repr` of the actual generated nim code.
It is also useful for checking what the expression is generating under the hood.

```nim
let a = [1,2,3]
a -->> foreach(echo(it))
```

will print during compilation:
```nim
if true:
  for __it__0 in a:
    echo(__it__0)
```

The printed code can be copied to your actual program for further investigation.
Remember to remove the dunder (double-underscores) of the generated variables before compiling the generated code and watch out for name-clashes with your own code.

### LICENSE

MIT, Alexander Ivanov

### Contributors

Co-maintainers and authors: [Michael Schulte](https://github.com/michael72), Alexander Ivanov

Creator: Alexander Ivanov

