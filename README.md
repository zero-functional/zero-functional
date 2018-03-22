# zero-functional

[![Build Status](https://travis-ci.org/alehander42/zero-functional.svg?branch=rework-iterator)](https://travis-ci.org/alehander42/zero-functional)

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

## rationale

Functional style handling of sequences is awesome, and nim is supposed to be fast and smart.
Allocating new sequences on each method in chains can be extremely wasteful and there are not a lot
of technical reasons to punish functional style like that.

This library can expand functional chains to simple loops fuzing the method bodies one after another.
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

## Supported methods

Those are not exactly the functions from sequtils, they have the some naming and almost the same behavior

The macro works as

```nim
sequence --> map(..).exists(..)
```

or 

```nim
zip(a, b, c) --> map(..).exists(..)
```

You can also use 

```nim
inline_iter(sequence, map(..), exists(..))
```

The methods work with auto it variable

### map

```nim
sequence --> map(op)
```
Map each item in the list to a new value. 
Example:
```nim
let x = @[1,2,3] --> map(it * 2)
check(x == @[2,4,6])
```
Map also supports converting the type of iterator item and thus of the list.

### filter

```nim
sequence --> filter(cond)
```
Filter the list elements with the given condition.
Example:
```nim
let x = [-1,2,-3] --> filter(it > 0)
check(x == [2])
```

### zip

`zip` can only be used at the beginning of the command chain and it can work with n sequences

### exists

Check if the given condition is true for at least one element of the list.

`exists` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..).exists(cond): bool
```

### all

Check if the given condition is true for all elements of the list.

`all` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..).all(cond): bool
```

### index

Get the first index of the item in the list, where the given condition is true.

`index` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..).index(cond): int
```


### indexedMap

Generates a tuple (index, it) for each element in the list

```nim
var n = zip(a, b, c) -->
            indexedMap(f(it[0], it[1])).
            filter(it[0] < 10 and it[1] mod 4 > 1).
            map(it[1] * 2).
            all(it > 4)
```

### fold

Currently a left fold (as easier to combine with the implementation)

the sequtils `a` is `_`, `a` is `it`

```nim
var n = zip(a, b) --> map(it[0] + it[1]).fold(0, a + it)
```

## reduce

Same as fold, but with the iterator converted to a tuple where
`it[0]` is the current result and `it[1]` the actual iterator on the list.

```nim
var n = a --> reduce(it[0] + it[1])
```

### foreach

Can only be used with functions that have side effects.
When last command in the chain the result is void. 
As in-between element, the code is simply executed on each element.
The iterator content may be changed in foreach resulting in changing
the original list.

```nim
@[1,2,3] --> 
    foreach(echo($it))
    
var a = @[1,2,3]
a --> foreach(it = it * 2)
check (a == @[2,4,6]
```

### flatten

Working on a collection of iterable items, the flatten function flattens out the elements of the list.

```nim
check(@[@[1,2], @[3,4]] --> flatten() == @[1,2,3,4])
```

### combinations

Combines each element with each other - the resulting variable is `c` with `c.it` as array of 2 containing the combined
iterator values and `c.idx` containg their indices.
Combinatiions is not allowed as last argument.

```nim
# find the indices of the elements in the list, where the diff to the other element is 1
check(@[11,2,7,3,4] --> combinations() --> filter(abs(c.it[1]-c.it[0]) == 1) --> map(c.idx) == @[[1,3],[3,4]])
#          ^   ^ ^
```

### to

Finally it is possible to force the result type to the type given in `to` - which is only allowed as last argument.
This method is handled differently from the others and removed internally so the command before `to` is the actual last argument. 

When the result type is given as `seq`, `array` or `list` (the latter is mapped to `DoublyLinkedList`) then the template argument
will be determined automatically.
However when all auto-detection fails, the result type may be given explicitly here.

```nim
check([1,2,3]) --> to(seq) == @[1,2,3])
var l = @[1,2,3] --> map($it) --> to(list)
let l2: DoublyLinkedList[string] = l
echo(l2)
```

## Overview Table

Result type depends on the function used as last parameter.

| Command       | 1st Param | in-between | Last Param | Result Type                 |
| ------------- | --------- | ---------- | ---------- | --------------------------- | 
|all            |           |            |     +      | bool                        |
|combinations   |   +       |     +      |            | N/A                         |
|exists         |           |            |     +      | bool                        |
|filter         |   +       |     +      |     +      | part list / zeroed array    |
|find           |           |            |     +      | int                         |
|flatten        |   +       |     +      |     +      | list                        |
|fold           |           |            |     +      | *                           |
|foreach        |   +       |     +      |     +      | void                        |
|index          |           |            |     +      | int                         |
|indexedMap     |   +       |     +      |     +      | seq[(int,*)]                |
|map            |   +       |     +      |     +      | orig[*]                     |
|reduce         |           |            |     +      | *                           |
|sub            |   +       |     +      |     +      | part list / zeroed array    |
|zip            |   +       |            |            | seq[(*,..,*)]               |
|to             |           |            |   virtual  | given type                  |

*: any type depending on given function parameters
list: is any input-list type
orig: is the original list type
to: is a "virtual" function, can only be given as last argument, but does not count as last argument.


### LICENSE

MIT, Alexander Ivanov

### Contributors

Substantial contributions : [Michael Schulte](https://github.com/michael72)
