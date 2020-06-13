# zero-functional

[![Build Status](https://travis-ci.org/zero-functional/zero-functional.svg?branch=master)](https://travis-ci.org/zero-functional/zero-functional)

A library providing (almost) zero-cost chaining for functional abstractions in Nim.


Table of Contents
=================

<!-- toc -->
<!-- add toc with markdown-toc -->
- [Initial Example](#initial-example)
- [Installation](#installation)
- [Rationale](#rationale)
- [Variable names](#variable-names)
- [Seq and arrays](#seq-and-arrays)
- [Other types](#other-types)
- [Supported methods](#supported-methods)
  * [map](#map)
  * [filter](#filter)
  * [zip](#zip)
  * [split](#split)
  * [exists](#exists)
  * [all](#all)
  * [index](#index)
  * [indexedMap](#indexedmap)
  * [enumerate](#enumerate)
  * [fold](#fold)
  * [reduce](#reduce)
    + [max](#max)
    + [min](#min)
    + [product](#product)
    + [sum](#sum)
  * [indexedReduce](#indexedreduce)
  * [foreach](#foreach)
    + [changing in-place](#changing-in-place)
  * [sub](#sub)
    + [drop](#drop)
    + [dropWhile](#dropwhile)
    + [take](#take)
    + [takeWhile](#takewhile)
  * [flatten](#flatten)
    + [indexedFlatten](#indexedflatten)
  * [combinations](#combinations)
    + [indexedCombinations](#indexedcombinations)
  * [concat](#concat)
  * [group](#group)
  * [partition](#partition)
  * [to](#to)
    + [createIter](#createiter)
- [Extending zero-functional](#extending-zero-functional)
  * [Extending with plain nim](#extending-with-plain-nim)
  * [Writing extensions with Zero-DSL](#writing-extensions-with-zero-dsl)
    + [Special variables](#special-variables)
    + [Setting the result](#setting-the-result)
  * [Defering to Zero-DSL inside plain nim](#defering-to-zero-dsl-inside-plain-nim)
    + [Creating compound commands with other commands](#creating-compound-commands-with-other-commands)
- [Overview Table](#overview-table)
- [Debugging using `-->>`](#debugging-using---)
- [Compile flags](#compile-flags)
- [LICENSE](#license)
- [Contributors](#contributors)

<!-- tocstop -->

## Initial Example
The example:

```nim
var n = zip(a, b) -->
            map(f(it[0], it[1])).
            filter(it mod 4 > 1).
            map(it * 2).
            all(it > 4)
```

is expanded on compile time - additional compile-time checks omitted - to the equivalent of:

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

Compared to:
```nim
import sequtils

var n = zip(a, b).
            mapIt(f(it[0], it[1])).
            filterIt(it mod 4 > 1).
            mapIt(it * 2).
            allIt(it > 4)
```

which is roughly equivalent to:

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

Note: the correct name is `zero_functional` (with an underscore).


## Rationale

Functional style handling of sequences is awesome, and Nim is supposed to be fast and smart.
Allocating new sequences on each method in a chain can be extremely wasteful and there are not a lot of technical reasons to punish functional style like that.

This library can expand functional chains to simple loops fusing the method bodies one after another.
It is still very experimental, but it shows that a purely metaprogramming approach can be used to optimize functional Nim code.


## Variable names

The supported variable names (can be changed at the beginning of the [zero_functional.nim](zero_functional.nim) file) are:

* `it` is used for the iterator variable
* `idx` is used as integer index of current iteration
* `a` is used as the accumulator in `fold`


## Seq and arrays

All supported methods work on finite indexable types and arrays.

If a handler returns a collection, it will be of the same shape as the input for seq-s, arrays and DoublyLinkedList-s.
Other collections are mapped to seq if it cannot be automatically converted. (e.g. array.map returns an array).

You can always get a seq if you use `<handler>Seq`, e.g. `mapSeq` - or `to(seq)`.
Some of the supported methods default to seq-output, e.g. `map` when changing the result type, `flatten` and `indexedMap`.

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
 + FiniteIndexable - contains `high`/`low` and `[]`-access (see above)
 + FiniteIndexableLen - contains `len` and `[]`

Collection types that will be generated as a result type need to implement either one of
 + Appendable (contains the `append` function as in DoublyLinkedList)
 + Addable (contains the `add` function as in `seq`)
 + `[]=` operator

Some of the supported methods will only work when the `[]=` operator is defined - except when using DoublyLinkedList or SinglyLinkedList types.
This is needed for `zip`, `combinations` and `foreach` when changing elements.

For the creation of a generic type as result, the type needs to implement
```nim
proc zfInit(a: MyType): MyType =
  MyType(...)
  # the `a` is not actually used but is needed for overloading.
```


## Supported methods

These are not exactly the functions from sequtils, they have the same naming and almost the same behavior.

The macros work with `-->`, `zfun` or `connect`. Multiple `-->` may be used or `.`.

```nim
sequence --> map(..) --> all(..)
```

or

```nim
zip(a, b, c) --> map(..).
                 all(..)
```

You can also use the call with sections - with the above calls omitting the dots or arrows or with several statements applied to the same function in one section.
```nim
let res = sequence.zfun:
  map:
    f1(it)
    f2(it)
  all(...)
```

or as simple arguments to a function:

```nim
connect(collection, map(..), all(..))
```

The methods work with the auto `it` variable.


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

Map is (currently) the only command supporting value definitions inside the map call that may be used instead or additionally to the `it` value in subsequent calls. Assignment to simple values or tuple assignments are possible as well.
```nim
let idx = rows --> map(row = it) --> index(row > someValue)
let posCoords = coords --> map((x,y) = it) --> filter(x > 0 and y > 0)
```

A shortcut syntax is also supported - actually replacing `it` with the given variable names:
```nim
# a iterates on x
check(x --> (a) --> exists(y --> exists(a == it)))
# this is equivalent to
check(x --> map(a = it) --> exists(y --> exists(a == it)))
```

`zfun` also supports this shortcut either as single line `(a)` or as a parameter to `zfun`:
```nim
let b = x.zfun(a):
  exists:
    z.zfun(b):
      exists(a == b)
check(not b)
```

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
For `zip` in order to work properly all arguments have to support access with `[]` and the `high` procedure.
If those procedures are not available the macro tries to call the procedure `mkIndexable` on that parameter.
Using this helper the parameter can be wrapped with a new type that supports `[]` and `high`.

### split
`split` is kind of the opposite of `zip`. It works with a collection of n-tuples and splits it up into an n-tuple of sequences.

```nim
check([(1, "one"), (2, "two")] --> split() == (@[1,2], @["one","two"])
```

### exists

Check if the given condition is true for at least one element of the collection.

`exists` can be used only at the end of the command chain.

```nim
sequence --> otherOperations(..) --> exists(cond): bool
```


### all

Check if the given condition is true for all the elements of the collection.

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

Adds or prepends the index of each element in the collection to the element itself and generates a named tuple `(idx: index, elem: it)` for each element in the collection. 

```nim
var n = zip(a, b, c) -->
            indexedMap(f(it[0], it[1])).
            filter(it.idx < 10 and it.elem mod 4 > 1).
            map(it.elem * 2).
            all(it > 4)
```

### enumerate 

Is similar to `indexedMap` and to [`enumerate`](https://docs.python.org/3/library/functions.html#enumerate) in python. It does not take any parameters and just works on the current collection adding the index of the current element.

```nim
@[8, 11, 12] --> enumerate() == @[(0,8), (1,11), (2,12)] 
```

### fold

Currently a left fold (as easier to combine with the implementation).

The sequtils `a` is `a`, `b` is `it`.

```nim
var n = zip(a, b) --> map(it[0] + it[1]) --> fold(0, a + it)
```


### reduce

Same as fold, but with the iterator converted to a tuple where `it[0]` or `it.accu` is the accumulated result and `it[1]` or `it.elem` the actual iterator on the collection.

The first item of the collection is used as initial value - the other items are then accumulated to it.
This is also useful when a type does not define the neutral element for the given operation.
E.g. for integers and `+` the neutral element is 0 but for user defined types the neutral element might not exist.

```nim
var n = a --> reduce(it.accu + it.elem)
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

Note that a named tuple is created and the index is also accessible via `.idx` and the actual element with `.elem`.

### foreach

Can only be used with functions that have side effects.
When used as the last command in the chain the result is void.
As in-between element the code is simply executed on each element.


#### changing in-place
The iterator content may be changed in `foreach` resulting in changing the original collection.
However there are a few restrictions (see [test.nim](test.nim#L455)):
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

Works on a part of the input collection - `sub(fromIndex, toIndex)` or `drop(fromIndex)` - similarly to ranges, starting with `fromIndex` and ending (inclusive) with `endIndex` or runs til the end, when `endIndex` is not given.
```nim
check((1..10) --> sub(2,5) --> to(list) == @[2,3,4,5])
```

The `endIndex` may be a `BackwardsIndex` like `^1`, but then the collection has to have a `len`.

`sub` is similar to the `filter` function working on the `idx` variable, however `sub` uses an internal index that is not affected by the outcome of preceding filtering functions.

```nim
# in filter `idx` counts the iterated items
check(@[-1,2,-3,4,-5,6,-7,8] --> filter(it > 0) --> filter(idx >= 3) == @[4,6,8])
# sub increments its own index when `it > 0`
check(@[-1,2,-3,4,-5,6,-7,8] --> filter(it > 0) --> sub(3) == @[8])
```

Similar commands like `sub` that result in parts of the lists being iterated on or generated are: `drop`, `dropWhile`, `take` and `takeWhile`.


#### drop
`drop(n)` drops n items before working on the collection. This is equivalent to `sub(n)`.

#### dropWhile
`dropWhile(cond)` drops the items as long as the condition in `cond` is met - it starts working on the collection when the condition is not fulfilled any more.
As opposed to `filter` the condition in `drop` is ignored, once it was not true any more.
```nim
check(@[-1,2,-3,4,-5] --> dropWhile(it < 0) --> sum() == -2)
check(@[-1,2,-3,4,-5] --> filter(it >= 0)   --> sum() == 6)
```

#### take
`take(n)` works on n items of the collection and then breaking. This is useful for very large (infinite) collections or iterators - the same for `takeWhile`.

#### takeWhile
`takeWhile(cond)` works on the collection as long as the condition in `cond` is met. Otherwise it breaks the processing.


### flatten

Working on a collection of iterable items, the `flatten` function flattens out the elements of the collection.

```nim
check(@[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6])
```


#### indexedFlatten

Is similar to `flatten`, except that it returns the index inside original sub-lists with the actual content.
```nim
check(@[@[1,2],@[3],@[4,5,6]] --> indexedFlatten()            == @[(0,1),(1,2),(0,3),(0,4),(1,5),(2,6)])
check(@[@[1,2],@[3],@[4,5,6]] --> flatten() --> map((idx,it)) == @[(0,1),(1,2),(2,3),(3,4),(4,5),(5,6)])
```
Note that as in the other `indexed` commands the tuples are named tuples and the index is also accessible via `.idx` and the actual element with `.elem`.

### combinations

Combines each element of the original collection with each other - the resulting variable is an array of 2 containing the combined iterator values. In case no other collection is supplied the combinations are done on the input collection - only combining different elements with each other.
```nim
# combine collection with itself results in unordered combinations - only different elements are combined
check(@[1,2,3] --> combinations() == @[[1,2],[1,3],[2,3])
# combine collection with the same collection again as a parameter results in comparing all elements in 
# all possible ordered combinations
check(@[1,2,3] --> combinations(@[1,2,3]) == @[[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]])

# combine all elements of first collection with elements of the second collections
check(@[1,2,3] --> combinations(@[4,5]) == @[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]])
```

#### indexedCombinations

Same as `combinations` with the additional indices of the resulting combined elements. The resulting iterator is a named tuple with the combined elements and their indices `(idx: [idx1, idx2], elem: [combined_element1, combined_element2])`.
```nim
# find the indices of the elements in the collection, where the diff to the other element is 1
check(@[11,2,7,3,4] --> indexedCombinations() --> filter(abs(it.elem[1]-it.elem[0]) == 1) --> map(it.idx) == @[[1,3],[3,4]])
#          ^   ^ ^
```

### concat

Concatenates the given arguments to one iterator. Can only be used as first command preceeding `-->`.
```nim
check(concat([1,2],[3,4]) --> to(seq) == @[1,2,3,4])
```
Alternatively `zf_concat` can be used to create an iterator that concatenates the given iterators to one.
```nim
zf_concat(con, @[1], (2,3,4))
check(con() --> to(seq) == @[1,2,3,4])
```

### group
Creates a table containing result of each element applied to a discriminator function as keys and the corresponding elements in sequences as values.

```nim
# taking the last character of the strings as key
let m = @[(1,"one"),(2,"two"),(3,"three")] --> group(it[1][^1])
check(m['e'] == @[(1, "one"), (3, "three")])
check(m['o'] == @[(2, "two")])
```

### partition
Simplified version of `group` and alternative version to `filter` that returns a tuple with all elements that match the filter condition as first tuple element named `yes` and all non-matching as the second named `no`.

```nim
proc isEven(i: int): bool = (i and 1) == 0
  let a = @[1,2,3,4,5,6]
let p = a --> partition(it.isEven())
check(p.yes == @[2,4,6])
check(p.no == @[1,3,5])
```
### to

Finally, it is possible to force the result type to the type given in `to` - which is only allowed as last argument when generating collection results (e.g. `map` or `filter` are the last arguments before `to`).
This method is handled differently from the others and removed internally so the command before `to` is the actual last command.

When the result type is given as `seq`, `array` or `list` (the latter is mapped to `DoublyLinkedList`) then the template argument can be determined automatically.
However, when all auto detection fails, the result type may be given explicitly here - the resulting code is also a bit more efficient for the compilation process.

```nim
check([1,2,3]) --> to(seq) == @[1,2,3])
var l = @[1,2,3] --> map($it) --> to(list)
let l2: DoublyLinkedList[string] = l
echo(l2)
```

`to` supports a second parameter `autoConvert`. This actually only suppresses a warning when the actual result type and the added type are not identical but can be cast to another.
A bit more complex example:
```nim
# doing a complex conversion from `seq[seq[uint8]]` to `seq[seq[int]]`
check(@[@[1u8, 2u8], @[3u8]] --> map(it --> to(seq[int],true)) --> to(seq[seq[int]]) == @[@[1,2], @[3]])
```
* the `map` call itself contains a new `-->` call, i.e.: each internal list is converted from `seq[uint8]` to `seq[int]`
* the parameter `true` is necessary to suppress a warning that the conversion is done by casting (each element)
* the final result is a `seq[seq[int]]` - this conversion is optional (maybe to document) and can be determined automatically.

#### createIter

When using `createIter(name:string,closure:bool=false)` as last function then an iterator `name` is created which can be used for further processing with zero-functional with only a small overhead.
Similar to `to` this is also a virtual function which is internally replaced and only used to check the output type.
The generated iterator is inline by default and can not be returned from a proc or given to another proc (see [Nim: Iterators](https://nim-lang.org/docs/manual.html#iterators-and-the-for-statement-first-class-iterators)).

To create a closure iterator, the optional argument `closure` has to be set to `true`. A closure iterator is created which can be used as a return result from a procedure or as a parameter to a procedure. *This does not work with JS backend!*

```nim
import zero_functional
import strutils
# filter all lines containing the word hint in the iterator
lines("nim.cfg") --> filter("hint" in it.string) --> createIter(errorLines)
errorLines() --> foreach(echo it)
```

## Extending zero-functional

Extending zero-functional with own functions is probably more complicated than with other fp-libraries as the functions have to be implemented with macros producing inlined imperative code. 
Some good examples from basic to more complicated can be found in [test.nim: registerExtension](test.nim) and in the source code of [zero-functional](zero-functional.nim)

### Extending with plain nim
When adding your own `foo` implementation you can write your own `inlineFoo` proc and register it with zero_functional. It should look like this:
```nim
proc inlineFoo*(ext: ExtNimNode) {.compileTime.} =
  # do some parameter checks
  if ext.node.len > SOME_MAX:
    zfFail("too many arguments in \'$1\', got $2 but expected only $3" %
        [ext.node.repr, $(ext.node.len - 1), $SOME_MAX])
  # do some stuff
  # ...
  # the actual 'loop' section code
  ext.node = quote:
    # do something
```

The vanilla `inline`-proc implementations should follow certain rules. 
- `ext: ExtNimNode` as parameter
- ExtNimNode should be used when implementing the functions with some helpers:
  - `ext.node` = place the actual code here that is being generated inside the current block 
	initially `ext.node` contains the current function call and its parameters (section: loop)
  - `ext.initials` = add the initialization code for variable definitions (section: init)
  - `ext.endLoop` = add code that can be inserted at the end of the loop (section: end)
  - `ext.finals` = add code after the loop - e.g. to calculate a result (section: final)
  - `ext.res` = helper: access to the function's result
  - `ext.prevItNode()` = access to the iterator generated in the previous statement or loop
  - `ext.nextItNode()` = generates a new iterator for the current block. This is the (intermediate) result of the current operation that can be used with the next function
- check of parameters / number of parameters has to be done in the implementation 
- use `zfFail()` if any checks fail
- register functions that use neither `zfInline` nor `zfInlineCall` using `zfAddFunction`
- functions that create another sequence have to be registered with `zfAddSequenceHandlers`
- finally call `zfCreateExtension` after all `zfInline...` definitions and `zfAddFunction` calls - before using the actual function implementation

### Writing extensions with Zero-DSL
Example - implement the (simple) map function:
```nim
zfInline map(f):
  loop:
    let it = f # create the next iterator in the loop setting it to the given parameter of the map function
```

`zfInline` is the actual macro that takes the created function name (here: `map`) and its parameters and a body with different sections as input.
`zfInline_dbg` will print the generated code `proc inlineMap` (see below).

The sections directly map to their counterparts in `ExtNimNode`:
- `pre` prepare section: initialize variables and constants. It is possible to do the entire implementation in the `pre` section.
	- all variables that are used in other sections have to be defined here!
- `init` variable initialization before the loop
- `loop` the actual loop action (maps to `ext.node`)
- `delegate` delegate to other functions (like map, filter, etc.)
- `endLoop` added to end of the loop
- `final` after the loop section - e.g. to set the result

The above `map` definition will be translated to:
```nim
proc inlineMap*(ext: ExtNimNode) {.compileTime.} =
  # do some parameter checks
  if ext.node.len - 1 > 1:
    zfFail("too many arguments in \'$1\', got $2 but expected only $3" %
        [ext.node.repr, $(ext.node.len - 1), $1])
  let f = 
    if ext.node.len > 1:
      adapt(ext, 1) # replace all occurences if internal iterator "it" with __it__0, __it__1, etc.
    else: # assert that the argument 'f' is supplied
      zfFail("missing argument \'$1\' for \'$2\'" % ["f", "map"])
      newIntLitNode(0)
  let nextIdent = ext.nextItNode() # create the next iterator
  # the actual 'loop' section
  ext.node = quote:
    let `nextIdent` = `f` # here the `it` is replaced by the next iterator
```

The Zero-DSL `map` function does not set the `result` as opposed to the `count` or `index` definition below - hence the result type is a collection result type, which is determined automatically by the zero_functional framework.
The `it` again is seen as keyword and the definition `let it = ...` will internally set the new iterator value which is consequently used by the next functions. In the generated macro it is replaced by the call `ext.nextItNode`.

While Zero-DSL is quite powerful, not all possibilities can be handled by it when implementing a function. For instance the `foreach` implementation is done completely manually and `reduce` and other functions use the macro `zfInlineCall` which provides Zero-DSL within a manual function implementation and also registers the function name.
The signature for creating an inline function is as in the `inlineMap` example above - each function `foo` is implemented by its `inlineFoo*(ext: ExtNimNode)` counterpart.

If the Zero-DSL should fail to create an own implementation of a function then `zfInline_dbg` instead of `zfInline` can be used to print the created function to the console, copy it - remember to add the `*` to the name - and adapt the code.

It is possible to set parameter types for the functions - for example in the `index` implementation:
```nim
zfInline index(cond: bool):
  init:
    result = -1 # index not found
  loop:
    if cond:
      return idx
```
The `cond: bool` definition adds additional compile time checks to the generated macros, so that when using the `index`-function with a different type than `bool` a compile error with the wrong parameter and the expected type is created.
In this example also the `idx` variable is replaced automatically with the running index that is increment during the loop.

Note: zfCreateExtension must be called in a compile time context. This could be in a static block or macro for example:
```nim
# Register above extensions during compile time
static:
  zfCreateExtension()
```


#### Special variables
Special variables for `zfInline` statements are:
- `it`: when used is the previous iterator, when defined with `let it = ` creates a new iterator, in the `init` section `it` refers to the first element in the underlying collection
- `idx`: the running index in the loop
- `result`: the overall result and return type of the operation
All other variables have to be defined in the `pre`-section, also when automatically assigned, e.g. when overriding the `idx` variable or when accessing a reference to the list as `listRef`.
See `intersect` or `removeDoubles` implementation in [test.nim](test.nim) as an example.
- `yield it`: opposed to setting the result this means that an iterator or a collection result is returned with `it` being added to it.

#### Setting the result
Example of `count` that sets a result:
```nim
zfInline count():
  init:
    result = 0
  loop:
    result += 1 # add one in each loop
```
Functions that set a result in any section are considered final functions - no other function may follow. Opposed to using `yield` used for iterator results:

Example of `filter` that returns a collection / iterator:
```nim
zfInline filter(cond: bool):
  loop:
    if cond:
      yield it # add the current iterator to the resulting collection
```

### Defering to Zero-DSL inside plain nim
As Zero-DSL is limited in its expressiveness - e.g. there is no possibility to defer to a different loop implementation depending on the input type - it is possible to defer to different Zero-DSL implementations in the nim code using `zfInlineCall`. Example calling two implementations - one specialized to work on lists, the other for sequences:
```nim
proc inlineFoo*(ext: ExtNimNode) {.compileTime.} =
  if ext.isListType():
    # provide list implementation
    zfInlineCall foo(param):
      ... 
  else:
    # provide plain implementation (e.g. using [])
    zfInlineCall reduce(op):
      ...
```

#### Creating compound commands with other commands
Zero-DSL can create commands as a sequence from already existing commands using the `delegate` section - which essentially calls the functions in that section.
Example is the `removeDoubles` function that returns the input sequence with unique elements. See code in [test.nim](test.nim).
```nim
zfInline removeDoubles():
# remove double elements. 
  pre:
    # initialize variables in `ext` that are used below
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
```

## Overview Table

The result type depends on the function used as last parameter.

| Command       | 1st Param | in-between | Last Param | Result Type                 |
| ------------- | --------- | ---------- | ---------- | --------------------------- |
|all            |           |            |     +      | `bool`                      |
|combinations   |   +       |    (+)     |    (+)     | `coll[Combination]`         |
|concat         |   +       |            |            | coll                        |
|exists         |           |            |     +      | bool                        |
|filter         |   +       |     +      |     +      | part coll / zeroed array    |
|find           |           |            |     +      | `int`                       |
|flatten        |   +       |     +      |     +      | coll                        |
|fold           |           |            |     +      | *                           |
|foreach        |   +       |     +      |     +      | `void`                      |
|group          |           |            |     +      | `table[*, seq[*]]`          |
|index          |           |            |     +      | `int`                       |
|indexedMap     |   +       |     +      |     +      | `seq[(int,*)]`              |
|enumerate      |           |     +      |     +      | `seq[(int,*)]`              |
|createIter     |           |            |  virtual   | iterator of given type      |
|map            |   +       |     +      |     +      | `collType[*]`               |
|partition      |           |            |     +      | `(yes:seq[*], no:seq[*])`   |
|reduce         |           |            |     +      | *                           |
|sub            |   +       |     +      |     +      | part coll / zeroed array    |
|split          |           |            |     +      | `(seq[*],...seq[*])`        |
|zip            |   +       |     +      |     +      | `seq[(*,..,*)]`             |
|to             |           |            |   virtual  | given type                  |

+ *: any type depending on given function parameters
+ coll: is the input collection
+ collType is the input collection type (without template argument)
+ "virtual" function: can only be given as last argument, but does not count as last argument.

## Debugging using `-->>`

As `zero_functional` macros are sometimes tricky to use, it can happen that the compiler crashes or that compile errors are hard to understand.

To see the actual code that is generated (provided the generation itself does not crash) you can use the `-->>` operator which prints the representation `repr` of the actual generated nim code.
It is also useful for checking what the macros are generating under the hood.

```nim
let a = [1,2,3]
a -->> foreach(echo(it))
```

will print during compilation:
```nim
block:
  for it0 in a:
    echo(it0)
```
The printed code can be copied to your actual program for further investigation.

## Compile flags

The following compile flags are supported:
* `-d:zf_iter` defaults to generating closure iterators instead of `seq` outputs.
* `-d:zf_list` same as above but generating `DoublyLinkedList`
* `-d:zf_debug_all` print all generated code

## LICENSE

MIT, Alexander Ivanov


## Contributors

Co-maintainers and authors: [Michael Schulte](https://github.com/michael72), Alexander Ivanov

Creator: Alexander Ivanov

