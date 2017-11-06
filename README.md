# zero-functional

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

## Supported methods

Those are not exactly the functions from sequtils, they have the some naming and almost the same behavior

The macro works as

```nim
sequence --> map(..).any(..)
```

or 

```nim
zip(a, b, c) --> map(..).any(..)
```

You can also use 

```nim
connect(sequence, map(..), any(..))
```

The methods work with auto it variable

### map

```nim
sequence --> map(..)
```

### filter

```nim
sequence --> filter(..)
```

### zip

zip is more special, it can be for now only in the begining, and it can work with n sequences

### any

any can be used only finally

```nim
sequence --> map(..).any(..)
```

### all

all can be used only finally

```nim
sequence --> map(..).all(..)
```

### indexedMap

Generates a tuple (position, it)

```nim
var n = zip(a, b, c) -->
            indexedMap(f(it[0], it[1])).
            filter(it[0] < 10 and it[1] mod 4 > 1).
            map(it[1] * 2).
            all(it > 4)
```

## fold

Currently a left fold (as easier to combine with my technique)

the sequtils `a` is `a`, `b` is `it`

```nim
var n = zip(a, b) --> map(it[0] + it[1]).fold(0, a + it)
```

### LICENSE

MIT, Alexander Ivanov

