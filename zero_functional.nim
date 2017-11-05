import strutils, sequtils, macros

proc connectFunction: NimNode {.compileTime.} =
  var empty = newEmptyNode()
  var returnType = quote:
    seq[int]
  result = nnkLambda.newTree(
    empty,
    empty,
    empty,
    nnkFormalParams.newTree(newIdentNode(!"auto")),
    empty,
    empty,
    nnkStmtList.newTree())
  var resultIdent = newIdentNode(!"result")
  var initResult = quote:
    `resultIdent` = @[]


proc itNode(index: int): NimNode {.compileTime.} =
  newIdentNode(!("it$1" % $index))

proc aNode(index: int): NimNode {.compileTime.} =
  newIdentNode(!("a$1" % $index))

proc adapt(node: var NimNode, index: int, aIndex: int): NimNode =
  case node.kind:
  of nnkIdent:
    if $node == "it":
      return itNode(index)
    elif aIndex != -1 and $node == "a":
      return aNode(aIndex)
    else:
      return node
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return node
  else:
    for z in 0..<len(node):
      var son = node[z]
      node.add(adapt(son, index, aIndex))
    node.del(0, len(node) div 2)
    return node

proc inlineZip(a: NimNode, index: int, last: bool): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var zIdent = newIdentNode(!"z")
  var emptyIdent = newIdentNode(!"empty")
  var m = nnkCall.newTree(newIdentNode(!"min"), nnkBracket.newTree())
  var p = nnkPar.newTree()
  for z, arg in a:
    if z > 0:
      m[^1].add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, zIdent))
  var q = quote:
    var minHigh = `m`
    var `emptyIdent` = true
    for `zIdent` in low(a)..minHigh:
      var `itIdent` = `p`
      # (`a`[`zIdent`], `b`[`zIdent`])
  result = (q, q[2][^1])

proc inlineMap(f: NimNode, index: int, last: bool, indexed: bool): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var emptyIdent = newIdentNode(!"empty")
  var adaptedF = f
  adaptedF = adapt(adaptedF, index - 1, -1)
  var next: NimNode
  if indexed:
    var zIdent = newIdentNode(!"z")
    next = quote:
      (`zIdent`, `adaptedF`)
  else:
    next = adaptedF
  var q: NimNode
  if last:
    var resultIdent = newIdentNode(!"result")
    q = quote:
      if `emptyIdent`:
        `emptyIdent` = false
        `resultIdent` = @[`next`]
      else:
        `resultIdent`.add(`next`)
  else:
    q = quote:
      var `itIdent` = `next`
  result = (q, nil)

proc inlineFilter(test: NimNode, index: int, last: bool): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var itPreviousIdent = itNode(index - 1)
  var emptyIdent = newIdentNode(!"empty")
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var q: NimNode
  if last:
    var resultIdent = newIdentNode(!"result")
    q = quote:
      if `adaptedTest`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itPreviousIdent`]
        else:
          `resultIdent`.add(`itPreviousIdent`)
  else:
    q = quote:
      if `adaptedTest`:
        var `itIdent` = `itPreviousIdent`
  result = (q, q[0][^1])

proc inlineAny(test: NimNode, index: int): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var resultIdent = newIdentNode(!"result")
  var q = quote:
    `resultIdent` = false
    if `adaptedTest`:
      return true
  result = (q, nil)

proc inlineAll(test: NimNode, index: int): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var resultIdent = newIdentNode(!"result")
  var q = quote:
    `resultIdent` = true
    if not `adaptedTest`:
      return false
  result = (q, nil)

proc inlineFold(initial: NimNode, handler: NimNode, index: int, last: bool, initials: var NimNode): (NimNode, NimNode) =
  var itIdent = itNode(index)
  var adaptedHandler = handler
  adaptedHandler = adapt(adaptedHandler, index - 1, index)
  var aIdent = aNode(index)
  var q: NimNode
  if last:
    var resultIdent = newIdentNode(!"result")
    q = quote:
      `aIdent` = `adaptedHandler`
      `resultIdent` = `aIdent`
  else:
    q = quote:
      `aIdent` = `adaptedHandler`
  var i = quote:
    var `aIdent` = `initial`
  initials.add(i)
  result = (q, nil)

proc inlineSeq(node: NimNode, last: bool): (NimNode, NimNode) =
  var itIdent = itNode(0)
  var zIdent = newIdentNode(!"z")
  var emptyIdent = newIdentNode(!"empty")
  var q = quote:
    for `zIdent` in `node`:
      var `itIdent` = `node`[`zIdent`]
  result = (q, q[^1])

proc ensureLast(label: string, last: bool, node: NimNode) =
  if not last:
    error("$1 can be only last in a chain" % label, node)

proc inlineElement(node: NimNode, index: int, last: bool, initials: var NimNode): (NimNode, NimNode) =
  if node.kind == nnkCall:
    var label = $node[0]
    case label:
    of "zip":
      return inlineZip(node, index, last)
    of "map":
      return inlineMap(node[1], index, last, false)
    of "filter":
      return inlineFilter(node[1], index, last)
    of "any":
      ensureLast("any", last, node)
      return inlineAny(node[1], index)
    of "all":
      ensureLast("all", last, node)
      return inlineAll(node[1], index)
    of "indexedMap":
      return inlineMap(node[1], index, last, true)
    of "fold":
      return inlineFold(node[1], node[2], index, last, initials)
    else:
      error("$1 is unknown" % label, node)    
  else:
    if index != 0:
      error("seq supposed to be first", node)
    return inlineSeq(node, last)

proc connectHandler(args: NimNode): NimNode =
  result = connectFunction()
  var code = result[^1]
  var initials = nnkStmtList.newTree()
  result[^1].add(initials)
  for index, arg in args:
    var last = index == len(args) - 1
    var (res, newCode) = inlineElement(arg, index, last, initials)
    code.add(res)
    if newCode != nil:
      code = newCode
  result = nnkCall.newTree(result)
  echo repr(result)

macro connect*(args: varargs[untyped]): untyped =
  result = connectHandler(args)

macro `-->`*(a: untyped, b: untyped): untyped =
  assert b.kind == nnkCall
  var methods = b
  var m: seq[NimNode] = @[]
  var node = methods
  while node.kind == nnkCall:
    if node[0].kind == nnkDotExpr:
      m.add(nnkCall.newTree(node[0][^1]))
      for z, b in node:
        if z > 0:
          m[^1].add(b)
      node = node[0][0]
    elif node[0].kind == nnkIdent:
      m.add(node)
      break
    else:
      break
  var m2: seq[NimNode] = @[a]
  for z in countdown(high(m), low(m)):
    # echo repr(m[z])
    m2.add(m[z])
  var mad = nnkArgList.newTree(m2)
  result = connectHandler(mad)

proc f(a: int, b: int): int =
  # dummy
  a * b

var a = @[2, 4, 5, 11, 52, 87, 74, 53, -2]
var b = @[0, 1, 2, 3, 4, 5, 6, 7, 8]
var c = @[2, 2, 2, 2, 2, 2, 2, 2, 2]

# var n = connect(zip(a, b, c), indexedMap(f(it[0], it[1])), filter(it[0] < 10 and it[1] mod 4 > 1), map(it[1] * 2), all(it > 4))
# var n = zip(a, b, c) -->
#             indexedMap(f(it[0], it[1])).
#             filter(it[0] < 10 and it[1] mod 4 > 1).
#             map(it[1] * 2).
#             all(it > 4)
# var n = connect(zip(a, b), map(it[0] + it[1]), fold(0, a + it))

# var n = zip(a, b) -->
#             map(f(it[0], it[1])).
#             filter(it mod 4 > 1).
#             map(it * 2).
#             all(it > 4)

var n = zip(a, b).
            mapIt(f(it[0], it[1])).
            filterIt(it mod 4 > 1).
            mapIt(it * 2).
            allIt(it > 4)

# var n = zip(a, b) --> map(it[0] + it[1]).fold(0, a + it)
# var n = connect(zip(a, b), map(it[0] + it[1]), any(it > 2))
# var n = connect(a, map(it), any(it > 2))
# var n = a --> map(it).filter(true).any(it > 2)
echo n

