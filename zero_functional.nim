import strutils, sequtils, macros

proc connectFunction: NimNode {.compileTime.} =
  var empty = newEmptyNode()
  result = nnkLambda.newTree(
    empty,
    empty,
    empty,
    nnkFormalParams.newTree(newIdentNode("auto")),
    empty,
    empty,
    nnkStmtList.newTree())

proc itNode(index: int): NimNode {.compileTime.} =
  newIdentNode("it$1" % $index)

proc aNode(index: int): NimNode {.compileTime.} =
  newIdentNode("a$1" % $index)

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

proc getStmtList(node: NimNode, removeNil = true): NimNode =
  var child = node
  while child.len > 0:
    child = child[^1]
    if child.kind == nnkStmtList:
      if removeNil:
        if child.len > 0 and child[^1].kind == nnkNilLit:
          child.del(1,1)
      return child
  return nil


proc inlineZip(a: NimNode, index: int): NimNode =
  var itIdent = itNode(index)
  var zIdent = newIdentNode("z")
  var m = nnkCall.newTree(newIdentNode("min"), nnkBracket.newTree())
  var p = nnkPar.newTree()
  var z = 0
  for arg in a:
    if z > 0:
      m[^1].add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, zIdent))
    z += 1
  var q = quote:
    var minHigh = `m`
    for `zIdent` in 0..minHigh:
      var `itIdent` = `p`
  result = q

proc inlineMap(f: NimNode, index: int, last: bool, indexed: bool): NimNode =
  var itIdent = itNode(index)
  var emptyIdent = newIdentNode("empty")
  var adaptedF = f
  adaptedF = adapt(adaptedF, index - 1, -1)
  var next: NimNode
  if indexed:
    var zIdent = newIdentNode("z")
    next = quote:
      (`zIdent`, `adaptedF`)
  else:
    next = adaptedF
  if last:
    var resultIdent = newIdentNode("result")
    result = quote:
      if `emptyIdent`:
        `emptyIdent` = false
        `resultIdent` = @[`next`]
      else:
        `resultIdent`.add(`next`)
  else:
    result = quote:
      var `itIdent` = `next`

proc inlineFilter(test: NimNode, index: int, last: bool): NimNode =
  var itIdent = itNode(index)
  var itPreviousIdent = itNode(index - 1)
  var emptyIdent = newIdentNode("empty")
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  if last:
    var resultIdent = newIdentNode("result")
    result = quote:
      if `adaptedTest`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itPreviousIdent`]
        else:
          `resultIdent`.add(`itPreviousIdent`)
  else:
    result = quote:
      if `adaptedTest`:
        let `itIdent` = `itPreviousIdent`

proc inlineExists(test: NimNode, index: int): NimNode =
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var resultIdent = newIdentNode("result")
  result = quote:
    `resultIdent` = false
    if `adaptedTest`:
      return true

proc inlineAll(test: NimNode, index: int): NimNode =
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var resultIdent = newIdentNode("result")
  result = quote:
    `resultIdent` = true
    if not `adaptedTest`:
      return false

proc inlineFold(initial: NimNode, handler: NimNode, index: int, last: bool, initials: var NimNode): NimNode =
  var adaptedHandler = handler
  adaptedHandler = adapt(adaptedHandler, index - 1, index)
  var aIdent = aNode(index)
  var i = quote:
    var `aIdent` = `initial`
  initials.add(i)
  if last:
    var resultIdent = newIdentNode("result")
    result = quote:
      `aIdent` = `adaptedHandler`
      `resultIdent` = `aIdent`
  else:
    result = quote:
      `aIdent` = `adaptedHandler`

proc inlineEmpty(): NimNode {.compileTime.} =
  let emptyIdent = newIdentNode("empty")
  let q = quote:
    var `emptyIdent` = true
  result = nnkStmtList.newTree().add(q)

proc inlineSeq(node: NimNode, needsEmpty: bool): NimNode =
  var itIdent = itNode(0)
  var zIdent = newIdentNode("z")
  let q = quote:
    for `zIdent`, `itIdent` in `node`:
      nil
  if needsEmpty:
    result = inlineEmpty().add(q)
  else:
    result = q

proc ensureLast(label: string, last: bool, node: NimNode) =
  if not last:
    error("$1 can be only last in a chain" % label, node)

proc inlineElement(node: NimNode, index: int, last: bool, needsEmpty: bool, initials: var NimNode): NimNode =
  if node.kind == nnkCall:
    var label = $node[0]
    case label:
    of "zip":
      return inlineZip(node, index)
    of "map":
      return inlineMap(node[1], index, last, false)
    of "filter":
      return inlineFilter(node[1], index, last)
    of "exists":
      ensureLast("exists", last, node)
      return inlineExists(node[1], index)
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
    return inlineSeq(node, needsEmpty)

proc connectHandler(args: NimNode): NimNode =
  result = connectFunction()
  var code = result[^1]
  var initials = nnkStmtList.newTree()
  result[^1].add(initials)
  var needsEmpty = false
  if args.len > 0 and args[^1].len > 0:
    let lastCall = $args[^1][0]
    needsEmpty = (lastCall == "map") or (lastCall == "indexedMap") or (lastCall == "filter")
    if needsEmpty and args[0].kind == nnkCall:
      code.add(inlineEmpty())
  var index = 0
  for arg in args:
    let last = index == (len(args) - 1)
    let res = inlineElement(arg, index, last, needsEmpty, initials)
    let newCode = res.getStmtList() 
    code.add(res)    
    if newCode != nil:
      code = newCode
    index += 1
  result = nnkCall.newTree(result)
  #echo repr(result)

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
      var z = 0
      for b in node:
        if z > 0:
          m[^1].add(b)
        z += 1
      node = node[0][0]
    elif node[0].kind == nnkIdent:
      m.add(node)
      break
    else:
      break
  var m2: seq[NimNode] = @[a]
  for z in countdown(high(m), low(m)):
    m2.add(m[z])
  var mad = nnkArgList.newTree(m2)
  result = connectHandler(mad)
  # echo repr(result)
