import strutils, sequtils, macros

const indexVariableName = "__index__"
const emptyCheckName = "__empty__"
const iteratorVariableName = "it"
const accuVariableName = "_"

proc iterFunction: NimNode {.compileTime.} =
  let empty = newEmptyNode()
  result = nnkLambda.newTree(
    empty,
    empty,
    empty,
    nnkFormalParams.newTree(newIdentNode("auto")),
    empty,
    empty,
    nnkStmtList.newTree())

proc itNode(index: int): NimNode {.compileTime.} =
  newIdentNode(("__" & iteratorVariableName & ("__$1" % $index)))

proc aNode(index: int): NimNode {.compileTime.} =
  newIdentNode((accuVariableName & ("$1" % $index)))

proc adapt(node: NimNode, index: int, aIndex: int): NimNode {.compileTime.} =
  case node.kind:
  of nnkIdent:
    if $node == iteratorVariableName:
      return itNode(index)
    elif aIndex != -1 and $node == accuVariableName:
      return aNode(aIndex)
    else:
      return node
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return node
  else:
    for z in 0..<len(node):
      let son = node[z]
      node.add(adapt(son, index, aIndex))
    node.del(0, len(node) div 2)
    return node

proc inlineZip(a: NimNode, index: int): (NimNode, NimNode) {.compileTime.} =
  let itIdent = itNode(index)
  let idxIdent = newIdentNode(indexVariableName)
  let m = nnkCall.newTree(newIdentNode("min"), nnkBracket.newTree())
  let p = nnkPar.newTree()
  var z = 0
  for arg in a:
    if z > 0:
      m[^1].add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, idxIdent))
    z += 1
  let q = quote:
    let minHigh = `m`
    for `idxIdent` in 0..minHigh:
      let `itIdent` = `p`
  result = (q, q[1][^1])

proc inlineMap(f: NimNode, index: int, last: bool, indexed: bool): (NimNode, NimNode) {.compileTime.} =
  let itIdent = itNode(index)
  var adaptedF = f
  adaptedF = adapt(adaptedF, index - 1, -1)
  var next: NimNode
  if indexed:
    let idxIdent = newIdentNode(indexVariableName)
    next = quote:
      (`idxIdent`, `adaptedF`)
  else:
    next = adaptedF
  var q: NimNode
  if last:
    let emptyIdent = newIdentNode(emptyCheckName)
    let resultIdent = newIdentNode("result")
    q = quote:
      if `emptyIdent`:
        `emptyIdent` = false
        `resultIdent` = @[`next`]
      else:
        `resultIdent`.add(`next`)
  else:
    q = quote:
      let `itIdent` = `next`
  result = (q, nil)

proc inlineFilter(test: NimNode, index: int, last: bool): (NimNode, NimNode) {.compileTime.} =
  let itIdent = itNode(index)
  let itPreviousIdent = itNode(index - 1)
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  
  if last:
    let emptyIdent = newIdentNode(emptyCheckName)
    let resultIdent = newIdentNode("result")
    let q = quote:
      if `adaptedTest`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itPreviousIdent`]
        else:
          `resultIdent`.add(`itPreviousIdent`)
    result = (q, q[0][^1])
  else:
    let q = quote :
        if `adaptedTest`:
            let `itIdent` = `itPreviousIdent`
    #[
        check with echo(treeRepr(q))

        StmtList (q)
            IfStmt (q[0])
                ElifBranch (q[0][0])
                    <<Conditition>>
                    StmtList (q[0][0][1])
                        <<LetSection>>..
    ]#
    result = (q, q[0][0][1])
 

proc inlineExists(test: NimNode, index: int): (NimNode, NimNode) {.compileTime.} =
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  let resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = false
    if `adaptedTest`:
      return true
  result = (q, nil)

proc inlineAll(test: NimNode, index: int): (NimNode, NimNode) {.compileTime.} =
  var adaptedTest = test
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  let resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = true
    if not `adaptedTest`:
      return false
  result = (q, nil)

proc inlineForeach(expression: NimNode, index: int, last: bool): (NimNode, NimNode) {.compileTime.} =
  var adaptedExpression = expression
  adaptedExpression = adapt(adaptedExpression, index - 1, -1)
  let q = quote:
    `adaptedExpression`
  if not last:
    let itIdent = itNode(index)
    let itPreviousIdent = itNode(index - 1)
    let q2 = quote:
      let `itIdent` = `itPreviousIdent`
    q.add(q2)
  result = (q, nil)
      
proc inlineIndex(test: NimNode, index: int): (NimNode, NimNode){.compileTime.} =
  var adaptedTest = test
  var idxIdent = newIdentNode(indexVariableName)
  adaptedTest = adapt(adaptedTest, index - 1, -1)
  var resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = -1 # index not found
    if `adaptedTest`:
      return `idxIdent` # return index
  result = (q, nil)  

proc inlineFold(initial: NimNode, handler: NimNode, index: int, last: bool, initials: NimNode): (NimNode, NimNode){.compileTime.} =
  var adaptedHandler = handler
  adaptedHandler = adapt(adaptedHandler, index - 1, index)
  let accuIdent = aNode(index)
  var q: NimNode
  if last:
    let resultIdent = newIdentNode("result")
    q = quote:
      `accuIdent` = `adaptedHandler`
      `resultIdent` = `accuIdent`
  else:
    q = quote:
      `accuIdent` = `adaptedHandler`
  let i = quote:
    var `accuIdent` = `initial`
  initials.add(i)
  result = (q, nil)

proc inlineEmpty(): NimNode {.compileTime.} =
  let emptyIdent = newIdentNode(emptyCheckName)
  result = quote:
    var `emptyIdent` = true
 
proc inlineSeq(node: NimNode, needsEmpty: bool): (NimNode, NimNode) {.compileTime.} =
  let itIdent = itNode(0)
  let idxIdent = newIdentNode(indexVariableName)
  var q = quote:
    for `idxIdent`, `itIdent` in `node`:
      nil
  q[0][^1].del(0, 1) # remove nil statement from above quote
  if needsEmpty:
    q = inlineEmpty().add(q) # insert 'var empty = true' to statement list in q
    result = (q, q[1][0][^1])
  else:
    result = (q, q[0][^1])

proc ensureLast(label: string, last: bool, node: NimNode) {.compileTime.} =
  if not last:
    error("$1 can be only last in a chain" % label, node)

proc ensureNotLast(label: string, last: bool, node: NimNode) {.compileTime.} =
  if last:
    error("$1 can be only last in a chain" % label, node)
        
proc inlineElement(node: NimNode, index: int, last: bool, needsEmpty: bool, initials: NimNode): (NimNode, NimNode) {.compileTime.} =
  if node.kind == nnkCall:
    let label = $node[0]
    case label:
    of "zip":
        ensureNotLast(label, last, node)
        return inlineZip(node, index)
    of "map":
      return inlineMap(node[1], index, last, false)
    of "filter":
      return inlineFilter(node[1], index, last)
    of "exists":
      ensureLast(label, last, node)
      return inlineExists(node[1], index)
    of "all":
      ensureLast(label, last, node)
      return inlineAll(node[1], index)
    of "index":
      ensureLast(label, last, node)
      return inlineIndex(node[1], index)
    of "indexedMap":
      return inlineMap(node[1], index, last, true)
    of "fold":
      return inlineFold(node[1], node[2], index, last, initials)
    of "foreach":
      return inlineForeach(node[1], index, last)
    else:
      error("$1 is unknown" % label, node)    
  else:
    if index != 0:
      error("seq supposed to be first", node)
    return inlineSeq(node, needsEmpty)

proc iterHandler(args: NimNode): NimNode {.compileTime.} =
  result = iterFunction()
  var code = result[^1]
  let initials = nnkStmtList.newTree()
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
    let (res, newCode) = inlineElement(arg, index, last, needsEmpty, initials)
    code.add(res)
    if newCode != nil:
      code = newCode
    index += 1
  result = nnkCall.newTree(result)

macro inline_iter*(args: varargs[untyped]): untyped =
  result = iterHandler(args)

proc delegateMacro(a: NimNode, b:NimNode): NimNode =
  assert b.kind == nnkCall
  let methods = b
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
  let mad = nnkArgList.newTree(m2)
  result = iterHandler(mad)


macro `-->`*(a: untyped, b: untyped): untyped =
  result = delegateMacro(a,b)

macro `=>`*(a: untyped, b: untyped): untyped =
  result = delegateMacro(a,b)

