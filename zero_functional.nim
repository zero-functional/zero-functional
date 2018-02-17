import strutils, sequtils, macros

const indexVariableName = "__index__"
const emptyCheckName = "__empty__"
const iteratorVariableName = "it"
const accuVariableName = "_"

type 
  ExtNimNode = ref object
    node: NimNode
    fun: NimNode
    index: int
    isLastItem: bool
    accuIndex: int
    needsEmpty: bool
    initValues: NimNode

proc newExtNode(node: NimNode, 
                   fun: NimNode, 
                   index: int, 
                   isLastItem = false, 
                   accuIndex = -1, 
                   needsEmpty = false, 
                   initValues = newEmptyNode()): ExtNimNode =
  result = ExtNimNode(node: node, 
                      fun: fun, 
                      index: index, 
                      isLastItem: isLastItem, 
                      accuIndex: accuIndex, 
                      needsEmpty: needsEmpty, 
                      initValues: initValues)

proc clone(x: ExtNimNode): ExtNimNode {.compileTime.} =
    result = x.node.newExtNode(fun = x.fun, index = x.index, isLastItem = x.isLastItem, accuIndex = x.accuIndex, needsEmpty = x.needsEmpty, initValues = x.initValues)

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

proc itNode(index: int) : NimNode {.compileTime.} = 
  newIdentNode(("__" & iteratorVariableName & ("__$1" % $index)))

proc aNode(accuIndex: int): NimNode {.compileTime.} =
  newIdentNode((accuVariableName & ("$1" % $accuIndex)))

# TODO lastStatement - also new function: reverse list! (by reversing the iterator), foreach (?)
#proc lastStatement(node: NimNode): NimNode {.compileTime.} = 
#  for child in node:

proc adapt(ext: ExtNimNode): NimNode {.compileTime.} =
  case ext.fun.kind:
  of nnkIdent:
    if $ext.fun == iteratorVariableName:
      return itNode(ext.index-1)
    elif ext.accuIndex != -1 and $ext.fun == accuVariableName:
      return ext.accuIndex.aNode()
    else:
      return ext.fun
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return ext.fun
  else:
    for z in 0..<ext.fun.len:
      let son = ext.clone()
      son.fun = ext.fun[z]
      ext.fun.add(son.adapt())
    ext.fun.del(0, ext.fun.len div 2)
    return ext.fun

proc inlineZip(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let itIdent = ext.index.itNode()
  let idxIdent = newIdentNode(indexVariableName)
  let m = nnkCall.newTree(newIdentNode("min"), nnkBracket.newTree())
  let p = nnkPar.newTree()
  var z = 0
  for arg in ext.node:
    if z > 0:
      m[^1].add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, idxIdent))
    z += 1
  let q = quote:
    let minHigh = `m`
    for `idxIdent` in 0..minHigh:
      let `itIdent` = `p`
  result = (q, q[1][^1], ext.index+1)

proc inlineMap(ext: ExtNimNode, indexed = false): (NimNode, NimNode, int) {.compileTime.} =
  let itIdent = ext.index.itNode()
  let adaptedF = ext.adapt()
  var next: NimNode
  if indexed:
    let idxIdent = newIdentNode(indexVariableName)
    next = quote:
      (`idxIdent`, `adaptedF`)
  else:
    next = adaptedF
  var q: NimNode
  if ext.isLastItem:
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
  result = (q, nil, ext.index+1)

proc inlineFilter(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let itPreviousIdent = (ext.index-1).itNode()
  let adaptedTest = ext.adapt()
  
  if ext.isLastItem:
    let emptyIdent = newIdentNode(emptyCheckName)
    let resultIdent = newIdentNode("result")
    let q = quote:
      if `adaptedTest`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itPreviousIdent`]
        else:
          `resultIdent`.add(`itPreviousIdent`)
    result = (q, q[0][^1], ext.index)
  else:
    let q = quote :
        if `adaptedTest`:
          nil
    q[0][0][1].del(0, 1)
    #[
        check with echo(treeRepr(q))

        StmtList (q)
            IfStmt (q[0])
                ElifBranch (q[0][0])
                    <<Conditition>>
                    StmtList (q[0][0][1])
                        <<LetSection>>..
    ]#
    result = (q, q[0][0][1], ext.index)
 

proc inlineExists(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let adaptedTest = ext.adapt()
  let resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = false
    if `adaptedTest`:
      return true
  result = (q, nil, ext.index)

proc inlineAll(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let adaptedTest = ext.adapt()
  let resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = true
    if not `adaptedTest`:
      return false
  result = (q, nil, ext.index)

proc inlineForeach(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let adaptedExpression = ext.adapt()
  let q = quote:
    `adaptedExpression`
  result = (q, nil, ext.index)
      
proc inlineIndex(ext: ExtNimNode): (NimNode, NimNode, int){.compileTime.} =
  let adaptedTest = ext.adapt()
  var idxIdent = newIdentNode(indexVariableName)
  var resultIdent = newIdentNode("result")
  let q = quote:
    `resultIdent` = -1 # index not found
    if `adaptedTest`:
      return `idxIdent` # return index
  result = (q, nil, ext.index)  

proc inlineFold(ext: ExtNimNode): (NimNode, NimNode, int){.compileTime.} =
  ext.accuIndex = ext.index+1
  let initialValue = ext.node[1]
  var initials = ext.initValues
  let accuIdent = ext.accuIndex.aNode()
  # TODO unhack hacking....
  ext.fun = ext.node[2]
  let adaptedHandler = ext.adapt()
  var q: NimNode
  if ext.isLastItem:
    let resultIdent = newIdentNode("result")
    q = quote:
      `accuIdent` = `adaptedHandler`
      `resultIdent` = `accuIdent`
  else:
    q = quote:
      `accuIdent` = `adaptedHandler`
  let i = quote:
    var `accuIdent` = `initialValue`
  initials.add(i)
  result = (q, nil, ext.index)

proc inlineEmpty(): NimNode {.compileTime.} =
  let emptyIdent = newIdentNode(emptyCheckName)
  result = quote:
    var `emptyIdent` = true
 
proc inlineSeq(ext: ExtNimNode): (NimNode, NimNode, int) {.compileTime.} =
  let itIdent = itNode(0)
  let node = ext.node
  let idxIdent = newIdentNode(indexVariableName)
  var q = quote:
    for `idxIdent`, `itIdent` in `node`:
      nil
  q[0][^1].del(0, 1) # remove nil statement from above quote
  if ext.needsEmpty:
    q = inlineEmpty().add(q) # insert 'var empty = true' to statement list in q
    result = (q, q[1][0][^1], ext.index+1)
  else:
    result = (q, q[0][^1], ext.index+1)

proc ensureLast(label: string, last: bool, node: NimNode) {.compileTime.} =
  if not last:
    error("$1 can be only last in a chain" % label, node)

proc ensureNotLast(label: string, last: bool, node: NimNode) {.compileTime.} =
  if last:
    error("$1 can be only last in a chain" % label, node)
        
proc inlineElement(node: NimNode, index: int, last: bool, needsEmpty: bool, initials: NimNode): (NimNode, NimNode, int) {.compileTime.} =
  if node.kind == nnkCall:
    var ext = node.newExtNode(fun = node[1], index = index, isLastItem = last, needsEmpty = needsEmpty)
    let label = $node[0]
    case label:
    of "zip":
        ensureNotLast(label, last, node)
        return ext.inlineZip()
    of "map":
      return ext.inlineMap()
    of "filter":
      return ext.inlineFilter()
    of "exists":
      ensureLast(label, last, node)
      return ext.inlineExists()
    of "all":
      ensureLast(label, last, node)
      return ext.inlineAll()
    of "index":
      ensureLast(label, last, node)
      return ext.inlineIndex()
    of "indexedMap":
      return ext.inlineMap(indexed=true)
    of "fold":
      ext.accuIndex = ext.index
      ext.initValues = initials
      return ext.inlineFold()
    of "foreach":
      return ext.inlineForeach()
    else:
      error("$1 is unknown" % label, node)    
  else:
    if index != 0:
      error("seq supposed to be first", node)
    var ext = node.newExtNode(fun = node, index = index, isLastItem = last, needsEmpty = needsEmpty)
    return ext.inlineSeq()

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
    let last = arg == args[^1]
    let (res, newCode, newIndex) = inlineElement(arg, index, last, needsEmpty, initials)
    code.add(res)
    if newCode != nil:
      code = newCode
    index = newIndex
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

