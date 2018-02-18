import strutils, sequtils, macros

const iteratorVariableName = "it"
const accuVariableName = "a"

const internalIteratorName = "__" & iteratorVariableName & "__"
const useInternalAccu = accuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & accuVariableName & "__" else: "result"
const indexVariableName = "__index__"
const emptyCheckName = "__empty__"

type 
  ExtNimNode = ref object
    node: NimNode
    index: int
    isLastItem: bool
    initials: NimNode
    nextIndexInc: int

proc newExtNode(node: NimNode, 
                   index: int, 
                   isLastItem: bool,
                   initials: NimNode,
                   nextIndexInc = 0): ExtNimNode =
  result = ExtNimNode(node: node, 
                      index: index, 
                      isLastItem: isLastItem,
                      initials: initials,
                      nextIndexInc: nextIndexInc)

proc clone(x: ExtNimNode): ExtNimNode {.compileTime.} =
    result = x.node.newExtNode(index = x.index, 
                               isLastItem = x.isLastItem, 
                               initials = x.initials,  
                               nextIndexInc = x.nextIndexInc)

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

proc mkItNode(index: int) : NimNode {.compileTime.} = 
  newIdentNode(internalIteratorName & ("$1" % $index))

proc itNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index)

proc prevItNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index-1)

proc res(ext: ExtNimNode): NimNode {.compileTime.} =
  result = newIdentNode("result")

proc adapt(ext: ExtNimNode, index=1): NimNode {.compileTime.} =
  let fun = ext.node[index]
  case fun.kind:
  of nnkIdent:
    if $fun == iteratorVariableName:
      return ext.prevItNode()
    elif useInternalAccu and $fun == accuVariableName:
      return newIdentNode(internalAccuName)
    else:
      return fun
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return fun
  else:
    for z in 0..<fun.len:
      let son = ext.clone()
      son.node = fun
      fun.add(son.adapt(index=z))
    fun.del(0, fun.len div 2)
    return fun

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
    
proc inlineZip(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let idxIdent = newIdentNode(indexVariableName)
  let m = nnkCall.newTree(newIdentNode("min"), nnkBracket.newTree())
  let p = nnkPar.newTree()
  var z = 0
  for arg in ext.node:
    if z > 0:
      m[^1].add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, idxIdent))
    z += 1
  ext.node = quote:
    let minHigh = `m`
    for `idxIdent` in 0..minHigh:
      let `itIdent` = `p`
  ext.nextIndexInc = 1
  result = ext
    
proc inlineMap(ext: ExtNimNode, indexed = false): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let adaptedF = ext.adapt()
  var next: NimNode
  if indexed:
    let idxIdent = newIdentNode(indexVariableName)
    next = quote:
      (`idxIdent`, `adaptedF`)
  else:
    next = adaptedF

  if ext.isLastItem:
    let emptyIdent = newIdentNode(emptyCheckName)
    let resultIdent = ext.res
    ext.node = quote:
      if `emptyIdent`:
        `emptyIdent` = false
        `resultIdent` = @[`next`]
      else:
        `resultIdent`.add(`next`)
  else:
    ext.node = quote:
      let `itIdent` = `next`
  ext.nextIndexInc = 1
  result = ext

proc inlineFilter(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let adaptedTest = ext.adapt()
  
  if ext.isLastItem:
    let emptyIdent = newIdentNode(emptyCheckName)
    let resultIdent = ext.res
    let itPrevIdent = ext.prevItNode()
    ext.node = quote:
      if `adaptedTest`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itPrevIdent`]
        else:
          `resultIdent`.add(`itPrevIdent`)
  else:
    ext.node = quote :
        if `adaptedTest`:
          nil
  result = ext


proc inlineExists(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let adaptedTest = ext.adapt()
  let resultIdent = ext.res
  let i = quote:
    `resultIdent` = false
  ext.initials.add(i)
  ext.node = quote:
    if `adaptedTest`:
      return true
  result = ext

proc inlineAll(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let adaptedTest = ext.adapt()
  let resultIdent = ext.res
  let i = quote:
    `resultIdent` = true
  ext.initials.add(i)
  ext.node = quote:
    if not `adaptedTest`:
      return false
  result = ext

proc inlineForeach(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let adaptedExpression = ext.adapt()
  ext.node = quote:
    `adaptedExpression`
  result = ext
      
proc inlineIndex(ext: ExtNimNode): ExtNimNode{.compileTime.} =
  let adaptedTest = ext.adapt()
  var idxIdent = newIdentNode(indexVariableName)
  var resultIdent = ext.res
  let i = quote:
    `resultIdent` = -1 # index not found
  ext.initials.add(i)
  ext.node = quote:
    if `adaptedTest`:
      return `idxIdent` # return index
  result = ext  

proc inlineFold(ext: ExtNimNode): ExtNimNode{.compileTime.} =
  let initialValue = ext.node[1]
  let resultIdent = ext.res
  let foldOperation = ext.adapt(index=2)

  var i : NimNode 
  if useInternalAccu:
    let accuIdent = newIdentNode(internalAccuName) 
    i = quote:
      var `accuIdent` = `initialValue`
    ext.node = quote:
      `accuIdent` = `foldOperation`
      `resultIdent` = `accuIdent`
  else:
    i = quote:
      `resultIdent` = `initialValue`
    ext.node = quote:
      `resultIdent` = `foldOperation`
  
  ext.initials.add(i)

  result = ext

proc inlineEmpty(): NimNode {.compileTime.} =
  let emptyIdent = newIdentNode(emptyCheckName)
  let q = quote:
    var `emptyIdent` = true
  result = nnkStmtList.newTree().add(q)

proc inlineSeq(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let node = ext.node
  let idxIdent = newIdentNode(indexVariableName)
  ext.node = quote:
    for `idxIdent`, `itIdent` in `node`:
      nil
  ext.nextIndexInc = 1
  result = ext
  
proc ensureLast(ext: ExtNimNode) {.compileTime.} =
  if not ext.isLastItem:
    error("$1 can be only last in a chain" % $ext.node[0], ext.node)

proc ensureFirst(ext: ExtNimNode) {.compileTime.} =
  if ext.index > 0:
    error("$1 supposed to be first" % $ext.node[0], ext.node)

proc ensureParameters(ext: ExtNimNode, no: int) {.compileTime.} = 
    if ext.node.len <= no:
      error($ext.node[0] & " needs at least $1 parameter(s)" % $no, ext.node)
        
proc inlineElement(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  if ext.node.kind == nnkCall:
    ext.ensureParameters(1)
    let label = $ext.node[0]
    case label:
    of "zip":
        ext.ensureFirst()
        return ext.inlineZip()
    of "map":
      return ext.inlineMap()
    of "filter":
      return ext.inlineFilter()
    of "exists":
      ext.ensureLast()
      return ext.inlineExists()
    of "all":
      ext.ensureLast()
      return ext.inlineAll()
    of "index":
      ext.ensureLast()
      return ext.inlineIndex()
    of "indexedMap":
      return ext.inlineMap(indexed=true)
    of "fold":
      ext.ensureLast()
      ext.ensureParameters(2)
      return ext.inlineFold()
    of "foreach":
      return ext.inlineForeach()
    of "any":
      warning("any is deprecated - use exists instead")
      return ext.inlineExists()
    else:
      error("$1 is unknown" % label, ext.node)    
  else:
    ext.ensureFirst()
    return ext.inlineSeq()

proc iterHandler(args: NimNode): NimNode {.compileTime.} =
  result = iterFunction()
  var code = result[^1]
  let initials = nnkStmtList.newTree()
  result[^1].add(initials)

  if args.len > 0 and args[^1].len > 0:
    let lastCall = $args[^1][0]
    if (lastCall == "map") or (lastCall == "indexedMap") or (lastCall == "filter"):
        code.add(inlineEmpty()) # need a `var empty = true;` at the beginning
  
  var index = 0
  for arg in args:
    let last = arg == args[^1]
    let ext = arg.newExtNode(index, last, initials).inlineElement()
    let newCode = ext.node.getStmtList()
    code.add(ext.node)
    if newCode != nil:
      code = newCode
    index += ext.nextIndexInc

  result = nnkCall.newTree(result)

macro connect*(args: varargs[untyped]): untyped =
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
