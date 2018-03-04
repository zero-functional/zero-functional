import strutils, sequtils, macros, options, sets, lists, typetraits

const iteratorVariableName = "it"
const accuVariableName = "a"
const indexVariableName = "idx"

const internalIteratorName = "__" & iteratorVariableName & "__"
const useInternalAccu = accuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & accuVariableName & "__" else: "result"
const emptyIdentifier = "__empty__"

type 
  ExtNimNode = ref object ## Store additional info the current NimNode used in the inline... functions
    node: NimNode     ## the current working node / the current function
    index: int        ## index used for the created iterator - 0 for the first 
    isLastItem: bool  ## true if the current item is the last item in the command chain
    initials: NimNode ## code section before the first iterator where variables can be defined
    finals: NimNode   ## code to set the final operations, e.g. the result
    listRef:  NimNode ## reference to the list the iterator is working on
    nextIndexInc: bool ## if set to true the index will be increment by 1 for the next iterator 
  
type

  FiniteIndexable[T] = concept a
    a.low() is int
    a.high() is int
    a[int]

  FiniteIndexableLen[T] = concept a
    a.len() is int
    a[int]


static: # need to use var to be able to concat
  let FORCE_SEQ_HANDLERS = ["indexedMap", "filterSeq", "mapSeq", "subSeq", "flatten", "zip"].toSet
  var SEQUENCE_HANDLERS = ["map", "filter", "sub", "subSeq"].toSet
  var HANDLERS = ["exists", "any", "all", "index", "fold", "foreach", "find", "del"].toSet
  SEQUENCE_HANDLERS.incl(FORCE_SEQ_HANDLERS)
  HANDLERS.incl(SEQUENCE_HANDLERS)

## iterator over tuples (needed for flatten to work on tuples, e.g. from zipped lists)
iterator items*[T: tuple](a:T) : untyped = 
  for i in a.fields:
    yield i

## iterate over concept FiniteIndexable
iterator items*[T: FiniteIndexable](f:T) : untyped =
  for i in f.low()..f.high():
    yield f[i]

## iterate over concept FiniteIndexable
iterator items*[T: FiniteIndexableLen](f:T) : untyped =
  for i in 0..<f.len():
    yield f[i]

proc compileTimeTypeInfer(node: NimNode): NimNode =
  let x = node[0]
  result = quote:
    `x`.type

proc newExtNode(node: NimNode, 
                   index: int, 
                   isLastItem: bool,
                   initials: NimNode,
                   finals: NimNode,
                   listRef: NimNode,
                   nextIndexInc = false): ExtNimNode =
  result = ExtNimNode(node: node, 
                      index: index, 
                      isLastItem: isLastItem,
                      initials: initials,
                      finals: finals,
                      listRef: listRef,
                      nextIndexInc: nextIndexInc)

proc clone(x: ExtNimNode): ExtNimNode {.compileTime.} =
    result = x.node.newExtNode(index = x.index, 
                               isLastItem = x.isLastItem, 
                               initials = x.initials,
                               finals = x.finals,
                               listRef = x.listRef,
                               nextIndexInc = x.nextIndexInc)

proc iterFunction(tpe: NimNode): NimNode {.compileTime.} =
  let empty = newEmptyNode()
  result = nnkLambda.newTree(
    empty,
    empty,
    empty,
    nnkFormalParams.newTree(tpe),
    empty,
    empty,
    nnkStmtList.newTree())

proc mkItNode(index: int) : NimNode {.compileTime.} = 
  newIdentNode(internalIteratorName & ("$1" % $index))

proc itNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index)

proc prevItNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index - 1)

proc res(ext: ExtNimNode): NimNode {.compileTime.} =
  result = newIdentNode("result")

proc adapt(ext: ExtNimNode, index=1, inFold=false): NimNode {.compileTime.} =
  let fun = ext.node[index]
  case fun.kind:
  of nnkIdent:
    if $fun == iteratorVariableName:
      return ext.prevItNode()
    elif inFold and useInternalAccu and $fun == accuVariableName:
      return newIdentNode(internalAccuName)
    else:
      return fun
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return fun
  else:
    for z in 0..<fun.len:
      let son = ext.clone()
      son.node = fun
      fun.add(son.adapt(index=z, inFold=inFold))
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
  ext.nextIndexInc = true
  result = ext

proc inlineAddElem(ext: ExtNimNode, addItem: NimNode, toSeq: bool = false): NimNode {.compileTime.} = 
  let resultIdent = ext.res
  let idxIdent = newIdentNode(indexVariableName)
  if not toSeq:
    quote:
      when compiles(`resultIdent`.add(`addItem`)):
        `resultIdent`.add(`addItem`)
      else:
        when compiles(`resultIdent`.append(`addItem`)):
          `resultIdent`.append(`addItem`)
        else:
          when compiles(`resultIdent`[`idxIdent`]):
            `resultIdent`[`idxIdent`] = `addItem`
          else:
            static:
              assert(false, ": Need either add, append or []= operator implemented to add elements")
  else:
    let emptyIdent = newIdentNode(emptyIdentifier)
    quote:
      if `emptyIdent`:
        `emptyIdent` = false
        `resultIdent` = @[`addItem`]
      else:
        `resultIdent`.add(`addItem`)

proc inlineMap(ext: ExtNimNode, indexed: bool = false, toSeq: bool = false): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let adaptedF = ext.adapt()
  let idxIdent = newIdentNode(indexVariableName)
  var next: NimNode
  
  if indexed:
    next = quote:
      (`idxIdent`, `adaptedF`)
  else:
    next = adaptedF

  if ext.isLastItem:
    ext.node = ext.inlineAddElem(next, toSeq)
  else:
    ext.node = quote:
      let `itIdent` = `next`
  ext.nextIndexInc = true
  result = ext

proc inlineFilter(ext: ExtNimNode, toSeq: bool = false): ExtNimNode {.compileTime.} =
  let adaptedTest = ext.adapt()
  if ext.isLastItem:
    let push = ext.inlineAddElem(ext.prevItNode(), toSeq)
    ext.node = quote:
      if `adaptedTest`:
        `push`
  else:
    ext.node = quote :
        if `adaptedTest`:
          nil
  result = ext

proc inlineFlatten(ext: ExtNimNode): ExtNimNode {.compileTime} = 
  let itIdent = ext.itNode()
  let itPrevIdent = ext.prevItNode()
  if not ext.isLastItem:
    let itIdent = ext.itNode()
    let idxIdent = newIdentNode(indexVariableName)
    ext.node = quote:
      var `idxIdent` = 0 
      for `itIdent` in `itPrevIdent`:
        `idxIdent` += 1
        nil
  else:
    let resultIdent = ext.res
    let emptyIdent = newIdentNode(emptyIdentifier)
    ext.node = quote:
      for `itIdent` in `itPrevIdent`:
        if `emptyIdent`:
          `emptyIdent` = false
          `resultIdent` = @[`itIdent`]
        else:
          `resultIdent`.add(`itIdent`)
  ext.nextIndexInc = true
  result = ext

proc inlineSub(ext: ExtNimNode, toSeq: bool = false): ExtNimNode {.compileTime.} =
  # sub is re-routed as filter implementation
  let index = newIdentNode(indexVariableName)
  let minIndex = ext.node[1]
  var newCheck: NimNode
  if ext.node.len == 2:
    newCheck = quote:
      `index` >= `minIndex`
  else:
    var endIndex = ext.node[2]
    if repr(endIndex)[0] == '^':
      let listRef = ext.listRef
      let endIndexAbs = endIndex[^1]
      endIndex = quote:
        len(`listRef`)-`endIndexAbs` # backwards index only works with collections that have a len
    newCheck = quote:
      `index` >= `minIndex` and `index` < `endIndex`
  ext.node = newCall("filter", newCheck)
  return ext.inlineFilter(toSeq)

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

proc inlineFind(ext: ExtNimNode): ExtNimNode {.compileTime.} = 
  let adaptedTest = ext.adapt()
  let resultIdent = ext.res
  let itIdent = ext.prevItNode()
  ext.node = quote:
    if `adaptedTest`:
      return some(`itIdent`)
    else:
      # this constant is unnecessarily written every loop - but should be optimized by the compiler in the end
      `resultIdent` = none(`itIdent`.type) 
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

proc findParentWithChildLabeled(node: NimNode, label: string): NimNode =
  if node.len > 0 and node[0].kind == nnkIdent and $node[0] == label:
    return node
  for child in node:
    let parent = child.findParentWithChildLabeled(label)
    if parent != nil:
      return parent
  return nil

proc inlineForeach(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  var adaptedExpression = ext.adapt()
  
  # special case: assignment to iterator -> try to assign to outer list (if possible)
  if adaptedExpression.kind == nnkExprEqExpr:
    var itNode = adaptedExpression.findParentWithChildLabeled($ext.prevItNode) 
    if itNode != nil:
      let listRef = ext.listRef
      let index = newIdentNode(indexVariableName)
      let rightSide = adaptedExpression[^1]
      # changing the iterator content will only work with indexable + variable containers
      if itNode == adaptedExpression:
        adaptedExpression = quote:
         `listRef`[`index`] = `rightSide`
      else:
        # when using a dot-expression the content is first saved to a temporary variable
        let tempVar = newIdentNode("__temp_var__")
        let leftSide = adaptedExpression[0]
        # use the tempVar instead of `it` -> replace `it.member = ...` with `tempVar.member = ...`
        itNode[0] = tempVar # changes adaptedExpression
        adaptedExpression = quote:
          var `tempVar` = `listRef`[`index`]
          `leftSide` = `rightSide`
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
  let foldOperation = ext.adapt(index=2, inFold=true)

  var i : NimNode 
  if useInternalAccu:
    let accuIdent = newIdentNode(internalAccuName) 
    i = quote:
      var `accuIdent` = `initialValue`
    ext.node = quote:
      `accuIdent` = `foldOperation`
    let f = quote:
      `resultIdent` = `accuIdent`
    ext.finals.add(f)
  else:
    i = quote:
      `resultIdent` = `initialValue`
    ext.node = quote:
      `resultIdent` = `foldOperation`
  
  ext.initials.add(i)
  result = ext

proc inlineSeq(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let node = ext.node
  ext.node = quote:
    for `itIdent` in `node`:
      nil
  ext.nextIndexInc = true
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
        
proc inlineElement(ext: ExtNimNode, forceSeq: bool): ExtNimNode {.compileTime.} =
  let label = if (ext.node.len > 0 and ext.node[0].kind == nnkIdent): $ext.node[0] else: ""
  if ext.node.kind == nnkCall and (ext.index > 0 or label in HANDLERS):
    if label != "flatten":    
      ext.ensureParameters(1)
    case label:
    of "zip":
        ext.ensureFirst()
        return ext.inlineZip()
    of "map":
      return ext.inlineMap(toSeq=forceSeq)
    of "filter":
      return ext.inlineFilter(toSeq=forceSeq)
    of "exists":
      ext.ensureLast()
      return ext.inlineExists()
    of "find":
      ext.ensureLast()
      return ext.inlineFind()
    of "all":
      ext.ensureLast()
      return ext.inlineAll()
    of "index":
      ext.ensureLast()
      return ext.inlineIndex()
    of "indexedMap":
      return ext.inlineMap(indexed=true, toSeq=forceSeq)
    of "fold":
      ext.ensureLast()
      ext.ensureParameters(2)
      return ext.inlineFold()
    of "foreach":
      return ext.inlineForeach()
    of "any":
      warning("any is deprecated - use exists instead")
      return ext.inlineExists()
    of "filterSeq":
      return ext.inlineFilter(toSeq=true)
    of "mapSeq":
      return ext.inlineMap(toSeq=true)
    of "indexedMapSeq":
      return ext.inlineMap(indexed=true, toSeq=true)
    of "sub":
      return ext.inlineSub(toSeq=forceSeq)
    of "subSeq":
      return ext.inlineSub(toSeq=true)
    of "flatten":
      return ext.inlineFlatten()
    else:
      error("$1 is unknown" % label, ext.node)    
  else:
    ext.ensureFirst()
    return ext.inlineSeq()


#template default[T](t: typedesc[T]): T =
#  var v: T
#  v

proc initInternal[U](a: U): U = 
  static:
    assert(false, ": Need an implementation for `proc initInternal(a:" & U.name & ")`") # name is in typetraits

proc initInternal*[T](a: seq[T]): seq[T] =
  @[]

proc initInternal*[A,T](a: array[A,T]): array[A,T] =
  discard

proc initInternal*[T](a: DoublyLinkedList[T]): DoublyLinkedList[T] =
  initDoublyLinkedList[T]()

proc findNode(node: NimNode, kind: NimNodeKind) : NimNode = 
  if node.kind == kind:
    return node
  for child in node:
    let res = child.findNode(kind)
    if res != nil:
      return res
  return nil


proc iterHandler(args: NimNode, debug=false): NimNode {.compileTime.} =
  let lastCall = $args[^1][0]
  let needsFunction = (lastCall != "foreach") and (lastCall != "del")
  let isSeq = lastCall in SEQUENCE_HANDLERS 
  var forceSeq = lastCall in FORCE_SEQ_HANDLERS
  var defineIdxVar = true

  for arg in args:
    if arg.kind == nnkCall:
      let label = $arg[0]
      if isSeq and not forceSeq:
        if label in FORCE_SEQ_HANDLERS:
          forceSeq = true
      if label == "zip" or label == "flatten":
        # zip and flatten both use the idx already - no need to define it (prevents "unused variable idx")
        defineIdxVar = false
  
  var code: NimNode
  if needsFunction:
    if isSeq and not forceSeq:
      result = iterFunction(args.compileTimeTypeInfer())
    else:
      result = iterFunction(newIdentNode("auto"))
    code = result[^1]
    result = nnkCall.newTree(result)
  else:
    # there is no extra function, but at least we have an own section here - preventing double definitions
    var q = quote:
      if true:
        nil
    code = q.getStmtList()
    result = q

  var init = code
  let initials = nnkStmtList.newTree()
  init.add(initials)

  if defineIdxVar:
    let idxIdent = newIdentNode(indexVariableName)
    let identDef = quote:
      var `idxIdent` = 0 
    init.add(identDef)

  if isSeq:
    let zero =
      if not forceSeq:
        let resultIdent = newIdentNode("result")
        let x = args[0]
        quote:
          # for user defined types this function has to be implemented
          `resultIdent` = initInternal(`x`)
      else:
        let emptyIdent = newIdentNode(emptyIdentifier)
        quote:
          var `emptyIdent` = true

    init.add(zero)

  var index = 0
  let listRef = args[0]
  let finals = nnkStmtList.newTree()
  
  for arg in args:
    let last = arg == args[^1]
    let ext = arg.newExtNode(index, last, initials, finals, listRef).inlineElement(forceSeq)
    let newCode = ext.node.getStmtList()
    code.add(ext.node)
    if newCode != nil:
      code = newCode
    if ext.nextIndexInc:
      index += 1
  if finals.len > 0:
    init.add(finals)

  if defineIdxVar:
    let forNode = result.findNode(nnkForStmt)
    if forNode != nil:
      # add index increment to end of the for loop
      let idxIdent = newIdentNode(indexVariableName)
      let incrIdx = quote:
        `idxIdent` += 1
      forNode[^1].add(incrIdx)

  if (debug):
    echo(repr(result))
    # for the whole tree do:
    # echo(treeRepr(result))
  

macro connect*(args: varargs[untyped]): untyped =
  result = iterHandler(args)

proc rearrangeDot(a: NimNode) : bool =
  result = false
  for i in 0..<a.len:
    if a[i].rearrangeDot():
      result = true
  if a.kind == nnkDotExpr and a.len > 1 and a[0].kind == nnkCall and a[^1].kind == nnkDotExpr and a[^1][0].kind == nnkCall:
    result = true
    let innerdot = a[^1]
    let innercall = innerdot[0]

    innerdot[0] = a[0]    
    a[0] = newCall(innerdot, innercall[^1])
    a[^1] = innerdot[^1]
    innerdot[^1] = innercall[0]


proc delegateMacro(a1: NimNode, b1:NimNode, debug=false): NimNode =
  expectKind(b1, nnkCall)
  var a = a1
  var b = b1

  while (a.kind == nnkInfix and a[0].kind == nnkIdent and $a[0]== "-->"):
    var new_b = nnkCall.newTree(nnkDotExpr.newTree(a[2], b[0]))
    for i in 1..<len(b):
      new_b.add(b[i])
    b = new_b
    a = a[1]

  while true:
    if not b.rearrangeDot():
      break

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
  result = iterHandler(mad, debug)

macro `-->`*(a: untyped, b: untyped): untyped =
  result = delegateMacro(a,b)

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  result = delegateMacro(a,b,true)
