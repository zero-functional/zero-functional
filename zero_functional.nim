import macros, options, sets, lists, typetraits, strutils, tables

const zfIteratorVariableName* = "it"
const zfAccuVariableName* = "a"
const zfCombinationsId* = "c"
const zfIndexVariableName* = "idx"
const zfListIteratorName* = "__itlist__"
const zfMinHighVariableName* = "__minHigh__"
const zfInternalIteratorName* = "__autoIter__"

const internalIteratorName = "__" & zfIteratorVariableName & "__"
const useInternalAccu = zfAccuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & zfAccuVariableName & "__" else: "result"
const implicitTypeSuffix = "?" # used when result type is automatically determined
const zfMaxTupleSize = 10

# if set to true: turns on prints code generated with zf (for macros -->, zfun and connect)
const debugAll = false

# See bug https://github.com/nim-lang/Nim/issues/7787
const hasIteratorBug = true
const createProc = hasIteratorBug

when defined(zf_iter):
  const defaultCollectionType = ""
  const defaultResultType = "iter"
else:
  when defined(zf_list):
    const defaultCollectionType = "DoublyLinkedList"
  else:
    const defaultCollectionType = "seq"
  const defaultResultType = defaultCollectionType & "[int]" & implicitTypeSuffix

type

  Command* {.pure.} = enum
    ## All available commands.
    ## 'to' - is a virtual command
    all, combinations, count, createIter, drop, dropWhile, exists, filter, find, flatten, fold, foreach,
    index, indexedFlatten, indexedMap, indexedReduce, map, reduce, sub, zip, take, takeWhile, to

  ReduceCommand {.pure.} = enum
    ## additional commands that operate as reduce command
    max, min, product, sum

  ExtNimNode* = ref object ## Store additional info the current NimNode used in the inline... functions
    node*: NimNode     ## the current working node / the current function
    nodeIndex: int     ## the position in args of the current working node
    prevItIndex*: int  ## index used for the previous iterator
    itIndex*: int      ## index used for the created iterator - 0 for the first. Will be incremented automatically.
    isLastItem: bool   ## true if the current item is the last item in the command chain
    initials*: NimNode ## code section before the first iterator where variables can be defined
    endLoop*: NimNode  ## code at the end of the for / while loop
    finals*: NimNode   ## code to set the final operations, e.g. the result
    listRef*:  NimNode ## reference to the list the iterator is working on
    args: NimNode      ## all arguments to the original macro
    typeDescription: string ## type description of the outer list type
    resultType: string ## result type when explicitly set
    needsIndex*: bool  ## true if the idx-variable is needed
    hasMinHigh: bool   ## true if the minHigh variable is defined and the loop should use indices rather than iterator
    isIter: bool       ## true if an iterator shall be created
    adapted: int       ## internally used to check that all iterators were created before the adapt call (otherwise adapt refers to an old iterator)
    elemAdded: bool    ## set when `addElem` has been called. Needed for generating collection output.
    forceIndexLoop: bool ## set when a loop with index rather than with iterator is needed (i.e. the loop changes the iterated collection)
    delegateUntil: int ## last argument index that is part of a "delegate" chain

  ## used for "combinations" command as output
  Combination*[A,T] = object
    it*: array[A,T]
    idx*: array[A,int]

type
  FiniteIndexable*[T] = concept a
    a.low() is int
    a.high() is int
    a[int]

  FiniteIndexableLen*[T] = concept a
    a.len() is int
    a[int]

  FiniteIndexableLenIter*[T] = concept a
    a.len() is int
    a[int] is T
    for it in a:
      type(it) is T

  Iterable*[T] = concept a
    for it in a:
      type(it) is T

  Appendable*[T] = concept a, var b
    for it in a:
      type(it) is T
    b.append(T)

  Addable*[T] = concept a, var b
    for it in a:
      type(it) is T
    b.add(T)


## Contains all functions that may result in a sequence result. Elements are added automatically to SEQUENCE_HANDLERS
var SEQUENCE_HANDLERS {.compileTime.} = [$Command.map, $Command.combinations, $Command.sub].toSet()

## Can be read in test implementation
var lastFailure {.compileTime.} : string = ""

proc zfGetLastFailure*() : string {.compileTime.} =
  result = lastFailure
  lastFailure = ""

## Called when some failure is detected during macro processing.
proc zfFail*(msg: string) {.compileTime.} =
  lastFailure = msg
  error(msg)

## This is the default extension complaining when a given function was not found.
proc extendDefault(ext: ExtNimNode) : ExtNimNode {.compileTime.} =
  zfFail("$1 is unknown, you could provide your own implementation! See `zfCreateExtension`!" % ext.node[0].repr)

## If you want to extend `zero_functional` assign this variable in your own code to a function with the given signature.
## The assignment has to be done during compile time - so for instance in a macro.
## See `test.nim` for an example of how to do that.
var zfExtension {.compileTime.} : proc(ext: ExtNimNode): ExtNimNode = extendDefault
var zfFunctionNames {.compileTime} : seq[string] = @[]

## Adds the given function name to the internal table of supported functions.
## This will be done automatically when using the macros `zf_inline` or `zf_inline_call`.
proc addFunction(functionName: string) {.compileTime.} =
  if functionName in zfFunctionNames:
    zfFail("Function $1 is already defined!" % (functionName))
  zfFunctionNames.add(functionName)

## Set the extension method.
## The extension method will be automatically created and registered using `zfRegister` or `zfRegisterExt`
proc zfSetExtension*(extension: proc(ext: ExtNimNode): ExtNimNode) {.compileTime.} =
  zfExtension = extension

## Register sequence handlers for an extension.
proc zfAddSequenceHandlers*(seqHandlers: seq[string]) {.compileTime.} =
  SEQUENCE_HANDLERS.incl(seqHandlers.toSet)

## same as zfAddSequenceHandlers(seq[string]) added for convenience
proc zfAddSequenceHandlers*(seqHandlers: varargs[string]) {.compileTime.} =
  SEQUENCE_HANDLERS.incl(seqHandlers.toSet)

## Find a node given its kind and - optionally - its content.
proc findNode*(node: NimNode, kind: NimNodeKind, content: string = "") : NimNode =
  if node.kind == kind and (content.len == 0 or content == $node):
    return node
  for child in node:
    let res = child.findNode(kind, content)
    if res != nil:
      return res
  return nil

macro idents(args: varargs[untyped]): untyped =
  ### shortcut implementation
  ### idents(ext) <=> let ext = newIdentNode("ext")
  ### idents(resultIdent("result")) <=> let resultIdent = newIdentNode("result")
  result = nnkStmtList.newTree()
  for a in args:
    var arg = a
    var s = repr(a)
    let idx = s.find("(")
    if idx != -1:
      arg = newIdentNode(s[0..idx-1])
      s = s[idx+1..s.len-2]
      if s[^1] != '"':
        # refer to the given variable name
        result.add(nnkLetSection.newTree(newIdentDefs(arg, newEmptyNode(), newCall("newIdentNode", newIdentNode(s)))))
        continue
      else:
        # remove the quotes
        s = s[1..s.len-2]
    result.add quote do:
      let `arg` = newIdentNode(`s`)

## Creates the extension function
proc createExtensionProc(name: string, cmdSeq: seq[string]): (NimNode,NimNode) =
  let procName = newIdentNode("zfExtend" & name)
  let cseq = if cmdSeq.len > 0: cmdSeq else: zfFunctionNames
  idents(ext, resultIdent("result"))
  let procDef = quote:
    proc `procName`(`ext`: ExtNimNode): ExtNimNode  {.compileTime.} =
      `resultIdent` = `ext`
      case `ext`.label
  let caseStmt = procDef.findNode(nnkCaseStmt)
  for cmd in cseq:
    let cmdName = newIdentNode("inline" & cmd.capitalizeAscii())
    let ofBranch = quote:
      case dummy
      of `cmd`:
        `ext`.`cmdName`()
    caseStmt.add(ofBranch.findNode(nnkOfBranch))
  let ofElse = quote:
    case dummy
    else:
      return nil
  caseStmt.add(ofElse.findNode(nnkElse))
  result = (procDef, procName)

## Create a delegate function for user-defined functions.
macro zfCreateExtension*(): untyped =
  let (funDef,funName) = createExtensionProc("UserExt", @[])
  result = quote:
    `funDef`
    zfSetExtension(`funName`)

## Create a delegate function for user-defined functions and register sequence handler functions.
macro zfCreateExtension*(additionalFunctions: static[seq[string]], seqHandlers: static[seq[string]]): untyped =
  for addFun in additionalFunctions:
    addFunction(addFun)
  zfAddSequenceHandlers(seqHandlers)
  result = quote:
    zfCreateExtension()

## Determines the closest possible type info of the input parameter to "-->".
## Sometimes the getType (node) works best, sometimes getTypeInst (nodeInst).
proc getTypeInfo(node: NimNode, nodeInst: NimNode): string =
  var typeinfo = node
  if typeinfo.len > 0:
    if node.kind == nnkEnumTy:
      result = "enum"
    elif ($typeinfo[0] == "ref"):
      result = $typeinfo[1]
      let idx = result.find(":")
      if idx != -1:
        result = result[0..idx-1]
    else:
      let res = repr(nodeInst)
      if res == "":
        result = repr(node)
      else:
        result = res
  else:
    let n1 = node.repr
    let n2 = nodeInst.repr
    if n2.len == 0 or n1.len > n2.len:
      result = n1
    else:
      result = n2

## Converts the id-string to the given enum type.
proc toEnum*[T:typedesc[enum]](key: string; t:T): auto =
  result = none(t)
  for it in t:
    if $it == key:
      result = some(it)
      break

## Converts the id-string to its ReduceCommand counterpart.
proc toReduceCommand(key: string): Option[ReduceCommand] =
  if key.startswith("indexed"):
    return key[7..key.len-1].toLowerAscii().toReduceCommand()
  result = key.toEnum(ReduceCommand)

{.push inline.}
## Special implementation to initialize array output.
proc zfInit*[A, T, U](s: array[A,T], handler: proc(it: T): U): array[A, U] =
  discard

## Special implementation to initialize DoublyLinkedList output.
proc zfInit*[T, U](a: DoublyLinkedList[T], handler: proc(it: T): U): DoublyLinkedList[U] =
  initDoublyLinkedList[U]()
## Special implementation to initialize SinglyLinkedList output.
proc zfInit*[T, U](a: SinglyLinkedList[T], handler: proc(it: T): U): SinglyLinkedList[U] =
  initSinglyLinkedList[U]()
## Special implementation to initialize HashSet output.
proc zfInit*[T, U](a: HashSet[T], handler: proc(it: T): U): HashSet[U] =
  initSet[U]()
proc zfInit*[T, U](a: seq[T], handler: proc(it: T): U): seq[U] =
  @[]

## Fallback implementation that converts each iterable type to the default seq type.
## Default is seq output type.
proc zfInitSeq*[T, U](a: Iterable[T], handler: proc(it: T): U): seq[U] =
  @[]

## General zfInit for iterable types.
## This should be overwritten for user defined types because otherwise the default = seq[T] on will be created.
proc zfInit*[T](a: Iterable[T]): Iterable[T] =
  proc zfIdent[T](it: T): T = it
  when compiles(zfInit(a, zfIdent)):
    zfInit(a, zfIdent)
  else:
    zfInitSeq(a, zfIdent)

proc createCombination*[A,T](it: array[A,T], idx: array[A,int]): Combination[A,T] =
  result = Combination[A,T](it: it, idx: idx)

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

## Add item to array
proc zfAddItem*[A,T](a: var array[A,T], idx: int, item: T) =
  a[idx] = item

## Add item to seq. Actually the below Addable could be used, but this does not always work out.
proc zfAddItem*[T](a: var seq[T], idx: int, item: T) =
  discard(idx)
  a.add(item)

## Special implementation for ``SinglyLinkedList`` which has only a ``preprend``
proc zfAddItem*[T](a: var SinglyLinkedList[T], idx: int, item: T) =
  discard(idx)
  a.prepend(item)

## Special implementation for ``DoublyLinkedList`` (needed for JS backend)
proc zfAddItem*[T](a: var DoublyLinkedList[T], idx: int, item: T) =
  discard(idx)
  a.append(item)

## Add item to type where an "add" proc is defined for
proc zfAddItem*[T](a: var Addable[T], idx: int, item: T) =
  discard(idx)
  a.add(item)

## Add item to type where an "append" proc is defined for (e.g. DoublyLinkedList)
proc zfAddItem*[T](a: var Appendable[T], idx: int, item: T) =
  discard(idx)
  a.append(item)

## Add item to HashSet
proc zfAddItem*[T](a: var HashSet[T], idx: int, item: T) =
  discard(idx)
  a.incl(item)

## Special implementations for tuple code (max `zfMaxTupleSize` items)
## The macro generates the zfAddItem[T] procs.
## For a tuple of size 2 that looks like:
#[
  proc zfAddItem[T](a: var (T,T), idx: int, item: T) =
    case idx:
    of 0: a[0] = item
    of 1: a[1] = item
    else: assert(false)
]#
macro genZfAddItemTuple(maxTupleSize: static[int]): untyped =
  result = nnkStmtList.newTree()
  idents(T,a,idx(zfIndexVariableName),item)
  let t = nnkPar.newTree(T)
  let cases = quote:
    case `idx`:
      of 0: `a`[0] = `item`
      else: assert(false)
  for l in 2..maxTupleSize:
    t.add(T)
    let l1 = l-1
    let c = quote:
      `a`[`l1`] = `item`
    cases.insert(l1,nnkOfBranch.newTree(newIntLitNode(l1), c))
    result.add quote do:
      proc zfAddItem[`T`](`a`: var `t`, `idx`: int, `item`: `T`) =
        `cases`
genZfAddItemTuple(zfMaxTupleSize)

{.pop.}

## Shortcut and safe way to get the ident label of a node
proc label(node: NimNode): string =
  if node.kind == nnkCall or node.kind == nnkOpenSymChoice or node.kind == nnkObjConstr:
    node[0].label
  elif node.kind == nnkIdent or node.kind == nnkSym:
    $node
  else:
    ""

## Get the label of the current command.
proc label*(ext: ExtNimNode): string =
  ext.node.label

## Replace the given identifier by the string expression
proc replace(node: NimNode, searchNode: NimNode, replNode: NimNode): NimNode =
  result = node
  if node.len > 0:
    for i in 0..<node.len:
      let child = node[i]
      if child.kind == searchNode.kind and child.label == searchNode.label:
        node[i] = replNode
      else:
        node[i] = child.replace(searchNode, replNode)
  elif node.kind == searchNode.kind and node.label == searchNode.label:
    result = replNode

## Searches for a given node type and returns the node and its path (indices) in the given root node.
## Search type is breadth first.
proc findNodePath(node: NimNode, kind: NimNodeKind, content: string = "") : (NimNode,seq[int]) =
  result = (nil, @[])
  for i,child in node:
    if child.kind == kind and (content.len == 0 or content == $node):
      return (child,@[i])
    let res = child.findNodePath(kind, content)
    if (res[0] != nil) and ((result[1].len == 0) or (result[1].len > res[1].len + 1)):
      result[0] = res[0] # the found node
      result[1] = @[i]   # index in current node
      result[1].add(res[1]) # add the children's indices at the end

## insert the new node with the given path
proc apply(node: NimNode, path: seq[int], newNode: NimNode): NimNode =
  var c = node
  for idx in 0..path.len-2:
    c = c[path[idx]]
  c[path[path.len-1]] = newNode
  result = node

## Helper that gets nnkStmtList and removes a 'nil' inside it - if present.
## The nil is used as placeholder for further added code.
proc getStmtList*(node: NimNode, removeNil = true): NimNode =
  var child = node
  while child.len > 0:
    child = child.last
    if child.kind == nnkStmtList:
      if removeNil and child.len > 0 and child.last.kind == nnkNilLit:
        child.del(child.len-1)
      else:
        let sub = child.getStmtList()
        if sub != nil:
          return sub
      return child
  return nil

proc mkItNode*(index: int) : NimNode {.compileTime.} =
  newIdentNode(internalIteratorName & ("$1" % $index))

proc nextItNode*(ext: ExtNimNode) : NimNode {.compileTime.} =
  if ext.adapted != -1 and ext.adapted != ext.prevItIndex:
    zfFail("ext has already been adapted! Call (all) nextItNode _before_ calling adapt!")
  result = mkItNode(ext.itIndex)
  ext.itIndex += 1

proc prevItNode*(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.prevItIndex)

proc res*(ext: ExtNimNode): NimNode {.compileTime.} =
  result = newIdentNode("result")

## Replace the variable name `it` with the continuos iterator variable name.
## The same goes for the accu `a`.
## Expressions on the left side of dot `.` are not replaced - because `it`could
## also be a member of a compund type - so `it.someMember` is replaced, `c.it` is not.
proc adapt(node: NimNode, iteratorIndex: int, inFold: bool=false): NimNode {.compileTime.} =
  case node.kind:
  of nnkIdent:
    if $node == zfIteratorVariableName:
      return mkItNode(iteratorIndex)
    elif inFold and useInternalAccu and $node == zfAccuVariableName:
      return newIdentNode(internalAccuName)
    else:
      return node
  of nnkFloatLit..nnkFloat128Lit, nnkCharLit..nnkUInt64Lit, nnkStrLit..nnkTripleStrLit, nnkSym:
    return node
  else:
    for z in 0..<node.len:
      node[z] = node[z].adapt(iteratorIndex, inFold)
      if node.kind == nnkDotExpr:
        break # change only left side of of dotExpr
    return node

## Shortcut for `node.adapt()` using the current iterator variable.
## Variable names like `it` or `idx` are replaced by their internal (unique) presentations.
proc adapt*(ext: ExtNimNode, index=1, inFold=false): NimNode {.compileTime.} =
  if (ext.adapted == -1) or (ext.adapted == ext.itIndex-1):
    ext.adapted = ext.itIndex-1
    result = ext.node[index].adapt(ext.itIndex-1, inFold)
  else:
    zfFail("ext has already been adapted with a different index!")
    result = ext.node[index]

proc isListType(td: string): bool =
  td.startswith("DoublyLinkedList") or td.startswith("SinglyLinkedList")

## Returns true if the input collection type is a `DoublyLinkedList` or a `SinglyLinkedList`.
proc isListType*(ext: ExtNimNode): bool =
  ext.typeDescription.isListType()

## Helper function that creates a list output if map, filter or flatten is the last command
## in the chain and a list is generated as output.
proc addElem*(ext: ExtNimNode, addItem: NimNode): NimNode {.compileTime.} =
  if ext.elemAdded:
    zfFail("addElem has already been called!")
  ext.elemAdded = true
  if ext.isLastItem:
    result = quote:
      yield `addItem`
  else:
    result = nil

macro zfAddItemChk*(resultIdent: untyped, idxIdent: untyped, addItem: untyped, typedescr: static[string], resultType: static[string]): untyped =
  result = quote:
    when compiles(zfAddItem(`resultIdent`, `idxIdent`, `addItem`)):
      zfAddItem(`resultIdent`, `idxIdent`, `addItem`)
    else:
      static:
        when (`resultType`.len == 0):
          zfFail("Need either 'add' or 'append' implemented in '" & `typedescr` & "' to add elements")
        else:
          zfFail("Result type '" & `resultType` & "' and added item of type '" & $`addItem`.type & "' do not match!")

proc addElemResult(ext: ExtNimNode, addItem: NimNode): NimNode {.compileTime.} =
  let resultIdent = ext.res
  # use -1 to force an error in case the index was actually needed instead of silently doing the wrong thing
  let idxIdent = if ext.needsIndex: newIdentNode(zfIndexVariableName) else: newIntLitNode(-1)
  let resultType = ext.resultType
  let typedescr = ext.typeDescription

  result = quote:
    zfAddItemChk(`resultIdent`, `idxIdent`, `addItem`, `typedescr`, `resultType`)

## Helper for Zero-DSL: quote all used variables that are defined somewhere in the created function.
proc addQuotes(a: NimNode, quotedVars: Table[string,string]) =
  if a.kind != nnkAccQuoted and (a.kind != nnkCall or a[0].label != "pre"):
    for i in 0..<a.len:
      let child = a[i]
      if child.kind == nnkIdent and child.label in quotedVars:
        a[i] = nnkAccQuoted.newTree(newIdentNode(quotedVars[child.label]))
      else:
        child.addQuotes(quotedVars)
      if a.kind == nnkDotExpr:
        break # only quote left side of dot expressions

## Helper for Zero-DSL: replace all 'it' nodes by the next iterator (in let expressions when defining a new iterator)
## or by previous iterator.
proc replaceIt(a: NimNode, repl: NimNode, left: bool) : bool =
  result = false
  for i in 0..<a.len:
    let child = a[i]
    if left and (child.kind == nnkExprEqExpr or child.kind == nnkIdentDefs):
      if child[0].kind == nnkIdent and child[0].label == zfIteratorVariableName:
        child[0] = repl
        return true

    elif not left:
      if child.kind == nnkIdent and child.label == zfIteratorVariableName:
        a[i] = repl
        result = true

    if child.replaceIt(repl, left):
      if left:
        return true # only once per left node
      result = true

    if a.kind == nnkDotExpr:
      break # only replace left side of dot

## Replace variable definitions with quoted variables, return let section for ident definitions
proc replaceVarDefs(a: NimNode, quotedVars: var Table[string,string], preSection=false): NimNode =
  result = nnkStmtList.newTree()
  let isPre = preSection or (a.kind == nnkCall and a[0].label == "pre")
  for b in a:
    if (a.kind == nnkVarSection or a.kind == nnkLetSection) and b.kind == nnkIdentDefs:
      let label = b[0].label
      if label != "" and label != "idx" and label != "it":
        if not isPre and not (label in quotedVars):
          let labelNode = newIdentNode(label)
          if (b[1].kind == nnkIdent): # symbol name was explicitly given
            let s = b[1]
            result.add quote do:
              let `labelNode`= `s`
            b[1] = newEmptyNode()
          else:
            # generate symbol
            let nsk = if a.kind == nnkVarSection: nskVar else: nskLet
            result.add quote do:
              let `labelNode` = genSym(NimSymKind(`nsk`), `label`)
        quotedVars[label] = label
    else:
      let c = b.replaceVarDefs(quotedVars, isPre)
      if c.len > 0:
        result.add(c)

## Called when delegate section is used in Zero-DSL
proc zfDelegate*(ext: ExtNimNode, caller: NimNode, delegateIndex: int) =
  let newNode = ext.node.copyNimTree()
  if newNode.len == 2 and newNode[1].label == "_":
    # copy default arguments from original function call when (_) is used as call
    newNode.del(1)
    for i in 1..<caller.len:
      newNode.add(caller[i])
  ext.delegateUntil = ext.nodeIndex + delegateIndex + 1
  ext.args.insert(ext.delegateUntil, newNode)
  ext.node = nnkStmtList.newTree()

macro zfParamChk(funNameExport: untyped, sym: untyped, symName: untyped, paramType: typedesc) : untyped =
# parameter type was explicitly given: check that the parameter is of the given type
# check is done in compile-time macro code only -
# but has to be done in the location where the content of the parameter is actually known: inside the loop!
  result = quote:
    static:
      when not ((`sym`) is `paramType`):
        zfFail("Function '$1': param '$2', expected type '$3'!" % [`funNameExport`, `symName`, $`paramType`])

## Parse the given Zero-DSL and create an inlineXyz function.
## E.g. `zf_inline index(): ...` will create `proc inlineIndex(ext: ExtNimNode)`.
## The DSL parses the content for the following sections:
## - pre: prepare used variables - also manipulated ext, when default behaviour is not sufficient.
##        as zero DSL has limitations when creating code the pre-section can be used to
##        circumvent these deficiencies
## - init: initialize variables used in the loop
## - loop: the main part that is running within the loop
## - delegate: delegates this function call to another function (created with zero-DSL) - possibly with changed parameters
## - endLoop: section added at the end of the loop
## - final: section added at the end of the function
##
## So in general it looks like this - in the overall generated code
## <pre-section> with definitions - referenced in the other sections
##
## # init section
## var myVar = 0
## # the actual loop
## for it in a:
##   # added loop section somewhere here
##   ...
##   # end loop around here
##   ...
## # final section
proc zeroParse(header: NimNode, body: NimNode): NimNode =
  if (header.kind == nnkCall or header.kind == nnkObjConstr) and body.kind == nnkStmtList:
    var funDef = header
    var hasDelegate = false
    var hasLoop = false
    let funName = funDef.label
    var funNameExport = funName
    # when this function has been called with zf_inline_call the "__call__" has to be stripped for the actual zero function name
    if funName.endswith("__call__"):
      funNameExport = funName[0..^9]
      if not (funNameExport in zfFunctionNames):
        addFunction(funNameExport) # only add it once
    else:
      addFunction(funNameExport)

    let procName = newIdentNode("inline" & funName.capitalizeAscii())
    # parameters given to the zero function
    var paramSection = nnkStmtList.newTree()
    # referenced variables are added here
    # this contains common definitions for all sections
    let letSection = nnkStmtList.newTree()
    # reference to the 'ext' parameter of the created proc
    idents(ext)
    var quotedVars = initTable[string,string]()
    letSection.add(body.replaceVarDefs(quotedVars))
    let numArgs = funDef.len-1
    let hasPre = body[0].label == "pre"
    let firstSym = if funDef.len > 1: funDef[1].label else: ""
    let chk =
      if firstSym == "_":
        nnkStmtList.newTree()
      else:
        quote:
          if `numArgs` < `ext`.node.len-1:
            zfFail("too many arguments in '$1', got $2 but expected only $3" % [`ext`.node.repr, $(`ext`.node.len-1), $`numArgs`])
    if not hasPre:
      paramSection.add(chk)

    # save type of parameters when explicitly given
    var paramTypes = initTable[string,NimNode]()

    for i in 1..numArgs:
      var defaultVal: NimNode = nil
      var funSym = funDef[i]
      var paramType: NimNode = nil
      if funSym.kind == nnkExprColonExpr:
        funSym = funDef[i][0]
        paramType = funDef[i][1]
      var symName = funSym.label
      let hasDefault = funSym.kind == nnkExprEqExpr
      if hasDefault:
        defaultVal = funSym[1]
        symName = funSym[0].label
        paramType = quote:
          type(`defaultVal`)
      let sym = newIdentNode(symName)
      if symName != "_":
        quotedVars[symName] = symName
        if paramType != nil:
          paramTypes[symName] = paramType
      paramSection.add quote do:
        let `sym` =
          if `i` < `ext`.node.len:
            adapt(`ext`, `i`)
          else:
            when bool(`hasDefault`):
              quote:
                `defaultVal`
            else:
              when `symName` != "":
                when `symName` == "_":
                  zfFail("'$1' needs at least 1 parameter!" % [`funNameExport`])
                else:
                  zfFail("missing argument '$1' for '$2'" % [`symName`, `funNameExport`])
              newIntLitNode(0)

    let hasResult = body.findNode(nnkIdent, "result") != nil
    if hasResult:
      idents(res("resultIdent"))
      letSection.add quote do:
        let `res` = newIdentNode("result")
      quotedVars["result"] = "resultIdent"
    if hasResult or body.findNode(nnkReturnStmt) != nil:
      letSection.add quote do:
        if not `ext`.isLastItem:
          zfFail("'$1' has a result and must be last item in chain!" % `funNameExport`)
    else:
      # register iterator as sequence handler
      zfAddSequenceHandlers(funName)

    if body.findNode(nnkIdent, zfIndexVariableName) != nil:
      idents(idxIdent)
      letSection.add quote do:
        let `idxIdent` = newIdentNode(`zfIndexVariableName`)
        discard(`idxIdent`)
        `ext`.needsIndex = true
      quotedVars[zfIndexVariableName] = "idxIdent"

    if (not hasPre and body[0].label != "delegate") or body.len != 2 or body[1].label != "delegate":
      # replace it in 'it = ...' with `nextIt` and create the next iterator
      if body.replaceIt(nnkAccQuoted.newTree(newIdentNode("nextIdent")), true):
        idents(nextIt("nextIdent"))
        letSection.add quote do:
          let `nextIt` = `ext`.nextItNode()
      # access the previous iterator replacing 'it'
      if body.replaceIt(nnkAccQuoted.newTree(newIdentNode("prevIdent")), false):
        idents(prev("prevIdent"))
        letSection.add quote do:
          let `prev` = `ext`.prevItNode()

    # create the proc
    let q = quote:
      proc `procName`( `ext`: ExtNimnode) {.compileTime.} =
        `paramSection`
        `letSection`
        nil

    let code = q.getStmtList()

    # save old state for later checks
    idents(delegateUntil)
    if paramTypes.len > 0:
      code.add quote do:
        let `delegateUntil` = `ext`.delegateUntil

    # generate the code sections
    for i in 0..<body.len:
      let tpe = body[i].label
      let cmd = body[i][1]
      if (i == 0 and not hasPre) or (i == 1 and hasPre):
        # only do this check after "pre" = prepare section
        if hasPre:
          code.add(chk)
        # all variables defined in the body have to be uniquely referenced
        # - independent from the code section they are located in
        body.addQuotes(quotedVars)

      case tpe:
      of "pre":
        code.add quote do:
          `cmd`
      of "init":
        doAssert(not hasDelegate, "delegate cannot be used together with init or loop")
        code.add quote do:
          let i = quote:
            `cmd`
          `ext`.initials.add(i)
      of "loop":
        hasLoop = true
        doAssert(not hasDelegate, "delegate cannot be used together with init or loop")
        code.add quote do:
          `ext`.node = quote:
            `cmd`
      of "delegate":
        hasDelegate = true
        doAssert(not hasLoop, "delegate cannot be used together with loop")
        idents(oldParams, isLast, isLastCmd)
        code.add quote do:
          let `isLast` = `ext`.isLastItem
          var `isLastCmd` = false
          let `oldParams` = `ext`.node
        for cmdIdx in 0..<cmd.len:
          let label = cmd[cmdIdx][0].label
          let p = newCall(label)
          for i in 1..<cmd[cmdIdx].len:
            p.add(cmd[cmdIdx][i])
          if cmdIdx == cmd.len-1:
            code.add quote do:
              `isLastCmd` = true
          code.add quote do:
            `ext`.node = quote:
              `p`
            `ext`.isLastItem = `isLast` and `isLastCmd`
            zfDelegate(`ext`, `oldParams`, `cmdIdx`)
        code.add quote do:
          discard(`oldParams`)
      of "endLoop":
        code.add quote do:
          let e = quote:
            `cmd`
          `ext`.endLoop.add(e)
      of "final":
        code.add quote do:
          let f = quote:
            `cmd`
          `ext`.finals.add(f)
      else:
        doAssert(false, "unsupported keyword: " & tpe)

    if paramTypes.len > 0:
      # parameter types where explicitly given: add check to code
      var nodeIdx = 0
      for it in paramTypes.pairs():
        let (symName,paramType) = it
        let sym = nnkAccQuoted.newTree(newIdentNode(symName))
        code.add quote do:
          if `nodeIdx` == 0: # first check: wrap the original loop code and add the below check statements
            `ext`.node = nnkStmtList.newTree(`ext`.node)
          if `ext`.nodeIndex > `delegateUntil`:
            let paramCheck = quote:
              zfParamChk(`funNameExport`, `sym`, `symName`, `paramType`)
            `ext`.node.insert(`nodeIdx`, paramCheck)
        nodeIdx += 1
    result = q
  else:
    if header.kind != nnkCall:
      zfFail("did not expect " & $header.kind & " in " & header.repr)
    if body.kind != nnkStmtList:
      zfFail("did not expect " & $body.kind & " in body of " & header.repr)

# alternative syntax still possible
macro zero*(a: untyped): untyped =
  let body = nnkStmtList.newTree()
  for i in 1..<a.len:
    body.add(a[i])
  result = zeroParse(a[0], body)

## Initate the Zero-DSL definition of an inline function.
## The macro expects the function name, its parameters (in brackets) and the body to implement in different sections.
## See zeroParse.
macro zf_inline*(header: untyped, body: untyped): untyped =
  result = zeroParse(header, body)

## Helper that prints the created inline function.
## Useful when Zero-DSL cannot be used for the whole implementation of an inline function.
macro zf_inline_dbg*(header: untyped, body:untyped): untyped =
  result = zeroParse(header, body)
  echo(result.repr)

## calls zf_inline registering the function call and calls the actual function.
## This can be used to add own implementations of inline-functions with parts in Zero-DSL.
macro zf_inline_call*(header: untyped, body: untyped): untyped =
  doAssert(header.kind == nnkCall or header.kind == nnkObjConstr)
  header[0] = newIdentNode(header.label & "__call__")
  let fun = newIdentNode("inline" & header.label.capitalizeAscii())
  idents(ext)
  let q = zeroParse(header, body)
  result = quote:
    `q`
    `fun`(`ext`)


## Implementation of the 'map' command.
## Each element of the input is mapped to a given function.
## It is also possible to add own definitions - either as normal value or as a tuple.
proc inlineMap*(ext: ExtNimNode) {.compileTime.} =
  let kind = ext.node[1].kind
  if kind == nnkExprEqExpr or kind == nnkAsgn:
    let label = ext.node[1][1].label
    let isInternal = label == internalIteratorName or label == zfIndexVariableName
    let v = ext.adapt()
    ext.node = nnkLetSection.newTree()
    if v[0].kind == nnkPar:
      # allow tuple unpacking: ((a,b) = c)
      let vt = nnkVarTuple.newTree()
      for n in v[0]:
        vt.add(n) # add all items to the tuple on the left
      vt.add(newEmptyNode())
      vt.add(v[1]) # add the assigned item on the right
      ext.node.add(vt)
    else:
      # just the "normal" definition (a = b)
      ext.node.add(newIdentDefs(v[0], newEmptyNode(), v[1]))
    if not isInternal: # leave out definitions that access it or idx directly
      # set next iterator
      let nextIt = ext.nextItNode()
      let f = v[0] # set 'it' to the previously assigned value / 'it' might also be consequently used
      ext.node = nnkStmtList.newTree(ext.node).add quote do:
        let `nextIt` = `f`
        discard(`nextIt`) # iterator might not be used
  else:
    zf_inline_call map(f):
      loop:
        let it = f

zf_inline indexedMap(f):
  loop:
    let it = (idx, f)

## Implementation of the 'filter' command.
## The trailing commands execution depend on the filter condition to be true.
zf_inline filter(cond: bool):
  loop:
    if cond:
      nil

## Implementation of the 'flatten' command.
## E.g. @[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6]
zf_inline flatten():
  init:
    var idxFlatten = -1
  loop:
    for flattened in it:
      let it = flattened
      idxFlatten += 1
      let idx = idxFlatten
      discard(idx)

zf_inline indexedFlatten():
  init:
    var idxFlatten = -1
  loop:
    var idxInner = -1
    for flattened in it:
      idxInner += 1
      idxFlatten += 1
      let it = (idxInner, flattened)
      let idx = idxFlatten
      discard(idx)

## Implementation of the `takeWhile` command.
## `takeWhile(cond)` : Take all elements as long as the given condition is true.
zf_inline takeWhile(cond: bool):
  loop:
    if not cond:
      break
    else:
      nil

## Implementation of the `take` command.
## `take(count)` : Take `count` elements.
zf_inline take(count: int):
  init:
    var idxTake = -1
  delegate:
    takeWhile:
      idxTake += 1
      idxTake < count

## Implementation of the `dropWhile` command.
## `dropWhile(cond)` : drop elements as long the given condition is true.
## Once the condition gets false, all following elements are used.
zf_inline dropWhile(cond: bool):
  init:
    var gate = false
  loop:
    if gate or not cond:
      gate = true
      nil

## Implementation of the `drop` command.
## `drop(count)` : drop (or discard) the next `count` elements.
zf_inline drop(count: int):
  init:
    var idxDrop = -1
  delegate:
    dropWhile:
      idxDrop += 1
      idxDrop < count

## Implementation of the 'sub' command.
## Creates a list from minIndex til maxIndex (inclusive) -
## e.g. `a --> sub(0,1)` would contain the first 2 elements of a.
## In sub also Backward indices (e.g. ^1) can be used.
proc inlineSub(ext: ExtNimNode) {.compileTime.} =
  let minIndex = ext.node[1]
  if ext.node.len == 2:
    # only one parameter: same as drop
    ext.node = newCall($Command.drop, minIndex)
    ext.inlineDrop()
  else:
    var endIndex = ext.node[2]
    if repr(endIndex)[0] == '^':
      let listRef = ext.listRef
      let endIndexAbs = endIndex.last
      ext.node[2] = quote:
        len(`listRef`)-`endIndexAbs` # backwards index only works with collections that have a len

    zf_inline_call sub(minIndex: int, endIndex: int):
      init:
        var idxSub = -1
      loop:
        idxSub += 1
        if idxSub >= minIndex:
          if idxSub > endIndex:
            break
          else:
            nil

## Implementation of the 'exists' command.
## Searches the input for a given expression. If one is found "true" is returned, else "false".
zf_inline exists(search: bool):
  init:
    result = false
  loop:
    if search:
      return true

## Implementation of the 'find' command.
## Searches the input for a given expression. Returns an option value.
zf_inline find(cond: bool):
  init:
    var i = 0
  loop:
    if cond:
      return some(it)
    else:
      # this constant is unnecessarily written every loop - but should be optimized by the compiler in the end
      result = none(it.type)

## Implementation of the 'all' command.
## Returns true of the given condition is true for all elements of the input, else false.
zf_inline all(test: bool):
  init:
    result = true
  loop:
    if not test:
      return false

proc findParentWithChildLabeled(node: NimNode, label: string): NimNode =
  if node.len > 0 and node[0].label == label:
    return node
  for child in node:
    let parent = child.findParentWithChildLabeled(label)
    if parent != nil:
      return parent
  return nil

## Implementation of the 'foreach' command.
## A command may be called on each element of the input list.
## Changing the list in-place is also supported.
proc inlineForeach*(ext: ExtNimNode) {.compileTime.} =
  let isEq = ext.node[1].kind == nnkExprEqExpr
  let hasIterator = isEq and (ext.node[1][0].findNode(nnkIdent, "it") != nil)
  var adaptedExpression = ext.adapt()

  # special case: assignment to iterator -> try to assign to outer list (if possible)
  if hasIterator:
    if ext.itIndex > 1:
      zfFail("Adapted list cannot be changed in-place!")
    # this only works if the current list has not (yet) been manipulated
    var itNode = adaptedExpression.findParentWithChildLabeled(ext.prevItNode.label)
    if itNode != nil:
      let listRef = ext.listRef
      idents(index(zfIndexVariableName))
      let rightSide = adaptedExpression.last
      # changing the iterator content will only work with indexable + variable containers
      if ext.isListType():
        idents(itlist(zfListIteratorName))
        adaptedExpression = quote:
          `itlist`.value = `rightSide`
      elif itNode == adaptedExpression:
        ext.needsIndex = true
        adaptedExpression = quote:
          `listRef`[`index`] = `rightSide`
      else:
        ext.needsIndex = true
        # when using a dot-expression the content is first saved to a temporary variable
        let tempVar = newIdentNode("__temp_var__")
        let leftSide = adaptedExpression[0]
        # use the tempVar instead of `it` -> replace `it.member = ...` with `tempVar.member = ...`
        itNode[0] = tempVar # changes adaptedExpression
        adaptedExpression = quote:
          var `tempVar` = `listRef`[`index`]
          `leftSide` = `rightSide`
  elif isEq:
    adaptedExpression = nnkAsgn.newTree(adaptedExpression[0], adaptedExpression[1])
  ext.node = nnkStmtList.newTree().add quote do:
    `adaptedExpression`

## Implementation of the 'index' command.
## Returns the index of the element in the input list when the given expression was found or -1 if not found.
zf_inline index(cond: bool):
  init:
    result = -1 # index not found
  loop:
    if cond:
      return idx

## Implementation of the 'fold' command.
## Initially the result is set to initial value given by the user, then each element is added
## to the result by subsequent calls.
when useInternalAccu:
  zf_inline fold(initialValue, _):
    pre:
      let foldOperation = ext.adapt(2, inFold=true) # special adapt for fold
      ext.node.del(2) # prevent error message: too many parameters
      let accuIdent = newIdentNode(internalAccuName)
    init:
      var accuIdent = initialValue # implicitly uses internalAccuName
    loop:
      accuIdent = foldOperation
    final:
      result = accuIdent
else:
  zf_inline fold(initialValue, foldOperation):
    init:
      result = initialValue
    loop:
      result = foldOperation

## Implementation of the 'reduce' command.
## Initially the result is set to the first element of the list, then each element is added
## to the result by subsequent calls.
proc inlineReduce(ext: ExtNimNode) {.compileTime.} =
  let reduceCmd = ext.label.toReduceCommand()
  if reduceCmd != none(ReduceCommand):
    # e.g. sum <=> reduce(sum(it))
    let operation =
      case reduceCmd.get():
      of ReduceCommand.max:
        quote:
          if it[0] > it[1]: it[0] else: it[1]
      of ReduceCommand.min:
        quote:
          if it[0] < it[1]: it[0] else: it[1]
      of ReduceCommand.product:
        quote:
          it[0] * it[1]
      of ReduceCommand.sum:
        quote:
          it[0] + it[1]
    ext.node.add(operation)

  # in reduce the operator has to use the newest iterator
  let nextIt = ext.nextItNode()
  if ext.label.startswith("indexed"):

    zf_inline_call reduce(op):
      init:
        var initAccu = true
      loop:
        if initAccu:
          result = (idx, it)
          initAccu = false
        else:
          let oldValue = result[1]
          let `nextIt` = (oldValue, it) # reduce
          let newValue = op
          if not (oldValue == newValue):
            result = (idx, newValue) # propagate new value with idx
      endLoop:
        if initAccu:
          result[0] = -1 # we actually do not have a result: set index to -1

  else:
    zf_inline_call reduce(op):
      init:
        var initAccu = true
      loop:
        if initAccu:
          result = it
          initAccu = false
        else:
          let prevIt = it
          let `nextIt` = (result, prevIt)
          result = op

## Implementation of the 'combinations' command.
## Each two distinct elements of the input list are combined to one element.
proc inlineCombinations(ext: ExtNimNode) {.compileTime.} =
  ext.needsIndex = true
  idents(idxIdent(zfIndexVariableName), itCombo(zfCombinationsId))
  if ext.node.len == 1:
    if ext.isListType():
      zf_inline_call combinations():
        pre:
          let itList = newIdentNode(zfListIteratorName)
        loop:
          var itListInner = itList.next
          var idxInner = idx
          while itListInner != nil:
            let `itCombo` = createCombination([it, itListInner.value], [idx, idxInner])
            idxInner += 1
            itListInner = itListInner.next
            nil
    else:
      zf_inline_call combinations():
        pre:
          let listRef = ext.listRef
        loop:
          when not (listRef is FiniteIndexableLenIter):
            static:
              zfFail("Only index with len types supported for combinations")
          for idxInner in idx+1..<listRef.len():
            let `itCombo` = createCombination([listRef[idx], listRef[idxInner]], [idx, idxInner])
            nil
  else: # combine with other collections
    var code = nnkStmtList.newTree()
    var root = code
    var itIdent = ext.prevItNode()
    let iterators = nnkBracket.newTree().add(itIdent)
    let indices = nnkBracket.newTree().add(idxIdent)
    var idx = 1
    while idx < ext.node.len:
      itIdent = ext.nextItNode()
      var idxInner = genSym(nskVar, "__idxInner__")
      iterators.add(itIdent)
      indices.add(idxInner)
      let listRef = ext.node[idx]
      idx += 1
      code.add quote do:
        var `idxInner` = -1
        for `itIdent` in `listRef`:
          `idxInner` += 1
          nil
      code = code.getStmtList()
    code.add quote do:
      let `itCombo` = createCombination(`iterators`, `indices`)
      nil
    ext.node = root

macro genTupleSeqCalls(maxTupleSize: static[int]): untyped =
  ## generates the procs initTupleSeq and addToTupleSeq needed for the split command
  #[
  proc initTupleSeq[T1,T2](t: (T1,T2)): (seq[T1],seq[T2]) =
    result = (newSeq[T1](), newSeq[T2]())

  proc addToTupleSeq[T1,T2](ts: var (seq[T1],seq[T2]), t: (T1,T2)) =
    ts[0].add(t[0])
    ts[1].add(t[1])
  ]#
  var Ts : seq[NimNode] = @[]
  for l in 1..maxTupleSize:
    Ts.add(newIdentNode("T" & $l))

  result = nnkStmtList.newTree()
  for tupleNum in 2..maxTupleSize:
    let genIdents = nnkIdentDefs.newTree()
    let paramIdents = nnkPar.newTree()
    let params = nnkFormalParams.newTree()
    let params2 = nnkFormalParams.newTree()
    let retVal = nnkPar.newTree()
    let calls = nnkPar.newTree()
    for i in 0..tupleNum-1:
      # Generic param is [T1, T2, ...]
      genIdents.add(Ts[i])
      # parameter is (T1, T2, ...)
      paramIdents.add(Ts[i])
      # return value is (seq[T1], seq[T2], ...)
      retVal.add(nnkBracketExpr.newTree(newIdentNode("seq"), Ts[i]))
      # result = (newSeq[T1](), newSeq[T2](), ...)
      calls.add(newCall(nnkBracketExpr.newTree(newIdentNode("newSeq"), Ts[i])))
    let tParam = newIdentDefs(newIdentNode("t"), paramIdents, newEmptyNode())
    let tsParam = newIdentDefs(newIdentNode("ts"), nnkVarTy.newTree(retVal), newEmptyNode())
    params.add(retVal).add(tParam)
    params2.add(newEmptyNode()).add(tsParam).add(tParam)
    genIdents.add(newEmptyNode()).add(newEmptyNode())

    # generate initTupleSeq
    let body = nnkStmtList.newTree(nnkAsgn.newTree(newIdentNode("result"), calls))
    result.add(nnkProcDef.newTree(newIdentNode("initTupleSeq"), newEmptyNode(),
      nnkGenericParams.newTree(genIdents), params, newEmptyNode(), newEmptyNode(), body))

    # generate addToTupleSeq
    let body2 = nnkStmtList.newTree()
    idents(ts,t)
    for i in 0..tupleNum-1:
      body2.add quote do:
        `ts`[`i`].add(`t`[`i`])
    result.add(nnkProcDef.newTree(newIdentNode("addToTupleSeq"), newEmptyNode(),
      nnkGenericParams.newTree(genIdents), params2, newEmptyNode(), newEmptyNode(), body2))
genTupleSeqCalls(zfMaxTupleSize)

## implementation of the `split` command. Splits a sequence of tuples to a tuple of sequences.
zf_inline split():
  init:
    var first = true
  loop:
    if first:
      first = false
      # if this fails to compile: increase zfMaxTupleSize!
      result = initTupleSeq(it)
    result.addToTupleSeq(it)

## Implementation of the `count` command. Counts all (filtered) items.
zf_inline count():
  init:
    result = 0
  loop:
    result += 1

## Initial creation of the outer iterator.
proc inlineSeq(ext: ExtNimNode) {.compileTime.} =
  let itIdent = ext.nextItNode()
  let listRef = ext.listRef
  let idxIdent = newIdentNode(zfIndexVariableName)

  if ext.hasMinHigh:
    let minHigh = newIdentNode(zfMinHighVariableName)
    var itDef = nnkStmtList.newTree()
    if ext.node.kind == nnkCall:
      let zipArgs = ext.adapt()
      itDef = quote:
        let `itIdent` = `zipArgs`
    else:
      itDef = quote:
        let `itIdent` = `listRef`[`idxIdent`]
      let e = quote:
        discard `itIdent`
      ext.endLoop.add(e)
    ext.node = quote:
      for `idxIdent` in 0..`minHigh`:
        `itDef`
        nil

  elif ext.isListType():
    # list iterator implemnentation
    let listRef = ext.listRef
    idents(itlist(zfListIteratorName), itNext("__itListNext__"))
    ext.node = quote:
      var `itlist` = `listRef`.head
      while `itlist` != nil:
        let `itIdent` = `itlist`.value
        let `itNext` = `itList`.next
        nil
    ext.endLoop.add quote do:
      `itlist` = `itNext`

  elif ext.forceIndexLoop:
    # iterate over index
    ext.needsIndex = true
    ext.initials.add quote do:
      `idxIdent` = low(`listRef`)
    ext.node = quote:
      while (`idxIdent` <= high(`listRef`)):
        let `itIdent` = `listRef`[`idxIdent`]
        nil
    # idx += 1 already done in iterHandler

  elif ext.typeDescription.startswith("Option["):
    let tpe = newIdentNode(ext.typeDescription[7..ext.typeDescription.len()-2])
    ext.node = quote:
      if `listRef` != none(`tpe`):
        let `itIdent` = `listRef`.get()
        nil

  else:
    # usual iterator implementation
    ext.node = quote:
      for `itIdent` in `listRef`:
        nil

proc ensureFirst(ext: ExtNimNode) {.compileTime.} =
  if ext.itIndex > 0:
    error("$1 supposed to be first - index is $2" % [ext.label, $ext.itIndex], ext.node)

macro createExtendDefaults(): untyped =
  # creates proc zfExtendDefaults
  # add inlineForeach + inlineDef manually
  addFunction($Command.foreach)
  let (funDef,_) = createExtensionProc("Defaults", @[])
  zfFunctionNames = @[]
  result = quote:
    `funDef`
createExtendDefaults()

## Delegates each function argument to the inline implementations of each command.
proc inlineElement(ext: ExtNimNode) {.compileTime.} =
  let label = ext.label
  if label != "" and ext.zfExtendDefaults() != nil:
    return # command has been handled
  if ext.node.kind == nnkCall and (ext.itIndex > 0):
    case label:
    of "any":
      warning("any is deprecated - use exists instead")
      ext.inlineExists()
    else:
      if (label.toReduceCommand() != none(ReduceCommand)):
        ext.inlineReduce()
      elif label != "":
        let res = zfExtension(ext)
        if res == nil:
          zfFail("add implementation for command '$1'" % label)
        elif res.node.kind == nnkCall:
          # call should have been replaced internally
          if res.label == label:
            zfFail("implementation for command '$1' was not replaced by the extension!" % label)
          elif res.label == "":
            # call was replaced by additional calls
            ext.node = nnkStmtList.newTree()
          else:
            # call was internally replaced by another call: repeat inlining
            ext.inlineElement()
  else:
    ext.ensureFirst()
    ext.inlineSeq()

type
  ## Helper type to allow `[]` access for SomeLinkedList types
  MkListIndexable[T,U,V] = ref object
    listRef: U
    currentIt: V
    currentIdx: Natural
    length: int

  ## Helper to allow `[]` access for slices
  MkSliceIndexable[T] = ref object
    slice: HSlice[T,T]

proc mkIndexable*[T](items: DoublyLinkedList[T]): MkListIndexable[T, DoublyLinkedList[T], DoublyLinkedNode[T]] =
  MkListIndexable[T, DoublyLinkedList[T], DoublyLinkedNode[T]](listRef: items, currentIt: items.head, currentIdx: 0, length: -1)
proc mkIndexable*[T](items: SinglyLinkedList[T]): MkListIndexable[T, SinglyLinkedList[T], SinglyLinkedNode[T]] =
  MkListIndexable[T, SinglyLinkedList[T], SinglyLinkedNode[T]](listRef: items, currentIt: items.head, currentIdx: 0, length: -1)
proc mkIndexable*[T](items: HSlice[T,T]): MkSliceIndexable[T] =
  MkSliceIndexable[T](slice: items)

proc `[]`*[T,U,V] (items: var MkListIndexable[T,U,V], idx: Natural): T =
  while idx != items.currentIdx and items.currentIt != nil:
    items.currentIt = items.currentIt.next
    items.currentIdx += 1
  if items.currentIt == nil:
    return nil
  items.currentIdx += 1
  let value = items.currentIt.value
  items.currentIt = items.currentIt.next
  result = value

proc high*(items: MkListIndexable): int =
  # high = is broken down to number of items - 1
  # so that low would be mapped to 0
  if items.length == -1:
    for it in items.listRef:
      items.length += 1
  return items.length

proc `[]`*[T] (items: MkSliceIndexable[T], idx: Natural): T =
  items.slice.a + T(idx)

proc high*(items: MkSliceIndexable): int =
  int(items.slice.b - items.slice.a)

## Helper macro to wrap a not indexable type to an indexable type
macro zfIndexableChk(a: untyped): untyped =
  result = quote:
    when not compiles(`a`[0]) or not compiles(high(`a`)):
      when not compiles(mkIndexable(`a`)):
        static:
          zfFail("need to provide an own implementation for mkIndexable(" & $`a`.type & ")")
      else:
        var `a` = mkIndexable(`a`)

## Wraps the given node with `mkIndexable` when the node type does not support access with `[]`
proc wrapIndexable(a: NimNode): NimNode {.compileTime.} =
  let q = quote:
    zfIndexableChk(`a`)
  result = q

## Replaces zip(a,b,c) --> ... with something like
## a --> filter(idx <= min([b.high(),c.high()]) --> map(it,b[idx],c[idx])
## Types that do not support `high` and `[]` have to implement the function
## `mkIndexable`(`MyType`) returning a type that supports both functions.
## This should be done, when the type cannot simply be mapped to `[]` and `high` -
## see `MkListIndexable`.
proc replaceZip(args: NimNode) : NimNode {.compileTime.} =
  var idx = 0
  result = nnkStmtList.newTree()

  # zip(a,b,c) <=> a --> zip(b,c) <~> a --> map(a[idx],b[idx],c[idx])
  let highList = nnkBracket.newTree()
  for arg in args:
    # search for all zip calls and replace them with filter --> map
    if arg.kind == nnkCall and arg[0].label == $Command.zip:
      let zipCmd = arg.copyNimTree()
      let params = newPar()
      if idx > 0:
        # a[idx]
        params.add(nnkBracketExpr.newTree(args[0], newIdentNode(zfIndexVariableName)))
        result.add(args[0].wrapIndexable())
      for paramIdx in 1..<zipCmd.len:
        let p = zipCmd[paramIdx]
        if p.kind != nnkInfix and p.kind != nnkBracketExpr and p.label != zfIteratorVariableName:
          result.add(p.wrapIndexable())
          highList.add(nnkCall.newTree(newIdentNode("high"), p))
          params.add(nnkBracketExpr.newTree(p, newIdentNode(zfIndexVariableName))) # map(it,b[idx],...)
        elif idx > 0:
          params.add(p)
        else:
          zfFail("No complex arguments allowed in 'zip' operation when used as first command - rather use 'a --> zip(it, ...)'.")

      if idx == 0:
        # set the first arg of zip as the first arg in the chain (e.g. moving `a` to the left of `-->`)
        args.insert(0, zipCmd[1]) # a --> ...
        idx = 1

      args[idx] = newCall($Command.map, params)
    idx += 1
  if highList.len > 0:
    let minHigh = newIdentNode(zfMinHighVariableName)
    result.add quote do:
      let `minHigh`= min(`highList`)

## Gets the result type, depending on the input-result type and the type-description of the input type.
## When the result type was given explicitly by the user that type is used.
## Otherwise the template argument is determined by the input type.
proc getResType(resultType: string, td: string): (NimNode, bool) {.compileTime.} =
  if resultType.len == 0:
    return (nil, false)
  var resType = resultType
  let explicitType = not resultType.endswith(implicitTypeSuffix)
  if not explicitType:
    resType = resType[0..resType.len-1-implicitTypeSuffix.len]

  let idx = resType.find("[")
  if idx != -1:
    result = (parseExpr(resType), explicitType)
  else:
    let res = newIdentNode(resType)
    let idx2 = td.find("[")
    var q : NimNode
    if idx2 != -1:
      var tdarg = td[idx2+1..td.len-2]
      let idxComma = tdarg.find(", ")
      let idxBracket = tdarg.find("[")
      if idxComma != -1 and (idxBracket == -1 or idxBracket > idxComma) and resType != "array":
        # e.g. array[0..2,...] -> seq[...]
        tdarg = tdarg[idxComma+2..tdarg.len-1]
      q = parseExpr(resType & "[" & tdarg & "]")
    else:
      q = quote:
        `res`[int] # this is actually a dummy type
    result = (q, false)

macro iteratorTypeTd(td: typedesc): untyped =
  let kind = td.getType[1][1].kind
  if kind == nnkBracketExpr and td.getType[1][1][0].kind == nnkSym and td.getType[1][1][0].repr == "array" and td.getType[1][1][1].kind == nnkBracketExpr and td.getType[1][1][1][0].repr == "range":
    # special handling for ranges (in arrays)
    let f = td.getType[1][1][1][1]
    let t = td.getType[1][1][1][2]
    let tpe = td.getType[1][1][2]
    result = quote:
      array[`f`..`t`, `tpe`]
  elif kind == nnkBracketExpr:
    # special handling for tuples
    result = newPar()
    for i in 1..td.getType[1][1].len-1:
      result.add(td.getType[1][1][i])
  elif kind == nnkEnumTy:
    result = td.getTypeInst[1][0][0]
  else:
    result = td.getType[1][1]

macro iteratorType*(td: untyped): untyped =
  quote:
    iteratorTypeTd(`td`.type)

proc createIteratorFunction(args: NimNode): NimNode =
  let iterName = newIdentNode(zfInternalIteratorName)
  if createProc:
    result = quote:
      proc `iterName`(): auto =
        nil
  else:
    result = quote:
      iterator `iterName`(): auto =
        nil


## Creates the function that returns the final result of all combined commands.
## The result type depends on map, zip or flatten calls. It may be set by the user explicitly using to(...)
proc createAutoProc(ext: ExtNimNode, args: NimNode, isSeq: bool, resultType: string, td: string, loopDef: NimNode, forceSeq: bool, isIter: bool): NimNode =
  var (resType, explicitType) = getResType(resultType, td)
  var collType = if resType != nil: resType.repr else: td

  var hasIter = false
  var itFun: NimNode = nil
  var itDef: NimNode = nil

  if isSeq and not isIter and (resType != nil or forceSeq):
    if not explicitType and (collType.find("[") != -1 or forceSeq):
      hasIter = true
      itFun = args.createIteratorFunction()
      itFun.getStmtList().add(loopDef)
      itDef = itFun[0]

  let resultIdent = newIdentNode("result")
  var autoProc = quote:
    (proc(): auto =
      nil)
  var code: NimNode = nil

  # set a default result in case the resType is not nil - this result will be used
  # if there is no explicit map, zip or flatten operation called
  if resType != nil:
    code = quote:
      var res: `resType`
      `resultIdent` = zfInit(res)

  # check explicitType: type was given explicitly (inclusive all template arguments) by user,
  # then we use resType directly:
  if explicitType:
    discard # use default result above
  elif isSeq:
    # now we try to determine the result type of the operation automatically...
    # this is a bit tricky as the operations zip, map and flatten may / will alter the result type.
    # hence we try to apply the map-operation to the iterator, etc. to get the resulting iterator (and list) type.
    var listRef = args[0]
    if listRef.kind == nnkCall and listRef[0].label == $Command.zip:
      listRef = listRef.findNode(nnkPar)[0][0]

    let i = collType.find("[")
    let isTuple = collType.startswith("(") and collType.endswith(")")
    if not isTuple and (i != -1 and hasIter and (not forceSeq or resultType.len > 0)):
      collType = collType[0..i-1]
      let collSym = parseExpr(collType)
      let resDef =
        if collType == "array":
          quote:
            array[`listRef`.len, iteratorType(`itDef`)]
        else:
          quote:
            `collSym`[iteratorType(`itDef`)]
      code = quote:
        var res: `resDef`
        `resultIdent` = zfInit(res)

    elif isTuple:
      let num = collType.split(",").len()
      ext.needsIndex = true
      if num < 2 or num > zfMaxTupleSize:
        zfFail("Tuple return types are only supported from 2 up to $1 elements" % [$zfMaxTupleSize])
      let x = genSym(nskVar, "x")
      let init = nnkAsgn.newTree(`resultIdent`, nnkPar.newTree())
      for _ in 1..num:
        init[1].add(x)
      code = quote:
        var `x`: iteratorType(`itDef`)
        `init`

    elif forceSeq and hasIter:
      let coll = newIdentNode(defaultCollectionType)
      code = quote:
        var res: `coll`[iteratorType(`itDef`)]
        `resultIdent` = zfInit(res)
    else:
      # use the same type as in the original list
      code = nnkStmtList.newTree().add quote do:
        `resultIdent` = zfInit(`listRef`)

  if isSeq:
    let idx = newIdentNode(zfIndexVariableName)
    # unfortunatly it does not (yet) work in Nim to define iterators anywhere in Nim and get a result from it
    # so defining an iterator inside an anonymous proc (this generated one) - inside another proc - yields no results
    # so we copy everything - this is also a tad more efficient for the running code in the end...
    if hasIteratorBug or not hasIter:
      var addLoop = loopDef.findNode(nnkStmtList)
      if hasIter:
        addLoop = addLoop.copyNimTree()
      # there should only be one yield statement - replace it with zfAddItem
      let (yieldStmt,path) = addLoop.findNodePath(nnkYieldStmt)
      code.add(addLoop.apply(path, ext.addElemResult(yieldStmt[0])))
      # due to a further bug in nim we now replace the iterator with an actual proc
      if createProc and hasIter:
        let yield2 = loopDef.findNode(nnkYieldStmt)
        if yield2 != nil:
          let ret2 = nnkReturnStmt.newTree(yield2[0])
          discard loopDef.replace(yield2, ret2)
    else:
      let it = newIdentNode(internalIteratorName)
      code.add quote do:
        var `idx` = 0
        for `it` in `itDef`():
          zfAddItem(`resultIdent`, `idx`, `it`)
          `idx` += 1

  # no sequence output:
  # we do _not_ need to initialize the resulting list type here
  else:
    code = nil

  let stmtInit = autoProc.getStmtList()
  if code != nil:
    stmtInit.add(code)
  stmtInit.add(newNilLit())
  if itFun != nil:
    autoProc.findNode(nnkStmtList).insert(0, itFun)
  result = autoProc

## Check if the "to" parameter is used to generate a specific result type.
## The requested result type is returned and the "to"-node is removed.
proc checkTo(args: NimNode, td: string): string {.compileTime.} =
  let last = args.last
  let hasTo = last.kind == nnkCall and last[0].repr == $Command.to
  var resultType : string = ""
  if hasTo:
    args.del(args.len-1) # remove the "to" node
    resultType = last[1].repr
    if args.len <= 1:
      # there is no argument other than "to": add default mapping function "map(it)"
      args.add(parseExpr($Command.map & "(" & zfIteratorVariableName & ")"))
    else:
      if (not (args.last[0].label in SEQUENCE_HANDLERS)) and resultType != "iter":
        zfFail("'to' can only be used with list results - last arg is '" & args.last[0].label & "'")
    if resultType == "list": # list as a shortcut for DoublyLinkedList
      resultType = "DoublyLinkedList" & implicitTypeSuffix
    elif resultType.startswith("list["):
      resultType = "DoublyLinkedList" & resultType[4..resultType.len-1]
    elif resultType == "set": # set as a shortcut for HashSet
      resultType = "HashSet" & implicitTypeSuffix
    elif resultType.startswith("set["):
      resultType = "HashSet" & resultType[3..resultType.len-1]
    elif resultType == "seq":
      resultType = "seq[int]" & implicitTypeSuffix
  if resultType.len == 0:
    for arg in args:
      if arg.kind == nnkCall:
        let label = arg[0].repr
        # shortcut handling for mapSeq(...)  <=> map(...).to(seq) and
        #                       mapList(...) <=> map(...).to(list) - etc.
        let isSeq = label.endswith("Seq")
        let isList = label.endswith("List")
        # Check forced sequences or lists
        if isSeq or isList:
          if isSeq:
            arg[0] = newIdentNode(label[0..label.len-4])
          elif isList:
            arg[0] = newIdentNode(label[0..label.len-5])
          if isSeq or isList or resultType.len == 0:
            resultType =
              if isSeq:
                "seq"
              elif isList:
                "DoublyLinkedList"
              elif (td.startswith("DoublyLinkedList")):
                td & implicitTypeSuffix
              else:
                defaultResultType # default to sequence - and use it if isSeq is used explicitly
  if resultType.len == 0 and td == "enum":
    resultType = "seq[" & $args[0] & "]" & implicitTypeSuffix
  result = resultType

## Main function that creates the outer function call.
proc iterHandler(args: NimNode, td: string, debugInfo: string): NimNode {.compileTime.} =
  let debug = debugInfo.len() > 0
  let orig = if debug: args.copyNimTree() else: nil
  var resultType = args.checkTo(td)
  let preInit = args.replaceZip() # zip is replaced with map + filter
  let hasMinHigh = preInit.len > 0
  let lastCall = args.last[0].label
  let toIter = resultType == "iter" or (resultType.len == 0 and defined(zf_iter))
  if toIter and defined(js):
    resultType = ""
  let isIter = lastCall == $Command.createIter or (toIter and not defined(js))
  var iterNode: NimNode = nil
  var isClosure = toIter
  var iterName: NimNode = nil
  if isIter:
    if not toIter:
      if args.last.len > 2:
        # parse 2nd argument of createIter (only "closure" allowed here)
        let closureParam = args.last[2]
        var closureVal: NimNode = nil
        if closureParam.kind == nnkExprEqExpr:
          if closureParam[0].label == "closure":
            closureVal = closureParam[1]
          else:
            zfFail("Unknown parameter '$1' to 'createIter'" % [closureParam[0].label])
        else:
          closureVal = closureParam
        if closureVal.label == "true":
          when not defined(js): # closure not supported by JS backend
            isClosure = true
          else:
            warning("closure not supported by JS backend")
        elif closureVal.label != "false":
          zfFail("Unsupported parameter value '$1' to 'createIter'" % [closureVal.label])
      iterName = newIdentNode(args.last[1].label)
      args.del(args.len-1)
    else:
      iterName = newIdentNode(zfInternalIteratorName)
    if args.last.len > 0 and not (args.last[0].label in SEQUENCE_HANDLERS):
      zfFail("'iter' can only be used with list results - last arg is '" & args.last[0].label & "'")

    if isClosure:
      # create closure iterator that delegates to the inline iterator
      let inlineName = newIdentNode(iterName.label & "_inline")
      let it = newIdentNode("it")
      iterNode = quote:
        iterator `inlineName`(): auto {.inline.} =
          nil
        iterator `iterName`(): type(`inlineName`()) {.closure.} =
          for `it` in `inlineName`():
            yield `it`
    else:
      iterNode = quote:
        iterator `iterName`(): auto {.inline.} =
          nil

  let isSeq = lastCall in SEQUENCE_HANDLERS

  var defineIdxVar = (not hasMinHigh and not isIter and not td.startswith("Option[")) and (isSeq and hasIteratorBug)
  var needsIndexVar = false

  if ((not isIter and (isSeq and (resultType.len > 0 and resultType.startswith("array") or
      (resultType.len == 0 and td.startswith("array"))))) or
      args.findNode(nnkIdent, zfIndexVariableName) != nil):
      needsIndexVar = true

  let init = nnkStmtList.newTree()
  let initials = nnkStmtList.newTree()
  let varDef = nnkStmtList.newTree()
  init.add(varDef)
  init.add(initials)

  var codeStart = nnkStmtList.newTree()
  var code = codeStart

  var index = 1
  let listRef = args[0]
  let finals = nnkStmtList.newTree()
  let endLoop = nnkStmtList.newTree()

  var argIdx = 1
  var ext: ExtNimNode
  var forceIndexLoop = false
  var delegateUntil = -1

  while argIdx <= args.len: # args could be changed
    let argIdxReal = if argIdx == args.len: 0 else: argIdx
    let arg = args[argIdxReal]
    let isLast = argIdxReal == args.len-1
    var oldIndex = index
    if argIdxReal == 0:
      index = 0
    ext = ExtNimNode(node: arg,
                    nodeIndex: argIdxReal,
                    prevItIndex: index-1,
                    itIndex: index,
                    isLastItem: isLast,
                    initials: initials,
                    endLoop: endLoop,
                    finals: finals,
                    listRef: listRef,
                    args: args,
                    typeDescription: td,
                    resultType: resultType,
                    needsIndex: needsIndexVar,
                    hasMinHigh: hasMinHigh,
                    isIter: isIter,
                    adapted: -1,
                    elemAdded: false,
                    forceIndexLoop: forceIndexLoop,
                    delegateUntil: delegateUntil)
    argIdx += 1
    ext.inlineElement()
    forceIndexLoop = forceIndexLoop or ext.forceIndexLoop
    delegateUntil = ext.delegateUntil
    index = ext.itIndex
    let newCode = ext.node.getStmtList()

    if argIdxReal == 0:
      # the actual outer loop is created last - so insert the functions here
      newCode.add(codeStart)
      # and set the current outer loop as new codeStart
      codeStart = nnkStmtList.newTree().add(ext.node)
      index = oldIndex
    else:
      code.add(ext.node)

    # make sure the collection is created
    if (argIdx == args.len) and not ext.elemAdded and arg.label in SEQUENCE_HANDLERS:
      var node = newCode
      if node == nil:
        # directly append the adding function
        node = code
      node.add(ext.addElem(mkItNode(ext.itIndex-1)))

    if newCode != nil:
      code = newCode
    if not hasMinHigh and not defineIdxVar:
      defineIdxVar = ext.needsIndex
  # end of loop

  if not hasMinHigh and defineIdxVar:
    let idxIdent = newIdentNode(zfIndexVariableName)
    varDef.add quote do:
      var `idxIdent` = 0

  # add the actual loop section
  init.add(codeStart)

  # add the endLoop section
  # could be combinations of for and while, but only one while (for list types)
  var loopNode: NimNode = nil
  if endLoop.len > 0 or (not hasMinHigh and defineIdxVar):
    loopNode = if (td.isListType() or ext.forceIndexLoop): codeStart.findNode(nnkWhileStmt) else: codeStart.findNode(nnkForStmt)
  if endLoop.len > 0:
    assert(loopNode != nil)
    loopNode.last.add(endLoop)
  if loopNode != nil and not hasMinHigh and defineIdxVar:
    # add index increment to end of the for loop
    let idxIdent = newIdentNode(zfIndexVariableName)
    loopNode.last.add quote do:
      `idxIdent` += 1

  # add the finals section
  if finals.len > 0:
    init.add(finals)

  let needsFunction = (lastCall != $Command.foreach)
  if needsFunction:
    var theProc: NimNode = nil
    if isIter:
      if toIter: # used with to(iter)
        idents(resultIdent("result"))
        theProc = quote:
          (proc(): auto =
            `iterNode`
            `resultIdent` = `iterName`)
      else:
        theProc = iterNode # used with createIter(name)
    else:
      theProc = ext.createAutoProc(args, isSeq, resultType, td, init, index > 1, false)
    result = theProc
    code = result.last.getStmtList()
    if isIter:
      code = result.getStmtList()
      if isClosure:
        if toIter:
          code = result.findNode(nnkIteratorDef).getStmtList()
        else:
          code = result[0].getStmtList()
      code.add(init)
    elif not isSeq:
      code.insert(0,init)
    if preInit.len > 0:
      code.insert(0, preInit)
    if ((not isIter) or toIter) and (not defined(js) or not toIter):
      # kind of a hack: with JS backend and toIter we do not add additioal brackets to the call
      # here a seq is returned, but is used later the same way iterators would: with additional brackets
      result = nnkCall.newTree(result)
  else:
    # there is no extra function, but at least we have an own section here - preventing double definitions
    var q = quote:
      block:
        `preInit`
        nil
    result = q
    code = q.getStmtList().add(init)

  if debug:
    echo ""
    echo debugInfo
    echo "# " & orig[0].repr & ".zfun:"
    var i = 1
    var fun = ""
    while i < orig.len():
      fun = orig[i].label
      if fun != "" and i < orig.len() - 2 and fun == orig[i+1].label:
        echo "#   " & orig[i].label & ":"
        while fun == orig[i].label:
          echo "#     " & orig[i][1].repr
          i += 1
      else:
        echo "#   " & orig[i].repr
        i += 1

    echo(repr(result).replace("__", ""))
    # for the whole tree do (but this could crash):
    # echo(treeRepr(result))

proc dbgLineInfo(a: NimNode, debug: bool): string =
  if debug or debugAll:
    let l = a.lineInfoObj
    var idx = l.filename.rfind('\\')
    if idx == -1:
      idx = l.filename.rfind('/')
    result = "# $1:$2" % [$l.filename[idx+1..^1], $l.line]
  else:
    result = ""

macro connectCall(td: typedesc, callInfo: untyped, args: varargs[untyped]): untyped =
  result = iterHandler(args, getTypeInfo(td.getType[1], td.getTypeInst[1]), callInfo.repr[1..^2])

## Preparse the call to the iterFunction.
proc delegateMacro(a: NimNode, b1:NimNode, td: string, debugInfo: string): NimNode =
  var b = b1

  # we expect b to be a call, but if we have another node - e.g. infix or bracketexpr - then
  # we search for the actual call, do the macro expansions on the call and
  # add the result back into the tree later
  var outer = b
  var path : seq[int] = @[]
  if b.kind != nnkCall:
    var call : NimNode
    (call,path) = outer.findNodePath(nnkCall)
    if call != nil:
      b = call
    else:
      zfFail("Unexpected expression in macro call on right side of '-->'")

  # now re-arrange all dot expressions to one big parameter call
  # i.e. a --> filter(it > 0).map($it) gets a.connect(filter(it>0),map($it))
  # SinglyLinkedList iterates over items in reverse order they have been prepended
  var m = initSinglyLinkedList[NimNode]()
  # b contains the calls in a tree - the first calls are deeper in the tree
  # this has to be flattened out as argument list
  var node = b
  let args = nnkArgList.newTree().add(a)
  while node.kind == nnkCall:
    if node[0].kind == nnkDotExpr:
      m.prepend(nnkCall.newTree(node[0].last))
      for z in 1..<node.len:
        m.head.value.add(node[z])
      node = node[0][0] # go down in the tree
    elif node[0].kind == nnkIdent:
      m.prepend(node)
      break
    else:
      break
  for it in m:
    args.add(it)
  result = iterHandler(args, td, debugInfo)

  if path.len > 0: # insert the result back into the original tree
    result = outer.apply(path, result)

## delegate call to get the type information.
macro delegateArrow(td: typedesc, a: untyped, b: untyped, debugInfo: untyped): untyped =
  result = delegateMacro(a, b, getTypeInfo(td.getType[1], td.getTypeInst[1]), debugInfo.repr[1..^2])

## The arrow "-->" should not be part of the left-side argument a.
proc checkArrow(a: NimNode, b: NimNode, arrow: string): (NimNode, NimNode, bool) =
  var b = b
  if a.kind == nnkInfix:
    let ar = a.repr
    let idx = ar.find(arrow)
    if idx != -1:
      # also replace the arrows with "."
      let debug = ar[idx+arrow.len] == '>' # debug arrow -->> is used
      let add = if debug: 1 else: 0
      let br = (ar[idx+arrow.len+add..ar.len-1] & "." & b.repr).replace(arrow, ".")
      return (parseExpr(ar[0..idx-1]), parseExpr(br), debug)
  result = (a,b,false)

## Alternative call with comma separated arguments.
macro connect*(args: varargs[untyped]): untyped =
  result = quote:
    connectCall(type(`args`[0]), `args`)

macro zfunCall(td: typedesc, debug: static[bool], a: untyped, b: untyped): untyped =
  let b2 = nnkStmtList.newTree(a)
  for c in b:
    if c.kind == nnkCall and c.len == 2 and c[1].kind == nnkStmtList:
      for callParam in c[1]:
        b2.add(newCall(c[0], callParam))
    else:
      b2.add(c)
  result = iterHandler(b2, getTypeInfo(td.getType[1], td.getTypeInst[1]), b.dbgLineInfo(debug or debugAll))

macro zfun*(a: untyped, b: untyped): untyped =
  result = quote:
    zfunCall(type(`a`), false, `a`, `b`)

macro zfunDbg*(a: untyped, b: untyped): untyped =
  result = quote:
    zfunCall(type(`a`), true, `a`, `b`)

## general macro to invoke all available zero_functional functions
macro `-->`*(a: untyped, b: untyped): untyped =
  let (a,b2,debug) = checkArrow(a,b,"-->")
  let l = b.dbgLineInfo(debug or debugAll)
  if a.label != $Command.zip:
    if not debug: # using debug (or `debug`) directly does not work (?!)
      result = quote:
        delegateArrow(type(`a`), `a`, `b2`, `l`)
    else:
      result = quote:
        delegateArrow(type(`a`), `a`, `b2`, `l`)
  else:
    result = delegateMacro(a, b2, "seq", l)

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  let (a,b2,_) = checkArrow(a,b,"-->")
  let l = b2.dbgLineInfo(true)
  if a.label != $Command.zip:
    result = quote:
      delegateArrow(type(`a`), `a`, `b2`, `l`)
  else:
    result = delegateMacro(a, b2, "seq", l)
