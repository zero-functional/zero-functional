import macros, options, sets, lists, typetraits, strutils, tables

const zfIteratorVariableName* = "it"
const zfAccuVariableName* = "a"
const zfIndexVariableName* = "idx"
const zfListIteratorName* = "__itList__"
const zfMinHighVariableName* = "__minHigh__"
const zfInternalHelperProc* = "__iterType__"
const zfInternalIteratorName* = "__autoIter__"
const zfIndexedElemName* = "elem"
const zfIndexedIndexName* = "idx"
const zfAccuName* = "accu"

const zfArrow = "-->"
const zfArrowDbg = "-->>"
const callSuffix = "__call__"
const internalIteratorName = "__" & zfIteratorVariableName
const useInternalAccu = zfAccuVariableName != "result"
const internalAccuName =
  if (useInternalAccu): "__" & zfAccuVariableName
  else: "result"
const zfMaxTupleSize = 10

# if set to true: turns on prints code generated with zf (for macros -->, zfun and connect)
when defined(zf_debug_all):
  const debugAll = true
else:
  const debugAll = false

when defined(zf_iter) and defined(js):
  static:
    warning("Iterator result not supported by JS backend - d:zf_iter will be ignored.")
when defined(zf_iter) and not defined(js):
  const defaultCollectionType = ""
  const defaultResultType = "iter"
else:
  when defined(zf_list):
    const defaultCollectionType = "DoublyLinkedList"
  else:
    const defaultCollectionType = "seq"
  const defaultResultType = defaultCollectionType & "[int]"

## Debug print the generated code
## Removing some of the internal macro stuff.
proc printCode(code: NimNode) =
  let gen = "`gensym"
  var c = repr(code)
  var i = c.find(gen, 0)
  while i != -1:
    var i2 = i + gen.len
    while c[i2].isDigit():
      i2 += 1
    c = c.substr(0, i-1) & c.substr(i2)
    i = c.find(gen, i)
  var a = c.split("\n")
  for i in 0..<a.len:
    if a[i].contains("zfParamChk") or a[i].contains("zfIndexableChk"):
      a[i] = ""
  c = a.join("\n").replace("\n\n", "\n").replace("__call__", callSuffix).replace("__", "")
  echo(c)

type

  Command* {.pure.} = enum
    ## All available commands.
    ## 'to' - is a virtual command
    all, combinations, concat, count, createIter, drop, dropWhile, exists,
        filter, find, flatten, fold, foreach,
    index, indexedCombinations, indexedFlatten, indexedMap, enumerate, indexedReduce,
        map, reduce, sub, zip, take, takeWhile, to, uniq

  ReduceCommand {.pure.} = enum
    ## additional commands that operate as reduce command
    max, min, product, sum

  ResultType = object
    id: string              ## the result type to create
    implicit: bool          ## set to true when the user did not give an explicit type
    autoConvert: bool       ## set to true when the second parameter of `to` is true - then the conversion (e.g. between numeric types) is tried automatically

  ExtNimNode* = ref object  ## Store additional info the current NimNode used in the inline... functions
    node*: NimNode          ## the current working node / the current function
    nodeIndex: int          ## the position in args of the current working node
    prevItIndex*: int       ## index used for the previous iterator
    itIndex*: int           ## index used for the created iterator - 0 for the first. Will be incremented automatically.
    maxIndex*: int          ## maximum created interator index
    isLastItem: bool        ## true if the current item is the last item in the command chain
    initials*: NimNode      ## code section before the first iterator where variables can be defined
    endLoop*: NimNode       ## code at the end of the for / while loop
    finals*: NimNode        ## code to set the final operations, e.g. the result
    listRef*: NimNode       ## reference to the list the iterator is working on
    args: NimNode           ## all arguments to the original macro
    typeDescription: string ## type description of the outer list type
    resultType: ResultType  ## result type when explicitly set
    needsIndex*: bool       ## true if the idx-variable is needed
    hasMinHigh: bool        ## true if the minHigh variable is defined and the loop should use indices rather than iterator
    isIter: bool            ## true if an iterator shall be created
    adapted: int            ## internally used to check that all iterators were created before the adapt call (otherwise adapt refers to an old iterator)
    elemAdded: bool         ## set when `addElem` has been called. Needed for generating collection output.
    forceIndexLoop: bool    ## set when a loop with index rather than with iterator is needed (i.e. the loop changes the iterated collection)
    delegateUntil: int      ## last argument index that is part of a "delegate" chain

  ## used for "combinations" command as output
  Combination*[A, T] = object
    it*: array[A, T]
    idx*: array[A, int]

type
  FiniteIndexable* = concept a
    a.low() is int
    a.high() is int
    a[int]

  FiniteIndexableLen* = concept a
    a.len() is int
    a[int]

  FiniteIndexableLenIter*[T] = concept a
    a.len() is int
    a[int] is T
    for it in a:
      it is T

  Iterable*[T] = concept a
    for it in a:
      it is T

  Appendable*[T] = concept a, var b
    for it in a:
      it is T
    b.append(T)

  Addable*[T] = concept a, var b
    for it in a:
      it is T
    b.add(T)

## Contains all functions that may result in a sequence result. Elements are added automatically to sequenceHandlers
var sequenceHandlers {.compileTime.} = [$Command.to].toHashSet()

## Can be read in test implementation
var lastFailure {.compileTime.}: string = ""

proc zfGetLastFailure*(): string {.compileTime.} =
  result = lastFailure
  lastFailure = ""

## Called when some failure is detected during macro processing.
proc zfFail*(msg: string) {.compileTime.} =
  lastFailure = msg
  error(msg)

## This is the default extension complaining when a given function was not found.
proc extendDefault(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  zfFail("$1 is unknown, you could provide your own implementation! See `zfCreateExtension`!" %
      ext.node[0].repr)

## If you want to extend `zero_functional` assign this variable in your own code to a function with the given signature.
## The assignment has to be done during compile time - so for instance in a macro.
## See `test.nim` for an example of how to do that.
var zfExtension {.compileTime.}: proc(ext: ExtNimNode): ExtNimNode = extendDefault
var zfFunctionNames {.compileTime.}: seq[string] = @[]

## Adds the given function name to the internal table of supported functions.
## This will be done automatically when using the macros `zfInline` or `zfInlineCall`.
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
  sequenceHandlers.incl(seqHandlers.toHashSet)

## same as zfAddSequenceHandlers(seq[string]) added for convenience
proc zfAddSequenceHandlers*(seqHandlers: varargs[string]) {.compileTime.} =
  sequenceHandlers.incl(seqHandlers.toHashSet)

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

proc findNodeParents(node: NimNode, discriminator: proc (
    n: NimNode): bool): seq[NimNode] =
  result = @[]
  if discriminator(node):
    return @[node]
  for child in node:
    let res = child.findNodeParents(discriminator)
    if res.len > 0 and (result.len == 0 or result.len > res.len + 1):
      result = res
      result.add(node)

proc findNodeParents(node: NimNode, kind: NimNodeKind,
    content: string = ""): seq[NimNode] =
  proc discriminator(node: NimNode): bool =
    result = node.kind == kind and (content.len == 0 or content == node.label)
  result = node.findNodeParents(discriminator)

## check if the discriminator function is valid for the node or one of its children (depth-first).
proc findNode(node: NimNode, discriminator: proc (n: NimNode): bool): NimNode =
  result = nil
  if discriminator(node):
    return node
  for child in node:
    let res = child.findNode(discriminator)
    if res != nil:
      return res

proc findNode*(node: NimNode, kind: NimNodeKind, content: string = ""): NimNode =
  ## Find a node given its kind and - optionally - its content.
  result = nil
  if node.kind == kind and (content == "" or content == node.label):
    return node
  for child in node:
    let res = child.findNode(kind, content)
    if res != nil:
      return res

proc findIdent*(node: NimNode, label: string): NimNode =
  result = node.findNode(nnkIdent, label)

proc hasIt*(node: NimNode): bool =
  result = node.findIdent(zfIteratorVariableName) != nil

proc findDefinition*(node: NimNode, label: string,
    kind = nnkLetSection): NimNode =
  ## Find the definition of a constant or variable
  proc hasDefinition(node: NimNode): bool =
    result = (node.kind == kind and node.findIdent(label) != nil)
  result = node.findNode(hasDefinition)

## Replace a given node by another in a specific parent
proc replace(node: NimNode, searchNode: NimNode, replNode: NimNode,
    all: bool = false) =
  if node.len > 0:
    for i in 0..<node.len:
      let child = node[i]
      if child.kind == searchNode.kind and (child == searchNode or
          all and child.repr == searchNode.repr):
        node[i] = replNode
      elif all:
        child.replace(searchNode, replNode, all)

proc getNilStmtParent(node: NimNode): (NimNode, int) = 
  result = (nil, -1)
  var n = node
  for i in countdown(n.len-1, 0):
    let c = n[i]
    let res = c.getNilStmtParent()
    if res[0] != nil:
      return res
    if c.kind == nnkNilLit:
      return (n, i)

## Helper that gets nnkStmtList and removes a 'nil' inside it - if present.
## The nil is used as placeholder for further added code.
proc getStmtList*(node: NimNode, removeNil = true) : NimNode =
  if removeNil:
    let parent = node.getNilStmtParent()
    if parent[0] != nil:
      parent[0].del(parent[1])
      return parent[0]
  var n = node
  while n.kind != nnkStmtList or (n.len > 0 and n.last.kind == nnkStmtList):
    n = n.last
  result = n


macro idents(args: varargs[untyped]): untyped =
  ### shortcut implementation
  ### idents(ext) <=> let ext = newIdentNode("ext")
  ### exception for result:
  ### idents(result) <=> let resultIdent = newIdentNode("result")
  result = newStmtList()
  for a in args:
    var arg = a
    var s = repr(a)
    let idx = s.find("(")
    if idx != -1:
      arg = newIdentNode(s[0..idx-1])
      s = s[idx+1..s.len-2]
      if s[s.len-1] != '"':
        # refer to the given variable name (without the string quotes)
        result.add(nnkLetSection.newTree(newIdentDefs(arg, newEmptyNode(),
            newCall("newIdentNode", newIdentNode(s)))))
        continue
      else:
        # remove the quotes
        s = s[1..s.len-2]
    result.add quote do:
      let `arg` = newIdentNode(`s`)

## Creates the extension function
proc createExtensionProc(name: string, cmdSeq: seq[string]): (NimNode, NimNode) =
  let procName = newIdentNode("zfExtend" & name)
  let cseq = if cmdSeq.len > 0: cmdSeq else: zfFunctionNames
  idents(ext)
  let procDef = quote:
    proc `procName`(`ext`: ExtNimNode): ExtNimNode {.compileTime.} =
      result = `ext`
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
  let (funDef, funName) = createExtensionProc("UserExt", @[])
  result = quote:
    `funDef`
    zfSetExtension(`funName`)

## Create a delegate function for user-defined functions and register sequence handler functions.
macro zfCreateExtension*(additionalFunctions: static[seq[string]],
    seqHandlers: static[seq[string]]): untyped =
  for addFun in additionalFunctions:
    addFunction(addFun)
  zfAddSequenceHandlers(seqHandlers)
  result = quote:
    zfCreateExtension()

## Determines the closest possible type info of the input parameter to "-->".
## Sometimes the getType (node) works best, sometimes getTypeInst (nodeInst).
proc getTypeInfo(node: NimNode, nodeInst: NimNode): string =
  if node.len > 0:
    if node.kind == nnkEnumTy:
      result = "enum"
    elif $node[0] == "ref":
      result = $node[1]
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
proc toEnum*[T: typedesc[enum]](key: string; t: T): auto =
  result = none(t)
  for it in t:
    if $it == key:
      result = some(it)
      break

## Converts the id-string to its ReduceCommand counterpart.
proc toReduceCommand(key: string): Option[ReduceCommand] =
  if key.startsWith("indexed"):
    return key[7..key.len-1].toLowerAscii().toReduceCommand()
  result = key.toEnum(ReduceCommand)

{.push inline.}
## Special implementation to initialize array output.
proc zfInit*[A, T, U](s: array[A, T], handler: proc(it: T): U): array[A, U] =
  discard

## Special implementation to initialize DoublyLinkedList output.
proc zfInit*[T, U](a: DoublyLinkedList[T], handler: proc(
    it: T): U): DoublyLinkedList[U] =
  initDoublyLinkedList[U]()
## Special implementation to initialize SinglyLinkedList output.
proc zfInit*[T, U](a: SinglyLinkedList[T], handler: proc(
    it: T): U): SinglyLinkedList[U] =
  initSinglyLinkedList[U]()
## Special implementation to initialize HashSet output.
proc zfInit*[T, U](a: HashSet[T], handler: proc(it: T): U): HashSet[U] =
  initHashSet[U]()
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

proc createCombination*[A, T](it: array[A, T], idx: array[A, int]): Combination[A, T] =
  result = Combination[A, T](it: it, idx: idx)

## iterator over tuples (needed for flatten to work on tuples, e.g. from zipped lists)
## NOTE: this iterator can only be used for tuples containing elements of the
## same type! Otherwise it errors at compile time.
iterator items*[T: tuple](a: T): auto =
  for i in a.fields:
    yield i

## iterate over concept FiniteIndexable
iterator items*[T: FiniteIndexable](f: T): auto =
  for i in f.low()..f.high():
    yield f[i]

## iterate over concept FiniteIndexableLen
iterator items*[T: FiniteIndexableLen](f: T): auto =
  for i in 0..<f.len():
    yield f[i]

## Add item to array
proc zfAddItem*[A, T](a: var array[A, T], idx: int, item: T) =
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

## Add item to seq, but converting from type U to T by explicit cast
proc zfAddItemConvert*[T, U](a: var Iterable[T], idx: int, item: U) =
  zfAddItem(a, idx, T(item))

## Special implementations for tuple code (max `zfMaxTupleSize` items)
## The macro generates the zfAddItem[T] procs.
## For a tuple of size 2 this looks like:
#[
  proc zfAddItem[T](a: var (T,T), idx: int, item: T) =
    case idx:
    of 0: a[0] = item
    of 1: a[1] = item
    else: assert(false)
]#
macro genZfAddItemTuple(maxTupleSize: static[int]): untyped =
  result = newStmtList()
  idents(T, a, idx(zfIndexVariableName), item)
  let t = newPar(T)
  let cases = quote:
    case `idx`:
      of 0: `a`[0] = `item`
      else: assert(false)
  for l in 2..maxTupleSize:
    t.add(T)
    let l1 = l-1
    let c = quote:
      `a`[`l1`] = `item`
    cases.insert(l1, nnkOfBranch.newTree(newIntLitNode(l1), c))
    result.add quote do:
      proc zfAddItem[`T`](`a`: var `t`, `idx`: int, `item`: `T`) =
        `cases`
genZfAddItemTuple(zfMaxTupleSize)

{.pop.}

proc mkItNode*(index: int): NimNode {.compileTime.} =
  newIdentNode(internalIteratorName & ("$1" % $index))

proc nextItNode*(ext: ExtNimNode): NimNode {.compileTime.} =
  if ext.adapted != -1 and ext.adapted != ext.prevItIndex:
    zfFail("ext has already been adapted! Call (all) nextItNode _before_ calling adapt!")
  result = mkItNode(ext.itIndex)
  ext.itIndex += 1
  ext.maxIndex = max(ext.itIndex, ext.maxIndex)

proc prevItNode*(ext: ExtNimNode): NimNode {.compileTime.} =
  result = mkItNode(ext.prevItIndex)

proc res*(ext: ExtNimNode): NimNode {.compileTime.} =
  result = newIdentNode("result")

## Replace the variable name `it` with the continuos iterator variable name.
## The same goes for the accu `a`.
## Expressions on the left side of dot `.` are not replaced - because `it`could
## also be a member of a compound type - so `it.someMember` is replaced, `c.it` is not.
proc adapt(node: NimNode, iteratorIndex: int, inFold: bool = false): NimNode {.compileTime.} =
  result = node

  if node.kind == nnkIdent:
    if $node == zfIteratorVariableName:
      result = mkItNode(iteratorIndex)
    elif inFold and useInternalAccu and $node == zfAccuVariableName:
      result = newIdentNode(internalAccuName)

  else:
    # adapt the child nodes
    for z in 0..<node.len:
      node[z] = node[z].adapt(iteratorIndex, inFold)
      if node.kind == nnkDotExpr:
        break # change only left side of of dotExpr or arrow
      if (z > 0 and (node.kind == nnkInfix and
        node[0].label.startsWith(zfArrow)) or
        (node.kind == nnkCall and node[0].kind == nnkDotExpr and
        node[0][1].label.startsWith("zfun"))):
        # arrow itself is node[0], node[1] could be changed but everything right of arrow should not be changed (z > 0)
        # this is only relevant for nested `-->` calls
        # this prevents replacing `it` in the context of the outer loop (outer `-->` call)
        # zfun should be same as `-->` except here it should break after the first parameter
        break

## Shortcut for `node.adapt()` using the current iterator variable.
## Variable names like `it` or `idx` are replaced by their internal (unique) presentations.
proc adapt*(ext: ExtNimNode, index = 1, inFold = false): NimNode {.compileTime.} =
  if (ext.adapted == -1) or (ext.adapted == ext.itIndex-1):
    ext.adapted = ext.itIndex-1
    result = ext.node[index].adapt(ext.itIndex-1, inFold)
  else:
    zfFail("ext has already been adapted with a different index!")
    result = ext.node[index]

proc isListType(td: string): bool =
  td.startsWith("DoublyLinkedList") or td.startsWith("SinglyLinkedList")

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

macro zfAddItemChk*(resultIdent: untyped, idxIdent: untyped, addItem: untyped,
    typedescr: static[string], resultType: static[string], autoConvert: static[
    bool]): untyped =
  result = quote:
    when compiles(zfAddItem(`resultIdent`, `idxIdent`, `addItem`)):
      zfAddItem(`resultIdent`, `idxIdent`, `addItem`)
    elif compiles(zfAddItemConvert(`resultIdent`, `idxIdent`, `addItem`)):
      zfAddItemConvert(`resultIdent`, `idxIdent`, `addItem`)
      # extra cast - see bug https://github.com/nim-lang/Nim/issues/7375
      when not bool(`autoConvert`):
        static:
          warning("Type " & $`addItem`.type & " was automatically converted to fit into " & $`resultType` &
                  "\nTo remove this warning either set second parameter of `to` to true or adapt the result type.")
    else:
      static:
        when (`resultType`.len == 0):
          zfFail("Need either 'add' or 'append' implemented in '" &
              `typedescr` & "' to add elements")
        else:
          zfFail("Result type '" & `resultType` & "' and added item of type '" &
              $`addItem`.type & "' do not match!")

proc addElemResult(ext: ExtNimNode, addItem: NimNode): NimNode {.compileTime.} =
  let resultIdent = ext.res
  # use -1 to force an error in case the index was actually needed instead of silently doing the wrong thing
  let idxIdent = if ext.needsIndex: newIdentNode(
      zfIndexVariableName) else: newIntLitNode(-1)
  let resultType = ext.resultType.id
  let autoConvert = newLit(ext.resultType.autoConvert)
  let typedescr = ext.typeDescription

  result = quote:
    zfAddItemChk(`resultIdent`, `idxIdent`, `addItem`, `typedescr`,
        `resultType`, `autoConvert`)

## Helper for Zero-DSL: quote all used variables that are defined somewhere in the created function.
proc addQuotes(a: NimNode, quotedVars: OrderedTable[string, string]) =
  if a.kind != nnkAccQuoted and (a.kind != nnkCall or a[0].label != "pre"):
    for i in 0..<a.len:
      let child = a[i]
      if child.kind == nnkIdent and child.label in quotedVars:
        a[i] = nnkAccQuoted.newTree(newIdentNode(quotedVars[child.label]))
      else:
        child.addQuotes(quotedVars)
      if a.kind == nnkDotExpr:
        break # only quote left side of dot expressions

## Helper for Zero-DSL: replace all `it` nodes by next or prev iteraor
proc replaceItSub(parent: NimNode, a: NimNode, repl: NimNode,
    left: bool): bool =
  result = false

  if a.kind == nnkIdent and a.label == zfIteratorVariableName:
    parent.replace(a, repl)
    result = true
  else:
    for child in a:
      result = a.replaceItSub(child, repl, left) or result
      if result and left and a.kind == nnkDotExpr:
        # only replace left side of dot
        break

## Helper for Zero-DSL: replace all 'it' nodes by the next iterator (in let expressions when defining a new iterator)
## or by previous iterator.
proc replaceIt(a: NimNode, replNext: NimNode, replPrev: NimNode): (bool, bool) =
  var res = (left: false, right: false)

  let replLeft = replNext;
  var replRight = replPrev;

  if a.kind == nnkYieldStmt:
    return res

  for child in a:
    let isAssignment = child.kind == nnkExprEqExpr or
      child.kind == nnkIdentDefs or child.kind == nnkAsgn

    if isAssignment:
      res.right = child.replaceItSub(child[child.len-1], replRight, false) or res.right
      res.left = child.replaceItSub(child[0], replLeft, true) or res.left
    else:
      if child.kind == nnkIdent and child.label == zfIteratorVariableName:
        a.replace(child, replRight)
        res.right = true
      else:
        let r = child.replaceIt(replLeft, replRight)
        res = (r[0] or res[0], r[1] or res[1])

    if res.left:
      replRight = replNext

  result = res

## Revert `it` node to the first item of the listref for init section _before_ the actual loop section
## as `it` is only defined within the loop section
proc revertIt(ext: ExtNimNode) =
  let it0 = mkItNode(0)
  let it = newIdentNode(zfIteratorVariableName)
  proc isIt(node: NimNode): bool =
    node.kind == nnkIdent and ($node == $it0 or $node == $it)
  if ext.initials.findNode(isIt) != nil:
    let listRef = ext.listRef
    let q = quote:
      zfFirstItem(`listRef`)
    ext.initials.replace(it, q, all = true)
    ext.initials.replace(it0, q, all = true)

## Replace an inner `init` section and move it down to its for loop.
proc checkInnerIt(ext: ExtNimNode) =
  if ext.initials.hasIt():
    let itX = mkItNode(ext.itIndex-1)
    let it = newIdentNode(zfIteratorVariableName)
    ext.initials.replace(it, itX, all = true)

## Replace variable definitions with quoted variables, return let section for ident definitions
proc replaceVarDefs(a: NimNode, quotedVars: var OrderedTable[string, string],
    preSection = false): NimNode =
  result = newStmtList()
  let isPre = preSection or (a.kind == nnkCall and a[0].label == "pre")
  for b in a:
    if (a.kind == nnkVarSection or a.kind == nnkLetSection) and b.kind == nnkIdentDefs:
      let label = b[0].label
      if label != "" and label != zfIndexVariableName and label != zfIteratorVariableName:
        if not isPre and not (label in quotedVars):
          let labelNode = newIdentNode(label)
          if (b[1].kind == nnkIdent): # symbol name was explicitly given
            let s = b[1]
            result.add quote do:
              let `labelNode` = `s`
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

## Add all symbol and variable definitions saved in symDefs to the letSection once
## in the order they appear in the given code node.
proc addDefinitions(node: NimNode, symDefs: var OrderedTable[string, NimNode],
    letSection: NimNode) =
  if (node.kind == nnkIdent or node.kind == nnkSym):
    let symDef = symDefs.getOrDefault($node, nil)
    if symDef != nil:
      letSection.add(symDef)
      # only added once
      symDefs.del(node.label)

  if symDefs.len > 0:
    for child in node:
      let isAssignment = child.kind == nnkExprEqExpr or child.kind ==
          nnkIdentDefs or child.kind == nnkAsgn

      if isAssignment:
        # right side goes first! This also ensures the next iterator (on the left side)
        # to be created after the previous (on the right side)
        child[child.len-1].addDefinitions(symDefs, letSection)
        child[0].addDefinitions(symDefs, letSection)
      else:
        child.addDefinitions(symDefs, letSection)

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
  ext.node = newStmtList()

macro zfParamChk(funNameExport: untyped, sym: untyped, symName: untyped,
    paramType: typedesc): untyped =
# parameter type was explicitly given: check that the parameter is of the given type
# check is done in compile-time macro code only -
# but has to be done in the location where the content of the parameter is actually known: inside the loop!
  result = quote:
    static:
      when not ((`sym`) is `paramType`):
        zfFail("Function '$1': param '$2', expected type '$3'!" % [
            `funNameExport`, `symName`, $`paramType`])

## Parse the given Zero-DSL and create an inlineXyz function.
## E.g. `zfInline index(): ...` will create `proc inlineIndex(ext: ExtNimNode)`.
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
    let isCall = funName.endsWith(callSuffix)
    # when this function has been called with zfInlineCall the callSuffix has to be stripped for the actual zero function name
    if isCall:
      funNameExport = funName[0..funName.len-callSuffix.len-1]
      if not (funNameExport in zfFunctionNames):
        addFunction(funNameExport) # only add it once
    else:
      addFunction(funNameExport)

    let procName = newIdentNode("inline" & funName.capitalizeAscii())
    # parameters given to the zero function
    let paramSection = newStmtList()
    # referenced variables are added here
    # this contains common definitions for all sections
    let letSection = newStmtList()
    # reference to the 'ext' parameter of the created proc
    idents(ext)
    var quotedVars = initOrderedTable[string, string]()
    letSection.add(body.replaceVarDefs(quotedVars))

    let numArgs = funDef.len-1
    let hasPre = body[0].label == "pre"
    let firstSym = if funDef.len > 1: funDef[1].label else: ""
    let chk =
      if firstSym == "_":
        newStmtList()
      else:
        quote:
          if `numArgs` < `ext`.node.len-1:
            zfFail("too many arguments in '$1', got $2 but expected only $3" % [
                `ext`.node.repr, $(`ext`.node.len-1), $`numArgs`])
    if not hasPre:
      paramSection.add(chk)

    # save type of parameters when explicitly given
    var paramTypes = initOrderedTable[string, NimNode]()
    # save all symbol / variable defintitions for later insertion
    # these are saved especially for automatic variables such as `it`
    var symDefs = initOrderedTable[string, NimNode]()

    # check the function parameter types
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
      symDefs[symName] = quote:
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
                  zfFail("missing argument '$1' for '$2'" % [`symName`,
                      `funNameExport`])
              newIntLitNode(0)

    let hasResult = body.findIdent("result") != nil
    if hasResult:
      idents(res("resultIdent"))
      letSection.add quote do:
        let `res` = newIdentNode("result")
      quotedVars["result"] = "resultIdent"
    if hasResult or body.findNode(nnkReturnStmt) != nil:
      letSection.add quote do:
        if not `ext`.isLastItem:
          zfFail("'$1' has a result and must be last item in chain!" %
              `funNameExport`)
    else:
      # register iterator as sequence handler
      zfAddSequenceHandlers(funNameExport)

    # check if idx is used but not defined in the body section
    if (body.hasIt() and
        body.findDefinition(zfIndexVariableName) == nil):
      idents(idxIdent)
      letSection.add quote do:
        # add a definition for idx
        let `idxIdent` = newIdentNode(`zfIndexVariableName`)
        discard(`idxIdent`)
        `ext`.needsIndex = true
      quotedVars[zfIndexVariableName] = "idxIdent"

    if not (hasPre or body[0].label == "delegate") or
        not (body.len == 2 and body[1].label == "delegate"):
      # replace it in 'it = ...' with `nextIt` and create the next iterator
      idents(nextIt("nextIdent"), prevIt("prevIdent"))
      discard body.replaceIt(nnkAccQuoted.newTree(nextIt), nnkAccQuoted.newTree(prevIt))
      symDefs["prevIdent"] = quote:
        let `prevIt` = `ext`.prevItNode()
      symDefs["nextIdent"] = quote:
        let `nextIt` = `ext`.nextItNode()

    # add all definitions in the order they are used
    body.addDefinitions(symDefs, paramSection)

    # create the proc
    let q = quote:
      proc `procName`(`ext`: ExtNimNode) {.compileTime.} =
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
        let (symName, paramType) = it
        let sym = nnkAccQuoted.newTree(newIdentNode(symName))
        code.add quote do:
          if `nodeIdx` == 0: # first check: wrap the original loop code and add the below check statements
            `ext`.node = newStmtList(`ext`.node)
          if `ext`.nodeIndex > `delegateUntil`:
            let paramCheck = quote:
              zfParamChk(`funNameExport`, `sym`, `symName`, `paramType`)
            `ext`.node.insert(`nodeIdx`, paramCheck)
        nodeIdx += 1
    # special syntax allowing yield it
    let path = q.findNodeParents(nnkYieldStmt)
    if path.len > 1:
      path[1].replace(path[0], nil)
    result = q
  else:
    if header.kind != nnkCall:
      zfFail("did not expect " & $header.kind & " in " & header.repr)
    if body.kind != nnkStmtList:
      zfFail("did not expect " & $body.kind & " in body of " & header.repr)

# alternative syntax still possible
macro zero*(a: untyped): untyped =
  let body = newStmtList()
  for i in 1..<a.len:
    body.add(a[i])
  result = zeroParse(a[0], body)

## Initate the Zero-DSL definition of an inline function.
## The macro expects the function name, its parameters (in brackets) and the body to implement in different sections.
## See zeroParse.
macro zfInline*(header: untyped, body: untyped): untyped =
  result = zeroParse(header, body)

## Helper that prints the created inline function.
## Useful when Zero-DSL cannot be used for the whole implementation of an inline function.
macro zfInlineDbg*(header: untyped, body: untyped): untyped =
  result = zeroParse(header, body)
  printCode(result)


## calls `zfInline` registering the function call and calls the actual function.
## This can be used to add own implementations of inline-functions with parts in Zero-DSL.
macro zfInlineCall*(header: untyped, body: untyped, dbg: static[bool] = false): untyped =
  doAssert(header.kind == nnkCall or header.kind == nnkObjConstr)
  header[0] = newIdentNode(header.label & callSuffix)
  let fun = newIdentNode("inline" & header.label.capitalizeAscii())
  idents(ext)
  let q = zeroParse(header, body)
  if dbg:
    printCode(q)
  result = quote:
    `q`
    `fun`(`ext`)

# debug version that prints the generated code
macro zfInlineCallDbg*(header: untyped, body: untyped): untyped =
  result = quote:
    zfInlineCall(`header`, `body`, true)

## creates the result tuple of an `indexed` command with index first, then the actual element.
macro mkIndexedResult(idxVar: untyped, elemVar: untyped): untyped =
  idents(elemName(zfIndexedElemName), idxName(zfIndexedIndexName))
  quote:
    (`idxName`: `idxVar`, `elemName`: `elemVar`)

macro mkAccuResult(accuVar: untyped, elemVar: untyped): untyped =
  idents(accuName(zfAccuName), elemName(zfIndexedElemName))
  quote:
    (`accuName`: `accuVar`, `elemName`: `elemVar`)

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
      # - but the unpacking is done manually to also allow assigning arrays or sequences
      for idx, varName in v[0]:
        # let ((a,b) = c) is translated to
        # let a = c[0]
        # let b = c[1]
        let vt = newIdentDefs(varName, newEmptyNode(), nnkBracketExpr.newTree(v[
            1]).add(newIntLitNode(idx)))
        ext.node.add(vt)
    else:
      # just the "normal" definition (a = b)
      ext.node.add(newIdentDefs(v[0], newEmptyNode(), v[1]))
    
    if not isInternal: # leave out definitions that access it or idx directly
      # set next iterator
      let f = v[0] # set 'it' to the previously assigned value / 'it' might also be consequently used
      ext.node = newStmtList(ext.node).add quote do:
        let it = `f`
        discard(it) # iterator might not be used
  
  # check for recursive arrow in the map: if used assign another iterator - prevents capturing error of outer iterator
  elif kind == nnkInfix and (ext.node[1][0].label == zfArrow or ext.node[1][0].label == zfArrowDbg):
    zfInlineCall map(f):
      loop:
        let it = it
        let it = f

  elif not ext.node.hasIt():
    # map was probably just called with the function name: auto add it
    zfInlineCall map(f):
      loop:
        let it =
          when compiles(f(it)):
            f(it)
          else:
            f 
  else:
    zfInlineCall map(f):
      loop:
        let it = f

zfInline indexedMap(f):
  loop:
    let it = mkIndexedResult(idx, f)

zfInline enumerate():
  loop:
    let it = mkIndexedResult(idx, it)

## Implementation of the 'filter' command.
## The trailing commands execution depend on the filter condition to be true.
zfInline filter(cond: bool):
  loop:
    if cond:
      yield it

## Return the first item of an iterable
proc zfFirstItem*(iter: Iterable): auto =
  for it in iter:
    return it

## Implementation of `uniq` command.
## All elements are processed that are not the same element as their preceeding element (or the first element).
zfInline uniq():
  init:
    var prev: type(it)
    var initialized = false
  loop:
    if not initialized or prev != it:
      initialized = true
      prev = it
      yield it

## Implementation of `partition` command.
## Applies each element to the discriminator function and sorts the elements a tuple with to sequences.
## The named tuple element `yes` contains all the elements matching the filter, `no` contains the rest.
zfInline partition(discriminator: bool):
  init:
    var initialized = false
  loop:
    if not initialized:
      result = (yes: newSeq[type(it)](),
                no: newSeq[type(it)]())
      initialized = true
    if discriminator:
      result.yes.add(it)
    else:
      result.no.add(it)

## Implementation of the `group` command.
## Applies each element to the discriminator and adds the result to a table as key adding the elements to a sequence
## for each key.
zfInline group(discriminator):
  init:
    var initialized = false
  loop:
    if not initialized:
      result = initOrderedTable[type(discriminator), seq[type(it)]]()
      initialized = true
    result.mgetOrPut(discriminator, @[]).add(it)

## Implementation of the 'flatten' command.
## E.g. @[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6]
zfInline flatten():
  pre:
    # `idx` has to be re-defined for the new collection
    # make sure `idx` is really named `idx` and not to an auto variable
    idents(idx(zfIndexVariableName))
  init:
    var idxFlatten = -1
  loop:
    for flattened in it:
      let it = flattened
      idxFlatten += 1
      let `idx` = idxFlatten
      discard(`idx`)
      yield it

zfInline indexedFlatten():
  pre:
    # same as in flatten
    idents(idx(zfIndexVariableName))
  init:
    var idxFlatten = -1
  loop:
    var idxInner = -1
    for flattened in it:
      idxInner += 1
      idxFlatten += 1
      let it = mkIndexedResult(idxInner, flattened)
      let `idx` = idxFlatten # overwrite the idx variable (if present)
      discard(`idx`)
      yield it

## Implementation of the `takeWhile` command.
## `takeWhile(cond)` : Take all elements as long as the given condition is true.
zfInline takeWhile(cond: bool):
  loop:
    if cond:
      yield it
    else:
      break

## Implementation of the `take` command.
## `take(count)` : Take `count` elements.
zfInline take(count: int):
  init:
    var idxTake = -1
  delegate:
    takeWhile:
      idxTake += 1
      idxTake < count

## Implementation of the `dropWhile` command.
## `dropWhile(cond)` : drop elements as long the given condition is true.
## Once the condition gets false, all following elements are used.
zfInline dropWhile(cond: bool):
  init:
    var gate = false
  loop:
    if gate or not cond:
      gate = true
      yield it

## Implementation of the `drop` command.
## `drop(count)` : drop (or discard) the next `count` elements.
zfInline drop(count: int):
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

    zfInlineCall sub(minIndex: int, endIndex: int):
      init:
        var idxSub = -1
      loop:
        idxSub += 1
        if idxSub >= minIndex:
          if idxSub > endIndex:
            break
          else:
            yield it

## Implementation of the 'exists' command.
## Searches the input for a given expression. If one is found "true" is returned, else "false".
zfInline exists(search: bool):
  loop:
    if search:
      return true

## Implementation of the 'find' command.
## Searches the input for a given expression. Returns an option value.
zfInline find(cond: bool):
  loop:
    if cond:
      return some(it)

## Implementation of the 'all' command.
## Returns true of the given condition is true for all elements of the input, else false.
zfInline all(test: bool):
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
  let hasIterator = isEq and ext.node[1][0].hasIt()
  var adaptedExpression = ext.adapt()

  # special case: assignment to iterator -> try to assign to outer list (if possible)
  if hasIterator:
    if ext.itIndex > 1:
      zfFail("Adapted list cannot be changed in-place!")
    # this only works if the current list has not (yet) been manipulated
    var itNode = adaptedExpression.findParentWithChildLabeled(
        ext.prevItNode.label)
    if itNode != nil:
      let listRef = ext.listRef
      idents(index(zfIndexVariableName))
      let rightSide = adaptedExpression.last
      # changing the iterator content will only work with indexable + variable containers
      if ext.isListType():
        idents(itList(zfListIteratorName))
        adaptedExpression = quote:
          `itList`.value = `rightSide`
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
  ext.node = newStmtList().add quote do:
    `adaptedExpression`
  
## Implementation of the 'index' command.
## Returns the index of the element in the input list when the given expression was found or -1 if not found.
zfInline index(cond: bool):
  init:
    result = -1 # index not found
  loop:
    if cond:
      return idx

## Implementation of the 'fold' command.
## Initially the result is set to initial value given by the user, then each element is added
## to the result by subsequent calls.
when useInternalAccu:
  zfInline fold(initialValue, _):
    pre:
      let foldOperation = ext.adapt(2, inFold = true) # special adapt for fold
      ext.node.del(2) # prevent error message: too many parameters
      let accuIdent = newIdentNode(internalAccuName)
    init:
      var accuIdent = initialValue # implicitly uses internalAccuName
    loop:
      accuIdent = foldOperation
    final:
      result = accuIdent
else:
  zfInline fold(initialValue, foldOperation):
    init:
      result = initialValue
    loop:
      result = foldOperation

## Implementation of the 'reduce' command.
## Initially the result is set to the first element of the list, then each element is added
## to the result by subsequent calls.
proc inlineReduce(ext: ExtNimNode) {.compileTime.} =
  let reduceCmd = ext.label.toReduceCommand()
  if reduceCmd.isSome():
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

  if ext.label.startsWith("indexed"):
    # indexed implementation
    zfInlineCall reduce(op):
      init:
        var initAccu = true
      loop:
        if initAccu:
          result = mkIndexedResult(idx, it)
          initAccu = false
        else:
          let oldValue = result.elem
          # assign the new iterator to the old value as accumulator 
          # before calling the operation `op`
          let it = mkAccuResult(oldValue, it)
          let newValue = op
          if not (oldValue == newValue):
            result = mkIndexedResult(idx, newValue) # propagate new value with idx
      endLoop:
        if initAccu:
          result.idx = -1 # we actually do not have a result: set index to -1

  else:
    # normal reduce without index
    zfInlineCall reduce(op):
      init:
        var initAccu = true
      loop:
        if initAccu:
          result = it
          initAccu = false
        else:
          let it = mkAccuResult(result, it)
          result = op

proc combineWithOtherCollection(ext: ExtNimNode, indexed: bool) {.compileTime.} =
  idents(idxIdent(zfIndexVariableName))
  var itIdent = ext.prevItNode()
  let iterators = nnkBracket.newTree(itIdent)
  let indices = nnkBracket.newTree(idxIdent)
  var code = newStmtList()
  var root = code

  var idx = 1
  while idx < ext.node.len:
    itIdent = ext.nextItNode()
    iterators.add(itIdent)
    let listRef = ext.node[idx]
    idx += 1
    if indexed:
      var idxInner = genSym(nskVar, "__idxInner__")
      indices.add(idxInner)
      code.add quote do:
        var `idxInner` = -1
        for `itIdent` in `listRef`:
          `idxInner` += 1
          nil
    else:
      code.add quote do:
        for `itIdent` in `listRef`:
          nil
    code = code.getStmtList()

  let nextIt = ext.nextItNode()
  if indexed:
    code.add quote do:
      let `nextIt` = mkIndexedResult(`indices`, `iterators`)
      nil
  else:
    code.add quote do:
      let `nextIt` = `iterators`
      nil
  ext.node = root

## Implementation of the 'combinations' command.
## Each two distinct elements of the input list are combined to one element.
proc inlineCombinations(ext: ExtNimNode) {.compileTime.} =
  if ext.node.len == 1:
    # combine elements in collection with itself
    # but prevent unnecessary unordered combinations
    if ext.isListType():
      zfInlineCall combinations():
        pre:
          let itList = newIdentNode(zfListIteratorName)
        loop:
          var itListInner = itList.next
          while itListInner != nil:
            let it = [it, itListInner.value]
            itListInner = itListInner.next
            nil
    else:
      zfInlineCall combinations():
        pre:
          let listRef = ext.listRef
        loop:
          when not (listRef is FiniteIndexableLenIter):
            static:
              zfFail("Only index with len types supported for combinations")
          for idxInner in idx+1..<listRef.len():
            let it = [listRef[idx], listRef[idxInner]]
            nil
  else:
    ext.combineWithOtherCollection(false)

proc inlineIndexedCombinations(ext: ExtNimNode) {.compileTime.} =
  if ext.node.len == 1:
    # combine elements in collection with itself
    if ext.isListType():
      zfInlineCall indexedCombinations():
        pre:
          let itList = newIdentNode(zfListIteratorName)
        loop:
          var itListInner = itList.next
          var idxInner = idx
          while itListInner != nil:
            let it = mkIndexedResult([idx, idxInner], [it, itListInner.value])
            idxInner += 1
            itListInner = itListInner.next
            nil
    else:
      zfInlineCall indexedCombinations():
        pre:
          let listRef = ext.listRef
        loop:
          when not (listRef is FiniteIndexableLenIter):
            static:
              zfFail("Only index with len types supported for combinations")
          for idxInner in idx+1..<listRef.len():
            let it = mkIndexedResult([idx, idxInner], [listRef[idx], listRef[idxInner]])
            nil
  else: # combine with other collections
    ext.combineWithOtherCollection(true)


macro genTupleSeqCalls(maxTupleSize: static[int]): untyped =
  ## generates the procs initTupleSeq and addToTupleSeq needed for the split command
  #[
  proc initTupleSeq[T1,T2](t: (T1,T2)): (seq[T1],seq[T2]) =
    result = (newSeq[T1](), newSeq[T2]())

  proc addToTupleSeq[T1,T2](ts: var (seq[T1],seq[T2]), t: (T1,T2)) =
    ts[0].add(t[0])
    ts[1].add(t[1])
  ]#
  var types: seq[NimNode] = @[]
  for l in 1..maxTupleSize:
    types.add(newIdentNode("T" & $l))

  result = newStmtList()
  for tupleNum in 2..maxTupleSize:
    let genIdents = nnkIdentDefs.newTree()
    let paramIdents = newPar()
    let params = nnkFormalParams.newTree()
    let params2 = nnkFormalParams.newTree()
    let retVal = newPar()
    let calls = newPar()
    for i in 0..tupleNum-1:
      # Generic param is [T1, T2, ...]
      genIdents.add(types[i])
      # parameter is (T1, T2, ...)
      paramIdents.add(types[i])
      # return value is (seq[T1], seq[T2], ...)
      retVal.add(nnkBracketExpr.newTree(newIdentNode("seq"), types[i]))
      # result = (newSeq[T1](), newSeq[T2](), ...)
      calls.add(newCall(nnkBracketExpr.newTree(newIdentNode("newSeq"), types[i])))
    let tParam = newIdentDefs(newIdentNode("t"), paramIdents, newEmptyNode())
    let tsParam = newIdentDefs(newIdentNode("types"), nnkVarTy.newTree(retVal),
        newEmptyNode())
    params.add(retVal).add(tParam)
    params2.add(newEmptyNode()).add(tsParam).add(tParam)
    genIdents.add(newEmptyNode()).add(newEmptyNode())

    # generate initTupleSeq
    let body = newStmtList(nnkAsgn.newTree(newIdentNode("result"), calls))
    result.add(nnkProcDef.newTree(newIdentNode("initTupleSeq"), newEmptyNode(),
      nnkGenericParams.newTree(genIdents), params, newEmptyNode(), newEmptyNode(), body))

    # generate addToTupleSeq
    let body2 = newStmtList()
    idents(types, t)
    for i in 0..tupleNum-1:
      body2.add quote do:
        `types`[`i`].add(`t`[`i`])
    result.add(nnkProcDef.newTree(newIdentNode("addToTupleSeq"), newEmptyNode(),
      nnkGenericParams.newTree(genIdents), params2, newEmptyNode(),
          newEmptyNode(), body2))
genTupleSeqCalls(zfMaxTupleSize)

## implementation of the `split` command. Splits a sequence of tuples to a tuple of sequences.
zfInline split():
  init:
    var first = true
  loop:
    if first:
      first = false
      # if this fails to compile: increase zfMaxTupleSize!
      result = initTupleSeq(it)
    result.addToTupleSeq(it)

## Implementation of the `count` command. Counts all (filtered) items.
zfInline count():
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
    var itDef = newStmtList()
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
    idents(itList(zfListIteratorName), itNext("_itListNext"))
    ext.node = quote:
      var `itList` = `listRef`.head
      while `itList` != nil:
        let `itIdent` = `itList`.value
        let `itNext` = `itList`.next
        nil
    ext.endLoop.add quote do:
      `itList` = `itNext`

  elif ext.forceIndexLoop:
    # iterate over index
    ext.needsIndex = true
    ext.initials.add quote do:
      var `idxIdent` = low(`listRef`)
    ext.node = quote:
      while (`idxIdent` <= high(`listRef`)):
        let `itIdent` = `listRef`[`idxIdent`]
        nil
    # idx += 1 added in iterHandler

  elif ext.typeDescription.startsWith("Option["):
    # iterate over option
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

macro createExtendDefaults(): untyped =
  # creates proc zfExtendDefaults
  # add inlineForeach + inlineDef manually
  addFunction($Command.foreach)
  let (funDef, _) = createExtensionProc("Defaults", @[])
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
      warning("`any` is deprecated - use exists instead")
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
            ext.node = newStmtList()
          else:
            # call was internally replaced by another call: repeat inlining
            ext.inlineElement()
  else:
    if ext.itIndex > 0:
      error("$1 supposed to be first - index is $2" % [ext.label, $ext.itIndex], ext.node)
    ext.inlineSeq()

type
  ## Helper type to allow `[]` access for SomeLinkedList types
  MkListIndexable[T, U, V] = ref object
    listRef: U
    currentIt: V
    currentIdx: Natural
    length: int

  ## Helper to allow `[]` access for slices
  MkSliceIndexable[T] = ref object
    slice: HSlice[T, T]

proc mkIndexable*[T](items: DoublyLinkedList[T]): MkListIndexable[T,
    DoublyLinkedList[T], DoublyLinkedNode[T]] =
  MkListIndexable[T, DoublyLinkedList[T], DoublyLinkedNode[T]](listRef: items,
      currentIt: items.head, currentIdx: 0, length: -1)
proc mkIndexable*[T](items: SinglyLinkedList[T]): MkListIndexable[T,
    SinglyLinkedList[T], SinglyLinkedNode[T]] =
  MkListIndexable[T, SinglyLinkedList[T], SinglyLinkedNode[T]](listRef: items,
      currentIt: items.head, currentIdx: 0, length: -1)
proc mkIndexable*[T](items: HSlice[T, T]): MkSliceIndexable[T] =
  MkSliceIndexable[T](slice: items)

proc `[]`*[T, U, V] (items: var MkListIndexable[T, U, V], idx: Natural): T =
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
  result = items.length

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
          zfFail("need to provide an own implementation for mkIndexable(" &
              $`a`.type & ")")
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
proc replaceZip(args: NimNode): NimNode {.compileTime.} =
  var idx = 0
  result = newStmtList()

  # zip(a,b,c) <=> a --> zip(b,c) <~> a --> map(a[idx],b[idx],c[idx])
  let highList = nnkBracket.newTree()

  for arg in args:
    # search for all zip calls and replace them with filter --> map
    if arg.kind == nnkCall and arg[0].label == $Command.zip:
      let zipCmd = arg.copyNimTree()
      let params = newPar()
      var namedTuple = true
      var createUniqueNames = false
      var names = initHashSet[string]()
      if idx > 0:
        # left side of original `-->` is actual first argument to zip
        zipCmd.insert(1, args[0])

      for paramIdx in 1..<zipCmd.len:
        let p = zipCmd[paramIdx]
        if not (p.kind == nnkIdent or (p.kind == nnkBracketExpr and p[0].kind == nnkIdent)):
          namedTuple = false
        elif (p.kind == nnkIdent):
          if names.contains(p.label):
            createUniqueNames = true
          names.incl(p.label)
        else:
          if p[0].label in names:
            createUniqueNames = true
          names.incl(p[0].label)

        if p.kind != nnkInfix and p.kind != nnkBracketExpr and p.label != zfIteratorVariableName:
          highList.add(nnkCall.newTree(newIdentNode("high"), p))
          result.add(p.wrapIndexable())
          params.add(nnkBracketExpr.newTree(p, newIdentNode(
              zfIndexVariableName))) # map(it,b[idx],...)
        elif idx > 0:
          params.add(p)
        else:
          zfFail("No complex arguments allowed in 'zip' operation when used as first command - rather use 'a --> zip(it, ...)'.")

      if namedTuple:
        # create a named tuple using the originally zipped items as name elements
        for i in 0..params.len-1:
          let p = params[i]
          let suffix = if createUniqueNames: $i else: ""
          params[i] = nnkExprColonExpr.newTree(newIdentNode(p[0].repr &
              $suffix), p)

      if idx == 0:
        # convert "zip(a,b,c) --> ..." to "a --> zip(b,c) --> ..."
        args.insert(0, zipCmd[1]) # a --> ...
        idx = 1

      args[idx] = newCall($Command.map, params)
    idx += 1
  if highList.len > 0:
    let minHigh = newIdentNode(zfMinHighVariableName)
    result.add quote do:
      let `minHigh` = min(`highList`)

## Gets the result type, depending on the input-result type and the type-description of the input type.
## When the result type was given explicitly by the user that type is used.
## Otherwise the template argument is determined by the input type.
proc getResType(resultType: ResultType, td: string): (NimNode, bool) {.compileTime.} =
  if resultType.id.len == 0:
    return (nil, false)
  var resType = resultType

  let idx = resType.id.find("[")
  if idx != -1:
    result = (parseExpr(resType.id), not resultType.implicit)
  else:
    let res = newIdentNode(resType.id)
    let idx2 = td.find("[")
    var q: NimNode
    if idx2 != -1:
      var tdarg = td[idx2+1..td.len-2]
      let idxComma = tdarg.find(", ")
      let idxBracket = tdarg.find("[")
      if idxComma != -1 and (idxBracket == -1 or idxBracket > idxComma) and
          resType.id != "array":
        # e.g. array[0..2,...] -> seq[...]
        tdarg = tdarg[idxComma+2..tdarg.len-1]
      q = parseExpr(resType.id & "[" & tdarg & "]")
    else:
      q = quote:
        `res`[int] # this is actually a dummy type
    result = (q, false)

proc moveItToInnerLoops(ext: ExtNimNode, node: NimNode): NimNode =
  result = node.copyNimTree()
  let initials = result[1]
  let loopSection = result[2]

  for i in 0..<ext.maxIndex:
    let it = mkItNode(i)
    for p in initials.findNodeParents(nnkIdent, it.label):
      if p.kind == nnkStmtList:
        for l in loopSection.findNodeParents(nnkIdent, it.label):
          if l.kind == nnkStmtList:
            let ins = initials.copyNimTree()
            if l[0].kind == nnkForStmt or l[0].kind == nnkWhileStmt:
              l[0][2].insert(0, ins)
            else:
              l.insert(1, ins)
            initials.del(0, initials.len)
            break
        break

proc iterResultType(ext: ExtNimNode, idx = -1): NimNode =
  let iterType = newIdentNode(zfInternalHelperProc)
  let i = if idx == -1: ext.maxIndex-1 else: idx
  if ext.maxIndex > 1:
    return quote:
      `iterType`()[`i`]
  let listRef = ext.listRef
  return quote:
    zfFirstItem(`listRef`)

proc adaptItDefs(ext: ExtNimNode, node: NimNode) =
  let initials = node[1]

  for i in 0..<ext.maxIndex:
    let it = mkItNode(i)
    initials.replace(it, ext.iterResultType(i), all = true)


## Creates the function that returns the final result of all combined commands.
## The result type depends on map, zip or flatten calls. It may be set by the user explicitly using to(...)
proc createAutoProc(ext: ExtNimNode, args: NimNode, isSeq: bool,
    resultType: ResultType, td: string, loopDef: NimNode, forceSeq: bool,
    isIter: bool): NimNode =
  var (resType, explicitType) = getResType(resultType, td)
  var collType = if resType != nil: resType.repr else: td

  var hasIter = false
  var itFun: NimNode = nil
  var itDef: NimNode = nil

  if isSeq and not isIter and (resType != nil or forceSeq):
    if not explicitType and (collType.find("[") != -1 or forceSeq):
      hasIter = true
      let procName = newIdentNode(zfInternalHelperProc)
      let fun = ext.moveItToInnerLoops(loopDef)
      ext.adaptItDefs(loopDef)
      if ext.maxIndex > 1:
        itFun = quote:
          {.push hint[XDeclaredButNotUsed]: off.}
          proc `procName`(): auto =
            `fun`
          {.pop.}
      itDef = ext.iterResultType()

  var autoProc = quote:
    (proc(): auto =
      nil)
  var code: NimNode = nil

  # set a default result in case the resType is not nil - this result will be used
  # if there is no explicit map, zip or flatten operation called
  if resType != nil:
    code = quote:
      var res: `resType`
      result = zfInit(res)

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
    let isTuple = collType.startsWith("(") and collType.endsWith(")")
    if not isTuple and (i != -1 and hasIter and (not forceSeq or
        resultType.id.len > 0)):
      collType = collType[0..i-1]
      let collSym = parseExpr(collType)
      let resDef =
        if collType == "array":
          quote:
            array[`listRef`.len, type(`itDef`)]
        else:
          quote:
            `collSym`[type(`itDef`)]
      code = quote:
        var res: `resDef`
        result = zfInit(res)

    elif isTuple:
      let num = collType.split(",").len()
      ext.needsIndex = true
      if num < 2 or num > zfMaxTupleSize:
        zfFail("Tuple return types are only supported from 2 up to $1 elements" %
            [$zfMaxTupleSize])
      let x = genSym(nskVar, "x")
      let tup = newPar()
      for _ in 1..num:
        tup.add(x)
      code = quote:
        var `x`: type(`itDef`)
        result = `tup`

    elif forceSeq and hasIter:
      let coll = newIdentNode(defaultCollectionType)
      code = quote:
        var res: `coll`[type(`itDef`)]
        result = zfInit(res)

    else:
      # use the same type as in the original list
      code = newStmtList().add quote do:
        result = zfInit(`listRef`)

  if isSeq:
    # unfortunatly it does not (yet) work in Nim to define iterators anywhere in Nim and get a result from it
    # so defining an iterator inside an anonymous proc (this generated one) - inside another proc - yields no results
    # so we copy everything - this is also a tad more efficient for the running code in the end...
    var addLoop = loopDef.findNode(nnkStmtList)
    if hasIter:
      addLoop = addLoop.copyNimTree()
    # there should only be one yield statement - replace it with zfAddItem
    let path = addLoop.findNodeParents(nnkYieldStmt)
    let itName = path[0][0]
    path[1].replace(path[0], ext.addElemResult(itName))
    code.add(addLoop)

    # in the proc replace the yield with result - returning all iterators in a tuple
    let path2 = itFun.findNodeParents(nnkYieldStmt)
    let res = newPar()
    for i in 0..<ext.maxIndex:
      res.add(mkItNode(i))
    if path2.len > 1:
      # replace yield expression in proc with a result expression
      # the created proc is used later on only to determine the result type - not to actually call the proc
      let setResult = quote:
        result = `res`
      # within parent replace yield with result = it
      path2[1].replace(path2[0], setResult)

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
proc checkTo(args: NimNode, td: string): ResultType {.compileTime.} =
  let last = args.last
  let hasTo = last.kind == nnkCall and last[0].repr == $Command.to
  let autoConvert = hasTo and last.len == 3 and (last[2].label == "true" or (
      last[2].kind == nnkExprEqExpr and last[2][1].label == "true"))
  result = ResultType(id: "", implicit: false, autoConvert: autoConvert)
  if hasTo:
    args.del(args.len-1) # remove the "to" node
    result.id = last[1].repr
    if args.len <= 1:
      # there is no argument other than "to": add default mapping function "map(it)"
      args.add(parseExpr($Command.map & "(" & zfIteratorVariableName & ")"))
    else:
      if (not (args.last[0].label in sequenceHandlers)) and result.id != "iter":
        zfFail("'to' can only be used with list results - last arg is '" &
            args.last[0].label & "'")
    if result.id == "list": # list as a shortcut for DoublyLinkedList
      result.id = "DoublyLinkedList"
      result.implicit = true
    elif result.id.startsWith("list["):
      result.id = "DoublyLinkedList" & result.id[4..^1]
    elif result.id == "set": # set as a shortcut for HashSet
      result.id = "HashSet"
      result.implicit = true
    elif result.id.startswith("set["):
      result.id = "HashSet" & result.id[3..result.id.len-1]
    elif result.id == "seq":
      result.id = "seq[int]"
      result.implicit = true
  if result.id.len == 0:
    for arg in args:
      if arg.kind == nnkCall:
        let label = arg[0].repr
        # shortcut handling for mapSeq(...)  <=> map(...).to(seq) and
        #                       mapList(...) <=> map(...).to(list) - etc.
        let isSeq = label.endsWith("Seq")
        let isList = label.endsWith("List")
        # Check forced sequences or lists
        if isSeq or isList:
          if isSeq:
            arg[0] = newIdentNode(label[0..label.len-4])
          elif isList:
            arg[0] = newIdentNode(label[0..label.len-5])
          if isSeq or isList or result.id.len == 0:
            if isSeq:
              result.id = "seq"
            elif isList:
              result.id = "DoublyLinkedList"
            elif (td.startsWith("DoublyLinkedList")):
              result.id = td
              result.implicit = true
            else:
              result.id = defaultResultType # default to sequence - and use it if isSeq is used explicitly
              result.implicit = true
  if result.id.len == 0 and td == "enum":
    result.id = "seq[" & $args[0] & "]"
    result.implicit = true

proc replaceSimpleMap(args: NimNode) {.compileTime.} =
  for i in 0..args.len-1:
    if args[i].kind == nnkPar:
      var allIds = true
      # special case: (id) or (id1,id2) - this is a shortcut for
      # map(id = it) or map((id1,id2) = it)
      if args[i].len == 1 and args[i][0].kind == nnkIdent:
        # for a single parameter remove the outer brackets
        args[i] = args[i][0]
      for child in args[i]:
        if child.kind != nnkIdent:
          allIds = false
          break
      if allIds:
        args[i] = nnkCall.newTree(newIdentNode("map"), nnkExprEqExpr.newTree(
            args[i], newIdentNode(zfIteratorVariableName)))

## Main function that creates the outer function call.
proc iterHandler(args: NimNode, td: string, debugInfo: string): NimNode {.compileTime.} =
  args.replaceSimpleMap()
  let debug = debugInfo.len() > 0
  let orig = if debug: args.copyNimTree() else: nil
  var resultType = args.checkTo(td)
  let preInit = args.replaceZip() # zip is replaced with map + filter
  let hasMinHigh = preInit.len > 0
  let lastCall = if args.last.kind == nnkIdent: args.last.label else: args.last[0].label
  let toIter = resultType.id == "iter" or (resultType.id.len == 0 and defined(zf_iter))
  if toIter and defined(js):
    resultType.id = ""
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
            warning("Closure not supported by JS backend - argument true of `createIter` will be ignored.")
        elif closureVal.label != "false":
          zfFail("Unsupported parameter value '$1' to 'createIter'" % [
              closureVal.label])
      iterName = newIdentNode(args.last[1].label)
      args.del(args.len-1)
    else:
      iterName = newIdentNode(zfInternalIteratorName)
    if args.last.len > 0 and not (args.last[0].label in sequenceHandlers):
      zfFail("'iter' can only be used with list results - last arg is '" &
          args.last[0].label & "'")

    if isClosure:
      # create closure iterator that delegates to the inline iterator
      let inlineName = newIdentNode(iterName.label & "Inline")
      let it = newIdentNode(zfIteratorVariableName)
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

  let isSeq = lastCall in sequenceHandlers

  var needsIndexVar = false

  if ((not isIter and (isSeq and (resultType.id.len > 0 and
      resultType.id.startsWith("array") or (resultType.id.len == 0 and
          td.startsWith("array"))))) or
      args.hasIt()):
    needsIndexVar = true

  let init = newStmtList()
  let initials = newStmtList()
  let varDef = newStmtList()
  init.add(varDef)
  init.add(initials)

  var codeStart = newStmtList()
  var code = codeStart

  var index = 1
  let listRef = args[0]
  let finals = newStmtList()
  let endLoop = newStmtList()

  var argIdx = 1
  var ext: ExtNimNode
  var forceIndexLoop = false
  var delegateUntil = -1
  var maxIndex = 0

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
                    maxIndex: maxIndex,
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
    ext.checkInnerIt()
    ext.revertIt()

    forceIndexLoop = forceIndexLoop or ext.forceIndexLoop
    delegateUntil = ext.delegateUntil
    index = ext.itIndex
    let newCode = ext.node.getStmtList()

    if argIdxReal == 0:
      # the actual outer loop is created last - so insert the functions here
      newCode.add(codeStart)
      # and set the current outer loop as new codeStart
      codeStart = newStmtList(ext.node)
      index = oldIndex
    else:
      code.add(ext.node)

    # make sure the collection is created
    if (argIdx == args.len) and not ext.elemAdded and arg.label in sequenceHandlers:
      var node = newCode
      if node == nil:
        # directly append the adding function
        node = code
      node.add(ext.addElem(mkItNode(ext.itIndex-1)))

    if newCode != nil:
      code = newCode

    maxIndex = ext.maxIndex

  # end of loop

  let idxIdent = newIdentNode(zfIndexVariableName)

  # add the actual loop section
  init.add(codeStart)

  # add the finals section
  if finals.len > 0:
    init.add(finals)

  let needsFunction = (lastCall != $Command.foreach)
  if needsFunction:
    var theProc: NimNode = nil
    if isIter:
      if toIter: # used with to(iter)
        theProc = quote:
          (proc(): auto =
            `iterNode`
            result = `iterName`)
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
      code.insert(0, init)

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

  # add the endLoop section
  # could be combinations of for and while, but only one while (for list types)
  var loopPath: seq[NimNode] = @[]
  var loopNode: NimNode = nil
  var kind = nnkForStmt

  if (td.isListType() or ext.forceIndexLoop):
    kind = nnkWhileStmt
  loopPath = result.findNodeParents(kind)
  if loopPath.len > 0:
    loopNode = loopPath[0]

  if loopNode != nil: # and not hasMinHigh:
    if endLoop.len > 0:
      loopNode.last.add(endLoop)

    # add `var idx = 0` and `idx += 1` - in case of minHigh idx is already looped through
    if not hasMinHigh:
      let path = result.findNodeParents(nnkIdent, zfIndexVariableName)
      if path.len > 0:
        # add index increment to end of the for loop
        loopNode.last.add quote do:
          `idxIdent` += 1
        # add idx definition if it is not already present
        if path.len > 1 and path[2].kind != nnkVarSection:
          proc addIdx(path: seq[NimNode]): bool =
            result = path.len > 1
            if result:
              let q = quote:
                var `idxIdent` = 0
              path[1].insert(path[1].len - 1, q)

          if loopPath.addIdx():
            let procDefNode = result.findNode(nnkProcDef).findNode(nnkProcDef)
            if procDefNode != nil:
              discard procDefNode.findNodeParents(kind).addIdx()

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

    printCode(result)
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

macro connectCall(td: typedesc, callInfo: untyped, args: varargs[
    untyped]): untyped =
  result = iterHandler(args, getTypeInfo(td.getType[1], td.getTypeInst[1]),
      callInfo.repr[1..^2])

## Preparse the call to the iterFunction.
proc delegateMacro(a: NimNode, b1: NimNode, td: string,
    debugInfo: string): NimNode =
  var b = b1
  # we expect b to be a call, but if we have another node - e.g. infix or bracketexpr - then
  # we search for the actual call, do the macro expansions on the call and
  # add the result back into the tree later
  var outer = b
  var path: seq[NimNode] = @[]
  if b.kind != nnkCall:
    path = outer.findNodeParents(nnkCall)
    if path.len > 0:
      b = path[0]
    else:
      if outer.kind == nnkIdent:
        # shortcut syntax for iterator definition - handle later
        return a
      else:
        zfFail("Unexpected expression in macro call on right side of '-->'")

  # now re-arrange all dot expressions to one big parameter call
  # i.e. a --> filter(it > 0).map($it) gets a.connect(filter(it>0),map($it))
  # SinglyLinkedList iterates over items in reverse order they have been prepended
  var m = initSinglyLinkedList[NimNode]()
  # b contains the calls in a tree - the first calls are deeper in the tree
  # this has to be flattened out as argument list
  var node = b
  while node.kind == nnkCall:
    if node[0].kind == nnkDotExpr:
      if node[0][0].kind == nnkPar:
        node[0].replaceSimpleMap()
      else:
        # could have something like foo.bar --> ...
        m.prepend(nnkCall.newTree(node[0].last))
        for z in 1..<node.len:
          m.head.value.add(node[z])
        node = node[0][0] # go down in the tree
    else:
      m.prepend(node)
      break

  let args = nnkArglist.newTree()
  # re-arrange shortcut expression of (a --> itName) to alternative shortcut a --> (itName)
  if a.kind == nnkPar and a[0].kind == nnkInfix and a[0][0].label.startsWith(zfArrow):
    args.add(nnkArglist.newTree(a[0][1])).add(newPar(a[0][2]))
  else:
    args.add(a)

  for it in m:
    args.add(it)
  result = iterHandler(args, td, debugInfo)

  if path.len > 1: # insert the result back into the original tree
    path[1].replace(path[0], result)
    result = path[^1] # upper most parent

## delegate call to get the type information.
macro delegateArrow(td: typedesc, a: untyped, b: untyped,
    debugInfo: untyped): untyped =
  result = delegateMacro(a, b, getTypeInfo(td.getType[1], td.getTypeInst[1]),
      debugInfo.repr[1..^2])

proc replArrow(n: NimNode, arrow: string): NimNode =
  if n.kind == nnkInfix and n[0].label == arrow:
    result = nnkDotExpr.newTree(n[1], n[2])
  else:
    result = n
    if n.kind != nnkCall:
      for i in 0..n.len-1:
        n[i] = n[i].replArrow(arrow)

proc getOp(a: NimNode): string =
  if a.kind == nnkInfix:
    result = a[0].repr
  else:
    result = ""

proc checkArrow(a: NimNode, b: NimNode, debug: bool = false): (NimNode, NimNode, bool) =
  result = (a, b, debug)
  let op = a.getOp()
  if op == zfArrow or op == zfArrowDbg:
    # as is infix(-->, left_of_arrow, right_of_arrow)
    # check recursively on left side
    let (left, right) = (a[1], a[2])
    # pack the right side to the other tree
    # also replace the arrows with "."
    let shiftedExpr = nnkDotExpr.newTree(right, b.replArrow(op))
    # ensure to use the left-most arrow
    let opLeft = left.getOp()
    if opLeft == zfArrow or opLeft == zfArrowDbg:
      return checkArrow(left, shiftedExpr, opLeft == zfArrowDbg)
    # it looks inefficient to call `parseExpr` with ` repr` here - but the tree needs to be re-arranged
    # i.e. all dot-expressions containing other expressions on the right side need to be handled
    # since `.` is stronger than other operators - and `.` replaced `-->` before.
    # An explicit implementation is not faster than this one.
    return (left, parseExpr(shiftedExpr.repr), op == zfArrowDbg)

## Alternative call with comma separated arguments.
macro connect*(args: varargs[untyped]): untyped =
  result = quote:
    connectCall(type(`args`[0]), `args`)

macro zfunCall(td: typedesc, debug: static[bool], a: untyped,
    b: untyped): untyped =
  let b2 = newStmtList(a)
  for c in b:
    if c.kind == nnkCall and c.len == 2 and c[1].kind == nnkStmtList:
      for callParam in c[1]:
        b2.add(newCall(c[0], callParam))
    else:
      b2.add(c)
  result = iterHandler(b2, getTypeInfo(td.getType[1], td.getTypeInst[1]),
      b.dbgLineInfo(debug or debugAll))

macro zfun*(a: untyped, b: untyped): untyped =
  result = quote:
    zfunCall(type(`a`), false, `a`, `b`)

macro zfun*(a: untyped, b: untyped, c: untyped): untyped =
  c.insert(0, newPar(b))
  result = quote:
    zfunCall(type(`a`), false, `a`, `c`)

macro zfunDbg*(a: untyped, b: untyped): untyped =
  result = quote:
    zfunCall(type(`a`), true, `a`, `b`)

macro zfunDbg*(a: untyped, b: untyped, c: untyped): untyped =
  c.insert(0, newPar(b))
  result = quote:
    zfunCall(type(`a`), true, `a`, `c`)

macro zfConcat*(name: untyped, iterables: varargs[untyped]): untyped =
  result = quote:
    iterator `name`(): auto {.inline.} =
      nil
  let params = result.findNode(nnkFormalParams)
  let code = result.getStmtList()
  var idx = 0
  for p in iterables:
    let paramName = genSym(nskParam, "p" & $idx)
    params.add(newIdentDefs(paramName, newCall("type", p), p))
    code.add quote do:
      for it in `paramName`:
        yield it
    idx += 1

macro callConcat(a: untyped, b: untyped, dbg: untyped): untyped =
  # create an iterator over all supplied items and call that
  let concatIter = genSym(nskIterator, "concat")
  let concatCall = quote:
    zfConcat(`concatIter`)
  let a2 = quote:
    `concatIter`()
  for call in [concatCall, a2]:
    for idx in 1..a.len-1:
      call.add(a[idx])
  result = quote:
    `concatCall`
    delegateArrow(type(`a2`), `a2`, `b`, `dbg`)

macro arrowCall(a: untyped, b: untyped, dbg: untyped): untyped =
  case a.label:
    of $Command.concat:
      result = quote:
        callConcat(`a`, `b`, `dbg`)
    of $Command.zip:
      result = delegateMacro(a, b, defaultCollectionType, $dbg)
    else:
      result = quote:
        delegateArrow(type(`a`), `a`, `b`, `dbg`)

## general macro to invoke all available zero_functional functions
macro `-->`*(a: untyped, b: untyped): untyped =
  let (a, b2, debug) = checkArrow(a, b)
  let dbg = b.dbgLineInfo(debug or debugAll)
  result = quote:
    arrowCall(`a`, `b2`, `dbg`)

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  let (a, b2, _) = checkArrow(a, b)
  let dbg = b.dbgLineInfo(true)
  result = quote:
    arrowCall(`a`, `b2`, `dbg`)

