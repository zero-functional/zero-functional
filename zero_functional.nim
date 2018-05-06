import macros, options, sets, lists, typetraits, strutils, tables

const zfIteratorVariableName* = "it"
const zfAccuVariableName* = "a"
const zfCombinationsId* = "c"
const zfIndexVariableName* = "idx"

const internalIteratorName = "__" & zfIteratorVariableName & "__"
const useInternalAccu = zfAccuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & zfAccuVariableName & "__" else: "result"
const implicitTypeSuffix = "?" # used when result type is automatically determined
const defaultResultType = "seq[int]" & implicitTypeSuffix
const listIteratorName = "__itlist__"
const minHighVariableName = "__minHigh__"
const zfInternalIteratorName = "__autoIter__"

# See bug https://github.com/nim-lang/Nim/issues/7787
const hasIteratorBug = true

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

  ## used for "combinations" command as output
  Combination*[A,T] = object
    it: array[A,T]
    idx: array[A,int]

type
  FiniteIndexable[T] = concept a
    a.low() is int
    a.high() is int
    a[int]

  FiniteIndexableLen[T] = concept a
    a.len() is int
    a[int]

  FiniteIndexableLenIter[T] = concept a
    a.len() is int
    a[int] is T 
    for it in a:
      type(it) is T

  Iterable[T] = concept a
    for it in a:
      type(it) is T
  
  Appendable[T] = concept a, var b
    for it in a:
      type(it) is T
    b.append(T)

  Addable[T] = concept a, var b
    for it in a:
      type(it) is T
    b.add(T)
  

static: 
  ## Contains all functions that may result in a sequence result. Elements are added automatically to SEQUENCE_HANDLERS
  var SEQUENCE_HANDLERS = [$Command.combinations, $Command.sub].toSet()

## Can be read in test implementation
var lastFailure {.compileTime.} : string = "" 

proc zfGetLastFailure*() : string {.compileTime.} = 
  result = lastFailure
  lastFailure = ""

## Called when some failure is detected during macro processing. 
proc zfFail*(msg: string) {.compileTime.} =
  lastFailure = msg
  doAssert(false, ": " & msg)

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
proc zfAddFunction*(functionName: string) {.compileTime.} =
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
proc findNode*(node: NimNode, kind: NimNodeKind, content: string = nil) : NimNode =
  if node.kind == kind and (content == nil or content == $node):
    return node
  for child in node:
    let res = child.findNode(kind, content)
    if res != nil:
      return res
  return nil

## Creates the extension function
proc createExtensionProc(name: string, cmdSeq: seq[string]): (NimNode,NimNode) = 
  let procName = newIdentNode("zfExtend" & name)
  # add inlineForeach manually
  zfAddFunction($Command.foreach)
  let cseq = if cmdSeq != nil: cmdSeq else: zfFunctionNames
  let ext = newIdentNode("ext")
  let resultIdent = newIdentNode("result")
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
  let (funDef,funName) = createExtensionProc("UserExt", nil)
  result = quote:
    `funDef`
    zfSetExtension(`funName`)

## Create a delegate function for user-defined functions and register sequence handler functions.
macro zfCreateExtension*(seqHandlers: static[seq[string]]): untyped =
  zfAddSequenceHandlers(seqHandlers)
  result = quote:
    zfCreateExtension()

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

## Special implementation to initialize array output.
proc zfInit*[A, T, U](s: array[A,T], handler: proc(it: T): U): array[A, U] =
  discard
    
## Special implementation to initialize DoublyLinkedList output.
proc zfInit*[T, U](a: DoublyLinkedList[T], handler: proc(it: T): U): DoublyLinkedList[U] =
  initDoublyLinkedList[U]()
## Special implementation to initialize SinglyLinkedList output.
proc zfInit*[T, U](a: SinglyLinkedList[T], handler: proc(it: T): U): SinglyLinkedList[U] =
  initSinglyLinkedList[U]()

## This one could be overwritten when the own type is a template and could be mapped to different
## target type.
## Default is seq output type.
proc zfInit*[T, U](a: Iterable[T], handler: proc(it: T): U): seq[U] =
  @[]
    
## General zfInit for iterable types.
## This should be overwritten for user defined types because otherwise the default = seq[T] on will be created.
proc zfInit*[T](a: Iterable[T]): Iterable[T] =
  proc zfIdent[T](it: T): T = it
  zfInit(a, zfIdent)

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

## Add item to type where an "add" proc is defined for
proc zfAddItem*[T](a: var Addable[T], idx: int, item: T) =
  discard(idx)
  a.add(item)

## Add item to type where an "append" proc is defined for (e.g. DoublyLinkedList)
proc zfAddItem*[T](a: var Appendable[T], idx: int, item: T) =
  discard(idx)
  a.append(item)

## Shortcut and safe way to get the ident label of a node
proc label(node: NimNode): string = 
  if node.kind == nnkCall or node.kind == nnkOpenSymChoice:
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
proc findNodePath(node: NimNode, kind: NimNodeKind, content: string = nil) : (NimNode,seq[int]) = 
  result = (nil, @[])
  for i,child in node:
    if child.kind == kind and (content == nil or content == $node):
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

## Returns true if the input collection type is a `DoublyLinkedList` or a `SinglyLinkedList`.
proc isListType*(ext: ExtNimNode): bool = 
  ext.typeDescription.startswith("DoublyLinkedList") or ext.typeDescription.startswith("SinglyLinkedList")

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

proc addElemResult(ext: ExtNimNode, addItem: NimNode): NimNode {.compileTime.} =
  let resultIdent = ext.res
  # use -1 to force an error in case the index was actually needed instead of silently doing the wrong thing
  let idxIdent = if ext.needsIndex: newIdentNode(zfIndexVariableName) else: newIntLitNode(-1)
  let resultType = ext.resultType
  let typedescr = ext.typeDescription
  
  result = quote:
    when compiles(zfAddItem(`resultIdent`, `idxIdent`, `addItem`)):
      zfAddItem(`resultIdent`, `idxIdent`, `addItem`)
    else:
      static:
        when (`resultType` == nil or `resultType` == ""):
          zfFail("Need either 'add' or 'append' implemented in '" & `typedescr` & "' to add elements")
        else:
          zfFail("Result type '" & `resultType` & "' and added item of type '" & $`addItem`.type & "' do not match!")

## Helper for Zero-DSL: quote all used variables that are defined somewhere in the created function.
proc addQuotes(a: NimNode, quotedVars: Table[string,string]) =
  if a.kind != nnkAccQuoted and (a.kind != nnkCall or a[0].label != "pre"):
    for i in 0..<a.len:
      let child = a[i]
      if child.kind == nnkIdent and child.label in quotedVars:
        a[i] = nnkAccQuoted.newTree(newIdentNode(quotedVars[child.label]))
      else:
        child.addQuotes(quotedVars)

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
  if ext.node.len == 2 and ext.node[1].label == "_":
    # delegate default arguments
    ext.node.del(1)
    for i in 1..<caller.len:
      ext.node.add(caller[i])
  ext.args.insert(ext.nodeIndex + delegateIndex + 1, ext.node.copyNimTree())
  ext.node = nnkStmtList.newTree()

## Parse the given Zero-DSL and create an inlineXyz function.
## E.g. `zf_inline index(): ...` will create `proc inlineIndex(ext: ExtNimNode)`.
## The DSL parses the content for the following sections:
## - pre: prepare used variables - also manipulated ext, when default behaviour is not sufficient.
##        as zero DSL has limitations when creating code the pre-section can be used to 
##        circumvent these deficiencies
## - init: initialize variables used in the loop
## - loop: the main part that is running within the loop
## - delegate: delegates this function call to another function (created with zero-DSL) - possibly with changed parameters
## - end: section added at the end of the loop
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
  if header.kind == nnkCall and body.kind == nnkStmtList:
    var funDef = header
    var hasDelegate = false
    var hasLoop = false
    let funName = funDef.label
    # when this function has been called with zf_inline_call the "__call__" has to be stripped for the actual zero function name
    if funName.endswith("__call__"):
      let n = funName[0..^9]
      if not (n in zfFunctionNames):
        zfAddFunction(n) # only add it once
    else:
      zfAddFunction(funName)

    let procName = newIdentNode("inline" & funName.capitalizeAscii())
    # parameters given to the zero function
    var paramSection = nnkStmtList.newTree()
    # referenced variables are added here
    # this contains common definitions for all sections
    let letSection = nnkStmtList.newTree()
    # reference to the 'ext' parameter of the created proc
    let ext = newIdentNode("ext")
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

    if funDef.kind == nnkCall:
      for i in 1..numArgs:
        var defaultVal: NimNode = nil
        var symName = funDef[i].label
        let hasDefault = funDef[i].kind == nnkExprEqExpr
        if hasDefault:
          defaultVal = funDef[i][1]
          symName = funDef[i][0].label
        let sym = newIdentNode(symName)
        if symName != "_":
          quotedVars[symName] = symName
        paramSection.add quote do:
          let `sym` =
            if `i` < `ext`.node.len: 
              adapt(`ext`, `i`)
            else:
              when `hasDefault`:
                quote:
                  `defaultVal`
              else:
                when `symName` != "":
                  when `symName` == "_":
                    zfFail("'$1' needs at least 1 parameter!" % [`funName`])
                  else:
                    zfFail("missing argument '$1' for '$2'" % [`symName`, `funName`])
                newIntLitNode(0)
    
    let hasResult = body.findNode(nnkIdent, "result") != nil 
    if hasResult:
      let res = newIdentNode("resultIdent")
      letSection.add quote do:
        let `res` = newIdentNode("result")
      quotedVars["result"] = "resultIdent"
    if hasResult or body.findNode(nnkReturnStmt) != nil:
      letSection.add quote do:
        if not `ext`.isLastItem:
          zfFail("'$1' has a result and must be last item in chain!" % `funName`)
    else:
      # register iterator as sequence handler
      zfAddSequenceHandlers(funName)
      
    if body.findNode(nnkIdent, zfIndexVariableName) != nil:
      let idxIdent = newIdentNode("idxIdent")
      letSection.add quote do:
        let `idxIdent` = newIdentNode(`zfIndexVariableName`)
        discard(`idxIdent`)
        `ext`.needsIndex = true
      quotedVars[zfIndexVariableName] = "idxIdent"

    if (not hasPre and body[0].label != "delegate") or body.len != 2 or body[1].label != "delegate": 
      # replace it in 'it = ...' with `nextIt` and create the next iterator
      if body.replaceIt(nnkAccQuoted.newTree(newIdentNode("nextIdent")), true):
        let nextIt = newIdentNode("nextIdent")
        letSection.add quote do:
          let `nextIt` = `ext`.nextItNode()
      # access the previous iterator replacing 'it'
      if body.replaceIt(nnkAccQuoted.newTree(newIdentNode("prevIdent")), false):
        let prev = newIdentNode("prevIdent")
        letSection.add quote do:
          let `prev` = `ext`.prevItNode()

    # create the proc
    let q = quote:
      proc `procName`( `ext`: ExtNimnode) {.compileTime.} =
        `paramSection`
        `letSection`
        nil

    let code = q.getStmtList()

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
        let oldParams = newIdentNode("oldParams")
        let isLast = newIdentNode("isLast")
        let isLastCmd = newIdentNode("isLastCmd")
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
            let caller = `ext`.node
            `ext`.node = quote:
              `p`
            `ext`.isLastItem = `isLast` and `isLastCmd`
            zfDelegate(`ext`, `oldParams`, `cmdIdx`)
        code.add quote do:
          discard(`oldParams`)
      of "end":
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
    result = q
  else:
    doAssert(false, "did not expect " & $header.kind & " or " & $body.kind)

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
  doAssert(header.kind == nnkCall)
  header[0] = newIdentNode(header.label & "__call__")
  let fun = newIdentNode("inline" & header.label.capitalizeAscii())
  let ext = newIdentNode("ext")
  let q = zeroParse(header, body)
  result = quote:
    `q`
    `fun`(`ext`)

## Implementation of the 'map' command.
## Each element of the input is mapped to a given function.
zf_inline map(f):
  loop:
    let it = f

zf_inline indexedMap(f):
  loop:
    let it = (idx, f)

## Implementation of the 'filter' command.
## The trailing commands execution depend on the filter condition to be true.
zf_inline filter(cond):
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
zf_inline takeWhile(cond):
  loop:
    if not cond:
      break
    else:
      nil  
        
## Implementation of the `take` command.
## `take(count)` : Take `count` elements.
zf_inline take(count):
  init:
    var idxTake = -1
  delegate:
    takeWhile:
      idxTake += 1
      idxTake < count

## Implementation of the `dropWhile` command.
## `dropWhile(cond)` : drop elements as long the given condition is true. 
## Once the condition gets false, all following elements are used.
zf_inline dropWhile(cond):
  init:
    var gate = false
  loop:
    if gate or not cond:
      gate = true
      nil

## Implementation of the `drop` command.
## `drop(count)` : drop (or discard) the next `count` elements.
zf_inline drop(count):
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
    
    zf_inline_call sub(minIndex, endIndex):
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
zf_inline exists(search):
  init:
    result = false
  loop:
    if search:
      return true

## Implementation of the 'find' command.
## Searches the input for a given expression. Returns an option value.
zf_inline find(cond):
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
zf_inline all(test):
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
  var adaptedExpression = ext.adapt()
  
  # special case: assignment to iterator -> try to assign to outer list (if possible)
  if adaptedExpression.kind == nnkExprEqExpr:
    if ext.itIndex > 1:
      zfFail("Adapted list cannot be changed in-place!")
    # this only works if the current list has not (yet) been manipulated    
    var itNode = adaptedExpression.findParentWithChildLabeled(ext.prevItNode.label) 
    if itNode != nil:
      let listRef = ext.listRef
      let index = newIdentNode(zfIndexVariableName)
      let rightSide = adaptedExpression.last
      # changing the iterator content will only work with indexable + variable containers
      if ext.isListType():
        let itlist = newIdentNode(listIteratorName)
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
  ext.node = quote:
    `adaptedExpression`

## Implementation of the 'index' command.
## Returns the index of the element in the input list when the given expression was found or -1 if not found.
zf_inline index(cond):
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
  let idxIdent = newIdentNode(zfIndexVariableName)
  let itCombo = newIdentNode(zfCombinationsId)
  if ext.node.len == 1:
    if ext.isListType():
      zf_inline_call combinations():
        pre:
          let itList = newIdentNode(listIteratorName)
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

  if ext.hasMinHigh:
    let idxIdent = newIdentNode(zfIndexVariableName)
    let minHigh = newIdentNode(minHighVariableName)
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
    let itlist = newIdentNode(listIteratorName)
    ext.node = quote:
      var `itlist` = `listRef`.head
      while `itlist` != nil:
        var `itIdent` = `itlist`.value
        nil
    ext.endLoop.add quote do:
      `itlist` = `itlist`.next
  
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
  let (funDef,_) = createExtensionProc("Defaults", nil)
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

## Wraps the given node with `mkIndexable` when the node type does not support access with `[]`
proc wrapIndexable(a: NimNode): NimNode {.compileTime.} =
  let q = quote:
    when not compiles(`a`[0]) or not compiles(high(`a`)):
      when not compiles(mkIndexable(`a`)):
        static:
          zfFail("need to provide an own implementation for mkIndexable(" & $`a`.type & ")")
      else:
        var `a` = mkIndexable(`a`)
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
    let minHigh = newIdentNode(minHighVariableName)
    result.add quote do:
      let `minHigh`= min(`highList`)

## Gets the result type, depending on the input-result type and the type-description of the input type.
## When the result type was given explicitly by the user that type is used.
## Otherwise the template argument is determined by the input type.
proc getResType(resultType: string, td: string): (NimNode, bool) {.compileTime.} =
  if resultType == nil:
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
    if i != -1 and hasIter and (not forceSeq or resultType != nil):
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
    elif forceSeq and hasIter:
      code = quote:
        var res: seq[iteratorType(`itDef`)]
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
  var resultType : string = nil
  if hasTo:
    args.del(args.len-1) # remove the "to" node
    if args.len <= 1:
      # there is no argument other than "to": add default mapping function "map(it)"
      args.add(parseExpr($Command.map & "(" & zfIteratorVariableName & ")"))
    else:
      if not (args.last[0].label in SEQUENCE_HANDLERS):
        zfFail("'to' can only be used with list results - last arg is '" & args.last[0].label & "'")
    resultType = last[1].repr
    if resultType == "list": # list as a shortcut for DoublyLinkedList
      resultType = "DoublyLinkedList" & implicitTypeSuffix
    elif resultType.startswith("list["):
      resultType = "DoublyLinkedList" & resultType[4..resultType.len-1]
    elif resultType == "seq":
      resultType = defaultResultType
  if resultType == nil:
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
          if isSeq or isList or resultType == nil:
            resultType =
              if isSeq:
                "seq"
              elif isList:
                "DoublyLinkedList"
              elif (td.startswith("DoublyLinkedList")):
                td & implicitTypeSuffix
              else:
                defaultResultType # default to sequence - and use it if isSeq is used explicitly
  if resultType == nil and td == "enum":
    resultType = "seq[" & $args[0] & "]" & implicitTypeSuffix
  result = resultType

## Main function that creates the outer function call.
proc iterHandler(args: NimNode, debug: bool, td: string): NimNode {.compileTime.} =
  let orig = if debug: args.copyNimTree() else: nil
  let resultType = args.checkTo(td) 
  let preInit = args.replaceZip() # zip is replaced with map + filter
  let hasMinHigh = preInit.len > 0
  var lastCall = args.last[0].label
  let isIter = lastCall == $Command.createIter
  var iterNode: NimNode = nil
  if isIter:
    lastCall = args[args.len-2][0].label
    let iterName = newIdentNode(args.last[1].label)
    args.del(args.len-1)
    if not (args.last[0].label in SEQUENCE_HANDLERS):
      zfFail("'iter' can only be used with list results - last arg is '" & args.last[0].label & "'")
    iterNode = quote:
      iterator `iterName`(): auto = 
        nil
  
  let isSeq = lastCall in SEQUENCE_HANDLERS

  var defineIdxVar = (not hasMinHigh and not isIter) and (isSeq and hasIteratorBug) 
  var needsIndexVar = false

  if ((not isIter and (isSeq and (resultType != nil and resultType.startswith("array") or 
      (resultType == nil and td.startswith("array"))))) or
      args.findNode(nnkIdent, zfIndexVariableName) != nil):
      needsIndexVar = true
  
  let codeStart = nnkStmtList.newTree()
  var code = codeStart

  var init = code
  let initials = nnkStmtList.newTree()
  let varDef = nnkStmtList.newTree()
  init.add(varDef)
  init.add(initials)

  var index = 0
  let listRef = args[0]
  let finals = nnkStmtList.newTree()
  let endLoop = nnkStmtList.newTree()
  var startNode: NimNode = nil

  var argIdx = 0
  var ext: ExtNimNode

  while argIdx < args.len: # args could be changed
    let arg = args[argIdx]
    let isLast = argIdx == args.len-1
    
    let cmdName = arg.label
    ext = ExtNimNode(node: arg, 
                    nodeIndex: argIdx,                    
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
                    elemAdded: false)
    argIdx += 1
    ext.inlineElement()
    let newCode = ext.node.getStmtList()
    code.add(ext.node)

    # make sure the collection is created
    if (argIdx == args.len) and not ext.elemAdded and cmdName in SEQUENCE_HANDLERS:
      var node = newCode
      if node == nil:
        # directly append the adding function
        node = code
      node.add(ext.addElem(mkItNode(ext.itIndex-1)))

    if startNode == nil:
      startNode = ext.node
    if newCode != nil:
      code = newCode
    if not hasMinHigh and not defineIdxVar:
      defineIdxVar = ext.needsIndex
    index = ext.itIndex
    
  if not hasMinHigh and defineIdxVar:
    let idxIdent = newIdentNode(zfIndexVariableName)
    varDef.add quote do:
      var `idxIdent` = 0 

  if finals.len > 0:
    init.add(finals)  
    
  # could be combinations of for and while, but only one while (for DoublyLinkedList) -> search while first
  var loopNode = startNode.findNode(nnkWhileStmt) 
  if loopNode == nil:
    loopNode = startNode.findNode(nnkForStmt)
  if endLoop.len > 0:
    loopNode.last.add(endLoop)

  if not hasMinHigh and defineIdxVar and loopNode != nil:
    # add index increment to end of the for loop
    let idxIdent = newIdentNode(zfIndexVariableName)
    loopNode.last.add quote do:
      `idxIdent` += 1

  let needsFunction = (lastCall != $Command.foreach)
  if needsFunction:
    let theProc = if isIter: iterNode else: ext.createAutoProc(args, isSeq, resultType, td, codeStart, index > 1, isIter)
    result = theProc
    code = result.last.getStmtList() 
    if isIter:
      code = result.getStmtList()
      code.add(codeStart)
    if not isSeq:
      code.insert(0,codeStart)
    if preInit.len > 0:
      code.insert(0, preInit)
    if not isIter:
      result = nnkCall.newTree(result)
  else:
    # there is no extra function, but at least we have an own section here - preventing double definitions
    var q = quote:
      if true:
        `preInit`
        nil
    result = q
    code = q.getStmtList()
    code.insert(0,codeStart)
  
  if (debug):
    echo("# " & repr(orig).replace(",", " -->"))
    echo(repr(result).replace("__", ""))
    # for the whole tree do (but this could crash):
    # echo(treeRepr(result))
  
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
      if res == nil:
        result = repr(node)
      else:
        result = res
  else:
    let n1 = node.repr
    let n2 = nodeInst.repr
    if n2 == nil or n1.len > n2.len:
      result = n1
    else:
      result = n2

macro connectCall(td: typedesc, args: varargs[untyped]): untyped = 
  result = iterHandler(args, false, getTypeInfo(td.getType[1], td.getTypeInst[1]))

## Preparse the call to the iterFunction.
proc delegateMacro(a: NimNode, b1:NimNode, debug: bool, td: string): NimNode =
  var b = b1

  # we expect b to be a call, but if we have another node - e.g. infix or bracketexpr - then
  # we search for the actual call, do the macro expansions on the call and 
  # add the result back into the tree later
  var outer = b  
  var path : seq[int] = nil
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
  result = iterHandler(args, debug, td)

  if path != nil: # insert the result back into the original tree
    result = outer.apply(path, result)

## delegate call to get the type information.
macro delegateArrow(td: typedesc, a: untyped, b: untyped, debug: static[bool]): untyped =
  result = delegateMacro(a, b, debug, getTypeInfo(td.getType[1], td.getTypeInst[1]))
  
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
  
const debugAll = false
## general macro to invoke all available zero_functional functions
macro `-->`*(a: untyped, b: untyped): untyped =
  let (a,b,debug) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    if not debug: # using debug (or `debug`) directly does not work (?!)
      result = quote:
        delegateArrow(type(`a`), `a`, `b`, debugAll)
    else:
      result = quote:
        delegateArrow(type(`a`), `a`, `b`, true)
  else:
    result = delegateMacro(a, b, debug or debugAll, "seq")

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  let (a,b,_) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    result = quote:
      delegateArrow(type(`a`), `a`, `b`, true)
  else:
    result = delegateMacro(a, b, true, "seq")
