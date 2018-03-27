import sequtils, macros, options, sets, lists, typetraits, strutils

const iteratorVariableName = "it"
const accuVariableName = "a"
const combinationsId = "c"
const indexVariableName = "idx"

const internalIteratorName = "__" & iteratorVariableName & "__"
const useInternalAccu = accuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & accuVariableName & "__" else: "result"
const implicitTypeSuffix = "?" # used when result type is automatically determined
const listIteratorName = "__itlist__"
const listIteratorInnerName = "__itlist2__"

type 

  Command {.pure.} = enum
    ## All available commands.
    ## 'to' - is a virtual command
    all, combinations, exists, filter, find, flatten, fold, foreach, index, indexedMap, map, reduce, sub, zip, to

  ExtNimNode = ref object ## Store additional info the current NimNode used in the inline... functions
    node: NimNode     ## the current working node / the current function
    commands: HashSet[Command] ## set of all used command
    index: int        ## index used for the created iterator - 0 for the first 
    isLastItem: bool  ## true if the current item is the last item in the command chain
    initials: NimNode ## code section before the first iterator where variables can be defined
    endLoop: NimNode  ## code at the end of the for / while loop
    finals: NimNode   ## code to set the final operations, e.g. the result
    listRef:  NimNode ## reference to the list the iterator is working on
    typeDescription: string ## type description of the outer list type
    resultType: string ## result type when explicitly set
    needsIndex: bool  ## true if the idx-variable is needed
    nextIndexInc: bool ## if set to true the index will be increment by 1 for the next iterator 

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
  

static: # need to use var to be able to concat
  let PARAMETERLESS_CALLS = [$Command.flatten, $Command.combinations].toSet
  let FORCE_SEQ_HANDLERS = [$Command.indexedMap, $Command.flatten, $Command.zip].toSet
  let NEEDS_INDEX_HANDLERS = [$Command.indexedMap, $Command.index, $Command.flatten, $Command.sub, $Command.index, $Command.combinations].toSet
  let CANNOT_BE_ALTERED_HANDLERS = [Command.map, Command.indexedMap, Command.combinations, Command.flatten, Command.zip].toSet
  var SEQUENCE_HANDLERS = [$Command.map, $Command.filter, $Command.sub, $Command.combinations].toSet
  var HANDLERS = [$Command.exists, $Command.all, $Command.index, $Command.fold, $Command.reduce, $Command.foreach, $Command.find].toSet
  SEQUENCE_HANDLERS.incl(FORCE_SEQ_HANDLERS)
  HANDLERS.incl(SEQUENCE_HANDLERS)

## Converts the id-string to the given command.
proc toCommand(key: string): Option[Command] =
  for it in Command:
    if $it == key:
      return some(it)
  return none(Command)

proc zf_fail(msg: string) {.compileTime.} =
  assert(false, ": " & msg)

## Special implementation to initialize array output.
proc init_zf*[A, T, U](s: array[A,T], handler: proc(it: T): U): array[A, U] =
  discard
    
## Special implementation to initialize DoublyLinkedList output.
proc init_zf*[T, U](a: DoublyLinkedList[T], handler: proc(it: T): U): DoublyLinkedList[U] =
  initDoublyLinkedList[U]()
## Special implementation to initialize SinglyLinkedList output.
proc init_zf*[T, U](a: SinglyLinkedList[T], handler: proc(it: T): U): SinglyLinkedList[U] =
  initSinglyLinkedList[U]()

## This one could be overwritten when the own type is a template and could be mapped to different
## target type.
## Default is seq output type.
proc init_zf*[T, U](a: Iterable[T], handler: proc(it: T): U): seq[U] =
  @[]
    
## General init_zf for iterable types.
## This should be overwritten for user defined types because otherwise the default = seq[T] on will be created.
proc init_zf*[T](a: Iterable[T]): Iterable[T] =
  proc ident(it: T): T = it
  init_zf(a, ident)

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
proc addItemZf*[A,T](a: var array[A,T], idx: int, item: T) = 
  a[idx] = item

## Add item to seq. Actually the below Addable could be used, but this does not always work out.
proc addItemZf*[T](a: var seq[T], idx: int, item: T) =
  discard(idx)
  a.add(item)

## Special implementation for ``SinglyLinkedList`` which has only a ``preprend``
proc addItemZf*[T](a: var SinglyLinkedList[T], idx: int, item: T) =
  discard(idx)
  a.prepend(item)

## Add item to type where an "add" proc is defined for
proc addItemZf*[T](a: var Addable[T], idx: int, item: T) =
  discard(idx)
  a.add(item)

## Add item to type where an "append" proc is defined for (e.g. DoublyLinkedList)
proc addItemZf*[T](a: var Appendable[T], idx: int, item: T) =
  discard(idx)
  a.append(item)

## Helper function to determine the type of the iterated item
proc iterType[T](items: Iterable[T]): T = 
  discard

## Replace the given identifier by the string expression
proc replace(node: NimNode, searchNode: NimNode, replNode: NimNode): NimNode =
  result = node
  if node.len > 0: 
    for i in 0..<node.len:
      let child = node[i]
      if child == searchNode:
        node[i] = replNode
      else:
        node[i] = child.replace(searchNode, replNode)
  elif node == searchNode:
    result = replNode

## Find a node given its kind and - optionally - its content.
proc findNode(node: NimNode, kind: NimNodeKind, content: string = nil) : NimNode =
  if node.kind == kind and (content == nil or content == $node):
    return node
  for child in node:
    let res = child.findNode(kind, content)
    if res != nil:
      return res
  return nil

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

## Shortcut and safe way to get the ident label of a node
proc label(node: NimNode): string = 
  if node.kind == nnkIdent or node.kind == nnkSym:
    return $node
  return ""

## Helper that gets nnkStmtList and removes a 'nil' inside it - if present.
## The nil is used as placeholder for further added code.
proc getStmtList(node: NimNode, removeNil = true): NimNode =
  var child = node
  while child.len > 0:
    child = child.last
    if child.kind == nnkStmtList:
      if removeNil:
        if child.len > 0 and child.last.kind == nnkNilLit:
          child.del(child.len-1,1)
      return child
  return nil
    
## Creates the result tuple for the zip operation.
proc createZipTuple(node: NimNode): NimNode =
  var ex = ""
  for i in 1..<node.len:
    if ex.len > 0:
      ex &= ","
    ex &= node[i].repr & "[0]"
  ex = "(" & ex & ")"
  result = parseExpr(ex)

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

## Creates the function that returns the final result of all combined commands.
## The result type depends on map, zip or flatten calls. It may be set by the user explicitly using to(...)
proc createAutoProc(node: NimNode, isSeq: bool, resultType: string, td: string): NimNode =
  var (resType, explicitType) = getResType(resultType, td)
  let resultIdent = newIdentNode("result")
  let autoProc = quote:
    (proc(): auto =
      nil)
  var code: NimNode = nil

  # set a default result in case the resType is not nil - this result will be used
  # if there is no explicit map, zip or flatten operation called
  if resType != nil:
    code = quote:
      var res: `resType` 
      `resultIdent` = init_zf(res)

  # check explicitType: type was given explicitly (inclusive all template arguments) by user,
  # then we use resType directly:
  if explicitType:
    discard # use default result above
  elif isSeq:
    # now we try to determine the result type of the operation automatically...
    # this is a bit tricky as the operations zip, map and flatten may / will alter the result type.
    # hence we try to apply the map-operation to the iterator, etc. to get the resulting iterator (and list) type.
    let listRef = node[0]
    let itIdent = newIdentNode(iteratorVariableName) # the original "it" used in the closure of "map" command
    let idxIdent = newIdentNode(indexVariableName)
    var handlerIdx = 0
    var handler : NimNode = nil
    let comboNode = newIdentNode(combinationsId)
    # the "handler" is the default mapping ``it`` to ``it`` (if "map" is not used)
    var handlerInit : NimNode = nil

    for child in node:
      if child.len > 0:
        let label = child[0].repr
        if label == $Command.map or label == $Command.indexedMap:
          let isIndexed = label == $Command.indexedMap
          var params = child[1].copyNimTree() # params of the map command
          # use / overwrite the last mapping to get the final result type
          let prevHandler = "handler" & $handlerIdx 
          handlerIdx += 1
          handler = newIdentNode("handler" & $handlerIdx)
          if handlerInit == nil:
            handlerInit = nnkStmtList.newTree()
          else:
            # call previous handler for each iterator instance
            params = params.replace(itIdent, parseExpr(prevHandler & "(" & iteratorVariableName & ")"))
          let q = quote:
            proc `handler`(`itIdent`: auto): auto =
              var `idxIdent` = 0  # could be used as map param
              var `comboNode` = createCombination([0,0],[0,0]) # could be used as map param
              when `isIndexed`:
                `resultIdent` = (0,`params`)
              else:
                `resultIdent` = `params`
              discard `idxIdent`
              discard `comboNode`
          handlerInit.add(q)

        elif label == $Command.zip:
          # zip(a,b,c) => params = (a[0],b[0],c[0])
          let params = createZipTuple(child.copyNimTree())
          handlerIdx += 1
          handler = newIdentNode("handler" & $handlerIdx)
          let q = quote:
            proc `handler`(`itIdent`: auto): auto =
              `params`
          handlerInit = nnkStmtList.newTree().add(q)

        elif label == $Command.flatten and resultType != nil:
          # try to find the resulting type of the flatten operation
          # e.g. list[array[2,int]] --> flatten() => list[int]
          let actualType = resType.repr
          var innerType = "int"
          if actualType.endswith("]]"):
            var idx = actualType.rfind("[")
            innerType = actualType[idx+1..actualType.len-3]
            idx = innerType.rfind(", ")
            if idx != -1:
              innerType = innerType[idx+2..innerType.len-1]
          let idx = actualType.find('[')
          let newType = actualType[0..idx-1] & "[" & innerType & "]" 
          handlerInit = nil
          handlerIdx = 0
          resType = parseExpr(newType)
          code = quote:
            var res: `resType`
            `resultIdent` = init_zf(res)
      
    if handlerInit != nil:
      # we have a handler(-chain): use it to map the result type
      if resultType == nil:
        code = quote:
          `handlerInit`
          `resultIdent` = init_zf(`listRef`,`handler`)
      else:
        let resTypeOk = resType.repr.startswith(resultType)
        if resultType == "seq":
          # this works with seq but unfortunately not (yet) with DoublyLinkedList
          code = quote:
            `handlerInit`
            var res: seq[iterType(`listRef`).type]
            `resultIdent` = init_zf(res, `handler`)
        elif (resultType == "list" or resultType == "DoublyLinkedList") and not resTypeOk:
          assert(false, "unable to determine the output type automatically - please use list[<outputType>].") 
        elif resultType == "array" and not resTypeOk:
          assert(false, "unable to determine the output type automatically - array needs size and type parameters.")
        else:
          code = quote:
            `handlerInit`
            var res: `resType`
            when compiles(init_zf(res,`handler`)):
              `resultIdent` = init_zf(res,`handler`)
            else:
              static:
                assert(false, "unable to determine the output type automatically.")
    elif resultType == nil: # no handlerInit used
      # result type was not given and map/zip/flatten not used: 
      # use the same type as in the original list
      code = quote:
        `resultIdent` = init_zf(`listRef`)
  # no sequence output:
  # we do _not_ need to initialize the resulting list type here
  else:
    code = nil

  let stmtInit = autoProc.getStmtList()
  if code != nil:
    stmtInit.add(code)
  stmtInit.add(newNilLit())
  result = autoProc;
  

proc mkItNode(index: int) : NimNode {.compileTime.} = 
  newIdentNode(internalIteratorName & ("$1" % $index))

proc itNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index)

proc prevItNode(ext: ExtNimNode) : NimNode {.compileTime.} =
  result = mkItNode(ext.index - 1)

proc res(ext: ExtNimNode): NimNode {.compileTime.} =
  result = newIdentNode("result")

## Replace the variable name `it`with the continuos iterator variable name.
## The same goes for the accu `a`.
## Expressions on the left side of dot `.` are not replaced - because `it`could
## also be a member of a compund type - so `it.someMember` is replaced, `c.it` is not.   
proc adapt(node: NimNode, iteratorIndex: int, inFold: bool): NimNode {.compileTime.} =
  case node.kind:
  of nnkIdent:
    if $node == iteratorVariableName:
      return mkItNode(iteratorIndex)
    elif inFold and useInternalAccu and $node == accuVariableName:
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

## Shortcut for `node.adapt()`
proc adapt(ext: ExtNimNode, index=1, inFold=false): NimNode {.compileTime.} =
  result = ext.node[index].adapt(ext.index-1, inFold)

proc isListType(ext: ExtNimNode): bool = 
  ext.typeDescription.startswith("DoublyLinkedList") or ext.typeDescription.startswith("SinglyLinkedList")

## Helper function that creates a list output if map, filter or flatten is the last command
## in the chain and a list is generated as output.
proc inlineAddElem(ext: ExtNimNode, addItem: NimNode): NimNode {.compileTime.} = 
  let resultIdent = ext.res
  # use -1 to force an error in case the index was actually needed instead of silently doing the wrong thing
  let idxIdent = if ext.needsIndex: newIdentNode(indexVariableName) else: newIntLitNode(-1)
  let resultType = ext.resultType
  let typedescr = ext.typeDescription
  quote:
    when compiles(addItemZf(`resultIdent`, `idxIdent`, `addItem`)):
      addItemZf(`resultIdent`, `idxIdent`, `addItem`)
    else:
      static:
        when (`resultType` == nil or `resultType` == ""):
          zf_fail("Need either 'add' or 'append' implemented in '" & `typedescr` & "' to add elements")
        else:
          zf_fail("Result type '" & `resultType` & "' and added item of type '" & $`addItem`.type & "' do not match!")

## Implementation of the 'zip' command.
## A list of tuples or tuple-iterators are created.
proc inlineZip(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let idxIdentZip = newIdentNode("idxZip")
  let m = nnkCall.newTree(newIdentNode("min"), nnkBracket.newTree())
  let p = nnkPar.newTree()
  var z = 0
  for arg in ext.node:
    if z > 0:
      m.last.add(nnkCall.newTree(newIdentNode("high"), arg))
      p.add(nnkBracketExpr.newTree(arg, idxIdentZip))
    z += 1
  ext.node = quote:
    let minHigh = `m`
    for `idxIdentZip` in 0..minHigh:
      let `itIdent` = `p`
      nil
  ext.nextIndexInc = true
  result = ext
          
## Implementation of the 'map' command.
## Each element of the input is mapped to a given function.
proc inlineMap(ext: ExtNimNode, indexed: bool = false): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let adaptedF = ext.adapt()
  var next: NimNode
  
  if indexed:
    ext.needsIndex = true
    let idxIdent = newIdentNode(indexVariableName)
    next = quote:
      (`idxIdent`, `adaptedF`)
  else:
    next = adaptedF

  if ext.isLastItem:
    ext.node = ext.inlineAddElem(next)
  else:
    ext.node = quote:
      let `itIdent` = `next`
  ext.nextIndexInc = true
  result = ext

## Implementation of the 'filter' command.
## The trailing commands execution depend on the filter condition to be true.
proc inlineFilter(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let adaptedTest = ext.adapt()
  if ext.isLastItem:
    let push = ext.inlineAddElem(ext.prevItNode())
    ext.node = quote:
      if `adaptedTest`:
        `push`
  else:
    ext.node = quote:
        if `adaptedTest`:
          nil
  result = ext

## Implementation of the 'flatten' command.
## E.g. @[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6]
proc inlineFlatten(ext: ExtNimNode): ExtNimNode {.compileTime} =
  let itIdent = ext.itNode()
  let itPrevIdent = ext.prevItNode()
  let idxIdent = newIdentNode(indexVariableName)
  ext.needsIndex = true
  if not ext.isLastItem:
    ext.node = quote:
      for `itIdent` in `itPrevIdent`:
        `idxIdent` += 1
        nil
  else:
    let push = ext.inlineAddElem(itIdent)
    ext.node = quote:
      for `itIdent` in `itPrevIdent`:
        `push`
        `idxIdent` += 1
  ext.nextIndexInc = true
  result = ext

## Implementation of the 'sub' command.
## Delegates to 'filter' with the given start and end indices of the sub-list to create.
## In sub also Backward indices (e.g. ^1) can be used.
proc inlineSub(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  # sub is re-routed as filter implementation
  ext.needsIndex = true
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
      let endIndexAbs = endIndex.last
      endIndex = quote:
        len(`listRef`)-`endIndexAbs` # backwards index only works with collections that have a len
    newCheck = quote:
      `index` >= `minIndex` and `index` < `endIndex`
  ext.node = newCall($Command.filter, newCheck)
  return ext.inlineFilter()

## Implementation of the 'exists' command.
## Searches the input for a given expression. If one is found "true" is returned, else "false".
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

## Implementation of the 'find' command.
## Searches the input for a given expression. Returns an option value.
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

## Implementation of the 'all' command.
## Returns true of the given condition is true for all elements of the input, else false.
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
proc inlineForeach(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  var adaptedExpression = ext.adapt()
  
  # special case: assignment to iterator -> try to assign to outer list (if possible)
  if adaptedExpression.kind == nnkExprEqExpr:
    # this only works if the current list has not (yet) been manipulated with the following methods:
    # map, indexedMap, combinations, flatten and zip
    if (ext.commands.intersection(CANNOT_BE_ALTERED_HANDLERS).len > 0):
      assert(false, "Cannot change list in foreach that has already been altered with: map, indexedMap, combinations, flatten or zip!")

    var itNode = adaptedExpression.findParentWithChildLabeled($ext.prevItNode) 
    if itNode != nil:
      let listRef = ext.listRef
      let index = newIdentNode(indexVariableName)
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
  result = ext

## Implementation of the 'index' command.
## Returns the index of the element in the input list when the given expression was found or -1 if not found.
proc inlineIndex(ext: ExtNimNode): ExtNimNode{.compileTime.} =
  let adaptedTest = ext.adapt()
  ext.needsIndex = true
  var idxIdent = newIdentNode(indexVariableName)
  var resultIdent = ext.res
  let i = quote:
    `resultIdent` = -1 # index not found
  ext.initials.add(i)
  ext.node = quote:
    if `adaptedTest`:
      return `idxIdent` # return index
  result = ext  

## Implementation of the 'fold' command.
## Initially the result is set to initial value given by the user, then each element is added
## to the result by subsequent calls.
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

## Implementation of the 'reduce' command.
## Initially the result is set to the first element of the list, then each element is added
## to the result by subsequent calls.
proc inlineReduce(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let prevIdent = ext.prevItNode()
  let itIdent = ext.itNode() 
  ext.index += 1
  let adaptedExpression = ext.adapt()
  let initAccu = newIdentNode("initAccu")
  let resultIdent = ext.res()
  let i = quote:
    var `initAccu` = true
  ext.initials.add(i)
  
  ext.node = quote:
    if `initAccu`:
      `resultIdent` = `prevIdent`
      `initAccu` = false
    else:
      let `itIdent` = (`resultIdent`, `prevIdent`)
      `resultIdent` = `adaptedExpression`
  ext.nextIndexInc = true
  result = ext

## Implementation of the 'combinations' command.
## Each two distinct elements of the input list are combined to one element.
proc inlineCombinations(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  ext.needsIndex = true
  let idxIdent = newIdentNode(indexVariableName)
  let idxInnerIdent = newIdentNode("idxInner")
  let itCombo = newIdentNode(combinationsId)

  if ext.isListType():
    let itlist = newIdentNode(listIteratorName)
    let itlistInner = newIdentNode(listIteratorInnerName)
    let itPrevIdent = ext.prevItNode()
    ext.node = quote:
      var `itlistInner` = `itlist`.next
      var `idxInnerIdent` = `idxIdent`
      while `itlistInner` != nil:
        let `itCombo` = createCombination([`itPrevIdent`, `itlistInner`.value], [`idxIdent`, `idxInnerIdent`])
        `idxInnerIdent` += 1
        `itlistInner` = `itlistInner`.next
        nil
  else:
    let listRef = ext.listRef
    ext.node = quote:
      when not (`listRef` is FiniteIndexableLenIter):
        static:
          zf_fail("Only index with len types supported for combinations")
      for `idxInnerIdent` in `idxIdent`+1..<`listRef`.len():
        let `itCombo` = createCombination([`listRef`[`idxIdent`], `listRef`[`idxInnerIdent`]], [`idxIdent`, `idxInnerIdent`])
        nil
  result = ext

## Initial creation of the outer iterator.
proc inlineSeq(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let itIdent = ext.itNode()
  let listRef = ext.listRef

  if ext.isListType():
    # list iterator implemnentation
    let listRef = ext.listRef
    let itlist = newIdentNode(listIteratorName)
    ext.node = quote:
      var `itlist` = `listRef`.head
      while `itlist` != nil:
        var `itIdent` = `itlist`.value
        nil
    let e = quote:
      `itlist` = `itlist`.next
    ext.endLoop.add(e)
  
  else:
    # usual iterator implementation
    ext.node = quote:
      for `itIdent` in `listRef`:
        nil
    
  ext.nextIndexInc = true
  result = ext
  
proc ensureLast(ext: ExtNimNode) {.compileTime.} =
  if not ext.isLastItem:
    error("$1 can be only last in a chain" % $ext.node[0], ext.node)

proc ensureFirstAfterArrow(ext: ExtNimNode) {.compileTime.} =
  if ext.index != 1:
    error("$1 must be first in chain after initialization" % $ext.node[0], ext.node)

proc ensureFirst(ext: ExtNimNode) {.compileTime.} =
  if ext.index > 0:
    error("$1 supposed to be first" % $ext.node[0], ext.node)

proc ensureParameters(ext: ExtNimNode, no: int) {.compileTime.} = 
  if ext.node.len <= no:
    error($ext.node[0] & " needs at least $1 parameter(s)" % $no, ext.node)
        
## Delegates each function argument to the inline implementations of each command.
proc inlineElement(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let label = if (ext.node.len > 0 and ext.node[0].kind == nnkIdent): $ext.node[0] else: ""
  if ext.node.kind == nnkCall and (ext.index > 0 or label in HANDLERS):
    if not (label in PARAMETERLESS_CALLS):    
      ext.ensureParameters(1)
    let cmdCheck = label.toCommand
    if none(Command) != cmdCheck:
      let cmd = cmdCheck.get()
      case cmd:
      of Command.zip:
        ext.ensureFirst()
        return ext.inlineZip()
      of Command.map:
        return ext.inlineMap()
      of Command.filter:
        return ext.inlineFilter()
      of Command.exists:
        ext.ensureLast()
        return ext.inlineExists()
      of Command.find:
        ext.ensureLast()
        return ext.inlineFind()
      of Command.all:
        ext.ensureLast()
        return ext.inlineAll()
      of Command.index:
        ext.ensureLast()
        return ext.inlineIndex()
      of Command.indexedMap:
        return ext.inlineMap(indexed=true)
      of Command.fold:
        ext.ensureLast()
        ext.ensureParameters(2)
        return ext.inlineFold()
      of Command.reduce:
        ext.ensureLast()
        return ext.inlineReduce()
      of Command.foreach:
        return ext.inlineForeach()
      of Command.sub:
        return ext.inlineSub()
      of Command.flatten:
        return ext.inlineFlatten()
      of Command.combinations:
        ext.ensureFirstAfterArrow()
        return ext.inlineCombinations()
      of Command.to:
        assert(false, "'to' is only allowed as last argument (and will be removed then).")
    else:
      if "any" == label:
        warning("any is deprecated - use exists instead")
        return ext.inlineExists()
      else:
        error("$1 is unknown" % label, ext.node)
  else:
    ext.ensureFirst()
    return ext.inlineSeq()


## Check if the "to" parameter is used to generate a specific result type.
## The requested result type is returned and the "to"-node is removed.
proc checkTo(args: NimNode, td: string): string {.compileTime.} = 
  let last = args.last
  var resultType : string = nil
  if last.kind == nnkCall and last[0].repr == $Command.to:
    args.del(args.len-1) # remove the "to" node
    if args.len <= 1:
      # there is no argument other than "to": add default mapping function "map(it)"
      args.add(parseExpr($Command.map & "(" & iteratorVariableName & ")"))
    else:
      assert(args.last[0].label in SEQUENCE_HANDLERS, "'to' can only be used with list results - last arg is '" & args.last[0].label & "'")
    resultType = last[1].repr
    if resultType == "list": # list as a shortcut for DoublyLinkedList
      resultType = "DoublyLinkedList"
    elif resultType.startswith("list["):
      resultType = "DoublyLinkedList" & resultType[4..resultType.len-1]
  if resultType == nil:
    for arg in args:
      if arg.kind == nnkCall:
        let label = arg[0].repr 
        # shortcut handling for mapSeq(...)  <=> map(...).to(seq) and
        #                       mapList(...) <=> map(...).to(list) - etc.
        let isSeq = label.endswith("Seq") 
        let isList = label.endswith("List")
        # Check forced sequences or lists
        if isSeq or isList or label in FORCE_SEQ_HANDLERS:
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
                "seq[int]" & implicitTypeSuffix # default to sequence - and use it if isSeq is used explicitly
  if resultType == nil and td == "enum":
    resultType = "seq[" & $args[0] & "]" & implicitTypeSuffix
  result = resultType

## Main function that creates the outer function call.
proc iterHandler(args: NimNode, debug: bool, td: string): NimNode {.compileTime.} =
  let resultType = args.checkTo(td)
  let lastCall = $args.last[0]
  let isSeq = lastCall in SEQUENCE_HANDLERS
  var defineIdxVar = true
  var addIdxIncr = true

  if defineIdxVar:
    if not isSeq or not (resultType != nil and resultType.startswith("array") or (resultType == nil and td.startswith("array"))): 
      if nil == args.findNode(nnkIdent, indexVariableName):  
        defineIdxVar = false

  # check if 'var idx' has to be created or not - and 'idx += 1' to be added
  for arg in args:
    if arg.kind == nnkCall:
      let label = arg[0].label
      if label == $Command.flatten:
        addIdxIncr = false
      elif label in NEEDS_INDEX_HANDLERS:
        defineIdxVar = true

  var code: NimNode
  let needsFunction = (lastCall != $Command.foreach)
  if needsFunction:
    result = args.createAutoProc(isSeq, resultType, td)
    code = result.last.getStmtList()
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
  let varDef = nnkStmtList.newTree()
  init.add(varDef)
  init.add(initials)

  var index = 0
  let listRef = args[0]
  let finals = nnkStmtList.newTree()
  let endLoop = nnkStmtList.newTree()

  var commands: seq[Command] = @[]
  for arg in args:
    let isLast = arg == args.last
    
    if arg.kind == nnkCall:
      let label = arg[0].label
      let cmdCheck = label.toCommand
      if none(Command) != cmdCheck:
        commands.add(cmdCheck.get())

    let ext = ExtNimNode(node: arg, 
                      commands: commands.toSet(),
                      index: index, 
                      isLastItem: isLast,
                      initials: initials,
                      endLoop: endLoop,
                      finals: finals,
                      listRef: listRef,
                      typeDescription: td,
                      resultType: resultType,
                      needsIndex: defineIdxVar,
                      nextIndexInc: false).inlineElement()
    let newCode = ext.node.getStmtList()
    code.add(ext.node)
    if newCode != nil:
      code = newCode
    if not defineIdxVar:
      defineIdxVar = ext.needsIndex
    if ext.nextIndexInc:
      index += 1

  if defineIdxVar:
    let idxIdent = newIdentNode(indexVariableName)
    let identDef = quote:
      var `idxIdent` = 0 
    varDef.add(identDef)

  if finals.len > 0:
    init.add(finals)  
    
  # could be combinations of for and while, but only one while (for DoublyLinkedList) -> search while first
  var loopNode = result.findNode(nnkWhileStmt) 
  if loopNode == nil:
    loopNode = result.findNode(nnkForStmt)
  if endLoop.len > 0:
    loopNode.last.add(endLoop)

  if defineIdxVar and addIdxIncr and loopNode != nil:
    # add index increment to end of the for loop
    let idxIdent = newIdentNode(indexVariableName)
    let incrIdx = quote:
      `idxIdent` += 1
    loopNode.last.add(incrIdx)

  if (debug):
    echo(repr(result))
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
      zf_fail("Unexpected expression in macro call on right side of '-->'")

  # now re-arrange all dot expressions to one big parameter call
  # i.e. a --> filter(it > 0).map($it) gets a.connect(filter(it>0),map($it))
  let methods = b
  var m: seq[NimNode] = @[]
  var node = methods
  while node.kind == nnkCall:
    if node[0].kind == nnkDotExpr:
      m.add(nnkCall.newTree(node[0].last))
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
  result = iterHandler(mad, debug, td)

  if path != nil: # insert the result back into the original tree
    result = outer.apply(path, result)

## delegate call to get the type information.
macro delegateArrow(td: typedesc, a: untyped, b: untyped): untyped =
  result = delegateMacro(a, b, false, getTypeInfo(td.getType[1], td.getTypeInst[1]))

## delegate call to get the type information (debug mode).
macro delegateArrowDbg(td: typedesc, a: untyped, b: untyped): untyped =
  result = delegateMacro(a, b, true, getTypeInfo(td.getType[1], td.getTypeInst[1]))
  
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
  
## general macro to invoke all available zero_functional functions
macro `-->`*(a: untyped, b: untyped): untyped =
  let (a,b,debug) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    if not debug:
      result = quote:
        delegateArrow(type(`a`), `a`, `b`)
    else:
      result = quote:
        delegateArrowDbg(type(`a`), `a`, `b`)
  else:
    result = delegateMacro(a, b, debug, "seq")

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  let (a,b,_) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    result = quote:
      delegateArrowDbg(type(`a`), `a`, `b`)
  else:
    result = delegateMacro(a, b, true, "seq")
