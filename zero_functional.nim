import macros, options, sets, lists, typetraits, strutils

const iteratorVariableName = "it"
const accuVariableName = "a"
const combinationsId = "c"
const indexVariableName = "idx"

const internalIteratorName = "__" & iteratorVariableName & "__"
const useInternalAccu = accuVariableName != "result"
const internalAccuName = if (useInternalAccu): "__" & accuVariableName & "__" else: "result"
const implicitTypeSuffix = "?" # used when result type is automatically determined
const defaultResultType = "seq[int]" & implicitTypeSuffix
const listIteratorName = "__itlist__"
const listIteratorInnerName = "__itlist2__"
const minHighVariableName = "__minHigh__"

type 

  Command {.pure.} = enum
    ## All available commands.
    ## 'to' - is a virtual command
    all, combinations, drop, dropWhile, exists, filter, find, flatten, fold, foreach, 
    index, indexedFlatten, indexedMap, indexedReduce, map, reduce, sub, zip, take, takeWhile, to

  ReduceCommand {.pure.} = enum
    ## additional commands that operate as reduce command
    max, min, product, sum, 

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
    hasMinHigh: bool  ## true if the minHigh variable is defined and the loop should use indices rather than iterator
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
  let PARAMETERLESS_CALLS = [$Command.flatten, $Command.indexedFlatten, $Command.combinations].toSet
  let FORCE_SEQ_HANDLERS = [$Command.indexedMap, $Command.flatten, $Command.indexedFlatten, $Command.zip].toSet
  let NEEDS_INDEX_HANDLERS = [$Command.indexedMap, $Command.index, $Command.indexedReduce, $Command.index, $Command.combinations].toSet
  let CANNOT_BE_ALTERED_HANDLERS = [Command.map, Command.indexedMap, Command.combinations, Command.flatten, Command.zip].toSet
  var SEQUENCE_HANDLERS = [$Command.map, $Command.filter, $Command.sub, $Command.combinations, 
                           $Command.drop, $Command.dropWhile, $Command.take, $Command.takeWhile].toSet
  var HANDLERS = [$Command.exists, $Command.all, $Command.index, $Command.fold, $Command.reduce, $Command.foreach, $Command.find].toSet
  SEQUENCE_HANDLERS.incl(FORCE_SEQ_HANDLERS)
  HANDLERS.incl(SEQUENCE_HANDLERS)

## Converts the id-string to the given enum type.
proc toEnum[T:typedesc[enum]](key: string; t:T): auto =
  result = none(t)
  for it in t:
    if $it == key:
      result = some(it)
      break

## Converts the id-string to its Command counterpart.
proc toCommand(key: string): Option[Command] =
  key.toEnum(Command)

## Converts the id-string to its ReduceCommand counterpart.
proc toReduceCommand(key: string): Option[ReduceCommand] =
  if key.startswith("indexed"): 
    return key[7..key.len-1].toLowerAscii().toReduceCommand()
  result = key.toEnum(ReduceCommand)

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

## Shortcut and safe way to get the ident label of a node
proc label(node: NimNode): string = 
  if node.kind == nnkIdent or node.kind == nnkSym:
    return $node
  return ""

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
    var listRef = node[0]
    if listRef.kind == nnkCall and listRef[0].label == $Command.zip:
      listRef = listRef.findNode(nnkPar)[0][0]
    let itIdent = newIdentNode(iteratorVariableName) # the original "it" used in the closure of "map" command
    let idxIdent = newIdentNode(indexVariableName)
    var handlerIdx = 0
    var handler : NimNode = nil
    let comboNode = newIdentNode(combinationsId)
    # the "handler" is the default mapping ``it`` to ``it`` (if "map" is not used)
    var handlerInit : NimNode = nil

    var hasCombination = false
    for child in node:
      if child.len > 0:
        let label = child[0].repr
        let isIndexed = label == $Command.indexedMap
        let isFlatten = label == $Command.flatten 
        let isIndexedFlatten = label == $Command.indexedFlatten
        if label == $Command.combinations: 
          hasCombination = true
        elif label == $Command.map or label == $Command.zip or isIndexed or isFlatten: # zip is part of map and will not be checked
          var params : NimNode = 
            if isFlatten:
              nnkStmtList.newTree().add(newIdentNode(iteratorVariableName))
            elif isIndexedFlatten:
              nnkStmtList.newTree().add(newPar(newIdentNode(indexVariableName),newIdentNode(iteratorVariableName)))
            else:
              child[1].copyNimTree() # params of the map command
          # use / overwrite the last mapping to get the final result type
          let prevHandler = "handler" & $handlerIdx 
          handlerIdx += 1
          handler = newIdentNode("handler" & $handlerIdx)
          if handlerInit == nil:
            handlerInit = nnkStmtList.newTree()
          else:
            # call previous handler for each iterator instance
            params = params.replace(itIdent, parseExpr(prevHandler & "(" & iteratorVariableName & ")"))
          let handlerCall = quote:
            proc `handler`(`itIdent`: auto): auto =
              nil
          var q : NimNode = nil 
          if isFlatten:
            q = quote:
              for it in `params`:
                return it
          elif isIndexedFlatten:
            q = quote:
              for it in `params`:
                return (0,it)
          else:
            var createCombo = nnkStmtList.newTree()
            if hasCombination:
              createCombo = quote:
                var `comboNode` = createCombination([0,0],[0,0]) # could be used as map param
                discard `comboNode`
            var createResult =
              if isIndexed:
                quote:
                  `resultIdent` = (0,`params`)
              else:
                quote:
                  `resultIdent` = `params`
            q = quote:
              var `idxIdent` = 0  # could be used as map param
              `createCombo`
              `createResult`
              discard `idxIdent`
          handlerCall.getStmtList().add(q)
          handlerInit.add(handlerCall)
      
    if handlerInit != nil:
      # we have a handler(-chain): use it to map the result type
      if resultType == nil:
        code = quote:
          `handlerInit`
          `resultIdent` = init_zf(`listRef`,`handler`)
      else:
        let resTypeOk = resType.repr.startswith(resultType)
        if resultType == "seq" or resultType == defaultResultType:
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
proc adapt(node: NimNode, iteratorIndex: int, inFold: bool=false): NimNode {.compileTime.} =
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

## Implementation of the 'map' command.
## Each element of the input is mapped to a given function.
proc inlineMap(ext: ExtNimNode, indexed: bool = false): ExtNimNode {.compileTime.} =
  assert(ext.node.len == 2, "number of parameters for 'map' must be 1! - for tuples use extra brackets.")
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
  let push = if ext.isLastItem: ext.inlineAddElem(ext.prevItNode()) else: nil
  ext.node = quote:
    if `adaptedTest`:
      `push`
  result = ext

## Implementation of the 'flatten' command.
## E.g. @[@[1,2],@[3],@[4,5,6]] --> flatten() == @[1,2,3,4,5,6]
proc inlineFlatten(ext: ExtNimNode, indexed: bool = false): ExtNimNode {.compileTime} =
  let itIdent = ext.itNode()
  let itIdentNew = mkItNode(ext.index+1)
  let itPrevIdent = ext.prevItNode()
  let idxIdent = newIdentNode(indexVariableName)
  let idxFlatten = newIdentNode("idxFlatten")
  let i = quote:
    var `idxFlatten` = -1
  ext.initials.add(i)

  var idxDef = nnkStmtList.newTree() # dummy statements when not using indexed
  var idxIncr = nnkStmtList.newTree()
  var newItIdent = nnkStmtList.newTree()
  var pushItIdent = itIdent
  let idxIdentInner = newIdentNode("__identInner__")
  if indexed:
    idxDef = quote:
      var `idxIdentInner` = -1 
    idxIncr = quote:
      `idxIdentInner` += 1
    newItIdent = quote:
      let `itIdentNew` = (`idxIdentInner`, `itIdent`)
    pushItIdent = itIdentNew
  let push = if ext.isLastItem: ext.inlineAddElem(pushItIdent) else: nil
  ext.node = quote:
    `idxDef`
    for `itIdent` in `itPrevIdent`:
      `idxIncr`
      `idxFlatten` += 1 # re-route idx to overall count of items that are flattened out
      let `idxIdent` = `idxFlatten` # overwrite the outer value locally
      `newItIdent`
      discard(`idxIdent`)
      `push` # push might need idxIdent (at least for array items)
  ext.nextIndexInc = true 
  if indexed:
    ext.index += 1 # double increment
  result = ext

## Implementation of the `takeWhile` command.
## `takeWhile(cond)` : Take all elements as long as the given condition is true.
proc inlineTakeWhile (ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let cond = ext.adapt()
  let push = if ext.isLastItem: ext.inlineAddElem(ext.prevItNode()) else: nnkStmtList.newTree()
  ext.node = quote:
    if not (`cond`):
      break
    `push`
  result = ext

## Implementation of the `take` command.
## `take(count)` : Take `count` elements.
proc inlineTake (ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let idxTake = genSym(nskVar, "__takeIdx__")
  let count = ext.adapt()
  let i = quote:
    var `idxTake` = -1
  let cmp = quote:
    `idxTake` += 1; `idxTake` < `count`
  ext.node = newCall($Command.takeWhile, cmp)
  ext.initials.add(i)
  result = ext.inlineTakeWhile()

## Implementation of the `dropWhile` command.
## `dropWhile(cond)` : drop elements as long the given condition is true. 
## Once the condition gets false, all following elements are used.
proc inlineDropWhile (ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let gate = genSym(nskVar, "__gate__")
  let cond = ext.adapt()
  let i = quote:
    var `gate` = false
  ext.initials.add(i)
  let push = if ext.isLastItem: ext.inlineAddElem(ext.prevItNode()) else: nnkStmtList.newTree()
  ext.node = quote:
    if `gate` or not (`cond`):
      `gate` = true
      `push`
  result = ext

## Implementation of the `drop` command.
## `drop(count)` : drop (or discard) the next `count` elements.
proc inlineDrop (ext: ExtNimNode): ExtNimNode {.compileTime.} =
  let idxDrop = genSym(nskVar, "__dropIdx__")
  let count = ext.adapt()
  let i = quote:
    var `idxDrop` = -1
  let cmp = quote:
    `idxDrop` += 1; `idxDrop` < `count`
  ext.node = newCall($Command.dropWhile, cmp)
  ext.initials.add(i)
  result = ext.inlineDropWhile()

## Implementation of the 'sub' command.
## Creates a list from minIndex til maxIndex (inclusive) - 
## e.g. `a --> sub(0,1)` would contain the first 2 elements of a.
## In sub also Backward indices (e.g. ^1) can be used.
proc inlineSub(ext: ExtNimNode): ExtNimNode {.compileTime.} =
  # sub is re-routed as filter implementation
  let minIndex = ext.node[1]
  if ext.node.len == 2:
    # only one parameter: same as drop
    ext.node = newCall($Command.drop, minIndex)
    result = ext.inlineDrop()
  else:
    var endIndex = ext.node[2]
    if repr(endIndex)[0] == '^':
      let listRef = ext.listRef
      let endIndexAbs = endIndex.last
      endIndex = quote:
        len(`listRef`)-`endIndexAbs` # backwards index only works with collections that have a len

    let idxSub = genSym(nskVar, "__subIdx__")
    let push = if ext.isLastItem: ext.inlineAddElem(ext.prevItNode()) else: nnkStmtList.newTree()
    let i = quote:
      var `idxSub` = -1
    ext.node = quote:
      `idxSub` += 1
      if  `idxSub` >= `minIndex`:
        if `idxSub` > `endIndex`:
          break
        else:
          `push`
    ext.initials.add(i)
    result = ext

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
  var label = ext.node[0].label
  let index = label.startswith("indexed")
  if index:
    ext.needsIndex = true
  let reduceCmd = label.toReduceCommand()
  if reduceCmd != none(ReduceCommand):
    # e.g. sum <=> reduce(sum(it))
    let operation = 
      case reduceCmd.get():
      of ReduceCommand.max:
        quote:
          if `itIdent`[0] > `itIdent`[1]: `itIdent`[0] else: `itIdent`[1]
      of ReduceCommand.min:
        quote:
          if `itIdent`[0] < `itIdent`[1]: `itIdent`[0] else: `itIdent`[1] 
      of ReduceCommand.product:
        quote:
          `itIdent`[0] * `itIdent`[1]
      of ReduceCommand.sum:
        quote:
          `itIdent`[0] + `itIdent`[1]
    ext.node.add(operation)  
  let adaptedExpression = ext.adapt()
  let initAccu = newIdentNode("initAccu")
  let resultIdent = ext.res()
  let i = quote:
    var `initAccu` = true
  ext.initials.add(i)
  
  if index:
    let idxIdent = newIdentNode(indexVariableName)
    # return tuple(idx, accu)
    ext.node = quote:
      if `initAccu`:
        `resultIdent` = (`idxIdent`, `prevIdent`)
        `initAccu` = false
      else:
        let oldValue = `resultIdent`[1]
        let `itIdent` = (oldValue, `prevIdent`)
        let newValue = `adaptedExpression`
        if not (oldValue == newValue): 
          `resultIdent` = (`idxIdent`, newValue)
  else:
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

  if ext.hasMinHigh:
    let idxIdent = newIdentNode(indexVariableName)
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
  let label = if (ext.node.len > 0): ext.node[0].label else: ""
  if ext.node.kind == nnkCall and (ext.index > 0 or label in HANDLERS):
    let cmdCheck = label.toCommand()
    if none(Command) != cmdCheck:
      let cmd = cmdCheck.get()
      if not (label in PARAMETERLESS_CALLS):    
        ext.ensureParameters(1)
      case cmd:
      of Command.zip:
        ext.ensureFirst()
        assert(ext.hasMinHigh, "internal error: 'minHigh' should be defined!")
        return ext.inlineSeq()
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
      of Command.indexedReduce:
        ext.ensureLast()
        return ext.inlineReduce()
      of Command.foreach:
        return ext.inlineForeach()
      of Command.sub:
        return ext.inlineSub()
      of Command.flatten:
        return ext.inlineFlatten()
      of Command.indexedFlatten:
        return ext.inlineFlatten(indexed=true)
      of Command.combinations:
        ext.ensureFirstAfterArrow()
        return ext.inlineCombinations()
      of Command.take:
        return ext.inlineTake()
      of Command.takeWhile:
        return ext.inlineTakeWhile()
      of Command.drop:
        return ext.inlineDrop()
      of Command.dropWhile:
        return ext.inlineDropWhile()
      of Command.to:
        assert(false, "'to' is only allowed as last argument (and will be removed then).")
    else:
      if "any" == label:
        warning("any is deprecated - use exists instead")
        return ext.inlineExists()
      elif (label.toReduceCommand() != none(ReduceCommand)):
        ext.ensureLast()
        return ext.inlineReduce()
      else:
        error("$1 is unknown" % label, ext.node)
  else:
    ext.ensureFirst()
    return ext.inlineSeq()

type
  ## Helper type to allow `[]` access for SomeLinkedList types
  MkListIndexable[T,U,V] = ref object
    listRef: U
    currentIt: V
    currentIdx: Natural
    length: int
  StopIteration* = object of Exception
  
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
    raise newException(StopIteration, "index too large!")
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
          assert(false, "need to provide an own implementation for mkIndexable(" & $`a`.type & ")")
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
        params.add(nnkBracketExpr.newTree(args[0], newIdentNode(indexVariableName)))
        result.add(args[0].wrapIndexable())
      for paramIdx in 1..<zipCmd.len:
        let p = zipCmd[paramIdx]
        if p.kind != nnkInfix and p.kind != nnkBracketExpr and p.label != iteratorVariableName:
          result.add(p.wrapIndexable())
          highList.add(nnkCall.newTree(newIdentNode("high"), p))
          params.add(nnkBracketExpr.newTree(p, newIdentNode(indexVariableName))) # map(it,b[idx],...)
        elif idx > 0:
          params.add(p)
        else:
          assert(false, "No complex arguments allowed in 'zip' operation when used as first command - rather use 'a --> zip(it, ...)'.")

      if idx == 0:
        # set the first arg of zip as the first arg in the chain (e.g. moving `a` to the left of `-->`)
        args.insert(0, zipCmd[1]) # a --> ...
        idx = 1

      args[idx] = newCall($Command.map, params)
    idx += 1
  if highList.len > 0:
    let minHigh = newIdentNode(minHighVariableName)
    let i = quote:
      let `minHigh`= min(`highList`)
    result.add(i)

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
                defaultResultType # default to sequence - and use it if isSeq is used explicitly
  if resultType == nil and td == "enum":
    resultType = "seq[" & $args[0] & "]" & implicitTypeSuffix
  result = resultType

## Main function that creates the outer function call.
proc iterHandler(args: NimNode, debug: bool, td: string): NimNode {.compileTime.} =
  let resultType = args.checkTo(td) 
  let preInit = args.replaceZip() # zip is replaced with map + filter
  let hasMinHigh = preInit.len > 0
  let lastCall = args.last[0].label
  let isSeq = lastCall in SEQUENCE_HANDLERS
  var defineIdxVar = not hasMinHigh
  var needsIndexVar = true

  if not isSeq or not (resultType != nil and resultType.startswith("array") or (resultType == nil and td.startswith("array"))): 
    if nil == args.findNode(nnkIdent, indexVariableName):  
      needsIndexVar = true

  if not defineIdxVar:
    # check if 'var idx' has to be created or not - and 'idx += 1' to be added
    for arg in args:
      if arg.kind == nnkCall:
        let label = arg[0].label
        if label in NEEDS_INDEX_HANDLERS:
          defineIdxVar = true

  var code: NimNode
  let needsFunction = (lastCall != $Command.foreach)
  if needsFunction:
    result = args.createAutoProc(isSeq, resultType, td)
    code = result.last.getStmtList() 
    #result.last.insert(1, preInit)
    if preInit.len > 0:
      code.insert(0, preInit)
    result = nnkCall.newTree(result)
  else:
    # there is no extra function, but at least we have an own section here - preventing double definitions
    var q = quote:
      if true:
        `preInit`
        nil
    result = q
    code = q.getStmtList()
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

  var commands: seq[Command] = @[]
  for arg in args:
    let isLast = arg == args.last
    
    if arg.kind == nnkCall:
      let label = arg[0].label
      let cmdCheck = label.toCommand()
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
                      needsIndex: needsIndexVar,
                      hasMinHigh: hasMinHigh,
                      nextIndexInc: false).inlineElement()
    let newCode = ext.node.getStmtList()
    code.add(ext.node)
    if startNode == nil:
      startNode = ext.node
    if newCode != nil:
      code = newCode
    if not hasMinHigh and not defineIdxVar:
      defineIdxVar = ext.needsIndex
    if ext.nextIndexInc:
      index = ext.index + 1

  if not hasMinHigh and defineIdxVar:
    let idxIdent = newIdentNode(indexVariableName)
    let identDef = quote:
      var `idxIdent` = 0 
    varDef.add(identDef)

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
  
## general macro to invoke all available zero_functional functions
macro `-->`*(a: untyped, b: untyped): untyped =
  let (a,b,debug) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    if not debug: # using debug (or `debug`) directly does not work (?!)
      result = quote:
        delegateArrow(type(`a`), `a`, `b`, false)
    else:
      result = quote:
        delegateArrow(type(`a`), `a`, `b`, true)
  else:
    result = delegateMacro(a, b, debug, "seq")

## use this macro for debugging - will output the created code
macro `-->>`*(a: untyped, b: untyped): untyped =
  let (a,b,_) = checkArrow(a,b,"-->")
  if a.kind == nnkIdent:
    result = quote:
      delegateArrow(type(`a`), `a`, `b`, true)
  else:
    result = delegateMacro(a, b, true, "seq")

