import times, os, strutils, sequtils, macros

template finish(benchmarkName: string, a: NimNode): untyped =
  let elapsed = (epochTime() - `a`) * 1000
  let elapsedStr = elapsed.formatFloat(format = ffDecimal, precision = 3)
  echo "$1 $2" % [benchmarkName, elapsedStr]

macro benchmark*(benchmarkName: static[string], code: untyped): untyped =
  var a = newIdentNode("c$1" % benchmarkName) # WORKS FOR MY SITUATION
  result = nnkStmtList.newTree()
  var empty = newEmptyNode()
  result.add(nnkVarSection.newTree(
    nnkIdentDefs.newTree(
      a,
      empty,
      nnkCall.newTree(
        newIdentNode("epochTime")))))
  result.add(code)
  result.add(getAst(finish(benchmarkName, a)))
  # echo repr(result)

proc echoBenchmark*[T](benchmarkName: static[string], call: proc (): T) =
  if true:
    var res: T.type
    benchmark benchmarkName:
      res = call()
    echo res

macro callBench*(a: untyped): untyped =
  quote:
    echoBenchmark(`a`.repr, `a`)
