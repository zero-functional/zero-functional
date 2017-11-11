package main

import "time"
import "fmt"
import "io/ioutil"
import "strings"
import "strconv"

type Pair struct {
  first int
  second int
}

func Zip(a []int, b []int) []Pair {
  vsz := make([]Pair, 0)
  for z := 0; z < len(a); z += 1 {
    vsz = append(vsz, Pair{first: a[z], second: b[z]})
  }
  return vsz
}

func Any(vs []int, f func(int) bool) bool {
    for _, v := range vs {
        if f(v) {
            return true
        }
    }
    return false
}

func All(vs []int, f func(int) bool) bool {
    for _, v := range vs {
        if !f(v) {
            return false
        }
    }
    return true
}

func Filter(vs []int, f func(int) bool) []int {
    vsf := make([]int, 0)
    for _, v := range vs {
        if f(v) {
            vsf = append(vsf, v)
        }
    }
    return vsf
}
  

func Map(vs []int, f func(int) int) []int {
    vsm := make([]int, len(vs))
    for i, v := range vs {
        vsm[i] = f(v)
    }
    return vsm
}

func MapPair(vs []Pair, f func(Pair) int) []int {
    vsm := make([]int, len(vs))
    for i, v := range vs {
        vsm[i] = f(v)
    }
    return vsm
}

func Foldl(vs []int, f func(int, int) int) int {
  vsf := 0
  for _, v := range vs {
    vsf = f(vsf, v)
  }
  return vsf
}


func f(a int, b int) int {
  return a + b
}


func main() {
  data, _ := ioutil.ReadFile("data.txt")
  var as = strings.Split(string(data), " ")

  a := make([]int, 2000000)
  b := make([]int, 2000000)

  for z := 0; z < 2000000; z += 1 {
    r, _ := strconv.ParseInt(as[z], 10, 8)
    a[z] = int(r)
    b[z] = z
  }


  start := time.Now().UnixNano() / int64(time.Millisecond)

  All(
        Map(
          Filter(
            MapPair(
              Zip(a, b),
              func(it Pair) int { return f(it.first, it.second) }),
            func(it int) bool { return it % 4 > 1 }),
          func(it int) int { return it * 2 }),
        func(it int) bool { return it > 4 })

  finish := time.Now().UnixNano() / int64(time.Millisecond)

  fmt.Println("example0 ", finish - start)

  start = time.Now().UnixNano() / int64(time.Millisecond)

  Foldl(
        Map(
          Map(
            a,
            func(it int) int { return f(it, it) }),
          func(it int) int { return it - 7 }),
        func(memo int, it int) int { return memo + it })

  finish = time.Now().UnixNano() / int64(time.Millisecond)

  fmt.Println("example1", finish - start)
}
