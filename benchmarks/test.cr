require "benchmark"

DATA = File.read("data.txt")

A = DATA.split.map { |child| child.to_i }
B = (0...2_000_000).to_a

def f(a, b)
  a + b
end

def example0
  n = A.zip(B)
       .map { |x, y| f(x, y) }
       .select { |it| it % 4 > 1 }
       .map { |it| it * 2 }
       .all? { |it| it > 4 }
  n
end

def example1
  o = A
    .map { |it| f(it, it) }
    .map { |it| it - 7 }
    .reduce { |it, memo| it + memo }
  o > 0
end

def run
  res = false
  puts Benchmark.measure { res = example0 }
  puts res
  puts Benchmark.measure { res = example1 }
  puts res
end

run
