require 'benchmark'

data = File.read('data.txt')

$a = data.split.map(&:to_i)
$b = (0...2_000_000).to_a

def f(a, b)
  a + b
end

def example0
  n = $a.zip($b).
        map { |x, y| f(x, y) }.
        select { |it| it % 4 > 1 }.
        map { |it| it * 2 }.
        all? { |it| it > 4 }
end

def example1
  o = $a.
        map { |it| f(it, it) }.
        map { |it| it - 7 }.
        inject { |it, memo| it + memo }
end

puts Benchmark.measure { example0 }
puts Benchmark.measure { example1 }
