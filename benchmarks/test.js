var fs = require('fs');
var _ = require('lodash');

function f(a, b) {
  return a + b
}

async function run() {
  fs.readFile('data.txt', 'utf8', function(err, data) {
    var a = _.map(data.split(' '), parseInt)
    var b = []
    for (var z = 0; z < 200000; z += 1) {
      b.push(z)
    }

    console.time('example0')
    var n = _.zip(a, b).
                map((it) => f(it[0], it[1])).
                filter((it) => it % 4 > 1).
                map((it) => it * 2).
                every((it) => it > 4)
    console.timeEnd('example0')

    console.time('example1')
    var o = _.map(a, (it) => f(it, it)).
                map((it) => it - 7).
                reduce((memo, x) => memo + x)
    console.timeEnd('example1')
  });
}

run()


// def example1
//   o = $a.
//         map { |it| f(it, it) }.
//         map { |it| it - 7 }.
//         inject { |it, memo| it + memo }
// end

// puts Benchmark.measure { example0 }
// puts Benchmark.measure { example1 }
