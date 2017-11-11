using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using System.IO;

public class Program
{ 
    static int f(int a, int b) {
      return a + b;
    }

    public static void Main()
    { 

        string data = File.ReadAllText("data.txt");
        var s1 = data.Split(' ').Select(child => Int32.Parse(child));
        var s2 = Enumerable.Range(0, 2000000);

        // based on http://jonskeet.uk/csharp/benchmark.html by jon skeet
        
        // Give the test as good a chance as possible
        // of avoiding garbage collection
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        Int64 timings = 0;
        int MAX = 50;

        for (var z = 0; z < MAX; z += 1) {
          var a = System.Diagnostics.Stopwatch.StartNew();

          var n = s1.Zip(s2, (int x, int y) => f(x, y)).
                    Where(it => it % 4 > 1).
                    Select(it => it * 2).
                    All(it => it > 4);

          a.Stop();

          timings += a.ElapsedMilliseconds;
        }

        Console.WriteLine("example0 {0}", timings / MAX);

        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        timings = 0;

        for (var z = 0; z < MAX; z += 1) {
          var a = System.Diagnostics.Stopwatch.StartNew();
        
          var o = s1.
              Select(it => f(it, it)).
              Select(it => it - 7).
              Aggregate(0, (memo, it) => it + memo);

          a.Stop();
          
          timings += a.ElapsedMilliseconds;
        }

        Console.WriteLine("example1 {0}", timings / MAX);
    

    }
}
