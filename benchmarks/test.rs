use std::fs::File;
use std::io::prelude::*;
use std::time::Instant;

fn f(a: u8, b: u8) -> u8 {
  a + b
}

fn main() {
  let mut f2 = File::open("data.txt").expect("file not found");

  let mut data = String::new();
  f2.read_to_string(&mut data)
      .expect("something went wrong reading the file");

  let a: Vec<u8> = data.split(" ").map(|s| s.parse::<u8>().unwrap()).collect();
  let b: Vec<u32> = (0..2_000_000).collect();

  let mut start = Instant::now();

  let n = a.iter().zip(b).
        map(|(x, y)| (*x as u32) + y).
        filter(|it| it % 4 > 1).
        map(|it| it * 2).
        all(|it| it > 4);

  let mut finish = start.elapsed();
  println!("example0 {}", (finish.as_secs() * 1_000) + (finish.subsec_nanos() / 1_000_000) as u64);
  println!("{}", n);

  start = Instant::now();

  let o = a.iter().
        map(|it| f(*it, *it)).
        map(|it| (it as i8) - 7).
        fold(0i64, |memo, it| memo + (it as i64));

  finish = start.elapsed();
  println!("example1 {}", (finish.as_secs() * 1_000) + (finish.subsec_nanos() / 1_000_000) as u64);
  println!("{}", o);
}
