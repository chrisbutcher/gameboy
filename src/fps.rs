extern crate time;

pub struct Counter {
  frames: i32,
  last_time: u64,
  print_fps: bool,
}

impl Counter {
  pub fn new(print_fps: bool) -> Counter {
    Counter { frames: 0, last_time: 0, print_fps: print_fps }
  }

  pub fn print_fps(&mut self) {
    let current_time = time::precise_time_ns();
    self.frames += 1;
    if current_time - self.last_time >= 1000000000 {
      if self.print_fps {
        println!("fps: {}", self.frames);
      }

      self.frames = 0;
      self.last_time = time::precise_time_ns();
    }
  }
}
