extern crate time;

pub struct Counter {
  frames: i32,
  last_time: time::Instant,
  print_fps: bool,
}

impl Counter {
  pub fn new(print_fps: bool) -> Counter {
    Counter {
      frames: 0,
      last_time: time::Instant::now(),
      print_fps: print_fps,
    }
  }

  pub fn print_fps(&mut self) {
    let current_time = time::Instant::now();
    self.frames += 1;
    if (current_time - self.last_time) >= time::Duration::seconds(1) {
      if self.print_fps {
        println!("fps: {}", self.frames);
      }

      self.frames = 0;
      self.last_time = time::Instant::now();
    }
  }
}
