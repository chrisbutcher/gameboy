extern crate time;

pub struct Counter {
  frames: i32,
  last_time: u64,
  current_frame_rate: u16,
}

impl Counter {
  pub fn new() -> Counter {
    Counter { frames: 0, last_time: 0, current_frame_rate: 0 }
  }

  pub fn print_fps(&mut self) {
    let current_time = time::precise_time_ns();
    self.frames += 1;
    if current_time - self.last_time >= 1000000000 {
      println!("fps: {}", self.frames);
      self.frames = 0;
      self.last_time = time::precise_time_ns();
    }
  }
}
