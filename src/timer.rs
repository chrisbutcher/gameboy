pub enum Speed {
  Hz4096,
  Hz262144,
  Hz65536,
  Hz16384
}

pub struct Timer {
  pub interrupt_flags: u8,

  divider: u8,
  counter: u8,
  modulo: u8,
  counter_speed: Speed,
  running: bool,

  internal_divider: u32,
  internal_counter: u32,
}

impl Timer {
  pub fn new() -> Timer {
    Timer {
      interrupt_flags: 0x00,

      divider: 0x00,
      counter: 0x00,
      modulo: 0x00,
      counter_speed: Speed::Hz16384,
      running: false,

      internal_divider: 0x00,
      internal_counter: 0x00,
    }
  }

  pub fn read(&self, address: u16) -> u8 {
    match address {
      0xFF04 => { self.divider },
      0xFF05 => { self.counter },
      0xFF06 => { self.modulo },
      0xFF07 => {
        (if self.running { 0b0100 } else { 0x00 }) | self.counter_speed_to_binary()
      },
      _ => { panic!("Invalid address in Timer#read") }
    }
  }

  pub fn write(&mut self, address: u16, value: u8){
    match address {
      0xFF04 => { self.divider = 0 },
      0xFF05 => { self.counter = value },
      0xFF06 => { self.modulo = value },
      0xFF07 => {
        self.running = value & 0b0100 != 0x00;

        self.counter_speed = match value & 0b0011 {
          0b00 => Speed::Hz4096,
          0b01 => Speed::Hz262144,
          0b10 => Speed::Hz65536,
          0b11 => Speed::Hz16384,
          _ => panic!("Invalid value in Timer#write")
        }
      },
      _ => { panic!("Invalid address in Timer#write") }
    }
  }

  pub fn tick(&mut self, cycles: i32) {
    let cycles = cycles as u32;

    self.internal_divider += cycles;

    while self.internal_divider >= self.counter_speed_to_dividend(&Speed::Hz16384) {
      self.divider = self.divider.wrapping_add(1);
      self.internal_divider -= self.counter_speed_to_dividend(&Speed::Hz16384);
    }

    if self.running {
      self.internal_counter += cycles;

      while self.internal_divider >= self.counter_self_speed_to_dividend() {
        self.counter = self.counter.wrapping_add(1);
        if self.counter == 0 {
          self.counter = self.modulo;
          self.interrupt_flags |= 0x04;
        }
        self.internal_counter -= self.counter_self_speed_to_dividend();
      }
    }
  }

  fn counter_speed_to_binary(&self) -> u8 {
    match self.counter_speed {
      Speed::Hz4096 => 0b00,
      Speed::Hz262144 => 0b01,
      Speed::Hz65536 => 0b10,
      Speed::Hz16384 => 0b11
    }
  }

  fn counter_self_speed_to_dividend(&self) -> u32 {
    self.counter_speed_to_dividend(&self.counter_speed)
  }

  fn counter_speed_to_dividend(&self, speed: &Speed) -> u32 {
    match *speed {
      Speed::Hz4096 => 1024,
      Speed::Hz262144 => 16,
      Speed::Hz65536 => 64,
      Speed::Hz16384 => 256
    }
  }
}
