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
  speed: Speed,
  stopped: bool,

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
      speed: Speed::Hz16384,
      stopped: false,

      internal_divider: 0x00,
      internal_counter: 0x00,
    }
  }

  pub fn read(&self, address: u16) -> u8 {
    match address {
      0xFF04 => { self.divider },
      0xFF05 => { self.counter },
      0xFF06 => { self.modulo },
      0xFF07 => { 0x00 },
      _ => { panic!("Invalid address in Timer#read") }
    }
  }

  pub fn write(&mut self, address: u16, value: u8){
  }
}
