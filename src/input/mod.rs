pub struct Input {
  rows: [u8; 2],
  column: u8,
}

impl Input {
  pub fn new() -> Input {
    let mut input = Input {
      rows: [0x0F, 0x0F],
      column: 0x00,
    };
    input.update();
    input
  }

  pub fn read(&self, _address: u16) -> u8 {
    self.column
  }

  pub fn write(&mut self, _address: u16, data: u8) {
    self.column = data;
    self.update();
  }

  fn update(&mut self) {
    self.column &= 0x30;
    if self.column & 0x10 == 0x10 { self.column |= self.rows[0]; }
    if self.column & 0x20 == 0x20 { self.column |= self.rows[1]; }
  }
}
