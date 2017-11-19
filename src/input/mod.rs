pub use super::types;

pub struct Input {
  rows: [types::Byte; 2],
  column: types::Byte,
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

  pub fn read(&self, address: types::Word) -> types::Byte {
    self.column
  }

  pub fn write(&mut self, address: types::Word, data: types::Byte) {
    self.column = data;
    self.update();
  }

  pub fn update(&mut self) {
    self.column &= 0x30;
    if self.column & 0x10 == 0x10 { self.column |= self.rows[0]; }
    if self.column & 0x20 == 0x20 { self.column |= self.rows[1]; }
  }
}
