pub use super::types;

pub struct Input {
  rows: [types::Byte; 2],
  column: types::Byte,
}

impl Input {
  pub fn new() -> Input {
    Input {
      rows: [0x0F, 0x0F],
      column: 0x00,
    }
  }

  pub fn read(&self, address: types::Word) -> types::Byte {
    match self.column {
      0x10 => {
        println!("Input, read via 0x10");
        self.rows[0]
      },
      0x20 => {
        println!("Input, read via 0x20");
        self.rows[1]
      },
      _ => {
        println!("Input, 0x00");
        0x00
      }
    }
  }

  pub fn write(&mut self, address: types::Word, data: types::Byte) {
    self.column = data & 0x30;
  }
}
