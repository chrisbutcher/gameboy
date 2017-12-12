pub struct Input {
  pub interrupt_flags: u8,

  rows: [u8; 2],
  column: u8,
}

#[derive(Debug)]
pub enum Button {
  Up,
  Down,
  Left,
  Right,
  A,
  B,
  Select,
  Start
}

impl Input {
  pub fn new() -> Input {
    let mut input = Input {
      interrupt_flags: 0x00,

      rows: [0x0F, 0x0F],
      column: 0x00,
    };
    input.update();
    input
  }

  pub fn read(&self) -> u8 {
    self.column
  }

  pub fn write(&mut self, data: u8) {
    self.column = data;
    self.update();
  }

  pub fn key_pressed(&mut self, button: Button) {
    println!("Button pressed! {:?}", button);
    match button {
      Button::Start => {
        self.rows[0] &= !0b0000_1000;
      },
      _ => {}
    }

    self.interrupt_flags |= 0x10;
    self.update();
  }

  pub fn key_released(&mut self, button: Button) {
    match button {
      Button::Start => {
        self.rows[0] |= 0b0000_1000;
      },
      _ => {}
    }

    self.update();
  }

  fn update(&mut self) {
    self.column &= 0x30;
    if self.column & 0x10 == 0x10 { self.column |= self.rows[0]; }
    if self.column & 0x20 == 0x20 { self.column |= self.rows[1]; }
  }
}
