pub use super::types;

const MAX_CARTRIDGE_SIZE: usize = 0x200000;

pub struct Cartridge {
  pub buffer: Vec<types::Byte>
}

impl Cartridge {
  pub fn new() -> Cartridge {
    Cartridge {
      buffer: vec![0x00; MAX_CARTRIDGE_SIZE]
    }
  }
}
