const MAX_CARTRIDGE_SIZE: usize = 0x200000;

pub struct Cartridge {
  pub buffer: Vec<u8>
}

impl Cartridge {
  pub fn new() -> Cartridge {
    Cartridge {
      buffer: vec![0x00; MAX_CARTRIDGE_SIZE]
    }
  }
}
