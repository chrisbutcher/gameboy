use std::fs::File;
use std::io::{Read};

const MAX_BOOTROM_SIZE: usize = 0x100;

pub struct Bootrom {
  pub buffer: [u8; MAX_BOOTROM_SIZE] // TODO make private, or delete?
}

impl Bootrom {
  pub fn new() -> Bootrom {
    let mut file = File::open("dmg_boot.bin").unwrap();
    let mut buffer = [0x00; MAX_BOOTROM_SIZE];
    file.read_exact(&mut buffer).unwrap();
    // Bootrom::from_data(data);

    Bootrom {
      buffer: buffer
    }
  }
}
