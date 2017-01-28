use std::io::prelude::*;
use std::fs::File;

pub use super::types;
pub use super::cartridge;

const MEMORY_SIZE: usize = 0x10000;

pub struct MMU {
  pub buffer: Vec<types::Byte>,
  pub cartridge: cartridge::Cartridge
  // Switches banks via the MBC (memory bank controller)
}

impl MMU {
  pub fn new() -> MMU {
    MMU {
      buffer: vec![0x00; MEMORY_SIZE],
      cartridge: cartridge::Cartridge::new()
    }
  }

  pub fn load_game(&mut self, rom_filename: &str) {
    let mut f = File::open(rom_filename).unwrap();
    f.read(&mut self.cartridge.buffer);
  }

  pub fn read(&self, address: types::Word) -> types::Byte {
    match address {
      0x0000 ... 0x7FFF => self.cartridge.buffer[address as usize],
      0x8000 ... 0xFFFF => self.buffer[address as usize],
      // 0xA000 ... 0xC000 => self.buffer[address],
      // 0xC000 ... 0xFFFF => self.buffer[address],
      _ => 0x0000
    }
  }

  pub fn read_word(&self, address: types::Word) -> types::Word {
    let lo_byte = self.read(address);
    let hi_byte = self.read(address + 1);
    let word = ((hi_byte as types::Word) << 8) | lo_byte as types::Word;
    // println!("hi_byte {:x}", hi_byte);
    // println!("lo_byte {:x}", lo_byte);
    // println!("word {:x}", word);

    word
  }

  pub fn write(&mut self, address: types::Word, data: types::Byte) {
    // Disallow writes to restricted memory regions
    if address < 0x8000 || (address >= 0xFEA0 && address < 0xFEFF) {
      return
    } else if address >= 0xE000 && address < 0xFE00 {
      let echo_ram_offset = 0x2000;
      self.write(address - echo_ram_offset, data)
    }

    self.buffer[address as usize] = data;
  }

  pub fn num_rom_banks(&self) -> types::Byte {
    self.read(0x147)
  }

  pub fn num_ram_banks(&self) -> types::Byte {
    self.read(0x148)
  }
}

#[test]
fn can_write_to_memory_in_allowed_regions() {
  let mut mmu = MMU::new();
  mmu.write(0xC000, 0xFF);
  assert_eq!(mmu.read(0xC000), 0xFF);
  mmu.write(0x8000, 0xFF);
  assert_eq!(mmu.read(0x8000), 0xFF);
}


#[test]
fn cannot_write_to_memory_in_disallowed_regions() {
  let mut mmu = MMU::new();
  mmu.write(0x0000, 0xFF);
  assert_eq!(mmu.read(0x0000), 0x00);
  mmu.write(0x7FFF, 0xFF);
  assert_eq!(mmu.read(0x7FFF), 0x00);
}
