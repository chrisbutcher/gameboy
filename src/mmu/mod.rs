use std::io::prelude::*;
use std::fs::File;

pub use super::types;
pub use super::cartridge;

pub struct MMU {
  pub cartridge: cartridge::Cartridge, // 0000-7fff
  pub video_ram: Vec<types::Byte>,     // 8000-9FFF
  pub external_ram: Vec<types::Byte>,  // A000-BFFF
  pub work_ram: Vec<types::Byte>,      // C000-DFFF, with E000-FDFF shadow
  pub sprite_info: Vec<types::Byte>,   // FE00-FE9F
  pub io: Vec<types::Byte>,            // FF00-FF7F
  pub zram: Vec<types::Byte>,          // FF80-FFFF (zero page ram)

  // Switches banks via the MBC (memory bank controller)
}

impl MMU {
  pub fn new() -> MMU {
    MMU {
      cartridge: cartridge::Cartridge::new(), // 0000-7fff
      video_ram: vec![0x00; 0x2000],
      external_ram: vec![0x00; 0x2000],
      work_ram: vec![0x00; 0x2000],
      sprite_info: vec![0x00; 0x100],
      io: vec![0x00; 0x100],
      zram: vec![0x00; 0x80],
    }
  }

  pub fn load_game(&mut self, rom_filename: &str) {
    let mut f = File::open(rom_filename).unwrap();
    f.read(&mut self.cartridge.buffer);
  }

  pub fn read(&self, address: types::Word) -> types::Byte {
    match address {
      0x0000 ... 0x7FFF => { println!("MMU#read from cartridge"); self.cartridge.buffer[address as usize] },
      0x8000 ... 0x9FFF => { println!("MMU#read from video_ram"); self.video_ram[address as usize - 0x8000] },
      0xA000 ... 0xBFFF => { println!("MMU#read from external_ram"); self.external_ram[address as usize - 0xA000] },
      0xC000 ... 0xDFFF => { println!("MMU#read from work_ram"); self.work_ram[address as usize - 0xC000] },
      0xE000 ... 0xFDFF => { println!("MMU#read from work_ram"); self.work_ram[address as usize - 0xE000 - 2000] }, // ECHO work ram
      0xFE00 ... 0xFE9F => { println!("MMU#read from sprite_info"); self.sprite_info[address as usize - 0xFE00] },
      0xFF00 ... 0xFF7F => { println!("MMU#read from io"); self.io[address as usize - 0xFF00] },
      0xFF80 ... 0xFFFF => { println!("MMU#read from zram"); self.zram[address as usize - 0xFF80] },
      _ => { panic!("Memory access is out of bounds: {:#X}", address); }
    }
  }

  pub fn read_word(&self, address: types::Word) -> types::Word {
    let lo_byte = self.read(address);
    let hi_byte = self.read(address + 1);
    ((hi_byte as types::Word) << 8) | lo_byte as types::Word
  }

  pub fn write(&mut self, address: types::Word, data: types::Byte) {
    // println!("Writing {:#X}, with {:#X}", address, data);

    match address {
      0x0000 ... 0x7FFF => { panic!("Writing to disallowed memory region: {:#X}", address); }, // no-op
      0x8000 ... 0x9FFF => { println!("MMU#write to video_ram"); self.video_ram[address as usize - 0x8000] = data },
      0xA000 ... 0xBFFF => { println!("MMU#write to external_ram"); self.external_ram[address as usize - 0xA000] = data },
      0xC000 ... 0xDFFF => { println!("MMU#write to work_ram"); self.work_ram[address as usize - 0xC000] = data },
      0xE000 ... 0xFDFF => { let echo_ram_offset = 0x2000; self.write(address - echo_ram_offset, data) }, // ECHO work ra }m
      0xFE00 ... 0xFE9F => { println!("MMU#write to sprite_info"); self.sprite_info[address as usize - 0xFE00] = data },
      0xFEA0 ... 0xFEFF => { panic!("Writing to disallowed memory region: {:#X}", address); }, // no-op
      0xFF00 ... 0xFF7F => { println!("MMU#write to io"); self.io[address as usize - 0xFF00] = data },
      0xFF80 ... 0xFFFF => { println!("MMU#write to zram"); self.zram[address as usize - 0xFF80] = data },
      _ => { panic!("Memory access is out of bounds: {:#X}", address); }
    }
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
