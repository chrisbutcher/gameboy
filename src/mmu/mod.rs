use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;
use std::cell::RefCell;
use std::rc::Rc;

pub use super::types;
pub use super::bootrom;
pub use super::cartridge;
pub use super::ppu;
pub use super::input;

pub struct MMU {
  pub bootrom: bootrom::Bootrom,
  pub bootroom_active: bool,

  pub cartridge: cartridge::Cartridge, // 0000-7fff
  // GPU's video ram                      8000-9FFF
  pub external_ram: Vec<types::Byte>, // A000-BFFF
  pub work_ram: Vec<types::Byte>, // C000-DFFF, with E000-FDFF shadow
  pub sprite_info: Vec<types::Byte>, // FE00-FE9F
  pub io: Vec<types::Byte>, // FF00-FF7F
  pub zram: Vec<types::Byte>, // FF80-FFFF (zero page ram)

  pub ppu: RefCell<ppu::PPU>,
  pub input: input::Input,

  // Switches banks via the MBC (memory bank controller)
  pub InterruptEnabled: types::Byte,
  pub InterruptFlags: types::Byte,
}

impl MMU {
  pub fn new() -> MMU {
    MMU {
      bootrom: bootrom::Bootrom::new(),
      bootroom_active: true,
      cartridge: cartridge::Cartridge::new(),
      external_ram: vec![ 0x00; 0x2000 ],
      work_ram: vec![ 0x00; 0x2000 ],
      sprite_info: vec![ 0x00; 0x100 ],
      io: vec![ 0x00; 0x100 ],
      zram: vec![ 0x00; 0x80 ],

      ppu: RefCell::new(ppu::PPU::new()),
      input: input::Input::new(),

      InterruptEnabled: 0x00,
      InterruptFlags: 0x00,
    }
  }

  pub fn load_game(&mut self, rom_filename: &str) {
    let mut f = File::open(rom_filename).unwrap();
    f.read(&mut self.cartridge.buffer);
  }

  pub fn read(&self, address: types::Word) -> types::Byte {
    let result = match address {
      0x0000...0x7FFF => {
        // if address <= 0x0100 && self.bootroom_active {
        //   self.bootrom.buffer [ address as usize ]
        // } else {
          self.cartridge.buffer[ address as usize ]
        // }
      },
      0x8000...0x9FFF => {
        debug!("MMU#read from PPU.video_ram");
        self.ppu.borrow_mut().video_ram[ address as usize - 0x8000 ]
      }
      0xA000...0xBFFF => {
        debug!("MMU#read from external_ram");
        self.external_ram[ address as usize - 0xA000 ]
      }
      0xC000...0xDFFF => {
        debug!("MMU#read from work_ram");
        self.work_ram[ address as usize - 0xC000 ]
      }
      0xE000...0xFDFF => {
        debug!("MMU#read from work_ram");
        self.work_ram[ address as usize - 0xC000 - 0x2000 ]
      } // ECHO work ram
      0xFE00...0xFE9F => {
        debug!("MMU#read from sprite_info");
        self.sprite_info[ address as usize - 0xFE00 ]
      }
      0xFF00 => {
        debug!("MMU#read from input");
        self.input.read(address)
      }
      0xFF01...0xFF0E => {
        debug!("MMU#read from io");
        self.io[ address as usize - 0xFF00 ]
      }
      0xFF0F => {
        self.InterruptFlags
      }
      0xFF10...0xFF3F => {
        debug!("MMU#read from io");
        self.io[ address as usize - 0xFF00 ]
      }
      addr @ 0xFF40...0xFF7F => {
        match addr {
          0xFF40...0xFF7F => {
            let result = self.ppu.borrow_mut().read(addr);
            result
          },
          _ => self.io[ addr as usize - 0xFF00 ],
        }
      }
      0xFF80...0xFFFE => {
        debug!("MMU#read from zram");
        self.zram[ address as usize - 0xFF80 ]
      }
      0xFFFF => {
        debug!("Read from InterruptEnabled");
        self.InterruptEnabled
      } // TODO Should return self.InterruptEnabled
      _ => {
        panic!("Memory access is out of bounds: {:#X}", address);
      }
    };

    result
  }

  pub fn read_word(&self, address: types::Word) -> types::Word {
    let lo_byte = self.read(address);
    let hi_byte = self.read(address + 1);
    ((hi_byte as types::Word) << 8) | lo_byte as types::Word
  }

  pub fn write_word(&mut self, address: types::Word, data: types::Word) {
    let lo_data = (0x00FF & data) as types::Byte;
    let hi_data = ((0xFF00 & data) >> 8) as types::Byte;
    self.write(address + 1, hi_data);
    self.write(address, lo_data);
  }

  pub fn write(&mut self, address: types::Word, data: types::Byte) {
    match address {
      0x0000...0x7FFF => {
        debug!("Writing to disallowed memory region: {:#X}", address);
      } // no-op
      0x8000...0x9FFF => {
        debug!("MMU#write to PPU.video_ram");
        let mut borrowed_ppu = self.ppu.borrow_mut();
        borrowed_ppu.video_ram[ address as usize - 0x8000 ] = data;

        if address <= 0x97FF {
          borrowed_ppu.update_tile(address, data)
        }
      }
      0xA000...0xBFFF => {
        debug!("MMU#write to external_ram");
        self.external_ram[ address as usize - 0xA000 ] = data
      }
      0xC000...0xDFFF => {
        debug!("MMU#write to work_ram");
        self.work_ram[ address as usize - 0xC000 ] = data
      }
      0xE000...0xFDFF => self.write(address - 0xC000 - 0x2000, data), // ECHO work ram
      0xFE00...0xFE9F => {
        debug!("MMU#write to sprite_info");
        self.sprite_info[ address as usize - 0xFE00 ] = data
      }
      0xFEA0...0xFEFF => {
        debug!("Writing to disallowed memory region: {:#X}", address);
      } // no-op
      0xFF00 => {
        self.input.write(address, data)
      }
      0xFF01...0xFF0E => {
        debug!("MMU#write to io");
        self.io[ address as usize - 0xFF00 ] = data
      }
      0xFF0F => {
        self.InterruptFlags = data;
      }
      0xFF10...0xFF3F => {
        debug!("MMU#write to io");
        self.io[ address as usize - 0xFF00 ] = data
      }
      0xFF40...0xFF7F => {
        if address == 0xFF40 {
          // NOTE LCD powering on
          // panic!("hi");
        }

        if address == 0xFF50 {
          self.bootroom_active = false;
        }

        debug!("MMU#write to ppu");
        self.ppu.borrow_mut().write(address, data)
        // self.io[address as usize - 0xFF00] = data
      }
      0xFF80...0xFFFE => {
        debug!("MMU#write to zram");
        self.zram[ address as usize - 0xFF80 ] = data
      }
      0xFFFF => {
        self.InterruptEnabled = data;
      }
      _ => {
        panic!("Memory access is out of bounds: {:#X}", address);
      }
    }
  }

  pub fn num_rom_banks(&self) -> types::Byte {
    self.read(0x147)
  }

  pub fn num_ram_banks(&self) -> types::Byte {
    self.read(0x148)
  }
}

// #[test]
// fn can_write_to_memory_in_allowed_regions() {
//   let mut mmu = MMU::new();
//   mmu.write(0xC000, 0xFF);
//   assert_eq!(mmu.read(0xC000), 0xFF);
//   mmu.write(0x8000, 0xFF);
//   assert_eq!(mmu.read(0x8000), 0xFF);
// }

// #[test]
// fn cannot_write_to_memory_in_disallowed_regions() {
//   let mut mmu = MMU::new();
//   mmu.write(0x0000, 0xFF);
//   assert_eq!(mmu.read(0x0000), 0x00);
//   mmu.write(0x7FFF, 0xFF);
//   assert_eq!(mmu.read(0x7FFF), 0x00);
// }
