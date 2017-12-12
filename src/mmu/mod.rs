use std::io::prelude::*;
use std::fs::File;
use std::cell::RefCell;

pub use super::bootrom;
pub use super::cartridge;
pub use super::ppu;
pub use super::input;

pub struct MMU {
  pub interrupt_flags: u8,
  pub interrupt_enabled: u8,
  pub ppu: RefCell<ppu::PPU>,

  bootrom: bootrom::Bootrom,
  bootroom_active: bool,

  cartridge: cartridge::Cartridge, // 0000-7fff
  // GPU's video ram                      8000-9FFF
  external_ram: Vec<u8>, // A000-BFFF
  work_ram: Vec<u8>, // C000-DFFF, with E000-FDFF shadow
  sprite_info: Vec<u8>, // FE00-FE9F
  io: Vec<u8>, // FF00-FF7F
  zram: Vec<u8>, // FF80-FFFF (zero page ram)

  input: input::Input,
  // Switches banks via the MBC (memory bank controller)
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

      interrupt_enabled: 0x00,
      interrupt_flags: 0x00,
    }
  }

  pub fn initialize(&mut self) {
    self.write(0xFF05, 0x00);
    self.write(0xFF06, 0x00);
    self.write(0xFF07, 0x00);
    self.write(0xFF10, 0x80);
    self.write(0xFF11, 0xBF);
    self.write(0xFF12, 0xF3);
    self.write(0xFF14, 0xBF);
    self.write(0xFF16, 0x3F);
    self.write(0xFF17, 0x00);
    self.write(0xFF19, 0xBF);
    self.write(0xFF1A, 0x7F);
    self.write(0xFF1B, 0xFF);
    self.write(0xFF1C, 0x9F);
    self.write(0xFF1E, 0xBF);
    self.write(0xFF20, 0xFF);
    self.write(0xFF21, 0x00);
    self.write(0xFF22, 0x00);
    self.write(0xFF23, 0xBF);
    self.write(0xFF24, 0x77);
    self.write(0xFF25, 0xF3);
    self.write(0xFF26, 0xF1);
    self.write(0xFF40, 0x91);
    self.write(0xFF42, 0x00);
    self.write(0xFF43, 0x00);
    self.write(0xFF45, 0x00);
    self.write(0xFF47, 0xFC);
    self.write(0xFF48, 0xFF);
    self.write(0xFF49, 0xFF);
    self.write(0xFF4A, 0x00);
    self.write(0xFF4B, 0x00);
    self.write(0xFFFF, 0x00);
  }

  pub fn load_game(&mut self, rom_filename: &str) {
    let mut f = File::open(rom_filename).unwrap();
    f.read(&mut self.cartridge.buffer).unwrap();
  }

  pub fn read(&self, address: u16) -> u8 {
    let result = match address {
      0x0000...0x7FFF => {
        // if address <= 0x0100 && self.bootroom_active {
        //   self.bootrom.buffer [ address as usize ]
        // } else {
          self.cartridge.buffer[ address as usize ]
        // }
      },
      0x8000...0x9FFF => {
        self.ppu.borrow_mut().video_ram[ address as usize - 0x8000 ]
      }
      0xA000...0xBFFF => {
        self.external_ram[ address as usize - 0xA000 ]
      }
      0xC000...0xDFFF => {
        self.work_ram[ address as usize - 0xC000 ]
      }
      0xE000...0xFDFF => {
        self.work_ram[ address as usize - 0xC000 - 0x2000 ]
      } // ECHO work ram
      0xFE00...0xFE9F => {
        self.sprite_info[ address as usize - 0xFE00 ]
      }
      0xFF00 => {
        self.input.read(address)
      }
      0xFF01...0xFF0E => {
        self.io[ address as usize - 0xFF00 ]
      }
      0xFF0F => {
        self.interrupt_flags
      }
      0xFF10...0xFF3F => {
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
        self.zram[ address as usize - 0xFF80 ]
      }
      0xFFFF => {
        self.interrupt_enabled
      }
      _ => {
        panic!("Memory access is out of bounds: {:#X}", address);
      }
    };

    result
  }

  pub fn read_word(&self, address: u16) -> u16 {
    let lo_byte = self.read(address);
    let hi_byte = self.read(address + 1);
    ((hi_byte as u16) << 8) | lo_byte as u16
  }

  pub fn write_word(&mut self, address: u16, data: u16) {
    let lo_data = (0x00FF & data) as u8;
    let hi_data = ((0xFF00 & data) >> 8) as u8;
    self.write(address + 1, hi_data);
    self.write(address, lo_data);
  }

  pub fn write(&mut self, address: u16, data: u8) {
    match address {
      0x0000...0x7FFF => {
      } // no-op
      0x8000...0x9FFF => {
        let mut borrowed_ppu = self.ppu.borrow_mut();
        borrowed_ppu.video_ram[ address as usize - 0x8000 ] = data;

        if address <= 0x97FF {
          borrowed_ppu.update_tile(address, data)
        }
      }
      0xA000...0xBFFF => {
        self.external_ram[ address as usize - 0xA000 ] = data
      }
      0xC000...0xDFFF => {
        self.work_ram[ address as usize - 0xC000 ] = data
      }
      0xE000...0xFDFF => self.write(address - 0xC000 - 0x2000, data), // ECHO work ram
      0xFE00...0xFE9F => {
        let mut borrowed_ppu = self.ppu.borrow_mut();

        let sprite_addr = address as usize - 0xFE00;
        self.sprite_info[ sprite_addr ] = data;

        borrowed_ppu.update_sprite_object(sprite_addr, data)
      }
      0xFEA0...0xFEFF => {
      } // no-op
      0xFF00 => {
        self.input.write(address, data)
      }
      0xFF01...0xFF0E => {
        self.io[ address as usize - 0xFF00 ] = data
      }
      0xFF0F => {
        self.interrupt_flags = data;
      }
      0xFF10...0xFF3F => {
        self.io[ address as usize - 0xFF00 ] = data
      }
      0xFF40...0xFF45 | 0xFF47...0xFF7F => { // 0xFF40...0xFF7F
        if address == 0xFF50 {
          self.bootroom_active = false;
        }

        self.ppu.borrow_mut().write(address, data)
      }
      0xFF46 => {
        self.oam_dma(data)
      }
      0xFF80...0xFFFE => {
        self.zram[ address as usize - 0xFF80 ] = data
      }
      0xFFFF => {
        self.interrupt_enabled = data;
      }
      _ => {
        panic!("Memory access is out of bounds: {:#X}", address);
      }
    }
  }

  fn oam_dma(&mut self, source_address_high_byte: u8) {
    let source_base_address = (source_address_high_byte as u16) << 8;
    const OAM_START_ADDRESS: u16 = 0xFE00;

    for index in 0x00 .. 0xA0 {
      let source_byte = self.read(source_base_address + index);
      self.write(OAM_START_ADDRESS + index, source_byte);
    }
  }

  pub fn num_rom_banks(&self) -> u8 {
    self.read(0x147)
  }

  pub fn num_ram_banks(&self) -> u8 {
    self.read(0x148)
  }
}
