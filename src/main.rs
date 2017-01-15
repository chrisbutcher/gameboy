use std::io;
use std::fmt;
use std::io::prelude::*;
use std::fs::File;

// Specs:
// http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
// http://web.textfiles.com/games/gbspec.txt

// Opcodes specifically:
// http://imrannazar.com/Gameboy-Z80-Opcode-Map
// http://gameboy.mongenel.com/dmg/opcodes.html

// Tutorials
// https://www.youtube.com/watch?v=_mHdUhVQOb8
// http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-The-CPU
// http://www.codeslinger.co.uk/pages/projects/gameboy/beginning.html

type Byte = u8;
type SignedByte = i8;
type Word = u16;
type SignedWord = i16;

struct Register {
  value: Word
}

impl Register {
  pub fn new() -> Register {
    Register{value: 0x0}
  }

  pub fn read(&self) -> Word {
    self.value
  }

  pub fn read_hi(&self) -> Byte {
    ((self.value >> 8) & 0xFF) as Byte
  }

  pub fn read_lo(&self) -> Byte {
    (self.value & 0xFF) as Byte
  }

  pub fn write(&mut self, v: Word) {
    self.value = v
  }

  pub fn write_hi(&mut self, v: Byte) {
    self.value = self.value & 0x00FF | (v as Word) << 8
  }

  pub fn write_lo(&mut self, v: Byte) {
    self.value = self.value | v as Word
  }
}

impl fmt::Debug for Register {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Register {{ value: {} }}", self.value)
  }
}

struct GameBoy {
  cpu: CPU,
  mmu: MMU
}

struct CPU {
  AF: Register, BC: Register, DE: Register, HL: Register,
  SP: Register, PC: Word,
}

struct MMU {
  buffer: Vec<Byte>,
  cartridge: Cartridge
  // Switches banks via the MBC (memory bank controller)
}

struct PPU {
  // Framebuffer -- array of pixels. 160 x 144
  // Has tiles: 8x8 pixel groups
  // modes: Sprite Read, Video Read, Horizontal Blank, Vertical Blank
  // Starts in vertical blank
}

struct Cartridge {
  buffer: Vec<Byte>
}

const MAX_CYCLES: i32 = 69905;
const MAX_CARTRIDGE_SIZE: usize = 0x200000;

const MEMORY_SIZE: usize = 0x10000;

impl GameBoy {
  pub fn new() -> GameBoy {
    GameBoy {
      cpu: CPU::new(),
      mmu: MMU::new()
    }
  }

  pub fn initialize(&mut self) {
    // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
    self.cpu.PC = 0x0100;
    self.cpu.AF.write(0x01B0);
    self.cpu.BC.write(0x0013);
    self.cpu.DE.write(0x00D8);
    self.cpu.HL.write(0x014D);
    self.cpu.SP.write(0xFFFE);
    self.mmu.buffer[0xFF05] = 0x00;
    self.mmu.buffer[0xFF06] = 0x00;
    self.mmu.buffer[0xFF07] = 0x00;
    self.mmu.buffer[0xFF10] = 0x80;
    self.mmu.buffer[0xFF11] = 0xBF;
    self.mmu.buffer[0xFF12] = 0xF3;
    self.mmu.buffer[0xFF14] = 0xBF;
    self.mmu.buffer[0xFF16] = 0x3F;
    self.mmu.buffer[0xFF17] = 0x00;
    self.mmu.buffer[0xFF19] = 0xBF;
    self.mmu.buffer[0xFF1A] = 0x7F;
    self.mmu.buffer[0xFF1B] = 0xFF;
    self.mmu.buffer[0xFF1C] = 0x9F;
    self.mmu.buffer[0xFF1E] = 0xBF;
    self.mmu.buffer[0xFF20] = 0xFF;
    self.mmu.buffer[0xFF21] = 0x00;
    self.mmu.buffer[0xFF22] = 0x00;
    self.mmu.buffer[0xFF23] = 0xBF;
    self.mmu.buffer[0xFF24] = 0x77;
    self.mmu.buffer[0xFF25] = 0xF3;
    self.mmu.buffer[0xFF26] = 0xF1;
    self.mmu.buffer[0xFF40] = 0x91;
    self.mmu.buffer[0xFF42] = 0x00;
    self.mmu.buffer[0xFF43] = 0x00;
    self.mmu.buffer[0xFF45] = 0x00;
    self.mmu.buffer[0xFF47] = 0xFC;
    self.mmu.buffer[0xFF48] = 0xFF;
    self.mmu.buffer[0xFF49] = 0xFF;
    self.mmu.buffer[0xFF4A] = 0x00;
    self.mmu.buffer[0xFF4B] = 0x00;
    self.mmu.buffer[0xFFFF] = 0x00;
  }

  pub fn tick(&mut self) {
    // let mut cycles_this_update = 0;

    // while cycles_this_update < MAX_CYCLES {
      // int cycles = ExecuteNextOpcode( ) ;
      // cyclesThisUpdate+=cycles ;
      // UpdateTimers(cycles) ;
      // UpdateGraphics(cycles) ;
      // DoInterupts( ) ;
    // }
    // RenderScreen( ) ;
  }

  pub fn print_game_title(&self) {
    let mut title = String::with_capacity(14);

    for i in 0x134..0x142 {
      title.push(self.mmu.cartridge.buffer[i] as char)
    }

    println!("Loading ROM: {:?}", title)
  }
}

const FlagZ: u8 = 7; // Zero
const FlagN: u8 = 6; // Negative
const FlagH: u8 = 5; // Half-carry
const FlagC: u8 = 4; // Carry

impl CPU {
  pub fn new() -> CPU {
      CPU {
        AF: Register::new(), BC: Register::new(), DE: Register::new(), HL: Register::new(),
        SP: Register::new(), PC: 0x0000,
      }
  }

  pub fn tick(&self) {
    println!("Tick!");
    // Fetch next op address. op_index = memory.read_byte[self.pc]
    // Increment pc
    // Execute. opcode_fn(op_index)
    // cycles = opcode_timing(op_index)
  }

  pub fn flags(&self) -> Byte {
    self.AF.read_lo()
  }

  pub fn flag_z(&self) -> bool {
    self.AF.read_lo() & FlagZ != 0
  }

  pub fn flag_n(&self) -> bool {
    self.AF.read_lo() & FlagN != 0
  }

  pub fn flag_h(&self) -> bool {
    self.AF.read_lo() & FlagH != 0
  }

  pub fn flag_c(&self) -> bool {
    self.AF.read_lo() & FlagC != 0
  }
}

impl Cartridge {
  pub fn new() -> Cartridge {
    Cartridge {
      buffer: vec![0x00; MAX_CARTRIDGE_SIZE]
    }
  }
}

impl MMU {
  pub fn new() -> MMU {
    MMU {
      buffer: vec![0x00; MEMORY_SIZE],
      cartridge: Cartridge::new()
    }

  }

  pub fn load_game(&mut self, rom_filename: &str) {
    let mut f = File::open(rom_filename).unwrap();
    f.read(&mut self.cartridge.buffer);
  }

  pub fn read(&self, index: usize) -> u8 {
    match index {
      0x0000 ... 0x8000 => self.cartridge.buffer[index],
      0x8000 ... 0xA000 => self.buffer[index],
      0xA000 ... 0xC000 => self.cartridge.buffer[index],
      0xC000 ... 0xFFFF => self.cartridge.buffer[index],
      _ => 0x0000
    }
  }

  pub fn write(&mut self, address: Word, data: Byte) {
    // Disallow writes to restricted memory regions
    if address < 0x8000 || (address >= 0xFEA0 && address < 0xFEFF) {
      return
    } else if address >= 0xE000 && address < 0xFE00 {
      let echo_ram_offset = 0x2000;
      self.write(address - echo_ram_offset, data)
    }

    self.buffer[address as usize] = data;
  }
}

fn main() {
  let mut game_boy = GameBoy::new();

  game_boy.initialize();
  game_boy.mmu.load_game("tetris.gb");
  game_boy.print_game_title();

  loop {
    game_boy.cpu.tick();
    // ppu.tick(cpu.cycles)
    // screen.render(ppu.frame_buffer)

    break // TODO
  }
}

#[test]
fn it_works() {
  let mut game_boy = GameBoy::new();
  println!("{:?}", game_boy.cpu.PC);
}

#[test]
fn register_setting() {
  let mut register = Register::new();
  register.write(0xAABB);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xBB, register.read_lo());
  assert_eq!(0xAABB, register.read());

  register.write_lo(0xFF);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xFF, register.read_lo());
  assert_eq!(0xAAFF, register.read());

  register.write_hi(0xDD);
  assert_eq!(0xDD, register.read_hi());
  assert_eq!(0xFF, register.read_lo());
  assert_eq!(0xDDFF, register.read());
}
