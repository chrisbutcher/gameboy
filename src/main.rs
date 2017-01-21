pub mod types;
pub mod cpu;
pub mod cartridge;
pub mod mmu;
pub mod ppu;

// Specs:
// http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
// http://web.textfiles.com/games/gbspec.txt
// http://bgb.bircd.org/pandocs.htm#aboutthepandocs

// Opcodes specifically:
// http://imrannazar.com/Gameboy-Z80-Opcode-Map
// http://gameboy.mongenel.com/dmg/opcodes.html

// Tutorials
// https://www.youtube.com/watch?v=_mHdUhVQOb8
// http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-The-CPU
// http://www.codeslinger.co.uk/pages/projects/gameboy/beginning.html
// https://cturt.github.io/cinoop.html

struct GameBoy {
  cpu: cpu::CPU,
  mmu: mmu::MMU,
  ppu: ppu::PPU
}

const MAX_CYCLES: i32 = 69905;

impl GameBoy {
  pub fn new() -> GameBoy {
    GameBoy {
      cpu: cpu::CPU::new(),
      mmu: mmu::MMU::new(),
      ppu: ppu::PPU::new()
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
    self.mmu.write(0xFF05, 0x00);
    self.mmu.write(0xFF06, 0x00);
    self.mmu.write(0xFF07, 0x00);
    self.mmu.write(0xFF10, 0x80);
    self.mmu.write(0xFF11, 0xBF);
    self.mmu.write(0xFF12, 0xF3);
    self.mmu.write(0xFF14, 0xBF);
    self.mmu.write(0xFF16, 0x3F);
    self.mmu.write(0xFF17, 0x00);
    self.mmu.write(0xFF19, 0xBF);
    self.mmu.write(0xFF1A, 0x7F);
    self.mmu.write(0xFF1B, 0xFF);
    self.mmu.write(0xFF1C, 0x9F);
    self.mmu.write(0xFF1E, 0xBF);
    self.mmu.write(0xFF20, 0xFF);
    self.mmu.write(0xFF21, 0x00);
    self.mmu.write(0xFF22, 0x00);
    self.mmu.write(0xFF23, 0xBF);
    self.mmu.write(0xFF24, 0x77);
    self.mmu.write(0xFF25, 0xF3);
    self.mmu.write(0xFF26, 0xF1);
    self.mmu.write(0xFF40, 0x91);
    self.mmu.write(0xFF42, 0x00);
    self.mmu.write(0xFF43, 0x00);
    self.mmu.write(0xFF45, 0x00);
    self.mmu.write(0xFF47, 0xFC);
    self.mmu.write(0xFF48, 0xFF);
    self.mmu.write(0xFF49, 0xFF);
    self.mmu.write(0xFF4A, 0x00);
    self.mmu.write(0xFF4B, 0x00);
    self.mmu.write(0xFFFF, 0x00);
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
      let ch = self.mmu.read(i) as char;
      let mut ch = if ch == '\u{0}' { ' ' } else { ch };

      title.push(ch)
    }

    println!("Loading ROM: {:?}", title.trim()) // TODO
  }
}

fn main() {
  let mut game_boy = GameBoy::new();

  game_boy.initialize();
  game_boy.mmu.load_game("tetris.gb");
  game_boy.print_game_title();
  println!("The game uses {:?} ROM banks", game_boy.mmu.num_rom_banks());
  println!("The game uses {:?} RAM banks", game_boy.mmu.num_ram_banks());

  loop {
    game_boy.cpu.tick();
    // ppu.tick(cpu.cycles)
    // screen.render(ppu.frame_buffer)

    break // TODO
  }
}

#[test]
fn initialization_works() {
  let mut game_boy = GameBoy::new();
  game_boy.initialize();

  assert_eq!(game_boy.cpu.PC, 0x100);

  assert_eq!(game_boy.cpu.AF.read(), 0x01B0);
  assert_eq!(game_boy.cpu.BC.read(), 0x0013);
  assert_eq!(game_boy.cpu.DE.read(), 0x00D8);
  assert_eq!(game_boy.cpu.HL.read(), 0x014D);
  assert_eq!(game_boy.cpu.SP.read(), 0xFFFE);

  assert_eq!(game_boy.mmu.read(0xFF05), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF06), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF07), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF10), 0x80);
  assert_eq!(game_boy.mmu.read(0xFF11), 0xBF);
  assert_eq!(game_boy.mmu.read(0xFF12), 0xF3);
  assert_eq!(game_boy.mmu.read(0xFF14), 0xBF);
  assert_eq!(game_boy.mmu.read(0xFF16), 0x3F);
  assert_eq!(game_boy.mmu.read(0xFF17), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF19), 0xBF);
  assert_eq!(game_boy.mmu.read(0xFF1A), 0x7F);
  assert_eq!(game_boy.mmu.read(0xFF1B), 0xFF);
  assert_eq!(game_boy.mmu.read(0xFF1C), 0x9F);
  assert_eq!(game_boy.mmu.read(0xFF1E), 0xBF);
  assert_eq!(game_boy.mmu.read(0xFF20), 0xFF);
  assert_eq!(game_boy.mmu.read(0xFF21), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF22), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF23), 0xBF);
  assert_eq!(game_boy.mmu.read(0xFF24), 0x77);
  assert_eq!(game_boy.mmu.read(0xFF25), 0xF3);
  assert_eq!(game_boy.mmu.read(0xFF26), 0xF1);
  assert_eq!(game_boy.mmu.read(0xFF40), 0x91);
  assert_eq!(game_boy.mmu.read(0xFF42), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF43), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF45), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF47), 0xFC);
  assert_eq!(game_boy.mmu.read(0xFF48), 0xFF);
  assert_eq!(game_boy.mmu.read(0xFF49), 0xFF);
  assert_eq!(game_boy.mmu.read(0xFF4A), 0x00);
  assert_eq!(game_boy.mmu.read(0xFF4B), 0x00);
  assert_eq!(game_boy.mmu.read(0xFFFF), 0x00)
}
