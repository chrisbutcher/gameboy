#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_must_use)]
#![allow(non_snake_case)]

extern crate time;

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate sdl2;
use sdl2::event::Event;

pub mod types;
pub mod cpu;
pub mod cartridge;
pub mod mmu;
pub mod ppu;

// Specs:
// http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
// http://web.textfiles.com/games/gbspec.txt
// http://bgb.bircd.org/pandocs.htm#aboutthepandocs
// https://www.youtube.com/watch?v=ecTQVa42sJc
// http://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/
// http://gameboy.mongenel.com/asmschool.html

// Rust sdl2
// http://jadpole.github.io/arcaders/arcaders-1-4
// https://github.com/simias/gb-rs/blob/master/src/ui/sdl2/display.rs#L10

// Test roms:
// http://gbdev.gg8.se/files/roms/blargg-gb-tests/

// Opcodes specifically:
// http://imrannazar.com/Gameboy-Z80-Opcode-Map
// http://gameboy.mongenel.com/dmg/opcodes.html

// Tools:
// https://github.com/mmuszkow/gb-disasm (Gameboy Rom disassembler)

// Tutorials
// https://www.youtube.com/watch?v=_mHdUhVQOb8
// http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-The-CPU
// http://www.codeslinger.co.uk/pages/projects/gameboy/beginning.html
// https://cturt.github.io/cinoop.html
// https://github.com/jedahan/rustboy/blob/master/development_log.md
// https://speakerdeck.com/albertofem/a-journey-into-hardware-emulation-building-a-gameboy-emulator-from-scratch

// Livecoding
// https://www.youtube.com/watch?v=025tC0DcFUI&t=625s

struct GameBoy {
  cpu: cpu::CPU,
  mmu: mmu::MMU,
}

const MAX_CYCLES_PER_UPDATE: i32 = 69905;

impl GameBoy {
  pub fn new() -> GameBoy {
    GameBoy {
      cpu: cpu::CPU::new(),
      mmu: mmu::MMU::new(),
    }
  }

  pub fn initialize(&mut self) {
    // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
    // self.cpu.PC = 0x0100;
    self.cpu.PC = 0x0; // TODO

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

  fn render_frame(&mut self) {
    // TODO LIMIT to max 60
    let mut cycles_this_update = 0;

    while cycles_this_update < MAX_CYCLES_PER_UPDATE {
      let cycles = self.cpu.execute_next_opcode(&mut self.mmu);
      cycles_this_update += cycles;
      self.update_timers(cycles);
      self.update_graphics(cycles);
      self.do_interrupts();

      // break; // TODO
    }
    self.render_screen();
  }

  fn update_timers(&mut self, cycles: i32) {
    // NOOP
  }

  fn update_graphics(&mut self, cycles: i32) {
    self.mmu.ppu.borrow_mut().tick(cycles);
  }

  fn do_interrupts(&mut self) {
    // NOOP
  }

  fn render_screen(&mut self) {
    self.mmu.ppu.borrow_mut().render_screen();
  }

  pub fn print_game_title(&self) {
    let mut title = String::with_capacity(14);

    for i in 0x134..0x142 {
      let ch = self.mmu.read(i) as char;
      let mut ch = if ch == '\u{0}' { ' ' } else { ch };

      title.push(ch)
    }

    debug!("Loading ROM: {:?}", title.trim()) // TODO
  }
}

// RUST_LOG=debug cargo run tetris.gb
fn main() {
  env_logger::init().unwrap();
  let mut game_boy = GameBoy::new();

  game_boy.initialize();
  game_boy.mmu.load_game("tetris.gb");
  // game_boy.mmu.load_game("cpu_instrs.gb");

  game_boy.print_game_title();
  debug!("The game uses {:?} ROM banks", game_boy.mmu.num_rom_banks());
  debug!("The game uses {:?} RAM banks", game_boy.mmu.num_ram_banks());

  let mut events = None;

  {
    let ppu = game_boy.mmu.ppu.borrow_mut();
    events = Some(ppu.sdl_context.event_pump().unwrap());
  }

  let mut events = events.unwrap();

  'main: loop {
    for event in events.poll_iter() {
      match event {
        Event::Quit {..} => break 'main,
        _ => {
        }
      }
    }

    game_boy.render_frame();
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
