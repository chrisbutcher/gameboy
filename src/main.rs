extern crate socket_state_reporter;
use self::socket_state_reporter::StateReporter;

const SYNC_STATE: bool = false;

extern crate clap;
use clap::App;

extern crate time;

#[macro_use]
extern crate log;
extern crate env_logger;

pub extern crate sdl2;
use sdl2::event::Event;

pub mod cpu;
pub mod bootrom;
pub mod cartridge;
pub mod mmu;
pub mod ppu;
pub mod input;
pub mod fps;

struct GameBoy {
  cpu: cpu::CPU,
  mmu: mmu::MMU,

  state_reporter: StateReporter,
}

impl GameBoy {
  fn new() -> GameBoy {
    GameBoy {
      cpu: cpu::CPU::new(),
      mmu: mmu::MMU::new(),

      state_reporter: StateReporter::new("5555"),
    }
  }

  fn initialize(&mut self) {
    // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
    self.cpu.pc = 0x0100;

    self.cpu.af.write(0x01B0);
    self.cpu.bc.write(0x0013);
    self.cpu.de.write(0x00D8);
    self.cpu.hl.write(0x014D);
    self.cpu.sp.write(0xFFFE);

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

  // NOTE
  // To get around mutable references being consume more than once, just re-bind as immutable when possible
  // let mut foo = blah;
  // do_something(&mut foo);
  // let foo = foo;

  // NOTE
  // When printing things, rather than giving ownership, just pass & reference so that after the print you can still
  // use it

  // Avoid large array allocations [0; SIZE], use vec![] instead or box [] to put data on heap, not stack

  // TODO remove all unwrap()s

  fn render_frame(&mut self) {
    let cycles_per_frame = (4194304f64 / 1000.0 * 16.0).round() as u32;
    let mut cycles_this_frame = 0;

    while cycles_this_frame < cycles_per_frame {
      let cycles = self.cpu.execute_next_opcode(&mut self.mmu);

      cycles_this_frame = cycles_this_frame.wrapping_add(cycles as u32);
      self.update_timers(cycles);
      self.update_graphics(cycles);

      if SYNC_STATE {
        self.state_reporter.send_message(format!("{}", cycles).as_bytes());
        let received = self.state_reporter.receive_message();
        if received == "kill" {
          println!("{:#?}", &self.cpu);
          panic!("Server stopped.");
        }
      }
    }

    cycles_this_frame.wrapping_sub(cycles_per_frame);

    self.render_screen();
  }

  fn update_timers(&mut self, _cycles: i32) {
    // NOOP
  }

  fn update_graphics(&mut self, cycles: i32) {
    self.mmu.ppu.borrow_mut().tick(cycles);
    self.mmu.interrupt_flags |= self.mmu.ppu.borrow_mut().interrupt_flags;
    self.mmu.ppu.borrow_mut().interrupt_flags = 0x00;
  }

  fn render_screen(&mut self) {
    self.mmu.ppu.borrow_mut().render_screen();
    self.mmu.ppu.borrow_mut().show_debug_tiles();
  }

  fn print_game_title(&self) {
    let mut title = String::with_capacity(14);

    for i in 0x134..0x142 {
      let mut ch = self.mmu.read(i) as char;
      ch = if ch == '\u{0}' { ' ' } else { ch };

      title.push(ch)
    }

    println!("Loading ROM: {:?}", title.trim());
    println!("The game uses {:?} ROM banks", self.mmu.num_rom_banks());
    println!("The game uses {:?} RAM banks", self.mmu.num_ram_banks());
  }
}

fn handle_cli_args<'a>() -> clap::ArgMatches<'a> {
  App::new("gameboy")
    .version("0.1.0")
    .author("Chris Butcher <cbutcher@gmail.com>")
    .about("Play Game Boy")
    .args_from_usage("-r, --rom=[FILE] 'Sets .gb rom to play'")
    .get_matches()
}

fn main() {
  let args = handle_cli_args();
  let rom_filename = args.value_of("rom").unwrap_or("tetris.gb");

  env_logger::init().unwrap();

  let mut game_boy = GameBoy::new();
  game_boy.initialize();
  game_boy.mmu.load_game(rom_filename);

  game_boy.print_game_title();

  let events;
  let mut fps_counter = fps::Counter::new();

  {
    let ppu = game_boy.mmu.ppu.borrow_mut();
    events = Some(ppu.sdl_context.event_pump().unwrap());
  }

  let mut events = events.unwrap();

  'main: loop {
    for event in events.poll_iter() {
      match event {
        Event::Quit { .. } => break 'main,
        _ => {}
      }
    }

    fps_counter.print_fps();

    game_boy.render_frame();
    // TODO limit to 60fps
  }
}

#[test]
fn can_run_tetris() {
  let mut game_boy = GameBoy::new();
  game_boy.initialize();
  game_boy.mmu.load_game("tetris.gb");

  loop {
    // if game_boy.cpu.tick_counter >= 1_000_000 {
    if game_boy.cpu.tick_counter >= 20_574_537 {
      break;
    }
    game_boy.render_frame();
  }

  assert!(game_boy.cpu.tick_counter >= 1_000_000);
}
