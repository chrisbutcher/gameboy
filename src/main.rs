
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
use sdl2::keyboard::Keycode;

pub mod cpu;
pub mod bootrom;
pub mod cartridge;
pub mod mmu;
pub mod timer;
pub mod ppu;
pub mod window_set;
pub mod input;
pub mod fps;

use std::thread;
use std::sync::mpsc::{channel, sync_channel};
use std::sync::mpsc::{SyncSender, Receiver, TrySendError, TryRecvError};

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
    self.cpu.initialize();
    self.mmu.initialize();
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
      self.update_mmu();
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
  }

  fn update_timers(&mut self, cycles: i32) {
    self.mmu.timer.tick(cycles);
  }

  fn update_mmu(&mut self) {
    self.mmu.interrupt_flags |= self.mmu.input.interrupt_flags;
    self.mmu.input.interrupt_flags = 0x00;
  }

  fn update_graphics(&mut self, cycles: i32) {
    self.mmu.ppu.borrow_mut().tick(cycles);
    self.mmu.interrupt_flags |= self.mmu.ppu.borrow_mut().interrupt_flags;
    self.mmu.ppu.borrow_mut().interrupt_flags = 0x00;
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

  let mut window_set = window_set::WindowSet::new();

  let (events_sender, events_receiver) = channel();
  let (frames_sender, frames_receiver) = sync_channel(1);

  let game_thread = thread::Builder::new().name("game".to_string()).spawn(move || {
    main_loop(game_boy, events_receiver, frames_sender)
  }).unwrap();

  let mut events = window_set.sdl_context.event_pump().unwrap();

  'main: loop {
    for event in events.poll_iter() {
      match event {
        Event::Quit { .. } => break 'main,
        Event::KeyDown { keycode, .. } => {
          match keycode { Some(Keycode::Escape) | Some(Keycode::Q) => break 'main, _ => {} }

          if let Some(pressed_button) = translate_sdl2_keycode(keycode) {
            events_sender.send((pressed_button, true)).unwrap();
          }
        },
        Event::KeyUp { keycode, .. } => {
          if let Some(pressed_button) = translate_sdl2_keycode(keycode) {
            events_sender.send((pressed_button, false)).unwrap();
          }
        },
        _ => {}
      }
    }

    match frames_receiver.try_recv() {
      Ok(framebuffer) => { window_set.render_screen(&framebuffer) },
      Err(TryRecvError::Empty) => (),
      Err(TryRecvError::Disconnected) => break 'main,
    }
  }

  drop(events_sender);
  drop(frames_receiver);
  game_thread.join().unwrap();
}

fn main_loop(mut game_boy: GameBoy, events_receiver: Receiver<(input::Button, bool)>, frames_sender: SyncSender<Vec<u8>>) {
  let mut fps_counter = fps::Counter::new();

  'game: loop {
    fps_counter.print_fps();

    match events_receiver.try_recv() {
      Ok((input_button, pressed)) => {
        if pressed {
          game_boy.mmu.input.key_pressed(input_button);
        } else {
          game_boy.mmu.input.key_released(input_button);
        }
      },
      _ => {}
    }

    game_boy.render_frame(); // TODO limit to 60fps

    let framebuffer = game_boy.mmu.ppu.borrow_mut().framebuffer.to_vec();
    match frames_sender.try_send(framebuffer) {
      Err(TrySendError::Disconnected(_)) => break 'game,
      _ => {}
    }
  }
}

fn translate_sdl2_keycode(keycode: Option<Keycode>) -> Option<input::Button> {
  match keycode {
    Some(Keycode::Up) => Some(input::Button::Up),
    Some(Keycode::Down) => Some(input::Button::Down),
    Some(Keycode::Left) => Some(input::Button::Left),
    Some(Keycode::Right) => Some(input::Button::Right),
    Some(Keycode::X) => Some(input::Button::B),
    Some(Keycode::Z) => Some(input::Button::A),
    Some(Keycode::RShift) => Some(input::Button::Select),
    Some(Keycode::Return) => Some(input::Button::Start),
    _ => None
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
