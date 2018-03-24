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

use std::thread::{self, sleep};
use std::sync::mpsc::{channel, sync_channel};
use std::sync::mpsc::{SyncSender, Receiver, TrySendError, TryRecvError};
use std::time::Duration;

const LIMIT_FRAME_RATE: bool = true;

struct GameBoy {
  cpu: cpu::CPU,
  mmu: mmu::MMU,
}

impl GameBoy {
  fn new() -> Box<GameBoy> {
    Box::new(
      GameBoy {
        cpu: cpu::CPU::new(),
        mmu: mmu::MMU::new(),
      }
    )
  }

  fn initialize(&mut self) {
    // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
    self.cpu.initialize();
    self.mmu.initialize();
  }

  // Reminders:
  // To get around mutable references being consume more than once, just re-bind as immutable when possible
  // let mut foo = blah;
  // do_something(&mut foo);
  // let foo = foo;

  // When printing things, rather than giving ownership, just pass & reference so that after the print you can still
  // use it

  fn render_frame(&mut self) {
    let cycles_per_frame = (4194304f64 / 1000.0 * 16.0).round() as u32;
    let mut cycles_this_frame = 0;

    while cycles_this_frame < cycles_per_frame {
      let cycles = self.cpu.execute_next_opcode(&mut self.mmu);

      cycles_this_frame = cycles_this_frame.wrapping_add(cycles as u32);
      self.update_timers(cycles);
      self.update_mmu();
      self.update_graphics(cycles);
    }

    cycles_this_frame.wrapping_sub(cycles_per_frame);
  }

  fn render_debug_screen(&mut self) {
    self.mmu.ppu.borrow_mut().update_debug_frame();
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
    game_loop(game_boy, events_receiver, frames_sender)
  }).unwrap();

  let mut events = window_set.sdl_context.event_pump().unwrap();

  'main: loop {
    for event in events.wait_timeout_iter(1) {
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
      Ok(framebuffers) => {
        window_set.render_screen(&framebuffers.0);
        window_set.render_debug_screen(&framebuffers.1);
      },
      Err(TryRecvError::Empty) => (),
      Err(TryRecvError::Disconnected) => break 'main,
    }
  }

  drop(events_sender);
  drop(frames_receiver);

  game_thread.join().unwrap();
}

fn game_loop(mut game_boy: Box<GameBoy>, events_receiver: Receiver<(input::Button, bool)>, frames_sender: SyncSender<(Vec<u8>, Vec<u8>)>) {
  let mut fps_counter = fps::Counter::new(false);
  let limiter = frame_limiter();

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

    game_boy.render_frame();
    game_boy.render_debug_screen();

    let framebuffer = game_boy.mmu.ppu.borrow_mut().framebuffer.to_vec();
    let debug_framebuffer = game_boy.mmu.ppu.borrow_mut().debug_framebuffer.to_vec();

    match frames_sender.try_send((framebuffer, debug_framebuffer)) {
      Err(TrySendError::Disconnected(_)) => break 'game,
      _ => {}
    }

    if LIMIT_FRAME_RATE {
      match limiter.recv() {
        Ok(_) => {},
        _ => { println!("limiter RecvError") }
      }
    }
  }
}

// TODO replace this frame limiter solution, it's too brittle and blows up when more CPU intensive work is done
fn frame_limiter() -> Receiver<()> {
  let (sender, receiver) = sync_channel(1);
  thread::Builder::new().name("frame_limiter".to_string()).spawn(move || {
    'frame_limiter: loop {
      sleep(Duration::from_millis(14)); // maintains 60fps
      match sender.try_send(()) {
        Err(_) => { } , // NOTE this used to break
        _ => {}
      }
    }
  }).unwrap();

  receiver
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

  let expected_cpu_state = "[PC] 0x36E\n[Regs] A:0x0, F:0xA0, B:0x0, C:0x8, D:0x0, E:0x10, H:0xFF, L:0xA8\n[Flags]: Z: 1, N: 0, H: 1 C: 0 [SP] 0xCFFF";
  let actual_cpu_state;

  loop {
    if game_boy.cpu.tick_counter >= 42_000_000 {
      actual_cpu_state = format!("{:?}", game_boy.cpu);
      break;
    }
    game_boy.render_frame();
  }

  assert_eq!(expected_cpu_state, actual_cpu_state);
}
