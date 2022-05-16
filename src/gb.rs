pub use super::bootrom;
pub use super::cartridge;
pub use super::cpu;
pub use super::fps;
pub use super::input;
pub use super::mmu;
pub use super::ppu;
pub use super::timer;
pub use super::window_set;

pub struct GameBoy {
  pub cpu: cpu::CPU,
  pub mmu: mmu::MMU,
}

impl GameBoy {
  pub fn new() -> Box<GameBoy> {
    Box::new(GameBoy {
      cpu: cpu::CPU::default(),
      mmu: mmu::MMU::default(),
    })
  }

  pub fn reset(&mut self) {
    // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
    self.cpu.initialize();
    self.mmu.initialize();
  }

  pub fn render_frame(&mut self) {
    // 4.19 mhz CPU, 60fps maintained by frame limiter channel in game loop thread
    let cycles_per_frame = (4194304f64 / 1000.0 * 16.0).round() as u32;
    let mut cycles_this_frame = 0;

    while cycles_this_frame < cycles_per_frame {
      let cycles = self.cpu.execute_next_opcode(&mut self.mmu);

      cycles_this_frame = cycles_this_frame.wrapping_add(cycles as u32);
      self.update_timers(cycles);
      self.update_mmu();
      self.update_graphics(cycles);
    }
  }

  pub fn render_debug_screen(&mut self) {
    self.mmu.ppu.borrow_mut().update_debug_frame();
  }

  pub fn print_game_title(&self) {
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
}
