pub use super::types;
pub use super::sdl2;
pub use super::sdl2::pixels;
pub use super::sdl2::render::Renderer;

// PPU supports tiles: 8x8 pixel groups
// Modes: Sprite Read, Video Read, Horizontal Blank, Vertical Blank
// Starts in vertical blank
pub struct PPU {
  // Framebuffer -- 3d array of pixels. 160 x 144 x 3 (3 bytes to support GBC, first byte for b&w)
  // framebuffer: [[[types::Byte; 160]; 144]; 3]
  pub mode: u8,
  pub mode_clock: i32,
  pub line: u8,
  pub scroll_x: u8,
  pub scroll_y: u8,

  pub sdl_context: sdl2::Sdl,
  pub renderer: Renderer<'static>,
}

impl PPU {
  pub fn new() -> PPU {
    let sdl_context = sdl2::init().unwrap();
    let video_subsys = sdl_context.video().unwrap();
    let window = video_subsys.window("GAMEBOY", 160, 144)
      .position_centered()
      .opengl()
      .build()
      .unwrap();

    let renderer = window.renderer()
      .present_vsync()
      .build()
      .unwrap();

    PPU {
      mode: 0,
      mode_clock: 0,
      line: 0,
      scroll_x: 0,
      scroll_y: 0,
      sdl_context: sdl_context,
      renderer: renderer,
    }
  }

  pub fn rb(&mut self, address: types::Word) -> types::Byte {
    match address {
      0xFF40 => {
        0x00
      },
      0xFF42 => {
        self.scroll_y as types::Byte
      },
      0xFF43 => {
        self.scroll_x as types::Byte
      },
      0xFF44 => {
        self.line as types::Byte
      },
      _ => {
        panic!("Unexpected address in PPU");
      }
    }
  }

  pub fn render_line(&mut self) {
    // NO OP
  }

  pub fn render_screen(&mut self) {
    // NO OP
    self.renderer.set_draw_color(pixels::Color::RGB(152, 184, 24));
    self.renderer.clear();

    self.renderer.set_draw_color(pixels::Color::RGB(255, 255, 255));
    self.renderer.draw_line(sdl2::rect::Point::new(0,0), sdl2::rect::Point::new(100,100));
    self.renderer.present();
  }

  pub fn tick(&mut self, cycles: i32) {
    self.mode_clock += cycles;

    match self.mode {
      2 => {
        // TODO update comments
        // OAM read mode, scanline active
        if self.mode_clock >= 80 {
          self.mode_clock = 0;
          self.mode = 3;
        }
      },
      3 => {
        // VRAM read mode, scanline active
	      // Treat end of mode 3 as end of scanline
        if self.mode_clock >= 172 {
          // Enter hblank
          self.mode_clock = 0;
          self.mode = 0;

          // Write a scanline to the framebuffer
          self.render_line();
        }
      },
      0 => {
        // Hblank
	      // After the last hblank, push the screen data to canvas
        if self.mode_clock >= 204 {
          self.mode_clock = 0;
          self.line += 1;

          if self.line == 143 {
            // Enter vblank
            self.mode = 1;
            self.render_screen();
          } else {
            self.mode = 2; // Re-enter OAM read mode
          }
        }
      },
      1 => {
        // Vblank (10 lines)
        if self.mode_clock >= 456 {
          self.mode_clock = 0;
          self.line += 1;

          if self.line > 153 {
            self.mode = 2;
            self.line = 0;
          }
        }
      },
      _ => {
        panic!("Unexpected PPU mode");
      }
    }

    // debug!("PPU#tick()");
  }
}
