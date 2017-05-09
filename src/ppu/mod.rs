pub use super::types;

pub use super::sdl2;
pub use super::sdl2::pixels;
pub use super::sdl2::render::Renderer;
pub use super::sdl2::video::WindowPos;

static mut COUNTER: i32 = 5;

fn print_call_count() {
  unsafe {
    COUNTER += 1;
    println!("print_call_count: {}", COUNTER);
  }
}

// PPU supports tiles: 8x8 pixel groups
// Modes: Sprite Read, Video Read, Horizontal Blank, Vertical Blank
// Starts in vertical blank
pub struct PPU {
  // Framebuffer -- 3d array of pixels. 160 x 144 x 4 (-- previous NOTE 3 bytes to support GBC, first byte for b&w)
  // pub framebuffer: [[[types::Byte; 160]; 144]; 4],
  // pub framebuffer: [[[types::Byte; 4]; 144]; 160],
  pub framebuffer: [types::Byte; 160 * 144 * 4],
  pub video_ram: Vec<types::Byte>,
  pub tileset: [[[types::Byte; 8]; 8]; 384], // HACK 512 should be 384
  pub palette: [[types::Byte; 4]; 4],

  pub mode: u8,
  pub mode_clock: i32,
  pub line: u8,
  pub scroll_x: u8,
  pub scroll_y: u8,
  pub switch_background: bool,
  pub switch_lcd: bool,
  pub background_map: bool,
  pub background_tile: bool,

  pub sdl_context: sdl2::Sdl,
  pub game_renderer: Renderer<'static>,
  pub debug_renderer: Renderer<'static>,
}

impl PPU {
  pub fn new() -> PPU {
    let sdl_context = sdl2::init().unwrap();
    let (debug_window, sdl_context) = PPU::new_window(sdl_context, "DEBUG", 192, 192, 160);
    let (game_window, sdl_context) = PPU::new_window(sdl_context, "GAMEBOY", 160, 144, 0);

    let debug_renderer = debug_window.renderer()
      .present_vsync()
      .build()
      .unwrap();

    let game_renderer = game_window.renderer()
      .present_vsync()
      .build()
      .unwrap();

    PPU {
      framebuffer: [0x00; 160 * 144 * 4],
      video_ram: vec![0x00; 0x2000],
      tileset: [[[0x00; 8]; 8]; 384],
      palette: [
        [255, 255, 255, 255], // RGBA, TODO simplify to RGB
        [192, 192, 192, 255],
        [96, 96, 96, 255],
        [0, 0, 0, 255],
      ],

      mode: 2,
      mode_clock: 0,
      line: 0,
      scroll_x: 0,
      scroll_y: 0,
      switch_background: false,
      switch_lcd: false,
      background_map: false,
      background_tile: false,

      sdl_context: sdl_context,
      game_renderer: game_renderer,
      debug_renderer: debug_renderer,
    }
  }

  pub fn new_window(sdl_context: sdl2::Sdl, title: &str, width: i32, height: i32, x_offset: i32) -> (sdl2::video::Window, sdl2::Sdl) {
    let video_subsys = sdl_context.video().unwrap();
    let mut window = video_subsys.window(title, 160, 144)
      .position_centered()
      .opengl()
      .build()
      .unwrap();

    let position = window.position();
    window.set_position(
      WindowPos::Positioned(position.0 + x_offset),
      WindowPos::Positioned(position.1)
    );

    (window, sdl_context)
  }

  pub fn read(&mut self, address: types::Word) -> types::Byte {
    match address {
      0xFF40 => {
        let switch_background_result = if self.switch_background { 0x01 } else { 0x00 };
        let background_map_result = if self.background_map { 0x08 } else { 0x00 };
        let background_tile_result = if self.background_tile { 0x10 } else { 0x00 };
        let switch_lcd_result = if self.switch_lcd { 0x80 } else { 0x00 };
        switch_background_result | background_map_result | background_tile_result | switch_lcd_result
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
        panic!("Unexpected address in PPU#read: {:#X}", address);
      }
    }
  }

  pub fn write(&mut self, address: types::Word, value: types::Byte) {
    match address {
      0xFF40 => {
        self.switch_background = if value & 0x01 != 0 { true } else { false };
        self.background_map = if value & 0x08 != 0 { true } else { false };
        self.background_tile = if value & 0x10 != 0 { true } else { false };
        self.switch_lcd = if value & 0x80 != 0 { true } else { false };
      },
      0xFF42 => {
        self.scroll_y = value
      },
      0xFF43 => {
        self.scroll_x = value
      },
      0xFF47 => {
        for i in 0..4 {
          match (value >> (i * 2)) & 3 {
            0 => { self.palette[i] = [255,255,255,255] },
            1 => { self.palette[i] = [192,192,192,255] },
            2 => { self.palette[i] = [ 96, 96, 96,255] },
            3 => { self.palette[i] = [  0,  0,  0,255] },
            _ => {
              panic!("Unexpected background palette value: {:#X}", i);
            }
          }
        }
      },
      0xFF44 => {
        panic!("Writing to LY in PPU#write: {:#X}", address);
      },
      _ => {
        // panic!("Unexpected address in PPU#write: {:#X}", address);
      }
    }
  }

  // TODO reword all comments
  pub fn update_tile(&mut self, address: types::Word, value: types::Byte) {
    // Get the "base address" for this tile row
    let base_address = address & 0x1FFE;

    if value != 0x00 {
      // Nothing but zeros being written, in infinite loop.
      // Notes taken here anyways: https://docs.google.com/spreadsheets/d/1_OZnBw-mbklkPoMhlYTAy9lQeTT5WjXocao5c2lvU3g/edit#gid=0
      debug!("Writing data to VRAM!: {:#X}", value);
    }

    // Work out which tile and row was updated
    let tile = (base_address >> 4) & 511;
	  let y = (base_address >> 1) & 7;

    for x in 0..8 {
      // Find bit index for this pixel
      let sx = 1 << (7-x);

      // Update tile set
      let pixel_colour = if self.video_ram[base_address as usize] & sx != 0 { 1 } else { 0 } + if self.video_ram[(base_address + 1) as usize] & sx != 0 { 2 } else { 0 };

      self.tileset[tile as usize][y as usize][x as usize] = pixel_colour;
    }
  }

  // TODO update all comments
  // NOTE borrowed from github.com/alexcrichton/jba
  pub fn render_scanline(&mut self) {
    // tiles: 8x8 pixels
    // two maps: 32x32 each

    let mapbase: usize = if self.background_map { 0x1C00 } else { 0x1800 };
    let line = self.line as usize + self.scroll_y as usize;

    let mapbase = mapbase + ((line % 256) >> 3) * 32;

    let y = (self.line + self.scroll_y) % 8;
    let mut x = self.scroll_x % 8;

    let mut coff = (self.line as usize) * 160 * 4;

    let mut i = 0;
    let tilebase = if !self.background_tile {256} else {0};

    loop {
      let mapoff = ((i as usize + self.scroll_x as usize) % 256) >> 3;
      let tilei = self.video_ram[mapbase + mapoff];

      let tilebase = if self.background_tile {
        tilebase + tilei as usize
      } else {
        (tilebase as isize + (tilei as i8 as isize)) as usize
      };

      let row;
      row = self.tileset[tilebase as usize][y as usize];

      while x < 8 && i < 160 as u8 {
        let colori = row[x as usize];
        let color = self.palette[colori as usize];

        self.framebuffer[coff] = color[0];
        self.framebuffer[coff + 1] = color[1];
        self.framebuffer[coff + 2] = color[2];
        self.framebuffer[coff + 3] = color[3];

        x += 1;
        i += 1;
        coff += 4;
      }

      x = 0;
      if i >= 160 as u8 { break }
    }
  }

  pub fn render_screen(&mut self) {
    for y in 0..144 {
      for mut x in 0..160 {
        let framebuffer_index = (y * 4) * 160 + (x * 4);

        let pixel_r = self.framebuffer[framebuffer_index as usize];
        let pixel_g = self.framebuffer[framebuffer_index as usize + 1];
        let pixel_b = self.framebuffer[framebuffer_index as usize + 2];
        let pixel_a = self.framebuffer[framebuffer_index as usize + 3];

        self.game_renderer.set_draw_color(pixels::Color::RGBA(pixel_r, pixel_g, pixel_b, pixel_a));
        self.game_renderer.draw_point(sdl2::rect::Point::new(x, y)); // TODO benchmark draw_points
      }
    }

    self.game_renderer.present();
  }

  pub fn show_debug_tiles(&mut self) {
    for tile_y in 0..17 {
      for tile_x in 0..17 {
        for y in 0..8 {
          for x in 0..8 {
            let target_tile = tile_x + (tile_y * 18);
            let pixel_palette = self.palette[self.tileset[target_tile as usize][y as usize][x as usize] as usize];

            self.debug_renderer.set_draw_color(pixels::Color::RGBA(pixel_palette[0], pixel_palette[1], pixel_palette[2], pixel_palette[3]));
            self.debug_renderer.draw_point(sdl2::rect::Point::new(x + (tile_x * 8), y + (tile_y * 8)));
          }
        }
      }
    }

    self.debug_renderer.present();
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
          self.render_scanline();
        }
      },
      0 => {
        // Hblank
	      // After the last hblank, push the screen data to framebuffer
        if self.mode_clock >= 204 {
          self.mode_clock = 0;
          self.line += 1;

          if self.line == 143 {
            // Enter vblank
            self.mode = 1;
            self.render_screen();
            self.show_debug_tiles();
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
