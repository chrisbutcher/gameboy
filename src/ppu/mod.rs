pub use super::types;
pub use super::sdl2;
pub use super::sdl2::pixels;
pub use super::sdl2::render::Renderer;

// PPU supports tiles: 8x8 pixel groups
// Modes: Sprite Read, Video Read, Horizontal Blank, Vertical Blank
// Starts in vertical blank
pub struct PPU {
  // Framebuffer -- 3d array of pixels. 160 x 144 x 4 (-- previous NOTE 3 bytes to support GBC, first byte for b&w)
  // pub framebuffer: [[[types::Byte; 160]; 144]; 4],
  // pub framebuffer: [[[types::Byte; 4]; 144]; 160],
  pub framebuffer: [types::Byte; 160 * 144 * 4],
  pub video_ram: Vec<types::Byte>,
  pub tileset: [[[types::Byte; 8]; 8]; 512], // HACK 512 should be 384
  pub palette: [[types::Byte; 4]; 4],

  pub mode: u8,
  pub mode_clock: i32,
  pub line: u8,
  pub scroll_x: u8,
  pub scroll_y: u8,
  pub background_map: bool,
  pub background_tile: u8,

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
      framebuffer: [0x00; 160 * 144 * 4],
      video_ram: vec![0x00; 0x2000],
      tileset: [[[0x00; 8]; 8]; 512],
      palette: [
        [255, 255, 255, 255], // RGBA, TODO simplify to RGB
        [192, 192, 192, 192],
        [96, 96, 96, 96],
        [0, 0, 0, 0],
      ],

      mode: 0,
      mode_clock: 0,
      line: 0,
      scroll_x: 0,
      scroll_y: 0,
      background_map: false,
      background_tile: 0,

      sdl_context: sdl_context,
      renderer: renderer,
    }
  }

  pub fn read(&mut self, address: types::Word) -> types::Byte {
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
        panic!("Unexpected address in PPU: {:#X}", address);
      }
    }
  }

  // TODO reword all comments
  pub fn update_tile(&mut self, address: types::Word, value: types::Byte) {
    // Get the "base address" for this tile row

    // println!("address {:#X}", address);
    let base_address = address & 0x1FFE;
    // println!("base_address {:#X}", base_address);

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
  pub fn render_scanline(&mut self) {
    // tiles: 8x8 pixels
    // two maps: 32x32 each

    // VRAM offset for the tile map
    let mut tile_map_offset = if self.background_map { 0x1C00 } else { 0x1800 };

    // Which line of tiles to use in the map
    tile_map_offset += ((self.line + self.scroll_y) & 255) >> 3;

    // Which tile to start with in the map line
    let mut line_offset = self.scroll_x >> 3;

    // Which line of pixels to use in the tiles
    let mut y = (self.line + self.scroll_y) & 7;

    // Where in the tileline to start
    let mut x = self.scroll_x & 7;

    // Where to render on the framebuffer
    let mut framebuffer_offset = self.line.wrapping_mul(160).wrapping_mul(4);

    // Read tile index from the background map
    let mut tile_index = self.video_ram[(tile_map_offset + line_offset) as usize];

    // If the tile data set in use is #1, the
	  // indices are signed; calculate a real tile offset
    if self.background_tile == 1 && tile_index < 128 {
      tile_index += 256;
    }

    for i in 0..160 {
      // Re-map the tile pixel through the palette
      let colour = self.palette[self.tileset[tile_index as usize][y as usize][x as usize] as usize];

      if colour[0] != 0xFF {
        println!("Got colour other than white {:#X}", colour[0]);
      }      

      // Plot the pixel to canvas
      self.framebuffer[framebuffer_offset as usize + 0] = colour[0];
      self.framebuffer[framebuffer_offset as usize + 1] = colour[1];
      self.framebuffer[framebuffer_offset as usize + 2] = colour[2];
      self.framebuffer[framebuffer_offset as usize + 3] = colour[3];
      framebuffer_offset = framebuffer_offset.wrapping_add(4);

      // When this tile ends, read another
      x += 1;
      if x == 8 {
        x = 0;
        line_offset = (line_offset + 1) & 31;
        tile_index = self.video_ram[(tile_map_offset as usize + line_offset as usize) as usize];
        if self.background_tile == 1 && tile_index < 128 {
          tile_index += 256;
        }
      }
    }

  }

  pub fn render_screen(&mut self) {
    for y in 0..144 {
      for x in 0..160 {
        let framebuffer_index = y * 160 + x;

        let pixel_r = self.framebuffer[framebuffer_index as usize];
        let pixel_g = self.framebuffer[framebuffer_index as usize + 1];
        let pixel_b = self.framebuffer[framebuffer_index as usize + 2];
        let pixel_a = self.framebuffer[framebuffer_index as usize + 3];

        self.renderer.set_draw_color(pixels::Color::RGBA(pixel_r, pixel_g, pixel_b, pixel_a));
        self.renderer.draw_point(sdl2::rect::Point::new(x, y)); // TODO benchmark draw_points
      }
    }

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
          self.render_scanline();
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
