pub use super::sdl2;
pub use super::sdl2::pixels;
pub use super::sdl2::render::WindowCanvas;
pub use super::sdl2::video::WindowPos;

extern crate socket_state_reporter;
use self::socket_state_reporter::StateReporter;

const SYNC_STATE: bool = false;
const RENDER_PIXELS: bool = true;

#[derive(Clone, Copy, Debug)]
pub struct Sprite {
  y_pos: i32, // TODO i8 or i16?
  x_pos: i32,
  tile: u8,

  priority_behind_bg: bool,
  y_flip: bool,
  x_flip: bool,
  use_palette_1: bool,

  index: usize,
}

impl Sprite {
  fn new() -> Sprite {
    Sprite {
      y_pos: 0x00,
      x_pos: 0x00,
      tile: 0x00,

      priority_behind_bg: false,
      y_flip: false,
      x_flip: false,
      use_palette_1: false,

      index: 0,
    }
  }
}

pub struct PPU {
  pub video_ram: Vec<u8>,
  pub sdl_context: sdl2::Sdl,
  pub interrupt_flags: u8,

  framebuffer: [ u8; 160 * 144 * 4 ],
  tileset: [ [ [ u8; 8 ]; 8 ]; 384 ],
  palette: [ [ u8; 4 ]; 4 ],

  sprites: [ Sprite; 40 ],

  mode: u8,
  mode_clock: i32,
  line: u8,
  scroll_x: u8,
  scroll_y: u8,

  lcdc_display_enabled: bool,
  lcdc_window_tilemap: bool,
  lcdc_window_enabled: bool,
  lcdc_bg_and_windown_tile_base: bool,
  lcdc_bg_tilemap_base: bool,
  lcdc_obj_sprite_size: bool,
  lcdc_obj_sprite_display_enabled: bool,
  lcdc_bg_enabled: bool,

  ly_coincidence: u8,
  ly_coincidence_interrupt_enabled: bool,
  mode_0_interrupt_enabled: bool,
  mode_1_interrupt_enabled: bool,
  mode_2_interrupt_enabled: bool,

  horiz_blanking: bool,

  game_canvas: Option<WindowCanvas>,
  debug_canvas: Option<WindowCanvas>,

  state_reporter: StateReporter,
  tick_counter: u64,
}

impl PPU {
  pub fn new() -> PPU {
    let sdl_context = sdl2::init().unwrap();
    let debug_canvas;
    let game_canvas;

    let sdl_context = if RENDER_PIXELS {
      let (debug_window, sdl_context) = PPU::new_window(sdl_context, "DEBUG", 192, 192, 160);
      let (game_window, sdl_context) = PPU::new_window(sdl_context, "GAMEBOY", 160, 144, 0);

      debug_canvas = Some(debug_window.into_canvas().accelerated().present_vsync().build().unwrap());
      game_canvas = Some(game_window.into_canvas().accelerated().present_vsync().build().unwrap());

      sdl_context
    } else {
      debug_canvas = None;
      game_canvas = None;

      sdl_context
    };

    PPU {
      framebuffer: [ 0x00; 160 * 144 * 4 ],
      video_ram: vec![ 0x00; 0x2000 ],
      tileset: [ [ [ 0x00; 8 ]; 8 ]; 384 ],
      palette: [ [ 255, 255, 255, 255 ], // RGBA, TODO simplify to RGB
                 [ 192, 192, 192, 255 ],
                 [ 96, 96, 96, 255 ],
                 [ 0, 0, 0, 255 ] ],

      sprites: [Sprite::new(); 40],

      mode: 0,
      mode_clock: 0,
      line: 0,
      scroll_x: 0,
      scroll_y: 0,

      lcdc_display_enabled: false,
      lcdc_window_tilemap: true,
      lcdc_window_enabled: false,
      lcdc_bg_and_windown_tile_base: true,
      lcdc_bg_tilemap_base: true,
      lcdc_obj_sprite_size: false,
      lcdc_obj_sprite_display_enabled: false,
      lcdc_bg_enabled: false,

      ly_coincidence: 0x00,
      ly_coincidence_interrupt_enabled: false,
      mode_0_interrupt_enabled: false,
      mode_1_interrupt_enabled: false,
      mode_2_interrupt_enabled: false,

      horiz_blanking: false,

      interrupt_flags: 0x00,

      sdl_context: sdl_context,
      game_canvas: game_canvas,
      debug_canvas: debug_canvas,

      state_reporter: StateReporter::new("5555"),
      tick_counter: 0,
    }
  }

  fn new_window(sdl_context: sdl2::Sdl, title: &str, width: u32, height: u32, x_offset: i32) -> (sdl2::video::Window, sdl2::Sdl) {
    let video_subsys = sdl_context.video().unwrap();
    let mut window = video_subsys.window(title, width, height).position_centered().opengl().build().unwrap();

    let position = window.position();
    window.set_position(WindowPos::Positioned(position.0 + x_offset), WindowPos::Positioned(position.1));

    (window, sdl_context)
  }

  pub fn read(&mut self, address: u16) -> u8 {
    match address {
      0xFF40 => {
        (if self.lcdc_display_enabled { 0b1000_0000 } else { 0 }) |
          (if self.lcdc_window_tilemap { 0b0100_0000 } else { 0 }) |
          (if self.lcdc_window_enabled { 0b0010_0000 } else { 0 }) |
          (if self.lcdc_bg_and_windown_tile_base { 0b0001_0000 } else { 0 }) |
          (if self.lcdc_bg_tilemap_base { 0b0000_1000 } else { 0 }) |
          (if self.lcdc_obj_sprite_size { 0b0000_0100 } else { 0 }) |
          (if self.lcdc_obj_sprite_display_enabled { 0b0000_0010 } else { 0 }) |
          (if self.lcdc_bg_enabled { 0b0000_0001 } else { 0 })
      },
      0xFF41 => {
        // TODO rename all
        let ff41_val = (if self.ly_coincidence_interrupt_enabled { 0x40 } else { 0 }) |
          (if self.mode_2_interrupt_enabled { 0x20 } else { 0 }) |
          (if self.mode_1_interrupt_enabled { 0x10 } else { 0 }) |
          (if self.mode_0_interrupt_enabled { 0x08 } else { 0 }) |
          (if self.line == self.ly_coincidence { 0x04 } else { 0 }) |
          self.mode;

        ff41_val
      }
      0xFF42 => self.scroll_y as u8,
      0xFF43 => {
        self.scroll_x
      },
      0xFF44 => {
        self.line
      },
      0xFF45 => {
        self.ly_coincidence
      }
      _ => {
        panic!("Unexpected address in PPU#read: {:#X}", address);
      }
    }
  }

  pub fn write(&mut self, address: u16, value: u8) {
    match address {
      0xFF40 => {
        let previous_lcdc_display_enabled = self.lcdc_display_enabled;

        self.lcdc_display_enabled = value & 0b1000_0000 != 0;
        self.lcdc_window_tilemap = value & 0b0100_0000 != 0;
        self.lcdc_window_enabled = value & 0b0010_0000 != 0;
        self.lcdc_bg_and_windown_tile_base = value & 0b0001_0000 != 0;
        self.lcdc_bg_tilemap_base = value & 0b0000_1000 != 0;
        self.lcdc_obj_sprite_size = value & 0b0000_0100 != 0;
        self.lcdc_obj_sprite_display_enabled = value & 0b0000_0010 != 0;
        self.lcdc_bg_enabled = value & 0b0000_0001 != 0;

        if previous_lcdc_display_enabled && !self.lcdc_display_enabled {
          self.mode_clock = 0;
          self.line = 0;
          self.mode = 0;
        }
      },
      0xFF41 => {
        self.ly_coincidence_interrupt_enabled = value & 0x40 == 0x40;
        self.mode_2_interrupt_enabled = value & 0x20 == 0x20;
        self.mode_1_interrupt_enabled = value & 0x10 == 0x10;
        self.mode_0_interrupt_enabled = value & 0x08 == 0x08;
      }
      0xFF42 => self.scroll_y = value,
      0xFF43 => self.scroll_x = value,
      0xFF44 => {
        panic!("Writing to LY in PPU#write: {:#X}", address);
      }
      0xFF45 => self.ly_coincidence = value,
      0xFF47 => {
        for i in 0..4 {
          match (value >> (i * 2)) & 3 {
            0 => self.palette[ i ] = [ 255, 255, 255, 255 ],
            1 => self.palette[ i ] = [ 192, 192, 192, 255 ],
            2 => self.palette[ i ] = [ 96, 96, 96, 255 ],
            3 => self.palette[ i ] = [ 0, 0, 0, 255 ],
            _ => {
              panic!("Unexpected background palette value: {:#X}", i);
            }
          }
        }
      },
      0xFF48 => {
        println!("TODO, writing to FF48")
      }
      0xFF49 => {
        println!("TODO, writing to FF49")
      }
      0xFF4A => {
        println!("TODO, writing to FF4A")
      }
      0xFF4B => {
        println!("TODO, writing to FF4B")
      }
      _ => {
        // panic!("Writing to PPU {:#x}", address);
      }
    }
  }

  pub fn update_sprite_object(&mut self, sprite_addr: usize, value: u8) {
    let sprite_index = sprite_addr >> 2;
    let byte = sprite_addr & 3;

    match byte {
      0x00 => { self.sprites[sprite_index].y_pos = value as i32 - 16 },
      0x01 => { self.sprites[sprite_index].x_pos = value as i32 - 8 },
      0x02 => { self.sprites[sprite_index].tile = value },
      0x03 => {
        self.sprites[sprite_index].priority_behind_bg = (value & 0b1000_0000) != 0;
        self.sprites[sprite_index].y_flip = (value & 0b0100_0000) != 0;
        self.sprites[sprite_index].x_flip = (value & 0b0010_0000) != 0;
        self.sprites[sprite_index].use_palette_1 = (value & 0b0001_0000) != 0;
      },
      _ => { panic!("Invalid byte in update_sprite_object") }
    }
  }

  // TODO optimize
  pub fn update_tile(&mut self, address: u16, value: u8) {
    if !RENDER_PIXELS { return }

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
      let sx = 1 << (7 - x);

      // Update tile set
      let pixel_colour = if self.video_ram[ base_address as usize ] & sx != 0 { 1 } else { 0 } + if self.video_ram[ (base_address + 1) as usize ] & sx != 0 { 2 } else { 0 };

      self.tileset[ tile as usize ][ y as usize ][ x as usize ] = pixel_colour;
    }
  }

  fn render_scanline(&mut self) {
    self.render_background();
    self.render_sprites();
  }

  fn render_sprites(&mut self) {
    if !RENDER_PIXELS { return }

    for sprite in self.sprites.iter() {
      let line = self.line as i32;

      if self.lcdc_obj_sprite_size {
        panic!("Double-sized sprites not yet supported");
      }

      // If the sprite falls within the scanline
      // TODO account for double-sized sprites
      if sprite.y_pos <= line && (sprite.y_pos + 8 ) > line {
        let mut canvas_offset = ((line * 160) + sprite.x_pos) * 4;
        let tile_row;

        if sprite.y_flip {
          tile_row = self.tileset[ sprite.tile as usize ][ 7 - (line - sprite.y_pos) as usize ];
        } else {
          tile_row = self.tileset[ sprite.tile as usize ][ (line - sprite.y_pos) as usize ];
        }

        let mut colour;

        for x in 0..8 {
          // TODO don't draw pixel if OAM doesn't have priority over BG and BG has non-zero colour pixel already drawn
          if sprite.x_pos + x >= 0 && sprite.x_pos + x < 160 {
            let palette_index = if sprite.x_flip { 7 - x as usize } else { x as usize };
            colour = self.palette[ tile_row[ palette_index ] as usize ];

            self.framebuffer[ canvas_offset as usize ] = colour[ 0 ];
            self.framebuffer[ canvas_offset as usize + 1 ] = colour[ 1 ];
            self.framebuffer[ canvas_offset as usize + 2 ] = colour[ 2 ];
            self.framebuffer[ canvas_offset as usize + 3 ] = colour[ 3 ];

            canvas_offset += 4;
          }
        }
      }
    }
  }

  // NOTE borrowed from github.com/alexcrichton/jba
  fn render_background(&mut self) {
    if !RENDER_PIXELS { return }

    // tiles: 8x8 pixels
    // two maps: 32x32 each

    let mapbase: usize = if self.lcdc_bg_tilemap_base { 0x1C00 } else { 0x1800 };
    let line = self.line as usize + self.scroll_y as usize;

    let mapbase = mapbase + ((line % 256) >> 3) * 32;

    let y = (self.line.wrapping_add(self.scroll_y)) % 8;
    let mut x = self.scroll_x % 8;

    let mut canvas_offset = (self.line as usize) * 160 * 4;

    let mut i = 0;
    let tilebase = if !self.lcdc_bg_and_windown_tile_base { 256 } else { 0 };

    loop {
      let mapoff = ((i as usize + self.scroll_x as usize) % 256) >> 3;
      let tilei = self.video_ram[ mapbase + mapoff ];

      let tilebase = if self.lcdc_bg_and_windown_tile_base { tilebase + tilei as usize } else { (tilebase as isize + (tilei as i8 as isize)) as usize };

      let row;
      row = self.tileset[ tilebase as usize ][ y as usize ];

      while x < 8 && i < 160 as u8 {
        let palette_index = row[ x as usize ];
        let colour = self.palette[ palette_index as usize ];

        self.framebuffer[ canvas_offset ] = colour[ 0 ];
        self.framebuffer[ canvas_offset + 1 ] = colour[ 1 ];
        self.framebuffer[ canvas_offset + 2 ] = colour[ 2 ];
        self.framebuffer[ canvas_offset + 3 ] = colour[ 3 ];

        x += 1;
        i += 1;
        canvas_offset += 4;
      }

      x = 0;
      if i >= 160 as u8 {
        break;
      }
    }
  }

  // Perf: Profiled with profile.release setting in Cargo.toml, and used XCode tool Instruments. Found that draw_point and Point::new dominated
  pub fn render_screen(&mut self) {
    if !RENDER_PIXELS { return }

    for y in 0..144 {
      for x in 0..160 {
        let framebuffer_index = (y * 4) * 160 + (x * 4);

        let pixel_r = self.framebuffer[ framebuffer_index as usize ];
        let pixel_g = self.framebuffer[ framebuffer_index as usize + 1 ];
        let pixel_b = self.framebuffer[ framebuffer_index as usize + 2 ];
        let pixel_a = self.framebuffer[ framebuffer_index as usize + 3 ];

        match self.game_canvas {
          Some(ref mut canvas) => {
            canvas.set_draw_color(pixels::Color::RGBA(pixel_r, pixel_g, pixel_b, pixel_a));
            canvas.draw_point(sdl2::rect::Point::new(x, y)).unwrap()
          }
          _ => {}
        }
      }
    }

    match self.game_canvas {
      Some(ref mut canvas) => { canvas.present() },
      _ => {}
    }
  }

  pub fn show_debug_tiles(&mut self) {
    if !RENDER_PIXELS { return }

    for tile_y in 0..17 {
      for tile_x in 0..17 {
        for y in 0..8 {
          for x in 0..8 {
            let target_tile = tile_x + (tile_y * 18);
            let pixel_palette = self.palette[ self.tileset[ target_tile as usize ][ y as usize ][ x as usize ] as usize ];

            match self.debug_canvas {
              Some(ref mut canvas) => {
                canvas.set_draw_color(
                  pixels::Color::RGBA(pixel_palette[ 0 ], pixel_palette[ 1 ], pixel_palette[ 2 ], pixel_palette[ 3 ]),
                );
                canvas.draw_point(sdl2::rect::Point::new(x + (tile_x * 8), y + (tile_y * 8))).unwrap()
              },
              _ => {}
            }
          }
        }
      }
    }

    match self.debug_canvas {
      Some(ref mut canvas) => {
        canvas.present()
      },
      _ => {}
    }
  }

  pub fn tick(&mut self, cycles: i32) {
    self.tick_counter += 1;

    if SYNC_STATE {
      self.state_reporter.send_message(
        format!("tick_counter: {}, cycles: {}, line: {}, mode_clock: {}, mode: {}, switch_lcd: {}",
          self.tick_counter, cycles, self.line, self.mode_clock, self.mode, self.lcdc_display_enabled).as_bytes()
        );
      let received = self.state_reporter.receive_message();
      if received == "kill" {
        panic!("Server stopped.");
      }
    }

    if !self.lcdc_display_enabled { return }
    self.horiz_blanking = false;

    let mut ticks_remaining = cycles;

    while ticks_remaining > 0 {
      let current_ticks = if ticks_remaining >= 80 { 80 } else { ticks_remaining };
      self.mode_clock += current_ticks;
      ticks_remaining -= current_ticks;

      if self.mode_clock >= 456 {
        self.mode_clock -= 456;
        self.line = (self.line + 1) % 154;
        if self.ly_coincidence_interrupt_enabled && self.line == self.ly_coincidence {
          self.interrupt_flags |= 0x02;
        }

        if self.line >= 144 && self.mode != 1 {
          self.change_mode(1);
        }
      }

      if self.line < 144 {
        if self.mode_clock <= 80 {
          if self.mode != 2 { self.change_mode(2); }
        } else if self.mode_clock <= (80 + 172) {
          if self.mode != 3 { self.change_mode(3); }
        } else {
          if self.mode != 0 { self.change_mode(0); }
        }
      }
    }
  }

  fn change_mode(&mut self, mode: u8) {
    self.mode = mode;

    if match self.mode {
      0 => {
        self.render_scanline();
        self.horiz_blanking = true;
        self.mode_0_interrupt_enabled
      },
      1 => {
        self.interrupt_flags |= 0x01;
        self.mode_1_interrupt_enabled
      },
      2 => self.mode_2_interrupt_enabled,
      _ => false,
    } {
      self.interrupt_flags |= 0x02;
    }
  }
}
