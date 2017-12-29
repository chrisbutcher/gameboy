pub use super::sdl2;
pub use super::sdl2::pixels;
pub use super::sdl2::pixels::PixelFormatEnum;
pub use super::sdl2::rect::Rect;

pub use super::sdl2::render::{Texture, TextureAccess, TextureCreator, WindowCanvas};
pub use super::sdl2::video::{WindowContext, WindowPos};

const RENDER_PIXELS: bool = true;

pub struct WindowSet {
  pub sdl_context: sdl2::Sdl,

  game_canvas: WindowCanvas,
  debug_canvas: WindowCanvas,
}

impl WindowSet {
  pub fn new() -> WindowSet {
    let sdl_context = sdl2::init().unwrap();

    let (debug_window, sdl_context) = WindowSet::new_window(sdl_context, "DEBUG", 192, 192, 160);
    let (game_window, sdl_context) = WindowSet::new_window(sdl_context, "GAMEBOY", 160, 144, 0);

    let game_canvas = game_window.into_canvas().accelerated().present_vsync().build().unwrap();

    WindowSet {
      sdl_context: sdl_context,
      game_canvas: game_canvas,
      debug_canvas: debug_window.into_canvas().accelerated().present_vsync().build().unwrap()
    }
  }

  fn new_window(sdl_context: sdl2::Sdl, title: &str, width: u32, height: u32, x_offset: i32) -> (sdl2::video::Window, sdl2::Sdl) {
    let video_subsys = sdl_context.video().unwrap();
    let mut window = video_subsys.window(title, width, height).position_centered().opengl().build().unwrap();

    let position = window.position();
    window.set_position(WindowPos::Positioned(position.0 + x_offset), WindowPos::Positioned(position.1));

    (window, sdl_context)
  }

  // Perf, in previous iteration: Profiled with profile.release setting in Cargo.toml, and used XCode tool Instruments.
  // Found that draw_point and Point::new dominated

  pub fn render_screen(&mut self, framebuffer: &[u8]) {
    if !RENDER_PIXELS { return }

    let game_texture_creator = self.game_canvas.texture_creator();
    let mut game_texture = game_texture_creator.create_texture(PixelFormatEnum::ABGR8888, TextureAccess::Target, 160, 144).unwrap();

    self.game_canvas.clear();
    game_texture.update(Rect::new(0, 0, 160, 144), framebuffer, 640).unwrap();
    self.game_canvas.copy(&game_texture, None, Some(Rect::new(0, 0, 160, 144))).unwrap();
    self.game_canvas.present();
  }

  pub fn show_debug_tiles(&mut self) {
    if !RENDER_PIXELS { return }

    // for tile_y in 0..17 {
    //   for tile_x in 0..17 {
    //     for y in 0..8 {
    //       for x in 0..8 {
    //         let target_tile = tile_x + (tile_y * 18);
            // let pixel_palette = self.palette[ self.tileset[ target_tile as usize ][ y as usize ][ x as usize ] as usize ];

            // match self.debug_canvas {
            //   Some(ref mut canvas) => {
            //     canvas.set_draw_color(
            //       pixels::Color::RGBA(pixel_palette[ 0 ], pixel_palette[ 1 ], pixel_palette[ 2 ], pixel_palette[ 3 ]),
            //     );
            //     canvas.draw_point(sdl2::rect::Point::new(x + (tile_x * 8), y + (tile_y * 8))).unwrap()
            //   },
            //   _ => {}
            // }
    //       }
    //     }
    //   }
    // }

    self.debug_canvas.present()
  }
}
