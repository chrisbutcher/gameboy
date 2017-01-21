pub use super::types;

// PPU supports tiles: 8x8 pixel groups
// Modes: Sprite Read, Video Read, Horizontal Blank, Vertical Blank
// Starts in vertical blank
pub struct PPU {
  // Framebuffer -- 3d array of pixels. 160 x 144 x 3 (3 bytes to support GBC, first byte for b&w)
  framebuffer: [[[types::Byte; 160]; 144]; 3]
}

impl PPU {
  pub fn new() -> PPU {
    PPU{framebuffer: [[[0x00; 160]; 144]; 3]}
  }
}
