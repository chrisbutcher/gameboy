// opcode cycles
pub fn regular(opcode: u8) -> i32 {
  match opcode {
    0x00 => 1,
    0x01 => 3,
    0x02 => 2,
    0x03 => 2,
    0x04 => 1,
    0x05 => 1,
    0x06 => 2,
    0x07 => 1,
    0x08 => 5,
    0x09 => 2,
    0x0A => 2,
    0x0B => 2,
    0x0C => 1,
    0x0D => 1,
    0x0E => 2,
    0x0F => 1,
    0x10 => 1,
    0x11 => 3,
    0x12 => 2,
    0x13 => 2,
    0x14 => 1,
    0x15 => 1,
    0x16 => 2,
    0x17 => 1,
    0x18 => 3,
    0x19 => 2,
    0x1A => 2,
    0x1B => 2,
    0x1C => 1,
    0x1D => 1,
    0x1E => 2,
    0x1F => 1,
    0x20 => 2,
    0x21 => 3,
    0x22 => 2,
    0x23 => 2,
    0x24 => 1,
    0x25 => 1,
    0x26 => 2,
    0x27 => 1,
    0x28 => 2,
    0x29 => 2,
    0x2A => 2,
    0x2B => 2,
    0x2C => 1,
    0x2D => 1,
    0x2E => 2,
    0x2F => 1,
    0x30 => 2,
    0x31 => 3,
    0x32 => 2,
    0x33 => 2,
    0x34 => 3,
    0x35 => 3,
    0x36 => 3,
    0x37 => 1,
    0x38 => 2,
    0x39 => 2,
    0x3A => 2,
    0x3B => 2,
    0x3C => 1,
    0x3D => 1,
    0x3E => 2,
    0x3F => 1,
    0x40 => 1,
    0x41 => 1,
    0x42 => 1,
    0x43 => 1,
    0x44 => 1,
    0x45 => 1,
    0x46 => 2,
    0x47 => 1,
    0x48 => 1,
    0x49 => 1,
    0x4A => 1,
    0x4B => 1,
    0x4C => 1,
    0x4D => 1,
    0x4E => 2,
    0x4F => 1,
    0x50 => 1,
    0x51 => 1,
    0x52 => 1,
    0x53 => 1,
    0x54 => 1,
    0x55 => 1,
    0x56 => 2,
    0x57 => 1,
    0x58 => 1,
    0x59 => 1,
    0x5A => 1,
    0x5B => 1,
    0x5C => 1,
    0x5D => 1,
    0x5E => 2,
    0x5F => 1,
    0x60 => 1,
    0x61 => 1,
    0x62 => 1,
    0x63 => 1,
    0x64 => 1,
    0x65 => 1,
    0x66 => 2,
    0x67 => 1,
    0x68 => 1,
    0x69 => 1,
    0x6A => 1,
    0x6B => 1,
    0x6C => 1,
    0x6D => 1,
    0x6E => 2,
    0x6F => 1,
    0x70 => 2,
    0x71 => 2,
    0x72 => 2,
    0x73 => 2,
    0x74 => 2,
    0x75 => 2,
    0x76 => 1,
    0x77 => 2,
    0x78 => 1,
    0x79 => 1,
    0x7A => 1,
    0x7B => 1,
    0x7C => 1,
    0x7D => 1,
    0x7E => 2,
    0x7F => 1,
    0x80 => 1,
    0x81 => 1,
    0x82 => 1,
    0x83 => 1,
    0x84 => 1,
    0x85 => 1,
    0x86 => 2,
    0x87 => 1,
    0x88 => 1,
    0x89 => 1,
    0x8A => 1,
    0x8B => 1,
    0x8C => 1,
    0x8D => 1,
    0x8E => 2,
    0x8F => 1,
    0x90 => 1,
    0x91 => 1,
    0x92 => 1,
    0x93 => 1,
    0x94 => 1,
    0x95 => 1,
    0x96 => 2,
    0x97 => 1,
    0x98 => 1,
    0x99 => 1,
    0x9A => 1,
    0x9B => 1,
    0x9C => 1,
    0x9D => 1,
    0x9E => 2,
    0x9F => 1,
    0xA0 => 1,
    0xA1 => 1,
    0xA2 => 1,
    0xA3 => 1,
    0xA4 => 1,
    0xA5 => 1,
    0xA6 => 2,
    0xA7 => 1,
    0xA8 => 1,
    0xA9 => 1,
    0xAA => 1,
    0xAB => 1,
    0xAC => 1,
    0xAD => 1,
    0xAE => 2,
    0xAF => 1,
    0xB0 => 1,
    0xB1 => 1,
    0xB2 => 1,
    0xB3 => 1,
    0xB4 => 1,
    0xB5 => 1,
    0xB6 => 2,
    0xB7 => 1,
    0xB8 => 1,
    0xB9 => 1,
    0xBA => 1,
    0xBB => 1,
    0xBC => 1,
    0xBD => 1,
    0xBE => 2,
    0xBF => 1,
    0xC0 => 2,
    0xC1 => 3,
    0xC2 => 3,
    0xC3 => 4,
    0xC4 => 3,
    0xC5 => 4,
    0xC6 => 2,
    0xC7 => 4,
    0xC8 => 2,
    0xC9 => 4,
    0xCA => 3,
    0xCB => 0,
    0xCC => 3,
    0xCD => 6,
    0xCE => 2,
    0xCF => 4,
    0xD0 => 2,
    0xD1 => 3,
    0xD2 => 3,
    0xD3 => 0,
    0xD4 => 3,
    0xD5 => 4,
    0xD6 => 2,
    0xD7 => 4,
    0xD8 => 2,
    0xD9 => 4,
    0xDA => 3,
    0xDB => 0,
    0xDC => 3,
    0xDD => 0,
    0xDE => 2,
    0xDF => 4,
    0xE0 => 3,
    0xE1 => 3,
    0xE2 => 2,
    0xE3 => 0,
    0xE4 => 0,
    0xE5 => 4,
    0xE6 => 2,
    0xE7 => 4,
    0xE8 => 4,
    0xE9 => 1,
    0xEA => 4,
    0xEB => 0,
    0xEC => 0,
    0xED => 0,
    0xEE => 2,
    0xEF => 4,
    0xF0 => 3,
    0xF1 => 3,
    0xF2 => 2,
    0xF3 => 1,
    0xF4 => 0,
    0xF5 => 4,
    0xF6 => 2,
    0xF7 => 4,
    0xF8 => 3,
    0xF9 => 2,
    0xFA => 4,
    0xFB => 1,
    0xFC => 0,
    0xFD => 0,
    0xFE => 2,
    0xFF => 4,
    _ => panic!("Unexpected opcode: {:#X}", opcode),
  }
}

pub fn branch(opcode: u8) -> i32 {
  match opcode {
    0x00 => 1,
    0x01 => 3,
    0x02 => 2,
    0x03 => 2,
    0x04 => 1,
    0x05 => 1,
    0x06 => 2,
    0x07 => 1,
    0x08 => 5,
    0x09 => 2,
    0x0A => 2,
    0x0B => 2,
    0x0C => 1,
    0x0D => 1,
    0x0E => 2,
    0x0F => 1,
    0x10 => 1,
    0x11 => 3,
    0x12 => 2,
    0x13 => 2,
    0x14 => 1,
    0x15 => 1,
    0x16 => 2,
    0x17 => 1,
    0x18 => 3,
    0x19 => 2,
    0x1A => 2,
    0x1B => 2,
    0x1C => 1,
    0x1D => 1,
    0x1E => 2,
    0x1F => 1,
    0x20 => 3,
    0x21 => 3,
    0x22 => 2,
    0x23 => 2,
    0x24 => 1,
    0x25 => 1,
    0x26 => 2,
    0x27 => 1,
    0x28 => 3,
    0x29 => 2,
    0x2A => 2,
    0x2B => 2,
    0x2C => 1,
    0x2D => 1,
    0x2E => 2,
    0x2F => 1,
    0x30 => 3,
    0x31 => 3,
    0x32 => 2,
    0x33 => 2,
    0x34 => 3,
    0x35 => 3,
    0x36 => 3,
    0x37 => 1,
    0x38 => 3,
    0x39 => 2,
    0x3A => 2,
    0x3B => 2,
    0x3C => 1,
    0x3D => 1,
    0x3E => 2,
    0x3F => 1,
    0x40 => 1,
    0x41 => 1,
    0x42 => 1,
    0x43 => 1,
    0x44 => 1,
    0x45 => 1,
    0x46 => 2,
    0x47 => 1,
    0x48 => 1,
    0x49 => 1,
    0x4A => 1,
    0x4B => 1,
    0x4C => 1,
    0x4D => 1,
    0x4E => 2,
    0x4F => 1,
    0x50 => 1,
    0x51 => 1,
    0x52 => 1,
    0x53 => 1,
    0x54 => 1,
    0x55 => 1,
    0x56 => 2,
    0x57 => 1,
    0x58 => 1,
    0x59 => 1,
    0x5A => 1,
    0x5B => 1,
    0x5C => 1,
    0x5D => 1,
    0x5E => 2,
    0x5F => 1,
    0x60 => 1,
    0x61 => 1,
    0x62 => 1,
    0x63 => 1,
    0x64 => 1,
    0x65 => 1,
    0x66 => 2,
    0x67 => 1,
    0x68 => 1,
    0x69 => 1,
    0x6A => 1,
    0x6B => 1,
    0x6C => 1,
    0x6D => 1,
    0x6E => 2,
    0x6F => 1,
    0x70 => 2,
    0x71 => 2,
    0x72 => 2,
    0x73 => 2,
    0x74 => 2,
    0x75 => 2,
    0x76 => 1,
    0x77 => 2,
    0x78 => 1,
    0x79 => 1,
    0x7A => 1,
    0x7B => 1,
    0x7C => 1,
    0x7D => 1,
    0x7E => 2,
    0x7F => 1,
    0x80 => 1,
    0x81 => 1,
    0x82 => 1,
    0x83 => 1,
    0x84 => 1,
    0x85 => 1,
    0x86 => 2,
    0x87 => 1,
    0x88 => 1,
    0x89 => 1,
    0x8A => 1,
    0x8B => 1,
    0x8C => 1,
    0x8D => 1,
    0x8E => 2,
    0x8F => 1,
    0x90 => 1,
    0x91 => 1,
    0x92 => 1,
    0x93 => 1,
    0x94 => 1,
    0x95 => 1,
    0x96 => 2,
    0x97 => 1,
    0x98 => 1,
    0x99 => 1,
    0x9A => 1,
    0x9B => 1,
    0x9C => 1,
    0x9D => 1,
    0x9E => 2,
    0x9F => 1,
    0xA0 => 1,
    0xA1 => 1,
    0xA2 => 1,
    0xA3 => 1,
    0xA4 => 1,
    0xA5 => 1,
    0xA6 => 2,
    0xA7 => 1,
    0xA8 => 1,
    0xA9 => 1,
    0xAA => 1,
    0xAB => 1,
    0xAC => 1,
    0xAD => 1,
    0xAE => 2,
    0xAF => 1,
    0xB0 => 1,
    0xB1 => 1,
    0xB2 => 1,
    0xB3 => 1,
    0xB4 => 1,
    0xB5 => 1,
    0xB6 => 2,
    0xB7 => 1,
    0xB8 => 1,
    0xB9 => 1,
    0xBA => 1,
    0xBB => 1,
    0xBC => 1,
    0xBD => 1,
    0xBE => 2,
    0xBF => 1,
    0xC0 => 5,
    0xC1 => 3,
    0xC2 => 4,
    0xC3 => 4,
    0xC4 => 6,
    0xC5 => 4,
    0xC6 => 2,
    0xC7 => 4,
    0xC8 => 5,
    0xC9 => 4,
    0xCA => 4,
    0xCB => 0,
    0xCC => 6,
    0xCD => 6,
    0xCE => 2,
    0xCF => 4,
    0xD0 => 5,
    0xD1 => 3,
    0xD2 => 4,
    0xD3 => 0,
    0xD4 => 6,
    0xD5 => 4,
    0xD6 => 2,
    0xD7 => 4,
    0xD8 => 5,
    0xD9 => 4,
    0xDA => 4,
    0xDB => 0,
    0xDC => 6,
    0xDD => 0,
    0xDE => 2,
    0xDF => 4,
    0xE0 => 3,
    0xE1 => 3,
    0xE2 => 2,
    0xE3 => 0,
    0xE4 => 0,
    0xE5 => 4,
    0xE6 => 2,
    0xE7 => 4,
    0xE8 => 4,
    0xE9 => 1,
    0xEA => 4,
    0xEB => 0,
    0xEC => 0,
    0xED => 0,
    0xEE => 2,
    0xEF => 4,
    0xF0 => 3,
    0xF1 => 3,
    0xF2 => 2,
    0xF3 => 1,
    0xF4 => 0,
    0xF5 => 4,
    0xF6 => 2,
    0xF7 => 4,
    0xF8 => 3,
    0xF9 => 2,
    0xFA => 4,
    0xFB => 1,
    0xFC => 0,
    0xFD => 0,
    0xFE => 2,
    0xFF => 4,
    _ => panic!("Unexpected opcode: {:#X}", opcode),
  }
}

// CB opcode cycles
pub fn cb(opcode: u8) -> i32 {
  match opcode {
    0x00 => 2,
    0x01 => 2,
    0x02 => 2,
    0x03 => 2,
    0x04 => 2,
    0x05 => 2,
    0x06 => 4,
    0x07 => 2,
    0x08 => 2,
    0x09 => 2,
    0x0A => 2,
    0x0B => 2,
    0x0C => 2,
    0x0D => 2,
    0x0E => 4,
    0x0F => 2,
    0x10 => 2,
    0x11 => 2,
    0x12 => 2,
    0x13 => 2,
    0x14 => 2,
    0x15 => 2,
    0x16 => 4,
    0x17 => 2,
    0x18 => 2,
    0x19 => 2,
    0x1A => 2,
    0x1B => 2,
    0x1C => 2,
    0x1D => 2,
    0x1E => 4,
    0x1F => 2,
    0x20 => 2,
    0x21 => 2,
    0x22 => 2,
    0x23 => 2,
    0x24 => 2,
    0x25 => 2,
    0x26 => 4,
    0x27 => 2,
    0x28 => 2,
    0x29 => 2,
    0x2A => 2,
    0x2B => 2,
    0x2C => 2,
    0x2D => 2,
    0x2E => 4,
    0x2F => 2,
    0x30 => 2,
    0x31 => 2,
    0x32 => 2,
    0x33 => 2,
    0x34 => 2,
    0x35 => 2,
    0x36 => 4,
    0x37 => 2,
    0x38 => 2,
    0x39 => 2,
    0x3A => 2,
    0x3B => 2,
    0x3C => 2,
    0x3D => 2,
    0x3E => 4,
    0x3F => 2,
    0x40 => 2,
    0x41 => 2,
    0x42 => 2,
    0x43 => 2,
    0x44 => 2,
    0x45 => 2,
    0x46 => 3,
    0x47 => 2,
    0x48 => 2,
    0x49 => 2,
    0x4A => 2,
    0x4B => 2,
    0x4C => 2,
    0x4D => 2,
    0x4E => 3,
    0x4F => 2,
    0x50 => 2,
    0x51 => 2,
    0x52 => 2,
    0x53 => 2,
    0x54 => 2,
    0x55 => 2,
    0x56 => 3,
    0x57 => 2,
    0x58 => 2,
    0x59 => 2,
    0x5A => 2,
    0x5B => 2,
    0x5C => 2,
    0x5D => 2,
    0x5E => 3,
    0x5F => 2,
    0x60 => 2,
    0x61 => 2,
    0x62 => 2,
    0x63 => 2,
    0x64 => 2,
    0x65 => 2,
    0x66 => 3,
    0x67 => 2,
    0x68 => 2,
    0x69 => 2,
    0x6A => 2,
    0x6B => 2,
    0x6C => 2,
    0x6D => 2,
    0x6E => 3,
    0x6F => 2,
    0x70 => 2,
    0x71 => 2,
    0x72 => 2,
    0x73 => 2,
    0x74 => 2,
    0x75 => 2,
    0x76 => 3,
    0x77 => 2,
    0x78 => 2,
    0x79 => 2,
    0x7A => 2,
    0x7B => 2,
    0x7C => 2,
    0x7D => 2,
    0x7E => 3,
    0x7F => 2,
    0x80 => 2,
    0x81 => 2,
    0x82 => 2,
    0x83 => 2,
    0x84 => 2,
    0x85 => 2,
    0x86 => 4,
    0x87 => 2,
    0x88 => 2,
    0x89 => 2,
    0x8A => 2,
    0x8B => 2,
    0x8C => 2,
    0x8D => 2,
    0x8E => 4,
    0x8F => 2,
    0x90 => 2,
    0x91 => 2,
    0x92 => 2,
    0x93 => 2,
    0x94 => 2,
    0x95 => 2,
    0x96 => 4,
    0x97 => 2,
    0x98 => 2,
    0x99 => 2,
    0x9A => 2,
    0x9B => 2,
    0x9C => 2,
    0x9D => 2,
    0x9E => 4,
    0x9F => 2,
    0xA0 => 2,
    0xA1 => 2,
    0xA2 => 2,
    0xA3 => 2,
    0xA4 => 2,
    0xA5 => 2,
    0xA6 => 4,
    0xA7 => 2,
    0xA8 => 2,
    0xA9 => 2,
    0xAA => 2,
    0xAB => 2,
    0xAC => 2,
    0xAD => 2,
    0xAE => 4,
    0xAF => 2,
    0xB0 => 2,
    0xB1 => 2,
    0xB2 => 2,
    0xB3 => 2,
    0xB4 => 2,
    0xB5 => 2,
    0xB6 => 4,
    0xB7 => 2,
    0xB8 => 2,
    0xB9 => 2,
    0xBA => 2,
    0xBB => 2,
    0xBC => 2,
    0xBD => 2,
    0xBE => 4,
    0xBF => 2,
    0xC0 => 2,
    0xC1 => 2,
    0xC2 => 2,
    0xC3 => 2,
    0xC4 => 2,
    0xC5 => 2,
    0xC6 => 4,
    0xC7 => 2,
    0xC8 => 2,
    0xC9 => 2,
    0xCA => 2,
    0xCB => 2,
    0xCC => 2,
    0xCD => 2,
    0xCE => 4,
    0xCF => 2,
    0xD0 => 2,
    0xD1 => 2,
    0xD2 => 2,
    0xD4 => 2,
    0xD5 => 2,
    0xD6 => 2,
    0xD7 => 4,
    0xD8 => 2,
    0xD9 => 2,
    0xDA => 2,
    0xDB => 2,
    0xDC => 2,
    0xDD => 2,
    0xDE => 2,
    0xDF => 4,
    0xE0 => 2,
    0xE1 => 2,
    0xE2 => 2,
    0xE3 => 2,
    0xE4 => 2,
    0xE5 => 2,
    0xE6 => 2,
    0xE7 => 4,
    0xE8 => 2,
    0xE9 => 2,
    0xEA => 2,
    0xEB => 2,
    0xEC => 2,
    0xED => 2,
    0xEE => 2,
    0xEF => 4,
    0xF0 => 2,
    0xF1 => 2,
    0xF2 => 2,
    0xF3 => 2,
    0xF4 => 2,
    0xF5 => 2,
    0xF6 => 2,
    0xF7 => 4,
    0xF8 => 2,
    0xF9 => 2,
    0xFA => 2,
    0xFB => 2,
    0xFC => 2,
    0xFD => 2,
    0xFE => 2,
    0xFF => 4,
    _ => panic!("Unexpected opcode: {:#X}", opcode),
  }
}
