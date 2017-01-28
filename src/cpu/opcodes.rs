pub use super::types;

pub const NOP: types::Byte = 0x00;
pub const JP_NN: types::Byte = 0xC3;

pub const XOR_A: types::Byte = 0xAF;
pub const XOR_B: types::Byte = 0xA8;

pub const DEC_B: types::Byte = 0x05;

pub const LD_HL_NN: types::Byte = 0x21;  // LD HL,nn
pub const LD_C_N: types::Byte = 0x0E;  // LD C,n
pub const LD_B_N: types::Byte = 0x06;  // LD B,n
pub const LDD_HL_A: types::Byte = 0x32;  // LDD (HL),A
