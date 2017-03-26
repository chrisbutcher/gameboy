use std::fmt;
use std::io;
use std::io::prelude::*;
use std::env;

pub use super::types;
pub use super::mmu;
pub use super::cartridge;

const FLAG_ZERO: types::Byte = 0x80; // Zero
const FLAG_SUB: types::Byte = 0x40; // Negative,
const FLAG_HALF_CARRY: types::Byte = 0x20; // Half-carry
const FLAG_CARRY: types::Byte = 0x10; // Carry

const FLAG_NONE: types::Byte = 0x00; // None

#[derive(Debug, Copy, Clone)]
pub enum RegEnum {
  A, F, B, C, D, E, H, L, S, P,
  AF, BC, DE, HL,
  SP
}

pub struct Register {
  value: types::Word
}

impl Register {
  pub fn new() -> Register {
    Register{value: 0x0}
  }

  pub fn read(&self) -> types::Word {
    self.value
  }

  pub fn read_hi(&self) -> types::Byte {
    ((self.value >> 8) & 0xFF) as types::Byte
  }

  pub fn read_lo(&self) -> types::Byte {
    (self.value & 0xFF) as types::Byte
  }

  pub fn write(&mut self, v: types::Word) {
    self.value = v
  }

  pub fn write_hi(&mut self, v: types::Byte) {
    self.value = self.value & 0x00FF | (v as types::Word) << 8
  }

  pub fn write_lo(&mut self, v: types::Byte) {
    self.value = self.value & 0xFF00 | v as types::Word
  }
}

impl fmt::Debug for Register {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Register {{ value: {} }}", self.value)
  }
}

pub struct CPU {
  pub AF: Register, pub BC: Register, pub DE: Register, pub HL: Register,
  pub SP: Register, pub PC: types::Word,

  pub BranchTaken: bool, pub IME: bool, pub IMECycles: i32 // so far unused
}

fn formatted_flags(cpu: &CPU) -> String {
  format!("Z: {}, N: {}, H: {} C: {}", cpu.util_is_flag_set(FLAG_ZERO) as u8, cpu.util_is_flag_set(FLAG_SUB) as u8, cpu.util_is_flag_set(FLAG_HALF_CARRY) as u8, cpu.util_is_flag_set(FLAG_CARRY) as u8)
}

impl fmt::Debug for CPU {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[PC] {:#X}\n[Regs] A:{:#X}, F:{:#X}, B:{:#X}, C:{:#X}, D:{:#X}, E:{:#X}, H:{:#X}, L:{:#X}\n[Flags]: {} [SP] {:#X}",
      self.PC, self.AF.read_hi(), self.AF.read_lo(), self.BC.read_hi(), self.BC.read_lo(), self.DE.read_hi(), self.DE.read_lo(), self.HL.read_hi(), self.HL.read_lo(), formatted_flags(self), self.SP.read()
    )
  }
}

impl CPU {
  pub fn new() -> CPU {
      CPU {
        AF: Register::new(), BC: Register::new(), DE: Register::new(), HL: Register::new(),
        SP: Register::new(), PC: 0x0000, BranchTaken: false, IME: false, IMECycles: 0
      }
  }

  pub fn write_byte_reg(&mut self, regEnum: RegEnum, byte: types::Byte) {
    match regEnum {
      RegEnum::A => { self.AF.write_hi(byte) },
      RegEnum::F => { self.AF.write_lo(byte) },
      RegEnum::B => { self.BC.write_hi(byte) },
      RegEnum::C => { self.BC.write_lo(byte) },
      RegEnum::D => { self.DE.write_hi(byte) },
      RegEnum::E => { self.DE.write_lo(byte) },
      RegEnum::H => { self.HL.write_hi(byte) },
      RegEnum::L => { self.HL.write_lo(byte) },
      RegEnum::S => { self.SP.write_hi(byte) },
      RegEnum::P => { self.SP.write_lo(byte) },
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  pub fn write_word_reg(&mut self, regEnum: RegEnum, word: types::Word) {
    match regEnum {
      RegEnum::AF => { self.AF.write(word) },
      RegEnum::BC => { self.BC.write(word) },
      RegEnum::DE => { self.DE.write(word) },
      RegEnum::HL => { self.HL.write(word) },
      RegEnum::SP => { self.SP.write(word) },
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  pub fn read_byte_reg(&mut self, regEnum: RegEnum) -> types::Byte {
    match regEnum {
      RegEnum::A => { self.AF.read_hi() },
      RegEnum::F => { self.AF.read_lo() },
      RegEnum::B => { self.BC.read_hi() },
      RegEnum::C => { self.BC.read_lo() },
      RegEnum::D => { self.DE.read_hi() },
      RegEnum::E => { self.DE.read_lo() },
      RegEnum::H => { self.HL.read_hi() },
      RegEnum::L => { self.HL.read_lo() },
      RegEnum::S => { self.SP.read_hi() },
      RegEnum::P => { self.SP.read_lo() },
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  pub fn read_word_reg(&mut self, regEnum: RegEnum) -> types::Word {
    match regEnum {
      RegEnum::AF => { self.AF.read() },
      RegEnum::BC => { self.BC.read() },
      RegEnum::DE => { self.DE.read() },
      RegEnum::HL => { self.HL.read() },
      RegEnum::SP => { self.SP.read() },
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  // pub fn write_word_reg() {
  // }

  pub fn execute_next_opcode(&mut self, mmu: &mut mmu::MMU) -> i32 {
    debug!("{:?}\n", self);

    // println!("PC: {:#X}", self.PC);
    let opcode: types::Byte = mmu.read(self.PC);

    match env::var("DEBUG") {
        Ok(_) => { let mut stdin = io::stdin(); let _ = stdin.read(&mut [0u8]).unwrap(); },
        _ => {},
    };

    if self.PC == 0x31F {
      // panic!("boom");
    }

    self.PC += 1;
    let cycles = self.execute_opcode(opcode, mmu);

    if cycles == 42 {
      panic!("Unexpected opcode: {:#X}", opcode);
    }

    cycles
  }

  // https://github.com/CTurt/Cinoop/blob/master/source/cpu.c
  // https://github.com/CTurt/Cinoop/blob/master/include/cpu.h
  // https://github.com/drhelius/Gearboy/blob/master/src/opcodes.cpp

  // ALSO SEE https://github.com/mvdnes/rboy/blob/master/src/cpu.rs (Note that cycles in this code are divided by 4)
  // http://gameboy.mongenel.com/dmg/lesson1.html
  fn execute_opcode(&mut self, opcode: types::Byte, mmu: &mut mmu::MMU) -> i32 {
    match opcode {
      0x00 => { debug!("NOP"); self.nop(); 4 },
      0x01 => { debug!("LD BC,nn"); self.ld_bc_nn(mmu); 12 },
      0x02 => { debug!("LD (BC),A : ld_bc_a() not implemented! {:#X}", opcode); 42 },
      0x03 => { debug!("INC BC : inc_bc() not implemented! {:#X}", opcode); 42 },
      0x04 => { debug!("INC B : inc_b() not implemented! {:#X}", opcode); 42 },
      0x05 => { debug!("DEC B"); self.dec_b(); 4 },
      0x06 => { debug!("LD B,n"); self.ld_b_n(mmu); 8 },
      0x07 => { debug!("RLCA : rlca() not implemented! {:#X}", opcode); 42 },
      0x08 => { debug!("LD (nn),SP : ld_nn_sp() not implemented! {:#X}", opcode); 42 },
      0x09 => { debug!("ADD HL,BC : add_hl_bc() not implemented! {:#X}", opcode); 42 },
      0x0A => { debug!("LD A,(BC) : ld_a_bc() not implemented! {:#X}", opcode); 42 },
      0x0B => { debug!("DEC BC"); self.dec_bc(); 8 },
      0x0C => { debug!("INC C"); self.inc_c(); 4 },
      0x0D => { debug!("DEC C"); self.dec_c(); 4 },
      0x0E => { debug!("LD C,n"); self.ld_c_n(mmu); 8 },
      0x0F => { debug!("RRCA : rrca() not implemented! {:#X}", opcode); 42 },
      0x10 => { debug!("STOP : stop() not implemented! {:#X}", opcode); 42 },
      0x11 => { debug!("LD DE,nn : ld_de_nn() not implemented! {:#X}", opcode); 42 },
      0x12 => { debug!("LD (DE),A : ld_de_a() not implemented! {:#X}", opcode); 42 },
      0x13 => { debug!("INC DE : inc_de() not implemented! {:#X}", opcode); 42 },
      0x14 => { debug!("INC D : inc_d() not implemented! {:#X}", opcode); 42 },
      0x15 => { debug!("DEC D : dec_d() not implemented! {:#X}", opcode); 42 },
      0x16 => { debug!("LD D,n : ld_d_n() not implemented! {:#X}", opcode); 42 },
      0x17 => { debug!("RLA : rla() not implemented! {:#X}", opcode); 42 },
      0x18 => { debug!("JR n : jr_n() not implemented! {:#X}", opcode); 42 },
      0x19 => { debug!("ADD HL,DE : add_hl_de() not implemented! {:#X}", opcode); 42 },
      0x1A => { debug!("LD A,(DE) : ld_a_de() not implemented! {:#X}", opcode); 42 },
      0x1B => { debug!("DEC DE : dec_de() not implemented! {:#X}", opcode); 42 },
      0x1C => { debug!("INC E : inc_e() not implemented! {:#X}", opcode); 42 },
      0x1D => { debug!("DEC E : dec_e() not implemented! {:#X}", opcode); 42 },
      0x1E => { debug!("LD E,n : ld_e_n() not implemented! {:#X}", opcode); 42 },
      0x1F => { debug!("RRA"); self.rra(); 4 },
      0x20 => { debug!("JR NZ,n"); self.jr_nz_n(mmu); 8 },
      0x21 => { debug!("LD HL,nn"); self.ld_hl_nn(mmu); 12 },
      0x22 => { debug!("LD (HLI),A : ld_hli_a() not implemented! {:#X}", opcode); 42 },
      0x23 => { debug!("INC HL : inc_hl() not implemented! {:#X}", opcode); 42 },
      0x24 => { debug!("INC H : inc_h() not implemented! {:#X}", opcode); 42 },
      0x25 => { debug!("DEC H : dec_h() not implemented! {:#X}", opcode); 42 },
      0x26 => { debug!("LD H,n : ld_h_n() not implemented! {:#X}", opcode); 42 },
      0x27 => { debug!("DAA : daa() not implemented! {:#X}", opcode); 42 },
      0x28 => { debug!("JR Z,n"); self.jr_z_n(mmu); 8 },
      0x29 => { debug!("ADD HL,HL : add_hl_hl() not implemented! {:#X}", opcode); 42 },
      0x2A => { debug!("LD A,(HLI)"); self.ld_a_hli(mmu); 8 },
      0x2B => { debug!("DEC HL : dec_hl() not implemented! {:#X}", opcode); 42 },
      0x2C => { debug!("INC L : inc_l() not implemented! {:#X}", opcode); 42 },
      0x2D => { debug!("DEC L : dec_l() not implemented! {:#X}", opcode); 42 },
      0x2E => { debug!("LD L,n : ld_l_n() not implemented! {:#X}", opcode); 42 },
      0x2F => { debug!("CPL : cpl() not implemented! {:#X}", opcode); 42 },
      0x30 => { debug!("JR NC,n : jr_nc_n() not implemented! {:#X}", opcode); 42 },
      0x31 => { debug!("LD SP,nn"); self.ld_sp_nn(mmu); 12 },
      0x32 => { debug!("LD (HLD), A"); self.ld_hld_a(mmu); 8 },
      0x33 => { debug!("INC SP : inc_sp() not implemented! {:#X}", opcode); 42 },
      0x34 => { debug!("INC (HL)"); self.inc_hl(mmu); 12 },
      0x35 => { debug!("DEC (HL) : dec_hl() not implemented! {:#X}", opcode); 42 },
      0x36 => { debug!("LD (HL),n"); self.ld_hl_n(mmu); 12 },
      0x37 => { debug!("SCF : scf() not implemented! {:#X}", opcode); 42 },
      0x38 => { debug!("JR C,n : jr_c_n() not implemented! {:#X}", opcode); 42 },
      0x39 => { debug!("ADD HL,SP : add_hl_sp() not implemented! {:#X}", opcode); 42 },
      0x3A => { debug!("LD A,(HLD) : ld_a_hld() not implemented! {:#X}", opcode); 42 },
      0x3B => { debug!("DEC SP : dec_sp() not implemented! {:#X}", opcode); 42 },
      0x3C => { debug!("INC A : inc_a() not implemented! {:#X}", opcode); 42 },
      0x3D => { debug!("DEC A : dec_a() not implemented! {:#X}", opcode); 42 },
      0x3E => { debug!("LD A,n"); self.ld_a_n(mmu); 8 },
      0x3F => { debug!("CCF : ccf() not implemented! {:#X}", opcode); 42 },
      0x40 => { debug!("LD B,B : ld_b_b() not implemented! {:#X}", opcode); 42 },
      0x41 => { debug!("LD B,C : ld_b_c() not implemented! {:#X}", opcode); 42 },
      0x42 => { debug!("LD B,D : ld_b_d() not implemented! {:#X}", opcode); 42 },
      0x43 => { debug!("LD B,E : ld_b_e() not implemented! {:#X}", opcode); 42 },
      0x44 => { debug!("LD B,H : ld_b_h() not implemented! {:#X}", opcode); 42 },
      0x45 => { debug!("LD B,L : ld_b_l() not implemented! {:#X}", opcode); 42 },
      0x46 => { debug!("LD B,(HL) : ld_b_hl() not implemented! {:#X}", opcode); 42 },
      0x47 => { debug!("LD B,A : ld_b_a() not implemented! {:#X}", opcode); 42 },
      0x48 => { debug!("LD C,B : ld_c_b() not implemented! {:#X}", opcode); 42 },
      0x49 => { debug!("LD C,C : ld_c_c() not implemented! {:#X}", opcode); 42 },
      0x4A => { debug!("LD C,D : ld_c_d() not implemented! {:#X}", opcode); 42 },
      0x4B => { debug!("LD C,E : ld_c_e() not implemented! {:#X}", opcode); 42 },
      0x4C => { debug!("LD C,H : ld_c_h() not implemented! {:#X}", opcode); 42 },
      0x4D => { debug!("LD C,L : ld_c_l() not implemented! {:#X}", opcode); 42 },
      0x4E => { debug!("LD C,(HL) : ld_c_hl() not implemented! {:#X}", opcode); 42 },
      0x4F => { debug!("LD C,A : ld_c_a() not implemented! {:#X}", opcode); 42 },
      0x50 => { debug!("LD D,B : ld_d_b() not implemented! {:#X}", opcode); 42 },
      0x51 => { debug!("LD D,C : ld_d_c() not implemented! {:#X}", opcode); 42 },
      0x52 => { debug!("LD D,D : ld_d_d() not implemented! {:#X}", opcode); 42 },
      0x53 => { debug!("LD D,E : ld_d_e() not implemented! {:#X}", opcode); 42 },
      0x54 => { debug!("LD D,H : ld_d_h() not implemented! {:#X}", opcode); 42 },
      0x55 => { debug!("LD D,L : ld_d_l() not implemented! {:#X}", opcode); 42 },
      0x56 => { debug!("LD D,(HL) : ld_d_hl() not implemented! {:#X}", opcode); 42 },
      0x57 => { debug!("LD D,A : ld_d_a() not implemented! {:#X}", opcode); 42 },
      0x58 => { debug!("LD E,B : ld_e_b() not implemented! {:#X}", opcode); 42 },
      0x59 => { debug!("LD E,C : ld_e_c() not implemented! {:#X}", opcode); 42 },
      0x5A => { debug!("LD E,D : ld_e_d() not implemented! {:#X}", opcode); 42 },
      0x5B => { debug!("LD E,E : ld_e_e() not implemented! {:#X}", opcode); 42 },
      0x5C => { debug!("LD E,H : ld_e_h() not implemented! {:#X}", opcode); 42 },
      0x5D => { debug!("LD E,L : ld_e_l() not implemented! {:#X}", opcode); 42 },
      0x5E => { debug!("LD E,(HL) : ld_e_hl() not implemented! {:#X}", opcode); 42 },
      0x5F => { debug!("LD E,A : ld_e_a() not implemented! {:#X}", opcode); 42 },
      0x60 => { debug!("LD H,B : ld_h_b() not implemented! {:#X}", opcode); 42 },
      0x61 => { debug!("LD H,C : ld_h_c() not implemented! {:#X}", opcode); 42 },
      0x62 => { debug!("LD H,D : ld_h_d() not implemented! {:#X}", opcode); 42 },
      0x63 => { debug!("LD H,E : ld_h_e() not implemented! {:#X}", opcode); 42 },
      0x64 => { debug!("LD H,H : ld_h_h() not implemented! {:#X}", opcode); 42 },
      0x65 => { debug!("LD H,L : ld_h_l() not implemented! {:#X}", opcode); 42 },
      0x66 => { debug!("LD H,(HL) : ld_h_hl() not implemented! {:#X}", opcode); 42 },
      0x67 => { debug!("LD H,A : ld_h_a() not implemented! {:#X}", opcode); 42 },
      0x68 => { debug!("LD L,B : ld_l_b() not implemented! {:#X}", opcode); 42 },
      0x69 => { debug!("LD L,C : ld_l_c() not implemented! {:#X}", opcode); 42 },
      0x6A => { debug!("LD L,D : ld_l_d() not implemented! {:#X}", opcode); 42 },
      0x6B => { debug!("LD L,E : ld_l_e() not implemented! {:#X}", opcode); 42 },
      0x6C => { debug!("LD L,H : ld_l_h() not implemented! {:#X}", opcode); 42 },
      0x6D => { debug!("LD L,L : ld_l_l() not implemented! {:#X}", opcode); 42 },
      0x6E => { debug!("LD L,(HL) : ld_l_hl() not implemented! {:#X}", opcode); 42 },
      0x6F => { debug!("LD L,A : ld_l_a() not implemented! {:#X}", opcode); 42 },
      0x70 => { debug!("LD (HL),B : ld_hl_b() not implemented! {:#X}", opcode); 42 },
      0x71 => { debug!("LD (HL),C : ld_hl_c() not implemented! {:#X}", opcode); 42 },
      0x72 => { debug!("LD (HL),D : ld_hl_d() not implemented! {:#X}", opcode); 42 },
      0x73 => { debug!("LD (HL),E : ld_hl_e() not implemented! {:#X}", opcode); 42 },
      0x74 => { debug!("LD (HL),H : ld_hl_h() not implemented! {:#X}", opcode); 42 },
      0x75 => { debug!("LD (HL),L : ld_hl_l() not implemented! {:#X}", opcode); 42 },
      0x76 => { debug!("HALT : halt() not implemented! {:#X}", opcode); 42 },
      0x77 => { debug!("LD (HL),A"); self.ld_hl_a(mmu); 8 },
      0x78 => { debug!("LD A,B"); self.ld_a_b(); 4 },
      0x79 => { debug!("LD A,C"); self.ld_a_c(); 4 },
      0x7A => { debug!("LD A,D"); self.ld_a_d(); 4 },
      0x7B => { debug!("LD A,E : ld_a_e() not implemented! {:#X}", opcode); 42 },
      0x7C => { debug!("LD A,H : ld_a_h() not implemented! {:#X}", opcode); 42 },
      0x7D => { debug!("LD A,L : ld_a_l() not implemented! {:#X}", opcode); 42 },
      0x7E => { debug!("LD A,(HL) : ld_a_hl() not implemented! {:#X}", opcode); 42 },
      0x7F => { debug!("LD A,A : ld_a_a() not implemented! {:#X}", opcode); 42 },
      0x80 => { debug!("ADD A,B : add_a_b() not implemented! {:#X}", opcode); 42 },
      0x81 => { debug!("ADD A,C : add_a_c() not implemented! {:#X}", opcode); 42 },
      0x82 => { debug!("ADD A,D : add_a_d() not implemented! {:#X}", opcode); 42 },
      0x83 => { debug!("ADD A,E : add_a_e() not implemented! {:#X}", opcode); 42 },
      0x84 => { debug!("ADD A,H : add_a_h() not implemented! {:#X}", opcode); 42 },
      0x85 => { debug!("ADD A,L : add_a_l() not implemented! {:#X}", opcode); 42 },
      0x86 => { debug!("ADD A,(HL) : add_a_hl() not implemented! {:#X}", opcode); 42 },
      0x87 => { debug!("ADD A,A : add_a_a() not implemented! {:#X}", opcode); 42 },
      0x88 => { debug!("ADC A,B : adc_a_b() not implemented! {:#X}", opcode); 42 },
      0x89 => { debug!("ADC A,C"); self.adc_a_c(); 4 },
      0x8A => { debug!("ADC A,D : adc_a_d() not implemented! {:#X}", opcode); 42 },
      0x8B => { debug!("ADC A,E : adc_a_e() not implemented! {:#X}", opcode); 42 },
      0x8C => { debug!("ADC A,H : adc_a_h() not implemented! {:#X}", opcode); 42 },
      0x8D => { debug!("ADC A,L : adc_a_l() not implemented! {:#X}", opcode); 42 },
      0x8E => { debug!("ADC A,(HL) : adc_a_hl() not implemented! {:#X}", opcode); 42 },
      0x8F => { debug!("ADC A,A : adc_a_a() not implemented! {:#X}", opcode); 42 },
      0x90 => { debug!("SUB B : sub_b() not implemented! {:#X}", opcode); 42 },
      0x91 => { debug!("SUB C : sub_c() not implemented! {:#X}", opcode); 42 },
      0x92 => { debug!("SUB D : sub_d() not implemented! {:#X}", opcode); 42 },
      0x93 => { debug!("SUB E : sub_e() not implemented! {:#X}", opcode); 42 },
      0x94 => { debug!("SUB H : sub_h() not implemented! {:#X}", opcode); 42 },
      0x95 => { debug!("SUB L : sub_l() not implemented! {:#X}", opcode); 42 },
      0x96 => { debug!("SUB (HL) : sub_hl() not implemented! {:#X}", opcode); 42 },
      0x97 => { debug!("SUB A : sub_a() not implemented! {:#X}", opcode); 42 },
      0x98 => { debug!("SBC B : sbc_b() not implemented! {:#X}", opcode); 42 },
      0x99 => { debug!("SBC C : sbc_c() not implemented! {:#X}", opcode); 42 },
      0x9A => { debug!("SBC D : sbc_d() not implemented! {:#X}", opcode); 42 },
      0x9B => { debug!("SBC E : sbc_e() not implemented! {:#X}", opcode); 42 },
      0x9C => { debug!("SBC H : sbc_h() not implemented! {:#X}", opcode); 42 },
      0x9D => { debug!("SBC L : sbc_l() not implemented! {:#X}", opcode); 42 },
      0x9E => { debug!("SBC (HL) : sbc_hl() not implemented! {:#X}", opcode); 42 },
      0x9F => { debug!("SBC A : sbc_a() not implemented! {:#X}", opcode); 42 },
      0xA0 => { debug!("AND B : and_b() not implemented! {:#X}", opcode); 42 },
      0xA1 => { debug!("AND C : and_c() not implemented! {:#X}", opcode); 42 },
      0xA2 => { debug!("AND D : and_d() not implemented! {:#X}", opcode); 42 },
      0xA3 => { debug!("AND E : and_e() not implemented! {:#X}", opcode); 42 },
      0xA4 => { debug!("AND H : and_h() not implemented! {:#X}", opcode); 42 },
      0xA5 => { debug!("AND L : and_l() not implemented! {:#X}", opcode); 42 },
      0xA6 => { debug!("AND (HL) : and_hl() not implemented! {:#X}", opcode); 42 },
      0xA7 => { debug!("AND A : and_a() not implemented! {:#X}", opcode); 42 },
      0xA8 => { debug!("XOR B"); self.xor_b(); 4 },
      0xA9 => { debug!("XOR C : xor_c() not implemented! {:#X}", opcode); 42 },
      0xAA => { debug!("XOR D : xor_d() not implemented! {:#X}", opcode); 42 },
      0xAB => { debug!("XOR E : xor_e() not implemented! {:#X}", opcode); 42 },
      0xAC => { debug!("XOR H : xor_h() not implemented! {:#X}", opcode); 42 },
      0xAD => { debug!("XOR L : xor_l() not implemented! {:#X}", opcode); 42 },
      0xAE => { debug!("XOR (HL) : xor_hl() not implemented! {:#X}", opcode); 42 },
      0xAF => { debug!("XOR A"); self.xor_a(); 4 },
      0xB0 => { debug!("OR B : or_b() not implemented! {:#X}", opcode); 42 },
      0xB1 => { debug!("OR C"); self.or_c(); 4 },
      0xB2 => { debug!("OR D : or_d() not implemented! {:#X}", opcode); 42 },
      0xB3 => { debug!("OR E : or_e() not implemented! {:#X}", opcode); 42 },
      0xB4 => { debug!("OR H : or_h() not implemented! {:#X}", opcode); 42 },
      0xB5 => { debug!("OR L : or_l() not implemented! {:#X}", opcode); 42 },
      0xB6 => { debug!("OR (HL) : or_hl() not implemented! {:#X}", opcode); 42 },
      0xB7 => { debug!("OR A : or_a() not implemented! {:#X}", opcode); 42 },
      0xB8 => { debug!("CP B : cp_b() not implemented! {:#X}", opcode); 42 },
      0xB9 => { debug!("CP C : cp_c() not implemented! {:#X}", opcode); 42 },
      0xBA => { debug!("CP D : cp_d() not implemented! {:#X}", opcode); 42 },
      0xBB => { debug!("CP E : cp_e() not implemented! {:#X}", opcode); 42 },
      0xBC => { debug!("CP H : cp_h() not implemented! {:#X}", opcode); 42 },
      0xBD => { debug!("CP L : cp_l() not implemented! {:#X}", opcode); 42 },
      0xBE => { debug!("CP (HL) : cp_hl() not implemented! {:#X}", opcode); 42 },
      0xBF => { debug!("CP A : cp_a() not implemented! {:#X}", opcode); 42 },
      0xC0 => { debug!("RET NZ : ret_nz() not implemented! {:#X}", opcode); 42 },
      0xC1 => { debug!("POP BC : pop_bc() not implemented! {:#X}", opcode); 42 },
      0xC2 => { debug!("JP NZ,nn : jp_nz_nn() not implemented! {:#X}", opcode); 42 },
      0xC3 => { debug!("JP nn"); self.jp_nn(mmu); 12 },
      0xC4 => { debug!("CALL NZ,nn : call_nz_nn() not implemented! {:#X}", opcode); 42 },
      0xC5 => { debug!("PUSH BC : push_bc() not implemented! {:#X}", opcode); 42 },
      0xC6 => { debug!("ADD A,n : add_a_n() not implemented! {:#X}", opcode); 42 },
      0xC7 => { debug!("RST 00H : rst_00h() not implemented! {:#X}", opcode); 42 },
      0xC8 => { debug!("RET Z : ret_z() not implemented! {:#X}", opcode); 42 },
      0xC9 => { debug!("RET"); self.ret(mmu); 8 },
      0xCA => { debug!("JP Z,nn : jp_z_nn() not implemented! {:#X}", opcode); 42 },
      0xCB => { debug!("CB prefixed instruction : cb_prefixed_instruction() not implemented! {:#X}", opcode); 42 },
      0xCC => { debug!("CALL Z,nn : call_z_nn() not implemented! {:#X}", opcode); 42 },
      0xCD => { debug!("CALL nn"); self.call_nn(mmu); 12 },
      0xCE => { debug!("ADC A,n : adc_a_n() not implemented! {:#X}", opcode); 42 },
      0xCF => { debug!("RST 08H : rst_08h() not implemented! {:#X}", opcode); 42 },
      0xD0 => { debug!("RET NC : ret_nc() not implemented! {:#X}", opcode); 42 },
      0xD1 => { debug!("POP DE : pop_de() not implemented! {:#X}", opcode); 42 },
      0xD2 => { debug!("JP NC,nn : jp_nc_nn() not implemented! {:#X}", opcode); 42 },
      0xD4 => { debug!("CALL NC,nn : call_nc_nn() not implemented! {:#X}", opcode); 42 },
      0xD5 => { debug!("PUSH DE : push_de() not implemented! {:#X}", opcode); 42 },
      0xD6 => { debug!("SUB n : sub_n() not implemented! {:#X}", opcode); 42 },
      0xD7 => { debug!("RST 10H : rst_10h() not implemented! {:#X}", opcode); 42 },
      0xD8 => { debug!("RET C : ret_c() not implemented! {:#X}", opcode); 42 },
      0xD9 => { debug!("RETI : reti() not implemented! {:#X}", opcode); 42 },
      0xDA => { debug!("JP C,nn : jp_c_nn() not implemented! {:#X}", opcode); 42 },
      0xDC => { debug!("CALL C,nn : call_c_nn() not implemented! {:#X}", opcode); 42 },
      0xDE => { debug!("SBC n : sbc_n() not implemented! {:#X}", opcode); 42 },
      0xDF => { debug!("RST 18H : rst_18h() not implemented! {:#X}", opcode); 42 },
      0xE0 => { debug!("LD (0xFF00+n),A"); self.ld_0xff00_plus_n_a(mmu); 12 },
      0xE1 => { debug!("POP HL : pop_hl() not implemented! {:#X}", opcode); 42 },
      0xE2 => { debug!("LD (0xFF00+C),A"); self.ld_0xff00_plus_c_a(mmu); 8 },
      0xE5 => { debug!("PUSH HL : push_hl() not implemented! {:#X}", opcode); 42 },
      0xE6 => { debug!("AND n"); self.and_n(mmu); 8 },
      0xE7 => { debug!("RST 20H : rst_20h() not implemented! {:#X}", opcode); 42 },
      0xE8 => { debug!("ADD SP,n : add_sp_n() not implemented! {:#X}", opcode); 42 },
      0xE9 => { debug!("JP (HL) : jp_hl() not implemented! {:#X}", opcode); 42 },
      0xEA => { debug!("LD (nn),A"); self.ld_nn_a(mmu); 16 },
      0xEE => { debug!("XOR n : xor_n() not implemented! {:#X}", opcode); 42 },
      0xEF => { debug!("RST 28H : rst_28h() not implemented! {:#X}", opcode); 42 },
      0xF0 => { debug!("LD A,(0xFF00+n)"); self.ld_a_0xff00_plus_n(mmu); 12 },
      0xF1 => { debug!("POP AF : pop_af() not implemented! {:#X}", opcode); 42 },
      0xF2 => { debug!("LD A,(C) : ld_a_c() not implemented! {:#X}", opcode); 42 },
      0xF3 => { debug!("DI"); self.di(); 4 },
      0xF5 => { debug!("PUSH AF : push_af() not implemented! {:#X}", opcode); 42 },
      0xF6 => { debug!("OR n : or_n() not implemented! {:#X}", opcode); 42 },
      0xF7 => { debug!("RST 30H : rst_30h() not implemented! {:#X}", opcode); 42 },
      0xF8 => { debug!("LD HL,SP+n : ld_hl_sp_plus_n() not implemented! {:#X}", opcode); 42 },
      0xF9 => { debug!("LD SP,HL : ld_sp_hl() not implemented! {:#X}", opcode); 42 },
      0xFA => { debug!("LD A,(nn) : ld_a_nn() not implemented! {:#X}", opcode); 42 },
      0xFB => { debug!("EI : ei() not implemented! {:#X}", opcode); 42 },
      0xFE => { debug!("CP"); self.cp_n(mmu); 8 },
      0xFF => { debug!("RST 38H : rst_38h() not implemented! {:#X}", opcode); 42 },
      _ => panic!("Unexpected opcode: {:#X}", opcode)
    }
    // Ok(cycles)
  }

  // Opcode implementations

  fn nop(&self) {
  }

  fn jp_nn(&mut self, mmu: &mmu::MMU) {
    self.PC = mmu.read_word(self.PC);
  }

  fn xor_a(&mut self) {
    let A = self.AF.read_hi();
    shared_xor_n(self, A);
  }

  fn xor_b(&mut self) {
    let B = self.BC.read_hi();
    shared_xor_n(self, B);
  }

  fn ld_hl_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.HL.write(value);

    self.PC += 2;
  }

  fn ld_c_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_ld_n_n(self, RegEnum::C, value);
  }

  fn ld_b_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_ld_n_n(self, RegEnum::B, value);
  }

  fn ld_hld_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let address = self.read_word_reg(RegEnum::HL);
    mmu.write(address, value);
    self.write_word_reg(RegEnum::HL, address - 1);
  }

  fn dec_b(&mut self) {
    shared_dec_byte_reg(self, RegEnum::B);
  }

  fn dec_c(&mut self) {
    shared_dec_byte_reg(self, RegEnum::C);
  }

  fn jr_nz_n(&mut self, mmu: &mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      let operand_dest = mmu.read(self.PC) as types::SignedByte;
      self.PC = self.PC.wrapping_add((1 + operand_dest) as types::Word);

      self.BranchTaken = true;
    } else {
      self.PC += 1;
    }
  }

  fn rra(&mut self) {
    shared_rotate_rr(self, RegEnum::A)
  }

  fn ld_a_b(&mut self) {
    let operand = self.read_byte_reg(RegEnum::B);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn ld_a_c(&mut self) {
    let operand = self.read_byte_reg(RegEnum::C);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn ld_a_d(&mut self) {
    let operand = self.read_byte_reg(RegEnum::D);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn adc_a_c(&mut self) {
    let operand = self.read_byte_reg(RegEnum::C);
    shared_adc(self, operand);

    panic!("Not implemented!")
  }

  fn ld_a_n(&mut self, mmu: &mmu::MMU) {
    let operand = mmu.read(self.PC);
    self.write_byte_reg(RegEnum::A, operand);
    self.PC += 1;
  }

  fn di(&mut self) {
    self.IME = false;
    self.IMECycles = 0;
  }

  fn ld_0xff00_plus_n_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let operand = mmu.read(self.PC) as types::Word;

    mmu.write(0xFF00 + operand, value);

    self.PC += 1;
  }

  fn ld_a_0xff00_plus_n(&mut self, mmu: &mut mmu::MMU) {
    let operand = mmu.read(self.PC) as types::Word;
    let value = mmu.read(0xFF00 + operand);

    self.write_byte_reg(RegEnum::A, value);
    self.PC += 1;
  }

  fn cp_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_cp(self, value);
    self.PC += 1;
  }

  fn ld_hl_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.write_word_reg(RegEnum::HL, value);
    self.PC += 1;
  }

  fn ld_nn_a(&mut self, mmu: &mmu::MMU) {
    let addr = mmu.read_word(self.PC);

    self.PC += 2;
    let value = mmu.read(addr);

    self.write_byte_reg(RegEnum::A, value);
  }

  fn ld_sp_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.PC += 2;

    self.write_word_reg(RegEnum::SP, value);
  }

  fn ld_a_hli(&mut self, mmu: &mmu::MMU) {
    // Put value at address HL into A. Increment HL
    let value = mmu.read(self.PC);
    self.write_byte_reg(RegEnum::A, value);

    let current_HL = self.read_word_reg(RegEnum::HL);
    self.write_word_reg(RegEnum::HL, current_HL.wrapping_add(1));
  }

  fn ld_0xff00_plus_c_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let operand = self.read_byte_reg(RegEnum::C);

    mmu.write(0xFF00 + operand as types::Word, value);
    self.PC += 1;
  }

  fn inc_c(&mut self) {
    shared_inc_byte_reg(self, RegEnum::C);
  }

  fn call_nn(&mut self, mmu: &mut mmu::MMU) {
    let current_PC = self.PC;
    self.stack_push(current_PC + 2, mmu);
    self.PC = mmu.read_word(self.PC);
  }

  fn ld_bc_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.write_word_reg(RegEnum::BC, value);

    self.PC += 2;
  }

  fn dec_bc(&mut self) {
    shared_dec_word_reg(self, RegEnum::BC);
  }

  fn or_c(&mut self) {
    let value = self.read_byte_reg(RegEnum::C);
    shared_or_n(self, value);
  }

  fn ret(&mut self, mmu: &mmu::MMU) {
    self.stack_pop(mmu);
  }

  fn ld_hl_a(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::SP);
    mmu.write(addr, self.read_byte_reg(RegEnum::A));
  }

  fn and_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_and_n(self, value);

    self.PC += 1;
  }

  fn jr_z_n(&mut self, mmu: &mmu::MMU) {
    if self.util_is_flag_set(FLAG_ZERO) {
      let operand_dest = mmu.read(self.PC) as types::SignedByte;
      self.PC = self.PC.wrapping_add((1 + operand_dest) as types::Word);

      self.BranchTaken = true;
    } else {
      self.PC += 1;
    }
  }

  fn inc_hl(&mut self, mmu: &mmu::MMU) {
    let result = self.read_word_reg(RegEnum::HL) + 1;

    if self.util_is_flag_set(FLAG_CARRY) { self.util_set_flag(FLAG_CARRY) } else { self.util_clear_all_flags() };
    self.util_toggle_zero_flag_from_word_result(result);

    if result & 0x0F == 0x00 {
      self.util_toggle_flag(FLAG_HALF_CARRY);
    }
  }

  // Helpers

  fn stack_push(&mut self, word: types::Word, mmu: &mut mmu::MMU) {
    let current_SP = self.read_word_reg(RegEnum::SP); // TODO just manipulate .value directly
    self.write_word_reg(RegEnum::SP, current_SP - 2);
    mmu.write_word(self.read_word_reg(RegEnum::SP), word);
  }

  fn stack_pop(&mut self, mmu: &mmu::MMU) {
    let addr = mmu.read_word(self.read_word_reg(RegEnum::SP));
    self.PC = addr.swap_bytes(); // NOTE keep this swap_bytes call?

    let current_SP = self.read_word_reg(RegEnum::SP);
    self.SP.write(current_SP + 2);
  }

  fn util_toggle_zero_flag_from_result(&mut self, result: types::Byte) {
    if result == 0 {
      self.util_toggle_flag(FLAG_ZERO);
    }
  }

  fn util_toggle_zero_flag_from_word_result(&mut self, result: types::Word) {
    if result == 0 {
      self.util_toggle_flag(FLAG_ZERO);
    }
  }

  fn util_clear_all_flags(&mut self) {
    self.util_set_flag(FLAG_NONE);
  }

  fn util_set_flag(&mut self, byte: types::Byte) {
    self.AF.write_lo(byte);
  }

  fn util_is_flag_set(&self, byte: types::Byte) -> bool {
    (self.AF.read_lo() & byte) != 0
  }

  fn util_toggle_flag(&mut self, byte: types::Byte) {
    let previous_flags = self.AF.read_lo();
    self.AF.write_lo(previous_flags | byte);
  }
}

fn shared_xor_n(cpu: &mut CPU, byte: types::Byte)
{
    let result = cpu.AF.read_hi() ^ byte;
    cpu.write_byte_reg(RegEnum::A, result);
    cpu.util_clear_all_flags();
    cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_ld_n_n(cpu: &mut CPU, regEnum: RegEnum, byte: types::Byte) {
  cpu.write_byte_reg(regEnum, byte);
  cpu.PC += 1;
}

fn shared_inc_byte_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_byte_reg(regEnum).wrapping_add(1);
  cpu.write_byte_reg(regEnum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x00 {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_dec_byte_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_byte_reg(regEnum).wrapping_sub(1);
  cpu.write_byte_reg(regEnum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_set_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x0F {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_dec_word_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_word_reg(regEnum).wrapping_sub(1);
  cpu.write_word_reg(regEnum, result);
}

fn shared_rotate_rr(cpu: &mut CPU, regEnum: RegEnum) {
  let carry = if cpu.util_is_flag_set(FLAG_CARRY) { 0x80 } else { 0x00 };
  let result = cpu.read_byte_reg(regEnum);

  if result & 0x01 != 0 { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  let result = result >> 1;
  let result = result | carry;
  cpu.write_byte_reg(regEnum, result);

  match regEnum {
    RegEnum::A => { cpu.util_toggle_zero_flag_from_result(result) },
    _ => {} // NOTE Only do this for Register A
  }
}

fn shared_adc(cpu: &mut CPU, byte: types::Byte) {
  panic!("not implemented!")
  //   int carry = IsSetFlag(FLAG_CARRY) ? 1 : 0;
  //   int result = AF.GetHigh() + number + carry;
  //   ClearAllFlags();
  //   ToggleZeroFlagFromResult(static_cast<u8> (result));
  //   if (result > 0xFF)
  //   {
  //       ToggleFlag(FLAG_CARRY);
  //   }
  //   if (((AF.GetHigh()& 0x0F) + (number & 0x0F) + carry) > 0x0F)
  //   {
  //       ToggleFlag(FLAG_HALF);
  //   }
  //   AF.SetHigh(static_cast<u8> (result));
}

fn shared_cp(cpu: &mut CPU, byte: types::Byte) {
  let value = cpu.read_byte_reg(RegEnum::A);
  cpu.util_set_flag(FLAG_SUB);

  if value < byte {
    cpu.util_toggle_flag(FLAG_CARRY);
  }

  if value == byte {
    cpu.util_toggle_flag(FLAG_ZERO);
  }

  if ((value.wrapping_sub(byte)) & 0xF) > (value & 0xF) {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_or_n(cpu: &mut CPU, byte: types::Byte) {
  let result = cpu.read_byte_reg(RegEnum::A) | byte;
  cpu.write_byte_reg(RegEnum::A, result);
  cpu.util_clear_all_flags();
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_and_n(cpu: &mut CPU, byte: types::Byte) {
  let result = cpu.read_byte_reg(RegEnum::A) & byte;
  cpu.write_byte_reg(RegEnum::A, result);
  cpu.util_set_flag(FLAG_HALF_CARRY);
  cpu.util_toggle_zero_flag_from_result(result);
}

#[test]
fn register_setting() {
  let mut register = Register::new();
  register.write(0xAABB);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xBB, register.read_lo());
  assert_eq!(0xAABB, register.read());

  register.write_lo(0x00);
  register.write_lo(0xFF);
  register.write_lo(0xCC);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xCC, register.read_lo());
  assert_eq!(0xAACC, register.read());

  register.write_hi(0xDD);
  assert_eq!(0xDD, register.read_hi());
  assert_eq!(0xCC, register.read_lo());
  assert_eq!(0xDDCC, register.read());
}
//
// #[test]
// fn opcode_nop() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::NOP] };
//
//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.PC);
// }
//
// #[test]
// fn opcode_jp_nn() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::JP_NN, 0x50, 0x01] };
//
//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(12, cycles);
//   assert_eq!(0x0150, cpu.PC);
// }

#[test]
fn opcode_xor_a() {
  let mut cpu = CPU::new();
  let mut mmu = mmu::MMU::new();
  mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::XOR_A] };

  cpu.AF.write_hi(0xFF);

  let cycles = cpu.execute_next_opcode(&mut mmu);
  assert_eq!(0x00, cpu.AF.read_hi());
  assert_eq!(4, cycles);
  assert_eq!(0x0001, cpu.PC);
  assert!(cpu.util_is_flag_set(FLAG_ZERO));
  assert!(!cpu.util_is_flag_set(FLAG_SUB));
  assert!(!cpu.util_is_flag_set(FLAG_HALF_CARRY));
  assert!(!cpu.util_is_flag_set(FLAG_CARRY));
}

#[test]
fn opcode_xor_b() {
  let mut cpu = CPU::new();
  let mut mmu = mmu::MMU::new();
  mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::XOR_B] };

  //     11100101 = E5
  // XOR 11010100 = D4
  //     00110001 = 31
  cpu.BC.write_hi(0xE5);
  cpu.AF.write_hi(0xD4);

  let cycles = cpu.execute_next_opcode(&mut mmu);
  assert_eq!(0x31, cpu.AF.read_hi());
  assert_eq!(4, cycles);
  assert_eq!(0x0001, cpu.PC);
  assert!(!cpu.util_is_flag_set(FLAG_ZERO));
  assert!(!cpu.util_is_flag_set(FLAG_SUB));
  assert!(!cpu.util_is_flag_set(FLAG_HALF_CARRY));
  assert!(!cpu.util_is_flag_set(FLAG_CARRY));
}

// #[test]
// fn opcode_dec_b() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::DEC_B] };
//
//   cpu.BC.write_hi(0xF0);
//
//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   // assert_eq!(0x31, cpu.AF.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.PC);
//   assert!(!cpu.util_is_flag_set(FLAG_ZERO));
//   assert!(cpu.util_is_flag_set(FLAG_SUB));
//   assert!(cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }

// Unused cb prefixed opcodes
// 0x00 => { debug!("RLC B : rlc_b() not implemented! {:#X}", opcode); 42 },
// 0x01 => { debug!("RLC C : rlc_c() not implemented! {:#X}", opcode); 42 },
// 0x02 => { debug!("RLC D : rlc_d() not implemented! {:#X}", opcode); 42 },
// 0x03 => { debug!("RLC E : rlc_e() not implemented! {:#X}", opcode); 42 },
// 0x04 => { debug!("RLC H : rlc_h() not implemented! {:#X}", opcode); 42 },
// 0x05 => { debug!("RLC L : rlc_l() not implemented! {:#X}", opcode); 42 },
// 0x06 => { debug!("RLC (HL) : rlc_hl() not implemented! {:#X}", opcode); 42 },
// 0x07 => { debug!("RLC A : rlc_a() not implemented! {:#X}", opcode); 42 },
// 0x08 => { debug!("RRC B : rrc_b() not implemented! {:#X}", opcode); 42 },
// 0x09 => { debug!("RRC C : rrc_c() not implemented! {:#X}", opcode); 42 },
// 0x0A => { debug!("RRC D : rrc_d() not implemented! {:#X}", opcode); 42 },
// 0x0B => { debug!("RRC E : rrc_e() not implemented! {:#X}", opcode); 42 },
// 0x0C => { debug!("RRC H : rrc_h() not implemented! {:#X}", opcode); 42 },
// 0x0D => { debug!("RRC L : rrc_l() not implemented! {:#X}", opcode); 42 },
// 0x0E => { debug!("RRC (HL) : rrc_hl() not implemented! {:#X}", opcode); 42 },
// 0x0F => { debug!("RRC A : rrc_a() not implemented! {:#X}", opcode); 42 },
// 0x10 => { debug!("RL B : rl_b() not implemented! {:#X}", opcode); 42 },
// 0x11 => { debug!("RL C : rl_c() not implemented! {:#X}", opcode); 42 },
// 0x12 => { debug!("RL D : rl_d() not implemented! {:#X}", opcode); 42 },
// 0x13 => { debug!("RL E : rl_e() not implemented! {:#X}", opcode); 42 },
// 0x14 => { debug!("RL H : rl_h() not implemented! {:#X}", opcode); 42 },
// 0x15 => { debug!("RL L : rl_l() not implemented! {:#X}", opcode); 42 },
// 0x16 => { debug!("RL (HL) : rl_hl() not implemented! {:#X}", opcode); 42 },
// 0x17 => { debug!("RL A : rl_a() not implemented! {:#X}", opcode); 42 },
// 0x18 => { debug!("RR B : rr_b() not implemented! {:#X}", opcode); 42 },
// 0x19 => { debug!("RR C : rr_c() not implemented! {:#X}", opcode); 42 },
// 0x1A => { debug!("RR D : rr_d() not implemented! {:#X}", opcode); 42 },
// 0x1B => { debug!("RR E : rr_e() not implemented! {:#X}", opcode); 42 },
// 0x1C => { debug!("RR H : rr_h() not implemented! {:#X}", opcode); 42 },
// 0x1D => { debug!("RR L : rr_l() not implemented! {:#X}", opcode); 42 },
// 0x1E => { debug!("RR (HL) : rr_hl() not implemented! {:#X}", opcode); 42 },
// 0x1F => { debug!("RR A : rr_a() not implemented! {:#X}", opcode); 42 },
// 0x20 => { debug!("SLA B : sla_b() not implemented! {:#X}", opcode); 42 },
// 0x21 => { debug!("SLA C : sla_c() not implemented! {:#X}", opcode); 42 },
// 0x22 => { debug!("SLA D : sla_d() not implemented! {:#X}", opcode); 42 },
// 0x23 => { debug!("SLA E : sla_e() not implemented! {:#X}", opcode); 42 },
// 0x24 => { debug!("SLA H : sla_h() not implemented! {:#X}", opcode); 42 },
// 0x25 => { debug!("SLA L : sla_l() not implemented! {:#X}", opcode); 42 },
// 0x26 => { debug!("SLA (HL) : sla_hl() not implemented! {:#X}", opcode); 42 },
// 0x27 => { debug!("SLA A : sla_a() not implemented! {:#X}", opcode); 42 },
// 0x28 => { debug!("SRA B : sra_b() not implemented! {:#X}", opcode); 42 },
// 0x29 => { debug!("SRA C : sra_c() not implemented! {:#X}", opcode); 42 },
// 0x2A => { debug!("SRA D : sra_d() not implemented! {:#X}", opcode); 42 },
// 0x2B => { debug!("SRA E : sra_e() not implemented! {:#X}", opcode); 42 },
// 0x2C => { debug!("SRA H : sra_h() not implemented! {:#X}", opcode); 42 },
// 0x2D => { debug!("SRA L : sra_l() not implemented! {:#X}", opcode); 42 },
// 0x2E => { debug!("SRA (HL) : sra_hl() not implemented! {:#X}", opcode); 42 },
// 0x2F => { debug!("SRA A : sra_a() not implemented! {:#X}", opcode); 42 },
// 0x30 => { debug!("SWAP B : swap_b() not implemented! {:#X}", opcode); 42 },
// 0x31 => { debug!("SWAP C : swap_c() not implemented! {:#X}", opcode); 42 },
// 0x32 => { debug!("SWAP D : swap_d() not implemented! {:#X}", opcode); 42 },
// 0x33 => { debug!("SWAP E : swap_e() not implemented! {:#X}", opcode); 42 },
// 0x34 => { debug!("SWAP H : swap_h() not implemented! {:#X}", opcode); 42 },
// 0x35 => { debug!("SWAP L : swap_l() not implemented! {:#X}", opcode); 42 },
// 0x36 => { debug!("SWAP (HL) : swap_hl() not implemented! {:#X}", opcode); 42 },
// 0x37 => { debug!("SWAP A : swap_a() not implemented! {:#X}", opcode); 42 },
// 0x38 => { debug!("SRL B : srl_b() not implemented! {:#X}", opcode); 42 },
// 0x39 => { debug!("SRL C : srl_c() not implemented! {:#X}", opcode); 42 },
// 0x3A => { debug!("SRL D : srl_d() not implemented! {:#X}", opcode); 42 },
// 0x3B => { debug!("SRL E : srl_e() not implemented! {:#X}", opcode); 42 },
// 0x3C => { debug!("SRL H : srl_h() not implemented! {:#X}", opcode); 42 },
// 0x3D => { debug!("SRL L : srl_l() not implemented! {:#X}", opcode); 42 },
// 0x3E => { debug!("SRL (HL) : srl_hl() not implemented! {:#X}", opcode); 42 },
// 0x3F => { debug!("SRL A : srl_a() not implemented! {:#X}", opcode); 42 },
// 0x40 => { debug!("BIT 0 B : bit_0_b() not implemented! {:#X}", opcode); 42 },
// 0x41 => { debug!("BIT 0 C : bit_0_c() not implemented! {:#X}", opcode); 42 },
// 0x42 => { debug!("BIT 0 D : bit_0_d() not implemented! {:#X}", opcode); 42 },
// 0x43 => { debug!("BIT 0 E : bit_0_e() not implemented! {:#X}", opcode); 42 },
// 0x44 => { debug!("BIT 0 H : bit_0_h() not implemented! {:#X}", opcode); 42 },
// 0x45 => { debug!("BIT 0 L : bit_0_l() not implemented! {:#X}", opcode); 42 },
// 0x46 => { debug!("BIT 0 (HL) : bit_0_hl() not implemented! {:#X}", opcode); 42 },
// 0x47 => { debug!("BIT 0 A : bit_0_a() not implemented! {:#X}", opcode); 42 },
// 0x48 => { debug!("BIT 1 B : bit_1_b() not implemented! {:#X}", opcode); 42 },
// 0x49 => { debug!("BIT 1 C : bit_1_c() not implemented! {:#X}", opcode); 42 },
// 0x4A => { debug!("BIT 1 D : bit_1_d() not implemented! {:#X}", opcode); 42 },
// 0x4B => { debug!("BIT 1 E : bit_1_e() not implemented! {:#X}", opcode); 42 },
// 0x4C => { debug!("BIT 1 H : bit_1_h() not implemented! {:#X}", opcode); 42 },
// 0x4D => { debug!("BIT 1 L : bit_1_l() not implemented! {:#X}", opcode); 42 },
// 0x4E => { debug!("BIT 1 (HL) : bit_1_hl() not implemented! {:#X}", opcode); 42 },
// 0x4F => { debug!("BIT 1 A : bit_1_a() not implemented! {:#X}", opcode); 42 },
// 0x50 => { debug!("BIT 2 B : bit_2_b() not implemented! {:#X}", opcode); 42 },
// 0x51 => { debug!("BIT 2 C : bit_2_c() not implemented! {:#X}", opcode); 42 },
// 0x52 => { debug!("BIT 2 D : bit_2_d() not implemented! {:#X}", opcode); 42 },
// 0x53 => { debug!("BIT 2 E : bit_2_e() not implemented! {:#X}", opcode); 42 },
// 0x54 => { debug!("BIT 2 H : bit_2_h() not implemented! {:#X}", opcode); 42 },
// 0x55 => { debug!("BIT 2 L : bit_2_l() not implemented! {:#X}", opcode); 42 },
// 0x56 => { debug!("BIT 2 (HL) : bit_2_hl() not implemented! {:#X}", opcode); 42 },
// 0x57 => { debug!("BIT 2 A : bit_2_a() not implemented! {:#X}", opcode); 42 },
// 0x58 => { debug!("BIT 3 B : bit_3_b() not implemented! {:#X}", opcode); 42 },
// 0x59 => { debug!("BIT 3 C : bit_3_c() not implemented! {:#X}", opcode); 42 },
// 0x5A => { debug!("BIT 3 D : bit_3_d() not implemented! {:#X}", opcode); 42 },
// 0x5B => { debug!("BIT 3 E : bit_3_e() not implemented! {:#X}", opcode); 42 },
// 0x5C => { debug!("BIT 3 H : bit_3_h() not implemented! {:#X}", opcode); 42 },
// 0x5D => { debug!("BIT 3 L : bit_3_l() not implemented! {:#X}", opcode); 42 },
// 0x5E => { debug!("BIT 3 (HL) : bit_3_hl() not implemented! {:#X}", opcode); 42 },
// 0x5F => { debug!("BIT 3 A : bit_3_a() not implemented! {:#X}", opcode); 42 },
// 0x60 => { debug!("BIT 4 B : bit_4_b() not implemented! {:#X}", opcode); 42 },
// 0x61 => { debug!("BIT 4 C : bit_4_c() not implemented! {:#X}", opcode); 42 },
// 0x62 => { debug!("BIT 4 D : bit_4_d() not implemented! {:#X}", opcode); 42 },
// 0x63 => { debug!("BIT 4 E : bit_4_e() not implemented! {:#X}", opcode); 42 },
// 0x64 => { debug!("BIT 4 H : bit_4_h() not implemented! {:#X}", opcode); 42 },
// 0x65 => { debug!("BIT 4 L : bit_4_l() not implemented! {:#X}", opcode); 42 },
// 0x66 => { debug!("BIT 4 (HL) : bit_4_hl() not implemented! {:#X}", opcode); 42 },
// 0x67 => { debug!("BIT 4 A : bit_4_a() not implemented! {:#X}", opcode); 42 },
// 0x68 => { debug!("BIT 5 B : bit_5_b() not implemented! {:#X}", opcode); 42 },
// 0x69 => { debug!("BIT 5 C : bit_5_c() not implemented! {:#X}", opcode); 42 },
// 0x6A => { debug!("BIT 5 D : bit_5_d() not implemented! {:#X}", opcode); 42 },
// 0x6B => { debug!("BIT 5 E : bit_5_e() not implemented! {:#X}", opcode); 42 },
// 0x6C => { debug!("BIT 5 H : bit_5_h() not implemented! {:#X}", opcode); 42 },
// 0x6D => { debug!("BIT 5 L : bit_5_l() not implemented! {:#X}", opcode); 42 },
// 0x6E => { debug!("BIT 5 (HL) : bit_5_hl() not implemented! {:#X}", opcode); 42 },
// 0x6F => { debug!("BIT 5 A : bit_5_a() not implemented! {:#X}", opcode); 42 },
// 0x70 => { debug!("BIT 6 B : bit_6_b() not implemented! {:#X}", opcode); 42 },
// 0x71 => { debug!("BIT 6 C : bit_6_c() not implemented! {:#X}", opcode); 42 },
// 0x72 => { debug!("BIT 6 D : bit_6_d() not implemented! {:#X}", opcode); 42 },
// 0x73 => { debug!("BIT 6 E : bit_6_e() not implemented! {:#X}", opcode); 42 },
// 0x74 => { debug!("BIT 6 H : bit_6_h() not implemented! {:#X}", opcode); 42 },
// 0x75 => { debug!("BIT 6 L : bit_6_l() not implemented! {:#X}", opcode); 42 },
// 0x76 => { debug!("BIT 6 (HL) : bit_6_hl() not implemented! {:#X}", opcode); 42 },
// 0x77 => { debug!("BIT 6 A : bit_6_a() not implemented! {:#X}", opcode); 42 },
// 0x78 => { debug!("BIT 7 B : bit_7_b() not implemented! {:#X}", opcode); 42 },
// 0x79 => { debug!("BIT 7 C : bit_7_c() not implemented! {:#X}", opcode); 42 },
// 0x7A => { debug!("BIT 7 D : bit_7_d() not implemented! {:#X}", opcode); 42 },
// 0x7B => { debug!("BIT 7 E : bit_7_e() not implemented! {:#X}", opcode); 42 },
// 0x7C => { debug!("BIT 7 H : bit_7_h() not implemented! {:#X}", opcode); 42 },
// 0x7D => { debug!("BIT 7 L : bit_7_l() not implemented! {:#X}", opcode); 42 },
// 0x7E => { debug!("BIT 7 (HL) : bit_7_hl() not implemented! {:#X}", opcode); 42 },
// 0x7F => { debug!("BIT 7 A : bit_7_a() not implemented! {:#X}", opcode); 42 },
// 0x80 => { debug!("RES 0 B : res_0_b() not implemented! {:#X}", opcode); 42 },
// 0x81 => { debug!("RES 0 C : res_0_c() not implemented! {:#X}", opcode); 42 },
// 0x82 => { debug!("RES 0 D : res_0_d() not implemented! {:#X}", opcode); 42 },
// 0x83 => { debug!("RES 0 E : res_0_e() not implemented! {:#X}", opcode); 42 },
// 0x84 => { debug!("RES 0 H : res_0_h() not implemented! {:#X}", opcode); 42 },
// 0x85 => { debug!("RES 0 L : res_0_l() not implemented! {:#X}", opcode); 42 },
// 0x86 => { debug!("RES 0 (HL) : res_0_hl() not implemented! {:#X}", opcode); 42 },
// 0x87 => { debug!("RES 0 A : res_0_a() not implemented! {:#X}", opcode); 42 },
// 0x88 => { debug!("RES 1 B : res_1_b() not implemented! {:#X}", opcode); 42 },
// 0x89 => { debug!("RES 1 C : res_1_c() not implemented! {:#X}", opcode); 42 },
// 0x8A => { debug!("RES 1 D : res_1_d() not implemented! {:#X}", opcode); 42 },
// 0x8B => { debug!("RES 1 E : res_1_e() not implemented! {:#X}", opcode); 42 },
// 0x8C => { debug!("RES 1 H : res_1_h() not implemented! {:#X}", opcode); 42 },
// 0x8D => { debug!("RES 1 L : res_1_l() not implemented! {:#X}", opcode); 42 },
// 0x8E => { debug!("RES 1 (HL) : res_1_hl() not implemented! {:#X}", opcode); 42 },
// 0x8F => { debug!("RES 1 A : res_1_a() not implemented! {:#X}", opcode); 42 },
// 0x90 => { debug!("RES 2 B : res_2_b() not implemented! {:#X}", opcode); 42 },
// 0x91 => { debug!("RES 2 C : res_2_c() not implemented! {:#X}", opcode); 42 },
// 0x92 => { debug!("RES 2 D : res_2_d() not implemented! {:#X}", opcode); 42 },
// 0x93 => { debug!("RES 2 E : res_2_e() not implemented! {:#X}", opcode); 42 },
// 0x94 => { debug!("RES 2 H : res_2_h() not implemented! {:#X}", opcode); 42 },
// 0x95 => { debug!("RES 2 L : res_2_l() not implemented! {:#X}", opcode); 42 },
// 0x96 => { debug!("RES 2 (HL) : res_2_hl() not implemented! {:#X}", opcode); 42 },
// 0x97 => { debug!("RES 2 A : res_2_a() not implemented! {:#X}", opcode); 42 },
// 0x98 => { debug!("RES 3 B : res_3_b() not implemented! {:#X}", opcode); 42 },
// 0x99 => { debug!("RES 3 C : res_3_c() not implemented! {:#X}", opcode); 42 },
// 0x9A => { debug!("RES 3 D : res_3_d() not implemented! {:#X}", opcode); 42 },
// 0x9B => { debug!("RES 3 E : res_3_e() not implemented! {:#X}", opcode); 42 },
// 0x9C => { debug!("RES 3 H : res_3_h() not implemented! {:#X}", opcode); 42 },
// 0x9D => { debug!("RES 3 L : res_3_l() not implemented! {:#X}", opcode); 42 },
// 0x9E => { debug!("RES 3 (HL) : res_3_hl() not implemented! {:#X}", opcode); 42 },
// 0x9F => { debug!("RES 3 A : res_3_a() not implemented! {:#X}", opcode); 42 },
// 0xA0 => { debug!("RES 4 B : res_4_b() not implemented! {:#X}", opcode); 42 },
// 0xA1 => { debug!("RES 4 C : res_4_c() not implemented! {:#X}", opcode); 42 },
// 0xA2 => { debug!("RES 4 D : res_4_d() not implemented! {:#X}", opcode); 42 },
// 0xA3 => { debug!("RES 4 E : res_4_e() not implemented! {:#X}", opcode); 42 },
// 0xA4 => { debug!("RES 4 H : res_4_h() not implemented! {:#X}", opcode); 42 },
// 0xA5 => { debug!("RES 4 L : res_4_l() not implemented! {:#X}", opcode); 42 },
// 0xA6 => { debug!("RES 4 (HL) : res_4_hl() not implemented! {:#X}", opcode); 42 },
// 0xA7 => { debug!("RES 4 A : res_4_a() not implemented! {:#X}", opcode); 42 },
// 0xA8 => { debug!("RES 5 B : res_5_b() not implemented! {:#X}", opcode); 42 },
// 0xA9 => { debug!("RES 5 C : res_5_c() not implemented! {:#X}", opcode); 42 },
// 0xAA => { debug!("RES 5 D : res_5_d() not implemented! {:#X}", opcode); 42 },
// 0xAB => { debug!("RES 5 E : res_5_e() not implemented! {:#X}", opcode); 42 },
// 0xAC => { debug!("RES 5 H : res_5_h() not implemented! {:#X}", opcode); 42 },
// 0xAD => { debug!("RES 5 L : res_5_l() not implemented! {:#X}", opcode); 42 },
// 0xAE => { debug!("RES 5 (HL) : res_5_hl() not implemented! {:#X}", opcode); 42 },
// 0xAF => { debug!("RES 5 A : res_5_a() not implemented! {:#X}", opcode); 42 },
// 0xB0 => { debug!("RES 6 B : res_6_b() not implemented! {:#X}", opcode); 42 },
// 0xB1 => { debug!("RES 6 C : res_6_c() not implemented! {:#X}", opcode); 42 },
// 0xB2 => { debug!("RES 6 D : res_6_d() not implemented! {:#X}", opcode); 42 },
// 0xB3 => { debug!("RES 6 E : res_6_e() not implemented! {:#X}", opcode); 42 },
// 0xB4 => { debug!("RES 6 H : res_6_h() not implemented! {:#X}", opcode); 42 },
// 0xB5 => { debug!("RES 6 L : res_6_l() not implemented! {:#X}", opcode); 42 },
// 0xB6 => { debug!("RES 6 (HL) : res_6_hl() not implemented! {:#X}", opcode); 42 },
// 0xB7 => { debug!("RES 6 A : res_6_a() not implemented! {:#X}", opcode); 42 },
// 0xB8 => { debug!("RES 7 B : res_7_b() not implemented! {:#X}", opcode); 42 },
// 0xB9 => { debug!("RES 7 C : res_7_c() not implemented! {:#X}", opcode); 42 },
// 0xBA => { debug!("RES 7 D : res_7_d() not implemented! {:#X}", opcode); 42 },
// 0xBB => { debug!("RES 7 E : res_7_e() not implemented! {:#X}", opcode); 42 },
// 0xBC => { debug!("RES 7 H : res_7_h() not implemented! {:#X}", opcode); 42 },
// 0xBD => { debug!("RES 7 L : res_7_l() not implemented! {:#X}", opcode); 42 },
// 0xBE => { debug!("RES 7 (HL) : res_7_hl() not implemented! {:#X}", opcode); 42 },
// 0xBF => { debug!("RES 7 A : res_7_a() not implemented! {:#X}", opcode); 42 },
// 0xC0 => { debug!("SET 0 B : set_0_b() not implemented! {:#X}", opcode); 42 },
// 0xC1 => { debug!("SET 0 C : set_0_c() not implemented! {:#X}", opcode); 42 },
// 0xC2 => { debug!("SET 0 D : set_0_d() not implemented! {:#X}", opcode); 42 },
// 0xC3 => { debug!("SET 0 E : set_0_e() not implemented! {:#X}", opcode); 42 },
// 0xC4 => { debug!("SET 0 H : set_0_h() not implemented! {:#X}", opcode); 42 },
// 0xC5 => { debug!("SET 0 L : set_0_l() not implemented! {:#X}", opcode); 42 },
// 0xC6 => { debug!("SET 0 (HL) : set_0_hl() not implemented! {:#X}", opcode); 42 },
// 0xC7 => { debug!("SET 0 A : set_0_a() not implemented! {:#X}", opcode); 42 },
// 0xC8 => { debug!("SET 1 B : set_1_b() not implemented! {:#X}", opcode); 42 },
// 0xC9 => { debug!("SET 1 C : set_1_c() not implemented! {:#X}", opcode); 42 },
// 0xCA => { debug!("SET 1 D : set_1_d() not implemented! {:#X}", opcode); 42 },
// 0xCB => { debug!("SET 1 E : set_1_e() not implemented! {:#X}", opcode); 42 },
// 0xCC => { debug!("SET 1 H : set_1_h() not implemented! {:#X}", opcode); 42 },
// 0xCD => { debug!("SET 1 L : set_1_l() not implemented! {:#X}", opcode); 42 },
// 0xCE => { debug!("SET 1 (HL) : set_1_hl() not implemented! {:#X}", opcode); 42 },
// 0xCF => { debug!("SET 1 A : set_1_a() not implemented! {:#X}", opcode); 42 },
// 0xD0 => { debug!("SET 2 B : set_2_b() not implemented! {:#X}", opcode); 42 },
// 0xD1 => { debug!("SET 2 C : set_2_c() not implemented! {:#X}", opcode); 42 },
// 0xD2 => { debug!("SET 2 D : set_2_d() not implemented! {:#X}", opcode); 42 },
// 0xD3 => { debug!("SET 2 E : set_2_e() not implemented! {:#X}", opcode); 42 },
// 0xD4 => { debug!("SET 2 H : set_2_h() not implemented! {:#X}", opcode); 42 },
// 0xD5 => { debug!("SET 2 L : set_2_l() not implemented! {:#X}", opcode); 42 },
// 0xD6 => { debug!("SET 2 (HL) : set_2_hl() not implemented! {:#X}", opcode); 42 },
// 0xD7 => { debug!("SET 2 A : set_2_a() not implemented! {:#X}", opcode); 42 },
// 0xD8 => { debug!("SET 3 B : set_3_b() not implemented! {:#X}", opcode); 42 },
// 0xD9 => { debug!("SET 3 C : set_3_c() not implemented! {:#X}", opcode); 42 },
// 0xDA => { debug!("SET 3 D : set_3_d() not implemented! {:#X}", opcode); 42 },
// 0xDB => { debug!("SET 3 E : set_3_e() not implemented! {:#X}", opcode); 42 },
// 0xDC => { debug!("SET 3 H : set_3_h() not implemented! {:#X}", opcode); 42 },
// 0xDD => { debug!("SET 3 L : set_3_l() not implemented! {:#X}", opcode); 42 },
// 0xDE => { debug!("SET 3 (HL) : set_3_hl() not implemented! {:#X}", opcode); 42 },
// 0xDF => { debug!("SET 3 A : set_3_a() not implemented! {:#X}", opcode); 42 },
// 0xE0 => { debug!("SET 4 B : set_4_b() not implemented! {:#X}", opcode); 42 },
// 0xE1 => { debug!("SET 4 C : set_4_c() not implemented! {:#X}", opcode); 42 },
// 0xE2 => { debug!("SET 4 D : set_4_d() not implemented! {:#X}", opcode); 42 },
// 0xE3 => { debug!("SET 4 E : set_4_e() not implemented! {:#X}", opcode); 42 },
// 0xE4 => { debug!("SET 4 H : set_4_h() not implemented! {:#X}", opcode); 42 },
// 0xE5 => { debug!("SET 4 L : set_4_l() not implemented! {:#X}", opcode); 42 },
// 0xE6 => { debug!("SET 4 (HL) : set_4_hl() not implemented! {:#X}", opcode); 42 },
// 0xE7 => { debug!("SET 4 A : set_4_a() not implemented! {:#X}", opcode); 42 },
// 0xE8 => { debug!("SET 5 B : set_5_b() not implemented! {:#X}", opcode); 42 },
// 0xE9 => { debug!("SET 5 C : set_5_c() not implemented! {:#X}", opcode); 42 },
// 0xEA => { debug!("SET 5 D : set_5_d() not implemented! {:#X}", opcode); 42 },
// 0xEB => { debug!("SET 5 E : set_5_e() not implemented! {:#X}", opcode); 42 },
// 0xEC => { debug!("SET 5 H : set_5_h() not implemented! {:#X}", opcode); 42 },
// 0xED => { debug!("SET 5 L : set_5_l() not implemented! {:#X}", opcode); 42 },
// 0xEE => { debug!("SET 5 (HL) : set_5_hl() not implemented! {:#X}", opcode); 42 },
// 0xEF => { debug!("SET 5 A : set_5_a() not implemented! {:#X}", opcode); 42 },
// 0xF0 => { debug!("SET 6 B : set_6_b() not implemented! {:#X}", opcode); 42 },
// 0xF1 => { debug!("SET 6 C : set_6_c() not implemented! {:#X}", opcode); 42 },
// 0xF2 => { debug!("SET 6 D : set_6_d() not implemented! {:#X}", opcode); 42 },
// 0xF3 => { debug!("SET 6 E : set_6_e() not implemented! {:#X}", opcode); 42 },
// 0xF4 => { debug!("SET 6 H : set_6_h() not implemented! {:#X}", opcode); 42 },
// 0xF5 => { debug!("SET 6 L : set_6_l() not implemented! {:#X}", opcode); 42 },
// 0xF6 => { debug!("SET 6 (HL) : set_6_hl() not implemented! {:#X}", opcode); 42 },
// 0xF7 => { debug!("SET 6 A : set_6_a() not implemented! {:#X}", opcode); 42 },
// 0xF8 => { debug!("SET 7 B : set_7_b() not implemented! {:#X}", opcode); 42 },
// 0xF9 => { debug!("SET 7 C : set_7_c() not implemented! {:#X}", opcode); 42 },
// 0xFA => { debug!("SET 7 D : set_7_d() not implemented! {:#X}", opcode); 42 },
// 0xFB => { debug!("SET 7 E : set_7_e() not implemented! {:#X}", opcode); 42 },
// 0xFC => { debug!("SET 7 H : set_7_h() not implemented! {:#X}", opcode); 42 },
// 0xFD => { debug!("SET 7 L : set_7_l() not implemented! {:#X}", opcode); 42 },
// 0xFE => { debug!("SET 7 (HL) : set_7_hl() not implemented! {:#X}", opcode); 42 },
// 0xFF => { debug!("SET 7 A : set_7_a() not implemented! {:#X}", opcode); 42 },
