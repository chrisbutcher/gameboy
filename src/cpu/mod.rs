use std::fmt;

pub use super::types;
pub use super::mmu;
pub use super::cartridge;
pub mod opcodes;

const FLAG_ZERO: types::Byte = 0x80; // Zero
const FLAG_SUB: types::Byte = 0x40; // Negative,
const FLAG_HALF_CARRY: types::Byte = 0x20; // Half-carry
const FLAG_CARRY: types::Byte = 0x10; // Carry

const FLAG_NONE: types::Byte = 0x00; // None

#[derive(Debug, Copy, Clone)]
pub enum RegEnum {
  A, F, B, C, D, E, H, L,
  AF, BC, DE, HL
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
  pub BranchTaken: bool // so far unused
}

fn formatted_flags(cpu: &CPU) -> String {
  format!("Z: {}, N: {}, H: {} C: {}", cpu.util_is_flag_set(FLAG_ZERO) as u8, cpu.util_is_flag_set(FLAG_SUB) as u8, cpu.util_is_flag_set(FLAG_HALF_CARRY) as u8, cpu.util_is_flag_set(FLAG_CARRY) as u8)
}

impl fmt::Debug for CPU {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[PC] {:#X} [Regs] A:{:#X}, F:{:#X}, B:{:#X}, C:{:#X}, D:{:#X}, E:{:#X}, H:{:#X}, L:{:#X} - FLAGS: {}",
      self.PC, self.AF.read_hi(), self.AF.read_lo(), self.BC.read_hi(), self.BC.read_lo(), self.DE.read_hi(), self.DE.read_lo(), self.HL.read_hi(), self.HL.read_lo(), formatted_flags(self)
    )
  }
}

impl CPU {
  pub fn new() -> CPU {
      CPU {
        AF: Register::new(), BC: Register::new(), DE: Register::new(), HL: Register::new(),
        SP: Register::new(), PC: 0x0000, BranchTaken: false
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
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  pub fn write_word_reg(&mut self, regEnum: RegEnum, word: types::Word) {
    match regEnum {
      RegEnum::AF => { self.AF.write(word) },
      RegEnum::BC => { self.BC.write(word) },
      RegEnum::DE => { self.DE.write(word) },
      RegEnum::HL => { self.HL.write(word) },
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
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  pub fn read_word_reg(&mut self, regEnum: RegEnum) -> types::Word {
    match regEnum {
      RegEnum::AF => { self.AF.read() },
      RegEnum::BC => { self.BC.read() },
      RegEnum::DE => { self.DE.read() },
      RegEnum::HL => { self.HL.read() },
      _ => panic!("Unexpected regEnum: {:?}", regEnum)
    }
  }

  // pub fn write_word_reg() {
  // }

  pub fn execute_next_opcode(&mut self, mmu: &mut mmu::MMU) -> i32 {
    let opcode: types::Byte = mmu.read(self.PC);

    println!("{:?}", self);
    self.PC += 1;
    let cycles = self.execute_opcode(opcode, mmu);

    if cycles == 42 {
      panic!("Unexpected opcode: {}", opcode);
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
      0x00 => { println!("NOP"); self.nop(); 4 },
      0x01 => { println!("LD BC,nn : ld_bc_nn() not implemented! {:#X}", opcode); 42 },
      0x02 => { println!("LD (BC),A : ld_bc_a() not implemented! {:#X}", opcode); 42 },
      0x03 => { println!("INC BC : inc_bc() not implemented! {:#X}", opcode); 42 },
      0x04 => { println!("INC B : inc_b() not implemented! {:#X}", opcode); 42 },
      0x05 => { println!("DEC B"); self.dec_b(); 4 },
      0x06 => { println!("LD B,n"); self.ld_b_n(mmu); 8 },
      0x07 => { println!("RLCA : rlca() not implemented! {:#X}", opcode); 42 },
      0x08 => { println!("LD (nn),SP : ld_nn_sp() not implemented! {:#X}", opcode); 42 },
      0x09 => { println!("ADD HL,BC : add_hl_bc() not implemented! {:#X}", opcode); 42 },
      0x0A => { println!("LD A,(BC) : ld_a_bc() not implemented! {:#X}", opcode); 42 },
      0x0B => { println!("DEC BC : dec_bc() not implemented! {:#X}", opcode); 42 },
      0x0C => { println!("INC C : inc_c() not implemented! {:#X}", opcode); 42 },
      0x0D => { println!("DEC C : dec_c() not implemented! {:#X}", opcode); 42 },
      0x0E => { println!("LD C,n"); self.ld_c_n(mmu); 8 },
      0x0F => { println!("RRCA : rrca() not implemented! {:#X}", opcode); 42 },
      0x10 => { println!("STOP : stop() not implemented! {:#X}", opcode); 42 },
      0x11 => { println!("LD DE,nn : ld_de_nn() not implemented! {:#X}", opcode); 42 },
      0x12 => { println!("LD (DE),A : ld_de_a() not implemented! {:#X}", opcode); 42 },
      0x13 => { println!("INC DE : inc_de() not implemented! {:#X}", opcode); 42 },
      0x14 => { println!("INC D : inc_d() not implemented! {:#X}", opcode); 42 },
      0x15 => { println!("DEC D : dec_d() not implemented! {:#X}", opcode); 42 },
      0x16 => { println!("LD D,n : ld_d_n() not implemented! {:#X}", opcode); 42 },
      0x17 => { println!("RLA : rla() not implemented! {:#X}", opcode); 42 },
      0x18 => { println!("JR n : jr_n() not implemented! {:#X}", opcode); 42 },
      0x19 => { println!("ADD HL,DE : add_hl_de() not implemented! {:#X}", opcode); 42 },
      0x1A => { println!("LD A,(DE) : ld_a_de() not implemented! {:#X}", opcode); 42 },
      0x1B => { println!("DEC DE : dec_de() not implemented! {:#X}", opcode); 42 },
      0x1C => { println!("INC E : inc_e() not implemented! {:#X}", opcode); 42 },
      0x1D => { println!("DEC E : dec_e() not implemented! {:#X}", opcode); 42 },
      0x1E => { println!("LD E,n : ld_e_n() not implemented! {:#X}", opcode); 42 },
      0x1F => { println!("RRA"); self.rra(); 4 },
      0x20 => { println!("JR NZ,n"); self.jr_nz_n(mmu); 8 },
      0x21 => { println!("LD HL,nn"); self.ld_hl_nn(mmu); 12 },
      0x22 => { println!("LD (HLI),A : ld_hli_a() not implemented! {:#X}", opcode); 42 },
      0x23 => { println!("INC HL : inc_hl() not implemented! {:#X}", opcode); 42 },
      0x24 => { println!("INC H : inc_h() not implemented! {:#X}", opcode); 42 },
      0x25 => { println!("DEC H : dec_h() not implemented! {:#X}", opcode); 42 },
      0x26 => { println!("LD H,n : ld_h_n() not implemented! {:#X}", opcode); 42 },
      0x27 => { println!("DAA : daa() not implemented! {:#X}", opcode); 42 },
      0x28 => { println!("JR Z,n : jr_z_n() not implemented! {:#X}", opcode); 42 },
      0x29 => { println!("ADD HL,HL : add_hl_hl() not implemented! {:#X}", opcode); 42 },
      0x2A => { println!("LD A,(HLI) : ld_a_hli() not implemented! {:#X}", opcode); 42 },
      0x2B => { println!("DEC HL : dec_hl() not implemented! {:#X}", opcode); 42 },
      0x2C => { println!("INC L : inc_l() not implemented! {:#X}", opcode); 42 },
      0x2D => { println!("DEC L : dec_l() not implemented! {:#X}", opcode); 42 },
      0x2E => { println!("LD L,n : ld_l_n() not implemented! {:#X}", opcode); 42 },
      0x2F => { println!("CPL : cpl() not implemented! {:#X}", opcode); 42 },
      0x30 => { println!("JR NC,n : jr_nc_n() not implemented! {:#X}", opcode); 42 },
      0x31 => { println!("LD SP,nn : ld_sp_nn() not implemented! {:#X}", opcode); 42 },
      0x32 => { println!("LD (HLD), A"); self.ld_hld_a(); 8 },
      0x33 => { println!("INC SP : inc_sp() not implemented! {:#X}", opcode); 42 },
      0x34 => { println!("INC (HL) : inc_hl() not implemented! {:#X}", opcode); 42 },
      0x35 => { println!("DEC (HL) : dec_hl() not implemented! {:#X}", opcode); 42 },
      0x36 => { println!("LD (HL),n : ld_hl_n() not implemented! {:#X}", opcode); 42 },
      0x37 => { println!("SCF : scf() not implemented! {:#X}", opcode); 42 },
      0x38 => { println!("JR C,n : jr_c_n() not implemented! {:#X}", opcode); 42 },
      0x39 => { println!("ADD HL,SP : add_hl_sp() not implemented! {:#X}", opcode); 42 },
      0x3A => { println!("LD A,(HLD) : ld_a_hld() not implemented! {:#X}", opcode); 42 },
      0x3B => { println!("DEC SP : dec_sp() not implemented! {:#X}", opcode); 42 },
      0x3C => { println!("INC A : inc_a() not implemented! {:#X}", opcode); 42 },
      0x3D => { println!("DEC A : dec_a() not implemented! {:#X}", opcode); 42 },
      0x3E => { println!("LD A,n : ld_a_n() not implemented! {:#X}", opcode); 42 },
      0x3F => { println!("CCF : ccf() not implemented! {:#X}", opcode); 42 },
      0x40 => { println!("LD B,B : ld_b_b() not implemented! {:#X}", opcode); 42 },
      0x41 => { println!("LD B,C : ld_b_c() not implemented! {:#X}", opcode); 42 },
      0x42 => { println!("LD B,D : ld_b_d() not implemented! {:#X}", opcode); 42 },
      0x43 => { println!("LD B,E : ld_b_e() not implemented! {:#X}", opcode); 42 },
      0x44 => { println!("LD B,H : ld_b_h() not implemented! {:#X}", opcode); 42 },
      0x45 => { println!("LD B,L : ld_b_l() not implemented! {:#X}", opcode); 42 },
      0x46 => { println!("LD B,(HL) : ld_b_hl() not implemented! {:#X}", opcode); 42 },
      0x47 => { println!("LD B,A : ld_b_a() not implemented! {:#X}", opcode); 42 },
      0x48 => { println!("LD C,B : ld_c_b() not implemented! {:#X}", opcode); 42 },
      0x49 => { println!("LD C,C : ld_c_c() not implemented! {:#X}", opcode); 42 },
      0x4A => { println!("LD C,D : ld_c_d() not implemented! {:#X}", opcode); 42 },
      0x4B => { println!("LD C,E : ld_c_e() not implemented! {:#X}", opcode); 42 },
      0x4C => { println!("LD C,H : ld_c_h() not implemented! {:#X}", opcode); 42 },
      0x4D => { println!("LD C,L : ld_c_l() not implemented! {:#X}", opcode); 42 },
      0x4E => { println!("LD C,(HL) : ld_c_hl() not implemented! {:#X}", opcode); 42 },
      0x4F => { println!("LD C,A : ld_c_a() not implemented! {:#X}", opcode); 42 },
      0x50 => { println!("LD D,B : ld_d_b() not implemented! {:#X}", opcode); 42 },
      0x51 => { println!("LD D,C : ld_d_c() not implemented! {:#X}", opcode); 42 },
      0x52 => { println!("LD D,D : ld_d_d() not implemented! {:#X}", opcode); 42 },
      0x53 => { println!("LD D,E : ld_d_e() not implemented! {:#X}", opcode); 42 },
      0x54 => { println!("LD D,H : ld_d_h() not implemented! {:#X}", opcode); 42 },
      0x55 => { println!("LD D,L : ld_d_l() not implemented! {:#X}", opcode); 42 },
      0x56 => { println!("LD D,(HL) : ld_d_hl() not implemented! {:#X}", opcode); 42 },
      0x57 => { println!("LD D,A : ld_d_a() not implemented! {:#X}", opcode); 42 },
      0x58 => { println!("LD E,B : ld_e_b() not implemented! {:#X}", opcode); 42 },
      0x59 => { println!("LD E,C : ld_e_c() not implemented! {:#X}", opcode); 42 },
      0x5A => { println!("LD E,D : ld_e_d() not implemented! {:#X}", opcode); 42 },
      0x5B => { println!("LD E,E : ld_e_e() not implemented! {:#X}", opcode); 42 },
      0x5C => { println!("LD E,H : ld_e_h() not implemented! {:#X}", opcode); 42 },
      0x5D => { println!("LD E,L : ld_e_l() not implemented! {:#X}", opcode); 42 },
      0x5E => { println!("LD E,(HL) : ld_e_hl() not implemented! {:#X}", opcode); 42 },
      0x5F => { println!("LD E,A : ld_e_a() not implemented! {:#X}", opcode); 42 },
      0x60 => { println!("LD H,B : ld_h_b() not implemented! {:#X}", opcode); 42 },
      0x61 => { println!("LD H,C : ld_h_c() not implemented! {:#X}", opcode); 42 },
      0x62 => { println!("LD H,D : ld_h_d() not implemented! {:#X}", opcode); 42 },
      0x63 => { println!("LD H,E : ld_h_e() not implemented! {:#X}", opcode); 42 },
      0x64 => { println!("LD H,H : ld_h_h() not implemented! {:#X}", opcode); 42 },
      0x65 => { println!("LD H,L : ld_h_l() not implemented! {:#X}", opcode); 42 },
      0x66 => { println!("LD H,(HL) : ld_h_hl() not implemented! {:#X}", opcode); 42 },
      0x67 => { println!("LD H,A : ld_h_a() not implemented! {:#X}", opcode); 42 },
      0x68 => { println!("LD L,B : ld_l_b() not implemented! {:#X}", opcode); 42 },
      0x69 => { println!("LD L,C : ld_l_c() not implemented! {:#X}", opcode); 42 },
      0x6A => { println!("LD L,D : ld_l_d() not implemented! {:#X}", opcode); 42 },
      0x6B => { println!("LD L,E : ld_l_e() not implemented! {:#X}", opcode); 42 },
      0x6C => { println!("LD L,H : ld_l_h() not implemented! {:#X}", opcode); 42 },
      0x6D => { println!("LD L,L : ld_l_l() not implemented! {:#X}", opcode); 42 },
      0x6E => { println!("LD L,(HL) : ld_l_hl() not implemented! {:#X}", opcode); 42 },
      0x6F => { println!("LD L,A : ld_l_a() not implemented! {:#X}", opcode); 42 },
      0x70 => { println!("LD (HL),B : ld_hl_b() not implemented! {:#X}", opcode); 42 },
      0x71 => { println!("LD (HL),C : ld_hl_c() not implemented! {:#X}", opcode); 42 },
      0x72 => { println!("LD (HL),D : ld_hl_d() not implemented! {:#X}", opcode); 42 },
      0x73 => { println!("LD (HL),E : ld_hl_e() not implemented! {:#X}", opcode); 42 },
      0x74 => { println!("LD (HL),H : ld_hl_h() not implemented! {:#X}", opcode); 42 },
      0x75 => { println!("LD (HL),L : ld_hl_l() not implemented! {:#X}", opcode); 42 },
      0x76 => { println!("HALT : halt() not implemented! {:#X}", opcode); 42 },
      0x77 => { println!("LD (HL),A : ld_hl_a() not implemented! {:#X}", opcode); 42 },
      0x78 => { println!("LD A,B : ld_a_b() not implemented! {:#X}", opcode); 42 },
      0x79 => { println!("LD A,C : ld_a_c() not implemented! {:#X}", opcode); 42 },
      0x7A => { println!("LD A,D"); self.ld_a_d(); 4 },
      0x7B => { println!("LD A,E : ld_a_e() not implemented! {:#X}", opcode); 42 },
      0x7C => { println!("LD A,H : ld_a_h() not implemented! {:#X}", opcode); 42 },
      0x7D => { println!("LD A,L : ld_a_l() not implemented! {:#X}", opcode); 42 },
      0x7E => { println!("LD A,(HL) : ld_a_hl() not implemented! {:#X}", opcode); 42 },
      0x7F => { println!("LD A,A : ld_a_a() not implemented! {:#X}", opcode); 42 },
      0x80 => { println!("ADD A,B : add_a_b() not implemented! {:#X}", opcode); 42 },
      0x81 => { println!("ADD A,C : add_a_c() not implemented! {:#X}", opcode); 42 },
      0x82 => { println!("ADD A,D : add_a_d() not implemented! {:#X}", opcode); 42 },
      0x83 => { println!("ADD A,E : add_a_e() not implemented! {:#X}", opcode); 42 },
      0x84 => { println!("ADD A,H : add_a_h() not implemented! {:#X}", opcode); 42 },
      0x85 => { println!("ADD A,L : add_a_l() not implemented! {:#X}", opcode); 42 },
      0x86 => { println!("ADD A,(HL) : add_a_hl() not implemented! {:#X}", opcode); 42 },
      0x87 => { println!("ADD A,A : add_a_a() not implemented! {:#X}", opcode); 42 },
      0x88 => { println!("ADC A,B : adc_a_b() not implemented! {:#X}", opcode); 42 },
      0x89 => { println!("ADC A,C"); self.adc_a_c(); 4 },
      0x8A => { println!("ADC A,D : adc_a_d() not implemented! {:#X}", opcode); 42 },
      0x8B => { println!("ADC A,E : adc_a_e() not implemented! {:#X}", opcode); 42 },
      0x8C => { println!("ADC A,H : adc_a_h() not implemented! {:#X}", opcode); 42 },
      0x8D => { println!("ADC A,L : adc_a_l() not implemented! {:#X}", opcode); 42 },
      0x8E => { println!("ADC A,(HL) : adc_a_hl() not implemented! {:#X}", opcode); 42 },
      0x8F => { println!("ADC A,A : adc_a_a() not implemented! {:#X}", opcode); 42 },
      0x90 => { println!("SUB B : sub_b() not implemented! {:#X}", opcode); 42 },
      0x91 => { println!("SUB C : sub_c() not implemented! {:#X}", opcode); 42 },
      0x92 => { println!("SUB D : sub_d() not implemented! {:#X}", opcode); 42 },
      0x93 => { println!("SUB E : sub_e() not implemented! {:#X}", opcode); 42 },
      0x94 => { println!("SUB H : sub_h() not implemented! {:#X}", opcode); 42 },
      0x95 => { println!("SUB L : sub_l() not implemented! {:#X}", opcode); 42 },
      0x96 => { println!("SUB (HL) : sub_hl() not implemented! {:#X}", opcode); 42 },
      0x97 => { println!("SUB A : sub_a() not implemented! {:#X}", opcode); 42 },
      0x98 => { println!("SBC B : sbc_b() not implemented! {:#X}", opcode); 42 },
      0x99 => { println!("SBC C : sbc_c() not implemented! {:#X}", opcode); 42 },
      0x9A => { println!("SBC D : sbc_d() not implemented! {:#X}", opcode); 42 },
      0x9B => { println!("SBC E : sbc_e() not implemented! {:#X}", opcode); 42 },
      0x9C => { println!("SBC H : sbc_h() not implemented! {:#X}", opcode); 42 },
      0x9D => { println!("SBC L : sbc_l() not implemented! {:#X}", opcode); 42 },
      0x9E => { println!("SBC (HL) : sbc_hl() not implemented! {:#X}", opcode); 42 },
      0x9F => { println!("SBC A : sbc_a() not implemented! {:#X}", opcode); 42 },
      0xA0 => { println!("AND B : and_b() not implemented! {:#X}", opcode); 42 },
      0xA1 => { println!("AND C : and_c() not implemented! {:#X}", opcode); 42 },
      0xA2 => { println!("AND D : and_d() not implemented! {:#X}", opcode); 42 },
      0xA3 => { println!("AND E : and_e() not implemented! {:#X}", opcode); 42 },
      0xA4 => { println!("AND H : and_h() not implemented! {:#X}", opcode); 42 },
      0xA5 => { println!("AND L : and_l() not implemented! {:#X}", opcode); 42 },
      0xA6 => { println!("AND (HL) : and_hl() not implemented! {:#X}", opcode); 42 },
      0xA7 => { println!("AND A : and_a() not implemented! {:#X}", opcode); 42 },
      0xA8 => { println!("XOR B"); self.xor_b(); 4 },
      0xA9 => { println!("XOR C : xor_c() not implemented! {:#X}", opcode); 42 },
      0xAA => { println!("XOR D : xor_d() not implemented! {:#X}", opcode); 42 },
      0xAB => { println!("XOR E : xor_e() not implemented! {:#X}", opcode); 42 },
      0xAC => { println!("XOR H : xor_h() not implemented! {:#X}", opcode); 42 },
      0xAD => { println!("XOR L : xor_l() not implemented! {:#X}", opcode); 42 },
      0xAE => { println!("XOR (HL) : xor_hl() not implemented! {:#X}", opcode); 42 },
      0xAF => { println!("XOR A"); self.xor_a(); 4 },
      0xB0 => { println!("OR B : or_b() not implemented! {:#X}", opcode); 42 },
      0xB1 => { println!("OR C : or_c() not implemented! {:#X}", opcode); 42 },
      0xB2 => { println!("OR D : or_d() not implemented! {:#X}", opcode); 42 },
      0xB3 => { println!("OR E : or_e() not implemented! {:#X}", opcode); 42 },
      0xB4 => { println!("OR H : or_h() not implemented! {:#X}", opcode); 42 },
      0xB5 => { println!("OR L : or_l() not implemented! {:#X}", opcode); 42 },
      0xB6 => { println!("OR (HL) : or_hl() not implemented! {:#X}", opcode); 42 },
      0xB7 => { println!("OR A : or_a() not implemented! {:#X}", opcode); 42 },
      0xB8 => { println!("CP B : cp_b() not implemented! {:#X}", opcode); 42 },
      0xB9 => { println!("CP C : cp_c() not implemented! {:#X}", opcode); 42 },
      0xBA => { println!("CP D : cp_d() not implemented! {:#X}", opcode); 42 },
      0xBB => { println!("CP E : cp_e() not implemented! {:#X}", opcode); 42 },
      0xBC => { println!("CP H : cp_h() not implemented! {:#X}", opcode); 42 },
      0xBD => { println!("CP L : cp_l() not implemented! {:#X}", opcode); 42 },
      0xBE => { println!("CP (HL) : cp_hl() not implemented! {:#X}", opcode); 42 },
      0xBF => { println!("CP A : cp_a() not implemented! {:#X}", opcode); 42 },
      0xC0 => { println!("RET NZ : ret_nz() not implemented! {:#X}", opcode); 42 },
      0xC1 => { println!("POP BC : pop_bc() not implemented! {:#X}", opcode); 42 },
      0xC2 => { println!("JP NZ,nn : jp_nz_nn() not implemented! {:#X}", opcode); 42 },
      0xC3 => { println!("JP nn"); self.jp_nn(mmu); 12 },
      0xC4 => { println!("CALL NZ,nn : call_nz_nn() not implemented! {:#X}", opcode); 42 },
      0xC5 => { println!("PUSH BC : push_bc() not implemented! {:#X}", opcode); 42 },
      0xC6 => { println!("ADD A,n : add_a_n() not implemented! {:#X}", opcode); 42 },
      0xC7 => { println!("RST 00H : rst_00h() not implemented! {:#X}", opcode); 42 },
      0xC8 => { println!("RET Z : ret_z() not implemented! {:#X}", opcode); 42 },
      0xC9 => { println!("RET : ret() not implemented! {:#X}", opcode); 42 },
      0xCA => { println!("JP Z,nn : jp_z_nn() not implemented! {:#X}", opcode); 42 },
      0xCB => { println!("CB prefixed instruction : cb_prefixed_instruction() not implemented! {:#X}", opcode); 42 },
      0xCC => { println!("CALL Z,nn : call_z_nn() not implemented! {:#X}", opcode); 42 },
      0xCD => { println!("CALL nn : call_nn() not implemented! {:#X}", opcode); 42 },
      0xCE => { println!("ADC A,n : adc_a_n() not implemented! {:#X}", opcode); 42 },
      0xCF => { println!("RST 08H : rst_08h() not implemented! {:#X}", opcode); 42 },
      0xD0 => { println!("RET NC : ret_nc() not implemented! {:#X}", opcode); 42 },
      0xD1 => { println!("POP DE : pop_de() not implemented! {:#X}", opcode); 42 },
      0xD2 => { println!("JP NC,nn : jp_nc_nn() not implemented! {:#X}", opcode); 42 },
      0xD4 => { println!("CALL NC,nn : call_nc_nn() not implemented! {:#X}", opcode); 42 },
      0xD5 => { println!("PUSH DE : push_de() not implemented! {:#X}", opcode); 42 },
      0xD6 => { println!("SUB n : sub_n() not implemented! {:#X}", opcode); 42 },
      0xD7 => { println!("RST 10H : rst_10h() not implemented! {:#X}", opcode); 42 },
      0xD8 => { println!("RET C : ret_c() not implemented! {:#X}", opcode); 42 },
      0xD9 => { println!("RETI : reti() not implemented! {:#X}", opcode); 42 },
      0xDA => { println!("JP C,nn : jp_c_nn() not implemented! {:#X}", opcode); 42 },
      0xDC => { println!("CALL C,nn : call_c_nn() not implemented! {:#X}", opcode); 42 },
      0xDE => { println!("SBC n : sbc_n() not implemented! {:#X}", opcode); 42 },
      0xDF => { println!("RST 18H : rst_18h() not implemented! {:#X}", opcode); 42 },
      0xE0 => { println!("LD (0xFF00+n),A : ld_0xff00_plus_n_a() not implemented! {:#X}", opcode); 42 },
      0xE1 => { println!("POP HL : pop_hl() not implemented! {:#X}", opcode); 42 },
      0xE2 => { println!("LD (0xFF00+C),A : ld_0xff00_plus_c_a() not implemented! {:#X}", opcode); 42 },
      0xE5 => { println!("PUSH HL : push_hl() not implemented! {:#X}", opcode); 42 },
      0xE6 => { println!("AND n : and_n() not implemented! {:#X}", opcode); 42 },
      0xE7 => { println!("RST 20H : rst_20h() not implemented! {:#X}", opcode); 42 },
      0xE8 => { println!("ADD SP,n : add_sp_n() not implemented! {:#X}", opcode); 42 },
      0xE9 => { println!("JP (HL) : jp_hl() not implemented! {:#X}", opcode); 42 },
      0xEA => { println!("LD (nn),A : ld_nn_a() not implemented! {:#X}", opcode); 42 },
      0xEE => { println!("XOR n : xor_n() not implemented! {:#X}", opcode); 42 },
      0xEF => { println!("RST 28H : rst_28h() not implemented! {:#X}", opcode); 42 },
      0xF0 => { println!("LD A,(0xFF00+n) : ld_a_0xff00_plus_n() not implemented! {:#X}", opcode); 42 },
      0xF1 => { println!("POP AF : pop_af() not implemented! {:#X}", opcode); 42 },
      0xF2 => { println!("LD A,(C) : ld_a_c() not implemented! {:#X}", opcode); 42 },
      0xF3 => { println!("DI : di() not implemented! {:#X}", opcode); 42 },
      0xF5 => { println!("PUSH AF : push_af() not implemented! {:#X}", opcode); 42 },
      0xF6 => { println!("OR n : or_n() not implemented! {:#X}", opcode); 42 },
      0xF7 => { println!("RST 30H : rst_30h() not implemented! {:#X}", opcode); 42 },
      0xF8 => { println!("LD HL,SP+n : ld_hl_sp_plus_n() not implemented! {:#X}", opcode); 42 },
      0xF9 => { println!("LD SP,HL : ld_sp_hl() not implemented! {:#X}", opcode); 42 },
      0xFA => { println!("LD A,(nn) : ld_a_nn() not implemented! {:#X}", opcode); 42 },
      0xFB => { println!("EI : ei() not implemented! {:#X}", opcode); 42 },
      0xFE => { println!("CP n : cp_n() not implemented! {:#X}", opcode); 42 },
      0xFF => { println!("RST 38H : rst_38h() not implemented! {:#X}", opcode); 42 },
      _ => panic!("Unexpected opcode: {:#X}", opcode)
    }
    // Ok(cycles)
  }

  // Opcodes

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
    shared_ld_n_n(self, RegEnum::C, mmu);
  }

  fn ld_b_n(&mut self, mmu: &mmu::MMU) {
    shared_ld_n_n(self, RegEnum::B, mmu);
  }

  fn ld_hld_a(&mut self) {
    let value = self.read_byte_reg(RegEnum::A);
    self.write_word_reg(RegEnum::HL, (value as types::Word).wrapping_sub(1));
  }

  fn dec_b(&mut self) {
    shared_dec_n(self, RegEnum::B);
  }

  fn jr_nz_n(&mut self, mmu: &mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      println!("self.PC [{:#X}] + 1 + mmu.read_word(self.PC) [{:#X}] as types::SignedByte) [{:#X}]", self.PC, mmu.read_word(self.PC), mmu.read_word(self.PC) as types::SignedByte);

      self.PC = self.PC + 1 + mmu.read(self.PC) as types::Word;
      self.BranchTaken = true;
    } else {
      self.PC += 1;
    }
  }

  fn rra(&mut self) {
    shared_rotate_rr(self, RegEnum::A)
  }

  fn ld_a_d(&mut self) {
    let value = self.read_byte_reg(RegEnum::D);
    self.write_byte_reg(RegEnum::A, value)
  }

  fn adc_a_c(&mut self) {
    let value = self.read_byte_reg(RegEnum::C);
    shared_adc(self, value);
    // OPCodes_ADC(BC.GetLow());

    panic!("Not implemented!")
  }

  // Helpers

  fn util_toggle_zero_flag_from_result(&mut self, result: types::Byte) {
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

fn shared_ld_n_n(cpu: &mut CPU, regEnum: RegEnum, mmu: &mmu::MMU) {
  let value = mmu.read(cpu.PC);
  cpu.write_byte_reg(regEnum, value);
  cpu.PC += 1;
}

fn shared_dec_n(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_byte_reg(regEnum).wrapping_sub(1);

  cpu.write_byte_reg(regEnum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_set_flag(FLAG_SUB);

  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x0F {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
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
    _ => {}
  }
}

fn shared_adc(cpu: &mut CPU, byte: types::Byte) {
  // int carry = IsSetFlag(FLAG_CARRY) ? 1 : 0;
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
// 0x00 => { println!("RLC B : rlc_b() not implemented! {:#X}", opcode); 42 },
// 0x01 => { println!("RLC C : rlc_c() not implemented! {:#X}", opcode); 42 },
// 0x02 => { println!("RLC D : rlc_d() not implemented! {:#X}", opcode); 42 },
// 0x03 => { println!("RLC E : rlc_e() not implemented! {:#X}", opcode); 42 },
// 0x04 => { println!("RLC H : rlc_h() not implemented! {:#X}", opcode); 42 },
// 0x05 => { println!("RLC L : rlc_l() not implemented! {:#X}", opcode); 42 },
// 0x06 => { println!("RLC (HL) : rlc_hl() not implemented! {:#X}", opcode); 42 },
// 0x07 => { println!("RLC A : rlc_a() not implemented! {:#X}", opcode); 42 },
// 0x08 => { println!("RRC B : rrc_b() not implemented! {:#X}", opcode); 42 },
// 0x09 => { println!("RRC C : rrc_c() not implemented! {:#X}", opcode); 42 },
// 0x0A => { println!("RRC D : rrc_d() not implemented! {:#X}", opcode); 42 },
// 0x0B => { println!("RRC E : rrc_e() not implemented! {:#X}", opcode); 42 },
// 0x0C => { println!("RRC H : rrc_h() not implemented! {:#X}", opcode); 42 },
// 0x0D => { println!("RRC L : rrc_l() not implemented! {:#X}", opcode); 42 },
// 0x0E => { println!("RRC (HL) : rrc_hl() not implemented! {:#X}", opcode); 42 },
// 0x0F => { println!("RRC A : rrc_a() not implemented! {:#X}", opcode); 42 },
// 0x10 => { println!("RL B : rl_b() not implemented! {:#X}", opcode); 42 },
// 0x11 => { println!("RL C : rl_c() not implemented! {:#X}", opcode); 42 },
// 0x12 => { println!("RL D : rl_d() not implemented! {:#X}", opcode); 42 },
// 0x13 => { println!("RL E : rl_e() not implemented! {:#X}", opcode); 42 },
// 0x14 => { println!("RL H : rl_h() not implemented! {:#X}", opcode); 42 },
// 0x15 => { println!("RL L : rl_l() not implemented! {:#X}", opcode); 42 },
// 0x16 => { println!("RL (HL) : rl_hl() not implemented! {:#X}", opcode); 42 },
// 0x17 => { println!("RL A : rl_a() not implemented! {:#X}", opcode); 42 },
// 0x18 => { println!("RR B : rr_b() not implemented! {:#X}", opcode); 42 },
// 0x19 => { println!("RR C : rr_c() not implemented! {:#X}", opcode); 42 },
// 0x1A => { println!("RR D : rr_d() not implemented! {:#X}", opcode); 42 },
// 0x1B => { println!("RR E : rr_e() not implemented! {:#X}", opcode); 42 },
// 0x1C => { println!("RR H : rr_h() not implemented! {:#X}", opcode); 42 },
// 0x1D => { println!("RR L : rr_l() not implemented! {:#X}", opcode); 42 },
// 0x1E => { println!("RR (HL) : rr_hl() not implemented! {:#X}", opcode); 42 },
// 0x1F => { println!("RR A : rr_a() not implemented! {:#X}", opcode); 42 },
// 0x20 => { println!("SLA B : sla_b() not implemented! {:#X}", opcode); 42 },
// 0x21 => { println!("SLA C : sla_c() not implemented! {:#X}", opcode); 42 },
// 0x22 => { println!("SLA D : sla_d() not implemented! {:#X}", opcode); 42 },
// 0x23 => { println!("SLA E : sla_e() not implemented! {:#X}", opcode); 42 },
// 0x24 => { println!("SLA H : sla_h() not implemented! {:#X}", opcode); 42 },
// 0x25 => { println!("SLA L : sla_l() not implemented! {:#X}", opcode); 42 },
// 0x26 => { println!("SLA (HL) : sla_hl() not implemented! {:#X}", opcode); 42 },
// 0x27 => { println!("SLA A : sla_a() not implemented! {:#X}", opcode); 42 },
// 0x28 => { println!("SRA B : sra_b() not implemented! {:#X}", opcode); 42 },
// 0x29 => { println!("SRA C : sra_c() not implemented! {:#X}", opcode); 42 },
// 0x2A => { println!("SRA D : sra_d() not implemented! {:#X}", opcode); 42 },
// 0x2B => { println!("SRA E : sra_e() not implemented! {:#X}", opcode); 42 },
// 0x2C => { println!("SRA H : sra_h() not implemented! {:#X}", opcode); 42 },
// 0x2D => { println!("SRA L : sra_l() not implemented! {:#X}", opcode); 42 },
// 0x2E => { println!("SRA (HL) : sra_hl() not implemented! {:#X}", opcode); 42 },
// 0x2F => { println!("SRA A : sra_a() not implemented! {:#X}", opcode); 42 },
// 0x30 => { println!("SWAP B : swap_b() not implemented! {:#X}", opcode); 42 },
// 0x31 => { println!("SWAP C : swap_c() not implemented! {:#X}", opcode); 42 },
// 0x32 => { println!("SWAP D : swap_d() not implemented! {:#X}", opcode); 42 },
// 0x33 => { println!("SWAP E : swap_e() not implemented! {:#X}", opcode); 42 },
// 0x34 => { println!("SWAP H : swap_h() not implemented! {:#X}", opcode); 42 },
// 0x35 => { println!("SWAP L : swap_l() not implemented! {:#X}", opcode); 42 },
// 0x36 => { println!("SWAP (HL) : swap_hl() not implemented! {:#X}", opcode); 42 },
// 0x37 => { println!("SWAP A : swap_a() not implemented! {:#X}", opcode); 42 },
// 0x38 => { println!("SRL B : srl_b() not implemented! {:#X}", opcode); 42 },
// 0x39 => { println!("SRL C : srl_c() not implemented! {:#X}", opcode); 42 },
// 0x3A => { println!("SRL D : srl_d() not implemented! {:#X}", opcode); 42 },
// 0x3B => { println!("SRL E : srl_e() not implemented! {:#X}", opcode); 42 },
// 0x3C => { println!("SRL H : srl_h() not implemented! {:#X}", opcode); 42 },
// 0x3D => { println!("SRL L : srl_l() not implemented! {:#X}", opcode); 42 },
// 0x3E => { println!("SRL (HL) : srl_hl() not implemented! {:#X}", opcode); 42 },
// 0x3F => { println!("SRL A : srl_a() not implemented! {:#X}", opcode); 42 },
// 0x40 => { println!("BIT 0 B : bit_0_b() not implemented! {:#X}", opcode); 42 },
// 0x41 => { println!("BIT 0 C : bit_0_c() not implemented! {:#X}", opcode); 42 },
// 0x42 => { println!("BIT 0 D : bit_0_d() not implemented! {:#X}", opcode); 42 },
// 0x43 => { println!("BIT 0 E : bit_0_e() not implemented! {:#X}", opcode); 42 },
// 0x44 => { println!("BIT 0 H : bit_0_h() not implemented! {:#X}", opcode); 42 },
// 0x45 => { println!("BIT 0 L : bit_0_l() not implemented! {:#X}", opcode); 42 },
// 0x46 => { println!("BIT 0 (HL) : bit_0_hl() not implemented! {:#X}", opcode); 42 },
// 0x47 => { println!("BIT 0 A : bit_0_a() not implemented! {:#X}", opcode); 42 },
// 0x48 => { println!("BIT 1 B : bit_1_b() not implemented! {:#X}", opcode); 42 },
// 0x49 => { println!("BIT 1 C : bit_1_c() not implemented! {:#X}", opcode); 42 },
// 0x4A => { println!("BIT 1 D : bit_1_d() not implemented! {:#X}", opcode); 42 },
// 0x4B => { println!("BIT 1 E : bit_1_e() not implemented! {:#X}", opcode); 42 },
// 0x4C => { println!("BIT 1 H : bit_1_h() not implemented! {:#X}", opcode); 42 },
// 0x4D => { println!("BIT 1 L : bit_1_l() not implemented! {:#X}", opcode); 42 },
// 0x4E => { println!("BIT 1 (HL) : bit_1_hl() not implemented! {:#X}", opcode); 42 },
// 0x4F => { println!("BIT 1 A : bit_1_a() not implemented! {:#X}", opcode); 42 },
// 0x50 => { println!("BIT 2 B : bit_2_b() not implemented! {:#X}", opcode); 42 },
// 0x51 => { println!("BIT 2 C : bit_2_c() not implemented! {:#X}", opcode); 42 },
// 0x52 => { println!("BIT 2 D : bit_2_d() not implemented! {:#X}", opcode); 42 },
// 0x53 => { println!("BIT 2 E : bit_2_e() not implemented! {:#X}", opcode); 42 },
// 0x54 => { println!("BIT 2 H : bit_2_h() not implemented! {:#X}", opcode); 42 },
// 0x55 => { println!("BIT 2 L : bit_2_l() not implemented! {:#X}", opcode); 42 },
// 0x56 => { println!("BIT 2 (HL) : bit_2_hl() not implemented! {:#X}", opcode); 42 },
// 0x57 => { println!("BIT 2 A : bit_2_a() not implemented! {:#X}", opcode); 42 },
// 0x58 => { println!("BIT 3 B : bit_3_b() not implemented! {:#X}", opcode); 42 },
// 0x59 => { println!("BIT 3 C : bit_3_c() not implemented! {:#X}", opcode); 42 },
// 0x5A => { println!("BIT 3 D : bit_3_d() not implemented! {:#X}", opcode); 42 },
// 0x5B => { println!("BIT 3 E : bit_3_e() not implemented! {:#X}", opcode); 42 },
// 0x5C => { println!("BIT 3 H : bit_3_h() not implemented! {:#X}", opcode); 42 },
// 0x5D => { println!("BIT 3 L : bit_3_l() not implemented! {:#X}", opcode); 42 },
// 0x5E => { println!("BIT 3 (HL) : bit_3_hl() not implemented! {:#X}", opcode); 42 },
// 0x5F => { println!("BIT 3 A : bit_3_a() not implemented! {:#X}", opcode); 42 },
// 0x60 => { println!("BIT 4 B : bit_4_b() not implemented! {:#X}", opcode); 42 },
// 0x61 => { println!("BIT 4 C : bit_4_c() not implemented! {:#X}", opcode); 42 },
// 0x62 => { println!("BIT 4 D : bit_4_d() not implemented! {:#X}", opcode); 42 },
// 0x63 => { println!("BIT 4 E : bit_4_e() not implemented! {:#X}", opcode); 42 },
// 0x64 => { println!("BIT 4 H : bit_4_h() not implemented! {:#X}", opcode); 42 },
// 0x65 => { println!("BIT 4 L : bit_4_l() not implemented! {:#X}", opcode); 42 },
// 0x66 => { println!("BIT 4 (HL) : bit_4_hl() not implemented! {:#X}", opcode); 42 },
// 0x67 => { println!("BIT 4 A : bit_4_a() not implemented! {:#X}", opcode); 42 },
// 0x68 => { println!("BIT 5 B : bit_5_b() not implemented! {:#X}", opcode); 42 },
// 0x69 => { println!("BIT 5 C : bit_5_c() not implemented! {:#X}", opcode); 42 },
// 0x6A => { println!("BIT 5 D : bit_5_d() not implemented! {:#X}", opcode); 42 },
// 0x6B => { println!("BIT 5 E : bit_5_e() not implemented! {:#X}", opcode); 42 },
// 0x6C => { println!("BIT 5 H : bit_5_h() not implemented! {:#X}", opcode); 42 },
// 0x6D => { println!("BIT 5 L : bit_5_l() not implemented! {:#X}", opcode); 42 },
// 0x6E => { println!("BIT 5 (HL) : bit_5_hl() not implemented! {:#X}", opcode); 42 },
// 0x6F => { println!("BIT 5 A : bit_5_a() not implemented! {:#X}", opcode); 42 },
// 0x70 => { println!("BIT 6 B : bit_6_b() not implemented! {:#X}", opcode); 42 },
// 0x71 => { println!("BIT 6 C : bit_6_c() not implemented! {:#X}", opcode); 42 },
// 0x72 => { println!("BIT 6 D : bit_6_d() not implemented! {:#X}", opcode); 42 },
// 0x73 => { println!("BIT 6 E : bit_6_e() not implemented! {:#X}", opcode); 42 },
// 0x74 => { println!("BIT 6 H : bit_6_h() not implemented! {:#X}", opcode); 42 },
// 0x75 => { println!("BIT 6 L : bit_6_l() not implemented! {:#X}", opcode); 42 },
// 0x76 => { println!("BIT 6 (HL) : bit_6_hl() not implemented! {:#X}", opcode); 42 },
// 0x77 => { println!("BIT 6 A : bit_6_a() not implemented! {:#X}", opcode); 42 },
// 0x78 => { println!("BIT 7 B : bit_7_b() not implemented! {:#X}", opcode); 42 },
// 0x79 => { println!("BIT 7 C : bit_7_c() not implemented! {:#X}", opcode); 42 },
// 0x7A => { println!("BIT 7 D : bit_7_d() not implemented! {:#X}", opcode); 42 },
// 0x7B => { println!("BIT 7 E : bit_7_e() not implemented! {:#X}", opcode); 42 },
// 0x7C => { println!("BIT 7 H : bit_7_h() not implemented! {:#X}", opcode); 42 },
// 0x7D => { println!("BIT 7 L : bit_7_l() not implemented! {:#X}", opcode); 42 },
// 0x7E => { println!("BIT 7 (HL) : bit_7_hl() not implemented! {:#X}", opcode); 42 },
// 0x7F => { println!("BIT 7 A : bit_7_a() not implemented! {:#X}", opcode); 42 },
// 0x80 => { println!("RES 0 B : res_0_b() not implemented! {:#X}", opcode); 42 },
// 0x81 => { println!("RES 0 C : res_0_c() not implemented! {:#X}", opcode); 42 },
// 0x82 => { println!("RES 0 D : res_0_d() not implemented! {:#X}", opcode); 42 },
// 0x83 => { println!("RES 0 E : res_0_e() not implemented! {:#X}", opcode); 42 },
// 0x84 => { println!("RES 0 H : res_0_h() not implemented! {:#X}", opcode); 42 },
// 0x85 => { println!("RES 0 L : res_0_l() not implemented! {:#X}", opcode); 42 },
// 0x86 => { println!("RES 0 (HL) : res_0_hl() not implemented! {:#X}", opcode); 42 },
// 0x87 => { println!("RES 0 A : res_0_a() not implemented! {:#X}", opcode); 42 },
// 0x88 => { println!("RES 1 B : res_1_b() not implemented! {:#X}", opcode); 42 },
// 0x89 => { println!("RES 1 C : res_1_c() not implemented! {:#X}", opcode); 42 },
// 0x8A => { println!("RES 1 D : res_1_d() not implemented! {:#X}", opcode); 42 },
// 0x8B => { println!("RES 1 E : res_1_e() not implemented! {:#X}", opcode); 42 },
// 0x8C => { println!("RES 1 H : res_1_h() not implemented! {:#X}", opcode); 42 },
// 0x8D => { println!("RES 1 L : res_1_l() not implemented! {:#X}", opcode); 42 },
// 0x8E => { println!("RES 1 (HL) : res_1_hl() not implemented! {:#X}", opcode); 42 },
// 0x8F => { println!("RES 1 A : res_1_a() not implemented! {:#X}", opcode); 42 },
// 0x90 => { println!("RES 2 B : res_2_b() not implemented! {:#X}", opcode); 42 },
// 0x91 => { println!("RES 2 C : res_2_c() not implemented! {:#X}", opcode); 42 },
// 0x92 => { println!("RES 2 D : res_2_d() not implemented! {:#X}", opcode); 42 },
// 0x93 => { println!("RES 2 E : res_2_e() not implemented! {:#X}", opcode); 42 },
// 0x94 => { println!("RES 2 H : res_2_h() not implemented! {:#X}", opcode); 42 },
// 0x95 => { println!("RES 2 L : res_2_l() not implemented! {:#X}", opcode); 42 },
// 0x96 => { println!("RES 2 (HL) : res_2_hl() not implemented! {:#X}", opcode); 42 },
// 0x97 => { println!("RES 2 A : res_2_a() not implemented! {:#X}", opcode); 42 },
// 0x98 => { println!("RES 3 B : res_3_b() not implemented! {:#X}", opcode); 42 },
// 0x99 => { println!("RES 3 C : res_3_c() not implemented! {:#X}", opcode); 42 },
// 0x9A => { println!("RES 3 D : res_3_d() not implemented! {:#X}", opcode); 42 },
// 0x9B => { println!("RES 3 E : res_3_e() not implemented! {:#X}", opcode); 42 },
// 0x9C => { println!("RES 3 H : res_3_h() not implemented! {:#X}", opcode); 42 },
// 0x9D => { println!("RES 3 L : res_3_l() not implemented! {:#X}", opcode); 42 },
// 0x9E => { println!("RES 3 (HL) : res_3_hl() not implemented! {:#X}", opcode); 42 },
// 0x9F => { println!("RES 3 A : res_3_a() not implemented! {:#X}", opcode); 42 },
// 0xA0 => { println!("RES 4 B : res_4_b() not implemented! {:#X}", opcode); 42 },
// 0xA1 => { println!("RES 4 C : res_4_c() not implemented! {:#X}", opcode); 42 },
// 0xA2 => { println!("RES 4 D : res_4_d() not implemented! {:#X}", opcode); 42 },
// 0xA3 => { println!("RES 4 E : res_4_e() not implemented! {:#X}", opcode); 42 },
// 0xA4 => { println!("RES 4 H : res_4_h() not implemented! {:#X}", opcode); 42 },
// 0xA5 => { println!("RES 4 L : res_4_l() not implemented! {:#X}", opcode); 42 },
// 0xA6 => { println!("RES 4 (HL) : res_4_hl() not implemented! {:#X}", opcode); 42 },
// 0xA7 => { println!("RES 4 A : res_4_a() not implemented! {:#X}", opcode); 42 },
// 0xA8 => { println!("RES 5 B : res_5_b() not implemented! {:#X}", opcode); 42 },
// 0xA9 => { println!("RES 5 C : res_5_c() not implemented! {:#X}", opcode); 42 },
// 0xAA => { println!("RES 5 D : res_5_d() not implemented! {:#X}", opcode); 42 },
// 0xAB => { println!("RES 5 E : res_5_e() not implemented! {:#X}", opcode); 42 },
// 0xAC => { println!("RES 5 H : res_5_h() not implemented! {:#X}", opcode); 42 },
// 0xAD => { println!("RES 5 L : res_5_l() not implemented! {:#X}", opcode); 42 },
// 0xAE => { println!("RES 5 (HL) : res_5_hl() not implemented! {:#X}", opcode); 42 },
// 0xAF => { println!("RES 5 A : res_5_a() not implemented! {:#X}", opcode); 42 },
// 0xB0 => { println!("RES 6 B : res_6_b() not implemented! {:#X}", opcode); 42 },
// 0xB1 => { println!("RES 6 C : res_6_c() not implemented! {:#X}", opcode); 42 },
// 0xB2 => { println!("RES 6 D : res_6_d() not implemented! {:#X}", opcode); 42 },
// 0xB3 => { println!("RES 6 E : res_6_e() not implemented! {:#X}", opcode); 42 },
// 0xB4 => { println!("RES 6 H : res_6_h() not implemented! {:#X}", opcode); 42 },
// 0xB5 => { println!("RES 6 L : res_6_l() not implemented! {:#X}", opcode); 42 },
// 0xB6 => { println!("RES 6 (HL) : res_6_hl() not implemented! {:#X}", opcode); 42 },
// 0xB7 => { println!("RES 6 A : res_6_a() not implemented! {:#X}", opcode); 42 },
// 0xB8 => { println!("RES 7 B : res_7_b() not implemented! {:#X}", opcode); 42 },
// 0xB9 => { println!("RES 7 C : res_7_c() not implemented! {:#X}", opcode); 42 },
// 0xBA => { println!("RES 7 D : res_7_d() not implemented! {:#X}", opcode); 42 },
// 0xBB => { println!("RES 7 E : res_7_e() not implemented! {:#X}", opcode); 42 },
// 0xBC => { println!("RES 7 H : res_7_h() not implemented! {:#X}", opcode); 42 },
// 0xBD => { println!("RES 7 L : res_7_l() not implemented! {:#X}", opcode); 42 },
// 0xBE => { println!("RES 7 (HL) : res_7_hl() not implemented! {:#X}", opcode); 42 },
// 0xBF => { println!("RES 7 A : res_7_a() not implemented! {:#X}", opcode); 42 },
// 0xC0 => { println!("SET 0 B : set_0_b() not implemented! {:#X}", opcode); 42 },
// 0xC1 => { println!("SET 0 C : set_0_c() not implemented! {:#X}", opcode); 42 },
// 0xC2 => { println!("SET 0 D : set_0_d() not implemented! {:#X}", opcode); 42 },
// 0xC3 => { println!("SET 0 E : set_0_e() not implemented! {:#X}", opcode); 42 },
// 0xC4 => { println!("SET 0 H : set_0_h() not implemented! {:#X}", opcode); 42 },
// 0xC5 => { println!("SET 0 L : set_0_l() not implemented! {:#X}", opcode); 42 },
// 0xC6 => { println!("SET 0 (HL) : set_0_hl() not implemented! {:#X}", opcode); 42 },
// 0xC7 => { println!("SET 0 A : set_0_a() not implemented! {:#X}", opcode); 42 },
// 0xC8 => { println!("SET 1 B : set_1_b() not implemented! {:#X}", opcode); 42 },
// 0xC9 => { println!("SET 1 C : set_1_c() not implemented! {:#X}", opcode); 42 },
// 0xCA => { println!("SET 1 D : set_1_d() not implemented! {:#X}", opcode); 42 },
// 0xCB => { println!("SET 1 E : set_1_e() not implemented! {:#X}", opcode); 42 },
// 0xCC => { println!("SET 1 H : set_1_h() not implemented! {:#X}", opcode); 42 },
// 0xCD => { println!("SET 1 L : set_1_l() not implemented! {:#X}", opcode); 42 },
// 0xCE => { println!("SET 1 (HL) : set_1_hl() not implemented! {:#X}", opcode); 42 },
// 0xCF => { println!("SET 1 A : set_1_a() not implemented! {:#X}", opcode); 42 },
// 0xD0 => { println!("SET 2 B : set_2_b() not implemented! {:#X}", opcode); 42 },
// 0xD1 => { println!("SET 2 C : set_2_c() not implemented! {:#X}", opcode); 42 },
// 0xD2 => { println!("SET 2 D : set_2_d() not implemented! {:#X}", opcode); 42 },
// 0xD3 => { println!("SET 2 E : set_2_e() not implemented! {:#X}", opcode); 42 },
// 0xD4 => { println!("SET 2 H : set_2_h() not implemented! {:#X}", opcode); 42 },
// 0xD5 => { println!("SET 2 L : set_2_l() not implemented! {:#X}", opcode); 42 },
// 0xD6 => { println!("SET 2 (HL) : set_2_hl() not implemented! {:#X}", opcode); 42 },
// 0xD7 => { println!("SET 2 A : set_2_a() not implemented! {:#X}", opcode); 42 },
// 0xD8 => { println!("SET 3 B : set_3_b() not implemented! {:#X}", opcode); 42 },
// 0xD9 => { println!("SET 3 C : set_3_c() not implemented! {:#X}", opcode); 42 },
// 0xDA => { println!("SET 3 D : set_3_d() not implemented! {:#X}", opcode); 42 },
// 0xDB => { println!("SET 3 E : set_3_e() not implemented! {:#X}", opcode); 42 },
// 0xDC => { println!("SET 3 H : set_3_h() not implemented! {:#X}", opcode); 42 },
// 0xDD => { println!("SET 3 L : set_3_l() not implemented! {:#X}", opcode); 42 },
// 0xDE => { println!("SET 3 (HL) : set_3_hl() not implemented! {:#X}", opcode); 42 },
// 0xDF => { println!("SET 3 A : set_3_a() not implemented! {:#X}", opcode); 42 },
// 0xE0 => { println!("SET 4 B : set_4_b() not implemented! {:#X}", opcode); 42 },
// 0xE1 => { println!("SET 4 C : set_4_c() not implemented! {:#X}", opcode); 42 },
// 0xE2 => { println!("SET 4 D : set_4_d() not implemented! {:#X}", opcode); 42 },
// 0xE3 => { println!("SET 4 E : set_4_e() not implemented! {:#X}", opcode); 42 },
// 0xE4 => { println!("SET 4 H : set_4_h() not implemented! {:#X}", opcode); 42 },
// 0xE5 => { println!("SET 4 L : set_4_l() not implemented! {:#X}", opcode); 42 },
// 0xE6 => { println!("SET 4 (HL) : set_4_hl() not implemented! {:#X}", opcode); 42 },
// 0xE7 => { println!("SET 4 A : set_4_a() not implemented! {:#X}", opcode); 42 },
// 0xE8 => { println!("SET 5 B : set_5_b() not implemented! {:#X}", opcode); 42 },
// 0xE9 => { println!("SET 5 C : set_5_c() not implemented! {:#X}", opcode); 42 },
// 0xEA => { println!("SET 5 D : set_5_d() not implemented! {:#X}", opcode); 42 },
// 0xEB => { println!("SET 5 E : set_5_e() not implemented! {:#X}", opcode); 42 },
// 0xEC => { println!("SET 5 H : set_5_h() not implemented! {:#X}", opcode); 42 },
// 0xED => { println!("SET 5 L : set_5_l() not implemented! {:#X}", opcode); 42 },
// 0xEE => { println!("SET 5 (HL) : set_5_hl() not implemented! {:#X}", opcode); 42 },
// 0xEF => { println!("SET 5 A : set_5_a() not implemented! {:#X}", opcode); 42 },
// 0xF0 => { println!("SET 6 B : set_6_b() not implemented! {:#X}", opcode); 42 },
// 0xF1 => { println!("SET 6 C : set_6_c() not implemented! {:#X}", opcode); 42 },
// 0xF2 => { println!("SET 6 D : set_6_d() not implemented! {:#X}", opcode); 42 },
// 0xF3 => { println!("SET 6 E : set_6_e() not implemented! {:#X}", opcode); 42 },
// 0xF4 => { println!("SET 6 H : set_6_h() not implemented! {:#X}", opcode); 42 },
// 0xF5 => { println!("SET 6 L : set_6_l() not implemented! {:#X}", opcode); 42 },
// 0xF6 => { println!("SET 6 (HL) : set_6_hl() not implemented! {:#X}", opcode); 42 },
// 0xF7 => { println!("SET 6 A : set_6_a() not implemented! {:#X}", opcode); 42 },
// 0xF8 => { println!("SET 7 B : set_7_b() not implemented! {:#X}", opcode); 42 },
// 0xF9 => { println!("SET 7 C : set_7_c() not implemented! {:#X}", opcode); 42 },
// 0xFA => { println!("SET 7 D : set_7_d() not implemented! {:#X}", opcode); 42 },
// 0xFB => { println!("SET 7 E : set_7_e() not implemented! {:#X}", opcode); 42 },
// 0xFC => { println!("SET 7 H : set_7_h() not implemented! {:#X}", opcode); 42 },
// 0xFD => { println!("SET 7 L : set_7_l() not implemented! {:#X}", opcode); 42 },
// 0xFE => { println!("SET 7 (HL) : set_7_hl() not implemented! {:#X}", opcode); 42 },
// 0xFF => { println!("SET 7 A : set_7_a() not implemented! {:#X}", opcode); 42 },
