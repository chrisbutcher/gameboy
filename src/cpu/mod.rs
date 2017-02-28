use std::fmt;

pub use super::types;
pub use super::mmu;
pub use super::cartridge;
pub mod opcodes;

const FLAG_Z: u8 = 7; // Zero
const FLAG_N: u8 = 6; // Negative
const FLAG_H: u8 = 5; // Half-carry
const FLAG_C: u8 = 4; // Carry

#[derive(Debug)]
enum RegEnum {
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
    self.value = self.value | v as types::Word
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
}

impl CPU {
  pub fn new() -> CPU {
      CPU {
        AF: Register::new(), BC: Register::new(), DE: Register::new(), HL: Register::new(),
        SP: Register::new(), PC: 0x0000,
      }
  }

  pub fn execute_next_opcode(&mut self, mmu: &mut mmu::MMU) -> i32 {
    let opcode: types::Byte = mmu.read(self.PC);
    self.PC += 1;
    self.execute_opcode(opcode, mmu)
  }

  // https://github.com/CTurt/Cinoop/blob/master/source/cpu.c
  // https://github.com/CTurt/Cinoop/blob/master/include/cpu.h
  // https://github.com/drhelius/Gearboy/blob/master/src/opcodes.cpp

  // ALSO SEE https://github.com/mvdnes/rboy/blob/master/src/cpu.rs (Note that cycles in this code are divided by 4)
  // http://gameboy.mongenel.com/dmg/lesson1.html
  fn execute_opcode(&mut self, opcode: types::Byte, mmu: &mut mmu::MMU) -> i32 {
    match opcode {
      0x00 => { self.nop() }, // NOP
      0x01 => { println!("self.ld_bc_nn() not implemented! {:x}", opcode); 42 }, // LD BC,nn
      0x02 => { println!("self.ld_bc_a() not implemented! {:x}", opcode); 42 }, // LD (BC),A
      0x03 => { println!("self.inc_bc() not implemented! {:x}", opcode); 42 }, // INC BC
      0x04 => { println!("self.inc_b() not implemented! {:x}", opcode); 42 }, // INC B
      0x05 => { println!("self.dec_b() not implemented! {:x}", opcode); 42 }, // DEC B
      0x06 => { println!("self.ld_b_n() not implemented! {:x}", opcode); 42 }, // LD B,n
      0x07 => { println!("self.rlca() not implemented! {:x}", opcode); 42 }, // RLCA
      0x08 => { println!("self.ld_nn_sp() not implemented! {:x}", opcode); 42 }, // LD (nn),SP
      0x09 => { println!("self.add_hl_bc() not implemented! {:x}", opcode); 42 }, // ADD HL,BC
      0x0A => { println!("self.ld_a_bc() not implemented! {:x}", opcode); 42 }, // LD A,(BC)
      0x0B => { println!("self.dec_bc() not implemented! {:x}", opcode); 42 }, // DEC BC
      0x0C => { println!("self.inc_c() not implemented! {:x}", opcode); 42 }, // INC C
      0x0D => { println!("self.dec_c() not implemented! {:x}", opcode); 42 }, // DEC C
      0x0E => { println!("self.ld_c_n() not implemented! {:x}", opcode); 42 }, // LD C,n
      0x0F => { println!("self.rrca() not implemented! {:x}", opcode); 42 }, // RRCA
      0x10 => { println!("self.stop() not implemented! {:x}", opcode); 42 }, // STOP
      0x11 => { println!("self.ld_de_nn() not implemented! {:x}", opcode); 42 }, // LD DE,nn
      0x12 => { println!("self.ld_de_a() not implemented! {:x}", opcode); 42 }, // LD (DE),A
      0x13 => { println!("self.inc_de() not implemented! {:x}", opcode); 42 }, // INC DE
      0x14 => { println!("self.inc_d() not implemented! {:x}", opcode); 42 }, // INC D
      0x15 => { println!("self.dec_d() not implemented! {:x}", opcode); 42 }, // DEC D
      0x16 => { println!("self.ld_d_n() not implemented! {:x}", opcode); 42 }, // LD D,n
      0x17 => { println!("self.rla() not implemented! {:x}", opcode); 42 }, // RLA
      0x18 => { println!("self.jr_n() not implemented! {:x}", opcode); 42 }, // JR n
      0x19 => { println!("self.add_hl_de() not implemented! {:x}", opcode); 42 }, // ADD HL,DE
      0x1A => { println!("self.ld_a_de() not implemented! {:x}", opcode); 42 }, // LD A,(DE)
      0x1B => { println!("self.dec_de() not implemented! {:x}", opcode); 42 }, // DEC DE
      0x1C => { println!("self.inc_e() not implemented! {:x}", opcode); 42 }, // INC E
      0x1D => { println!("self.dec_e() not implemented! {:x}", opcode); 42 }, // DEC E
      0x1E => { println!("self.ld_e_n() not implemented! {:x}", opcode); 42 }, // LD E,n
      0x1F => { println!("self.rra() not implemented! {:x}", opcode); 42 }, // RRA
      0x20 => { println!("self.jr_nz_n() not implemented! {:x}", opcode); 42 }, // JR NZ,n
      0x21 => { println!("self.ld_hl_nn() not implemented! {:x}", opcode); 42 }, // LD HL,nn
      0x22 => { println!("self.ld_hli_a() not implemented! {:x}", opcode); 42 }, // LD (HLI),A
      0x23 => { println!("self.inc_hl() not implemented! {:x}", opcode); 42 }, // INC HL
      0x24 => { println!("self.inc_h() not implemented! {:x}", opcode); 42 }, // INC H
      0x25 => { println!("self.dec_h() not implemented! {:x}", opcode); 42 }, // DEC H
      0x26 => { println!("self.ld_h_n() not implemented! {:x}", opcode); 42 }, // LD H,n
      0x27 => { println!("self.daa() not implemented! {:x}", opcode); 42 }, // DAA
      0x28 => { println!("self.jr_z_n() not implemented! {:x}", opcode); 42 }, // JR Z,n
      0x29 => { println!("self.add_hl_hl() not implemented! {:x}", opcode); 42 }, // ADD HL,HL
      0x2A => { println!("self.ld_a_hli() not implemented! {:x}", opcode); 42 }, // LD A,(HLI)
      0x2B => { println!("self.dec_hl() not implemented! {:x}", opcode); 42 }, // DEC HL
      0x2C => { println!("self.inc_l() not implemented! {:x}", opcode); 42 }, // INC L
      0x2D => { println!("self.dec_l() not implemented! {:x}", opcode); 42 }, // DEC L
      0x2E => { println!("self.ld_l_n() not implemented! {:x}", opcode); 42 }, // LD L,n
      0x2F => { println!("self.cpl() not implemented! {:x}", opcode); 42 }, // CPL
      0x30 => { println!("self.jr_nc_n() not implemented! {:x}", opcode); 42 }, // JR NC,n
      0x31 => { println!("self.ld_sp_nn() not implemented! {:x}", opcode); 42 }, // LD SP,nn
      0x32 => { println!("self.ld_hld__a() not implemented! {:x}", opcode); 42 }, // LD (HLD), A
      0x33 => { println!("self.inc_sp() not implemented! {:x}", opcode); 42 }, // INC SP
      0x34 => { println!("self.inc_hl() not implemented! {:x}", opcode); 42 }, // INC (HL)
      0x35 => { println!("self.dec_hl() not implemented! {:x}", opcode); 42 }, // DEC (HL)
      0x36 => { println!("self.ld_hl_n() not implemented! {:x}", opcode); 42 }, // LD (HL),n
      0x37 => { println!("self.scf() not implemented! {:x}", opcode); 42 }, // SCF
      0x38 => { println!("self.jr_c_n() not implemented! {:x}", opcode); 42 }, // JR C,n
      0x39 => { println!("self.add_hl_sp() not implemented! {:x}", opcode); 42 }, // ADD HL,SP
      0x3A => { println!("self.ld_a_hld() not implemented! {:x}", opcode); 42 }, // LD A,(HLD)
      0x3B => { println!("self.dec_sp() not implemented! {:x}", opcode); 42 }, // DEC SP
      0x3C => { println!("self.inc_a() not implemented! {:x}", opcode); 42 }, // INC A
      0x3D => { println!("self.dec_a() not implemented! {:x}", opcode); 42 }, // DEC A
      0x3E => { println!("self.ld_a_n() not implemented! {:x}", opcode); 42 }, // LD A,n
      0x3F => { println!("self.ccf() not implemented! {:x}", opcode); 42 }, // CCF
      0x40 => { println!("self.ld_b_b() not implemented! {:x}", opcode); 42 }, // LD B,B
      0x41 => { println!("self.ld_b_c() not implemented! {:x}", opcode); 42 }, // LD B,C
      0x42 => { println!("self.ld_b_d() not implemented! {:x}", opcode); 42 }, // LD B,D
      0x43 => { println!("self.ld_b_e() not implemented! {:x}", opcode); 42 }, // LD B,E
      0x44 => { println!("self.ld_b_h() not implemented! {:x}", opcode); 42 }, // LD B,H
      0x45 => { println!("self.ld_b_l() not implemented! {:x}", opcode); 42 }, // LD B,L
      0x46 => { println!("self.ld_b_hl() not implemented! {:x}", opcode); 42 }, // LD B,(HL)
      0x47 => { println!("self.ld_b_a() not implemented! {:x}", opcode); 42 }, // LD B,A
      0x48 => { println!("self.ld_c_b() not implemented! {:x}", opcode); 42 }, // LD C,B
      0x49 => { println!("self.ld_c_c() not implemented! {:x}", opcode); 42 }, // LD C,C
      0x4A => { println!("self.ld_c_d() not implemented! {:x}", opcode); 42 }, // LD C,D
      0x4B => { println!("self.ld_c_e() not implemented! {:x}", opcode); 42 }, // LD C,E
      0x4C => { println!("self.ld_c_h() not implemented! {:x}", opcode); 42 }, // LD C,H
      0x4D => { println!("self.ld_c_l() not implemented! {:x}", opcode); 42 }, // LD C,L
      0x4E => { println!("self.ld_c_hl() not implemented! {:x}", opcode); 42 }, // LD C,(HL)
      0x4F => { println!("self.ld_c_a() not implemented! {:x}", opcode); 42 }, // LD C,A
      0x50 => { println!("self.ld_d_b() not implemented! {:x}", opcode); 42 }, // LD D,B
      0x51 => { println!("self.ld_d_c() not implemented! {:x}", opcode); 42 }, // LD D,C
      0x52 => { println!("self.ld_d_d() not implemented! {:x}", opcode); 42 }, // LD D,D
      0x53 => { println!("self.ld_d_e() not implemented! {:x}", opcode); 42 }, // LD D,E
      0x54 => { println!("self.ld_d_h() not implemented! {:x}", opcode); 42 }, // LD D,H
      0x55 => { println!("self.ld_d_l() not implemented! {:x}", opcode); 42 }, // LD D,L
      0x56 => { println!("self.ld_d_hl() not implemented! {:x}", opcode); 42 }, // LD D,(HL)
      0x57 => { println!("self.ld_d_a() not implemented! {:x}", opcode); 42 }, // LD D,A
      0x58 => { println!("self.ld_e_b() not implemented! {:x}", opcode); 42 }, // LD E,B
      0x59 => { println!("self.ld_e_c() not implemented! {:x}", opcode); 42 }, // LD E,C
      0x5A => { println!("self.ld_e_d() not implemented! {:x}", opcode); 42 }, // LD E,D
      0x5B => { println!("self.ld_e_e() not implemented! {:x}", opcode); 42 }, // LD E,E
      0x5C => { println!("self.ld_e_h() not implemented! {:x}", opcode); 42 }, // LD E,H
      0x5D => { println!("self.ld_e_l() not implemented! {:x}", opcode); 42 }, // LD E,L
      0x5E => { println!("self.ld_e_hl() not implemented! {:x}", opcode); 42 }, // LD E,(HL)
      0x5F => { println!("self.ld_e_a() not implemented! {:x}", opcode); 42 }, // LD E,A
      0x60 => { println!("self.ld_h_b() not implemented! {:x}", opcode); 42 }, // LD H,B
      0x61 => { println!("self.ld_h_c() not implemented! {:x}", opcode); 42 }, // LD H,C
      0x62 => { println!("self.ld_h_d() not implemented! {:x}", opcode); 42 }, // LD H,D
      0x63 => { println!("self.ld_h_e() not implemented! {:x}", opcode); 42 }, // LD H,E
      0x64 => { println!("self.ld_h_h() not implemented! {:x}", opcode); 42 }, // LD H,H
      0x65 => { println!("self.ld_h_l() not implemented! {:x}", opcode); 42 }, // LD H,L
      0x66 => { println!("self.ld_h_hl() not implemented! {:x}", opcode); 42 }, // LD H,(HL)
      0x67 => { println!("self.ld_h_a() not implemented! {:x}", opcode); 42 }, // LD H,A
      0x68 => { println!("self.ld_l_b() not implemented! {:x}", opcode); 42 }, // LD L,B
      0x69 => { println!("self.ld_l_c() not implemented! {:x}", opcode); 42 }, // LD L,C
      0x6A => { println!("self.ld_l_d() not implemented! {:x}", opcode); 42 }, // LD L,D
      0x6B => { println!("self.ld_l_e() not implemented! {:x}", opcode); 42 }, // LD L,E
      0x6C => { println!("self.ld_l_h() not implemented! {:x}", opcode); 42 }, // LD L,H
      0x6D => { println!("self.ld_l_l() not implemented! {:x}", opcode); 42 }, // LD L,L
      0x6E => { println!("self.ld_l_hl() not implemented! {:x}", opcode); 42 }, // LD L,(HL)
      0x6F => { println!("self.ld_l_a() not implemented! {:x}", opcode); 42 }, // LD L,A
      0x70 => { println!("self.ld_hl_b() not implemented! {:x}", opcode); 42 }, // LD (HL),B
      0x71 => { println!("self.ld_hl_c() not implemented! {:x}", opcode); 42 }, // LD (HL),C
      0x72 => { println!("self.ld_hl_d() not implemented! {:x}", opcode); 42 }, // LD (HL),D
      0x73 => { println!("self.ld_hl_e() not implemented! {:x}", opcode); 42 }, // LD (HL),E
      0x74 => { println!("self.ld_hl_h() not implemented! {:x}", opcode); 42 }, // LD (HL),H
      0x75 => { println!("self.ld_hl_l() not implemented! {:x}", opcode); 42 }, // LD (HL),L
      0x76 => { println!("self.halt() not implemented! {:x}", opcode); 42 }, // HALT
      0x77 => { println!("self.ld_hl_a() not implemented! {:x}", opcode); 42 }, // LD (HL),A
      0x78 => { println!("self.ld_a_b() not implemented! {:x}", opcode); 42 }, // LD A,B
      0x79 => { println!("self.ld_a_c() not implemented! {:x}", opcode); 42 }, // LD A,C
      0x7A => { println!("self.ld_a_d() not implemented! {:x}", opcode); 42 }, // LD A,D
      0x7B => { println!("self.ld_a_e() not implemented! {:x}", opcode); 42 }, // LD A,E
      0x7C => { println!("self.ld_a_h() not implemented! {:x}", opcode); 42 }, // LD A,H
      0x7D => { println!("self.ld_a_l() not implemented! {:x}", opcode); 42 }, // LD A,L
      0x7E => { println!("self.ld_a_hl() not implemented! {:x}", opcode); 42 }, // LD A,(HL)
      0x7F => { println!("self.ld_a_a() not implemented! {:x}", opcode); 42 }, // LD A,A
      0x80 => { println!("self.add_a_b() not implemented! {:x}", opcode); 42 }, // ADD A,B
      0x81 => { println!("self.add_a_c() not implemented! {:x}", opcode); 42 }, // ADD A,C
      0x82 => { println!("self.add_a_d() not implemented! {:x}", opcode); 42 }, // ADD A,D
      0x83 => { println!("self.add_a_e() not implemented! {:x}", opcode); 42 }, // ADD A,E
      0x84 => { println!("self.add_a_h() not implemented! {:x}", opcode); 42 }, // ADD A,H
      0x85 => { println!("self.add_a_l() not implemented! {:x}", opcode); 42 }, // ADD A,L
      0x86 => { println!("self.add_a_hl() not implemented! {:x}", opcode); 42 }, // ADD A,(HL)
      0x87 => { println!("self.add_a_a() not implemented! {:x}", opcode); 42 }, // ADD A,A
      0x88 => { println!("self.adc_a_b() not implemented! {:x}", opcode); 42 }, // ADC A,B
      0x89 => { println!("self.adc_a_c() not implemented! {:x}", opcode); 42 }, // ADC A,C
      0x8A => { println!("self.adc_a_d() not implemented! {:x}", opcode); 42 }, // ADC A,D
      0x8B => { println!("self.adc_a_e() not implemented! {:x}", opcode); 42 }, // ADC A,E
      0x8C => { println!("self.adc_a_h() not implemented! {:x}", opcode); 42 }, // ADC A,H
      0x8D => { println!("self.adc_a_l() not implemented! {:x}", opcode); 42 }, // ADC A,L
      0x8E => { println!("self.adc_a_hl() not implemented! {:x}", opcode); 42 }, // ADC A,(HL)
      0x8F => { println!("self.adc_a_a() not implemented! {:x}", opcode); 42 }, // ADC A,A
      0x90 => { println!("self.sub_b() not implemented! {:x}", opcode); 42 }, // SUB B
      0x91 => { println!("self.sub_c() not implemented! {:x}", opcode); 42 }, // SUB C
      0x92 => { println!("self.sub_d() not implemented! {:x}", opcode); 42 }, // SUB D
      0x93 => { println!("self.sub_e() not implemented! {:x}", opcode); 42 }, // SUB E
      0x94 => { println!("self.sub_h() not implemented! {:x}", opcode); 42 }, // SUB H
      0x95 => { println!("self.sub_l() not implemented! {:x}", opcode); 42 }, // SUB L
      0x96 => { println!("self.sub_hl() not implemented! {:x}", opcode); 42 }, // SUB (HL)
      0x97 => { println!("self.sub_a() not implemented! {:x}", opcode); 42 }, // SUB A
      0x98 => { println!("self.sbc_b() not implemented! {:x}", opcode); 42 }, // SBC B
      0x99 => { println!("self.sbc_c() not implemented! {:x}", opcode); 42 }, // SBC C
      0x9A => { println!("self.sbc_d() not implemented! {:x}", opcode); 42 }, // SBC D
      0x9B => { println!("self.sbc_e() not implemented! {:x}", opcode); 42 }, // SBC E
      0x9C => { println!("self.sbc_h() not implemented! {:x}", opcode); 42 }, // SBC H
      0x9D => { println!("self.sbc_l() not implemented! {:x}", opcode); 42 }, // SBC L
      0x9E => { println!("self.sbc_hl() not implemented! {:x}", opcode); 42 }, // SBC (HL)
      0x9F => { println!("self.sbc_a() not implemented! {:x}", opcode); 42 }, // SBC A
      0xA0 => { println!("self.and_b() not implemented! {:x}", opcode); 42 }, // AND B
      0xA1 => { println!("self.and_c() not implemented! {:x}", opcode); 42 }, // AND C
      0xA2 => { println!("self.and_d() not implemented! {:x}", opcode); 42 }, // AND D
      0xA3 => { println!("self.and_e() not implemented! {:x}", opcode); 42 }, // AND E
      0xA4 => { println!("self.and_h() not implemented! {:x}", opcode); 42 }, // AND H
      0xA5 => { println!("self.and_l() not implemented! {:x}", opcode); 42 }, // AND L
      0xA6 => { println!("self.and_hl() not implemented! {:x}", opcode); 42 }, // AND (HL)
      0xA7 => { println!("self.and_a() not implemented! {:x}", opcode); 42 }, // AND A
      0xA8 => { println!("self.xor_b() not implemented! {:x}", opcode); 42 }, // XOR B
      0xA9 => { println!("self.xor_c() not implemented! {:x}", opcode); 42 }, // XOR C
      0xAA => { println!("self.xor_d() not implemented! {:x}", opcode); 42 }, // XOR D
      0xAB => { println!("self.xor_e() not implemented! {:x}", opcode); 42 }, // XOR E
      0xAC => { println!("self.xor_h() not implemented! {:x}", opcode); 42 }, // XOR H
      0xAD => { println!("self.xor_l() not implemented! {:x}", opcode); 42 }, // XOR L
      0xAE => { println!("self.xor_hl() not implemented! {:x}", opcode); 42 }, // XOR (HL)
      0xAF => { println!("self.xor_a() not implemented! {:x}", opcode); 42 }, // XOR A
      0xB0 => { println!("self.or_b() not implemented! {:x}", opcode); 42 }, // OR B
      0xB1 => { println!("self.or_c() not implemented! {:x}", opcode); 42 }, // OR C
      0xB2 => { println!("self.or_d() not implemented! {:x}", opcode); 42 }, // OR D
      0xB3 => { println!("self.or_e() not implemented! {:x}", opcode); 42 }, // OR E
      0xB4 => { println!("self.or_h() not implemented! {:x}", opcode); 42 }, // OR H
      0xB5 => { println!("self.or_l() not implemented! {:x}", opcode); 42 }, // OR L
      0xB6 => { println!("self.or_hl() not implemented! {:x}", opcode); 42 }, // OR (HL)
      0xB7 => { println!("self.or_a() not implemented! {:x}", opcode); 42 }, // OR A
      0xB8 => { println!("self.cp_b() not implemented! {:x}", opcode); 42 }, // CP B
      0xB9 => { println!("self.cp_c() not implemented! {:x}", opcode); 42 }, // CP C
      0xBA => { println!("self.cp_d() not implemented! {:x}", opcode); 42 }, // CP D
      0xBB => { println!("self.cp_e() not implemented! {:x}", opcode); 42 }, // CP E
      0xBC => { println!("self.cp_h() not implemented! {:x}", opcode); 42 }, // CP H
      0xBD => { println!("self.cp_l() not implemented! {:x}", opcode); 42 }, // CP L
      0xBE => { println!("self.cp_hl() not implemented! {:x}", opcode); 42 }, // CP (HL)
      0xBF => { println!("self.cp_a() not implemented! {:x}", opcode); 42 }, // CP A
      0xC0 => { println!("self.ret_nz() not implemented! {:x}", opcode); 42 }, // RET NZ
      0xC1 => { println!("self.pop_bc() not implemented! {:x}", opcode); 42 }, // POP BC
      0xC2 => { println!("self.jp_nz_nn() not implemented! {:x}", opcode); 42 }, // JP NZ,nn
      0xC3 => { self.jp_nn(mmu); 42 }, // JP nn
      0xC4 => { println!("self.call_nz_nn() not implemented! {:x}", opcode); 42 }, // CALL NZ,nn
      0xC5 => { println!("self.push_bc() not implemented! {:x}", opcode); 42 }, // PUSH BC
      0xC6 => { println!("self.add_a_n() not implemented! {:x}", opcode); 42 }, // ADD A,n
      0xC7 => { println!("self.rst_00h() not implemented! {:x}", opcode); 42 }, // RST 00H
      0xC8 => { println!("self.ret_z() not implemented! {:x}", opcode); 42 }, // RET Z
      0xC9 => { println!("self.ret() not implemented! {:x}", opcode); 42 }, // RET
      0xCA => { println!("self.jp_z_nn() not implemented! {:x}", opcode); 42 }, // JP Z,nn
      0xCB => { println!("self.cb_prefixed_instruction() not implemented! {:x}", opcode); 42 }, // CB prefixed instruction
      0xCC => { println!("self.call_z_nn() not implemented! {:x}", opcode); 42 }, // CALL Z,nn
      0xCD => { println!("self.call_nn() not implemented! {:x}", opcode); 42 }, // CALL nn
      0xCE => { println!("self.adc_a_n() not implemented! {:x}", opcode); 42 }, // ADC A,n
      0xCF => { println!("self.rst_08h() not implemented! {:x}", opcode); 42 }, // RST 08H
      0xD0 => { println!("self.ret_nc() not implemented! {:x}", opcode); 42 }, // RET NC
      0xD1 => { println!("self.pop_de() not implemented! {:x}", opcode); 42 }, // POP DE
      0xD2 => { println!("self.jp_nc_nn() not implemented! {:x}", opcode); 42 }, // JP NC,nn
      0xD4 => { println!("self.call_nc_nn() not implemented! {:x}", opcode); 42 }, // CALL NC,nn
      0xD5 => { println!("self.push_de() not implemented! {:x}", opcode); 42 }, // PUSH DE
      0xD6 => { println!("self.sub_n() not implemented! {:x}", opcode); 42 }, // SUB n
      0xD7 => { println!("self.rst_10h() not implemented! {:x}", opcode); 42 }, // RST 10H
      0xD8 => { println!("self.ret_c() not implemented! {:x}", opcode); 42 }, // RET C
      0xD9 => { println!("self.reti() not implemented! {:x}", opcode); 42 }, // RETI
      0xDA => { println!("self.jp_c_nn() not implemented! {:x}", opcode); 42 }, // JP C,nn
      0xDC => { println!("self.call_c_nn() not implemented! {:x}", opcode); 42 }, // CALL C,nn
      0xDE => { println!("self.sbc_n() not implemented! {:x}", opcode); 42 }, // SBC n
      0xDF => { println!("self.rst_18h() not implemented! {:x}", opcode); 42 }, // RST 18H
      0xE0 => { println!("self.ld_0xff00_plus_n_a() not implemented! {:x}", opcode); 42 }, // LD (0xFF00+n),A
      0xE1 => { println!("self.pop_hl() not implemented! {:x}", opcode); 42 }, // POP HL
      0xE2 => { println!("self.ld_0xff00_plus_c_a() not implemented! {:x}", opcode); 42 }, // LD (0xFF00+C),A
      0xE5 => { println!("self.push_hl() not implemented! {:x}", opcode); 42 }, // PUSH HL
      0xE6 => { println!("self.and_n() not implemented! {:x}", opcode); 42 }, // AND n
      0xE7 => { println!("self.rst_20h() not implemented! {:x}", opcode); 42 }, // RST 20H
      0xE8 => { println!("self.add_sp_n() not implemented! {:x}", opcode); 42 }, // ADD SP,n
      0xE9 => { println!("self.jp_hl() not implemented! {:x}", opcode); 42 }, // JP (HL)
      0xEA => { println!("self.ld_nn_a() not implemented! {:x}", opcode); 42 }, // LD (nn),A
      0xEE => { println!("self.xor_n() not implemented! {:x}", opcode); 42 }, // XOR n
      0xEF => { println!("self.rst_28h() not implemented! {:x}", opcode); 42 }, // RST 28H
      0xF0 => { println!("self.ld_a_0xff00_plus_n() not implemented! {:x}", opcode); 42 }, // LD A,(0xFF00+n)
      0xF1 => { println!("self.pop_af() not implemented! {:x}", opcode); 42 }, // POP AF
      0xF2 => { println!("self.ld_a_c() not implemented! {:x}", opcode); 42 }, // LD A,(C)
      0xF3 => { println!("self.di() not implemented! {:x}", opcode); 42 }, // DI
      0xF5 => { println!("self.push_af() not implemented! {:x}", opcode); 42 }, // PUSH AF
      0xF6 => { println!("self.or_n() not implemented! {:x}", opcode); 42 }, // OR n
      0xF7 => { println!("self.rst_30h() not implemented! {:x}", opcode); 42 }, // RST 30H
      0xF8 => { println!("self.ld_hl_sp_plus_n() not implemented! {:x}", opcode); 42 }, // LD HL,SP+n
      0xF9 => { println!("self.ld_sp_hl() not implemented! {:x}", opcode); 42 }, // LD SP,HL
      0xFA => { println!("self.ld_a_nn() not implemented! {:x}", opcode); 42 }, // LD A,(nn)
      0xFB => { println!("self.ei() not implemented! {:x}", opcode); 42 }, // EI
      0xFE => { println!("self.cp_n() not implemented! {:x}", opcode); 42 }, // CP n
      0xFF => { println!("self.rst_38h() not implemented! {:x}", opcode); 42 }, // RST 38H
      _ => panic!("Unexpected opcode: {:x}", opcode)
    }
    // Ok(cycles)
  }

  fn nop(&self) -> i32 {
    4
  }

  fn jp_nn(&mut self, mmu: &mmu::MMU) -> i32 {
    self.PC = mmu.read_word(self.PC);

    12
  }

  // fn op_xor_8bit_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
  //   let A = self.AF.read_hi();
  //
  //   let n = match reg {
  //     RegEnum::A => A,
  //     RegEnum::B => self.BC.read_hi(),
  //     _ => panic!("Unexpected RegEnum: {:?}", reg)
  //   };
  //
  //   let result = n ^ A;
  //   self.AF.write_hi(result);
  //
  //   self.reset_all_flags();
  //
  //   if result == 0x00 {
  //     self.set_flag(FLAG_Z);
  //   } else {
  //     self.reset_flag(FLAG_Z);
  //   }
  //
  //   4
  // }
  //
  // fn dec_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
  //   let (n, before) = match reg {
  //     RegEnum::B => {
  //       let before = self.BC.read_hi();
  //       let n = before.wrapping_sub(1);
  //       self.BC.write_hi(n);
  //       (n, before)
  //     },
  //     _ => panic!("Unexpected RegEnum: {:?}", reg)
  //   };
  //
  //   if n == 0x00 {
  //     self.set_flag(FLAG_Z);
  //   }
  //
  //   self.set_flag(FLAG_N);
  //
  //   if (before & 0x0f) == 0 {
  //     self.set_flag(FLAG_H);
  //   } else {
  //     self.reset_flag(FLAG_H);
  //   }
  //
  //   4 // TODO
  // }
  //
  // fn ld_n_nn(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
  //   let nn = mmu.read_word(self.PC);
  //   self.PC += 2; // TODO is this correct?
  //   // println!("ld_n_nn -> nn: {:x}", nn);
  //
  //   match reg {
  //     RegEnum::HL => {
  //       self.HL.write(nn);
  //     },
  //     _ => panic!("Unexpected RegEnum: {:?}", reg)
  //   };
  //
  //   12
  // }
  //
  // fn ld_nn_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
  //   let nn = mmu.read(self.PC);
  //   self.PC += 1;
  //
  //   match reg {
  //     RegEnum::C => {
  //       self.BC.write_lo(nn);
  //     },
  //     RegEnum::B => {
  //       self.BC.write_hi(nn);
  //     },
  //     _ => panic!("Unexpected RegEnum: {:?}", reg)
  //   };
  //
  //   8
  // }
  //
  // fn ldd_nn_n(&mut self, regSrc: RegEnum, regDstAndDec: RegEnum) -> i32 {
  //   let srcVal = match regSrc {
  //     RegEnum::A => {
  //       self.AF.read_hi()
  //     },
  //     _ => panic!("Unexpected RegEnum: {:?}", regSrc)
  //   };
  //
  //   // println!("{:x}", srcVal);
  //   // NOTE: wrapping_sub
  //   // https://doc.rust-lang.org/std/primitive.u8.html#method.wrapping_add
  //
  //   match regDstAndDec {
  //     RegEnum::HL => {
  //       self.HL.write((srcVal.wrapping_sub(1)) as types::Word);
  //     },
  //     _ => panic!("Unexpected RegEnum: {:?}", regDstAndDec)
  //   };
  //
  //   8
  // }

  pub fn set_flag(&mut self, flag: u8) {
    let flags = self.AF.read_lo();
    self.AF.write_lo(flags | 1 << flag);
  }

  pub fn reset_flag(&mut self, flag: u8) {
    let flags = self.AF.read_lo();
    self.AF.write_lo(flags | 0 << flag);
  }

  pub fn reset_all_flags(&mut self) {
    self.AF.write_lo(0x00);
  }

  pub fn read_flag(&self, flag: u8) -> bool {
    (self.AF.read_lo() & 1 << flag) != 0
  }
}

#[test]
fn set_flags() {
  let mut cpu = CPU::new();
  assert_eq!(false, cpu.read_flag(FLAG_Z));
  cpu.set_flag(FLAG_Z);
  assert_eq!(true, cpu.read_flag(FLAG_Z));
}

#[test]
fn register_setting() {
  let mut register = Register::new();
  register.write(0xAABB);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xBB, register.read_lo());
  assert_eq!(0xAABB, register.read());

  register.write_lo(0xFF);
  assert_eq!(0xAA, register.read_hi());
  assert_eq!(0xFF, register.read_lo());
  assert_eq!(0xAAFF, register.read());

  register.write_hi(0xDD);
  assert_eq!(0xDD, register.read_hi());
  assert_eq!(0xFF, register.read_lo());
  assert_eq!(0xDDFF, register.read());
}

#[test]
fn opcode_nop() {
  let mut cpu = CPU::new();
  let mut mmu = mmu::MMU::new();
  mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::NOP] };

  let cycles = cpu.execute_next_opcode(&mut mmu);
  assert_eq!(4, cycles);
  assert_eq!(0x0001, cpu.PC);
}

#[test]
fn opcode_jp_nn() {
  let mut cpu = CPU::new();
  let mut mmu = mmu::MMU::new();
  mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::JP_NN, 0x50, 0x01] };

  let cycles = cpu.execute_next_opcode(&mut mmu);
  assert_eq!(12, cycles);
  assert_eq!(0x0150, cpu.PC);
}

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
  assert!(cpu.read_flag(FLAG_Z));
  assert!(!cpu.read_flag(FLAG_N));
  assert!(!cpu.read_flag(FLAG_H));
  assert!(!cpu.read_flag(FLAG_C));
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
  assert!(!cpu.read_flag(FLAG_Z));
  assert!(!cpu.read_flag(FLAG_N));
  assert!(!cpu.read_flag(FLAG_H));
  assert!(!cpu.read_flag(FLAG_C));
}

#[test]
fn opcode_dec_b() {
  let mut cpu = CPU::new();
  let mut mmu = mmu::MMU::new();
  mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::DEC_B] };

  cpu.BC.write_hi(0xF0);

  let cycles = cpu.execute_next_opcode(&mut mmu);
  // assert_eq!(0x31, cpu.AF.read_hi());
  assert_eq!(4, cycles);
  assert_eq!(0x0001, cpu.PC);
  assert!(!cpu.read_flag(FLAG_Z));
  assert!(cpu.read_flag(FLAG_N));
  assert!(cpu.read_flag(FLAG_H));
  assert!(!cpu.read_flag(FLAG_C));
}

// Unused cb prefixed opcodes
// 0x00 => { println!("self.rlc_b() not implemented! {:x}", opcode); 42 }, // RLC B
// 0x01 => { println!("self.rlc_c() not implemented! {:x}", opcode); 42 }, // RLC C
// 0x02 => { println!("self.rlc_d() not implemented! {:x}", opcode); 42 }, // RLC D
// 0x03 => { println!("self.rlc_e() not implemented! {:x}", opcode); 42 }, // RLC E
// 0x04 => { println!("self.rlc_h() not implemented! {:x}", opcode); 42 }, // RLC H
// 0x05 => { println!("self.rlc_l() not implemented! {:x}", opcode); 42 }, // RLC L
// 0x06 => { println!("self.rlc_hl() not implemented! {:x}", opcode); 42 }, // RLC (HL)
// 0x07 => { println!("self.rlc_a() not implemented! {:x}", opcode); 42 }, // RLC A
// 0x08 => { println!("self.rrc_b() not implemented! {:x}", opcode); 42 }, // RRC B
// 0x09 => { println!("self.rrc_c() not implemented! {:x}", opcode); 42 }, // RRC C
// 0x0A => { println!("self.rrc_d() not implemented! {:x}", opcode); 42 }, // RRC D
// 0x0B => { println!("self.rrc_e() not implemented! {:x}", opcode); 42 }, // RRC E
// 0x0C => { println!("self.rrc_h() not implemented! {:x}", opcode); 42 }, // RRC H
// 0x0D => { println!("self.rrc_l() not implemented! {:x}", opcode); 42 }, // RRC L
// 0x0E => { println!("self.rrc_hl() not implemented! {:x}", opcode); 42 }, // RRC (HL)
// 0x0F => { println!("self.rrc_a() not implemented! {:x}", opcode); 42 }, // RRC A
// 0x10 => { println!("self.rl_b() not implemented! {:x}", opcode); 42 }, // RL B
// 0x11 => { println!("self.rl_c() not implemented! {:x}", opcode); 42 }, // RL C
// 0x12 => { println!("self.rl_d() not implemented! {:x}", opcode); 42 }, // RL D
// 0x13 => { println!("self.rl_e() not implemented! {:x}", opcode); 42 }, // RL E
// 0x14 => { println!("self.rl_h() not implemented! {:x}", opcode); 42 }, // RL H
// 0x15 => { println!("self.rl_l() not implemented! {:x}", opcode); 42 }, // RL L
// 0x16 => { println!("self.rl_hl() not implemented! {:x}", opcode); 42 }, // RL (HL)
// 0x17 => { println!("self.rl_a() not implemented! {:x}", opcode); 42 }, // RL A
// 0x18 => { println!("self.rr_b() not implemented! {:x}", opcode); 42 }, // RR B
// 0x19 => { println!("self.rr_c() not implemented! {:x}", opcode); 42 }, // RR C
// 0x1A => { println!("self.rr_d() not implemented! {:x}", opcode); 42 }, // RR D
// 0x1B => { println!("self.rr_e() not implemented! {:x}", opcode); 42 }, // RR E
// 0x1C => { println!("self.rr_h() not implemented! {:x}", opcode); 42 }, // RR H
// 0x1D => { println!("self.rr_l() not implemented! {:x}", opcode); 42 }, // RR L
// 0x1E => { println!("self.rr_hl() not implemented! {:x}", opcode); 42 }, // RR (HL)
// 0x1F => { println!("self.rr_a() not implemented! {:x}", opcode); 42 }, // RR A
// 0x20 => { println!("self.sla_b() not implemented! {:x}", opcode); 42 }, // SLA B
// 0x21 => { println!("self.sla_c() not implemented! {:x}", opcode); 42 }, // SLA C
// 0x22 => { println!("self.sla_d() not implemented! {:x}", opcode); 42 }, // SLA D
// 0x23 => { println!("self.sla_e() not implemented! {:x}", opcode); 42 }, // SLA E
// 0x24 => { println!("self.sla_h() not implemented! {:x}", opcode); 42 }, // SLA H
// 0x25 => { println!("self.sla_l() not implemented! {:x}", opcode); 42 }, // SLA L
// 0x26 => { println!("self.sla_hl() not implemented! {:x}", opcode); 42 }, // SLA (HL)
// 0x27 => { println!("self.sla_a() not implemented! {:x}", opcode); 42 }, // SLA A
// 0x28 => { println!("self.sra_b() not implemented! {:x}", opcode); 42 }, // SRA B
// 0x29 => { println!("self.sra_c() not implemented! {:x}", opcode); 42 }, // SRA C
// 0x2A => { println!("self.sra_d() not implemented! {:x}", opcode); 42 }, // SRA D
// 0x2B => { println!("self.sra_e() not implemented! {:x}", opcode); 42 }, // SRA E
// 0x2C => { println!("self.sra_h() not implemented! {:x}", opcode); 42 }, // SRA H
// 0x2D => { println!("self.sra_l() not implemented! {:x}", opcode); 42 }, // SRA L
// 0x2E => { println!("self.sra_hl() not implemented! {:x}", opcode); 42 }, // SRA (HL)
// 0x2F => { println!("self.sra_a() not implemented! {:x}", opcode); 42 }, // SRA A
// 0x30 => { println!("self.swap_b() not implemented! {:x}", opcode); 42 }, // SWAP B
// 0x31 => { println!("self.swap_c() not implemented! {:x}", opcode); 42 }, // SWAP C
// 0x32 => { println!("self.swap_d() not implemented! {:x}", opcode); 42 }, // SWAP D
// 0x33 => { println!("self.swap_e() not implemented! {:x}", opcode); 42 }, // SWAP E
// 0x34 => { println!("self.swap_h() not implemented! {:x}", opcode); 42 }, // SWAP H
// 0x35 => { println!("self.swap_l() not implemented! {:x}", opcode); 42 }, // SWAP L
// 0x36 => { println!("self.swap_hl() not implemented! {:x}", opcode); 42 }, // SWAP (HL)
// 0x37 => { println!("self.swap_a() not implemented! {:x}", opcode); 42 }, // SWAP A
// 0x38 => { println!("self.srl_b() not implemented! {:x}", opcode); 42 }, // SRL B
// 0x39 => { println!("self.srl_c() not implemented! {:x}", opcode); 42 }, // SRL C
// 0x3A => { println!("self.srl_d() not implemented! {:x}", opcode); 42 }, // SRL D
// 0x3B => { println!("self.srl_e() not implemented! {:x}", opcode); 42 }, // SRL E
// 0x3C => { println!("self.srl_h() not implemented! {:x}", opcode); 42 }, // SRL H
// 0x3D => { println!("self.srl_l() not implemented! {:x}", opcode); 42 }, // SRL L
// 0x3E => { println!("self.srl_hl() not implemented! {:x}", opcode); 42 }, // SRL (HL)
// 0x3F => { println!("self.srl_a() not implemented! {:x}", opcode); 42 }, // SRL A
// 0x40 => { println!("self.bit_0_b() not implemented! {:x}", opcode); 42 }, // BIT 0 B
// 0x41 => { println!("self.bit_0_c() not implemented! {:x}", opcode); 42 }, // BIT 0 C
// 0x42 => { println!("self.bit_0_d() not implemented! {:x}", opcode); 42 }, // BIT 0 D
// 0x43 => { println!("self.bit_0_e() not implemented! {:x}", opcode); 42 }, // BIT 0 E
// 0x44 => { println!("self.bit_0_h() not implemented! {:x}", opcode); 42 }, // BIT 0 H
// 0x45 => { println!("self.bit_0_l() not implemented! {:x}", opcode); 42 }, // BIT 0 L
// 0x46 => { println!("self.bit_0_hl() not implemented! {:x}", opcode); 42 }, // BIT 0 (HL)
// 0x47 => { println!("self.bit_0_a() not implemented! {:x}", opcode); 42 }, // BIT 0 A
// 0x48 => { println!("self.bit_1_b() not implemented! {:x}", opcode); 42 }, // BIT 1 B
// 0x49 => { println!("self.bit_1_c() not implemented! {:x}", opcode); 42 }, // BIT 1 C
// 0x4A => { println!("self.bit_1_d() not implemented! {:x}", opcode); 42 }, // BIT 1 D
// 0x4B => { println!("self.bit_1_e() not implemented! {:x}", opcode); 42 }, // BIT 1 E
// 0x4C => { println!("self.bit_1_h() not implemented! {:x}", opcode); 42 }, // BIT 1 H
// 0x4D => { println!("self.bit_1_l() not implemented! {:x}", opcode); 42 }, // BIT 1 L
// 0x4E => { println!("self.bit_1_hl() not implemented! {:x}", opcode); 42 }, // BIT 1 (HL)
// 0x4F => { println!("self.bit_1_a() not implemented! {:x}", opcode); 42 }, // BIT 1 A
// 0x50 => { println!("self.bit_2_b() not implemented! {:x}", opcode); 42 }, // BIT 2 B
// 0x51 => { println!("self.bit_2_c() not implemented! {:x}", opcode); 42 }, // BIT 2 C
// 0x52 => { println!("self.bit_2_d() not implemented! {:x}", opcode); 42 }, // BIT 2 D
// 0x53 => { println!("self.bit_2_e() not implemented! {:x}", opcode); 42 }, // BIT 2 E
// 0x54 => { println!("self.bit_2_h() not implemented! {:x}", opcode); 42 }, // BIT 2 H
// 0x55 => { println!("self.bit_2_l() not implemented! {:x}", opcode); 42 }, // BIT 2 L
// 0x56 => { println!("self.bit_2_hl() not implemented! {:x}", opcode); 42 }, // BIT 2 (HL)
// 0x57 => { println!("self.bit_2_a() not implemented! {:x}", opcode); 42 }, // BIT 2 A
// 0x58 => { println!("self.bit_3_b() not implemented! {:x}", opcode); 42 }, // BIT 3 B
// 0x59 => { println!("self.bit_3_c() not implemented! {:x}", opcode); 42 }, // BIT 3 C
// 0x5A => { println!("self.bit_3_d() not implemented! {:x}", opcode); 42 }, // BIT 3 D
// 0x5B => { println!("self.bit_3_e() not implemented! {:x}", opcode); 42 }, // BIT 3 E
// 0x5C => { println!("self.bit_3_h() not implemented! {:x}", opcode); 42 }, // BIT 3 H
// 0x5D => { println!("self.bit_3_l() not implemented! {:x}", opcode); 42 }, // BIT 3 L
// 0x5E => { println!("self.bit_3_hl() not implemented! {:x}", opcode); 42 }, // BIT 3 (HL)
// 0x5F => { println!("self.bit_3_a() not implemented! {:x}", opcode); 42 }, // BIT 3 A
// 0x60 => { println!("self.bit_4_b() not implemented! {:x}", opcode); 42 }, // BIT 4 B
// 0x61 => { println!("self.bit_4_c() not implemented! {:x}", opcode); 42 }, // BIT 4 C
// 0x62 => { println!("self.bit_4_d() not implemented! {:x}", opcode); 42 }, // BIT 4 D
// 0x63 => { println!("self.bit_4_e() not implemented! {:x}", opcode); 42 }, // BIT 4 E
// 0x64 => { println!("self.bit_4_h() not implemented! {:x}", opcode); 42 }, // BIT 4 H
// 0x65 => { println!("self.bit_4_l() not implemented! {:x}", opcode); 42 }, // BIT 4 L
// 0x66 => { println!("self.bit_4_hl() not implemented! {:x}", opcode); 42 }, // BIT 4 (HL)
// 0x67 => { println!("self.bit_4_a() not implemented! {:x}", opcode); 42 }, // BIT 4 A
// 0x68 => { println!("self.bit_5_b() not implemented! {:x}", opcode); 42 }, // BIT 5 B
// 0x69 => { println!("self.bit_5_c() not implemented! {:x}", opcode); 42 }, // BIT 5 C
// 0x6A => { println!("self.bit_5_d() not implemented! {:x}", opcode); 42 }, // BIT 5 D
// 0x6B => { println!("self.bit_5_e() not implemented! {:x}", opcode); 42 }, // BIT 5 E
// 0x6C => { println!("self.bit_5_h() not implemented! {:x}", opcode); 42 }, // BIT 5 H
// 0x6D => { println!("self.bit_5_l() not implemented! {:x}", opcode); 42 }, // BIT 5 L
// 0x6E => { println!("self.bit_5_hl() not implemented! {:x}", opcode); 42 }, // BIT 5 (HL)
// 0x6F => { println!("self.bit_5_a() not implemented! {:x}", opcode); 42 }, // BIT 5 A
// 0x70 => { println!("self.bit_6_b() not implemented! {:x}", opcode); 42 }, // BIT 6 B
// 0x71 => { println!("self.bit_6_c() not implemented! {:x}", opcode); 42 }, // BIT 6 C
// 0x72 => { println!("self.bit_6_d() not implemented! {:x}", opcode); 42 }, // BIT 6 D
// 0x73 => { println!("self.bit_6_e() not implemented! {:x}", opcode); 42 }, // BIT 6 E
// 0x74 => { println!("self.bit_6_h() not implemented! {:x}", opcode); 42 }, // BIT 6 H
// 0x75 => { println!("self.bit_6_l() not implemented! {:x}", opcode); 42 }, // BIT 6 L
// 0x76 => { println!("self.bit_6_hl() not implemented! {:x}", opcode); 42 }, // BIT 6 (HL)
// 0x77 => { println!("self.bit_6_a() not implemented! {:x}", opcode); 42 }, // BIT 6 A
// 0x78 => { println!("self.bit_7_b() not implemented! {:x}", opcode); 42 }, // BIT 7 B
// 0x79 => { println!("self.bit_7_c() not implemented! {:x}", opcode); 42 }, // BIT 7 C
// 0x7A => { println!("self.bit_7_d() not implemented! {:x}", opcode); 42 }, // BIT 7 D
// 0x7B => { println!("self.bit_7_e() not implemented! {:x}", opcode); 42 }, // BIT 7 E
// 0x7C => { println!("self.bit_7_h() not implemented! {:x}", opcode); 42 }, // BIT 7 H
// 0x7D => { println!("self.bit_7_l() not implemented! {:x}", opcode); 42 }, // BIT 7 L
// 0x7E => { println!("self.bit_7_hl() not implemented! {:x}", opcode); 42 }, // BIT 7 (HL)
// 0x7F => { println!("self.bit_7_a() not implemented! {:x}", opcode); 42 }, // BIT 7 A
// 0x80 => { println!("self.res_0_b() not implemented! {:x}", opcode); 42 }, // RES 0 B
// 0x81 => { println!("self.res_0_c() not implemented! {:x}", opcode); 42 }, // RES 0 C
// 0x82 => { println!("self.res_0_d() not implemented! {:x}", opcode); 42 }, // RES 0 D
// 0x83 => { println!("self.res_0_e() not implemented! {:x}", opcode); 42 }, // RES 0 E
// 0x84 => { println!("self.res_0_h() not implemented! {:x}", opcode); 42 }, // RES 0 H
// 0x85 => { println!("self.res_0_l() not implemented! {:x}", opcode); 42 }, // RES 0 L
// 0x86 => { println!("self.res_0_hl() not implemented! {:x}", opcode); 42 }, // RES 0 (HL)
// 0x87 => { println!("self.res_0_a() not implemented! {:x}", opcode); 42 }, // RES 0 A
// 0x88 => { println!("self.res_1_b() not implemented! {:x}", opcode); 42 }, // RES 1 B
// 0x89 => { println!("self.res_1_c() not implemented! {:x}", opcode); 42 }, // RES 1 C
// 0x8A => { println!("self.res_1_d() not implemented! {:x}", opcode); 42 }, // RES 1 D
// 0x8B => { println!("self.res_1_e() not implemented! {:x}", opcode); 42 }, // RES 1 E
// 0x8C => { println!("self.res_1_h() not implemented! {:x}", opcode); 42 }, // RES 1 H
// 0x8D => { println!("self.res_1_l() not implemented! {:x}", opcode); 42 }, // RES 1 L
// 0x8E => { println!("self.res_1_hl() not implemented! {:x}", opcode); 42 }, // RES 1 (HL)
// 0x8F => { println!("self.res_1_a() not implemented! {:x}", opcode); 42 }, // RES 1 A
// 0x90 => { println!("self.res_2_b() not implemented! {:x}", opcode); 42 }, // RES 2 B
// 0x91 => { println!("self.res_2_c() not implemented! {:x}", opcode); 42 }, // RES 2 C
// 0x92 => { println!("self.res_2_d() not implemented! {:x}", opcode); 42 }, // RES 2 D
// 0x93 => { println!("self.res_2_e() not implemented! {:x}", opcode); 42 }, // RES 2 E
// 0x94 => { println!("self.res_2_h() not implemented! {:x}", opcode); 42 }, // RES 2 H
// 0x95 => { println!("self.res_2_l() not implemented! {:x}", opcode); 42 }, // RES 2 L
// 0x96 => { println!("self.res_2_hl() not implemented! {:x}", opcode); 42 }, // RES 2 (HL)
// 0x97 => { println!("self.res_2_a() not implemented! {:x}", opcode); 42 }, // RES 2 A
// 0x98 => { println!("self.res_3_b() not implemented! {:x}", opcode); 42 }, // RES 3 B
// 0x99 => { println!("self.res_3_c() not implemented! {:x}", opcode); 42 }, // RES 3 C
// 0x9A => { println!("self.res_3_d() not implemented! {:x}", opcode); 42 }, // RES 3 D
// 0x9B => { println!("self.res_3_e() not implemented! {:x}", opcode); 42 }, // RES 3 E
// 0x9C => { println!("self.res_3_h() not implemented! {:x}", opcode); 42 }, // RES 3 H
// 0x9D => { println!("self.res_3_l() not implemented! {:x}", opcode); 42 }, // RES 3 L
// 0x9E => { println!("self.res_3_hl() not implemented! {:x}", opcode); 42 }, // RES 3 (HL)
// 0x9F => { println!("self.res_3_a() not implemented! {:x}", opcode); 42 }, // RES 3 A
// 0xA0 => { println!("self.res_4_b() not implemented! {:x}", opcode); 42 }, // RES 4 B
// 0xA1 => { println!("self.res_4_c() not implemented! {:x}", opcode); 42 }, // RES 4 C
// 0xA2 => { println!("self.res_4_d() not implemented! {:x}", opcode); 42 }, // RES 4 D
// 0xA3 => { println!("self.res_4_e() not implemented! {:x}", opcode); 42 }, // RES 4 E
// 0xA4 => { println!("self.res_4_h() not implemented! {:x}", opcode); 42 }, // RES 4 H
// 0xA5 => { println!("self.res_4_l() not implemented! {:x}", opcode); 42 }, // RES 4 L
// 0xA6 => { println!("self.res_4_hl() not implemented! {:x}", opcode); 42 }, // RES 4 (HL)
// 0xA7 => { println!("self.res_4_a() not implemented! {:x}", opcode); 42 }, // RES 4 A
// 0xA8 => { println!("self.res_5_b() not implemented! {:x}", opcode); 42 }, // RES 5 B
// 0xA9 => { println!("self.res_5_c() not implemented! {:x}", opcode); 42 }, // RES 5 C
// 0xAA => { println!("self.res_5_d() not implemented! {:x}", opcode); 42 }, // RES 5 D
// 0xAB => { println!("self.res_5_e() not implemented! {:x}", opcode); 42 }, // RES 5 E
// 0xAC => { println!("self.res_5_h() not implemented! {:x}", opcode); 42 }, // RES 5 H
// 0xAD => { println!("self.res_5_l() not implemented! {:x}", opcode); 42 }, // RES 5 L
// 0xAE => { println!("self.res_5_hl() not implemented! {:x}", opcode); 42 }, // RES 5 (HL)
// 0xAF => { println!("self.res_5_a() not implemented! {:x}", opcode); 42 }, // RES 5 A
// 0xB0 => { println!("self.res_6_b() not implemented! {:x}", opcode); 42 }, // RES 6 B
// 0xB1 => { println!("self.res_6_c() not implemented! {:x}", opcode); 42 }, // RES 6 C
// 0xB2 => { println!("self.res_6_d() not implemented! {:x}", opcode); 42 }, // RES 6 D
// 0xB3 => { println!("self.res_6_e() not implemented! {:x}", opcode); 42 }, // RES 6 E
// 0xB4 => { println!("self.res_6_h() not implemented! {:x}", opcode); 42 }, // RES 6 H
// 0xB5 => { println!("self.res_6_l() not implemented! {:x}", opcode); 42 }, // RES 6 L
// 0xB6 => { println!("self.res_6_hl() not implemented! {:x}", opcode); 42 }, // RES 6 (HL)
// 0xB7 => { println!("self.res_6_a() not implemented! {:x}", opcode); 42 }, // RES 6 A
// 0xB8 => { println!("self.res_7_b() not implemented! {:x}", opcode); 42 }, // RES 7 B
// 0xB9 => { println!("self.res_7_c() not implemented! {:x}", opcode); 42 }, // RES 7 C
// 0xBA => { println!("self.res_7_d() not implemented! {:x}", opcode); 42 }, // RES 7 D
// 0xBB => { println!("self.res_7_e() not implemented! {:x}", opcode); 42 }, // RES 7 E
// 0xBC => { println!("self.res_7_h() not implemented! {:x}", opcode); 42 }, // RES 7 H
// 0xBD => { println!("self.res_7_l() not implemented! {:x}", opcode); 42 }, // RES 7 L
// 0xBE => { println!("self.res_7_hl() not implemented! {:x}", opcode); 42 }, // RES 7 (HL)
// 0xBF => { println!("self.res_7_a() not implemented! {:x}", opcode); 42 }, // RES 7 A
// 0xC0 => { println!("self.set_0_b() not implemented! {:x}", opcode); 42 }, // SET 0 B
// 0xC1 => { println!("self.set_0_c() not implemented! {:x}", opcode); 42 }, // SET 0 C
// 0xC2 => { println!("self.set_0_d() not implemented! {:x}", opcode); 42 }, // SET 0 D
// 0xC3 => { println!("self.set_0_e() not implemented! {:x}", opcode); 42 }, // SET 0 E
// 0xC4 => { println!("self.set_0_h() not implemented! {:x}", opcode); 42 }, // SET 0 H
// 0xC5 => { println!("self.set_0_l() not implemented! {:x}", opcode); 42 }, // SET 0 L
// 0xC6 => { println!("self.set_0_hl() not implemented! {:x}", opcode); 42 }, // SET 0 (HL)
// 0xC7 => { println!("self.set_0_a() not implemented! {:x}", opcode); 42 }, // SET 0 A
// 0xC8 => { println!("self.set_1_b() not implemented! {:x}", opcode); 42 }, // SET 1 B
// 0xC9 => { println!("self.set_1_c() not implemented! {:x}", opcode); 42 }, // SET 1 C
// 0xCA => { println!("self.set_1_d() not implemented! {:x}", opcode); 42 }, // SET 1 D
// 0xCB => { println!("self.set_1_e() not implemented! {:x}", opcode); 42 }, // SET 1 E
// 0xCC => { println!("self.set_1_h() not implemented! {:x}", opcode); 42 }, // SET 1 H
// 0xCD => { println!("self.set_1_l() not implemented! {:x}", opcode); 42 }, // SET 1 L
// 0xCE => { println!("self.set_1_hl() not implemented! {:x}", opcode); 42 }, // SET 1 (HL)
// 0xCF => { println!("self.set_1_a() not implemented! {:x}", opcode); 42 }, // SET 1 A
// 0xD0 => { println!("self.set_2_b() not implemented! {:x}", opcode); 42 }, // SET 2 B
// 0xD1 => { println!("self.set_2_c() not implemented! {:x}", opcode); 42 }, // SET 2 C
// 0xD2 => { println!("self.set_2_d() not implemented! {:x}", opcode); 42 }, // SET 2 D
// 0xD3 => { println!("self.set_2_e() not implemented! {:x}", opcode); 42 }, // SET 2 E
// 0xD4 => { println!("self.set_2_h() not implemented! {:x}", opcode); 42 }, // SET 2 H
// 0xD5 => { println!("self.set_2_l() not implemented! {:x}", opcode); 42 }, // SET 2 L
// 0xD6 => { println!("self.set_2_hl() not implemented! {:x}", opcode); 42 }, // SET 2 (HL)
// 0xD7 => { println!("self.set_2_a() not implemented! {:x}", opcode); 42 }, // SET 2 A
// 0xD8 => { println!("self.set_3_b() not implemented! {:x}", opcode); 42 }, // SET 3 B
// 0xD9 => { println!("self.set_3_c() not implemented! {:x}", opcode); 42 }, // SET 3 C
// 0xDA => { println!("self.set_3_d() not implemented! {:x}", opcode); 42 }, // SET 3 D
// 0xDB => { println!("self.set_3_e() not implemented! {:x}", opcode); 42 }, // SET 3 E
// 0xDC => { println!("self.set_3_h() not implemented! {:x}", opcode); 42 }, // SET 3 H
// 0xDD => { println!("self.set_3_l() not implemented! {:x}", opcode); 42 }, // SET 3 L
// 0xDE => { println!("self.set_3_hl() not implemented! {:x}", opcode); 42 }, // SET 3 (HL)
// 0xDF => { println!("self.set_3_a() not implemented! {:x}", opcode); 42 }, // SET 3 A
// 0xE0 => { println!("self.set_4_b() not implemented! {:x}", opcode); 42 }, // SET 4 B
// 0xE1 => { println!("self.set_4_c() not implemented! {:x}", opcode); 42 }, // SET 4 C
// 0xE2 => { println!("self.set_4_d() not implemented! {:x}", opcode); 42 }, // SET 4 D
// 0xE3 => { println!("self.set_4_e() not implemented! {:x}", opcode); 42 }, // SET 4 E
// 0xE4 => { println!("self.set_4_h() not implemented! {:x}", opcode); 42 }, // SET 4 H
// 0xE5 => { println!("self.set_4_l() not implemented! {:x}", opcode); 42 }, // SET 4 L
// 0xE6 => { println!("self.set_4_hl() not implemented! {:x}", opcode); 42 }, // SET 4 (HL)
// 0xE7 => { println!("self.set_4_a() not implemented! {:x}", opcode); 42 }, // SET 4 A
// 0xE8 => { println!("self.set_5_b() not implemented! {:x}", opcode); 42 }, // SET 5 B
// 0xE9 => { println!("self.set_5_c() not implemented! {:x}", opcode); 42 }, // SET 5 C
// 0xEA => { println!("self.set_5_d() not implemented! {:x}", opcode); 42 }, // SET 5 D
// 0xEB => { println!("self.set_5_e() not implemented! {:x}", opcode); 42 }, // SET 5 E
// 0xEC => { println!("self.set_5_h() not implemented! {:x}", opcode); 42 }, // SET 5 H
// 0xED => { println!("self.set_5_l() not implemented! {:x}", opcode); 42 }, // SET 5 L
// 0xEE => { println!("self.set_5_hl() not implemented! {:x}", opcode); 42 }, // SET 5 (HL)
// 0xEF => { println!("self.set_5_a() not implemented! {:x}", opcode); 42 }, // SET 5 A
// 0xF0 => { println!("self.set_6_b() not implemented! {:x}", opcode); 42 }, // SET 6 B
// 0xF1 => { println!("self.set_6_c() not implemented! {:x}", opcode); 42 }, // SET 6 C
// 0xF2 => { println!("self.set_6_d() not implemented! {:x}", opcode); 42 }, // SET 6 D
// 0xF3 => { println!("self.set_6_e() not implemented! {:x}", opcode); 42 }, // SET 6 E
// 0xF4 => { println!("self.set_6_h() not implemented! {:x}", opcode); 42 }, // SET 6 H
// 0xF5 => { println!("self.set_6_l() not implemented! {:x}", opcode); 42 }, // SET 6 L
// 0xF6 => { println!("self.set_6_hl() not implemented! {:x}", opcode); 42 }, // SET 6 (HL)
// 0xF7 => { println!("self.set_6_a() not implemented! {:x}", opcode); 42 }, // SET 6 A
// 0xF8 => { println!("self.set_7_b() not implemented! {:x}", opcode); 42 }, // SET 7 B
// 0xF9 => { println!("self.set_7_c() not implemented! {:x}", opcode); 42 }, // SET 7 C
// 0xFA => { println!("self.set_7_d() not implemented! {:x}", opcode); 42 }, // SET 7 D
// 0xFB => { println!("self.set_7_e() not implemented! {:x}", opcode); 42 }, // SET 7 E
// 0xFC => { println!("self.set_7_h() not implemented! {:x}", opcode); 42 }, // SET 7 H
// 0xFD => { println!("self.set_7_l() not implemented! {:x}", opcode); 42 }, // SET 7 L
// 0xFE => { println!("self.set_7_hl() not implemented! {:x}", opcode); 42 }, // SET 7 (HL)
// 0xFF => { println!("self.set_7_a() not implemented! {:x}", opcode); 42 }, // SET 7 A
