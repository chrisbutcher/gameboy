use std::fmt;

pub use super::types;
pub use super::mmu;
pub use super::cartridge;
pub mod opcodes;

const FLAG_ZERO: types::Byte = 0x80; // Zero
const FLAG_NEGATIVE: types::Byte = 0x40; // Negative, aka. FLAG_SUB
const FLAG_HALF_CARRY: types::Byte = 0x20; // Half-carry
const FLAG_CARRY: types::Byte = 0x10; // Carry
const FLAG_NONE: types::Byte = 0x0; // None

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

    println!("[Program counter] {:x} Executing opcode {:x}", self.PC, opcode);
    println!(
      "[Registers] A:{:x}, F:{:x}, B:{:x}, C:{:x}, D:{:x}, E:{:x}, H:{:x}, L:{:x}",
      self.AF.read_hi(), self.AF.read_lo(), self.BC.read_hi(), self.BC.read_lo(), self.DE.read_hi(), self.DE.read_lo(), self.HL.read_hi(), self.HL.read_lo()
    );
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
      0x00 => { self.nop(); 4}, // NOP
      0x01 => { panic!("self.ld_bc_nn() not implemented! {:x}", opcode); 42 }, // LD BC,nn
      0x02 => { panic!("self.ld_bc_a() not implemented! {:x}", opcode); 42 }, // LD (BC),A
      0x03 => { panic!("self.inc_bc() not implemented! {:x}", opcode); 42 }, // INC BC
      0x04 => { panic!("self.inc_b() not implemented! {:x}", opcode); 42 }, // INC B
      0x05 => { panic!("self.dec_b() not implemented! {:x}", opcode); 42 }, // DEC B
      0x06 => { self.ld_b_n(mmu); 8 }, // LD B,n
      0x07 => { panic!("self.rlca() not implemented! {:x}", opcode); 42 }, // RLCA
      0x08 => { panic!("self.ld_nn_sp() not implemented! {:x}", opcode); 42 }, // LD (nn),SP
      0x09 => { panic!("self.add_hl_bc() not implemented! {:x}", opcode); 42 }, // ADD HL,BC
      0x0A => { panic!("self.ld_a_bc() not implemented! {:x}", opcode); 42 }, // LD A,(BC)
      0x0B => { panic!("self.dec_bc() not implemented! {:x}", opcode); 42 }, // DEC BC
      0x0C => { panic!("self.inc_c() not implemented! {:x}", opcode); 42 }, // INC C
      0x0D => { panic!("self.dec_c() not implemented! {:x}", opcode); 42 }, // DEC C
      0x0E => { self.ld_c_n(mmu); 8 }, // LD C,n
      0x0F => { panic!("self.rrca() not implemented! {:x}", opcode); 42 }, // RRCA
      0x10 => { panic!("self.stop() not implemented! {:x}", opcode); 42 }, // STOP
      0x11 => { panic!("self.ld_de_nn() not implemented! {:x}", opcode); 42 }, // LD DE,nn
      0x12 => { panic!("self.ld_de_a() not implemented! {:x}", opcode); 42 }, // LD (DE),A
      0x13 => { panic!("self.inc_de() not implemented! {:x}", opcode); 42 }, // INC DE
      0x14 => { panic!("self.inc_d() not implemented! {:x}", opcode); 42 }, // INC D
      0x15 => { panic!("self.dec_d() not implemented! {:x}", opcode); 42 }, // DEC D
      0x16 => { panic!("self.ld_d_n() not implemented! {:x}", opcode); 42 }, // LD D,n
      0x17 => { panic!("self.rla() not implemented! {:x}", opcode); 42 }, // RLA
      0x18 => { panic!("self.jr_n() not implemented! {:x}", opcode); 42 }, // JR n
      0x19 => { panic!("self.add_hl_de() not implemented! {:x}", opcode); 42 }, // ADD HL,DE
      0x1A => { panic!("self.ld_a_de() not implemented! {:x}", opcode); 42 }, // LD A,(DE)
      0x1B => { panic!("self.dec_de() not implemented! {:x}", opcode); 42 }, // DEC DE
      0x1C => { panic!("self.inc_e() not implemented! {:x}", opcode); 42 }, // INC E
      0x1D => { panic!("self.dec_e() not implemented! {:x}", opcode); 42 }, // DEC E
      0x1E => { panic!("self.ld_e_n() not implemented! {:x}", opcode); 42 }, // LD E,n
      0x1F => { panic!("self.rra() not implemented! {:x}", opcode); 42 }, // RRA
      0x20 => { panic!("self.jr_nz_n() not implemented! {:x}", opcode); 42 }, // JR NZ,n
      0x21 => { self.ld_hl_nn(mmu); 12 }, // LD HL,nn
      0x22 => { panic!("self.ld_hli_a() not implemented! {:x}", opcode); 42 }, // LD (HLI),A
      0x23 => { panic!("self.inc_hl() not implemented! {:x}", opcode); 42 }, // INC HL
      0x24 => { panic!("self.inc_h() not implemented! {:x}", opcode); 42 }, // INC H
      0x25 => { panic!("self.dec_h() not implemented! {:x}", opcode); 42 }, // DEC H
      0x26 => { panic!("self.ld_h_n() not implemented! {:x}", opcode); 42 }, // LD H,n
      0x27 => { panic!("self.daa() not implemented! {:x}", opcode); 42 }, // DAA
      0x28 => { panic!("self.jr_z_n() not implemented! {:x}", opcode); 42 }, // JR Z,n
      0x29 => { panic!("self.add_hl_hl() not implemented! {:x}", opcode); 42 }, // ADD HL,HL
      0x2A => { panic!("self.ld_a_hli() not implemented! {:x}", opcode); 42 }, // LD A,(HLI)
      0x2B => { panic!("self.dec_hl() not implemented! {:x}", opcode); 42 }, // DEC HL
      0x2C => { panic!("self.inc_l() not implemented! {:x}", opcode); 42 }, // INC L
      0x2D => { panic!("self.dec_l() not implemented! {:x}", opcode); 42 }, // DEC L
      0x2E => { panic!("self.ld_l_n() not implemented! {:x}", opcode); 42 }, // LD L,n
      0x2F => { panic!("self.cpl() not implemented! {:x}", opcode); 42 }, // CPL
      0x30 => { panic!("self.jr_nc_n() not implemented! {:x}", opcode); 42 }, // JR NC,n
      0x31 => { panic!("self.ld_sp_nn() not implemented! {:x}", opcode); 42 }, // LD SP,nn
      0x32 => { panic!("self.ld_hld__a() not implemented! {:x}", opcode); 42 }, // LD (HLD), A
      0x33 => { panic!("self.inc_sp() not implemented! {:x}", opcode); 42 }, // INC SP
      0x34 => { panic!("self.inc_hl() not implemented! {:x}", opcode); 42 }, // INC (HL)
      0x35 => { panic!("self.dec_hl() not implemented! {:x}", opcode); 42 }, // DEC (HL)
      0x36 => { panic!("self.ld_hl_n() not implemented! {:x}", opcode); 42 }, // LD (HL),n
      0x37 => { panic!("self.scf() not implemented! {:x}", opcode); 42 }, // SCF
      0x38 => { panic!("self.jr_c_n() not implemented! {:x}", opcode); 42 }, // JR C,n
      0x39 => { panic!("self.add_hl_sp() not implemented! {:x}", opcode); 42 }, // ADD HL,SP
      0x3A => { panic!("self.ld_a_hld() not implemented! {:x}", opcode); 42 }, // LD A,(HLD)
      0x3B => { panic!("self.dec_sp() not implemented! {:x}", opcode); 42 }, // DEC SP
      0x3C => { panic!("self.inc_a() not implemented! {:x}", opcode); 42 }, // INC A
      0x3D => { panic!("self.dec_a() not implemented! {:x}", opcode); 42 }, // DEC A
      0x3E => { panic!("self.ld_a_n() not implemented! {:x}", opcode); 42 }, // LD A,n
      0x3F => { panic!("self.ccf() not implemented! {:x}", opcode); 42 }, // CCF
      0x40 => { panic!("self.ld_b_b() not implemented! {:x}", opcode); 42 }, // LD B,B
      0x41 => { panic!("self.ld_b_c() not implemented! {:x}", opcode); 42 }, // LD B,C
      0x42 => { panic!("self.ld_b_d() not implemented! {:x}", opcode); 42 }, // LD B,D
      0x43 => { panic!("self.ld_b_e() not implemented! {:x}", opcode); 42 }, // LD B,E
      0x44 => { panic!("self.ld_b_h() not implemented! {:x}", opcode); 42 }, // LD B,H
      0x45 => { panic!("self.ld_b_l() not implemented! {:x}", opcode); 42 }, // LD B,L
      0x46 => { panic!("self.ld_b_hl() not implemented! {:x}", opcode); 42 }, // LD B,(HL)
      0x47 => { panic!("self.ld_b_a() not implemented! {:x}", opcode); 42 }, // LD B,A
      0x48 => { panic!("self.ld_c_b() not implemented! {:x}", opcode); 42 }, // LD C,B
      0x49 => { panic!("self.ld_c_c() not implemented! {:x}", opcode); 42 }, // LD C,C
      0x4A => { panic!("self.ld_c_d() not implemented! {:x}", opcode); 42 }, // LD C,D
      0x4B => { panic!("self.ld_c_e() not implemented! {:x}", opcode); 42 }, // LD C,E
      0x4C => { panic!("self.ld_c_h() not implemented! {:x}", opcode); 42 }, // LD C,H
      0x4D => { panic!("self.ld_c_l() not implemented! {:x}", opcode); 42 }, // LD C,L
      0x4E => { panic!("self.ld_c_hl() not implemented! {:x}", opcode); 42 }, // LD C,(HL)
      0x4F => { panic!("self.ld_c_a() not implemented! {:x}", opcode); 42 }, // LD C,A
      0x50 => { panic!("self.ld_d_b() not implemented! {:x}", opcode); 42 }, // LD D,B
      0x51 => { panic!("self.ld_d_c() not implemented! {:x}", opcode); 42 }, // LD D,C
      0x52 => { panic!("self.ld_d_d() not implemented! {:x}", opcode); 42 }, // LD D,D
      0x53 => { panic!("self.ld_d_e() not implemented! {:x}", opcode); 42 }, // LD D,E
      0x54 => { panic!("self.ld_d_h() not implemented! {:x}", opcode); 42 }, // LD D,H
      0x55 => { panic!("self.ld_d_l() not implemented! {:x}", opcode); 42 }, // LD D,L
      0x56 => { panic!("self.ld_d_hl() not implemented! {:x}", opcode); 42 }, // LD D,(HL)
      0x57 => { panic!("self.ld_d_a() not implemented! {:x}", opcode); 42 }, // LD D,A
      0x58 => { panic!("self.ld_e_b() not implemented! {:x}", opcode); 42 }, // LD E,B
      0x59 => { panic!("self.ld_e_c() not implemented! {:x}", opcode); 42 }, // LD E,C
      0x5A => { panic!("self.ld_e_d() not implemented! {:x}", opcode); 42 }, // LD E,D
      0x5B => { panic!("self.ld_e_e() not implemented! {:x}", opcode); 42 }, // LD E,E
      0x5C => { panic!("self.ld_e_h() not implemented! {:x}", opcode); 42 }, // LD E,H
      0x5D => { panic!("self.ld_e_l() not implemented! {:x}", opcode); 42 }, // LD E,L
      0x5E => { panic!("self.ld_e_hl() not implemented! {:x}", opcode); 42 }, // LD E,(HL)
      0x5F => { panic!("self.ld_e_a() not implemented! {:x}", opcode); 42 }, // LD E,A
      0x60 => { panic!("self.ld_h_b() not implemented! {:x}", opcode); 42 }, // LD H,B
      0x61 => { panic!("self.ld_h_c() not implemented! {:x}", opcode); 42 }, // LD H,C
      0x62 => { panic!("self.ld_h_d() not implemented! {:x}", opcode); 42 }, // LD H,D
      0x63 => { panic!("self.ld_h_e() not implemented! {:x}", opcode); 42 }, // LD H,E
      0x64 => { panic!("self.ld_h_h() not implemented! {:x}", opcode); 42 }, // LD H,H
      0x65 => { panic!("self.ld_h_l() not implemented! {:x}", opcode); 42 }, // LD H,L
      0x66 => { panic!("self.ld_h_hl() not implemented! {:x}", opcode); 42 }, // LD H,(HL)
      0x67 => { panic!("self.ld_h_a() not implemented! {:x}", opcode); 42 }, // LD H,A
      0x68 => { panic!("self.ld_l_b() not implemented! {:x}", opcode); 42 }, // LD L,B
      0x69 => { panic!("self.ld_l_c() not implemented! {:x}", opcode); 42 }, // LD L,C
      0x6A => { panic!("self.ld_l_d() not implemented! {:x}", opcode); 42 }, // LD L,D
      0x6B => { panic!("self.ld_l_e() not implemented! {:x}", opcode); 42 }, // LD L,E
      0x6C => { panic!("self.ld_l_h() not implemented! {:x}", opcode); 42 }, // LD L,H
      0x6D => { panic!("self.ld_l_l() not implemented! {:x}", opcode); 42 }, // LD L,L
      0x6E => { panic!("self.ld_l_hl() not implemented! {:x}", opcode); 42 }, // LD L,(HL)
      0x6F => { panic!("self.ld_l_a() not implemented! {:x}", opcode); 42 }, // LD L,A
      0x70 => { panic!("self.ld_hl_b() not implemented! {:x}", opcode); 42 }, // LD (HL),B
      0x71 => { panic!("self.ld_hl_c() not implemented! {:x}", opcode); 42 }, // LD (HL),C
      0x72 => { panic!("self.ld_hl_d() not implemented! {:x}", opcode); 42 }, // LD (HL),D
      0x73 => { panic!("self.ld_hl_e() not implemented! {:x}", opcode); 42 }, // LD (HL),E
      0x74 => { panic!("self.ld_hl_h() not implemented! {:x}", opcode); 42 }, // LD (HL),H
      0x75 => { panic!("self.ld_hl_l() not implemented! {:x}", opcode); 42 }, // LD (HL),L
      0x76 => { panic!("self.halt() not implemented! {:x}", opcode); 42 }, // HALT
      0x77 => { panic!("self.ld_hl_a() not implemented! {:x}", opcode); 42 }, // LD (HL),A
      0x78 => { panic!("self.ld_a_b() not implemented! {:x}", opcode); 42 }, // LD A,B
      0x79 => { panic!("self.ld_a_c() not implemented! {:x}", opcode); 42 }, // LD A,C
      0x7A => { panic!("self.ld_a_d() not implemented! {:x}", opcode); 42 }, // LD A,D
      0x7B => { panic!("self.ld_a_e() not implemented! {:x}", opcode); 42 }, // LD A,E
      0x7C => { panic!("self.ld_a_h() not implemented! {:x}", opcode); 42 }, // LD A,H
      0x7D => { panic!("self.ld_a_l() not implemented! {:x}", opcode); 42 }, // LD A,L
      0x7E => { panic!("self.ld_a_hl() not implemented! {:x}", opcode); 42 }, // LD A,(HL)
      0x7F => { panic!("self.ld_a_a() not implemented! {:x}", opcode); 42 }, // LD A,A
      0x80 => { panic!("self.add_a_b() not implemented! {:x}", opcode); 42 }, // ADD A,B
      0x81 => { panic!("self.add_a_c() not implemented! {:x}", opcode); 42 }, // ADD A,C
      0x82 => { panic!("self.add_a_d() not implemented! {:x}", opcode); 42 }, // ADD A,D
      0x83 => { panic!("self.add_a_e() not implemented! {:x}", opcode); 42 }, // ADD A,E
      0x84 => { panic!("self.add_a_h() not implemented! {:x}", opcode); 42 }, // ADD A,H
      0x85 => { panic!("self.add_a_l() not implemented! {:x}", opcode); 42 }, // ADD A,L
      0x86 => { panic!("self.add_a_hl() not implemented! {:x}", opcode); 42 }, // ADD A,(HL)
      0x87 => { panic!("self.add_a_a() not implemented! {:x}", opcode); 42 }, // ADD A,A
      0x88 => { panic!("self.adc_a_b() not implemented! {:x}", opcode); 42 }, // ADC A,B
      0x89 => { panic!("self.adc_a_c() not implemented! {:x}", opcode); 42 }, // ADC A,C
      0x8A => { panic!("self.adc_a_d() not implemented! {:x}", opcode); 42 }, // ADC A,D
      0x8B => { panic!("self.adc_a_e() not implemented! {:x}", opcode); 42 }, // ADC A,E
      0x8C => { panic!("self.adc_a_h() not implemented! {:x}", opcode); 42 }, // ADC A,H
      0x8D => { panic!("self.adc_a_l() not implemented! {:x}", opcode); 42 }, // ADC A,L
      0x8E => { panic!("self.adc_a_hl() not implemented! {:x}", opcode); 42 }, // ADC A,(HL)
      0x8F => { panic!("self.adc_a_a() not implemented! {:x}", opcode); 42 }, // ADC A,A
      0x90 => { panic!("self.sub_b() not implemented! {:x}", opcode); 42 }, // SUB B
      0x91 => { panic!("self.sub_c() not implemented! {:x}", opcode); 42 }, // SUB C
      0x92 => { panic!("self.sub_d() not implemented! {:x}", opcode); 42 }, // SUB D
      0x93 => { panic!("self.sub_e() not implemented! {:x}", opcode); 42 }, // SUB E
      0x94 => { panic!("self.sub_h() not implemented! {:x}", opcode); 42 }, // SUB H
      0x95 => { panic!("self.sub_l() not implemented! {:x}", opcode); 42 }, // SUB L
      0x96 => { panic!("self.sub_hl() not implemented! {:x}", opcode); 42 }, // SUB (HL)
      0x97 => { panic!("self.sub_a() not implemented! {:x}", opcode); 42 }, // SUB A
      0x98 => { panic!("self.sbc_b() not implemented! {:x}", opcode); 42 }, // SBC B
      0x99 => { panic!("self.sbc_c() not implemented! {:x}", opcode); 42 }, // SBC C
      0x9A => { panic!("self.sbc_d() not implemented! {:x}", opcode); 42 }, // SBC D
      0x9B => { panic!("self.sbc_e() not implemented! {:x}", opcode); 42 }, // SBC E
      0x9C => { panic!("self.sbc_h() not implemented! {:x}", opcode); 42 }, // SBC H
      0x9D => { panic!("self.sbc_l() not implemented! {:x}", opcode); 42 }, // SBC L
      0x9E => { panic!("self.sbc_hl() not implemented! {:x}", opcode); 42 }, // SBC (HL)
      0x9F => { panic!("self.sbc_a() not implemented! {:x}", opcode); 42 }, // SBC A
      0xA0 => { panic!("self.and_b() not implemented! {:x}", opcode); 42 }, // AND B
      0xA1 => { panic!("self.and_c() not implemented! {:x}", opcode); 42 }, // AND C
      0xA2 => { panic!("self.and_d() not implemented! {:x}", opcode); 42 }, // AND D
      0xA3 => { panic!("self.and_e() not implemented! {:x}", opcode); 42 }, // AND E
      0xA4 => { panic!("self.and_h() not implemented! {:x}", opcode); 42 }, // AND H
      0xA5 => { panic!("self.and_l() not implemented! {:x}", opcode); 42 }, // AND L
      0xA6 => { panic!("self.and_hl() not implemented! {:x}", opcode); 42 }, // AND (HL)
      0xA7 => { panic!("self.and_a() not implemented! {:x}", opcode); 42 }, // AND A
      0xA8 => { self.xor_b(mmu); 4 }, // XOR B
      0xA9 => { panic!("self.xor_c() not implemented! {:x}", opcode); 42 }, // XOR C
      0xAA => { panic!("self.xor_d() not implemented! {:x}", opcode); 42 }, // XOR D
      0xAB => { panic!("self.xor_e() not implemented! {:x}", opcode); 42 }, // XOR E
      0xAC => { panic!("self.xor_h() not implemented! {:x}", opcode); 42 }, // XOR H
      0xAD => { panic!("self.xor_l() not implemented! {:x}", opcode); 42 }, // XOR L
      0xAE => { panic!("self.xor_hl() not implemented! {:x}", opcode); 42 }, // XOR (HL)
      0xAF => { self.xor_a(mmu); 4 }, // XOR A
      0xB0 => { panic!("self.or_b() not implemented! {:x}", opcode); 42 }, // OR B
      0xB1 => { panic!("self.or_c() not implemented! {:x}", opcode); 42 }, // OR C
      0xB2 => { panic!("self.or_d() not implemented! {:x}", opcode); 42 }, // OR D
      0xB3 => { panic!("self.or_e() not implemented! {:x}", opcode); 42 }, // OR E
      0xB4 => { panic!("self.or_h() not implemented! {:x}", opcode); 42 }, // OR H
      0xB5 => { panic!("self.or_l() not implemented! {:x}", opcode); 42 }, // OR L
      0xB6 => { panic!("self.or_hl() not implemented! {:x}", opcode); 42 }, // OR (HL)
      0xB7 => { panic!("self.or_a() not implemented! {:x}", opcode); 42 }, // OR A
      0xB8 => { panic!("self.cp_b() not implemented! {:x}", opcode); 42 }, // CP B
      0xB9 => { panic!("self.cp_c() not implemented! {:x}", opcode); 42 }, // CP C
      0xBA => { panic!("self.cp_d() not implemented! {:x}", opcode); 42 }, // CP D
      0xBB => { panic!("self.cp_e() not implemented! {:x}", opcode); 42 }, // CP E
      0xBC => { panic!("self.cp_h() not implemented! {:x}", opcode); 42 }, // CP H
      0xBD => { panic!("self.cp_l() not implemented! {:x}", opcode); 42 }, // CP L
      0xBE => { panic!("self.cp_hl() not implemented! {:x}", opcode); 42 }, // CP (HL)
      0xBF => { panic!("self.cp_a() not implemented! {:x}", opcode); 42 }, // CP A
      0xC0 => { panic!("self.ret_nz() not implemented! {:x}", opcode); 42 }, // RET NZ
      0xC1 => { panic!("self.pop_bc() not implemented! {:x}", opcode); 42 }, // POP BC
      0xC2 => { panic!("self.jp_nz_nn() not implemented! {:x}", opcode); 42 }, // JP NZ,nn
      0xC3 => { self.jp_nn(mmu); 12 }, // JP nn
      0xC4 => { panic!("self.call_nz_nn() not implemented! {:x}", opcode); 42 }, // CALL NZ,nn
      0xC5 => { panic!("self.push_bc() not implemented! {:x}", opcode); 42 }, // PUSH BC
      0xC6 => { panic!("self.add_a_n() not implemented! {:x}", opcode); 42 }, // ADD A,n
      0xC7 => { panic!("self.rst_00h() not implemented! {:x}", opcode); 42 }, // RST 00H
      0xC8 => { panic!("self.ret_z() not implemented! {:x}", opcode); 42 }, // RET Z
      0xC9 => { panic!("self.ret() not implemented! {:x}", opcode); 42 }, // RET
      0xCA => { panic!("self.jp_z_nn() not implemented! {:x}", opcode); 42 }, // JP Z,nn
      0xCB => { panic!("self.cb_prefixed_instruction() not implemented! {:x}", opcode); 42 }, // CB prefixed instruction
      0xCC => { panic!("self.call_z_nn() not implemented! {:x}", opcode); 42 }, // CALL Z,nn
      0xCD => { panic!("self.call_nn() not implemented! {:x}", opcode); 42 }, // CALL nn
      0xCE => { panic!("self.adc_a_n() not implemented! {:x}", opcode); 42 }, // ADC A,n
      0xCF => { panic!("self.rst_08h() not implemented! {:x}", opcode); 42 }, // RST 08H
      0xD0 => { panic!("self.ret_nc() not implemented! {:x}", opcode); 42 }, // RET NC
      0xD1 => { panic!("self.pop_de() not implemented! {:x}", opcode); 42 }, // POP DE
      0xD2 => { panic!("self.jp_nc_nn() not implemented! {:x}", opcode); 42 }, // JP NC,nn
      0xD4 => { panic!("self.call_nc_nn() not implemented! {:x}", opcode); 42 }, // CALL NC,nn
      0xD5 => { panic!("self.push_de() not implemented! {:x}", opcode); 42 }, // PUSH DE
      0xD6 => { panic!("self.sub_n() not implemented! {:x}", opcode); 42 }, // SUB n
      0xD7 => { panic!("self.rst_10h() not implemented! {:x}", opcode); 42 }, // RST 10H
      0xD8 => { panic!("self.ret_c() not implemented! {:x}", opcode); 42 }, // RET C
      0xD9 => { panic!("self.reti() not implemented! {:x}", opcode); 42 }, // RETI
      0xDA => { panic!("self.jp_c_nn() not implemented! {:x}", opcode); 42 }, // JP C,nn
      0xDC => { panic!("self.call_c_nn() not implemented! {:x}", opcode); 42 }, // CALL C,nn
      0xDE => { panic!("self.sbc_n() not implemented! {:x}", opcode); 42 }, // SBC n
      0xDF => { panic!("self.rst_18h() not implemented! {:x}", opcode); 42 }, // RST 18H
      0xE0 => { panic!("self.ld_0xff00_plus_n_a() not implemented! {:x}", opcode); 42 }, // LD (0xFF00+n),A
      0xE1 => { panic!("self.pop_hl() not implemented! {:x}", opcode); 42 }, // POP HL
      0xE2 => { panic!("self.ld_0xff00_plus_c_a() not implemented! {:x}", opcode); 42 }, // LD (0xFF00+C),A
      0xE5 => { panic!("self.push_hl() not implemented! {:x}", opcode); 42 }, // PUSH HL
      0xE6 => { panic!("self.and_n() not implemented! {:x}", opcode); 42 }, // AND n
      0xE7 => { panic!("self.rst_20h() not implemented! {:x}", opcode); 42 }, // RST 20H
      0xE8 => { panic!("self.add_sp_n() not implemented! {:x}", opcode); 42 }, // ADD SP,n
      0xE9 => { panic!("self.jp_hl() not implemented! {:x}", opcode); 42 }, // JP (HL)
      0xEA => { panic!("self.ld_nn_a() not implemented! {:x}", opcode); 42 }, // LD (nn),A
      0xEE => { panic!("self.xor_n() not implemented! {:x}", opcode); 42 }, // XOR n
      0xEF => { panic!("self.rst_28h() not implemented! {:x}", opcode); 42 }, // RST 28H
      0xF0 => { panic!("self.ld_a_0xff00_plus_n() not implemented! {:x}", opcode); 42 }, // LD A,(0xFF00+n)
      0xF1 => { panic!("self.pop_af() not implemented! {:x}", opcode); 42 }, // POP AF
      0xF2 => { panic!("self.ld_a_c() not implemented! {:x}", opcode); 42 }, // LD A,(C)
      0xF3 => { panic!("self.di() not implemented! {:x}", opcode); 42 }, // DI
      0xF5 => { panic!("self.push_af() not implemented! {:x}", opcode); 42 }, // PUSH AF
      0xF6 => { panic!("self.or_n() not implemented! {:x}", opcode); 42 }, // OR n
      0xF7 => { panic!("self.rst_30h() not implemented! {:x}", opcode); 42 }, // RST 30H
      0xF8 => { panic!("self.ld_hl_sp_plus_n() not implemented! {:x}", opcode); 42 }, // LD HL,SP+n
      0xF9 => { panic!("self.ld_sp_hl() not implemented! {:x}", opcode); 42 }, // LD SP,HL
      0xFA => { panic!("self.ld_a_nn() not implemented! {:x}", opcode); 42 }, // LD A,(nn)
      0xFB => { panic!("self.ei() not implemented! {:x}", opcode); 42 }, // EI
      0xFE => { panic!("self.cp_n() not implemented! {:x}", opcode); 42 }, // CP n
      0xFF => { panic!("self.rst_38h() not implemented! {:x}", opcode); 42 }, // RST 38H
      _ => panic!("Unexpected opcode: {:x}", opcode)
    }
    // Ok(cycles)
  }

  // Opcodes

  fn nop(&self) {
  }

  fn jp_nn(&mut self, mmu: &mmu::MMU) {
    self.PC = mmu.read_word(self.PC);
  }

  fn xor_a(&mut self, mmu: &mmu::MMU) {
    let A = self.AF.read_hi();
    self.util_xor_n(A);
  }

  fn xor_b(&mut self, mmu: &mmu::MMU) {
    let B = self.AF.read_hi();
    self.util_xor_n(B);
  }

  fn ld_hl_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.HL.write(value);

    self.PC += 2;
  }

  fn ld_c_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    self.BC.write_lo(value);
    self.PC += 1;
  }

  fn ld_b_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    self.BC.write_hi(value);
    self.PC += 1;
  }

  // Helpers

  fn util_xor_n(&mut self, byte: types::Byte)
  {
      let result = self.AF.read_hi() ^ byte;
      self.AF.write_hi(result);
      self.util_clear_all_flags();
      self.util_toggle_zero_flag_from_result(result);
  }

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

  fn util_is_flag_set(&mut self, byte: types::Byte) -> bool {
    (self.AF.read_lo() & byte) != 0
  }

  fn util_toggle_flag(&mut self, byte: types::Byte) {
    let previous_flags = self.AF.read_lo();
    self.AF.write_lo(previous_flags | byte);
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
  //     self.set_flag(FLAG_ZERO);
  //   } else {
  //     self.reset_flag(FLAG_ZERO);
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
  //     self.set_flag(FLAG_ZERO);
  //   }
  //
  //   self.set_flag(FLAG_NEGATIVE);
  //
  //   if (before & 0x0f) == 0 {
  //     self.set_flag(FLAG_HALF_CARRY);
  //   } else {
  //     self.reset_flag(FLAG_HALF_CARRY);
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
}

// #[test]
// fn set_flags() {
//   let mut cpu = CPU::new();
//   assert_eq!(false, cpu.util_is_flag_set(FLAG_ZERO));
//   cpu.set_flag(FLAG_ZERO);
//   assert_eq!(true, cpu.util_is_flag_set(FLAG_ZERO));
// }
//
// #[test]
// fn register_setting() {
//   let mut register = Register::new();
//   register.write(0xAABB);
//   assert_eq!(0xAA, register.read_hi());
//   assert_eq!(0xBB, register.read_lo());
//   assert_eq!(0xAABB, register.read());
//
//   register.write_lo(0xFF);
//   assert_eq!(0xAA, register.read_hi());
//   assert_eq!(0xFF, register.read_lo());
//   assert_eq!(0xAAFF, register.read());
//
//   register.write_hi(0xDD);
//   assert_eq!(0xDD, register.read_hi());
//   assert_eq!(0xFF, register.read_lo());
//   assert_eq!(0xDDFF, register.read());
// }
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
  assert!(!cpu.util_is_flag_set(FLAG_NEGATIVE));
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
  assert!(!cpu.util_is_flag_set(FLAG_NEGATIVE));
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
//   assert!(cpu.util_is_flag_set(FLAG_NEGATIVE));
//   assert!(cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }

// Unused cb prefixed opcodes
// 0x00 => { panic!("self.rlc_b() not implemented! {:x}", opcode); 42 }, // RLC B
// 0x01 => { panic!("self.rlc_c() not implemented! {:x}", opcode); 42 }, // RLC C
// 0x02 => { panic!("self.rlc_d() not implemented! {:x}", opcode); 42 }, // RLC D
// 0x03 => { panic!("self.rlc_e() not implemented! {:x}", opcode); 42 }, // RLC E
// 0x04 => { panic!("self.rlc_h() not implemented! {:x}", opcode); 42 }, // RLC H
// 0x05 => { panic!("self.rlc_l() not implemented! {:x}", opcode); 42 }, // RLC L
// 0x06 => { panic!("self.rlc_hl() not implemented! {:x}", opcode); 42 }, // RLC (HL)
// 0x07 => { panic!("self.rlc_a() not implemented! {:x}", opcode); 42 }, // RLC A
// 0x08 => { panic!("self.rrc_b() not implemented! {:x}", opcode); 42 }, // RRC B
// 0x09 => { panic!("self.rrc_c() not implemented! {:x}", opcode); 42 }, // RRC C
// 0x0A => { panic!("self.rrc_d() not implemented! {:x}", opcode); 42 }, // RRC D
// 0x0B => { panic!("self.rrc_e() not implemented! {:x}", opcode); 42 }, // RRC E
// 0x0C => { panic!("self.rrc_h() not implemented! {:x}", opcode); 42 }, // RRC H
// 0x0D => { panic!("self.rrc_l() not implemented! {:x}", opcode); 42 }, // RRC L
// 0x0E => { panic!("self.rrc_hl() not implemented! {:x}", opcode); 42 }, // RRC (HL)
// 0x0F => { panic!("self.rrc_a() not implemented! {:x}", opcode); 42 }, // RRC A
// 0x10 => { panic!("self.rl_b() not implemented! {:x}", opcode); 42 }, // RL B
// 0x11 => { panic!("self.rl_c() not implemented! {:x}", opcode); 42 }, // RL C
// 0x12 => { panic!("self.rl_d() not implemented! {:x}", opcode); 42 }, // RL D
// 0x13 => { panic!("self.rl_e() not implemented! {:x}", opcode); 42 }, // RL E
// 0x14 => { panic!("self.rl_h() not implemented! {:x}", opcode); 42 }, // RL H
// 0x15 => { panic!("self.rl_l() not implemented! {:x}", opcode); 42 }, // RL L
// 0x16 => { panic!("self.rl_hl() not implemented! {:x}", opcode); 42 }, // RL (HL)
// 0x17 => { panic!("self.rl_a() not implemented! {:x}", opcode); 42 }, // RL A
// 0x18 => { panic!("self.rr_b() not implemented! {:x}", opcode); 42 }, // RR B
// 0x19 => { panic!("self.rr_c() not implemented! {:x}", opcode); 42 }, // RR C
// 0x1A => { panic!("self.rr_d() not implemented! {:x}", opcode); 42 }, // RR D
// 0x1B => { panic!("self.rr_e() not implemented! {:x}", opcode); 42 }, // RR E
// 0x1C => { panic!("self.rr_h() not implemented! {:x}", opcode); 42 }, // RR H
// 0x1D => { panic!("self.rr_l() not implemented! {:x}", opcode); 42 }, // RR L
// 0x1E => { panic!("self.rr_hl() not implemented! {:x}", opcode); 42 }, // RR (HL)
// 0x1F => { panic!("self.rr_a() not implemented! {:x}", opcode); 42 }, // RR A
// 0x20 => { panic!("self.sla_b() not implemented! {:x}", opcode); 42 }, // SLA B
// 0x21 => { panic!("self.sla_c() not implemented! {:x}", opcode); 42 }, // SLA C
// 0x22 => { panic!("self.sla_d() not implemented! {:x}", opcode); 42 }, // SLA D
// 0x23 => { panic!("self.sla_e() not implemented! {:x}", opcode); 42 }, // SLA E
// 0x24 => { panic!("self.sla_h() not implemented! {:x}", opcode); 42 }, // SLA H
// 0x25 => { panic!("self.sla_l() not implemented! {:x}", opcode); 42 }, // SLA L
// 0x26 => { panic!("self.sla_hl() not implemented! {:x}", opcode); 42 }, // SLA (HL)
// 0x27 => { panic!("self.sla_a() not implemented! {:x}", opcode); 42 }, // SLA A
// 0x28 => { panic!("self.sra_b() not implemented! {:x}", opcode); 42 }, // SRA B
// 0x29 => { panic!("self.sra_c() not implemented! {:x}", opcode); 42 }, // SRA C
// 0x2A => { panic!("self.sra_d() not implemented! {:x}", opcode); 42 }, // SRA D
// 0x2B => { panic!("self.sra_e() not implemented! {:x}", opcode); 42 }, // SRA E
// 0x2C => { panic!("self.sra_h() not implemented! {:x}", opcode); 42 }, // SRA H
// 0x2D => { panic!("self.sra_l() not implemented! {:x}", opcode); 42 }, // SRA L
// 0x2E => { panic!("self.sra_hl() not implemented! {:x}", opcode); 42 }, // SRA (HL)
// 0x2F => { panic!("self.sra_a() not implemented! {:x}", opcode); 42 }, // SRA A
// 0x30 => { panic!("self.swap_b() not implemented! {:x}", opcode); 42 }, // SWAP B
// 0x31 => { panic!("self.swap_c() not implemented! {:x}", opcode); 42 }, // SWAP C
// 0x32 => { panic!("self.swap_d() not implemented! {:x}", opcode); 42 }, // SWAP D
// 0x33 => { panic!("self.swap_e() not implemented! {:x}", opcode); 42 }, // SWAP E
// 0x34 => { panic!("self.swap_h() not implemented! {:x}", opcode); 42 }, // SWAP H
// 0x35 => { panic!("self.swap_l() not implemented! {:x}", opcode); 42 }, // SWAP L
// 0x36 => { panic!("self.swap_hl() not implemented! {:x}", opcode); 42 }, // SWAP (HL)
// 0x37 => { panic!("self.swap_a() not implemented! {:x}", opcode); 42 }, // SWAP A
// 0x38 => { panic!("self.srl_b() not implemented! {:x}", opcode); 42 }, // SRL B
// 0x39 => { panic!("self.srl_c() not implemented! {:x}", opcode); 42 }, // SRL C
// 0x3A => { panic!("self.srl_d() not implemented! {:x}", opcode); 42 }, // SRL D
// 0x3B => { panic!("self.srl_e() not implemented! {:x}", opcode); 42 }, // SRL E
// 0x3C => { panic!("self.srl_h() not implemented! {:x}", opcode); 42 }, // SRL H
// 0x3D => { panic!("self.srl_l() not implemented! {:x}", opcode); 42 }, // SRL L
// 0x3E => { panic!("self.srl_hl() not implemented! {:x}", opcode); 42 }, // SRL (HL)
// 0x3F => { panic!("self.srl_a() not implemented! {:x}", opcode); 42 }, // SRL A
// 0x40 => { panic!("self.bit_0_b() not implemented! {:x}", opcode); 42 }, // BIT 0 B
// 0x41 => { panic!("self.bit_0_c() not implemented! {:x}", opcode); 42 }, // BIT 0 C
// 0x42 => { panic!("self.bit_0_d() not implemented! {:x}", opcode); 42 }, // BIT 0 D
// 0x43 => { panic!("self.bit_0_e() not implemented! {:x}", opcode); 42 }, // BIT 0 E
// 0x44 => { panic!("self.bit_0_h() not implemented! {:x}", opcode); 42 }, // BIT 0 H
// 0x45 => { panic!("self.bit_0_l() not implemented! {:x}", opcode); 42 }, // BIT 0 L
// 0x46 => { panic!("self.bit_0_hl() not implemented! {:x}", opcode); 42 }, // BIT 0 (HL)
// 0x47 => { panic!("self.bit_0_a() not implemented! {:x}", opcode); 42 }, // BIT 0 A
// 0x48 => { panic!("self.bit_1_b() not implemented! {:x}", opcode); 42 }, // BIT 1 B
// 0x49 => { panic!("self.bit_1_c() not implemented! {:x}", opcode); 42 }, // BIT 1 C
// 0x4A => { panic!("self.bit_1_d() not implemented! {:x}", opcode); 42 }, // BIT 1 D
// 0x4B => { panic!("self.bit_1_e() not implemented! {:x}", opcode); 42 }, // BIT 1 E
// 0x4C => { panic!("self.bit_1_h() not implemented! {:x}", opcode); 42 }, // BIT 1 H
// 0x4D => { panic!("self.bit_1_l() not implemented! {:x}", opcode); 42 }, // BIT 1 L
// 0x4E => { panic!("self.bit_1_hl() not implemented! {:x}", opcode); 42 }, // BIT 1 (HL)
// 0x4F => { panic!("self.bit_1_a() not implemented! {:x}", opcode); 42 }, // BIT 1 A
// 0x50 => { panic!("self.bit_2_b() not implemented! {:x}", opcode); 42 }, // BIT 2 B
// 0x51 => { panic!("self.bit_2_c() not implemented! {:x}", opcode); 42 }, // BIT 2 C
// 0x52 => { panic!("self.bit_2_d() not implemented! {:x}", opcode); 42 }, // BIT 2 D
// 0x53 => { panic!("self.bit_2_e() not implemented! {:x}", opcode); 42 }, // BIT 2 E
// 0x54 => { panic!("self.bit_2_h() not implemented! {:x}", opcode); 42 }, // BIT 2 H
// 0x55 => { panic!("self.bit_2_l() not implemented! {:x}", opcode); 42 }, // BIT 2 L
// 0x56 => { panic!("self.bit_2_hl() not implemented! {:x}", opcode); 42 }, // BIT 2 (HL)
// 0x57 => { panic!("self.bit_2_a() not implemented! {:x}", opcode); 42 }, // BIT 2 A
// 0x58 => { panic!("self.bit_3_b() not implemented! {:x}", opcode); 42 }, // BIT 3 B
// 0x59 => { panic!("self.bit_3_c() not implemented! {:x}", opcode); 42 }, // BIT 3 C
// 0x5A => { panic!("self.bit_3_d() not implemented! {:x}", opcode); 42 }, // BIT 3 D
// 0x5B => { panic!("self.bit_3_e() not implemented! {:x}", opcode); 42 }, // BIT 3 E
// 0x5C => { panic!("self.bit_3_h() not implemented! {:x}", opcode); 42 }, // BIT 3 H
// 0x5D => { panic!("self.bit_3_l() not implemented! {:x}", opcode); 42 }, // BIT 3 L
// 0x5E => { panic!("self.bit_3_hl() not implemented! {:x}", opcode); 42 }, // BIT 3 (HL)
// 0x5F => { panic!("self.bit_3_a() not implemented! {:x}", opcode); 42 }, // BIT 3 A
// 0x60 => { panic!("self.bit_4_b() not implemented! {:x}", opcode); 42 }, // BIT 4 B
// 0x61 => { panic!("self.bit_4_c() not implemented! {:x}", opcode); 42 }, // BIT 4 C
// 0x62 => { panic!("self.bit_4_d() not implemented! {:x}", opcode); 42 }, // BIT 4 D
// 0x63 => { panic!("self.bit_4_e() not implemented! {:x}", opcode); 42 }, // BIT 4 E
// 0x64 => { panic!("self.bit_4_h() not implemented! {:x}", opcode); 42 }, // BIT 4 H
// 0x65 => { panic!("self.bit_4_l() not implemented! {:x}", opcode); 42 }, // BIT 4 L
// 0x66 => { panic!("self.bit_4_hl() not implemented! {:x}", opcode); 42 }, // BIT 4 (HL)
// 0x67 => { panic!("self.bit_4_a() not implemented! {:x}", opcode); 42 }, // BIT 4 A
// 0x68 => { panic!("self.bit_5_b() not implemented! {:x}", opcode); 42 }, // BIT 5 B
// 0x69 => { panic!("self.bit_5_c() not implemented! {:x}", opcode); 42 }, // BIT 5 C
// 0x6A => { panic!("self.bit_5_d() not implemented! {:x}", opcode); 42 }, // BIT 5 D
// 0x6B => { panic!("self.bit_5_e() not implemented! {:x}", opcode); 42 }, // BIT 5 E
// 0x6C => { panic!("self.bit_5_h() not implemented! {:x}", opcode); 42 }, // BIT 5 H
// 0x6D => { panic!("self.bit_5_l() not implemented! {:x}", opcode); 42 }, // BIT 5 L
// 0x6E => { panic!("self.bit_5_hl() not implemented! {:x}", opcode); 42 }, // BIT 5 (HL)
// 0x6F => { panic!("self.bit_5_a() not implemented! {:x}", opcode); 42 }, // BIT 5 A
// 0x70 => { panic!("self.bit_6_b() not implemented! {:x}", opcode); 42 }, // BIT 6 B
// 0x71 => { panic!("self.bit_6_c() not implemented! {:x}", opcode); 42 }, // BIT 6 C
// 0x72 => { panic!("self.bit_6_d() not implemented! {:x}", opcode); 42 }, // BIT 6 D
// 0x73 => { panic!("self.bit_6_e() not implemented! {:x}", opcode); 42 }, // BIT 6 E
// 0x74 => { panic!("self.bit_6_h() not implemented! {:x}", opcode); 42 }, // BIT 6 H
// 0x75 => { panic!("self.bit_6_l() not implemented! {:x}", opcode); 42 }, // BIT 6 L
// 0x76 => { panic!("self.bit_6_hl() not implemented! {:x}", opcode); 42 }, // BIT 6 (HL)
// 0x77 => { panic!("self.bit_6_a() not implemented! {:x}", opcode); 42 }, // BIT 6 A
// 0x78 => { panic!("self.bit_7_b() not implemented! {:x}", opcode); 42 }, // BIT 7 B
// 0x79 => { panic!("self.bit_7_c() not implemented! {:x}", opcode); 42 }, // BIT 7 C
// 0x7A => { panic!("self.bit_7_d() not implemented! {:x}", opcode); 42 }, // BIT 7 D
// 0x7B => { panic!("self.bit_7_e() not implemented! {:x}", opcode); 42 }, // BIT 7 E
// 0x7C => { panic!("self.bit_7_h() not implemented! {:x}", opcode); 42 }, // BIT 7 H
// 0x7D => { panic!("self.bit_7_l() not implemented! {:x}", opcode); 42 }, // BIT 7 L
// 0x7E => { panic!("self.bit_7_hl() not implemented! {:x}", opcode); 42 }, // BIT 7 (HL)
// 0x7F => { panic!("self.bit_7_a() not implemented! {:x}", opcode); 42 }, // BIT 7 A
// 0x80 => { panic!("self.res_0_b() not implemented! {:x}", opcode); 42 }, // RES 0 B
// 0x81 => { panic!("self.res_0_c() not implemented! {:x}", opcode); 42 }, // RES 0 C
// 0x82 => { panic!("self.res_0_d() not implemented! {:x}", opcode); 42 }, // RES 0 D
// 0x83 => { panic!("self.res_0_e() not implemented! {:x}", opcode); 42 }, // RES 0 E
// 0x84 => { panic!("self.res_0_h() not implemented! {:x}", opcode); 42 }, // RES 0 H
// 0x85 => { panic!("self.res_0_l() not implemented! {:x}", opcode); 42 }, // RES 0 L
// 0x86 => { panic!("self.res_0_hl() not implemented! {:x}", opcode); 42 }, // RES 0 (HL)
// 0x87 => { panic!("self.res_0_a() not implemented! {:x}", opcode); 42 }, // RES 0 A
// 0x88 => { panic!("self.res_1_b() not implemented! {:x}", opcode); 42 }, // RES 1 B
// 0x89 => { panic!("self.res_1_c() not implemented! {:x}", opcode); 42 }, // RES 1 C
// 0x8A => { panic!("self.res_1_d() not implemented! {:x}", opcode); 42 }, // RES 1 D
// 0x8B => { panic!("self.res_1_e() not implemented! {:x}", opcode); 42 }, // RES 1 E
// 0x8C => { panic!("self.res_1_h() not implemented! {:x}", opcode); 42 }, // RES 1 H
// 0x8D => { panic!("self.res_1_l() not implemented! {:x}", opcode); 42 }, // RES 1 L
// 0x8E => { panic!("self.res_1_hl() not implemented! {:x}", opcode); 42 }, // RES 1 (HL)
// 0x8F => { panic!("self.res_1_a() not implemented! {:x}", opcode); 42 }, // RES 1 A
// 0x90 => { panic!("self.res_2_b() not implemented! {:x}", opcode); 42 }, // RES 2 B
// 0x91 => { panic!("self.res_2_c() not implemented! {:x}", opcode); 42 }, // RES 2 C
// 0x92 => { panic!("self.res_2_d() not implemented! {:x}", opcode); 42 }, // RES 2 D
// 0x93 => { panic!("self.res_2_e() not implemented! {:x}", opcode); 42 }, // RES 2 E
// 0x94 => { panic!("self.res_2_h() not implemented! {:x}", opcode); 42 }, // RES 2 H
// 0x95 => { panic!("self.res_2_l() not implemented! {:x}", opcode); 42 }, // RES 2 L
// 0x96 => { panic!("self.res_2_hl() not implemented! {:x}", opcode); 42 }, // RES 2 (HL)
// 0x97 => { panic!("self.res_2_a() not implemented! {:x}", opcode); 42 }, // RES 2 A
// 0x98 => { panic!("self.res_3_b() not implemented! {:x}", opcode); 42 }, // RES 3 B
// 0x99 => { panic!("self.res_3_c() not implemented! {:x}", opcode); 42 }, // RES 3 C
// 0x9A => { panic!("self.res_3_d() not implemented! {:x}", opcode); 42 }, // RES 3 D
// 0x9B => { panic!("self.res_3_e() not implemented! {:x}", opcode); 42 }, // RES 3 E
// 0x9C => { panic!("self.res_3_h() not implemented! {:x}", opcode); 42 }, // RES 3 H
// 0x9D => { panic!("self.res_3_l() not implemented! {:x}", opcode); 42 }, // RES 3 L
// 0x9E => { panic!("self.res_3_hl() not implemented! {:x}", opcode); 42 }, // RES 3 (HL)
// 0x9F => { panic!("self.res_3_a() not implemented! {:x}", opcode); 42 }, // RES 3 A
// 0xA0 => { panic!("self.res_4_b() not implemented! {:x}", opcode); 42 }, // RES 4 B
// 0xA1 => { panic!("self.res_4_c() not implemented! {:x}", opcode); 42 }, // RES 4 C
// 0xA2 => { panic!("self.res_4_d() not implemented! {:x}", opcode); 42 }, // RES 4 D
// 0xA3 => { panic!("self.res_4_e() not implemented! {:x}", opcode); 42 }, // RES 4 E
// 0xA4 => { panic!("self.res_4_h() not implemented! {:x}", opcode); 42 }, // RES 4 H
// 0xA5 => { panic!("self.res_4_l() not implemented! {:x}", opcode); 42 }, // RES 4 L
// 0xA6 => { panic!("self.res_4_hl() not implemented! {:x}", opcode); 42 }, // RES 4 (HL)
// 0xA7 => { panic!("self.res_4_a() not implemented! {:x}", opcode); 42 }, // RES 4 A
// 0xA8 => { panic!("self.res_5_b() not implemented! {:x}", opcode); 42 }, // RES 5 B
// 0xA9 => { panic!("self.res_5_c() not implemented! {:x}", opcode); 42 }, // RES 5 C
// 0xAA => { panic!("self.res_5_d() not implemented! {:x}", opcode); 42 }, // RES 5 D
// 0xAB => { panic!("self.res_5_e() not implemented! {:x}", opcode); 42 }, // RES 5 E
// 0xAC => { panic!("self.res_5_h() not implemented! {:x}", opcode); 42 }, // RES 5 H
// 0xAD => { panic!("self.res_5_l() not implemented! {:x}", opcode); 42 }, // RES 5 L
// 0xAE => { panic!("self.res_5_hl() not implemented! {:x}", opcode); 42 }, // RES 5 (HL)
// 0xAF => { panic!("self.res_5_a() not implemented! {:x}", opcode); 42 }, // RES 5 A
// 0xB0 => { panic!("self.res_6_b() not implemented! {:x}", opcode); 42 }, // RES 6 B
// 0xB1 => { panic!("self.res_6_c() not implemented! {:x}", opcode); 42 }, // RES 6 C
// 0xB2 => { panic!("self.res_6_d() not implemented! {:x}", opcode); 42 }, // RES 6 D
// 0xB3 => { panic!("self.res_6_e() not implemented! {:x}", opcode); 42 }, // RES 6 E
// 0xB4 => { panic!("self.res_6_h() not implemented! {:x}", opcode); 42 }, // RES 6 H
// 0xB5 => { panic!("self.res_6_l() not implemented! {:x}", opcode); 42 }, // RES 6 L
// 0xB6 => { panic!("self.res_6_hl() not implemented! {:x}", opcode); 42 }, // RES 6 (HL)
// 0xB7 => { panic!("self.res_6_a() not implemented! {:x}", opcode); 42 }, // RES 6 A
// 0xB8 => { panic!("self.res_7_b() not implemented! {:x}", opcode); 42 }, // RES 7 B
// 0xB9 => { panic!("self.res_7_c() not implemented! {:x}", opcode); 42 }, // RES 7 C
// 0xBA => { panic!("self.res_7_d() not implemented! {:x}", opcode); 42 }, // RES 7 D
// 0xBB => { panic!("self.res_7_e() not implemented! {:x}", opcode); 42 }, // RES 7 E
// 0xBC => { panic!("self.res_7_h() not implemented! {:x}", opcode); 42 }, // RES 7 H
// 0xBD => { panic!("self.res_7_l() not implemented! {:x}", opcode); 42 }, // RES 7 L
// 0xBE => { panic!("self.res_7_hl() not implemented! {:x}", opcode); 42 }, // RES 7 (HL)
// 0xBF => { panic!("self.res_7_a() not implemented! {:x}", opcode); 42 }, // RES 7 A
// 0xC0 => { panic!("self.set_0_b() not implemented! {:x}", opcode); 42 }, // SET 0 B
// 0xC1 => { panic!("self.set_0_c() not implemented! {:x}", opcode); 42 }, // SET 0 C
// 0xC2 => { panic!("self.set_0_d() not implemented! {:x}", opcode); 42 }, // SET 0 D
// 0xC3 => { panic!("self.set_0_e() not implemented! {:x}", opcode); 42 }, // SET 0 E
// 0xC4 => { panic!("self.set_0_h() not implemented! {:x}", opcode); 42 }, // SET 0 H
// 0xC5 => { panic!("self.set_0_l() not implemented! {:x}", opcode); 42 }, // SET 0 L
// 0xC6 => { panic!("self.set_0_hl() not implemented! {:x}", opcode); 42 }, // SET 0 (HL)
// 0xC7 => { panic!("self.set_0_a() not implemented! {:x}", opcode); 42 }, // SET 0 A
// 0xC8 => { panic!("self.set_1_b() not implemented! {:x}", opcode); 42 }, // SET 1 B
// 0xC9 => { panic!("self.set_1_c() not implemented! {:x}", opcode); 42 }, // SET 1 C
// 0xCA => { panic!("self.set_1_d() not implemented! {:x}", opcode); 42 }, // SET 1 D
// 0xCB => { panic!("self.set_1_e() not implemented! {:x}", opcode); 42 }, // SET 1 E
// 0xCC => { panic!("self.set_1_h() not implemented! {:x}", opcode); 42 }, // SET 1 H
// 0xCD => { panic!("self.set_1_l() not implemented! {:x}", opcode); 42 }, // SET 1 L
// 0xCE => { panic!("self.set_1_hl() not implemented! {:x}", opcode); 42 }, // SET 1 (HL)
// 0xCF => { panic!("self.set_1_a() not implemented! {:x}", opcode); 42 }, // SET 1 A
// 0xD0 => { panic!("self.set_2_b() not implemented! {:x}", opcode); 42 }, // SET 2 B
// 0xD1 => { panic!("self.set_2_c() not implemented! {:x}", opcode); 42 }, // SET 2 C
// 0xD2 => { panic!("self.set_2_d() not implemented! {:x}", opcode); 42 }, // SET 2 D
// 0xD3 => { panic!("self.set_2_e() not implemented! {:x}", opcode); 42 }, // SET 2 E
// 0xD4 => { panic!("self.set_2_h() not implemented! {:x}", opcode); 42 }, // SET 2 H
// 0xD5 => { panic!("self.set_2_l() not implemented! {:x}", opcode); 42 }, // SET 2 L
// 0xD6 => { panic!("self.set_2_hl() not implemented! {:x}", opcode); 42 }, // SET 2 (HL)
// 0xD7 => { panic!("self.set_2_a() not implemented! {:x}", opcode); 42 }, // SET 2 A
// 0xD8 => { panic!("self.set_3_b() not implemented! {:x}", opcode); 42 }, // SET 3 B
// 0xD9 => { panic!("self.set_3_c() not implemented! {:x}", opcode); 42 }, // SET 3 C
// 0xDA => { panic!("self.set_3_d() not implemented! {:x}", opcode); 42 }, // SET 3 D
// 0xDB => { panic!("self.set_3_e() not implemented! {:x}", opcode); 42 }, // SET 3 E
// 0xDC => { panic!("self.set_3_h() not implemented! {:x}", opcode); 42 }, // SET 3 H
// 0xDD => { panic!("self.set_3_l() not implemented! {:x}", opcode); 42 }, // SET 3 L
// 0xDE => { panic!("self.set_3_hl() not implemented! {:x}", opcode); 42 }, // SET 3 (HL)
// 0xDF => { panic!("self.set_3_a() not implemented! {:x}", opcode); 42 }, // SET 3 A
// 0xE0 => { panic!("self.set_4_b() not implemented! {:x}", opcode); 42 }, // SET 4 B
// 0xE1 => { panic!("self.set_4_c() not implemented! {:x}", opcode); 42 }, // SET 4 C
// 0xE2 => { panic!("self.set_4_d() not implemented! {:x}", opcode); 42 }, // SET 4 D
// 0xE3 => { panic!("self.set_4_e() not implemented! {:x}", opcode); 42 }, // SET 4 E
// 0xE4 => { panic!("self.set_4_h() not implemented! {:x}", opcode); 42 }, // SET 4 H
// 0xE5 => { panic!("self.set_4_l() not implemented! {:x}", opcode); 42 }, // SET 4 L
// 0xE6 => { panic!("self.set_4_hl() not implemented! {:x}", opcode); 42 }, // SET 4 (HL)
// 0xE7 => { panic!("self.set_4_a() not implemented! {:x}", opcode); 42 }, // SET 4 A
// 0xE8 => { panic!("self.set_5_b() not implemented! {:x}", opcode); 42 }, // SET 5 B
// 0xE9 => { panic!("self.set_5_c() not implemented! {:x}", opcode); 42 }, // SET 5 C
// 0xEA => { panic!("self.set_5_d() not implemented! {:x}", opcode); 42 }, // SET 5 D
// 0xEB => { panic!("self.set_5_e() not implemented! {:x}", opcode); 42 }, // SET 5 E
// 0xEC => { panic!("self.set_5_h() not implemented! {:x}", opcode); 42 }, // SET 5 H
// 0xED => { panic!("self.set_5_l() not implemented! {:x}", opcode); 42 }, // SET 5 L
// 0xEE => { panic!("self.set_5_hl() not implemented! {:x}", opcode); 42 }, // SET 5 (HL)
// 0xEF => { panic!("self.set_5_a() not implemented! {:x}", opcode); 42 }, // SET 5 A
// 0xF0 => { panic!("self.set_6_b() not implemented! {:x}", opcode); 42 }, // SET 6 B
// 0xF1 => { panic!("self.set_6_c() not implemented! {:x}", opcode); 42 }, // SET 6 C
// 0xF2 => { panic!("self.set_6_d() not implemented! {:x}", opcode); 42 }, // SET 6 D
// 0xF3 => { panic!("self.set_6_e() not implemented! {:x}", opcode); 42 }, // SET 6 E
// 0xF4 => { panic!("self.set_6_h() not implemented! {:x}", opcode); 42 }, // SET 6 H
// 0xF5 => { panic!("self.set_6_l() not implemented! {:x}", opcode); 42 }, // SET 6 L
// 0xF6 => { panic!("self.set_6_hl() not implemented! {:x}", opcode); 42 }, // SET 6 (HL)
// 0xF7 => { panic!("self.set_6_a() not implemented! {:x}", opcode); 42 }, // SET 6 A
// 0xF8 => { panic!("self.set_7_b() not implemented! {:x}", opcode); 42 }, // SET 7 B
// 0xF9 => { panic!("self.set_7_c() not implemented! {:x}", opcode); 42 }, // SET 7 C
// 0xFA => { panic!("self.set_7_d() not implemented! {:x}", opcode); 42 }, // SET 7 D
// 0xFB => { panic!("self.set_7_e() not implemented! {:x}", opcode); 42 }, // SET 7 E
// 0xFC => { panic!("self.set_7_h() not implemented! {:x}", opcode); 42 }, // SET 7 H
// 0xFD => { panic!("self.set_7_l() not implemented! {:x}", opcode); 42 }, // SET 7 L
// 0xFE => { panic!("self.set_7_hl() not implemented! {:x}", opcode); 42 }, // SET 7 (HL)
// 0xFF => { panic!("self.set_7_a() not implemented! {:x}", opcode); 42 }, // SET 7 A
