use std::fmt;

pub use super::cartridge;
pub use super::mmu;
pub mod opcode_cycles;

const FLAG_ZERO: u8 = 0x80; // Zero
const FLAG_SUB: u8 = 0x40; // Negative,
const FLAG_HALF_CARRY: u8 = 0x20; // Half-carry
const FLAG_CARRY: u8 = 0x10; // Carry
const FLAG_NONE: u8 = 0x00; // None

const SYNC_STATE: bool = false;
const AFTER_TICK_COUNT: u64 = 32057900;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegEnum { A, F, B, C, D, E, H, L, S, P, AF, BC, DE, HL, SP }

pub struct Register { value: u16 }

impl Register {
  fn new() -> Register {
    Register { value: 0x0 }
  }

  fn read(&self) -> u16 {
    self.value
  }

  fn read_hi(&self) -> u8 {
    ((self.value >> 8) & 0xFF) as u8
  }

  fn read_lo(&self) -> u8 {
    (self.value & 0xFF) as u8
  }

  fn write(&mut self, v: u16) {
    self.value = v
  }

  fn write_hi(&mut self, v: u8) {
    self.value = self.value & 0x00FF | (v as u16) << 8
  }

  fn write_lo(&mut self, v: u8) {
    self.value = self.value & 0xFF00 | v as u16
  }
}

impl fmt::Debug for Register {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Register {{ value: {} }}", self.value)
  }
}

pub struct CPU {
  af: Register,
  bc: Register,
  de: Register,
  hl: Register,
  sp: Register,
  pc: u16,

  branch_taken: bool,
  master_interrupt_toggle: bool, // Master interrupt
  ei_cycles: u8,
  di_cycles: u8,

  halted: bool,

  pub tick_counter: u64, // TODO make this private
}

fn formatted_flags(cpu: &CPU) -> String {
  format!(
    "Z: {}, N: {}, H: {} C: {}",
    cpu.util_is_flag_set(FLAG_ZERO) as u8,
    cpu.util_is_flag_set(FLAG_SUB) as u8,
    cpu.util_is_flag_set(FLAG_HALF_CARRY) as u8,
    cpu.util_is_flag_set(FLAG_CARRY) as u8
  )
}

impl fmt::Debug for CPU {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "[PC] {:#X}\n[Regs] A:{:#X}, F:{:#X}, B:{:#X}, C:{:#X}, D:{:#X}, E:{:#X}, H:{:#X}, L:{:#X}\n[Flags]: {} [SP] {:#X}",
      self.pc, self.af.read_hi(), self.af.read_lo(), self.bc.read_hi(), self.bc.read_lo(),
      self.de.read_hi(), self.de.read_lo(), self.hl.read_hi(), self.hl.read_lo(),
      formatted_flags(self), self.sp.read()
    )
  }
}

impl CPU {
  pub fn new() -> CPU {
    CPU {
      af: Register::new(), bc: Register::new(), de: Register::new(), hl: Register::new(), sp: Register::new(),
      pc: 0x0000,

      branch_taken: false,
      master_interrupt_toggle: true,

      ei_cycles: 0,
      di_cycles: 0,

      halted: false,
      tick_counter: 0u64,
    }
  }

  pub fn initialize(&mut self) {
    self.pc = 0x0100;

    self.af.write(0x01B0);
    self.bc.write(0x0013);
    self.de.write(0x00D8);
    self.hl.write(0x014D);
    self.sp.write(0xFFFE);
  }

  fn write_byte_reg(&mut self, reg_enum: RegEnum, byte: u8) {
    match reg_enum {
      RegEnum::A => self.af.write_hi(byte),
      RegEnum::F => self.af.write_lo(byte),
      RegEnum::B => self.bc.write_hi(byte),
      RegEnum::C => self.bc.write_lo(byte),
      RegEnum::D => self.de.write_hi(byte),
      RegEnum::E => self.de.write_lo(byte),
      RegEnum::H => self.hl.write_hi(byte),
      RegEnum::L => self.hl.write_lo(byte),
      RegEnum::S => self.sp.write_hi(byte),
      RegEnum::P => self.sp.write_lo(byte),
      _ => panic!("Unexpected reg_enum: {:?}", reg_enum),
    }
  }

  fn write_word_reg(&mut self, reg_enum: RegEnum, word: u16) {
    match reg_enum {
      RegEnum::AF => self.af.write(word),
      RegEnum::BC => self.bc.write(word),
      RegEnum::DE => self.de.write(word),
      RegEnum::HL => self.hl.write(word),
      RegEnum::SP => self.sp.write(word),
      _ => {panic!("Unexpected reg_enum: {:?}", reg_enum)},
    }
  }

  fn read_byte_reg(&mut self, reg_enum: RegEnum) -> u8 {
    match reg_enum {
      RegEnum::A => self.af.read_hi(),
      RegEnum::F => self.af.read_lo(),
      RegEnum::B => self.bc.read_hi(),
      RegEnum::C => self.bc.read_lo(),
      RegEnum::D => self.de.read_hi(),
      RegEnum::E => self.de.read_lo(),
      RegEnum::H => self.hl.read_hi(),
      RegEnum::L => self.hl.read_lo(),
      RegEnum::S => self.sp.read_hi(),
      RegEnum::P => self.sp.read_lo(),
      _ => panic!("Unexpected reg_enum: {:?}", reg_enum),
    }
  }

  fn read_word_reg(&mut self, reg_enum: RegEnum) -> u16 {
    match reg_enum {
      RegEnum::AF => self.af.read(),
      RegEnum::BC => self.bc.read(),
      RegEnum::DE => self.de.read(),
      RegEnum::HL => self.hl.read(),
      RegEnum::SP => self.sp.read(),
      _ => panic!("Unexpected reg_enum: {:?}", reg_enum),
    }
  }

  fn explode(&mut self, message: String) {
    println!("pc: {:?} tick_counter: {}", self, self.tick_counter);
    panic!(message)
  }

  pub fn execute_next_opcode(&mut self, mmu: &mut mmu::MMU) -> i32 {
    self.update_ime();

    match self.handle_interrupts(mmu) {
        0 => {},
        interrupt_cycles => return interrupt_cycles,
    };

    if self.halted { return 1 }

    let opcode: u8 = mmu.read(self.pc);

    if SYNC_STATE && self.tick_counter >= AFTER_TICK_COUNT {
      let _registers = format!(
        "PC:{:04x} SP:{:04x} A:{:02x} F:{:04b} B:{:02x} C:{:02x} D:{:02x} E:{:02x} H:{:02x} L:{:02x}\n",
        self.pc, self.sp.value,
        self.af.read_hi(), self.af.read_lo(), self.bc.read_hi(), self.bc.read_lo(),
        self.de.read_hi(), self.de.read_lo(), self.hl.read_hi(), self.hl.read_lo()
      );
    }

    self.tick_counter += 1;

    self.pc += 1;
    self.execute_opcode(opcode, mmu)
  }

  fn handle_interrupts(&mut self, mmu: &mut mmu::MMU) -> i32 {
    if self.master_interrupt_toggle == false && self.halted == false { return 0 }

    let interrupt_to_handle = mmu.interrupt_enabled & mmu.interrupt_flags;
    if interrupt_to_handle == 0 { return 0 }

    self.halted = false;
    if self.master_interrupt_toggle == false { return 0 }
    self.master_interrupt_toggle = false;

    let interrupt_offset = interrupt_to_handle.trailing_zeros() as u16;
    if interrupt_offset >= 5 { panic!("Invalid interrupt"); }

    mmu.interrupt_flags &= !(1 << interrupt_offset);

    let current_pc = self.pc;
    self.stack_push(current_pc, mmu);

    if interrupt_offset != 0 {
      panic!("Doing interrupt other than vblank, {}", interrupt_offset);
    }

    self.pc = 0x0040 | ((interrupt_offset) << 3);

    16
  }

  fn update_ime(&mut self) {
    self.di_cycles = match self.di_cycles {
        2 => 1,
        1 => { self.master_interrupt_toggle = false; 0 },
        _ => 0,
    };
    self.ei_cycles = match self.ei_cycles {
        2 => 1,
        1 => { self.master_interrupt_toggle = true; 0 },
        _ => 0,
    };
  }

  fn execute_opcode(&mut self, opcode: u8, mmu: &mut mmu::MMU) -> i32 {
    let mut cb_opcode: Option<u8> = None;

    match opcode {
      0x8E => { debug!("ADC A,(HL)"); let address = self.read_word_reg(RegEnum::HL); let value = mmu.read(address); shared_add_n(self, value, true) },
      0x8F => { debug!("ADC A,A"); let value = self.read_byte_reg(RegEnum::A); shared_add_n(self, value, true) }
      0x88 => { debug!("ADC A,B"); let value = self.read_byte_reg(RegEnum::B); shared_add_n(self, value, true) }
      0x89 => { debug!("ADC A,C"); let value = self.read_byte_reg(RegEnum::C); shared_add_n(self, value, true) }
      0x8A => { debug!("ADC A,D"); let value = self.read_byte_reg(RegEnum::D); shared_add_n(self, value, true) }
      0x8B => { debug!("ADC A,E"); let value = self.read_byte_reg(RegEnum::E); shared_add_n(self, value, true) }
      0x8C => { debug!("ADC A,H"); let value = self.read_byte_reg(RegEnum::H); shared_add_n(self, value, true) }
      0x8D => { debug!("ADC A,L"); let value = self.read_byte_reg(RegEnum::L); shared_add_n(self, value, true) }
      0xCE => self.explode(format!("ADC A,n {:#X}", opcode)),
      0x86 => { debug!("ADD A,(HL)"); self.add_a_hl(mmu) }
      0x87 => { debug!("ADD A,A"); let value = self.read_byte_reg(RegEnum::A); shared_add_n(self, value, false) }
      0x80 => { debug!("ADD A,B"); let value = self.read_byte_reg(RegEnum::B); shared_add_n(self, value, false) }
      0x81 => { debug!("ADD A,C"); let value = self.read_byte_reg(RegEnum::C); shared_add_n(self, value, false) }
      0x82 => { debug!("ADD A,D"); let value = self.read_byte_reg(RegEnum::D); shared_add_n(self, value, false) }
      0x83 => { debug!("ADD A,E"); let value = self.read_byte_reg(RegEnum::E); shared_add_n(self, value, false) }
      0x84 => { debug!("ADD A,H"); let value = self.read_byte_reg(RegEnum::H); shared_add_n(self, value, false) }
      0x85 => { debug!("ADD A,L"); let value = self.read_byte_reg(RegEnum::L); shared_add_n(self, value, false) }
      0xC6 => { debug!("ADD A,N"); self.add_a_n(mmu) }
      0x09 => { debug!("ADD HL,BC"); self.add_hl_bc() }
      0x19 => { debug!("ADD HL,DE"); self.add_hl_de() }
      0x29 => self.explode(format!("ADD HL,HL {:#X}", opcode)),
      0x39 => { debug!("ADD HL,SP"); shared_add_word_and_word_regs(self, RegEnum::HL, RegEnum::SP); },
      0xE8 => self.explode(format!("ADD SP,n {:#X}", opcode)),
      0xA6 => self.explode(format!("AND (HL) {:#X}", opcode)),
      0xA7 => { debug!("AND A"); self.and_a() }
      0xA0 => { debug!("AND B"); self.and_b() }
      0xA1 => { debug!("AND C"); self.and_c() }
      0xA2 => { debug!("AND D"); self.and_d() }
      0xA3 => { debug!("AND E"); self.and_e() }
      0xA4 => { debug!("AND H"); self.and_h() }
      0xA5 => { debug!("AND L"); self.and_l() }
      0xE6 => { debug!("AND n"); self.and_n(mmu) }
      0xDC => self.explode(format!("CALL C,nn {:#X}", opcode)),
      0xD4 => self.explode(format!("CALL NC,nn {:#X}", opcode)),
      0xC4 => { debug!("CALL NZ,nn"); self.call_nz_nn(mmu) }
      0xCC => self.explode(format!("CALL Z,nn {:#X}", opcode)),
      0xCD => { debug!("CALL nn"); self.call_nn(mmu) }
      0xCB => {
        debug!("CB prefixed instruction");
        cb_opcode = Some(mmu.read(self.pc));
        self.cb_prefixed_instruction(cb_opcode.unwrap(), mmu)
      }
      0x3F => self.explode(format!("CCF {:#X}", opcode)),
      0xBE => { debug!("CP (HL)"); self.cp_hl(mmu) }
      0xBF => self.explode(format!("CP A {:#X}", opcode)),
      0xB8 => {
        debug!("CP B");
        let a = self.read_byte_reg(RegEnum::A);
        let value = self.read_byte_reg(RegEnum::B);
        shared_cp(self, value);
        self.write_byte_reg(RegEnum::A, a);
      },
      0xB9 => {
        debug!("CP C");
        let a = self.read_byte_reg(RegEnum::A);
        let value = self.read_byte_reg(RegEnum::C);
        shared_cp(self, value);
        self.write_byte_reg(RegEnum::A, a);
      },
      0xBA => self.explode(format!("CP D {:#X}", opcode)),
      0xBB => self.explode(format!("CP E {:#X}", opcode)),
      0xBC => self.explode(format!("CP H {:#X}", opcode)),
      0xBD => self.explode(format!("CP L {:#X}", opcode)),
      0xFE => { debug!("CP"); self.cp_n(mmu) }
      0x2F => { debug!("CPL"); self.cpl() }
      0x27 => { debug!("DAA"); self.daa() } ,
      0x35 => { debug!("DEC (HL)"); self.dec_hl_indirect(mmu) }
      0x3D => { debug!("DEC A"); self.dec_a() }
      0x05 => { debug!("DEC B"); self.dec_b() }
      0x0B => { debug!("DEC BC"); self.dec_bc() }
      0x0D => { debug!("DEC C"); self.dec_c() }
      0x15 => { debug!("DEC D"); self.dec_d() }
      0x1B => { debug!("DEC DE"); shared_dec_word_reg(self, RegEnum::DE) },
      0x1D => { debug!("DEC E"); self.dec_e() }
      0x25 => { debug!("DEC H"); shared_dec_byte_reg(self, RegEnum::H) }
      0x2B => { debug!("DEC HL"); shared_dec_word_reg(self, RegEnum::HL) },
      0x2D => { debug!("DEC L"); self.dec_l(); }
      0x3B => {
        debug!("DEC SP");
        let sp = self.read_word_reg(RegEnum::SP);
        self.write_word_reg(RegEnum::SP, sp.wrapping_sub(1))
      }
      0xF3 => { debug!("DI"); self.di() }
      0xFB => { debug!("EI"); self.ei() }
      0x76 => self.explode(format!("HALT {:#X}", opcode)),
      0x34 => { debug!("INC (HL)"); self.inc_hl_indirect(mmu) }
      0x3C => { debug!("INC A"); self.inc_a() }
      0x04 => { debug!("INC B"); self.inc_b() }
      0x03 => { debug!("INC BC"); self.inc_bc() }
      0x0C => { debug!("INC C"); self.inc_c() }
      0x14 => { debug!("INC D"); shared_inc_byte_reg(self, RegEnum::D) },
      0x13 => { debug!("INC DE"); self.inc_de() }
      0x1C => { debug!("INC E"); self.inc_e() }
      0x24 => { debug!("INC H"); self.inc_h() }
      0x23 => { debug!("INC HL"); self.inc_hl() }
      0x2C => { debug!("INC L"); self.inc_l() }
      0x33 => self.explode(format!("INC SP {:#X}", opcode)),
      0xE9 => { debug!("JP (HL)"); self.jp_hl() }
      0xDA => self.explode(format!("JP C,nn {:#X}", opcode)),
      0xD2 => self.explode(format!("JP NC,nn {:#X}", opcode)),
      0xC2 => { debug!("JP NZ,nn"); self.jp_nz_nn(mmu) }
      0xCA => { debug!("JP Z,nn"); self.jp_z_nn(mmu) }
      0xC3 => { debug!("JP nn"); self.jp_nn(mmu) }
      0x38 => { debug!("JR C,n"); self.jr_c_n(mmu) },
      0x30 => { debug!("JR NC,n"); self.jr_nc_n(mmu) },
      0x20 => { debug!("JR NZ,n"); self.jr_nz_n(mmu) }
      0x28 => { debug!("JR Z,n"); self.jr_z_n(mmu) }
      0x18 => { debug!("JR n"); self.jr_n(mmu) }
      0xE2 => { debug!("LD (0xFF00+C),A"); self.ld_0xff00_plus_c_a(mmu) }
      0xE0 => { debug!("LD (0xFF00+n),A"); self.ld_0xff00_plus_n_a(mmu) }
      0x02 => { debug!("LD (BC),A"); self.shared_ld_nn_reg(RegEnum::A, RegEnum::BC, mmu) }
      0x12 => { debug!("LD (DE),A"); self.ld_de_a(mmu) }
      0x77 => { debug!("LD (HL),A"); self.ld_hl_a(mmu) }
      0x70 => self.explode(format!("LD (HL),B {:#X}", opcode)),
      0x71 => { debug!("LD (HL),C"); self.ld_hl_c(mmu) }
      0x72 => { debug!("LD (HL),D"); self.ld_hl_d(mmu) }
      0x73 => { debug!("LD (HL),E"); self.ld_hl_e(mmu) }
      0x74 => self.explode(format!("LD (HL),H {:#X}", opcode)),
      0x75 => self.explode(format!("LD (HL),L {:#X}", opcode)),
      0x36 => { debug!("LD (HL),n"); self.ld_hl_n(mmu) }
      0x32 => { debug!("LD (HLD), A"); self.ld_hld_a(mmu) }
      0x22 => { debug!("LD (HLI),A"); self.ld_hli_a(mmu) }
      0xEA => { debug!("LD (nn),A"); self.ld_nn_a(mmu) }
      0x08 => { debug!("LD (nn),SP"); self.ld_nn_sp(mmu) }
      0xF0 => { debug!("LD A,(0xFF00+n)"); self.ld_a_0xff00_plus_n(mmu) }
      0x0A => { debug!("LD A,(BC)"); self.ld_a_bc(mmu) }
      0xF2 => self.explode(format!("LD A,(C) {:#X}", opcode)),
      0x1A => { debug!("LD A,(DE)"); self.ld_a_de(mmu) }
      0x7E => { debug!("LD A,(HL)"); self.ld_a_hl(mmu) }
      0x3A => { debug!("LD A,(HLD)"); self.ld_a_hld(mmu) }
      0x2A => { debug!("LD A,(HLI)"); self.ld_a_hli(mmu) }
      0xFA => { debug!("LD A,(nn)"); self.ld_a_nn(mmu) }
      0x7F => { debug!("LD A,A"); }
      0x78 => { debug!("LD A,B"); self.ld_a_b() }
      0x79 => { debug!("LD A,C"); self.ld_a_c() }
      0x7A => { debug!("LD A,D"); self.ld_a_d() }
      0x7B => { debug!("LD A,E"); self.ld_a_e() },
      0x7C => { debug!("LD A,H"); self.ld_a_h() }
      0x7D => { debug!("LD A,L"); self.ld_a_l() }
      0x3E => { debug!("LD A,n"); self.ld_a_n(mmu) }
      0x46 => { debug!("LD B,(HL)"); self.ld_b_hl(mmu) }
      0x47 => { debug!("LD B,A"); self.ld_b_a() }
      0x40 => { debug!("LD B,B"); self.ld_b_b() }
      0x41 => { debug!("LD B,C"); self.shared_ld_reg_reg(RegEnum::C, RegEnum::B) }
      0x42 => { debug!("LD B,D"); self.shared_ld_reg_reg(RegEnum::D, RegEnum::B) }
      0x43 => { debug!("LD B,E"); self.shared_ld_reg_reg(RegEnum::E, RegEnum::B) }
      0x44 => { debug!("LD B,H"); self.shared_ld_reg_reg(RegEnum::H, RegEnum::B) }
      0x45 => { debug!("LD B,L"); self.shared_ld_reg_reg(RegEnum::L, RegEnum::B) }
      0x06 => { debug!("LD B,n"); self.ld_b_n(mmu) }
      0x01 => { debug!("LD BC,nn"); self.ld_bc_nn(mmu) }
      0x4E => { debug!("LD C,(HL)"); self.ld_c_hl(mmu) }
      0x4F => { debug!("LD C,A"); self.ld_c_a() }
      0x48 => self.explode(format!("LD C,B {:#X}", opcode)),
      0x49 => debug!("LD C,C"), // no-op
      0x4A => self.explode(format!("LD C,D {:#X}", opcode)),
      0x4B => self.explode(format!("LD C,E {:#X}", opcode)),
      0x4C => self.explode(format!("LD C,H {:#X}", opcode)),
      0x4D => self.explode(format!("LD C,L {:#X}", opcode)),
      0x0E => { debug!("LD C,n"); self.ld_c_n(mmu) }
      0x56 => { debug!("LD D,(HL)"); self.ld_d_hl(mmu) }
      0x57 => { debug!("LD D,A"); self.ld_d_a() },
      0x50 => self.explode(format!("LD D,B {:#X}", opcode)),
      0x51 => self.explode(format!("LD D,C {:#X}", opcode)),
      0x52 => self.explode(format!("LD D,D {:#X}", opcode)),
      0x53 => self.explode(format!("LD D,E {:#X}", opcode)),
      0x54 => { debug!("LD D,H"); self.ld_d_h() }
      0x55 => self.explode(format!("LD D,L {:#X}", opcode)),
      0x16 => { debug!("LD D,n"); self.ld_d_n(mmu) }
      0x11 => { debug!("LD DE,nn"); self.ld_de_nn(mmu) }
      0x5E => { debug!("LD E,(HL)"); self.ld_e_hl(mmu) }
      0x5F => { debug!("LD E,A"); self.ld_e_a() }
      0x58 => self.explode(format!("LD E,B {:#X}", opcode)),
      0x59 => self.explode(format!("LD E,C {:#X}", opcode)),
      0x5A => self.explode(format!("LD E,D {:#X}", opcode)),
      0x5B => self.explode(format!("LD E,E {:#X}", opcode)),
      0x5C => self.explode(format!("LD E,H {:#X}", opcode)),
      0x5D => { debug!("LD E,L"); self.ld_e_l() }
      0x1E => { debug!("LD E,n"); self.ld_e_n(mmu) }
      0x66 => self.explode(format!("LD H,(HL) {:#X}", opcode)),
      0x67 => { debug!("LD H,A"); self.ld_h_a() },
      0x60 => { debug!("LD H,B"); self.ld_h_b() }
      0x61 => { debug!("LD H,C"); self.ld_h_c() }
      0x62 => { debug!("LD H,D"); self.ld_h_d() }
      0x63 => self.explode(format!("LD H,E {:#X}", opcode)),
      0x64 => self.explode(format!("LD H,H {:#X}", opcode)),
      0x65 => self.explode(format!("LD H,L {:#X}", opcode)),
      0x26 => { debug!("LD H,n"); shared_ld_reg_n(self, RegEnum::H, mmu) }
      0xF8 => self.explode(format!("LD HL,SP+n {:#X}", opcode)),
      0x21 => { debug!("LD HL,nn"); self.ld_hl_nn(mmu) }
      0x6E => self.explode(format!("LD L,(HL) {:#X}", opcode)),
      0x6F => { debug!("LD L,A"); self.ld_l_a() }
      0x68 => self.explode(format!("LD L,B {:#X}", opcode)),
      0x69 => { debug!("LD L,C"); self.ld_l_c() }
      0x6A => self.explode(format!("LD L,D {:#X}", opcode)),
      0x6B => { debug!("LD L,E"); self.ld_l_e() }
      0x6C => self.explode(format!("LD L,H {:#X}", opcode)),
      0x6D => self.explode(format!("LD L,L {:#X}", opcode)),
      0x2E => { debug!("LD L,n"); self.ld_l_n(mmu) },
      0xF9 => self.explode(format!("LD SP,HL {:#X}", opcode)),
      0x31 => { debug!("LD SP,nn"); self.ld_sp_nn(mmu) }
      0x00 => { debug!("NOP"); self.nop() }
      0xB6 => self.explode(format!("OR (HL) {:#X}", opcode)),
      0xB7 => { debug!("OR A"); let value = self.read_byte_reg(RegEnum::A); shared_or_n(self, value) },
      0xB0 => { debug!("OR B"); self.or_b() }
      0xB1 => { debug!("OR C"); self.or_c() }
      0xB2 => { debug!("OR D"); let value = self.read_byte_reg(RegEnum::D); shared_or_n(self, value); }
      0xB3 => { debug!("OR E"); let value = self.read_byte_reg(RegEnum::E); shared_or_n(self, value); }
      0xB4 => { debug!("OR H"); let value = self.read_byte_reg(RegEnum::H); shared_or_n(self, value); }
      0xB5 => { debug!("OR L"); let value = self.read_byte_reg(RegEnum::L); shared_or_n(self, value); }
      0xF6 => { debug!("OR N"); self.or_n(mmu) }
      0xF1 => { debug!("POP AF"); self.pop_af(mmu) }
      0xC1 => { debug!("POP BC"); self.pop_bc(mmu) }
      0xD1 => { debug!("POP DE"); self.pop_de(mmu) }
      0xE1 => { debug!("POP HL"); self.pop_hl(mmu) }
      0xF5 => { debug!("PUSH AF"); self.push_af(mmu) }
      0xC5 => { debug!("PUSH BC"); self.push_bc(mmu) }
      0xD5 => { debug!("PUSH DE"); self.push_de(mmu) }
      0xE5 => { debug!("PUSH HL"); self.push_hl(mmu) }
      0xD8 => { debug!("RET C"); self.ret_c(mmu) },
      0xD0 => { debug!("RET NC"); self.ret_nc(mmu) },
      0xC0 => { debug!("RET NZ"); self.ret_nz(mmu) }
      0xC8 => { debug!("RET Z"); self.ret_z(mmu) }
      0xC9 => { debug!("RET"); self.ret(mmu) }
      0xD9 => { debug!("RETI"); self.reti(mmu) }
      0x17 => { debug!("RLA"); self.rla() }
      0x07 => { debug!("RLCA"); shared_rlc_n(self, RegEnum::A); self.util_set_flag_by_boolean(FLAG_ZERO, false) }
      0x1F => { debug!("RRA"); self.rra() }
      0x0F => self.explode(format!("RRCA {:#X}", opcode)),
      0xC7 => self.explode(format!("RST 00H {:#X}", opcode)),
      0xCF => self.explode(format!("RST 08H {:#X}", opcode)),
      0xD7 => self.explode(format!("RST 10H {:#X}", opcode)),
      0xDF => self.explode(format!("RST 18H {:#X}", opcode)),
      0xE7 => self.explode(format!("RST 20H {:#X}", opcode)),
      0xEF => { debug!("RST 28H"); self.rst_28h(mmu) }
      0xF7 => self.explode(format!("RST 30H {:#X}", opcode)),
      0xFF => { debug!("RST 38H"); self.rst_38h(mmu) }
      0x9E => self.explode(format!("SBC (HL) {:#X}", opcode)),
      0x9F => self.explode(format!("SBC A {:#X}", opcode)),
      0x98 => self.explode(format!("SBC B {:#X}", opcode)),
      0x99 => self.explode(format!("SBC C {:#X}", opcode)),
      0x9A => self.explode(format!("SBC D {:#X}", opcode)),
      0x9B => self.explode(format!("SBC E {:#X}", opcode)),
      0x9C => self.explode(format!("SBC H {:#X}", opcode)),
      0x9D => self.explode(format!("SBC L {:#X}", opcode)),
      0xDE => self.explode(format!("SBC n {:#X}", opcode)),
      0x37 => self.explode(format!("SCF {:#X}", opcode)),
      0x10 => {
        debug!("STOP");
        // >>>>>>> TODO <<<<<<<<
        // println!(" not implemented.")
      },
      0x96 => { debug!("SUB (HL)"); let address = self.read_word_reg(RegEnum:: HL); let value = mmu.read(address); shared_sub_n(self, value, false); }
      0x97 => self.explode(format!("SUB A {:#X}", opcode)),
      0x90 => { debug!("SUB B"); let b = self.read_byte_reg(RegEnum::B); shared_sub_n(self, b, false) }
      0x91 => self.explode(format!("SUB C {:#X}", opcode)),
      0x92 => self.explode(format!("SUB D {:#X}", opcode)),
      0x93 => self.explode(format!("SUB E {:#X}", opcode)),
      0x94 => self.explode(format!("SUB H {:#X}", opcode)),
      0x95 => self.explode(format!("SUB L {:#X}", opcode)),
      0xD6 => { debug!("SUB n"); let value = mmu.read(self.pc); shared_sub_n(self, value, false); }
      0xFD => debug!("Unhandled opcode"),
      0xAE => { debug!("XOR (HL)"); let hl = self.read_word_reg(RegEnum::HL); let value = mmu.read(hl); shared_xor_n(self, value) }
      0xAF => { debug!("XOR A"); self.xor_a() }
      0xA8 => { debug!("XOR B"); self.xor_b() }
      0xA9 => { debug!("XOR C"); self.xor_c() }
      0xAA => { debug!("XOR D"); self.xor_d() }
      0xAB => { debug!("XOR E"); self.xor_e() }
      0xAC => { debug!("XOR H"); self.xor_h() }
      0xAD => { debug!("XOR L"); self.xor_l() }
      0xEE => { debug!("XOR n"); let value = mmu.read(self.pc); shared_xor_n(self, value); self.pc += 1; }
      _ => self.explode(format!("Unexpected opcode: {:#X}", opcode)),
    }

    let raw_cycles = if opcode == 0xCB {
      opcode_cycles::cb(cb_opcode.unwrap())
    } else {
      if self.branch_taken {
        self.branch_taken = false;
        opcode_cycles::branch(opcode)
      } else {
        opcode_cycles::regular(opcode)
      }
    };
    raw_cycles * 4
  }

  // Opcode implementations

  fn nop(&self) {}

  fn jp_nn(&mut self, mmu: &mmu::MMU) {
    self.pc = mmu.read_word(self.pc);
  }

  fn xor_a(&mut self) {
    let a = self.af.read_hi();
    shared_xor_n(self, a);
  }

  fn xor_b(&mut self) {
    let b = self.bc.read_hi();
    shared_xor_n(self, b);
  }

  fn xor_c(&mut self) {
    let c = self.bc.read_lo();
    shared_xor_n(self, c);
  }

  fn xor_d(&mut self) {
    let d = self.de.read_hi();
    shared_xor_n(self, d);
  }

  fn xor_e(&mut self) {
    let e = self.de.read_lo();
    shared_xor_n(self, e);
  }

  fn xor_h(&mut self) {
    let h = self.hl.read_hi();
    shared_xor_n(self, h);
  }

  fn xor_l(&mut self) {
    let l = self.hl.read_lo();
    shared_xor_n(self, l);
  }

  fn ld_hl_nn(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read_word(self.pc);
    self.write_word_reg(RegEnum::HL, value);
    self.pc += 2;
  }

  fn ld_hl_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.pc);
    let address = self.read_word_reg(RegEnum::HL);
    mmu.write(address, value);

    self.pc += 1;
  }

  fn ld_b_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_ld_n_n(self, RegEnum::B, value);
  }

  fn ld_c_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_ld_n_n(self, RegEnum::C, value);
  }

  fn ld_d_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_ld_n_n(self, RegEnum::D, value);
  }

  fn ld_e_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_ld_n_n(self, RegEnum::E, value);
  }

  fn ld_hld_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let address = self.read_word_reg(RegEnum::HL);
    mmu.write(address, value);
    self.write_word_reg(RegEnum::HL, address - 1);
  }

  fn ld_a_hld(&mut self, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    self.write_byte_reg(RegEnum::A, value);
    self.write_word_reg(RegEnum::HL, address - 1);
  }

  fn dec_a(&mut self) {
    shared_dec_byte_reg(self, RegEnum::A);
  }

  fn dec_b(&mut self) {
    shared_dec_byte_reg(self, RegEnum::B);
  }

  fn dec_c(&mut self) {
    shared_dec_byte_reg(self, RegEnum::C);
  }

  fn dec_d(&mut self) {
    shared_dec_byte_reg(self, RegEnum::D);
  }

  fn dec_e(&mut self) {
    shared_dec_byte_reg(self, RegEnum::E);
  }

  fn dec_l(&mut self) {
    shared_dec_byte_reg(self, RegEnum::L);
  }

  fn jr_nz_n(&mut self, mmu: &mmu::MMU) {
    let offset = mmu.read(self.pc) as i8;
    self.pc = self.pc.wrapping_add(1);

    if !self.util_is_flag_set(FLAG_ZERO) {
      self.pc = self.pc.wrapping_add(offset as u16);
      self.branch_taken = true;
    }
  }

  fn jr_c_n(&mut self, mmu: &mmu::MMU) {
    let offset = mmu.read(self.pc) as i8;
    self.pc = self.pc.wrapping_add(1);

    if self.util_is_flag_set(FLAG_CARRY) {
      self.pc = self.pc.wrapping_add(offset as u16);
      self.branch_taken = true;
    }
  }

  fn jr_nc_n(&mut self, mmu: &mmu::MMU) {
    let offset = mmu.read(self.pc) as i8;
    self.pc = self.pc.wrapping_add(1);

    if !self.util_is_flag_set(FLAG_CARRY) {
      self.pc = self.pc.wrapping_add(offset as u16);
      self.branch_taken = true;
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

  fn ld_a_e(&mut self) {
    let operand = self.read_byte_reg(RegEnum::E);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn ld_a_h(&mut self) {
    let operand = self.read_byte_reg(RegEnum::H);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn ld_a_l(&mut self) {
    let operand = self.read_byte_reg(RegEnum::L);
    self.write_byte_reg(RegEnum::A, operand)
  }

  fn ld_b_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::B, operand)
  }

  fn ld_b_b(&mut self) {
    let operand = self.read_byte_reg(RegEnum::B);
    self.write_byte_reg(RegEnum::B, operand)
  }

  fn ld_c_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::C, operand)
  }

  fn ld_d_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::D, operand)
  }

  fn ld_e_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::E, operand)
  }

  fn ld_e_l(&mut self) {
    let operand = self.read_byte_reg(RegEnum::L);
    self.write_byte_reg(RegEnum::E, operand)
  }

  fn ld_d_h(&mut self) {
    let operand = self.read_byte_reg(RegEnum::H);
    self.write_byte_reg(RegEnum::D, operand)
  }

  fn ld_l_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::L, operand)
  }

  fn ld_l_c(&mut self) {
    let operand = self.read_byte_reg(RegEnum::C);
    self.write_byte_reg(RegEnum::L, operand)
  }

  fn ld_l_e(&mut self) {
    let operand = self.read_byte_reg(RegEnum::E);
    self.write_byte_reg(RegEnum::L, operand)
  }

  fn ld_h_a(&mut self) {
    let operand = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::H, operand)
  }

  fn ld_h_b(&mut self) {
    let operand = self.read_byte_reg(RegEnum::B);
    self.write_byte_reg(RegEnum::H, operand)
  }

  fn ld_h_d(&mut self) {
    let operand = self.read_byte_reg(RegEnum::D);
    self.write_byte_reg(RegEnum::H, operand)
  }

  fn ld_h_c(&mut self) {
    let operand = self.read_byte_reg(RegEnum::C);
    self.write_byte_reg(RegEnum::H, operand)
  }

  fn shared_ld_reg_reg(&mut self, source_reg: RegEnum, dest_reg: RegEnum) {
    let operand = self.read_byte_reg(source_reg);
    self.write_byte_reg(dest_reg, operand);
  }

  fn ld_a_n(&mut self, mmu: &mmu::MMU) {
    let operand = mmu.read(self.pc);
    self.write_byte_reg(RegEnum::A, operand);
    self.pc += 1;
  }

  fn ld_l_n(&mut self, mmu: &mmu::MMU) {
    let operand = mmu.read(self.pc);
    self.write_byte_reg(RegEnum::L, operand);
    self.pc += 1;
  }

  fn ld_0xff00_plus_n_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let operand = mmu.read(self.pc) as u16;

    mmu.write(0xFF00 + operand, value);

    self.pc += 1;
  }

  fn ld_a_0xff00_plus_n(&mut self, mmu: &mut mmu::MMU) {
    let operand = mmu.read(self.pc) as u16;
    let value = mmu.read(0xFF00 + operand);

    self.write_byte_reg(RegEnum::A, value);
    self.pc += 1;
  }

  fn cp_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.pc);
    let a = self.read_byte_reg(RegEnum::A);
    shared_cp(self, value);
    self.write_byte_reg(RegEnum::A, a);
    self.pc += 1;
  }

  fn cp_hl(&mut self, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    shared_cp(self, value);
  }

  fn ld_nn_sp(&mut self, mmu: &mut mmu::MMU) {
    let address = mmu.read_word(self.pc);
    let value = self.read_word_reg(RegEnum::SP);
    mmu.write_word(address, value);

    self.pc += 2;
  }

  fn ld_nn_a(&mut self, mmu: &mut mmu::MMU) {
    let address = mmu.read_word(self.pc);
    let value = self.read_byte_reg(RegEnum::A);
    mmu.write(address, value);

    self.pc += 2;
  }

  fn ld_sp_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.pc);
    self.pc += 2;

    self.write_word_reg(RegEnum::SP, value);
  }

  fn ld_a_hli(&mut self, mmu: &mmu::MMU) {
    // Put value at address HL into A. Increment HL
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);

    self.write_byte_reg(RegEnum::A, value);
    self.write_word_reg(RegEnum::HL, address.wrapping_add(1));
  }

  fn ld_0xff00_plus_c_a(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_byte_reg(RegEnum::A);
    let operand = self.read_byte_reg(RegEnum::C);

    mmu.write(0xFF00 + operand as u16, value);
  }

  fn inc_a(&mut self) {
    shared_inc_byte_reg(self, RegEnum::A);
  }

  fn inc_b(&mut self) {
    shared_inc_byte_reg(self, RegEnum::B);
  }

  fn inc_c(&mut self) {
    shared_inc_byte_reg(self, RegEnum::C);
  }

  fn inc_e(&mut self) {
    shared_inc_byte_reg(self, RegEnum::E);
  }

  fn inc_h(&mut self) {
    shared_inc_byte_reg(self, RegEnum::H);
  }

  fn inc_l(&mut self) {
    shared_inc_byte_reg(self, RegEnum::L);
  }

  fn call_nn(&mut self, mmu: &mut mmu::MMU) {
    let current_pc = self.pc; // Get current PC
    self.pc = mmu.read_word(self.pc); // set PC to nn
    self.stack_push(current_pc + 2, mmu); // Push onto stack current PC + 2
  }

  fn call_nz_nn(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      self.call_nn(mmu)
    }
  }

  fn ld_bc_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.pc);
    self.write_word_reg(RegEnum::BC, value);

    self.pc += 2;
  }

  fn dec_bc(&mut self) {
    shared_dec_word_reg(self, RegEnum::BC);
  }

  fn or_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_or_n(self, value);
    self.pc += 1;
  }

  fn or_b(&mut self) {
    let value = self.read_byte_reg(RegEnum::B); shared_or_n(self, value);
  }

  fn or_c(&mut self) {
    let value = self.read_byte_reg(RegEnum::C);
    shared_or_n(self, value);
  }

  fn ret(&mut self, mmu: &mmu::MMU) {
    self.pc = self.stack_pop(mmu);
  }

  fn ld_hl_a(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::HL);
    mmu.write(addr, self.read_byte_reg(RegEnum::A));
  }

  fn ld_hl_c(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::HL);
    mmu.write(addr, self.read_byte_reg(RegEnum::C));
  }

  fn ld_hl_d(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::HL);
    mmu.write(addr, self.read_byte_reg(RegEnum::D));
  }

  fn ld_hl_e(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::HL);
    mmu.write(addr, self.read_byte_reg(RegEnum::E));
  }

  fn ld_de_a(&mut self, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(RegEnum::DE);
    mmu.write(addr, self.read_byte_reg(RegEnum::A));
  }

  fn shared_ld_nn_reg(&mut self, source_reg: RegEnum, dest_reg_addr: RegEnum, mmu: &mut mmu::MMU) {
    let addr = self.read_word_reg(dest_reg_addr);
    mmu.write(addr, self.read_byte_reg(source_reg));
  }

  fn and_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_and_n(self, value);

    self.pc += 1;
  }

  fn jr_z_n(&mut self, mmu: &mmu::MMU) {
    if self.util_is_flag_set(FLAG_ZERO) {
      let operand_dest = mmu.read(self.pc) as i8;
      self.pc = self.pc.wrapping_add((1 + operand_dest) as u16);

      self.branch_taken = true;
    } else {
      self.pc += 1;
    }
  }

  fn inc_bc(&mut self) {
    shared_inc_word_reg(self, RegEnum::BC);
  }

  fn inc_de(&mut self) {
    shared_inc_word_reg(self, RegEnum::DE);
  }

  fn inc_hl(&mut self) {
    shared_inc_word_reg(self, RegEnum::HL);
  }

  fn inc_hl_indirect(&mut self, mmu: &mut mmu::MMU) {
    let hl = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(hl);
    let result = value.wrapping_add(1);
    mmu.write(hl, result);

    if self.util_is_flag_set(FLAG_CARRY) { self.util_set_flag(FLAG_CARRY) } else { self.util_clear_all_flags() }
    self.util_toggle_zero_flag_from_result(result);

    if (result & 0x0F) == 0x00 {
      self.util_toggle_flag(FLAG_HALF_CARRY);
    }
  }

  fn dec_hl_indirect(&mut self, mmu: &mut mmu::MMU) {
    let hl = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(hl);
    let result = value.wrapping_sub(1);
    mmu.write(hl, result);

    if self.util_is_flag_set(FLAG_CARRY) { self.util_set_flag(FLAG_CARRY) } else { self.util_clear_all_flags() }
    self.util_toggle_flag(FLAG_SUB);
    self.util_toggle_zero_flag_from_result(result);

    if (result & 0x0F) == 0x0F {
      self.util_toggle_flag(FLAG_HALF_CARRY);
    }
  }

  fn ei(&mut self) {
    self.ei_cycles = 2;
  }

  fn di(&mut self) {
    self.di_cycles = 2;
  }

  fn cpl(&mut self) {
    let value = self.read_byte_reg(RegEnum::A);
    self.write_byte_reg(RegEnum::A, !value);

    self.util_toggle_flag(FLAG_HALF_CARRY);
    self.util_toggle_flag(FLAG_SUB);
  }

  fn push_af(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_word_reg(RegEnum::AF);
    self.stack_push(value, mmu);
  }

  fn push_bc(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_word_reg(RegEnum::BC);
    self.stack_push(value, mmu);
  }

  fn push_de(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_word_reg(RegEnum::DE);
    self.stack_push(value, mmu);
  }

  fn push_hl(&mut self, mmu: &mut mmu::MMU) {
    let value = self.read_word_reg(RegEnum::HL);
    self.stack_push(value, mmu);
  }

  fn and_a(&mut self) {
    let value = self.read_byte_reg(RegEnum::A);
    shared_and_n(self, value);
  }

  fn and_b(&mut self) {
    let value = self.read_byte_reg(RegEnum::B);
    shared_and_n(self, value);
  }

  fn and_c(&mut self) {
    let value = self.read_byte_reg(RegEnum::C);
    shared_and_n(self, value);
  }

  fn and_d(&mut self) {
    let value = self.read_byte_reg(RegEnum::D);
    shared_and_n(self, value);
  }

  fn and_e(&mut self) {
    let value = self.read_byte_reg(RegEnum::E);
    shared_and_n(self, value);
  }

  fn and_h(&mut self) {
    let value = self.read_byte_reg(RegEnum::H);
    shared_and_n(self, value);
  }

  fn and_l(&mut self) {
    let value = self.read_byte_reg(RegEnum::L);
    shared_and_n(self, value);
  }

  fn ret_nz(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      self.pc = self.stack_pop(mmu);
      self.branch_taken = true;
    }
  }

  fn ret_c(&mut self, mmu: &mut mmu::MMU) {
    if self.util_is_flag_set(FLAG_CARRY) {
      self.pc = self.stack_pop(mmu);
      self.branch_taken = true;
    }
  }

  fn ret_nc(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_CARRY) {
      self.pc = self.stack_pop(mmu);
      self.branch_taken = true;
    }
  }

  fn ld_a_nn(&mut self, mmu: &mut mmu::MMU) {
    let address = mmu.read_word(self.pc);
    let value = mmu.read(address);
    self.write_byte_reg(RegEnum::A, value);

    self.pc += 2;
  }

  fn ret_z(&mut self, mmu: &mut mmu::MMU) {
    if self.util_is_flag_set(FLAG_ZERO) {
      self.pc = self.stack_pop(mmu);
      self.branch_taken = true;
    }
  }

  fn pop_af(&mut self, mmu: &mut mmu::MMU) {
    let current_sp = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_sp);
    self.write_word_reg(RegEnum::AF, value);
    self.stack_pop(mmu);
  }

  fn pop_bc(&mut self, mmu: &mut mmu::MMU) {
    let value = self.stack_pop(mmu);
    self.write_word_reg(RegEnum::BC, value); // TODO do the same change to the other pop_ operations?
  }

  fn pop_de(&mut self, mmu: &mut mmu::MMU) {
    let current_sp = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_sp);
    self.write_word_reg(RegEnum::DE, value);
    self.stack_pop(mmu);
  }

  fn pop_hl(&mut self, mmu: &mut mmu::MMU) {
    let current_sp = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_sp);
    self.write_word_reg(RegEnum::HL, value);
    self.stack_pop(mmu);
  }

  fn reti(&mut self, mmu: &mut mmu::MMU) {
    self.pc = self.stack_pop(mmu);
    self.ei_cycles = 1;
  }

  fn rst_28h(&mut self, mmu: &mut mmu::MMU) {
    let current_pc = self.pc;
    self.stack_push(current_pc, mmu);
    self.pc = 0x28;
  }

  fn rst_38h(&mut self, mmu: &mut mmu::MMU) {
    let current_pc = self.pc;
    self.stack_push(current_pc, mmu);
    self.pc = 0x38;
  }

  fn add_a_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.pc);
    shared_add_n(self, value, false);
    self.pc += 1;
  }

  fn add_a_hl(&mut self, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    shared_add_n(self, value, false);
  }

  fn add_hl_bc(&mut self) {
    shared_add_word_and_word_regs(self, RegEnum::HL, RegEnum::BC);
  }

  fn add_hl_de(&mut self) {
    shared_add_word_and_word_regs(self, RegEnum::HL, RegEnum::DE);
  }

  fn ld_b_hl(&mut self, mmu: &mut mmu::MMU) {
    shared_ld_n_nn(self, mmu, RegEnum::B, RegEnum::HL);
  }

  fn ld_c_hl(&mut self, mmu: &mut mmu::MMU) {
    shared_ld_n_nn(self, mmu, RegEnum::C, RegEnum::HL);
  }

  fn ld_d_hl(&mut self, mmu: &mut mmu::MMU) {
    shared_ld_n_nn(self, mmu, RegEnum::D, RegEnum::HL);
  }

  fn ld_e_hl(&mut self, mmu: &mut mmu::MMU) {
    shared_ld_n_nn(self, mmu, RegEnum::E, RegEnum::HL);
  }

  fn jp_hl(&mut self) {
    let address = self.read_word_reg(RegEnum::HL);
    self.pc = address;
  }

  fn ld_de_nn(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read_word(self.pc);
    self.write_word_reg(RegEnum::DE, value);

    self.pc += 2;
  }

  fn ld_a_de(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.read_word_reg(RegEnum::DE));
    self.write_byte_reg(RegEnum::A, value);
  }

  fn ld_hli_a(&mut self, mmu: &mut mmu::MMU) {
    self.ld_hl_a(mmu);
    self.inc_hl();
  }

  fn jp_z_nn(&mut self, mmu: &mut mmu::MMU) {
    if self.util_is_flag_set(FLAG_ZERO) {
      let address = mmu.read_word(self.pc);
      self.pc = address;
      self.branch_taken = true;
    } else {
      self.pc += 2;
    }
  }

  fn jp_nz_nn(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      let address = mmu.read_word(self.pc);
      self.pc = address;
      self.branch_taken = true;
    } else {
      self.pc += 2;
    }
  }

  fn ld_a_bc(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.read_word_reg(RegEnum::BC));
    self.write_byte_reg(RegEnum::A, value);
  }

  fn ld_a_hl(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.read_word_reg(RegEnum::HL));
    self.write_byte_reg(RegEnum::A, value);
  }

  fn jr_n(&mut self, mmu: &mut mmu::MMU) {
    let operand_dest = mmu.read(self.pc) as i8;
    self.pc = self.pc.wrapping_add((1 + operand_dest) as u16);
  }

  fn daa(&mut self) {
    let mut a = self.read_byte_reg(RegEnum::A);
    let mut add_or_subtract = if self.util_is_flag_set(FLAG_CARRY) { 0x60 } else { 0x00 };

    if self.util_is_flag_set(FLAG_HALF_CARRY) {
      add_or_subtract |= 0x06;
    }

    if self.util_is_flag_set(FLAG_SUB) {
      a = a.wrapping_sub(add_or_subtract);
    } else {
      if a & 0x0F > 0x09 { add_or_subtract |= 0x06; }
      if a > 0x99 { add_or_subtract |= 0x60; }
      a = a.wrapping_add(add_or_subtract);
    }

    self.util_set_flag_by_boolean(FLAG_ZERO, a == 0);
    self.util_set_flag_by_boolean(FLAG_HALF_CARRY, false);
    self.util_set_flag_by_boolean(FLAG_CARRY, add_or_subtract >= 0x60);
    self.write_byte_reg(RegEnum::A, a);
  }

  // Helpers

  fn cb_prefixed_instruction(&mut self, cb_opcode: u8, mmu: &mut mmu::MMU) {
    self.execute_cb_opcode(cb_opcode, mmu);
    self.pc += 1;
  }

  fn stack_push(&mut self, word: u16, mmu: &mut mmu::MMU) {
    let current_sp = self.read_word_reg(RegEnum::SP); // TODO just manipulate .value directly
    self.write_word_reg(RegEnum::SP, current_sp - 2);
    mmu.write_word(self.read_word_reg(RegEnum::SP), word);
  }

  fn stack_pop(&mut self, mmu: &mmu::MMU) -> u16 {
    let current_sp = self.read_word_reg(RegEnum::SP);
    let popped_value = mmu.read_word(current_sp);
    self.sp.write(current_sp + 2);

    popped_value
  }

  fn util_set_flag_by_boolean(&mut self, flag: u8, b: bool) {
    if b {
      self.util_toggle_flag(flag);
    } else {
      self.util_untoggle_flag(flag);
    }
  }

  fn util_toggle_zero_flag_from_result(&mut self, result: u8) {
    if result == 0 {
      self.util_toggle_flag(FLAG_ZERO);
    } else {
      self.util_untoggle_flag(FLAG_ZERO);
    }
  }

  fn util_clear_all_flags(&mut self) {
    self.util_set_flag(FLAG_NONE);
  }

  fn util_set_flag(&mut self, byte: u8) {
    self.af.write_lo(byte);
  }

  fn util_is_flag_set(&self, byte: u8) -> bool {
    (self.af.read_lo() & byte) != 0
  }

  fn util_toggle_flag(&mut self, byte: u8) {
    let previous_flags = self.af.read_lo();
    self.af.write_lo(previous_flags | byte);
  }

  fn util_untoggle_flag(&mut self, byte: u8) {
    let previous_flags = self.af.read_lo();
    self.af.write_lo(previous_flags & (!byte));
  }

  // CB opcode handling

  fn execute_cb_opcode(&mut self, opcode: u8, mmu: &mut mmu::MMU) {
    match opcode {
      0x46 => { debug!("CB: BIT 0 (HL)"); shared_bit_n_hl(self, 0, mmu) }
      0x47 => { debug!("CB: BIT 0 A"); shared_bit_n_reg(self, 0, RegEnum::A) }
      0x40 => { debug!("CB: BIT 0 B"); shared_bit_n_reg(self, 0, RegEnum::B) }
      0x41 => { debug!("CB: BIT 0 C"); shared_bit_n_reg(self, 0, RegEnum::C) }
      0x42 => { debug!("CB: BIT 0 D"); shared_bit_n_reg(self, 0, RegEnum::D) }
      0x43 => { debug!("CB: BIT 0 E"); shared_bit_n_reg(self, 0, RegEnum::E) }
      0x44 => { debug!("CB: BIT 0 H"); shared_bit_n_reg(self, 0, RegEnum::H) }
      0x45 => { debug!("CB: BIT 0 L"); shared_bit_n_reg(self, 0, RegEnum::L) }
      0x4E => { debug!("CB: BIT 1 (HL)"); shared_bit_n_hl(self, 1, mmu) }
      0x4F => { debug!("CB: BIT 1 A"); shared_bit_n_reg(self, 1, RegEnum::A) }
      0x48 => { debug!("CB: BIT 1 B"); shared_bit_n_reg(self, 1, RegEnum::B) }
      0x49 => { debug!("CB: BIT 1 C"); shared_bit_n_reg(self, 1, RegEnum::C) }
      0x4A => { debug!("CB: BIT 1 D"); shared_bit_n_reg(self, 1, RegEnum::D) }
      0x4B => { debug!("CB: BIT 1 E"); shared_bit_n_reg(self, 1, RegEnum::E) }
      0x4C => { debug!("CB: BIT 1 H"); shared_bit_n_reg(self, 1, RegEnum::H) }
      0x4D => { debug!("CB: BIT 1 L"); shared_bit_n_reg(self, 1, RegEnum::L) }
      0x56 => { debug!("CB: BIT 2 (HL)"); shared_bit_n_hl(self, 2, mmu) }
      0x57 => { debug!("CB: BIT 2 A"); shared_bit_n_reg(self, 2, RegEnum::A) }
      0x50 => { debug!("CB: BIT 2 B"); shared_bit_n_reg(self, 2, RegEnum::B) }
      0x51 => { debug!("CB: BIT 2 C"); shared_bit_n_reg(self, 2, RegEnum::C) }
      0x52 => { debug!("CB: BIT 2 D"); shared_bit_n_reg(self, 2, RegEnum::D) }
      0x53 => { debug!("CB: BIT 2 E"); shared_bit_n_reg(self, 2, RegEnum::E) }
      0x54 => { debug!("CB: BIT 2 H"); shared_bit_n_reg(self, 2, RegEnum::H) }
      0x55 => { debug!("CB: BIT 2 L"); shared_bit_n_reg(self, 2, RegEnum::L) }
      0x5E => { debug!("CB: BIT 3 (HL)"); shared_bit_n_hl(self, 3, mmu) }
      0x5F => { debug!("CB: BIT 3 A"); shared_bit_n_reg(self, 3, RegEnum::A) }
      0x58 => { debug!("CB: BIT 3 B"); shared_bit_n_reg(self, 3, RegEnum::B) }
      0x59 => { debug!("CB: BIT 3 C"); shared_bit_n_reg(self, 3, RegEnum::C) }
      0x5A => { debug!("CB: BIT 3 D"); shared_bit_n_reg(self, 3, RegEnum::D) }
      0x5B => { debug!("CB: BIT 3 E"); shared_bit_n_reg(self, 3, RegEnum::E) }
      0x5C => { debug!("CB: BIT 3 H"); shared_bit_n_reg(self, 3, RegEnum::H) }
      0x5D => { debug!("CB: BIT 3 L"); shared_bit_n_reg(self, 3, RegEnum::L) }
      0x66 => { debug!("CB: BIT 4 (HL)"); shared_bit_n_hl(self, 4, mmu) }
      0x67 => { debug!("CB: BIT 4 A"); shared_bit_n_reg(self, 4, RegEnum::A) }
      0x60 => { debug!("CB: BIT 4 B"); shared_bit_n_reg(self, 4, RegEnum::B) }
      0x61 => { debug!("CB: BIT 4 C"); shared_bit_n_reg(self, 4, RegEnum::C) }
      0x62 => { debug!("CB: BIT 4 D"); shared_bit_n_reg(self, 4, RegEnum::D) }
      0x63 => { debug!("CB: BIT 4 E"); shared_bit_n_reg(self, 4, RegEnum::E) }
      0x64 => { debug!("CB: BIT 4 H"); shared_bit_n_reg(self, 4, RegEnum::H) }
      0x65 => { debug!("CB: BIT 4 L"); shared_bit_n_reg(self, 4, RegEnum::L) }
      0x6E => { debug!("CB: BIT 5 (HL)"); shared_bit_n_hl(self, 5, mmu) }
      0x6F => { debug!("CB: BIT 5 A"); shared_bit_n_reg(self, 5, RegEnum::A) }
      0x68 => { debug!("CB: BIT 5 B"); shared_bit_n_reg(self, 5, RegEnum::B) }
      0x69 => { debug!("CB: BIT 5 C"); shared_bit_n_reg(self, 5, RegEnum::C) }
      0x6A => { debug!("CB: BIT 5 D"); shared_bit_n_reg(self, 5, RegEnum::D) }
      0x6B => { debug!("CB: BIT 5 E"); shared_bit_n_reg(self, 5, RegEnum::E) }
      0x6C => { debug!("CB: BIT 5 H"); shared_bit_n_reg(self, 5, RegEnum::H) }
      0x6D => { debug!("CB: BIT 5 L"); shared_bit_n_reg(self, 5, RegEnum::L) }
      0x76 => { debug!("CB: BIT 6 (HL)"); shared_bit_n_hl(self, 6, mmu) }
      0x77 => { debug!("CB: BIT 6 A"); shared_bit_n_reg(self, 6, RegEnum::A) }
      0x70 => { debug!("CB: BIT 6 B"); shared_bit_n_reg(self, 6, RegEnum::B) }
      0x71 => { debug!("CB: BIT 6 C"); shared_bit_n_reg(self, 6, RegEnum::C) }
      0x72 => { debug!("CB: BIT 6 D"); shared_bit_n_reg(self, 6, RegEnum::D) }
      0x73 => { debug!("CB: BIT 6 E"); shared_bit_n_reg(self, 6, RegEnum::E) }
      0x74 => { debug!("CB: BIT 6 H"); shared_bit_n_reg(self, 6, RegEnum::H) }
      0x75 => { debug!("CB: BIT 6 L"); shared_bit_n_reg(self, 6, RegEnum::L) }
      0x7E => { debug!("CB: BIT 7 (HL)"); shared_bit_n_hl(self, 7, mmu) }
      0x7F => { debug!("CB: BIT 7 A"); shared_bit_n_reg(self, 7, RegEnum::A) }
      0x78 => { debug!("CB: BIT 7 B"); shared_bit_n_reg(self, 7, RegEnum::B) }
      0x79 => { debug!("CB: BIT 7 C"); shared_bit_n_reg(self, 7, RegEnum::C) }
      0x7A => { debug!("CB: BIT 7 D"); shared_bit_n_reg(self, 7, RegEnum::D) }
      0x7B => { debug!("CB: BIT 7 E"); shared_bit_n_reg(self, 7, RegEnum::E) }
      0x7C => { debug!("CB: BIT 7 H"); shared_bit_n_reg(self, 7, RegEnum::H) }
      0x7D => { debug!("CB: BIT 7 L"); shared_bit_n_reg(self, 7, RegEnum::L) }
      0x86 => { debug!("CB: RES 0 (HL)"); self.res_bit_hl(0, mmu) }
      0x87 => { debug!("CB: RES 0 A"); self.res_bit_a(0) }
      0x80 => self.explode(format!("CB: RES 0 B {:#X}", opcode)),
      0x81 => self.explode(format!("CB: RES 0 C {:#X}", opcode)),
      0x82 => self.explode(format!("CB: RES 0 D {:#X}", opcode)),
      0x83 => self.explode(format!("CB: RES 0 E {:#X}", opcode)),
      0x84 => self.explode(format!("CB: RES 0 H {:#X}", opcode)),
      0x85 => self.explode(format!("CB: RES 0 L {:#X}", opcode)),
      0x8E => self.explode(format!("CB: RES 1 (HL) {:#X}", opcode)),
      0x8F => self.explode(format!("CB: RES 1 A {:#X}", opcode)),
      0x88 => self.explode(format!("CB: RES 1 B {:#X}", opcode)),
      0x89 => self.explode(format!("CB: RES 1 C {:#X}", opcode)),
      0x8A => self.explode(format!("CB: RES 1 D {:#X}", opcode)),
      0x8B => self.explode(format!("CB: RES 1 E {:#X}", opcode)),
      0x8C => self.explode(format!("CB: RES 1 H {:#X}", opcode)),
      0x8D => self.explode(format!("CB: RES 1 L {:#X}", opcode)),
      0x96 => self.explode(format!("CB: RES 2 (HL) {:#X}", opcode)),
      0x97 => self.explode(format!("CB: RES 2 A {:#X}", opcode)),
      0x90 => self.explode(format!("CB: RES 2 B {:#X}", opcode)),
      0x91 => self.explode(format!("CB: RES 2 C {:#X}", opcode)),
      0x92 => self.explode(format!("CB: RES 2 D {:#X}", opcode)),
      0x93 => self.explode(format!("CB: RES 2 E {:#X}", opcode)),
      0x94 => self.explode(format!("CB: RES 2 H {:#X}", opcode)),
      0x95 => self.explode(format!("CB: RES 2 L {:#X}", opcode)),
      0x9E => { debug!("CB: RES 3 (HL)"); self.shared_reset_n_hl(3, mmu) }
      0x9F => self.explode(format!("CB: RES 3 A {:#X}", opcode)),
      0x98 => self.explode(format!("CB: RES 3 B {:#X}", opcode)),
      0x99 => self.explode(format!("CB: RES 3 C {:#X}", opcode)),
      0x9A => self.explode(format!("CB: RES 3 D {:#X}", opcode)),
      0x9B => self.explode(format!("CB: RES 3 E {:#X}", opcode)),
      0x9C => self.explode(format!("CB: RES 3 H {:#X}", opcode)),
      0x9D => self.explode(format!("CB: RES 3 L {:#X}", opcode)),
      0xA6 => self.explode(format!("CB: RES 4 (HL) {:#X}", opcode)),
      0xA7 => self.explode(format!("CB: RES 4 A {:#X}", opcode)),
      0xA0 => self.explode(format!("CB: RES 4 B {:#X}", opcode)),
      0xA1 => self.explode(format!("CB: RES 4 C {:#X}", opcode)),
      0xA2 => self.explode(format!("CB: RES 4 D {:#X}", opcode)),
      0xA3 => self.explode(format!("CB: RES 4 E {:#X}", opcode)),
      0xA4 => self.explode(format!("CB: RES 4 H {:#X}", opcode)),
      0xA5 => self.explode(format!("CB: RES 4 L {:#X}", opcode)),
      0xAE => self.explode(format!("CB: RES 5 (HL) {:#X}", opcode)),
      0xAF => self.explode(format!("CB: RES 5 A {:#X}", opcode)),
      0xA8 => self.explode(format!("CB: RES 5 B {:#X}", opcode)),
      0xA9 => self.explode(format!("CB: RES 5 C {:#X}", opcode)),
      0xAA => self.explode(format!("CB: RES 5 D {:#X}", opcode)),
      0xAB => self.explode(format!("CB: RES 5 E {:#X}", opcode)),
      0xAC => self.explode(format!("CB: RES 5 H {:#X}", opcode)),
      0xAD => self.explode(format!("CB: RES 5 L {:#X}", opcode)),
      0xB6 => self.explode(format!("CB: RES 6 (HL) {:#X}", opcode)),
      0xB7 => self.explode(format!("CB: RES 6 A {:#X}", opcode)),
      0xB0 => self.explode(format!("CB: RES 6 B {:#X}", opcode)),
      0xB1 => self.explode(format!("CB: RES 6 C {:#X}", opcode)),
      0xB2 => self.explode(format!("CB: RES 6 D {:#X}", opcode)),
      0xB3 => self.explode(format!("CB: RES 6 E {:#X}", opcode)),
      0xB4 => self.explode(format!("CB: RES 6 H {:#X}", opcode)),
      0xB5 => self.explode(format!("CB: RES 6 L {:#X}", opcode)),
      0xBE => { debug!("CB: RES 7 (HL)"); self.shared_reset_n_hl(7, mmu) }
      0xBF => self.explode(format!("CB: RES 7 A {:#X}", opcode)),
      0xB8 => self.explode(format!("CB: RES 7 B {:#X}", opcode)),
      0xB9 => self.explode(format!("CB: RES 7 C {:#X}", opcode)),
      0xBA => self.explode(format!("CB: RES 7 D {:#X}", opcode)),
      0xBB => self.explode(format!("CB: RES 7 E {:#X}", opcode)),
      0xBC => self.explode(format!("CB: RES 7 H {:#X}", opcode)),
      0xBD => self.explode(format!("CB: RES 7 L {:#X}", opcode)),
      0x16 => self.explode(format!("CB: RL (HL) {:#X}", opcode)),
      0x17 => self.explode(format!("CB: RL A {:#X}", opcode)),
      0x10 => self.explode(format!("CB: RL B {:#X}", opcode)),
      0x11 => { debug!("CB: RL C"); self.rl_c() },
      0x12 => self.explode(format!("CB: RL D {:#X}", opcode)),
      0x13 => self.explode(format!("CB: RL E {:#X}", opcode)),
      0x14 => self.explode(format!("CB: RL H {:#X}", opcode)),
      0x15 => self.explode(format!("CB: RL L {:#X}", opcode)),
      0x06 => self.explode(format!("CB: RLC (HL) {:#X}", opcode)),
      0x07 => self.explode(format!("CB: RLC A {:#X}", opcode)),
      0x00 => self.explode(format!("CB: RLC B {:#X}", opcode)),
      0x01 => self.explode(format!("CB: RLC C {:#X}", opcode)),
      0x02 => self.explode(format!("CB: RLC D {:#X}", opcode)),
      0x03 => self.explode(format!("CB: RLC E {:#X}", opcode)),
      0x04 => self.explode(format!("CB: RLC H {:#X}", opcode)),
      0x05 => self.explode(format!("CB: RLC L {:#X}", opcode)),
      0x1E => self.explode(format!("CB: RR (HL) {:#X}", opcode)),
      0x1F => self.explode(format!("CB: RR A {:#X}", opcode)),
      0x18 => self.explode(format!("CB: RR B {:#X}", opcode)),
      0x19 => self.explode(format!("CB: RR C {:#X}", opcode)),
      0x1A => self.explode(format!("CB: RR D {:#X}", opcode)),
      0x1B => self.explode(format!("CB: RR E {:#X}", opcode)),
      0x1C => self.explode(format!("CB: RR H {:#X}", opcode)),
      0x1D => self.explode(format!("CB: RR L {:#X}", opcode)),
      0x0E => self.explode(format!("CB: RRC (HL) {:#X}", opcode)),
      0x0F => self.explode(format!("CB: RRC A {:#X}", opcode)),
      0x08 => self.explode(format!("CB: RRC B {:#X}", opcode)),
      0x09 => self.explode(format!("CB: RRC C {:#X}", opcode)),
      0x0A => self.explode(format!("CB: RRC D {:#X}", opcode)),
      0x0B => self.explode(format!("CB: RRC E {:#X}", opcode)),
      0x0C => self.explode(format!("CB: RRC H {:#X}", opcode)),
      0x0D => self.explode(format!("CB: RRC L {:#X}", opcode)),
      0xC6 => self.explode(format!("CB: SET 0 (HL) {:#X}", opcode)),
      0xC7 => self.explode(format!("CB: SET 0 A {:#X}", opcode)),
      0xC0 => self.explode(format!("CB: SET 0 B {:#X}", opcode)),
      0xC1 => self.explode(format!("CB: SET 0 C {:#X}", opcode)),
      0xC2 => self.explode(format!("CB: SET 0 D {:#X}", opcode)),
      0xC3 => self.explode(format!("CB: SET 0 E {:#X}", opcode)),
      0xC4 => self.explode(format!("CB: SET 0 H {:#X}", opcode)),
      0xC5 => self.explode(format!("CB: SET 0 L {:#X}", opcode)),
      0xCE => self.explode(format!("CB: SET 1 (HL) {:#X}", opcode)),
      0xCF => self.explode(format!("CB: SET 1 A {:#X}", opcode)),
      0xC8 => self.explode(format!("CB: SET 1 B {:#X}", opcode)),
      0xC9 => self.explode(format!("CB: SET 1 C {:#X}", opcode)),
      0xCA => self.explode(format!("CB: SET 1 D {:#X}", opcode)),
      0xCB => self.explode(format!("CB: SET 1 E {:#X}", opcode)),
      0xCC => self.explode(format!("CB: SET 1 H {:#X}", opcode)),
      0xCD => self.explode(format!("CB: SET 1 L {:#X}", opcode)),
      0xD6 => self.explode(format!("CB: SET 2 (HL) {:#X}", opcode)),
      0xD7 => self.explode(format!("CB: SET 2 A {:#X}", opcode)),
      0xD0 => { debug!("CB: SET 2 B"); self.shared_set_n_reg(2, RegEnum::B) }
      0xD1 => { debug!("CB: SET 2 C"); self.shared_set_n_reg(2, RegEnum::C) }
      0xD2 => { debug!("CB: SET 2 D"); self.shared_set_n_reg(2, RegEnum::D) }
      0xD3 => { debug!("CB: SET 2 E"); self.shared_set_n_reg(2, RegEnum::E) }
      0xD4 => { debug!("CB: SET 2 H"); self.shared_set_n_reg(2, RegEnum::H) }
      0xD5 => { debug!("CB: SET 2 L"); self.shared_set_n_reg(2, RegEnum::L) }
      0xDE => { debug!("CB: SET 3 (HL)"); self.shared_set_n_hl(3, mmu) }
      0xDF => self.explode(format!("CB: SET 3 A {:#X}", opcode)),
      0xD8 => { debug!("CB: SET 3 B"); self.shared_set_n_reg(3, RegEnum::B) }
      0xD9 => self.explode(format!("CB: SET 3 C {:#X}", opcode)),
      0xDA => self.explode(format!("CB: SET 3 D {:#X}", opcode)),
      0xDB => self.explode(format!("CB: SET 3 E {:#X}", opcode)),
      0xDC => self.explode(format!("CB: SET 3 H {:#X}", opcode)),
      0xDD => self.explode(format!("CB: SET 3 L {:#X}", opcode)),
      0xE6 => self.explode(format!("CB: SET 4 (HL) {:#X}", opcode)),
      0xE7 => self.explode(format!("CB: SET 4 A {:#X}", opcode)),
      0xE0 => self.explode(format!("CB: SET 4 B {:#X}", opcode)),
      0xE1 => self.explode(format!("CB: SET 4 C {:#X}", opcode)),
      0xE2 => self.explode(format!("CB: SET 4 D {:#X}", opcode)),
      0xE3 => self.explode(format!("CB: SET 4 E {:#X}", opcode)),
      0xE4 => self.explode(format!("CB: SET 4 H {:#X}", opcode)),
      0xE5 => self.explode(format!("CB: SET 4 L {:#X}", opcode)),
      0xEE => self.explode(format!("CB: SET 5 (HL) {:#X}", opcode)),
      0xEF => self.explode(format!("CB: SET 5 A {:#X}", opcode)),
      0xE8 => self.explode(format!("CB: SET 5 B {:#X}", opcode)),
      0xE9 => self.explode(format!("CB: SET 5 C {:#X}", opcode)),
      0xEA => self.explode(format!("CB: SET 5 D {:#X}", opcode)),
      0xEB => self.explode(format!("CB: SET 5 E {:#X}", opcode)),
      0xEC => self.explode(format!("CB: SET 5 H {:#X}", opcode)),
      0xED => self.explode(format!("CB: SET 5 L {:#X}", opcode)),
      0xF6 => self.explode(format!("CB: SET 6 (HL) {:#X}", opcode)),
      0xF7 => self.explode(format!("CB: SET 6 A {:#X}", opcode)),
      0xF0 => self.explode(format!("CB: SET 6 B {:#X}", opcode)),
      0xF1 => self.explode(format!("CB: SET 6 C {:#X}", opcode)),
      0xF2 => self.explode(format!("CB: SET 6 D {:#X}", opcode)),
      0xF3 => self.explode(format!("CB: SET 6 E {:#X}", opcode)),
      0xF4 => self.explode(format!("CB: SET 6 H {:#X}", opcode)),
      0xF5 => self.explode(format!("CB: SET 6 L {:#X}", opcode)),
      0xFE => { debug!("CB: SET 7 (HL)"); self.shared_set_n_hl(7, mmu) }
      0xFF => self.explode(format!("CB: SET 7 A {:#X}", opcode)),
      0xF8 => { debug!("CB: SET 7 B"); self.shared_set_n_reg(7, RegEnum::B) }
      0xF9 => self.explode(format!("CB: SET 7 C {:#X}", opcode)),
      0xFA => self.explode(format!("CB: SET 7 D {:#X}", opcode)),
      0xFB => self.explode(format!("CB: SET 7 E {:#X}", opcode)),
      0xFC => self.explode(format!("CB: SET 7 H {:#X}", opcode)),
      0xFD => self.explode(format!("CB: SET 7 L {:#X}", opcode)),
      0x26 => self.explode(format!("CB: SLA (HL) {:#X}", opcode)),
      0x27 => { debug!("CB: SLA A"); self.sla_a() }
      0x20 => self.explode(format!("CB: SLA B {:#X}", opcode)),
      0x21 => self.explode(format!("CB: SLA C {:#X}", opcode)),
      0x22 => self.explode(format!("CB: SLA D {:#X}", opcode)),
      0x23 => self.explode(format!("CB: SLA E {:#X}", opcode)),
      0x24 => self.explode(format!("CB: SLA H {:#X}", opcode)),
      0x25 => self.explode(format!("CB: SLA L {:#X}", opcode)),
      0x2E => self.explode(format!("CB: SRA (HL) {:#X}", opcode)),
      0x2F => self.explode(format!("CB: SRA A {:#X}", opcode)),
      0x28 => self.explode(format!("CB: SRA B {:#X}", opcode)),
      0x29 => self.explode(format!("CB: SRA C {:#X}", opcode)),
      0x2A => self.explode(format!("CB: SRA D {:#X}", opcode)),
      0x2B => self.explode(format!("CB: SRA E {:#X}", opcode)),
      0x2C => self.explode(format!("CB: SRA H {:#X}", opcode)),
      0x2D => self.explode(format!("CB: SRA L {:#X}", opcode)),
      0x3E => self.explode(format!("CB: SRL (HL) {:#X}", opcode)),
      0x3F => { debug!("CB: SRL A"); shared_srl_n(self, RegEnum::A) },
      0x38 => self.explode(format!("CB: SRL B {:#X}", opcode)),
      0x39 => self.explode(format!("CB: SRL C {:#X}", opcode)),
      0x3A => self.explode(format!("CB: SRL D {:#X}", opcode)),
      0x3B => self.explode(format!("CB: SRL E {:#X}", opcode)),
      0x3C => self.explode(format!("CB: SRL H {:#X}", opcode)),
      0x3D => self.explode(format!("CB: SRL L {:#X}", opcode)),
      0x36 => self.explode(format!("CB: SWAP (HL) {:#X}", opcode)),
      0x37 => { debug!("CB: SWAP A"); shared_swap_register(self, RegEnum::A); }
      0x30 => self.explode(format!("CB: SWAP B {:#X}", opcode)),
      0x31 => self.explode(format!("CB: SWAP C {:#X}", opcode)),
      0x32 => self.explode(format!("CB: SWAP D {:#X}", opcode)),
      0x33 => { debug!("CB: SWAP E"); shared_swap_register(self, RegEnum::E); }
      0x34 => self.explode(format!("CB: SWAP H {:#X}", opcode)),
      0x35 => self.explode(format!("CB: SWAP L {:#X}", opcode)),
      _ => self.explode(format!("Unexpected CB opcode: {:#X}", opcode)),
    }
  }

  // CB opcodes

  fn res_bit_a(&mut self, bit: u8) {
    shared_reset_bit_reg(self, bit, RegEnum::A);
  }

  fn res_bit_hl(&mut self, bit: u8, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    let result = value & (!(0x1 << bit));
    mmu.write(address, result);
  }

  fn rl_c(&mut self) {
    shared_rl_n(self, RegEnum::C);
  }

  fn rla(&mut self) {
    shared_rl_n(self, RegEnum::A);
  }

  fn sla_a(&mut self) {
    shared_sla_n(self, RegEnum::A);
  }

  fn shared_set_n_hl(&mut self, n: u8, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address) | (1 << n);
    mmu.write(address, value);
  }

  fn shared_reset_n_hl(&mut self, n: u8, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address) & !(1 << n);
    mmu.write(address, value);
  }

  fn shared_set_n_reg(&mut self, n: u8, reg_enum: RegEnum) {
    let value = self.read_byte_reg(reg_enum) | (1 << n);
    self.write_byte_reg(reg_enum, value);
  }

  // TODO use
  #[allow(dead_code)]
  fn shared_reset_n_reg(&mut self, n: u8, reg_enum: RegEnum) {
    let value = self.read_byte_reg(reg_enum) & !(1 << n);
    self.write_byte_reg(reg_enum, value);
  }
}

fn shared_reset_bit_reg(cpu: &mut CPU, bit: u8, reg_enum: RegEnum) {
  let value = cpu.read_byte_reg(reg_enum);
  let result = value & (!(0x1 << bit));
  cpu.write_byte_reg(reg_enum, result);
}

fn shared_bit_n_reg(cpu: &mut CPU, bit: u8, reg_enum: RegEnum) {
  let reg_value = cpu.read_byte_reg(reg_enum);
  let result = reg_value & (1 << (bit as u32)) == 0;

  cpu.util_set_flag_by_boolean(FLAG_SUB, false);
  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, true);
  cpu.util_set_flag_by_boolean(FLAG_ZERO, result);
}

// TODO reduce duplication between this and shared_bit_n_reg
fn shared_bit_n_hl(cpu: &mut CPU, bit: u8, mmu: &mmu::MMU) {
  let address = cpu.read_word_reg(RegEnum::HL);
  let value = mmu.read(address);

  if (value & (1 << bit)) == 0 {
    cpu.util_set_flag(FLAG_ZERO);
  } else {
    cpu.util_untoggle_flag(FLAG_ZERO);
  }

  cpu.util_toggle_flag(FLAG_HALF_CARRY);
  cpu.util_untoggle_flag(FLAG_SUB);
}

fn shared_rl_n(cpu: &mut CPU, reg_enum: RegEnum) {
  let carry = cpu.util_is_flag_set(FLAG_CARRY);

  let mut result = cpu.read_byte_reg(reg_enum);
  if result & 0x80 != 0 {
    cpu.util_set_flag(FLAG_CARRY);
  } else {
    cpu.util_clear_all_flags();
  }

  result <<= 1;
  result |= carry as u8;

  cpu.write_byte_reg(reg_enum, result);

  if reg_enum != RegEnum::A {
    cpu.util_toggle_zero_flag_from_result(result);
  }
}

fn shared_sla_n(cpu: &mut CPU, reg_enum: RegEnum) {
  let value = cpu.read_byte_reg(reg_enum);
  if value & 0x80 != 0 {
    cpu.util_set_flag(FLAG_CARRY);
  } else {
    cpu.util_clear_all_flags();
  }

  let result = value << 1;

  cpu.write_byte_reg(reg_enum, result);
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_srl_n(cpu: &mut CPU, reg_enum: RegEnum) {
  let value = cpu.read_byte_reg(reg_enum);
  let carry = value & 0x01 == 0x01;
  let result = value >> 1;

  shared_update_srl_flags(cpu, result, carry);
  cpu.write_byte_reg(reg_enum, result);
}

fn shared_update_srl_flags(cpu: &mut CPU, result: u8, carry: bool) {
  cpu.util_untoggle_flag(FLAG_HALF_CARRY);
  cpu.util_untoggle_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, carry);
}

fn shared_ld_reg_n(cpu: &mut CPU, reg_enum: RegEnum, mmu: &mmu::MMU) {
    let operand = mmu.read(cpu.pc);
    cpu.write_byte_reg(reg_enum, operand);
    cpu.pc += 1;
  }

fn shared_swap_register(cpu: &mut CPU, reg_enum: RegEnum) {
  let value = cpu.read_byte_reg(reg_enum);
  let low_half = value & 0x0F;
  let high_half = (value >> 4) & 0x0F;
  let result = (low_half << 4) + high_half;
  cpu.write_byte_reg(reg_enum, result);

  cpu.util_clear_all_flags();
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_inc_word_reg(cpu: &mut CPU, reg_enum: RegEnum) {
  let result = cpu.read_word_reg(reg_enum).wrapping_add(1);
  cpu.write_word_reg(reg_enum, result);
}

fn shared_xor_n(cpu: &mut CPU, byte: u8) {
  let result = cpu.af.read_hi() ^ byte;
  cpu.write_byte_reg(RegEnum::A, result);
  cpu.util_clear_all_flags();
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_ld_n_n(cpu: &mut CPU, reg_enum: RegEnum, byte: u8) {
  cpu.write_byte_reg(reg_enum, byte);
  cpu.pc += 1;
}

fn shared_inc_byte_reg(cpu: &mut CPU, reg_enum: RegEnum) {
  let result = cpu.read_byte_reg(reg_enum).wrapping_add(1);
  cpu.write_byte_reg(reg_enum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x00 {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_add_n(cpu: &mut CPU, byte: u8, carry_preserve: bool) {
  let carry = if carry_preserve && cpu.util_is_flag_set(FLAG_CARRY) { 1 } else { 0 };
  let a = cpu.read_byte_reg(RegEnum::A);
  let result = a.wrapping_add(byte).wrapping_add(carry);

  cpu.util_set_flag_by_boolean(FLAG_ZERO, result == 0);
  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (a & 0xF) + (byte & 0xF) + carry > 0xF);
  cpu.util_set_flag_by_boolean(FLAG_SUB, false);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, (a as u16) + (byte as u16) + (carry as u16) > 0xFF);

  cpu.write_byte_reg(RegEnum::A, result);
}

fn shared_add_word_and_word_regs(cpu: &mut CPU, reg_enum1: RegEnum, reg_enum2: RegEnum) {
  let value_1 = cpu.read_word_reg(reg_enum1);
  let value_2 = cpu.read_word_reg(reg_enum2);

  let result = value_1.wrapping_add(value_2);

  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (value_1 & 0x07FF) + (value_2 & 0x07FF) > 0x07FF);
  cpu.util_set_flag_by_boolean(FLAG_SUB, false);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, value_1 > 0xFFFF - value_2);
  cpu.write_word_reg(reg_enum1, result);
}

fn shared_ld_n_nn(cpu: &mut CPU, mmu: &mut mmu::MMU, reg_enum_dst: RegEnum, reg_enum_src: RegEnum) {
  let address = cpu.read_word_reg(reg_enum_src);
  let value = mmu.read(address);
  cpu.write_byte_reg(reg_enum_dst, value);
}

fn shared_dec_byte_reg(cpu: &mut CPU, reg_enum: RegEnum) {
  let result = cpu.read_byte_reg(reg_enum).wrapping_sub(1);
  cpu.write_byte_reg(reg_enum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_toggle_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x0F {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_dec_word_reg(cpu: &mut CPU, reg_enum: RegEnum) {
  let result = cpu.read_word_reg(reg_enum).wrapping_sub(1);
  cpu.write_word_reg(reg_enum, result);
}

fn shared_rotate_rr(cpu: &mut CPU, reg_enum: RegEnum) {
  let carry = if cpu.util_is_flag_set(FLAG_CARRY) { 0x80 } else { 0x00 };
  let result = cpu.read_byte_reg(reg_enum);

  if result & 0x01 != 0 { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  let result = result >> 1;
  let result = result | carry;
  cpu.write_byte_reg(reg_enum, result);

  match reg_enum {
    RegEnum::A => cpu.util_toggle_zero_flag_from_result(result),
    _ => {} // NOTE Only do this for Register A
  }
}

fn shared_cp(cpu: &mut CPU, byte: u8) {
  let a = cpu.read_byte_reg(RegEnum::A);
  let result = a.wrapping_sub(byte);

  cpu.util_set_flag_by_boolean(FLAG_ZERO, result == 0);
  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (a & 0x0F) < (byte & 0x0F));
  cpu.util_set_flag_by_boolean(FLAG_SUB, true);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, (a as u16) < (byte as u16));

  cpu.write_byte_reg(RegEnum::A, result);
}

fn shared_or_n(cpu: &mut CPU, byte: u8) {
  let result = cpu.read_byte_reg(RegEnum::A) | byte;
  cpu.write_byte_reg(RegEnum::A, result);
  cpu.util_clear_all_flags();
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_and_n(cpu: &mut CPU, byte: u8) {
  let result = cpu.read_byte_reg(RegEnum::A) & byte;
  cpu.write_byte_reg(RegEnum::A, result);
  cpu.util_set_flag(FLAG_HALF_CARRY);
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_rlc_n(cpu: &mut CPU, reg_enum: RegEnum) {
  let value = cpu.read_byte_reg(reg_enum);
  let carry = value & 0x80 == 0x80;
  let result = (value << 1) | (if carry { 1 } else { 0 });
  shared_update_srl_flags(cpu, result, carry);
  cpu.write_byte_reg(reg_enum, result);
}

fn shared_sub_n(cpu: &mut CPU, value: u8, carry_preserve: bool) {
  let carry = if carry_preserve && cpu.util_is_flag_set(FLAG_CARRY) { 1 } else { 0 };
  let a = cpu.read_byte_reg(RegEnum::A);
  let result = a.wrapping_sub(value).wrapping_sub(carry);

  cpu.util_set_flag_by_boolean(FLAG_ZERO, result == 0);
  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (a & 0x0F) < (value & 0x0F) + carry);
  cpu.util_set_flag_by_boolean(FLAG_SUB, true);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, (a as u16) < (value as u16) + (carry as u16));
  cpu.write_byte_reg(RegEnum::A, result);

  cpu.pc += 1;
}

// #[test]
// fn register_setting() {
//   let mut register = Register::new();
//   register.write(0xAABB);
//   assert_eq!(0xAA, register.read_hi());
//   assert_eq!(0xBB, register.read_lo());
//   assert_eq!(0xAABB, register.read());

//   register.write_lo(0x00);
//   register.write_lo(0xFF);
//   register.write_lo(0xCC);
//   assert_eq!(0xAA, register.read_hi());
//   assert_eq!(0xCC, register.read_lo());
//   assert_eq!(0xAACC, register.read());

//   register.write_hi(0xDD);
//   assert_eq!(0xDD, register.read_hi());
//   assert_eq!(0xCC, register.read_lo());
//   assert_eq!(0xDDCC, register.read());
// }
// #[test]
// fn opcode_nop() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::NOP] };
//
//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.pc);
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
//   assert_eq!(0x0150, cpu.pc);
// }

// #[test]
// fn opcode_xor_a() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![ opcodes::XOR_A ] };

//   cpu.af.write_hi(0xFF);

//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(0x00, cpu.af.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.pc);
//   assert!(cpu.util_is_flag_set(FLAG_ZERO));
//   assert!(!cpu.util_is_flag_set(FLAG_SUB));
//   assert!(!cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }

// #[test]
// fn opcode_xor_b() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![ opcodes::XOR_B ] };

//   //     11100101 = E5
//   // XOR 11010100 = D4
//   //     00110001 = 31
//   cpu.BC.write_hi(0xE5);
//   cpu.af.write_hi(0xD4);

//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(0x31, cpu.af.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.pc);
//   assert!(!cpu.util_is_flag_set(FLAG_ZERO));
//   assert!(!cpu.util_is_flag_set(FLAG_SUB));
//   assert!(!cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }

// #[test]
// fn opcode_dec_b() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![opcodes::DEC_B] };
//
//   cpu.BC.write_hi(0xF0);
//
//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   // assert_eq!(0x31, cpu.af.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.pc);
//   assert!(!cpu.util_is_flag_set(FLAG_ZERO));
//   assert!(cpu.util_is_flag_set(FLAG_SUB));
//   assert!(cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }
