use std::env;
use std::fmt;
use std::io;

use std::io::prelude::*;
pub use super::cartridge;
pub use super::mmu;
pub use super::types;
pub mod opcode_cycles;

const FLAG_ZERO: types::Byte = 0x80; // Zero
const FLAG_SUB: types::Byte = 0x40; // Negative,
const FLAG_HALF_CARRY: types::Byte = 0x20; // Half-carry
const FLAG_CARRY: types::Byte = 0x10; // Carry
const FLAG_NONE: types::Byte = 0x00; // None

extern crate socket_state_reporter;
use self::socket_state_reporter::StateReporter;

const SYNC_STATE: bool = false;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegEnum {
  A, F, B, C, D, E, H, L, S, P, AF, BC, DE, HL, SP,
}

pub struct Register {
  value: types::Word,
}

impl Register {
  pub fn new() -> Register {
    Register { value: 0x0 }
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
  pub AF: Register,
  pub BC: Register,
  pub DE: Register,
  pub HL: Register,
  pub SP: Register,
  pub PC: types::Word,

  pub BranchTaken: bool,
  pub IME: bool, // Master interrupt
  pub ei_cycles: u8,
  pub di_cycles: u8,

  pub halted: bool,

  pub state_reporter: StateReporter,
  pub tick_counter: u64,
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
      self.PC, self.AF.read_hi(), self.AF.read_lo(), self.BC.read_hi(), self.BC.read_lo(),
      self.DE.read_hi(), self.DE.read_lo(), self.HL.read_hi(), self.HL.read_lo(),
      formatted_flags(self), self.SP.read()
    )
  }
}

impl CPU {
  pub fn new() -> CPU {
    let state_reporter = StateReporter::new("5555");

    CPU {
      AF: Register::new(), BC: Register::new(), DE: Register::new(), HL: Register::new(), SP: Register::new(),
      PC: 0x0000,

      BranchTaken: false,
      IME: true,
      // IMECycles: 0,
      ei_cycles: 0,
      di_cycles: 0,

      halted: false,

      state_reporter: state_reporter,
      tick_counter: 0u64,
    }
  }

  pub fn write_byte_reg(&mut self, regEnum: RegEnum, byte: types::Byte) {
    match regEnum {
      RegEnum::A => self.AF.write_hi(byte),
      RegEnum::F => self.AF.write_lo(byte),
      RegEnum::B => self.BC.write_hi(byte),
      RegEnum::C => self.BC.write_lo(byte),
      RegEnum::D => self.DE.write_hi(byte),
      RegEnum::E => self.DE.write_lo(byte),
      RegEnum::H => self.HL.write_hi(byte),
      RegEnum::L => self.HL.write_lo(byte),
      RegEnum::S => self.SP.write_hi(byte),
      RegEnum::P => self.SP.write_lo(byte),
      _ => panic!("Unexpected regEnum: {:?}", regEnum),
    }
  }

  pub fn write_word_reg(&mut self, regEnum: RegEnum, word: types::Word) {
    match regEnum {
      RegEnum::AF => self.AF.write(word),
      RegEnum::BC => self.BC.write(word),
      RegEnum::DE => self.DE.write(word),
      RegEnum::HL => self.HL.write(word),
      RegEnum::SP => self.SP.write(word),
      _ => panic!("Unexpected regEnum: {:?}", regEnum),
    }
  }

  pub fn read_byte_reg(&mut self, regEnum: RegEnum) -> types::Byte {
    match regEnum {
      RegEnum::A => self.AF.read_hi(),
      RegEnum::F => self.AF.read_lo(),
      RegEnum::B => self.BC.read_hi(),
      RegEnum::C => self.BC.read_lo(),
      RegEnum::D => self.DE.read_hi(),
      RegEnum::E => self.DE.read_lo(),
      RegEnum::H => self.HL.read_hi(),
      RegEnum::L => self.HL.read_lo(),
      RegEnum::S => self.SP.read_hi(),
      RegEnum::P => self.SP.read_lo(),
      _ => panic!("Unexpected regEnum: {:?}", regEnum),
    }
  }

  pub fn read_word_reg(&mut self, regEnum: RegEnum) -> types::Word {
    match regEnum {
      RegEnum::AF => self.AF.read(),
      RegEnum::BC => self.BC.read(),
      RegEnum::DE => self.DE.read(),
      RegEnum::HL => self.HL.read(),
      RegEnum::SP => self.SP.read(),
      _ => panic!("Unexpected regEnum: {:?}", regEnum),
    }
  }

  fn explode(&mut self, message: String) {
    println!("PC: {:?} tick_counter: {}", self, self.tick_counter);
    panic!(message)
  }

  pub fn execute_next_opcode(&mut self, mmu: &mut mmu::MMU) -> i32 {
    self.update_ime();
    match self.handle_interrupts(mmu) {
        0 => {},
        interrupt_cycles => return interrupt_cycles,
    };

    if self.halted {
      return 1
    }

    let opcode: types::Byte = mmu.read(self.PC);
    // println!("pc: {:4x}, opcode: {:2x}", self.PC, opcode);

    if SYNC_STATE && self.tick_counter >= 19_286_329 {
      let registers = format!(
        "PC:{:04x} SP:{:04x} A:{:02x} F:{:04b} B:{:02x} C:{:02x} D:{:02x} E:{:02x} H:{:02x} L:{:02x}\n",
        self.PC, self.SP.value,
        self.AF.read_hi(), self.AF.read_lo(), self.BC.read_hi(), self.BC.read_lo(),
        self.DE.read_hi(), self.DE.read_lo(), self.HL.read_hi(), self.HL.read_lo()
      );

      let msg = format!("Tick: {}, Registers: {}, opcode: {:02x}, IME: {}, MMU INTE: {:02x}, MMU INTF: {:02x}", self.tick_counter, registers, opcode, self.IME, mmu.InterruptEnabled, mmu.InterruptFlags);
      self.state_reporter.send_message(msg.as_bytes());
      let received = self.state_reporter.receive_message();
      if received == "kill" {
        panic!("Server stopped.");
      }
    }

    self.tick_counter += 1;

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
    let mut cb_opcode: Option<types::Byte> = None;

    match opcode {
      0x00 => {
        debug!("NOP");
        self.nop()
      }
      0x01 => {
        debug!("LD BC,nn");
        self.ld_bc_nn(mmu)
      }
      0x02 => self.explode(format!("LD (BC),A : ld_bc_a() not implemented! {:#X}", opcode)),
      0x03 => {
        debug!("INC BC");
        self.inc_bc();
      }
      0x04 => {
        debug!("INC B : inc_b()");
        self.inc_b()
      }
      0x05 => {
        debug!("DEC B");
        self.dec_b()
      }
      0x06 => {
        debug!("LD B,n");
        self.ld_b_n(mmu)
      }
      0x07 => self.explode(format!("RLCA : rlca() not implemented! {:#X}", opcode)),
      0x08 => self.explode(format!("LD (nn),SP : ld_nn_sp() not implemented! {:#X}", opcode)),
      0x09 => {
        debug!("ADD HL,BC");
        self.add_hl_bc()
      }
      0x0A => {
        debug!("LD A,(BC)");
        self.ld_a_bc(mmu)
      }
      0x0B => {
        debug!("DEC BC");
        self.dec_bc()
      }
      0x0C => {
        debug!("INC C");
        self.inc_c()
      }
      0x0D => {
        debug!("DEC C");
        self.dec_c()
      }
      0x0E => {
        debug!("LD C,n");
        self.ld_c_n(mmu)
      }
      0x0F => self.explode(format!("RRCA : rrca() not implemented! {:#X}", opcode)),
      0x10 => self.explode(format!("STOP : stop() not implemented! {:#X}", opcode)),
      0x11 => {
        debug!("LD DE,nn");
        self.ld_de_nn(mmu)
      }
      0x12 => {
        debug!("LD (DE),A");
        self.ld_de_a(mmu)
      }
      0x13 => {
        debug!("INC DE");
        self.inc_de()
      }
      0x14 => self.explode(format!("INC D : inc_d() not implemented! {:#X}", opcode)),
      0x15 => {
        debug!("DEC D");
        self.dec_d()
      }
      0x16 => {
        debug!("LD D,n");
        self.ld_d_n(mmu)
      }
      0x17 => {
        debug!("RLA : rla()");
        self.rla();
      },
      0x18 => {
        debug!("JR n");
        self.jr_n(mmu)
      }
      0x19 => {
        debug!("ADD HL,DE");
        self.add_hl_de()
      }
      0x1A => {
        debug!("LD A,(DE)");
        self.ld_a_de(mmu)
      }
      0x1B => self.explode(format!("DEC DE : dec_de() not implemented! {:#X}", opcode)),
      0x1C => {
        debug!("INC E");
        self.inc_e()
      }
      0x1D => {
        debug!("DEC E");
        self.dec_e();
      }
      0x1E => {
        debug!("LD E,n");
        self.ld_e_n(mmu)
      }
      0x1F => {
        debug!("RRA");
        self.rra()
      }
      0x20 => {
        debug!("JR NZ,n");
        self.jr_nz_n(mmu)
      }
      0x21 => {
        debug!("LD HL,nn");
        self.ld_hl_nn(mmu)
      }
      0x22 => {
        debug!("LD (HLI),A");
        self.ld_hli_a(mmu)
      }
      0x23 => {
        debug!("INC HL");
        self.inc_hl()
      }
      0x24 => {
        debug!("INC H");
        self.inc_h();
      }
      0x25 => self.explode(format!("DEC H : dec_h() not implemented! {:#X}", opcode)),
      0x26 => {
        debug!("LD H,n : ld_h_n()");
        shared_ld_reg_n(self, RegEnum::H, mmu)
      }
      0x27 => self.explode(format!("DAA : daa() not implemented! {:#X}", opcode)),
      0x28 => {
        debug!("JR Z,n");
        self.jr_z_n(mmu)
      }
      0x29 => self.explode(format!("ADD HL,HL : add_hl_hl() not implemented! {:#X}", opcode)),
      0x2A => {
        debug!("LD A,(HLI)");
        self.ld_a_hli(mmu)
      }
      0x2B => self.explode(format!("DEC HL : dec_hl() not implemented! {:#X}", opcode)),
      0x2C => {
        debug!("INC L");
        self.inc_l()
      }
      0x2D => {
        debug!("DEC L");
        self.dec_l();
      }
      0x2E => {
        debug!("LD L,n : ld_l_n()");
        self.ld_l_n(mmu)
      },
      0x2F => {
        debug!("CPL");
        self.cpl()
      }
      0x30 => self.explode(format!("JR NC,n : jr_nc_n() not implemented! {:#X}", opcode)),
      0x31 => {
        debug!("LD SP,nn");
        self.ld_sp_nn(mmu)
      }
      0x32 => {
        debug!("LD (HLD), A");
        self.ld_hld_a(mmu)
      }
      0x33 => self.explode(format!("INC SP : inc_sp() not implemented! {:#X}", opcode)),
      0x34 => {
        debug!("INC (HL)");
        self.inc_hl_indirect(mmu)
      }
      0x35 => {
        debug!("DEC (HL)");
        self.dec_hl_indirect(mmu)
      }
      0x36 => {
        debug!("LD (HL),n");
        self.ld_hl_n(mmu)
      }
      0x37 => self.explode(format!("SCF : scf() not implemented! {:#X}", opcode)),
      0x38 => self.explode(format!("JR C,n : jr_c_n() not implemented! {:#X}", opcode)),
      0x39 => self.explode(format!("ADD HL,SP : add_hl_sp() not implemented! {:#X}", opcode)),
      0x3A => {
        debug!("LD A,(HLD)");
        self.ld_a_hld(mmu)
      }
      0x3B => self.explode(format!("DEC SP : dec_sp() not implemented! {:#X}", opcode)),
      0x3C => {
        debug!("INC A");
        self.inc_a()
      }
      0x3D => {
        debug!("DEC A");
        self.dec_a()
      }
      0x3E => {
        debug!("LD A,n");
        self.ld_a_n(mmu)
      }
      0x3F => self.explode(format!("CCF : ccf() not implemented! {:#X}", opcode)),
      0x40 => {
        debug!("LD B,B");
        self.ld_b_b()
      }
      0x41 => self.explode(format!("LD B,C : ld_b_c() not implemented! {:#X}", opcode)),
      0x42 => self.explode(format!("LD B,D : ld_b_d() not implemented! {:#X}", opcode)),
      0x43 => self.explode(format!("LD B,E : ld_b_e() not implemented! {:#X}", opcode)),
      0x44 => self.explode(format!("LD B,H : ld_b_h() not implemented! {:#X}", opcode)),
      0x45 => self.explode(format!("LD B,L : ld_b_l() not implemented! {:#X}", opcode)),
      0x46 => {
        debug!("LD B,(HL)");
        self.ld_b_hl(mmu)
      }
      0x47 => {
        debug!("LD B,A");
        self.ld_b_a()
      }
      0x48 => self.explode(format!("LD C,B : ld_c_b() not implemented! {:#X}", opcode)),
      0x49 => self.explode(format!("LD C,C : ld_c_c() not implemented! {:#X}", opcode)),
      0x4A => self.explode(format!("LD C,D : ld_c_d() not implemented! {:#X}", opcode)),
      0x4B => self.explode(format!("LD C,E : ld_c_e() not implemented! {:#X}", opcode)),
      0x4C => self.explode(format!("LD C,H : ld_c_h() not implemented! {:#X}", opcode)),
      0x4D => self.explode(format!("LD C,L : ld_c_l() not implemented! {:#X}", opcode)),
      0x4E => {
        debug!("LD C,(HL)");
        self.ld_c_hl(mmu)
      }
      0x4F => {
        debug!("LD C,A");
        self.ld_c_a()
      }
      0x50 => self.explode(format!("LD D,B : ld_d_b() not implemented! {:#X}", opcode)),
      0x51 => self.explode(format!("LD D,C : ld_d_c() not implemented! {:#X}", opcode)),
      0x52 => self.explode(format!("LD D,D : ld_d_d() not implemented! {:#X}", opcode)),
      0x53 => self.explode(format!("LD D,E : ld_d_e() not implemented! {:#X}", opcode)),
      0x54 => {
        debug!("LD D,H");
        self.ld_d_h()
      }
      0x55 => self.explode(format!("LD D,L : ld_d_l() not implemented! {:#X}", opcode)),
      0x56 => {
        debug!("LD D,(HL)");
        self.ld_d_hl(mmu)
      }
      0x57 => {
        debug!("LD D,A : ld_d_a()");
        self.ld_d_a()
      },
      0x58 => self.explode(format!("LD E,B : ld_e_b() not implemented! {:#X}", opcode)),
      0x59 => self.explode(format!("LD E,C : ld_e_c() not implemented! {:#X}", opcode)),
      0x5A => self.explode(format!("LD E,D : ld_e_d() not implemented! {:#X}", opcode)),
      0x5B => self.explode(format!("LD E,E : ld_e_e() not implemented! {:#X}", opcode)),
      0x5C => self.explode(format!("LD E,H : ld_e_h() not implemented! {:#X}", opcode)),
      0x5D => {
        debug!("LD E,L");
        self.ld_e_l()
      }
      0x5E => {
        debug!("LD E,(HL)");
        self.ld_e_hl(mmu)
      }
      0x5F => {
        debug!("LD E,A");
        self.ld_e_a()
      }
      0x60 => {
        debug!("LD H,B");
        self.ld_h_b()
      }
      0x61 => self.explode(format!("LD H,C : ld_h_c() not implemented! {:#X}", opcode)),
      0x62 => {
        debug!("LD H,D");
        self.ld_h_d()
      }
      0x63 => self.explode(format!("LD H,E : ld_h_e() not implemented! {:#X}", opcode)),
      0x64 => self.explode(format!("LD H,H : ld_h_h() not implemented! {:#X}", opcode)),
      0x65 => self.explode(format!("LD H,L : ld_h_l() not implemented! {:#X}", opcode)),
      0x66 => self.explode(format!("LD H,(HL) : ld_h_hl() not implemented! {:#X}", opcode)),
      0x67 => {
        debug!("LD H,A : ld_h_a()");
        self.ld_h_a()
      },
      0x68 => self.explode(format!("LD L,B : ld_l_b() not implemented! {:#X}", opcode)),
      0x69 => {
        debug!("LD L,C");
        self.ld_l_c()
      }
      0x6A => self.explode(format!("LD L,D : ld_l_d() not implemented! {:#X}", opcode)),
      0x6B => {
        debug!("LD L,E");
        self.ld_l_e()
      }
      0x6C => self.explode(format!("LD L,H : ld_l_h() not implemented! {:#X}", opcode)),
      0x6D => self.explode(format!("LD L,L : ld_l_l() not implemented! {:#X}", opcode)),
      0x6E => self.explode(format!("LD L,(HL) : ld_l_hl() not implemented! {:#X}", opcode)),
      0x6F => {
        debug!("LD L,A");
        self.ld_l_a()
      }
      0x70 => self.explode(format!("LD (HL),B : ld_hl_b() not implemented! {:#X}", opcode)),
      0x71 => {
        debug!("LD (HL),C");
        self.ld_hl_c(mmu)
      }
      0x72 => {
        debug!("LD (HL),D");
        self.ld_hl_d(mmu)
      }
      0x73 =>  {
        debug!("LD (HL),E");
        self.ld_hl_e(mmu)
      }
      0x74 => self.explode(format!("LD (HL),H : ld_hl_h() not implemented! {:#X}", opcode)),
      0x75 => self.explode(format!("LD (HL),L : ld_hl_l() not implemented! {:#X}", opcode)),
      0x76 => self.explode(format!("HALT : halt() not implemented! {:#X}", opcode)),
      0x77 => {
        debug!("LD (HL),A");
        self.ld_hl_a(mmu)
      }
      0x78 => {
        debug!("LD A,B");
        self.ld_a_b()
      }
      0x79 => {
        debug!("LD A,C");
        self.ld_a_c()
      }
      0x7A => {
        debug!("LD A,D");
        self.ld_a_d()
      }
      0x7B => {
        debug!("LD A,E : ld_a_e()");
        self.ld_a_e()
      },
      0x7C => {
        debug!("LD A,H");
        self.ld_a_h()
      }
      0x7D => {
        debug!("LD A,L");
        self.ld_a_l()
      }
      0x7E => {
        debug!("LD A,(HL)");
        self.ld_a_hl(mmu)
      }
      0x7F => self.explode(format!("LD A,A : ld_a_a() not implemented! {:#X}", opcode)),
      0x80 => {
        debug!("ADD A,B");
        self.add_a_b()
      }
      0x81 => self.explode(format!("ADD A,C : add_a_c() not implemented! {:#X}", opcode)),
      0x82 => self.explode(format!("ADD A,D : add_a_d() not implemented! {:#X}", opcode)),
      0x83 => self.explode(format!("ADD A,E : add_a_e() not implemented! {:#X}", opcode)),
      0x84 => self.explode(format!("ADD A,H : add_a_h() not implemented! {:#X}", opcode)),
      0x85 => {
        debug!("ADD A,L");
        self.add_a_l()
      }
      0x86 => {
        debug!("ADD A,(HL)");
        self.add_a_hl(mmu)
      }
      0x87 => {
        debug!("ADD A,A");
        self.add_a_a()
      }
      0x88 => self.explode(format!("ADC A,B : adc_a_b() not implemented! {:#X}", opcode)),
      0x89 => {
        debug!("ADC A,C");
        self.adc_a_c()
      }
      0x8A => self.explode(format!("ADC A,D : adc_a_d() not implemented! {:#X}", opcode)),
      0x8B => self.explode(format!("ADC A,E : adc_a_e() not implemented! {:#X}", opcode)),
      0x8C => self.explode(format!("ADC A,H : adc_a_h() not implemented! {:#X}", opcode)),
      0x8D => self.explode(format!("ADC A,L : adc_a_l() not implemented! {:#X}", opcode)),
      0x8E => self.explode(format!("ADC A,(HL) : adc_a_hl() not implemented! {:#X}", opcode)),
      0x8F => self.explode(format!("ADC A,A : adc_a_a() not implemented! {:#X}", opcode)),
      0x90 => {
        debug!("SUB B");
        self.sub_b()
      }
      0x91 => self.explode(format!("SUB C : sub_c() not implemented! {:#X}", opcode)),
      0x92 => self.explode(format!("SUB D : sub_d() not implemented! {:#X}", opcode)),
      0x93 => self.explode(format!("SUB E : sub_e() not implemented! {:#X}", opcode)),
      0x94 => self.explode(format!("SUB H : sub_h() not implemented! {:#X}", opcode)),
      0x95 => self.explode(format!("SUB L : sub_l() not implemented! {:#X}", opcode)),
      0x96 => self.explode(format!("SUB (HL) : sub_hl() not implemented! {:#X}", opcode)),
      0x97 => self.explode(format!("SUB A : sub_a() not implemented! {:#X}", opcode)),
      0x98 => self.explode(format!("SBC B : sbc_b() not implemented! {:#X}", opcode)),
      0x99 => self.explode(format!("SBC C : sbc_c() not implemented! {:#X}", opcode)),
      0x9A => self.explode(format!("SBC D : sbc_d() not implemented! {:#X}", opcode)),
      0x9B => self.explode(format!("SBC E : sbc_e() not implemented! {:#X}", opcode)),
      0x9C => self.explode(format!("SBC H : sbc_h() not implemented! {:#X}", opcode)),
      0x9D => self.explode(format!("SBC L : sbc_l() not implemented! {:#X}", opcode)),
      0x9E => self.explode(format!("SBC (HL) : sbc_hl() not implemented! {:#X}", opcode)),
      0x9F => self.explode(format!("SBC A : sbc_a() not implemented! {:#X}", opcode)),
      0xA0 => {
        debug!("AND B");
        self.and_b()
      }
      0xA1 => {
        debug!("AND C");
        self.and_c()
      }
      0xA2 => {
        debug!("AND D");
        self.and_d()
      }
      0xA3 => {
        debug!("AND E");
        self.and_e()
      }
      0xA4 => {
        debug!("AND H");
        self.and_h()
      }
      0xA5 => {
        debug!("AND L");
        self.and_l()
      }
      0xA6 => self.explode(format!("AND (HL) : and_hl() not implemented! {:#X}", opcode)),
      0xA7 => {
        debug!("AND A");
        self.and_a()
      }
      0xA8 => {
        debug!("XOR B");
        self.xor_b()
      }
      0xA9 => {
        debug!("XOR C");
        self.xor_c()
      }
      0xAA => {
        debug!("XOR D");
        self.xor_d()
      }
      0xAB => {
        debug!("XOR E");
        self.xor_e()
      }
      0xAC => {
        debug!("XOR H");
        self.xor_h()
      }
      0xAD => {
        debug!("XOR L");
        self.xor_l()
      }
      0xAE => self.explode(format!("XOR (HL) : xor_hl() not implemented! {:#X}", opcode)),
      0xAF => {
        debug!("XOR A");
        self.xor_a()
      }
      0xB0 => {
        debug!("OR B");
        self.or_b()
      }
      0xB1 => {
        debug!("OR C");
        self.or_c()
      }
      0xB2 => self.explode(format!("OR D : or_d() not implemented! {:#X}", opcode)),
      0xB3 => self.explode(format!("OR E : or_e() not implemented! {:#X}", opcode)),
      0xB4 => self.explode(format!("OR H : or_h() not implemented! {:#X}", opcode)),
      0xB5 => self.explode(format!("OR L : or_l() not implemented! {:#X}", opcode)),
      0xB6 => self.explode(format!("OR (HL) : or_hl() not implemented! {:#X}", opcode)),
      0xB7 => self.explode(format!("OR A : or_a() not implemented! {:#X}", opcode)),
      0xB8 => self.explode(format!("CP B : cp_b() not implemented! {:#X}", opcode)),
      0xB9 => self.explode(format!("CP C : cp_c() not implemented! {:#X}", opcode)),
      0xBA => self.explode(format!("CP D : cp_d() not implemented! {:#X}", opcode)),
      0xBB => self.explode(format!("CP E : cp_e() not implemented! {:#X}", opcode)),
      0xBC => self.explode(format!("CP H : cp_h() not implemented! {:#X}", opcode)),
      0xBD => self.explode(format!("CP L : cp_l() not implemented! {:#X}", opcode)),
      0xBE => {
        debug!("CP (HL)");
        self.cp_hl(mmu)
      }
      0xBF => self.explode(format!("CP A : cp_a() not implemented! {:#X}", opcode)),
      0xC0 => {
        debug!("RET NZ");
        self.ret_nz(mmu)
      }
      0xC1 => {
        debug!("POP BC");
        self.pop_bc(mmu)
      }
      0xC2 => {
        debug!("JP NZ,nn");
        self.jp_nz_nn(mmu)
      }
      0xC3 => {
        debug!("JP nn");
        self.jp_nn(mmu)
      }
      0xC4 => {
        debug!("CALL NZ,nn");
        self.call_nz_nn(mmu)
      }
      0xC5 => {
        debug!("PUSH BC");
        self.push_bc(mmu)
      }
      0xC6 => {
        debug!("ADD A,N");
        self.add_a_n(mmu)
      }
      0xC7 => self.explode(format!("RST 00H : rst_00h() not implemented! {:#X}", opcode)),
      0xC8 => {
        debug!("RET Z");
        self.ret_z(mmu)
      }
      0xC9 => {
        debug!("RET");
        self.ret(mmu)
      }
      0xCA => {
        debug!("JP Z,nn");
        self.jp_z_nn(mmu)
      }
      0xCB => {
        debug!("CB prefixed instruction");
        cb_opcode = Some(mmu.read(self.PC));
        self.cb_prefixed_instruction(cb_opcode.unwrap(), mmu)
      }
      0xCC => self.explode(format!("CALL Z,nn : call_z_nn() not implemented! {:#X}", opcode)),
      0xCD => {
        debug!("CALL nn");
        self.call_nn(mmu)
      }
      0xCE => self.explode(format!("ADC A,n : adc_a_n() not implemented! {:#X}", opcode)),
      0xCF => self.explode(format!("RST 08H : rst_08h() not implemented! {:#X}", opcode)),
      0xD0 => self.explode(format!("RET NC : ret_nc() not implemented! {:#X}", opcode)),
      0xD1 => {
        debug!("POP DE");
        self.pop_de(mmu)
      }
      0xD2 => self.explode(format!("JP NC,nn : jp_nc_nn() not implemented! {:#X}", opcode)),
      0xD3 => debug!("Unhandled opcode"),
      0xD4 => self.explode(format!("CALL NC,nn : call_nc_nn() not implemented! {:#X}", opcode)),
      0xD5 => {
        debug!("PUSH DE");
        self.push_de(mmu)
      }
      0xD6 => self.explode(format!("SUB n : sub_n() not implemented! {:#X}", opcode)),
      0xD7 => self.explode(format!("RST 10H : rst_10h() not implemented! {:#X}", opcode)),
      0xD8 => self.explode(format!("RET C : ret_c() not implemented! {:#X}", opcode)),
      0xD9 => {
        debug!("RETI");
        self.reti(mmu)
      }
      0xDA => self.explode(format!("JP C,nn : jp_c_nn() not implemented! {:#X}", opcode)),
      0xDB => debug!("Unhandled opcode"),
      0xDC => self.explode(format!("CALL C,nn : call_c_nn() not implemented! {:#X}", opcode)),
      0xDD => debug!("Unhandled opcode"),
      0xDE => self.explode(format!("SBC n : sbc_n() not implemented! {:#X}", opcode)),
      0xDF => self.explode(format!("RST 18H : rst_18h() not implemented! {:#X}", opcode)),
      0xE0 => {
        debug!("LD (0xFF00+n),A");
        self.ld_0xff00_plus_n_a(mmu)
      }
      0xE1 => {
        debug!("POP HL");
        self.pop_hl(mmu)
      }
      0xE2 => {
        debug!("LD (0xFF00+C),A");
        self.ld_0xff00_plus_c_a(mmu)
      }
      0xE3 => debug!("Unhandled opcode"),
      0xE4 => debug!("Unhandled opcode"),
      0xE5 => {
        debug!("PUSH HL");
        self.push_hl(mmu)
      }
      0xE6 => {
        debug!("AND n");
        self.and_n(mmu)
      }
      0xE7 => self.explode(format!("RST 20H : rst_20h() not implemented! {:#X}", opcode)),
      0xE8 => self.explode(format!("ADD SP,n : add_sp_n() not implemented! {:#X}", opcode)),
      0xE9 => {
        debug!("JP (HL)");
        self.jp_hl()
      }
      0xEA => {
        debug!("LD (nn),A");
        self.ld_nn_a(mmu)
      }
      0xEB => debug!("Unhandled opcode"),
      0xEC => debug!("Unhandled opcode"),
      0xED => debug!("Unhandled opcode"),
      0xEE => self.explode(format!("XOR n : xor_n() not implemented! {:#X}", opcode)),
      0xEF => {
        debug!("RST 28H");
        self.rst_28h(mmu)
      }
      0xF0 => {
        debug!("LD A,(0xFF00+n)");
        self.ld_a_0xff00_plus_n(mmu)
      }
      0xF1 => {
        debug!("POP AF");
        self.pop_af(mmu)
      }
      0xF2 => self.explode(format!("LD A,(C) : ld_a_c() not implemented! {:#X}", opcode)),
      0xF3 => {
        debug!("DI");
        self.di(mmu)
      }
      0xF4 => debug!("Unhandled opcode"),
      0xF5 => {
        debug!("PUSH AF");
        self.push_af(mmu)
      }
      0xF6 => {
        debug!("OR N");
        self.or_n(mmu)
      }
      0xF7 => self.explode(format!("RST 30H : rst_30h() not implemented! {:#X}", opcode)),
      0xF8 => self.explode(format!("LD HL,SP+n : ld_hl_sp_plus_n() not implemented! {:#X}", opcode)),
      0xF9 => self.explode(format!("LD SP,HL : ld_sp_hl() not implemented! {:#X}", opcode)),
      0xFA => {
        debug!("LD A,(nn)");
        self.ld_a_nn(mmu)
      }
      0xFB => {
        debug!("EI");
        self.ei(mmu)
      }
      0xFC => debug!("Unhandled opcode"),
      0xFD => debug!("Unhandled opcode"),
      0xFE => {
        debug!("CP");
        self.cp_n(mmu)
      }
      0xFF => self.explode(format!("RST 38H : rst_38h() not implemented! {:#X}", opcode)),
      _ => self.explode(format!("Unexpected opcode: {:#X}", opcode)),
    }

    let raw_cycles = if opcode == 0xCB {
      opcode_cycles::cb(cb_opcode.unwrap())
    } else {
      if self.BranchTaken {
        self.BranchTaken = false;
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

  fn xor_c(&mut self) {
    let C = self.BC.read_lo();
    shared_xor_n(self, C);
  }

  fn xor_d(&mut self) {
    let D = self.DE.read_hi();
    shared_xor_n(self, D);
  }

  fn xor_e(&mut self) {
    let E = self.DE.read_lo();
    shared_xor_n(self, E);
  }

  fn xor_h(&mut self) {
    let H = self.HL.read_hi();
    shared_xor_n(self, H);
  }

  fn xor_l(&mut self) {
    let L = self.HL.read_lo();
    shared_xor_n(self, L);
  }

  fn ld_hl_nn(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.write_word_reg(RegEnum::HL, value);
    self.PC += 2;
  }

  fn ld_hl_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.PC);
    let address = self.read_word_reg(RegEnum::HL);
    mmu.write(address, value);

    self.PC += 1;
  }

  fn ld_b_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_ld_n_n(self, RegEnum::B, value);
  }

  fn ld_c_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_ld_n_n(self, RegEnum::C, value);
  }

  fn ld_d_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_ld_n_n(self, RegEnum::D, value);
  }

  fn ld_e_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
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

  fn sub_b(&mut self) {
    shared_sub_byte_reg(self, RegEnum::B);
  }

  fn jr_nz_n(&mut self, mmu: &mmu::MMU) {
    let addr = self.PC;
    let offset = mmu.read(self.PC) as i8;
    self.PC = self.PC.wrapping_add(1);

    if !self.util_is_flag_set(FLAG_ZERO) {
      self.PC = self.PC.wrapping_add(offset as types::Word);
      self.BranchTaken = true;
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

  fn ld_l_n(&mut self, mmu: &mmu::MMU) {
    let operand = mmu.read(self.PC);
    self.write_byte_reg(RegEnum::L, operand);
    self.PC += 1;
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
    let a = self.read_byte_reg(RegEnum::A);
    shared_cp(self, value);
    self.write_byte_reg(RegEnum::A, a);
    self.PC += 1;
  }

  fn cp_hl(&mut self, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    shared_cp(self, value);
  }

  fn ld_nn_a(&mut self, mmu: &mut mmu::MMU) {
    let address = mmu.read_word(self.PC);
    let value = self.read_byte_reg(RegEnum::A);
    mmu.write(address, value);

    self.PC += 2;
  }

  fn ld_sp_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.PC += 2;

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

    mmu.write(0xFF00 + operand as types::Word, value);
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
    let current_PC = self.PC; // Get current PC
    self.PC = mmu.read_word(self.PC); // set PC to nn
    self.stack_push(current_PC + 2, mmu); // Push onto stack current PC + 2
  }

  fn call_nz_nn(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      self.call_nn(mmu)
    }
  }

  fn ld_bc_nn(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.write_word_reg(RegEnum::BC, value);

    self.PC += 2;
  }

  fn dec_bc(&mut self) {
    shared_dec_word_reg(self, RegEnum::BC);
  }

  fn or_n(&mut self, mmu: &mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_or_n(self, value);
    self.PC += 1;
  }

  fn or_b(&mut self) {
    let value = self.read_byte_reg(RegEnum::B);
    shared_or_n(self, value);
  }

  fn or_c(&mut self) {
    let value = self.read_byte_reg(RegEnum::C);
    shared_or_n(self, value);
  }

  fn ret(&mut self, mmu: &mmu::MMU) {
    self.PC = self.stack_pop(mmu);
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
    let HL = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(HL);
    let result = value.wrapping_add(1);
    mmu.write(HL, result);

    if self.util_is_flag_set(FLAG_CARRY) { self.util_set_flag(FLAG_CARRY) } else { self.util_clear_all_flags() }
    self.util_toggle_zero_flag_from_result(result);

    if (result & 0x0F) == 0x00 {
      self.util_toggle_flag(FLAG_HALF_CARRY);
    }
  }

  fn dec_hl_indirect(&mut self, mmu: &mut mmu::MMU) {
    let HL = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(HL);
    let result = value.wrapping_sub(1);
    mmu.write(HL, result);

    if self.util_is_flag_set(FLAG_CARRY) { self.util_set_flag(FLAG_CARRY) } else { self.util_clear_all_flags() }
    self.util_toggle_flag(FLAG_SUB);
    self.util_toggle_zero_flag_from_result(result);

    if (result & 0x0F) == 0x0F {
      self.util_toggle_flag(FLAG_HALF_CARRY);
    }
  }

  fn ei(&mut self, mmu: &mut mmu::MMU) {
    self.ei_cycles = 2;
  }

  fn di(&mut self, mmu: &mut mmu::MMU) {
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
      self.PC = self.stack_pop(mmu);
      self.BranchTaken = true;
    }
  }

  fn ld_a_nn(&mut self, mmu: &mut mmu::MMU) {
    let address = mmu.read_word(self.PC);
    let value = mmu.read(address);
    self.write_byte_reg(RegEnum::A, value);

    self.PC += 2;
  }

  fn ret_z(&mut self, mmu: &mut mmu::MMU) {
    if self.util_is_flag_set(FLAG_ZERO) {
      self.PC = self.stack_pop(mmu);
      self.BranchTaken = true;
    }
  }

  fn pop_af(&mut self, mmu: &mut mmu::MMU) {
    let current_SP = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_SP);
    self.write_word_reg(RegEnum::AF, value);
    self.stack_pop(mmu);
  }

  fn pop_bc(&mut self, mmu: &mut mmu::MMU) {
    let value = self.stack_pop(mmu);
    self.write_word_reg(RegEnum::BC, value); // TODO do the same change to the other pop_ operations?
  }

  fn pop_de(&mut self, mmu: &mut mmu::MMU) {
    let current_SP = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_SP);
    self.write_word_reg(RegEnum::DE, value);
    self.stack_pop(mmu);
  }

  fn pop_hl(&mut self, mmu: &mut mmu::MMU) {
    let current_SP = self.read_word_reg(RegEnum::SP);
    let value = mmu.read_word(current_SP);
    self.write_word_reg(RegEnum::HL, value);
    self.stack_pop(mmu);
  }

  fn reti(&mut self, mmu: &mut mmu::MMU) {
    self.PC = self.stack_pop(mmu);
    self.ei_cycles = 1;
  }

  fn rst_28h(&mut self, mmu: &mut mmu::MMU) {
    let current_PC = self.PC;
    self.stack_push(current_PC, mmu);
    self.PC = 0x28;
  }

  fn add_a_a(&mut self) {
    shared_add_byte_reg(self, RegEnum::A);
  }

  fn add_a_b(&mut self) {
    shared_add_byte_reg(self, RegEnum::B);
  }

  fn add_a_l(&mut self) {
    shared_add_byte_reg(self, RegEnum::L);
  }

  fn add_a_n(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read(self.PC);
    shared_add_n(self, value, false);
    self.PC += 1;
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
    self.PC = address;
  }

  fn ld_de_nn(&mut self, mmu: &mut mmu::MMU) {
    let value = mmu.read_word(self.PC);
    self.write_word_reg(RegEnum::DE, value);

    self.PC += 2;
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
      let address = mmu.read_word(self.PC);
      self.PC = address;
      self.BranchTaken = true;
    } else {
      self.PC += 2;
    }
  }

  fn jp_nz_nn(&mut self, mmu: &mut mmu::MMU) {
    if !self.util_is_flag_set(FLAG_ZERO) {
      let address = mmu.read_word(self.PC);
      self.PC = address;
      self.BranchTaken = true;
    } else {
      self.PC += 2;
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
    let operand_dest = mmu.read(self.PC) as types::SignedByte;
    self.PC = self.PC.wrapping_add((1 + operand_dest) as types::Word);
  }

  // Helpers

  fn cb_prefixed_instruction(&mut self, cb_opcode: types::Byte, mmu: &mut mmu::MMU) {
    self.execute_cb_opcode(cb_opcode, mmu);
    self.PC += 1;
  }

  fn stack_push(&mut self, word: types::Word, mmu: &mut mmu::MMU) {
    let current_SP = self.read_word_reg(RegEnum::SP); // TODO just manipulate .value directly
    self.write_word_reg(RegEnum::SP, current_SP - 2);
    mmu.write_word(self.read_word_reg(RegEnum::SP), word);
  }

  fn stack_pop(&mut self, mmu: &mmu::MMU) -> types::Word {
    let current_SP = self.read_word_reg(RegEnum::SP);
    let popped_value = mmu.read_word(current_SP);
    self.SP.write(current_SP + 2);

    popped_value
  }

  fn util_set_flag_by_boolean(&mut self, flag: u8, b: bool) {
    if b {
      self.util_toggle_flag(flag);
    } else {
      self.util_untoggle_flag(flag);
    }
  }

  fn util_toggle_zero_flag_from_result(&mut self, result: types::Byte) {
    if result == 0 {
      self.util_toggle_flag(FLAG_ZERO);
    } else {
      self.util_untoggle_flag(FLAG_ZERO);
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

  fn util_untoggle_flag(&mut self, byte: types::Byte) {
    let previous_flags = self.AF.read_lo();
    self.AF.write_lo(previous_flags & (!byte));
  }

  // CB opcode handling

  fn execute_cb_opcode(&mut self, opcode: types::Byte, mmu: &mut mmu::MMU) {
    match opcode {
      0x00 => self.explode(format!("CB: RLC B : rlc_b() not implemented! {:#X}", opcode)),
      0x01 => self.explode(format!("CB: RLC C : rlc_c() not implemented! {:#X}", opcode)),
      0x02 => self.explode(format!("CB: RLC D : rlc_d() not implemented! {:#X}", opcode)),
      0x03 => self.explode(format!("CB: RLC E : rlc_e() not implemented! {:#X}", opcode)),
      0x04 => self.explode(format!("CB: RLC H : rlc_h() not implemented! {:#X}", opcode)),
      0x05 => self.explode(format!("CB: RLC L : rlc_l() not implemented! {:#X}", opcode)),
      0x06 => self.explode(format!("CB: RLC (HL) : rlc_hl() not implemented! {:#X}", opcode)),
      0x07 => self.explode(format!("CB: RLC A : rlc_a() not implemented! {:#X}", opcode)),
      0x08 => self.explode(format!("CB: RRC B : rrc_b() not implemented! {:#X}", opcode)),
      0x09 => self.explode(format!("CB: RRC C : rrc_c() not implemented! {:#X}", opcode)),
      0x0A => self.explode(format!("CB: RRC D : rrc_d() not implemented! {:#X}", opcode)),
      0x0B => self.explode(format!("CB: RRC E : rrc_e() not implemented! {:#X}", opcode)),
      0x0C => self.explode(format!("CB: RRC H : rrc_h() not implemented! {:#X}", opcode)),
      0x0D => self.explode(format!("CB: RRC L : rrc_l() not implemented! {:#X}", opcode)),
      0x0E => self.explode(format!("CB: RRC (HL) : rrc_hl() not implemented! {:#X}", opcode)),
      0x0F => self.explode(format!("CB: RRC A : rrc_a() not implemented! {:#X}", opcode)),
      0x10 => self.explode(format!("CB: RL B : rl_b() not implemented! {:#X}", opcode)),
      0x11 => {
        debug!("CB: RL C : rl_c()");
        self.rl_c()
      },
      0x12 => self.explode(format!("CB: RL D : rl_d() not implemented! {:#X}", opcode)),
      0x13 => self.explode(format!("CB: RL E : rl_e() not implemented! {:#X}", opcode)),
      0x14 => self.explode(format!("CB: RL H : rl_h() not implemented! {:#X}", opcode)),
      0x15 => self.explode(format!("CB: RL L : rl_l() not implemented! {:#X}", opcode)),
      0x16 => self.explode(format!("CB: RL (HL) : rl_hl() not implemented! {:#X}", opcode)),
      0x17 => self.explode(format!("CB: RL A : rl_a() not implemented! {:#X}", opcode)),
      0x18 => self.explode(format!("CB: RR B : rr_b() not implemented! {:#X}", opcode)),
      0x19 => self.explode(format!("CB: RR C : rr_c() not implemented! {:#X}", opcode)),
      0x1A => self.explode(format!("CB: RR D : rr_d() not implemented! {:#X}", opcode)),
      0x1B => self.explode(format!("CB: RR E : rr_e() not implemented! {:#X}", opcode)),
      0x1C => self.explode(format!("CB: RR H : rr_h() not implemented! {:#X}", opcode)),
      0x1D => self.explode(format!("CB: RR L : rr_l() not implemented! {:#X}", opcode)),
      0x1E => self.explode(format!("CB: RR (HL) : rr_hl() not implemented! {:#X}", opcode)),
      0x1F => self.explode(format!("CB: RR A : rr_a() not implemented! {:#X}", opcode)),
      0x20 => self.explode(format!("CB: SLA B : sla_b() not implemented! {:#X}", opcode)),
      0x21 => self.explode(format!("CB: SLA C : sla_c() not implemented! {:#X}", opcode)),
      0x22 => self.explode(format!("CB: SLA D : sla_d() not implemented! {:#X}", opcode)),
      0x23 => self.explode(format!("CB: SLA E : sla_e() not implemented! {:#X}", opcode)),
      0x24 => self.explode(format!("CB: SLA H : sla_h() not implemented! {:#X}", opcode)),
      0x25 => self.explode(format!("CB: SLA L : sla_l() not implemented! {:#X}", opcode)),
      0x26 => self.explode(format!("CB: SLA (HL) : sla_hl() not implemented! {:#X}", opcode)),
      0x27 => {
        debug!("CB: SLA A");
        self.sla_a();
      }
      0x28 => self.explode(format!("CB: SRA B : sra_b() not implemented! {:#X}", opcode)),
      0x29 => self.explode(format!("CB: SRA C : sra_c() not implemented! {:#X}", opcode)),
      0x2A => self.explode(format!("CB: SRA D : sra_d() not implemented! {:#X}", opcode)),
      0x2B => self.explode(format!("CB: SRA E : sra_e() not implemented! {:#X}", opcode)),
      0x2C => self.explode(format!("CB: SRA H : sra_h() not implemented! {:#X}", opcode)),
      0x2D => self.explode(format!("CB: SRA L : sra_l() not implemented! {:#X}", opcode)),
      0x2E => self.explode(format!("CB: SRA (HL) : sra_hl() not implemented! {:#X}", opcode)),
      0x2F => self.explode(format!("CB: SRA A : sra_a() not implemented! {:#X}", opcode)),
      0x30 => self.explode(format!("CB: SWAP B : swap_b() not implemented! {:#X}", opcode)),
      0x31 => self.explode(format!("CB: SWAP C : swap_c() not implemented! {:#X}", opcode)),
      0x32 => self.explode(format!("CB: SWAP D : swap_d() not implemented! {:#X}", opcode)),
      0x33 => {
        debug!("CB: SWAP E : swap_e()");
        shared_swap_register(self, RegEnum::E);
      }
      0x34 => self.explode(format!("CB: SWAP H : swap_h() not implemented! {:#X}", opcode)),
      0x35 => self.explode(format!("CB: SWAP L : swap_l() not implemented! {:#X}", opcode)),
      0x36 => self.explode(format!("CB: SWAP (HL) : swap_hl() not implemented! {:#X}", opcode)),
      0x37 => {
        debug!("CB: SWAP A");
        shared_swap_register(self, RegEnum::A);
      }
      0x38 => self.explode(format!("CB: SRL B : srl_b() not implemented! {:#X}", opcode)),
      0x39 => self.explode(format!("CB: SRL C : srl_c() not implemented! {:#X}", opcode)),
      0x3A => self.explode(format!("CB: SRL D : srl_d() not implemented! {:#X}", opcode)),
      0x3B => self.explode(format!("CB: SRL E : srl_e() not implemented! {:#X}", opcode)),
      0x3C => self.explode(format!("CB: SRL H : srl_h() not implemented! {:#X}", opcode)),
      0x3D => self.explode(format!("CB: SRL L : srl_l() not implemented! {:#X}", opcode)),
      0x3E => self.explode(format!("CB: SRL (HL) : srl_hl() not implemented! {:#X}", opcode)),
      0x3F => {
        debug!("CB: SRL A : srl_a()");
        shared_srl_n(self, RegEnum::A)
      },
      0x40 => { debug!("CB: BIT 0 B"); shared_bit_n_reg(self, 0, RegEnum::B) }
      0x41 => { debug!("CB: BIT 0 C"); shared_bit_n_reg(self, 0, RegEnum::C) }
      0x42 => { debug!("CB: BIT 0 D"); shared_bit_n_reg(self, 0, RegEnum::D) }
      0x43 => { debug!("CB: BIT 0 E"); shared_bit_n_reg(self, 0, RegEnum::E) }
      0x44 => { debug!("CB: BIT 0 H"); shared_bit_n_reg(self, 0, RegEnum::H) }
      0x45 => { debug!("CB: BIT 0 L"); shared_bit_n_reg(self, 0, RegEnum::L) }
      0x46 => { panic!("CB: BIT 0 (HL) not implemented, {:#X}", opcode) }
      0x47 => { debug!("CB: BIT 0 A"); shared_bit_n_reg(self, 0, RegEnum::A) }
      0x48 => { debug!("CB: BIT 1 B"); shared_bit_n_reg(self, 1, RegEnum::B) }
      0x49 => { debug!("CB: BIT 1 C"); shared_bit_n_reg(self, 1, RegEnum::C) }
      0x4A => { debug!("CB: BIT 1 D"); shared_bit_n_reg(self, 1, RegEnum::D) }
      0x4B => { debug!("CB: BIT 1 E"); shared_bit_n_reg(self, 1, RegEnum::E) }
      0x4C => { debug!("CB: BIT 1 H"); shared_bit_n_reg(self, 1, RegEnum::H) }
      0x4D => { debug!("CB: BIT 1 L"); shared_bit_n_reg(self, 1, RegEnum::L) }
      0x4E => { panic!("CB: BIT 1 (HL) not implemented, {:#X}", opcode) }
      0x4F => { debug!("CB: BIT 1 A"); shared_bit_n_reg(self, 1, RegEnum::A) }
      0x50 => { debug!("CB: BIT 2 B"); shared_bit_n_reg(self, 2, RegEnum::B) }
      0x51 => { debug!("CB: BIT 2 C"); shared_bit_n_reg(self, 2, RegEnum::C) }
      0x52 => { debug!("CB: BIT 2 D"); shared_bit_n_reg(self, 2, RegEnum::D) }
      0x53 => { debug!("CB: BIT 2 E"); shared_bit_n_reg(self, 2, RegEnum::E) }
      0x54 => { debug!("CB: BIT 2 H"); shared_bit_n_reg(self, 2, RegEnum::H) }
      0x55 => { debug!("CB: BIT 2 L"); shared_bit_n_reg(self, 2, RegEnum::L) }
      0x56 => { panic!("CB: BIT 2 (HL) not implemented, {:#X}", opcode) }
      0x57 => { debug!("CB: BIT 2 A"); shared_bit_n_reg(self, 2, RegEnum::A) }
      0x58 => { debug!("CB: BIT 3 B"); shared_bit_n_reg(self, 3, RegEnum::B) }
      0x59 => { debug!("CB: BIT 3 C"); shared_bit_n_reg(self, 3, RegEnum::C) }
      0x5A => { debug!("CB: BIT 3 D"); shared_bit_n_reg(self, 3, RegEnum::D) }
      0x5B => { debug!("CB: BIT 3 E"); shared_bit_n_reg(self, 3, RegEnum::E) }
      0x5C => { debug!("CB: BIT 3 H"); shared_bit_n_reg(self, 3, RegEnum::H) }
      0x5D => { debug!("CB: BIT 3 L"); shared_bit_n_reg(self, 3, RegEnum::L) }
      0x5E => { panic!("CB: BIT 3 (HL) not implemented, {:#X}", opcode) }
      0x5F => { debug!("CB: BIT 3 A"); shared_bit_n_reg(self, 3, RegEnum::A) }
      0x60 => { debug!("CB: BIT 4 B"); shared_bit_n_reg(self, 4, RegEnum::B) }
      0x61 => { debug!("CB: BIT 4 C"); shared_bit_n_reg(self, 4, RegEnum::C) }
      0x62 => { debug!("CB: BIT 4 D"); shared_bit_n_reg(self, 4, RegEnum::D) }
      0x63 => { debug!("CB: BIT 4 E"); shared_bit_n_reg(self, 4, RegEnum::E) }
      0x64 => { debug!("CB: BIT 4 H"); shared_bit_n_reg(self, 4, RegEnum::H) }
      0x65 => { debug!("CB: BIT 4 L"); shared_bit_n_reg(self, 4, RegEnum::L) }
      0x66 => { panic!("CB: BIT 4 (HL) not implemented, {:#X}", opcode) }
      0x67 => { debug!("CB: BIT 4 A"); shared_bit_n_reg(self, 4, RegEnum::A) }
      0x68 => { debug!("CB: BIT 5 B"); shared_bit_n_reg(self, 5, RegEnum::B) }
      0x69 => { debug!("CB: BIT 5 C"); shared_bit_n_reg(self, 5, RegEnum::C) }
      0x6A => { debug!("CB: BIT 5 D"); shared_bit_n_reg(self, 5, RegEnum::D) }
      0x6B => { debug!("CB: BIT 5 E"); shared_bit_n_reg(self, 5, RegEnum::E) }
      0x6C => { debug!("CB: BIT 5 H"); shared_bit_n_reg(self, 5, RegEnum::H) }
      0x6D => { debug!("CB: BIT 5 L"); shared_bit_n_reg(self, 5, RegEnum::L) }
      0x6E => { panic!("CB: BIT 5 (HL) not implemented, {:#X}", opcode) }
      0x6F => { debug!("CB: BIT 5 A"); shared_bit_n_reg(self, 5, RegEnum::A) }
      0x70 => { debug!("CB: BIT 6 B"); shared_bit_n_reg(self, 6, RegEnum::B) }
      0x71 => { debug!("CB: BIT 6 C"); shared_bit_n_reg(self, 6, RegEnum::C) }
      0x72 => { debug!("CB: BIT 6 D"); shared_bit_n_reg(self, 6, RegEnum::D) }
      0x73 => { debug!("CB: BIT 6 E"); shared_bit_n_reg(self, 6, RegEnum::E) }
      0x74 => { debug!("CB: BIT 6 H"); shared_bit_n_reg(self, 6, RegEnum::H) }
      0x75 => { debug!("CB: BIT 6 L"); shared_bit_n_reg(self, 6, RegEnum::L) }
      0x76 => { panic!("CB: BIT 6 (HL) not implemented, {:#X}", opcode) }
      0x77 => { debug!("CB: BIT 6 A"); shared_bit_n_reg(self, 6, RegEnum::A) }
      0x78 => { debug!("CB: BIT 7 B"); shared_bit_n_reg(self, 7, RegEnum::B) }
      0x79 => { debug!("CB: BIT 7 C"); shared_bit_n_reg(self, 7, RegEnum::C) }
      0x7A => { debug!("CB: BIT 7 D"); shared_bit_n_reg(self, 7, RegEnum::D) }
      0x7B => { debug!("CB: BIT 7 E"); shared_bit_n_reg(self, 7, RegEnum::E) }
      0x7C => { debug!("CB: BIT 7 H"); shared_bit_n_reg(self, 7, RegEnum::H) }
      0x7D => { debug!("CB: BIT 7 L"); shared_bit_n_reg(self, 7, RegEnum::L) }
      0x7E => {
        debug!("CB: BIT 7 (HL)");
        shared_bit_n_hl(self, 7, mmu)
      }
      0x7F => { debug!("CB: BIT 7 A"); shared_bit_n_reg(self, 7, RegEnum::A) }
      0x80 => self.explode(format!("CB: RES 0 B : res_0_b() not implemented! {:#X}", opcode)),
      0x81 => self.explode(format!("CB: RES 0 C : res_0_c() not implemented! {:#X}", opcode)),
      0x82 => self.explode(format!("CB: RES 0 D : res_0_d() not implemented! {:#X}", opcode)),
      0x83 => self.explode(format!("CB: RES 0 E : res_0_e() not implemented! {:#X}", opcode)),
      0x84 => self.explode(format!("CB: RES 0 H : res_0_h() not implemented! {:#X}", opcode)),
      0x85 => self.explode(format!("CB: RES 0 L : res_0_l() not implemented! {:#X}", opcode)),
      0x86 => { debug!("CB: RES 0 (HL)"); self.res_bit_hl(0, mmu) }
      0x87 => { debug!("CB: RES 0 A"); self.res_bit_a(0) }
      0x88 => self.explode(format!("CB: RES 1 B : res_1_b() not implemented! {:#X}", opcode)),
      0x89 => self.explode(format!("CB: RES 1 C : res_1_c() not implemented! {:#X}", opcode)),
      0x8A => self.explode(format!("CB: RES 1 D : res_1_d() not implemented! {:#X}", opcode)),
      0x8B => self.explode(format!("CB: RES 1 E : res_1_e() not implemented! {:#X}", opcode)),
      0x8C => self.explode(format!("CB: RES 1 H : res_1_h() not implemented! {:#X}", opcode)),
      0x8D => self.explode(format!("CB: RES 1 L : res_1_l() not implemented! {:#X}", opcode)),
      0x8E => self.explode(format!("CB: RES 1 (HL) : res_1_hl() not implemented! {:#X}", opcode)),
      0x8F => self.explode(format!("CB: RES 1 A : res_1_a() not implemented! {:#X}", opcode)),
      0x90 => self.explode(format!("CB: RES 2 B : res_2_b() not implemented! {:#X}", opcode)),
      0x91 => self.explode(format!("CB: RES 2 C : res_2_c() not implemented! {:#X}", opcode)),
      0x92 => self.explode(format!("CB: RES 2 D : res_2_d() not implemented! {:#X}", opcode)),
      0x93 => self.explode(format!("CB: RES 2 E : res_2_e() not implemented! {:#X}", opcode)),
      0x94 => self.explode(format!("CB: RES 2 H : res_2_h() not implemented! {:#X}", opcode)),
      0x95 => self.explode(format!("CB: RES 2 L : res_2_l() not implemented! {:#X}", opcode)),
      0x96 => self.explode(format!("CB: RES 2 (HL) : res_2_hl() not implemented! {:#X}", opcode)),
      0x97 => self.explode(format!("CB: RES 2 A : res_2_a() not implemented! {:#X}", opcode)),
      0x98 => self.explode(format!("CB: RES 3 B : res_3_b() not implemented! {:#X}", opcode)),
      0x99 => self.explode(format!("CB: RES 3 C : res_3_c() not implemented! {:#X}", opcode)),
      0x9A => self.explode(format!("CB: RES 3 D : res_3_d() not implemented! {:#X}", opcode)),
      0x9B => self.explode(format!("CB: RES 3 E : res_3_e() not implemented! {:#X}", opcode)),
      0x9C => self.explode(format!("CB: RES 3 H : res_3_h() not implemented! {:#X}", opcode)),
      0x9D => self.explode(format!("CB: RES 3 L : res_3_l() not implemented! {:#X}", opcode)),
      0x9E => self.explode(format!("CB: RES 3 (HL) : res_3_hl() not implemented! {:#X}", opcode)),
      0x9F => self.explode(format!("CB: RES 3 A : res_3_a() not implemented! {:#X}", opcode)),
      0xA0 => self.explode(format!("CB: RES 4 B : res_4_b() not implemented! {:#X}", opcode)),
      0xA1 => self.explode(format!("CB: RES 4 C : res_4_c() not implemented! {:#X}", opcode)),
      0xA2 => self.explode(format!("CB: RES 4 D : res_4_d() not implemented! {:#X}", opcode)),
      0xA3 => self.explode(format!("CB: RES 4 E : res_4_e() not implemented! {:#X}", opcode)),
      0xA4 => self.explode(format!("CB: RES 4 H : res_4_h() not implemented! {:#X}", opcode)),
      0xA5 => self.explode(format!("CB: RES 4 L : res_4_l() not implemented! {:#X}", opcode)),
      0xA6 => self.explode(format!("CB: RES 4 (HL) : res_4_hl() not implemented! {:#X}", opcode)),
      0xA7 => self.explode(format!("CB: RES 4 A : res_4_a() not implemented! {:#X}", opcode)),
      0xA8 => self.explode(format!("CB: RES 5 B : res_5_b() not implemented! {:#X}", opcode)),
      0xA9 => self.explode(format!("CB: RES 5 C : res_5_c() not implemented! {:#X}", opcode)),
      0xAA => self.explode(format!("CB: RES 5 D : res_5_d() not implemented! {:#X}", opcode)),
      0xAB => self.explode(format!("CB: RES 5 E : res_5_e() not implemented! {:#X}", opcode)),
      0xAC => self.explode(format!("CB: RES 5 H : res_5_h() not implemented! {:#X}", opcode)),
      0xAD => self.explode(format!("CB: RES 5 L : res_5_l() not implemented! {:#X}", opcode)),
      0xAE => self.explode(format!("CB: RES 5 (HL) : res_5_hl() not implemented! {:#X}", opcode)),
      0xAF => self.explode(format!("CB: RES 5 A : res_5_a() not implemented! {:#X}", opcode)),
      0xB0 => self.explode(format!("CB: RES 6 B : res_6_b() not implemented! {:#X}", opcode)),
      0xB1 => self.explode(format!("CB: RES 6 C : res_6_c() not implemented! {:#X}", opcode)),
      0xB2 => self.explode(format!("CB: RES 6 D : res_6_d() not implemented! {:#X}", opcode)),
      0xB3 => self.explode(format!("CB: RES 6 E : res_6_e() not implemented! {:#X}", opcode)),
      0xB4 => self.explode(format!("CB: RES 6 H : res_6_h() not implemented! {:#X}", opcode)),
      0xB5 => self.explode(format!("CB: RES 6 L : res_6_l() not implemented! {:#X}", opcode)),
      0xB6 => self.explode(format!("CB: RES 6 (HL) : res_6_hl() not implemented! {:#X}", opcode)),
      0xB7 => self.explode(format!("CB: RES 6 A : res_6_a() not implemented! {:#X}", opcode)),
      0xB8 => self.explode(format!("CB: RES 7 B : res_7_b() not implemented! {:#X}", opcode)),
      0xB9 => self.explode(format!("CB: RES 7 C : res_7_c() not implemented! {:#X}", opcode)),
      0xBA => self.explode(format!("CB: RES 7 D : res_7_d() not implemented! {:#X}", opcode)),
      0xBB => self.explode(format!("CB: RES 7 E : res_7_e() not implemented! {:#X}", opcode)),
      0xBC => self.explode(format!("CB: RES 7 H : res_7_h() not implemented! {:#X}", opcode)),
      0xBD => self.explode(format!("CB: RES 7 L : res_7_l() not implemented! {:#X}", opcode)),
      0xBE => self.explode(format!("CB: RES 7 (HL) : res_7_hl() not implemented! {:#X}", opcode)),
      0xBF => self.explode(format!("CB: RES 7 A : res_7_a() not implemented! {:#X}", opcode)),
      0xC0 => self.explode(format!("CB: SET 0 B : set_0_b() not implemented! {:#X}", opcode)),
      0xC1 => self.explode(format!("CB: SET 0 C : set_0_c() not implemented! {:#X}", opcode)),
      0xC2 => self.explode(format!("CB: SET 0 D : set_0_d() not implemented! {:#X}", opcode)),
      0xC3 => self.explode(format!("CB: SET 0 E : set_0_e() not implemented! {:#X}", opcode)),
      0xC4 => self.explode(format!("CB: SET 0 H : set_0_h() not implemented! {:#X}", opcode)),
      0xC5 => self.explode(format!("CB: SET 0 L : set_0_l() not implemented! {:#X}", opcode)),
      0xC6 => self.explode(format!("CB: SET 0 (HL) : set_0_hl() not implemented! {:#X}", opcode)),
      0xC7 => self.explode(format!("CB: SET 0 A : set_0_a() not implemented! {:#X}", opcode)),
      0xC8 => self.explode(format!("CB: SET 1 B : set_1_b() not implemented! {:#X}", opcode)),
      0xC9 => self.explode(format!("CB: SET 1 C : set_1_c() not implemented! {:#X}", opcode)),
      0xCA => self.explode(format!("CB: SET 1 D : set_1_d() not implemented! {:#X}", opcode)),
      0xCB => self.explode(format!("CB: SET 1 E : set_1_e() not implemented! {:#X}", opcode)),
      0xCC => self.explode(format!("CB: SET 1 H : set_1_h() not implemented! {:#X}", opcode)),
      0xCD => self.explode(format!("CB: SET 1 L : set_1_l() not implemented! {:#X}", opcode)),
      0xCE => self.explode(format!("CB: SET 1 (HL) : set_1_hl() not implemented! {:#X}", opcode)),
      0xCF => self.explode(format!("CB: SET 1 A : set_1_a() not implemented! {:#X}", opcode)),
      0xD0 => self.explode(format!("CB: SET 2 B : set_2_b() not implemented! {:#X}", opcode)),
      0xD1 => self.explode(format!("CB: SET 2 C : set_2_c() not implemented! {:#X}", opcode)),
      0xD2 => self.explode(format!("CB: SET 2 D : set_2_d() not implemented! {:#X}", opcode)),
      0xD3 => self.explode(format!("CB: SET 2 E : set_2_e() not implemented! {:#X}", opcode)),
      0xD4 => self.explode(format!("CB: SET 2 H : set_2_h() not implemented! {:#X}", opcode)),
      0xD5 => self.explode(format!("CB: SET 2 L : set_2_l() not implemented! {:#X}", opcode)),
      0xD6 => self.explode(format!("CB: SET 2 (HL) : set_2_hl() not implemented! {:#X}", opcode)),
      0xD7 => self.explode(format!("CB: SET 2 A : set_2_a() not implemented! {:#X}", opcode)),
      0xD8 => self.explode(format!("CB: SET 3 B : set_3_b() not implemented! {:#X}", opcode)),
      0xD9 => self.explode(format!("CB: SET 3 C : set_3_c() not implemented! {:#X}", opcode)),
      0xDA => self.explode(format!("CB: SET 3 D : set_3_d() not implemented! {:#X}", opcode)),
      0xDB => self.explode(format!("CB: SET 3 E : set_3_e() not implemented! {:#X}", opcode)),
      0xDC => self.explode(format!("CB: SET 3 H : set_3_h() not implemented! {:#X}", opcode)),
      0xDD => self.explode(format!("CB: SET 3 L : set_3_l() not implemented! {:#X}", opcode)),
      0xDE => self.explode(format!("CB: SET 3 (HL) : set_3_hl() not implemented! {:#X}", opcode)),
      0xDF => self.explode(format!("CB: SET 3 A : set_3_a() not implemented! {:#X}", opcode)),
      0xE0 => self.explode(format!("CB: SET 4 B : set_4_b() not implemented! {:#X}", opcode)),
      0xE1 => self.explode(format!("CB: SET 4 C : set_4_c() not implemented! {:#X}", opcode)),
      0xE2 => self.explode(format!("CB: SET 4 D : set_4_d() not implemented! {:#X}", opcode)),
      0xE3 => self.explode(format!("CB: SET 4 E : set_4_e() not implemented! {:#X}", opcode)),
      0xE4 => self.explode(format!("CB: SET 4 H : set_4_h() not implemented! {:#X}", opcode)),
      0xE5 => self.explode(format!("CB: SET 4 L : set_4_l() not implemented! {:#X}", opcode)),
      0xE6 => self.explode(format!("CB: SET 4 (HL) : set_4_hl() not implemented! {:#X}", opcode)),
      0xE7 => self.explode(format!("CB: SET 4 A : set_4_a() not implemented! {:#X}", opcode)),
      0xE8 => self.explode(format!("CB: SET 5 B : set_5_b() not implemented! {:#X}", opcode)),
      0xE9 => self.explode(format!("CB: SET 5 C : set_5_c() not implemented! {:#X}", opcode)),
      0xEA => self.explode(format!("CB: SET 5 D : set_5_d() not implemented! {:#X}", opcode)),
      0xEB => self.explode(format!("CB: SET 5 E : set_5_e() not implemented! {:#X}", opcode)),
      0xEC => self.explode(format!("CB: SET 5 H : set_5_h() not implemented! {:#X}", opcode)),
      0xED => self.explode(format!("CB: SET 5 L : set_5_l() not implemented! {:#X}", opcode)),
      0xEE => self.explode(format!("CB: SET 5 (HL) : set_5_hl() not implemented! {:#X}", opcode)),
      0xEF => self.explode(format!("CB: SET 5 A : set_5_a() not implemented! {:#X}", opcode)),
      0xF0 => self.explode(format!("CB: SET 6 B : set_6_b() not implemented! {:#X}", opcode)),
      0xF1 => self.explode(format!("CB: SET 6 C : set_6_c() not implemented! {:#X}", opcode)),
      0xF2 => self.explode(format!("CB: SET 6 D : set_6_d() not implemented! {:#X}", opcode)),
      0xF3 => self.explode(format!("CB: SET 6 E : set_6_e() not implemented! {:#X}", opcode)),
      0xF4 => self.explode(format!("CB: SET 6 H : set_6_h() not implemented! {:#X}", opcode)),
      0xF5 => self.explode(format!("CB: SET 6 L : set_6_l() not implemented! {:#X}", opcode)),
      0xF6 => self.explode(format!("CB: SET 6 (HL) : set_6_hl() not implemented! {:#X}", opcode)),
      0xF7 => self.explode(format!("CB: SET 6 A : set_6_a() not implemented! {:#X}", opcode)),
      0xF8 => self.explode(format!("CB: SET 7 B : set_7_b() not implemented! {:#X}", opcode)),
      0xF9 => self.explode(format!("CB: SET 7 C : set_7_c() not implemented! {:#X}", opcode)),
      0xFA => self.explode(format!("CB: SET 7 D : set_7_d() not implemented! {:#X}", opcode)),
      0xFB => self.explode(format!("CB: SET 7 E : set_7_e() not implemented! {:#X}", opcode)),
      0xFC => self.explode(format!("CB: SET 7 H : set_7_h() not implemented! {:#X}", opcode)),
      0xFD => self.explode(format!("CB: SET 7 L : set_7_l() not implemented! {:#X}", opcode)),
      0xFE => self.explode(format!("CB: SET 7 (HL) : set_7_hl() not implemented! {:#X}", opcode)),
      0xFF => self.explode(format!("CB: SET 7 A : set_7_a() not implemented! {:#X}", opcode)),
      _ => self.explode(format!("Unexpected CB opcode: {:#X}", opcode)),
    }
  }

  // CB opcodes

  fn res_bit_a(&mut self, bit: types::Byte) {
    shared_reset_bit_reg(self, bit, RegEnum::A);
  }

  fn res_bit_hl(&mut self, bit: types::Byte, mmu: &mut mmu::MMU) {
    let address = self.read_word_reg(RegEnum::HL);
    let value = mmu.read(address);
    let result = value & (!(0x1 << bit));
    mmu.write(address, value);
  }

  fn bit_7_a(&mut self) {
    shared_bit_n_reg(self, 7, RegEnum::A);
  }

  fn bit_7_h(&mut self, bit_number: u8, register: RegEnum) {
    shared_bit_n_reg(self, bit_number, register);
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
}

fn shared_reset_bit_reg(cpu: &mut CPU, bit: types::Byte, regEnum: RegEnum) {
  let value = cpu.read_byte_reg(regEnum);
  let result = value & (!(0x1 << bit));
  cpu.write_byte_reg(regEnum, result);
}

fn shared_bit_n_reg(cpu: &mut CPU, bit: types::Byte, regEnum: RegEnum) {
  if (cpu.read_byte_reg(regEnum) & (1 << bit)) == 0 {
    cpu.util_set_flag(FLAG_ZERO);
  } else {
    cpu.util_untoggle_flag(FLAG_ZERO);
  }

  cpu.util_toggle_flag(FLAG_HALF_CARRY);
  cpu.util_untoggle_flag(FLAG_SUB);
}

// TODO reduce duplication between this and shared_bit_n_reg
fn shared_bit_n_hl(cpu: &mut CPU, bit: types::Byte, mmu: &mmu::MMU) {
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

fn shared_rl_n(cpu: &mut CPU, regEnum: RegEnum) {
  let carry = cpu.util_is_flag_set(FLAG_CARRY);

  let mut result = cpu.read_byte_reg(regEnum);
  if result & 0x80 != 0 {
    cpu.util_set_flag(FLAG_CARRY);
  } else {
    cpu.util_clear_all_flags();
  }

  result <<= 1;
  result |= carry as types::Byte;

  cpu.write_byte_reg(regEnum, result);

  if regEnum != RegEnum::A {
    cpu.util_toggle_zero_flag_from_result(result);
  }
}

fn shared_sla_n(cpu: &mut CPU, regEnum: RegEnum) {
  let value = cpu.read_byte_reg(regEnum);
  if value & 0x80 != 0 {
    cpu.util_set_flag(FLAG_CARRY);
  } else {
    cpu.util_clear_all_flags();
  }

  let result = value << 1;

  cpu.write_byte_reg(regEnum, result);
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_srl_n(cpu: &mut CPU, regEnum: RegEnum) {
  let value = cpu.read_byte_reg(regEnum);
  let carry = value & 0x01 == 0x01;
  let result = value >> 1;

  shared_update_srl_flags(cpu, result, carry);
  cpu.write_byte_reg(regEnum, result);
}

fn shared_update_srl_flags(cpu: &mut CPU, result: u8, carry: bool) {
  cpu.util_untoggle_flag(FLAG_HALF_CARRY);
  cpu.util_untoggle_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result);
  if carry {
    cpu.util_toggle_flag(FLAG_CARRY);
  } else {
    cpu.util_untoggle_flag(FLAG_CARRY);
  }
}

fn shared_ld_reg_n(cpu: &mut CPU, regEnum: RegEnum, mmu: &mmu::MMU) {
    let operand = mmu.read(cpu.PC);
    cpu.write_byte_reg(regEnum, operand);
    cpu.PC += 1;
  }

fn shared_swap_register(cpu: &mut CPU, regEnum: RegEnum) {
  let value = cpu.read_byte_reg(regEnum);
  let low_half = value & 0x0F;
  let high_half = (value >> 4) & 0x0F;
  let result = (low_half << 4) + high_half;
  cpu.write_byte_reg(regEnum, result);

  cpu.util_clear_all_flags();
  cpu.util_toggle_zero_flag_from_result(result);
}

fn shared_inc_word_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_word_reg(regEnum).wrapping_add(1);
  cpu.write_word_reg(regEnum, result);
}

fn shared_xor_n(cpu: &mut CPU, byte: types::Byte) {
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

fn shared_add_byte_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let A = cpu.read_byte_reg(RegEnum::A);
  let value = cpu.read_byte_reg(regEnum);
  let result = value.wrapping_add(A);
  cpu.write_byte_reg(RegEnum::A, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_toggle_zero_flag_from_result(result);

  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (A & 0xF) + (value & 0xF) > 0xF);
}

fn shared_add_n(cpu: &mut CPU, byte: types::Byte, carry_preserve: bool) {
  let c = if carry_preserve && cpu.util_is_flag_set(FLAG_CARRY) { 1 } else { 0 };
  let A = cpu.read_byte_reg(RegEnum::A);
  let result = A.wrapping_add(byte).wrapping_add(c);

  cpu.util_toggle_zero_flag_from_result(result);

  if (A & 0xF) + (byte & 0xF) + c > 0xF {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }

  cpu.util_untoggle_flag(FLAG_SUB);

  if carry_preserve && cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.write_byte_reg(RegEnum::A, result);
}

fn shared_add_word_and_word_regs(cpu: &mut CPU, regEnum1: RegEnum, regEnum2: RegEnum) {
  let value_1 = cpu.read_word_reg(regEnum1);
  let value_2 = cpu.read_word_reg(regEnum2);

  let result = value_1.wrapping_add(value_2);

  cpu.util_set_flag_by_boolean(FLAG_HALF_CARRY, (value_1 & 0x07FF) + (value_2 & 0x07FF) > 0x07FF);
  cpu.util_set_flag_by_boolean(FLAG_SUB, false);
  cpu.util_set_flag_by_boolean(FLAG_CARRY, value_1 > 0xFFFF - value_2);
  cpu.write_word_reg(regEnum1, result);
}

fn shared_ld_n_nn(cpu: &mut CPU, mmu: &mut mmu::MMU, regEnumDst: RegEnum, regEnumSrc: RegEnum) {
  let address = cpu.read_word_reg(regEnumSrc);
  let value = mmu.read(address);
  cpu.write_byte_reg(regEnumDst, value);
}

fn shared_dec_byte_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let result = cpu.read_byte_reg(regEnum).wrapping_sub(1);
  cpu.write_byte_reg(regEnum, result);

  if cpu.util_is_flag_set(FLAG_CARRY) { cpu.util_set_flag(FLAG_CARRY) } else { cpu.util_clear_all_flags() }
  cpu.util_toggle_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result);

  if (result & 0x0F) == 0x0F {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }
}

fn shared_sub_byte_reg(cpu: &mut CPU, regEnum: RegEnum) {
  let A = cpu.read_byte_reg(RegEnum::A);
  let number = cpu.read_byte_reg(regEnum);
  let result = A - number;
  let carry_bits = A ^ number ^ result;

  cpu.write_byte_reg(RegEnum::A, carry_bits as types::Byte);
  cpu.util_set_flag(FLAG_SUB);
  cpu.util_toggle_zero_flag_from_result(result as types::Byte);

  if carry_bits & 0x100 != 0 {
    cpu.util_toggle_flag(FLAG_CARRY);
  }

  if carry_bits & 0x10 != 0 {
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
    RegEnum::A => cpu.util_toggle_zero_flag_from_result(result),
    _ => {} // NOTE Only do this for Register A
  }
}

fn shared_adc(cpu: &mut CPU, byte: types::Byte) {
  panic!("not implemented!")
}

fn shared_cp(cpu: &mut CPU, byte: types::Byte) {
  let a = cpu.read_byte_reg(RegEnum::A);
  let result = a.wrapping_sub(byte);

  cpu.util_set_flag(FLAG_SUB);

  if a < byte {
    cpu.util_toggle_flag(FLAG_CARRY);
  }

  if result == 0x00 {
    cpu.util_toggle_flag(FLAG_ZERO);
  }

  if (result & 0xF) > (a & 0xF) {
    cpu.util_toggle_flag(FLAG_HALF_CARRY);
  }

  cpu.write_byte_reg(RegEnum::A, result);
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

impl CPU {
  // Interrupt handling

  pub fn handle_interrupts(&mut self, mmu: &mut mmu::MMU) -> i32 {
    if self.IME == false && self.halted == false { return 0 }

    let interrupt_to_handle = mmu.InterruptEnabled & mmu.InterruptFlags;
    if interrupt_to_handle == 0 { return 0 }

    self.halted = false;
    if self.IME == false { return 0 }
    self.IME = false;

    let interrupt_offset = interrupt_to_handle.trailing_zeros() as types::Word;
    if interrupt_offset >= 5 { panic!("Invalid interrupt"); }

    mmu.InterruptFlags &= !(1 << interrupt_offset);

    let current_PC = self.PC;
    self.stack_push(current_PC, mmu);

    if interrupt_offset != 0 {
      panic!("Doing interrupt other than vblank, {}", interrupt_offset);
    }

    self.PC = 0x0040 | ((interrupt_offset) << 3);

    16
  }

  fn update_ime(&mut self) {
    self.di_cycles = match self.di_cycles {
        2 => 1,
        1 => { self.IME = false; 0 },
        _ => 0,
    };
    self.ei_cycles = match self.ei_cycles {
        2 => 1,
        1 => { self.IME = true; 0 },
        _ => 0,
    };
  }
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

// #[test]
// fn opcode_xor_a() {
//   let mut cpu = CPU::new();
//   let mut mmu = mmu::MMU::new();
//   mmu.cartridge = cartridge::Cartridge { buffer: vec![ opcodes::XOR_A ] };

//   cpu.AF.write_hi(0xFF);

//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(0x00, cpu.AF.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.PC);
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
//   cpu.AF.write_hi(0xD4);

//   let cycles = cpu.execute_next_opcode(&mut mmu);
//   assert_eq!(0x31, cpu.AF.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.PC);
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
//   // assert_eq!(0x31, cpu.AF.read_hi());
//   assert_eq!(4, cycles);
//   assert_eq!(0x0001, cpu.PC);
//   assert!(!cpu.util_is_flag_set(FLAG_ZERO));
//   assert!(cpu.util_is_flag_set(FLAG_SUB));
//   assert!(cpu.util_is_flag_set(FLAG_HALF_CARRY));
//   assert!(!cpu.util_is_flag_set(FLAG_CARRY));
// }
