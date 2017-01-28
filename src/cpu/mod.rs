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

  fn execute_opcode(&mut self, opcode: types::Byte, mmu: &mut mmu::MMU) -> i32 {
    match opcode {
      opcodes::NOP => {
        self.op_nop()
      },
      opcodes::JP_NN => {
        self.op_jp_nn(mmu)
      },
      opcodes::XOR_A => {
        self.op_xor_8bit_n(RegEnum::A, mmu)
      },
      opcodes::XOR_B => {
        self.op_xor_8bit_n(RegEnum::B, mmu)
      },
      opcodes::DEC_B => {
        self.dec_n(RegEnum::B, mmu)
      },
      opcodes::LD_HL_NN => {
        self.ld_n_nn(RegEnum::HL, mmu)
      },
      opcodes::LD_C_N => {
        self.ld_nn_n(RegEnum::C, mmu)
      },
      opcodes::LD_B_N => {
        self.ld_nn_n(RegEnum::B, mmu)
      },
      opcodes::LDD_HL_A => {
        self.ldd_nn_n(RegEnum::A, RegEnum::HL)
      },
      _ => panic!("Unexpected opcode: {:x}", opcode)
    }
    // Ok(cycles)
  }

  fn op_nop(&self) -> i32 {
    4
  }

  fn op_jp_nn(&mut self, mmu: &mmu::MMU) -> i32 {
    self.PC = mmu.read_word(self.PC);

    12
  }

  fn op_xor_8bit_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
    let A = self.AF.read_hi();

    let n = match reg {
      RegEnum::A => A,
      RegEnum::B => self.BC.read_hi(),
      _ => panic!("Unexpected RegEnum: {:?}", reg)
    };

    let result = n ^ A;
    self.AF.write_hi(result);

    self.reset_all_flags();

    if result == 0x00 {
      self.set_flag(FLAG_Z);
    } else {
      self.reset_flag(FLAG_Z);
    }

    4
  }

  fn dec_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
    let (n, before) = match reg {
      RegEnum::B => {
        let before = self.BC.read_hi();
        let n = before.wrapping_sub(1);
        self.BC.write_hi(n);
        (n, before)
      },
      _ => panic!("Unexpected RegEnum: {:?}", reg)
    };

    if n == 0x00 {
      self.set_flag(FLAG_Z);
    }

    self.set_flag(FLAG_N);

    if (before & 0x0f) == 0 {
      self.set_flag(FLAG_H);
    } else {
      self.reset_flag(FLAG_H);
    }

    4 // TODO
  }

  fn ld_n_nn(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
    let nn = mmu.read_word(self.PC);
    self.PC += 2; // TODO is this correct?
    // println!("ld_n_nn -> nn: {:x}", nn);

    match reg {
      RegEnum::HL => {
        self.HL.write(nn);
      },
      _ => panic!("Unexpected RegEnum: {:?}", reg)
    };

    12
  }

  fn ld_nn_n(&mut self, reg: RegEnum, mmu: &mut mmu::MMU) -> i32 {
    let nn = mmu.read(self.PC);
    self.PC += 1;

    match reg {
      RegEnum::C => {
        self.BC.write_lo(nn);
      },
      RegEnum::B => {
        self.BC.write_hi(nn);
      },
      _ => panic!("Unexpected RegEnum: {:?}", reg)
    };

    8
  }

  fn ldd_nn_n(&mut self, regSrc: RegEnum, regDstAndDec: RegEnum) -> i32 {
    let srcVal = match regSrc {
      RegEnum::A => {
        self.AF.read_hi()
      },
      _ => panic!("Unexpected RegEnum: {:?}", regSrc)
    };

    // println!("{:x}", srcVal);
    // NOTE: wrapping_sub
    // https://doc.rust-lang.org/std/primitive.u8.html#method.wrapping_add

    match regDstAndDec {
      RegEnum::HL => {
        self.HL.write((srcVal.wrapping_sub(1)) as types::Word);
      },
      _ => panic!("Unexpected RegEnum: {:?}", regDstAndDec)
    };

    8
  }

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
