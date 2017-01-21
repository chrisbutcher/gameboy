use std::fmt;

pub use super::types;

const FLAG_Z: u8 = 7; // Zero
const FLAG_N: u8 = 6; // Negative
const FLAG_H: u8 = 5; // Half-carry
const FLAG_C: u8 = 4; // Carry

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

  pub fn tick(&self) {
    println!("Tick!");
    // Fetch next op address. op_index = memory.read_byte[self.pc]
    // Increment pc
    // Execute. opcode_fn(op_index)
    // cycles = opcode_timing(op_index)
  }

  pub fn flags(&self) -> types::Byte {
    self.AF.read_lo()
  }

  pub fn set_flag_z(&mut self) {
    let flags = self.AF.read_lo();
    let result = flags | 1 << FLAG_Z;

    println!("{:b}", flags);
    println!("{:b}", result);
    self.AF.write_lo(result);
  }

  pub fn flag_z(&self) -> bool {
    (self.AF.read_lo() & 1 << FLAG_Z) != 0
  }

  pub fn flag_n(&self) -> bool {
    (self.AF.read_lo() & 1 << FLAG_N) != 0
  }

  pub fn flag_h(&self) -> bool {
    (self.AF.read_lo() & 1 << FLAG_H) != 0
  }

  pub fn flag_c(&self) -> bool {
    (self.AF.read_lo() & 1 << FLAG_C) != 0
  }
}

#[test]
fn set_flags() {
  let mut cpu = CPU::new();
  assert_eq!(false, cpu.flag_z());
  cpu.set_flag_z();
  assert_eq!(true, cpu.flag_z());
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
