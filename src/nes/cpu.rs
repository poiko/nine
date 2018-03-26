use std::fmt;
use super::super::nes;
use super::ppu;

const RAM_SIZE: usize = 0x10000;

const INT_NMI_POS: usize = 0xfffa;
const INT_RESET_POS: usize = 0xfffc;
const INT_IRQ_POS: usize = 0xfffe;

enum AddrMode {
    Imm,
    Rel,
    ZP,
    IdxZPX,
    IdxZPY,
    Abs,
    IdxAbsX,
    IdxAbsY,
    Ind,
    IdxInd,
    IndIdx,
    None
}

impl fmt::Display for AddrMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match *self {
            AddrMode::Imm => "immediate",
            AddrMode::Rel => "relative",
            AddrMode::ZP => "zero page",
            AddrMode::IdxZPX => "X-indexed zero page",
            AddrMode::IdxZPY => "Y-indexed zero page",
            AddrMode::Abs => "absolute",
            AddrMode::IdxAbsX => "X-indexed absolute",
            AddrMode::IdxAbsY => "Y-indexed absolute",
            AddrMode::Ind => "indirect",
            AddrMode::IdxInd => "indexed indirect",
            AddrMode::IndIdx => "indirect indexed",
            AddrMode::None => "none"
        };
        write!(f, "{}", text)
    }
}

struct Instruction {
    name: &'static str,
    opcode: u8,
    bytes: u8,
    cycles: u8,
    addr_mode: AddrMode,
    page_cycle: bool,
    update_nz: bool
}

#[derive(Default)]
pub struct CPU {
    ppu: ppu::PPU,

    pc_reg: u16,
    sp_reg: u8,
    acc_reg: u8,
    x_reg: u8,
    y_reg: u8,

    carry: bool,
    zero: bool,
    int_disable: bool,
    bcd_mode: bool,
    brk_cmd: bool,
    overflow: bool,
    negative: bool,

    cycles_run: u32,
    ram: Vec<u8>
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            ppu: ppu::PPU::new(),
            ram: vec![0; RAM_SIZE],
            ..Default::default()
        }
    }

    fn read_word(&self, addr: usize) -> u16 {
        self.ram[addr] as u16 | ((self.ram[addr+1] as u16) << 8)
    }

    fn read_word_mapped(&mut self, addr: usize) -> u16 {
        self.read_byte_mapped(addr) as u16 | ((self.read_byte_mapped(addr+1) as u16) << 8)
    }

    fn read_byte_mapped(&mut self, addr: usize) -> u8 {
        if addr < 0x2000 {
            self.ram[addr & 0x7ff]
        } else if addr < 0x4000 {
            self.ppu.read_io_reg(addr & 0b111)
        } else if addr >= 0x8000 {
            self.ram[addr]
        } else {
            panic!("Unhandled CPU memory mapped read at {:x}", addr);
        }
    }

    fn write_byte_mapped(&mut self, addr: usize, val: u8) {
        if addr < 0x2000 {
            self.ram[addr] = val;
        } else if addr < 0x4000 {
            self.ppu.write_io_reg(addr & 0b111, val);
        } else {
            panic!("Unhandled CPU memory mapped write at {:x}", addr);
        }
    }

    fn push_byte(&mut self, val: u8) {
        let sp_addr = self.sp_reg as usize + 0x100;
        self.ram[sp_addr] = val;
        self.sp_reg = self.sp_reg.wrapping_sub(1);
    }

    fn pop_byte(&mut self) -> u8 {
        self.sp_reg = self.sp_reg.wrapping_add(1);
        let sp_addr = self.sp_reg as usize + 0x100;
        self.ram[sp_addr]
    }

    pub fn print_stack(&self) {
        let mut addr = self.sp_reg as usize + 0x100 + 1;
        while addr < 0x200 {
            print!(" {:x}", self.ram[addr]);
            addr += 1;
        }
        println!("");
    }

    pub fn load_rom(&mut self, rom: &nes::ROM) {
        if rom.num_prg_banks == 1 {
            self.ram[0x8000..0xc000].copy_from_slice(&rom.prg_rom);
            self.ram[0xc000..0x10000].copy_from_slice(&rom.prg_rom);
        } else if rom.num_prg_banks == 2 {
            self.ram[0x8000..0x10000].copy_from_slice(&rom.prg_rom);
        } else {
//            match rom.mapper_id {
//                1 => {
//                    self.ram[0x8000..0xc000].copy_from_slice(&rom.prg_rom[..0x4000]);
//                    let last_bank_start = nes::PRG_BANK_SIZE * (rom.num_prg_banks-1) as usize;
//                    let last_bank_end = last_bank_start + 0x4000;
//                    self.ram[0xc000..0x10000].copy_from_slice(&rom.prg_rom[last_bank_start..last_bank_end]);
//                },
//                _ => panic!("unsupported mapper id {}!", rom.mapper_id)
            //            }
                panic!("Unsupported mapper id {}!", rom.mapper_id)
        }
    }

    pub fn reset(&mut self) {
        self.pc_reg = self.read_word(INT_RESET_POS);
        self.cycles_run = 0;
    }

    pub fn run_step(&mut self) {
        let opcode = self.ram[self.pc_reg as usize];
        self.run_instruction(&INSTRUCTIONS[opcode as usize]);
        self.ppu.run_step();
    }

    pub fn run_instruction(&mut self, instr: &Instruction) {
        let pc = self.pc_reg as usize + 1;
        let addr = match instr.addr_mode {
            AddrMode::Imm | AddrMode::Rel => {
                pc as usize
            }
            AddrMode::ZP => {
                self.ram[pc] as usize
            }
            AddrMode::IdxZPX => {
                self.ram[pc].wrapping_add(self.x_reg) as usize
            }
            AddrMode::IdxZPY => {
                self.ram[pc].wrapping_add(self.y_reg) as usize
            }
            AddrMode::Abs => {
                self.read_word(pc) as usize
            }
            AddrMode::IdxAbsX => {
                self.read_word(pc).wrapping_add(self.x_reg as u16) as usize
            }
            AddrMode::IdxAbsY => {
                self.read_word(pc).wrapping_add(self.y_reg as u16) as usize
            }
            AddrMode::Ind => {
                let ind_addr = self.read_word(pc) as usize;
                if ind_addr & 0xff == 0xff {
                    (self.read_byte_mapped(ind_addr) as u16 | ((self.read_byte_mapped(ind_addr & 0xff00) as u16) << 8)) as usize
                } else {
                    self.read_word_mapped(ind_addr) as usize
                }
            }
            AddrMode::IdxInd => {
                let ind_addr = self.ram[pc].wrapping_add(self.x_reg) as usize;
                self.read_word(ind_addr) as usize
            }
            AddrMode::IndIdx => {
                let ind_addr = self.ram[pc] as usize;
                self.read_word(ind_addr).wrapping_add(self.y_reg as u16) as usize
            }
            AddrMode::None => { 0 }
        };

        self.pc_reg += instr.bytes as u16;
        self.cycles_run += instr.cycles as u32;

        let mut result: u8 = 0;
        match instr.opcode {
            0x00 => {
                // BRK
                unimplemented!();
            }
            0x01 | 0x05 | 0x09 | 0x0d | 0x11 | 0x15 | 0x19 | 0x1d => {
                // ORA
                self.acc_reg |= self.read_byte_mapped(addr);
                result = self.acc_reg;
            }
            0x10 => {
                // BPL
                if !self.negative {
                    let rel_addr = self.ram[addr] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    // TODO: page boundary check
                    self.cycles_run += 1;
                }
            }
            0x18 => {
                // CLC
                self.carry = false;
            }
            0x20 => {
                // JSR
                let ret_addr = self.pc_reg + 2;
                self.push_byte((ret_addr >> 8) as u8);
                self.push_byte((ret_addr & 0xff) as u8);
                self.pc_reg = addr as u16;
            }
            0x21 | 0x25 | 0x29 | 0x2d | 0x31 | 0x35 | 0x39 | 0x3d => {
                // AND
                self.acc_reg &= self.read_byte_mapped(addr);
                result = self.acc_reg;
            }
            0x41 | 0x45 | 0x49 | 0x4d | 0x51 | 0x55 | 0x59 | 0x5d => {
                // EOR
                self.acc_reg ^= self.read_byte_mapped(addr);
                result = self.acc_reg;
            }
            0x46 | 0x4e | 0x56 | 0x5e => {
                // LSR memory
                let val = self.read_byte_mapped(addr);
                self.carry = (val & 1) != 0;
                self.acc_reg = val >> 1;
                result = self.acc_reg;
            }
            0x48 => {
                // PHA
                let val = self.acc_reg;
                self.push_byte(val);
            }
            0x4a => {
                // LSR accumulator
                self.carry = (self.acc_reg & 1) != 0;
                self.acc_reg >>= 1;
                result = self.acc_reg;
            }
            0x4c | 0x6c => {
                // JMP
                self.pc_reg = self.read_word(addr);
            }
            0x60 => {
                // RTS
                let ret_addr_lo = self.pop_byte() as u16;
                let ret_addr_hi = self.pop_byte() as u16;
                self.pc_reg = ((ret_addr_hi << 8) + ret_addr_lo).wrapping_add(1);
            }
            0x61 | 0x65 | 0x69 | 0x6d | 0x71 | 0x75 | 0x79 | 0x7d => {
                // ADC
                let val = self.read_byte_mapped(addr);
                let mut acc_int = self.acc_reg as i32 + val as i32;
                if self.carry {
                    acc_int += 1;
                }
                self.carry = acc_int > 0xff;
                // TODO: this is wrong
                if acc_int > 127 || acc_int < -128 {
                    self.overflow = true;
                } else {
                    self.overflow = false;
                }
                self.acc_reg = acc_int as u8;
                result = self.acc_reg;
            }
            0x66 | 0x6e | 0x76 | 0x7e => {
                // ROR memory
                let mut val = self.read_byte_mapped(addr);
                let new_carry = val & 1 != 0;
                val >>= 1;
                if self.carry {
                    val |= 128;
                }
                self.write_byte_mapped(addr, val);
                self.carry = new_carry;
                result = val;
            }
            0x68 => {
                // PLA
                self.acc_reg = self.pop_byte();
            }
            0x6a => {
                // ROR accumulator
                let new_carry = self.acc_reg & 1 != 0;
                self.acc_reg >>= 1;
                if self.carry {
                    self.acc_reg |= 128;
                }
                self.carry = new_carry;
                result = self.acc_reg;
            }
            0x78 => {
                // SEI
                self.int_disable = true;
            }
            0x81 | 0x85 | 0x8d | 0x91 | 0x95 | 0x99 | 0x9d => {
                // STA
                let val = self.acc_reg;
                self.write_byte_mapped(addr, val);
            }
            0x84 | 0x8c | 0x94 => {
                // STY
                let val = self.y_reg;
                self.write_byte_mapped(addr, val);
            }
            0x88 => {
                // DEY
                self.y_reg = self.y_reg.wrapping_sub(1);
                result = self.y_reg;
            }
            0x8a => {
                // TXA
                self.acc_reg = self.x_reg;
                result = self.x_reg;
            }
            0x81 | 0x85 | 0x8d | 0x91 | 0x95 | 0x99 | 0x9d => {
                // STA
                let val = self.acc_reg;
                self.write_byte_mapped(addr, val);
            }
            0x98 => {
                // TYA
                self.acc_reg = self.y_reg;
                result = self.y_reg;
            }
            0x9a => {
                // TXS
                self.sp_reg = self.x_reg;
            }
            0xa0 | 0xa4 | 0xac | 0xb4 | 0xbc => {
                // LDY
                self.y_reg = self.read_byte_mapped(addr);
                result = self.y_reg;
            }
            0xa1 | 0xa5 | 0xa9 | 0xad | 0xb1 | 0xb5 | 0xb9 | 0xbd => {
                // LDA
                self.acc_reg = self.read_byte_mapped(addr);
                result = self.acc_reg;
            }
            0xa2 | 0xa6 | 0xae | 0xb6 | 0xbe => {
                // LDX
                self.x_reg = self.read_byte_mapped(addr);
                result = self.x_reg;
            }
            0xa8 => {
                // TAY
                self.y_reg = self.acc_reg;
                result = self.acc_reg;
            }
            0xaa => {
                // TAX
                self.x_reg = self.acc_reg;
                result = self.acc_reg;
            }
            0xc1 | 0xc5 | 0xc9 | 0xcd | 0xd1 | 0xd5 | 0xd9 | 0xdd => {
                // CMP
                let val = self.read_byte_mapped(addr);
                self.carry = self.acc_reg >= val;
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == val;
                result = self.acc_reg;
            }
            0xca => {
                // DEX
                self.x_reg = self.x_reg.wrapping_sub(1);
                result = self.x_reg;
            }
            0xc6 | 0xce | 0xd6 | 0xde => {
                // DEC
                result = self.read_byte_mapped(addr).wrapping_sub(1);
                self.write_byte_mapped(addr, result);
            }
            0xc8 => {
                // INY
                self.y_reg = self.y_reg.wrapping_add(1);
                result = self.y_reg;
            }
            0xd0 => {
                // BNE
                if !self.zero {
                    let rel_addr = self.ram[addr] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    // TODO: page boundary check
                    self.cycles_run += 1;
                }
            }
            0xd8 => {
                // CLD
            }
            0xe1 | 0xe5 | 0xe9 | 0xed | 0xf1 | 0xf5 | 0xf9 | 0xfd => {
                // SBC
                let val = self.read_byte_mapped(addr);
                let mut acc_int = self.acc_reg as i32 - val as i32;
                if !self.carry {
                    acc_int -= 1;
                }
                self.carry = acc_int >= 0;
                // TODO: this is wrong
                if acc_int > 127 || acc_int < -128 {
                    self.overflow = true;
                } else {
                    self.overflow = false;
                }
                self.acc_reg = acc_int as u8;
                result = self.acc_reg;
            }
            0xf0 => {
                // BEQ
                if self.zero {
                    let rel_addr = self.ram[addr] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    // TODO: page boundary check
                    self.cycles_run += 1;
                }
            }
            _ => {
                panic!("Unrecognized opcode {:x}", instr.opcode);
            }
        };

        if instr.update_nz {
            self.negative = (result & 128) != 0;
            self.zero = result == 0;
        }

        println!("{:x}: {} (0x{:x}) {}, result = 0x{:x}", self.pc_reg, instr.name, instr.opcode, instr.addr_mode,
                 result);
    }
}

macro_rules! instruction {
    ($name: expr, $opcode: expr, $bytes: expr, $cycles: expr, $addr_mode: ident, $page_cycle: expr, $update_nz: expr) =>
        (Instruction { name: $name, opcode: $opcode, bytes: $bytes, cycles: $cycles, addr_mode: AddrMode::$addr_mode,
                      page_cycle: $page_cycle, update_nz: $update_nz })
}

const INSTRUCTIONS: [Instruction; 256] = [
    instruction!("BRK", 0x00, 1, 3, None, false, false),
    instruction!("ORA", 0x01, 2, 6, IdxInd, false, true),
    instruction!("ERR", 0x02, 0, 0, None, false, false),
    instruction!("ERR", 0x03, 0, 0, None, false, false),
    instruction!("ERR", 0x04, 0, 0, None, false, false),
    instruction!("ORA", 0x05, 2, 3, ZP, false, true),
    instruction!("ERR", 0x06, 0, 0, None, false, false),
    instruction!("ERR", 0x07, 0, 0, None, false, false),
    instruction!("ERR", 0x08, 0, 0, None, false, false),
    instruction!("ORA", 0x09, 2, 2, Imm, false, true),
    instruction!("ERR", 0x0a, 0, 0, None, false, false),
    instruction!("ERR", 0x0b, 0, 0, None, false, false),
    instruction!("ERR", 0x0c, 0, 0, None, false, false),
    instruction!("ORA", 0x0d, 3, 4, Abs, false, true),
    instruction!("ERR", 0x0e, 0, 0, None, false, false),
    instruction!("ERR", 0x0f, 0, 0, None, false, false),
    instruction!("BPL", 0x10, 2, 2, Rel, false, false),
    instruction!("ORA", 0x11, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0x12, 0, 0, None, false, false),
    instruction!("ERR", 0x13, 0, 0, None, false, false),
    instruction!("ERR", 0x14, 0, 0, None, false, false),
    instruction!("ORA", 0x15, 2, 4, IdxZPX, false, true),
    instruction!("ERR", 0x16, 0, 0, None, false, false),
    instruction!("ERR", 0x17, 0, 0, None, false, false),
    instruction!("CLC", 0x18, 1, 7, None, false, false),
    instruction!("ORA", 0x19, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0x1a, 0, 0, None, false, false),
    instruction!("ERR", 0x1b, 0, 0, None, false, false),
    instruction!("ERR", 0x1c, 0, 0, None, false, false),
    instruction!("ORA", 0x1d, 3, 4, IdxAbsX, true, true),
    instruction!("ERR", 0x1e, 0, 0, None, false, false),
    instruction!("ERR", 0x1f, 0, 0, None, false, false),
    instruction!("JSR", 0x20, 3, 6, Abs, false, false),
    instruction!("AND", 0x21, 2, 6, IdxInd, false, true),
    instruction!("ERR", 0x22, 0, 0, None, false, false),
    instruction!("ERR", 0x23, 0, 0, None, false, false),
    instruction!("ERR", 0x24, 0, 0, None, false, false),
    instruction!("AND", 0x25, 2, 3, ZP, false, true),
    instruction!("ERR", 0x26, 0, 0, None, false, false),
    instruction!("ERR", 0x27, 0, 0, None, false, false),
    instruction!("ERR", 0x28, 0, 0, None, false, false),
    instruction!("AND", 0x29, 2, 2, Imm, false, true),
    instruction!("ERR", 0x2a, 0, 0, None, false, false),
    instruction!("ERR", 0x2b, 0, 0, None, false, false),
    instruction!("ERR", 0x2c, 0, 0, None, false, false),
    instruction!("AND", 0x2d, 3, 4, Abs, false, true),
    instruction!("ERR", 0x2e, 0, 0, None, false, false),
    instruction!("ERR", 0x2f, 0, 0, None, false, false),
    instruction!("ERR", 0x30, 0, 0, None, false, false),
    instruction!("AND", 0x31, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0x32, 0, 0, None, false, false),
    instruction!("ERR", 0x33, 0, 0, None, false, false),
    instruction!("ERR", 0x34, 0, 0, None, false, false),
    instruction!("AND", 0x35, 2, 4, IdxZPX, false, true),
    instruction!("ERR", 0x36, 0, 0, None, false, false),
    instruction!("ERR", 0x37, 0, 0, None, false, false),
    instruction!("ERR", 0x38, 0, 0, None, false, false),
    instruction!("AND", 0x39, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0x3a, 0, 0, None, false, false),
    instruction!("ERR", 0x3b, 0, 0, None, false, false),
    instruction!("ERR", 0x3c, 0, 0, None, false, false),
    instruction!("AND", 0x3d, 3, 4, IdxAbsX, true, true),
    instruction!("ERR", 0x3e, 0, 0, None, false, false),
    instruction!("ERR", 0x3f, 0, 0, None, false, false),
    instruction!("ERR", 0x40, 0, 0, None, false, false),
    instruction!("EOR", 0x41, 2, 6, IdxInd, false, true),
    instruction!("ERR", 0x42, 0, 0, None, false, false),
    instruction!("ERR", 0x43, 0, 0, None, false, false),
    instruction!("ERR", 0x44, 0, 0, None, false, false),
    instruction!("EOR", 0x45, 2, 3, ZP, false, true),
    instruction!("LSR", 0x46, 2, 5, ZP, false, true),
    instruction!("ERR", 0x47, 0, 0, None, false, false),
    instruction!("PHA", 0x48, 1, 3, None, false, false),
    instruction!("EOR", 0x49, 2, 2, Imm, false, true),
    instruction!("LSR", 0x4a, 1, 2, None, false, true),
    instruction!("ERR", 0x4b, 0, 0, None, false, false),
    instruction!("JMP", 0x4c, 3, 3, Abs, false, false),
    instruction!("EOR", 0x4d, 3, 4, Abs, false, true),
    instruction!("LSR", 0x4e, 3, 6, Abs, false, true),
    instruction!("ERR", 0x4f, 0, 0, None, false, false),
    instruction!("ERR", 0x50, 0, 0, None, false, false),
    instruction!("EOR", 0x51, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0x52, 0, 0, None, false, false),
    instruction!("ERR", 0x53, 0, 0, None, false, false),
    instruction!("ERR", 0x54, 0, 0, None, false, false),
    instruction!("EOR", 0x55, 2, 4, IdxZPX, false, true),
    instruction!("LSR", 0x56, 2, 6, IdxZPX, false, true),
    instruction!("ERR", 0x57, 0, 0, None, false, false),
    instruction!("ERR", 0x58, 0, 0, None, false, false),
    instruction!("EOR", 0x59, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0x5a, 0, 0, None, false, false),
    instruction!("ERR", 0x5b, 0, 0, None, false, false),
    instruction!("ERR", 0x5c, 0, 0, None, false, false),
    instruction!("EOR", 0x5d, 3, 4, IdxAbsX, true, true),
    instruction!("LSR", 0x5e, 3, 7, IdxAbsX, false, true),
    instruction!("ERR", 0x5f, 0, 0, None, false, false),
    instruction!("RTS", 0x60, 1, 6, None, false, false),
    instruction!("ADC", 0x61, 2, 6, IdxInd, false, true),
    instruction!("ERR", 0x62, 0, 0, None, false, false),
    instruction!("ERR", 0x63, 0, 0, None, false, false),
    instruction!("ERR", 0x64, 0, 0, None, false, false),
    instruction!("ADC", 0x65, 2, 3, ZP, false, true),
    instruction!("ROR", 0x66, 2, 5, ZP, false, true),
    instruction!("ERR", 0x67, 0, 0, None, false, false),
    instruction!("PLA", 0x68, 1, 4, None, false, false),
    instruction!("ADC", 0x69, 2, 2, Imm, false, true),
    instruction!("ROR", 0x6a, 1, 2, None, false, true),
    instruction!("ERR", 0x6b, 0, 0, None, false, false),
    instruction!("JMP", 0x6c, 3, 5, Ind, false, false),
    instruction!("ADC", 0x6d, 3, 4, Abs, false, true),
    instruction!("ROR", 0x6e, 3, 6, Abs, false, true),
    instruction!("ERR", 0x6f, 0, 0, None, false, false),
    instruction!("ERR", 0x70, 0, 0, None, false, false),
    instruction!("ADC", 0x71, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0x72, 0, 0, None, false, false),
    instruction!("ERR", 0x73, 0, 0, None, false, false),
    instruction!("ERR", 0x74, 0, 0, None, false, false),
    instruction!("ADC", 0x75, 2, 4, IdxZPX, false, true),
    instruction!("ROR", 0x76, 2, 6, IdxZPX, false, true),
    instruction!("ERR", 0x77, 0, 0, None, false, false),
    instruction!("SEI", 0x78, 1, 2, None, false, false),
    instruction!("ADC", 0x79, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0x7a, 0, 0, None, false, false),
    instruction!("ERR", 0x7b, 0, 0, None, false, false),
    instruction!("ERR", 0x7c, 0, 0, None, false, false),
    instruction!("ADC", 0x7d, 3, 4, IdxAbsX, true, true),
    instruction!("ROR", 0x7e, 3, 7, IdxAbsX, false, true),
    instruction!("ERR", 0x7f, 0, 0, None, false, false),
    instruction!("ERR", 0x80, 0, 0, None, false, false),
    instruction!("STA", 0x81, 2, 6, IdxInd, false, false),
    instruction!("ERR", 0x82, 0, 0, None, false, false),
    instruction!("ERR", 0x83, 0, 0, None, false, false),
    instruction!("STY", 0x84, 2, 3, ZP, false, false),
    instruction!("STA", 0x85, 2, 3, ZP, false, false),
    instruction!("ERR", 0x86, 0, 0, None, false, false),
    instruction!("ERR", 0x87, 0, 0, None, false, false),
    instruction!("DEY", 0x88, 1, 2, None, false, true),
    instruction!("ERR", 0x89, 0, 0, None, false, false),
    instruction!("TXA", 0x8a, 1, 2, None, false, true),
    instruction!("ERR", 0x8b, 0, 0, None, false, false),
    instruction!("STY", 0x8c, 3, 4, Abs, false, false),
    instruction!("STA", 0x8d, 3, 4, Abs, false, false),
    instruction!("ERR", 0x8e, 0, 0, None, false, false),
    instruction!("ERR", 0x8f, 0, 0, None, false, false),
    instruction!("ERR", 0x90, 0, 0, None, false, false),
    instruction!("STA", 0x91, 2, 6, IndIdx, false, false),
    instruction!("ERR", 0x92, 0, 0, None, false, false),
    instruction!("ERR", 0x93, 0, 0, None, false, false),
    instruction!("STY", 0x94, 2, 4, IdxZPX, false, false),
    instruction!("STA", 0x95, 2, 4, IdxZPX, false, false),
    instruction!("ERR", 0x96, 0, 0, None, false, false),
    instruction!("ERR", 0x97, 0, 0, None, false, false),
    instruction!("TYA", 0x98, 1, 2, None, false, true),
    instruction!("STA", 0x99, 3, 5, IdxAbsY, false, false),
    instruction!("TXS", 0x9a, 1, 2, None, false, false),
    instruction!("ERR", 0x9b, 0, 0, None, false, false),
    instruction!("ERR", 0x9c, 0, 0, None, false, false),
    instruction!("STA", 0x9d, 3, 5, IdxAbsX, false, false),
    instruction!("ERR", 0x9e, 0, 0, None, false, false),
    instruction!("ERR", 0x9f, 0, 0, None, false, false),
    instruction!("LDY", 0xa0, 2, 2, Imm, false, true),
    instruction!("LDA", 0xa1, 2, 6, IdxInd, false, true),
    instruction!("LDX", 0xa2, 2, 2, Imm, false, true),
    instruction!("ERR", 0xa3, 0, 0, None, false, false),
    instruction!("LDY", 0xa4, 2, 3, ZP, false, true),
    instruction!("LDA", 0xa5, 2, 3, ZP, false, true),
    instruction!("LDX", 0xa6, 2, 3, ZP, false, true),
    instruction!("ERR", 0xa7, 0, 0, None, false, false),
    instruction!("TAY", 0xa8, 1, 2, None, false, true),
    instruction!("LDA", 0xa9, 2, 2, Imm, false, true),
    instruction!("TAX", 0xaa, 1, 2, None, false, true),
    instruction!("ERR", 0xab, 0, 0, None, false, false),
    instruction!("LDY", 0xac, 3, 4, Abs, false, true),
    instruction!("LDA", 0xad, 3, 4, Abs, false, true),
    instruction!("LDX", 0xae, 3, 4, Abs, false, true),
    instruction!("ERR", 0xaf, 0, 0, None, false, false),
    instruction!("ERR", 0xb0, 0, 0, None, false, false),
    instruction!("LDA", 0xb1, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0xb2, 0, 0, None, false, false),
    instruction!("ERR", 0xb3, 0, 0, None, false, false),
    instruction!("LDY", 0xb4, 2, 4, IdxZPX, false, true),
    instruction!("LDA", 0xb5, 2, 4, IdxZPX, false, true),
    instruction!("LDX", 0xb6, 2, 4, IdxZPY, false, true),
    instruction!("ERR", 0xb7, 0, 0, None, false, false),
    instruction!("ERR", 0xb8, 0, 0, None, false, false),
    instruction!("LDA", 0xb9, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0xba, 0, 0, None, false, false),
    instruction!("ERR", 0xbb, 0, 0, None, false, false),
    instruction!("LDY", 0xbc, 3, 4, IdxAbsX, true, true),
    instruction!("LDA", 0xbd, 3, 4, IdxAbsX, true, true),
    instruction!("LDX", 0xbe, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0xbf, 0, 0, None, false, false),
    instruction!("ERR", 0xc0, 0, 0, None, false, false),
    instruction!("CMP", 0xc1, 2, 6, IdxInd, false, false),
    instruction!("ERR", 0xc2, 0, 0, None, false, false),
    instruction!("ERR", 0xc3, 0, 0, None, false, false),
    instruction!("ERR", 0xc4, 0, 0, None, false, false),
    instruction!("CMP", 0xc5, 2, 3, ZP, false, false),
    instruction!("DEC", 0xc6, 2, 5, ZP, false, true),
    instruction!("ERR", 0xc7, 0, 0, None, false, false),
    instruction!("INY", 0xc8, 1, 2, None, false, true),
    instruction!("CMP", 0xc9, 2, 2, Imm, false, false),
    instruction!("DEX", 0xca, 1, 2, None, false, true),
    instruction!("ERR", 0xcb, 0, 0, None, false, false),
    instruction!("ERR", 0xcc, 0, 0, None, false, false),
    instruction!("CMP", 0xcd, 3, 4, Abs, false, false),
    instruction!("DEC", 0xce, 3, 6, Abs, false, true),
    instruction!("ERR", 0xcf, 0, 0, None, false, false),
    instruction!("BNE", 0xd0, 2, 2, Rel, false, false),
    instruction!("CMP", 0xd1, 2, 5, IndIdx, true, false),
    instruction!("ERR", 0xd2, 0, 0, None, false, false),
    instruction!("ERR", 0xd3, 0, 0, None, false, false),
    instruction!("ERR", 0xd4, 0, 0, None, false, false),
    instruction!("CMP", 0xd5, 2, 4, IdxZPX, false, false),
    instruction!("DEC", 0xd6, 2, 6, IdxZPX, false, true),
    instruction!("ERR", 0xd7, 0, 0, None, false, false),
    instruction!("CLD", 0xd8, 1, 2, None, false, false),
    instruction!("CMP", 0xd9, 3, 4, IdxAbsY, true, false),
    instruction!("ERR", 0xda, 0, 0, None, false, false),
    instruction!("ERR", 0xdb, 0, 0, None, false, false),
    instruction!("ERR", 0xdc, 0, 0, None, false, false),
    instruction!("CMP", 0xdd, 3, 4, IdxAbsX, true, false),
    instruction!("DEC", 0xde, 3, 7, IdxAbsX, false, true),
    instruction!("ERR", 0xdf, 0, 0, None, false, false),
    instruction!("ERR", 0xe0, 0, 0, None, false, false),
    instruction!("SBC", 0xe1, 2, 6, IdxInd, false, true),
    instruction!("ERR", 0xe2, 0, 0, None, false, false),
    instruction!("ERR", 0xe3, 0, 0, None, false, false),
    instruction!("ERR", 0xe4, 0, 0, None, false, false),
    instruction!("SBC", 0xe5, 2, 3, ZP, false, true),
    instruction!("ERR", 0xe6, 0, 0, None, false, false),
    instruction!("ERR", 0xe7, 0, 0, None, false, false),
    instruction!("ERR", 0xe8, 0, 0, None, false, false),
    instruction!("SBC", 0xe9, 2, 2, Imm, false, true),
    instruction!("ERR", 0xea, 0, 0, None, false, false),
    instruction!("ERR", 0xeb, 0, 0, None, false, false),
    instruction!("ERR", 0xec, 0, 0, None, false, false),
    instruction!("SBC", 0xed, 3, 4, Abs, false, true),
    instruction!("ERR", 0xee, 0, 0, None, false, false),
    instruction!("ERR", 0xef, 0, 0, None, false, false),
    instruction!("BEQ", 0xf0, 2, 2, Rel, false, false),
    instruction!("SBC", 0xf1, 2, 5, IndIdx, true, true),
    instruction!("ERR", 0xf2, 0, 0, None, false, false),
    instruction!("ERR", 0xf3, 0, 0, None, false, false),
    instruction!("ERR", 0xf4, 0, 0, None, false, false),
    instruction!("SBC", 0xf5, 2, 4, IdxZPX, false, true),
    instruction!("ERR", 0xf6, 0, 0, None, false, false),
    instruction!("ERR", 0xf7, 0, 0, None, false, false),
    instruction!("ERR", 0xf8, 0, 0, None, false, false),
    instruction!("SBC", 0xf9, 3, 4, IdxAbsY, true, true),
    instruction!("ERR", 0xfa, 0, 0, None, false, false),
    instruction!("ERR", 0xfb, 0, 0, None, false, false),
    instruction!("ERR", 0xfc, 0, 0, None, false, false),
    instruction!("SBC", 0xfd, 3, 4, IdxAbsX, true, true),
    instruction!("ERR", 0xfe, 0, 0, None, false, false),
    instruction!("ERR", 0xff, 0, 0, None, false, false),
];
