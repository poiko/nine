use super::super::nes;
use super::ppu;

const RAM_SIZE: usize = 0x10000;

const INT_NMI_POS: usize = 0xfffa;
const INT_RESET_POS: usize = 0xfffc;
const INT_IRQ_POS: usize = 0xfffe;

enum OpCode {
    SEI
}

//impl From 

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

    fn read_byte(&mut self, addr: usize) -> u8 {
        if addr < 0x2000 {
            self.ram[addr]
        } else if addr < 0x4000 {
            self.ppu.read_io_reg(addr & 0b111)
        } else {
            panic!("Unhandled CPU memory read at {:x}", addr);
        }
    }

    fn write_byte(&mut self, addr: usize, val: u8) {
        if addr < 0x2000 {
            self.ram[addr] = val;
        } else if addr < 0x4000 {
            self.ppu.write_io_reg(addr & 0b111, val);
        } else {
            panic!("Unhandled CPU memory write at {:x}", addr);
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
    }

    pub fn run_step(&mut self) {
        self.run_instruction();
        self.ppu.run_step();
    }

    pub fn run_instruction(&mut self) {
        //println!("Program counter: {:x}", self.pc_reg);
        let pc = self.pc_reg as usize;
        let opcode = self.ram[pc];
        match opcode {
            0x10 => {
                // BPL
                self.pc_reg += 2;
                if !self.negative {
                    let rel_addr = self.ram[pc+1] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    println!("BPL branch {}", rel_addr);
                } else {
                    println!("BPL no branch");
                }
            }
            0x18 => {
                // CLC
                self.carry = false;
                self.pc_reg += 1;
                println!("CLC");
            }
            0x20 => {
                // JSR
                let ret_addr = self.pc_reg + 2;
                self.push_byte((ret_addr >> 8) as u8);
                self.push_byte((ret_addr & 0xff) as u8);
                self.pc_reg = self.read_word(pc+1);
                println!("JSR to {:x}", self.pc_reg);
            }
            0x25 => {
                // AND zero page
                let addr = self.ram[pc+1] as usize;
                self.acc_reg &= self.ram[addr];
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 2;
                println!("AND zp {:x}, result {:x}", self.ram[addr], self.acc_reg);
            }
            0x29 => {
                // AND immediate
                self.acc_reg &= self.ram[pc+1];
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 2;
                println!("AND imm {:x}, result {:x}", self.ram[pc+1], self.acc_reg);
            }
            0x48 => {
                // PHA
                let val = self.acc_reg;
                self.push_byte(val);
                self.pc_reg += 1;
                println!("PHA");
            }
            0x4a => {
                // LSR acc
                self.carry = (self.acc_reg & 1) != 0;
                self.acc_reg >>= 1;
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 1;
                println!("LSR acc, result {:x}", self.acc_reg);
            }
            0x4c => {
                // JMP
                self.pc_reg = self.read_word(pc+1);
                println!("JMP to {:x}", self.pc_reg);
            }
            0x60 => {
                // RTS
                let ret_addr_lo = self.pop_byte() as u16;
                let ret_addr_hi = self.pop_byte() as u16;
                self.pc_reg = ((ret_addr_hi << 8) + ret_addr_lo).wrapping_add(1);
                println!("RTS to {:x}", self.pc_reg);
            }
            0x65 => {
                // ADC zero page
                // TODO: decimal flag
                let addr = self.ram[pc+1] as usize;
                let mut acc_int = self.acc_reg as i32 + self.ram[addr] as i32;
                self.acc_reg = self.acc_reg.wrapping_add(self.ram[addr]);
                if self.carry {
                    acc_int += 1;
                    self.acc_reg = self.acc_reg.wrapping_add(1);
                }
                if acc_int > 127 || acc_int < -128 {
                    self.overflow = true;
                } else {
                    self.overflow = false;
                }
                self.pc_reg += 2;
                println!("ADC zp at {:x}, result {:x}", addr, self.acc_reg);
            }
            0x68 => {
                // PLA
                let val = self.acc_reg;
                self.push_byte(val);
                self.pc_reg += 1;
                println!("PHA");
            }
            0x69 => {
                // ADC immediate
                // TODO: decimal flag
                let val = self.ram[pc+1];
                let mut acc_int = self.acc_reg as i32 + val as i32;
                self.acc_reg = self.acc_reg.wrapping_add(val);
                if self.carry {
                    acc_int += 1;
                    self.acc_reg = self.acc_reg.wrapping_add(1);
                }
                if acc_int > 127 || acc_int < -128 {
                    self.overflow = true;
                } else {
                    self.overflow = false;
                }
                self.pc_reg += 2;
                println!("ADC imm {:x}, result {:x}", val, self.acc_reg);
            }
            0x78 => {
                // SEI
                self.int_disable = true;
                self.pc_reg += 1;
                println!("SEI");
            }
            0x84 => {
                // STY zero page
                let addr = self.ram[pc+1] as usize;
                self.ram[addr] = self.y_reg;
                self.pc_reg += 2;
                println!("STY zp at {:x}", addr);
            }
            0x85 => {
                // STA zero page
                let addr = self.ram[pc+1] as usize;
                self.ram[addr] = self.acc_reg;
                self.pc_reg += 2;
                println!("STA zp at {:x}", addr);
            }
            0x88 => {
                // DEY
                self.y_reg = self.y_reg.wrapping_sub(1);
                self.negative = (self.y_reg & 128) != 0;
                self.zero = self.y_reg == 0;
                self.pc_reg += 1;
                println!("DEY new val {:x}", self.y_reg);
            }
            0x8a => {
                // TXA
                self.acc_reg = self.x_reg;
                self.negative = (self.x_reg & 128) != 0;
                self.zero = self.x_reg == 0;
                self.pc_reg += 1;
                println!("TXA");
            }
            0x8d => {
                // STA absolute
                let addr = self.read_word(pc+1) as usize;
                let val = self.acc_reg;
                self.write_byte(addr, val);
                self.pc_reg += 3;
                println!("STA abs at {:x}", addr);
            }
            0x91 => {
                // STA zero page indirect indexed with Y
                let zp_addr = self.ram[pc+1] as usize;
                let addr = self.read_word(zp_addr) as usize + self.y_reg as usize;
                let val = self.acc_reg;
                self.write_byte(addr, val);
                self.pc_reg += 2;
                println!("STA zp indir Y at {:x}", addr);
            }
            0x98 => {
                // TYA
                self.acc_reg = self.y_reg;
                self.negative = (self.y_reg & 128) != 0;
                self.zero = self.y_reg == 0;
                self.pc_reg += 1;
                println!("TYA");
            }
            0x9a => {
                // TXS
                self.sp_reg = self.x_reg;
                self.pc_reg += 1;
                println!("TXS val {:x}", self.sp_reg);
            }
            0xa0 => {
                // LDY immediate
                self.y_reg = self.ram[pc+1];
                self.negative = (self.y_reg & 128) != 0;
                self.zero = self.y_reg == 0;
                self.pc_reg += 2;
                println!("LDY imm {:x}", self.y_reg);
            }
            0xa2 => {
                // LDX immediate
                self.x_reg = self.ram[pc+1];
                self.negative = (self.x_reg & 128) != 0;
                self.zero = self.x_reg == 0;
                self.pc_reg += 2;
                println!("LDX imm {:x}", self.x_reg);
            }
            0xa5 => {
                // LDA zero page
                let addr = self.ram[pc+1] as usize;
                self.acc_reg = self.read_byte(addr);
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 2;
                println!("LDA zp at {:x}, val {:x}", addr, self.acc_reg);
            }
            0xa6 => {
                // LDX zero page
                let addr = self.ram[pc+1] as usize;
                self.x_reg = self.read_byte(addr);
                self.negative = (self.x_reg & 128) != 0;
                self.zero = self.x_reg == 0;
                self.pc_reg += 2;
                println!("LDX zp at {:x}, val {:x}", addr, self.x_reg);
            }
            0xa8 => {
                // TAY
                self.y_reg = self.acc_reg;
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 1;
                println!("TAY");
            }
            0xa9 => {
                // LDA immediate
                self.acc_reg = self.ram[pc+1];
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 2;
                println!("LDA imm {:x}", self.acc_reg);
            }
            0xaa => {
                // TAX
                self.x_reg = self.acc_reg;
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 1;
                println!("TAX");
            }
            0xad => {
                // LDA absolute
                let addr = self.read_word(pc+1) as usize;
                self.acc_reg = self.read_byte(addr);
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == 0;
                self.pc_reg += 3;
                println!("LDA abs at {:x}, val {:x}", addr, self.acc_reg);
            }
            0xca => {
                // DEX
                self.x_reg = self.x_reg.wrapping_sub(1);
                self.negative = (self.x_reg & 128) != 0;
                self.zero = self.x_reg == 0;
                self.pc_reg += 1;
                println!("DEX new val {:x}", self.x_reg);
            }
            0xc6 => {
                // DEC zero page
                let addr = self.ram[pc+1] as usize;
                let new_val = self.ram[addr].wrapping_sub(1);
                self.ram[addr] = new_val;
                self.negative = (new_val & 128) != 0;
                self.zero = new_val == 0;
                self.pc_reg += 2;
                println!("DEC zp new val {:x} at {:x}", new_val, addr);
            }
            0xc8 => {
                // INY
                self.y_reg = self.y_reg.wrapping_add(1);
                self.negative = (self.y_reg & 128) != 0;
                self.zero = self.y_reg == 0;
                self.pc_reg += 1;
                println!("INY new val {:x}", self.y_reg);
            }
            0xc9 => {
                // CMP immediate
                let val = self.ram[pc+1];
                self.carry = self.acc_reg >= val;
                self.negative = (self.acc_reg & 128) != 0;
                self.zero = self.acc_reg == val;
                self.pc_reg += 2;
                println!("CMP imm {:x}", val);
            }
            0xd0 => {
                // BNE
                self.pc_reg += 2;
                if !self.zero {
                    let rel_addr = self.ram[pc+1] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    println!("BNE branch {}", rel_addr);
                } else {
                    println!("BNE no branch");
                }
            }
            0xd8 => {
                // CLD
                self.bcd_mode = false;
                self.pc_reg += 1;
                println!("CLD");
            }
            0xf0 => {
                // BEQ
                self.pc_reg += 2;
                if self.zero {
                    let rel_addr = self.ram[pc+1] as i8;
                    self.pc_reg = (self.pc_reg as i32 + rel_addr as i32) as u16;
                    println!("BEQ branch {}", rel_addr);
                } else {
                    println!("BEQ no branch");
                }
            }
            _ => panic!("Unrecognized opcode {:x}", opcode)
                
        }
    }
}

