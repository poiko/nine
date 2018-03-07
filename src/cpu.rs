use super::nes;
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

//macro_rules! update_nz {
//}

impl CPU {
    pub fn new() -> CPU {
        CPU { ram: vec![0; RAM_SIZE], ..Default::default() }
    }

    fn read_word(&self, addr: usize) -> u16 {
        self.ram[addr] as u16 | ((self.ram[addr+1] as u16) << 8)
    }

    //fn write_word

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

    pub fn run_instruction(&mut self, ppu: &ppu::PPU) {
        println!("program counter: {:x}", self.pc_reg);
        let pc = self.pc_reg as usize;
        let opcode = self.ram[pc];
        match opcode {
            0x29 => {
                // AND immediate
                self.acc_reg &= self.ram[pc+1];
                self.zero = self.acc_reg != 0;
                self.negative = (self.acc_reg & 128) != 0;
                self.pc_reg += 2;
                println!("AND imm {:x}", self.ram[pc+1]);
            }
            0x78 => {
                // SEI
                self.int_disable = true;
                self.pc_reg += 1;
                println!("SEI");
            }
            0x8d => {
                // STA absolute
                let addr = self.read_word(pc+1) as usize;
                self.ram[addr] = self.acc_reg;
                self.pc_reg += 3;
                println!("STA abs at {:x}", addr);
            }
            0x9a => {
                // TXS
                self.sp_reg = self.x_reg;
                self.pc_reg += 1;
                println!("TXS");
            }
            0xa2 => {
                // LDX immediate
                self.x_reg = self.ram[pc+1];
                self.zero = self.x_reg != 0;
                self.negative = (self.x_reg & 128) != 0;
                self.pc_reg += 2;
                println!("LDX imm {:x}", self.x_reg);
            }
            0xa9 => {
                // LDA immediate
                self.acc_reg = self.ram[pc+1];
                self.pc_reg += 2;
                println!("LDA imm {:x}", self.acc_reg);
            }
            0xad => {
                // LDA absolute
                let addr = self.read_word(pc+1) as usize;
                self.acc_reg = self.ram[addr];
                self.pc_reg += 3;
                println!("LDA abs at {:x}", addr);
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

