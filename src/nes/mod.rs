use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::fmt;

mod cpu;
mod ppu;

#[derive(Debug)]
pub enum NESError {
    Io(io::Error)
}

impl From<io::Error> for NESError {
    fn from(err: io::Error) -> NESError {
        NESError::Io(err)
    }
}

impl fmt::Display for NESError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NESError::Io(ref err) => err.fmt(f)
        }
    }
}


#[derive(Default)]
pub struct ROM {
    pub num_prg_banks: u32,
    pub num_chr_banks: u32,
    pub num_ram_banks: u32,
    pub mapper_id: u32,

    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>
}

pub const PRG_BANK_SIZE: usize = 0x4000;
pub const CHR_BANK_SIZE: usize = 0x2000;
const HEADER_SIZE: usize = 16;
const TRAINER_SIZE: usize = 512;

pub fn load_nes_file<P: AsRef<Path>>(path: P) -> Result<ROM, NESError> {
    let mut file = try!(File::open(&path));
    let mut file_buf = Vec::new();
    try!(file.read_to_end(&mut file_buf));

    assert_eq!(&file_buf[..4], &[0x4e, 0x45, 0x53, 0x1a], "Not a valid NES file.");

    let mut rom = ROM::default();
    rom.num_prg_banks = file_buf[4] as u32;
    rom.num_chr_banks = file_buf[5] as u32;
    // TODO: handle the remaining bits
    let ctrl1 = file_buf[6];
    let ctrl2 = file_buf[7];
    rom.mapper_id = ((ctrl2 & 0xf0) | (ctrl1 >> 4)) as u32;
    rom.num_ram_banks = file_buf[8] as u32;

    let prg_rom_start = HEADER_SIZE;
    let prg_rom_end = prg_rom_start + rom.num_prg_banks as usize * PRG_BANK_SIZE;
    rom.prg_rom = file_buf[prg_rom_start..prg_rom_end].to_vec();

    Ok(rom)
}

#[derive(Default)]
pub struct NES {
    cpu: cpu::CPU,
}

impl NES {
    pub fn new() -> NES {
        NES {
            cpu: cpu::CPU::new(),
        }
    }

    pub fn run(&mut self, rom: &ROM) {
        self.cpu.load_rom(rom);
        self.cpu.reset();
        
        loop {
            self.cpu.run_step();
        }
    }
}
