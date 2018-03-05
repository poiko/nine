use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::fmt;

#[derive(Debug)]
enum NineError {
    Io(io::Error)
}

impl From<io::Error> for NineError {
    fn from(err: io::Error) -> NineError {
        NineError::Io(err)
    }
}

impl fmt::Display for NineError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NineError::Io(ref err) => err.fmt(f)
        }
    }
}

struct CPU {
    pc_reg: u16,
    sp_reg: u8,
    acc_reg: u8,
    x_reg: u8,
    y_reg: u8,
    status_reg: u8
}

impl CPU {
    fn new() -> CPU {
        CPU {
            pc_reg: 0,
            sp_reg: 0,
            acc_reg: 0,
            x_reg: 0,
            y_reg: 0,
            status_reg: 0
        }
    }

    fn run() {
    }
}

struct PPU {
    blah: u8
}

impl PPU {
    fn new() -> PPU {
        PPU {
            blah: 0
        }
    }
}

struct NES {
    cpu: CPU,
    ppu: PPU
}

impl NES {
    fn new() -> NES {
        NES {
            cpu: CPU::new(),
            ppu: PPU::new()
        }
    }
    
    fn run(&self, program: ROM) {
    }
}

const PRG_BANK_SIZE: u32 = 16384;
const CHR_BANK_SIZE: u32 = 8192;

#[derive(Default)]
struct ROM {
    num_prg_banks: u32,
    num_chr_banks: u32,
    num_ram_banks: u32,
    mapper_id: u32,

    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>
}

impl ROM {
    fn new() -> ROM {
        ROM::default()
    }
}

fn load_nes_file<P: AsRef<Path>>(path: P) -> Result<ROM, NineError> {
    let mut file = try!(File::open(&path));
    let mut file_buf = Vec::new();
    try!(file.read_to_end(&mut file_buf));

    assert_eq!(&file_buf[..4], &[0x4e, 0x45, 0x53, 0x1a], "Not a valid NES file.");

    let mut rom = ROM::new();
    rom.num_prg_banks = file_buf[4] as u32;
    rom.num_chr_banks = file_buf[5] as u32;
    // TODO: handle this
    let ctrl1 = file_buf[6];
    let ctrl2 = file_buf[7];
    rom.num_ram_banks = file_buf[8] as u32;

    let prg_rom_size = (rom.num_prg_banks * PRG_BANK_SIZE) as usize;
    rom.prg_rom = file_buf[272..prg_rom_size].to_vec();

    Ok(rom)
}

fn main() {
    let nes = NES::new();
    let file_name = "roms/zelda.nes";
    println!("Loading ROM file {}:", file_name);
    match load_nes_file(file_name) {
        Ok(rom) => {
            println!("    number of PRGROM banks: {}", rom.num_prg_banks);
            println!("    number of CHRROM banks: {}", rom.num_chr_banks);
            println!("    number of RAM banks: {}", rom.num_ram_banks);
            println!("    mapper id: {}", rom.mapper_id);
            nes.run(rom);
        },
        Err(err) => println!("Error: {}", err)
    }
}
