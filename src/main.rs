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
    
    fn run(&self, program: Vec<u8>) {
    }
}

struct ROM {
    
}

fn load_nes_file<P: AsRef<Path>>(path: P) -> Result<Vec<u8>, NineError> {
    let mut file = try!(File::open(&path));
    let mut file_buf = Vec::new();
    try!(file.read_to_end(&mut file_buf));
    Ok(file_buf)
}

fn main() {
    let nes = NES::new();
    match load_nes_file("roms/zelda.nes") {
        Ok(rom) => nes.run(rom),
        Err(err) => println!("Error: {}", err)
    }
}
