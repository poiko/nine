use std::env;
use std::fs;
use std::io::Read;
use std::path::Path;

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

fn load_file<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = fs::File::open(&path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    file_buf
}

fn main() {
    let file_buf = load_file("roms/zelda.nes");
}
