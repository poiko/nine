extern crate byteorder;

mod nes;

fn main() {
    let mut nes = nes::NES::new();
    let file_name = "roms/donkey.nes";
    println!("Loading ROM file {}:", file_name);
    match nes::load_nes_file(file_name) {
        Ok(rom) => {
            println!("    number of PRGROM banks: {}", rom.num_prg_banks);
            println!("    number of CHRROM banks: {}", rom.num_chr_banks);
            println!("    number of RAM banks: {}", rom.num_ram_banks);
            println!("    mapper id: {}", rom.mapper_id);
            nes.run(&rom);
        },
        Err(err) => println!("Error: {}", err)
    }
}
