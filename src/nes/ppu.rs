const RAM_SIZE: usize = 65536;
const SPR_RAM_SIZE: usize = 256;
const NAME_TABLE_ADDRS: [usize; 4] = [0x2000, 0x2400, 0x2800, 0x2c00];

#[derive(Default)]
pub struct PPU {
    io_regs: [u8; 8],

    // Control register 1
    name_table: usize,
    addr_incr: u32,
    pattern_table: usize,
    background_table: usize,
    sprite_size: u32,
    vblank_nmi: bool,

    // Control register 2
    monochrome: bool,
    background_clip: bool,
    sprite_clip: bool,
    background: bool,
    sprites: bool,
    background_col: u32,

    // SPR-RAM address register
    spr_ram_addr: usize,

    // VRAM address register 1 and 2
    vram_addr1: u8,
    vram_addr2: u8,
    
    ram: Vec<u8>,
    spr_ram: Vec<u8>
}

impl PPU {
    pub fn new() -> PPU {
        PPU {
            ram: vec![0; RAM_SIZE],
            spr_ram: vec![0; SPR_RAM_SIZE],
            ..Default::default()
        }
    }

    pub fn read_io_reg(&mut self, reg: usize) -> u8 {
        if reg == 2 {
            self.vram_addr1 = 0;
            self.vram_addr2 = 0;
            self.io_regs[reg]            
        } else if reg == 7 {
            self.io_regs[reg]
        } else {
            panic!("Attempt to read write-only I/O register {}", reg);
        }
    }

    pub fn write_io_reg(&mut self, reg: usize, val: u8) {
        if reg != 2 {
            match reg {
                0 => {
                    // Control reg 1
                    self.name_table = NAME_TABLE_ADDRS[(val & 0b11) as usize];
                    self.addr_incr = if (val & 0b100) != 0 { 32 } else { 1 };
                    self.pattern_table = if (val & 0b1000) != 0 { 0x1000 } else { 0 };
                    self.background_table = if (val & 0b10000) != 0 { 0x1000 } else { 0 };
                    self.sprite_size = if (val & 0b100000) != 0 { 16 } else { 8 };
                    self.vblank_nmi = (val & 0b10000000) != 0;
                }
                1 => {
                    // Control reg 2
                    self.monochrome = (val & 1) != 0;
                    self.background_clip = (val & 0b10) == 0;
                    self.sprite_clip = (val & 0b100) == 0;
                    self.background = (val & 0b1000) != 0;
                    self.sprites = (val & 0b10000) != 0;
                    self.background_col = (val >> 5) as u32;
                }
                5 => {
                    // VRAM addr reg 1
                    self.vram_addr1 = val;
                }
                6 => {
                    // VRAM addr reg 2
                    self.vram_addr2 = val;
                }
                _ => { panic!("Unhandled I/O register {} write", reg); }
            }
            self.io_regs[reg] = val;
        } else {
            panic!("Attempt to write to read-only I/O register {}", reg);
        }
    }

    pub fn run_step(&mut self) {
        self.io_regs[2] = 0x80;
    }
}
