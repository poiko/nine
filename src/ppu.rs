const RAM_SIZE: usize = 65536;

#[derive(Default)]
pub struct PPU {
    ram: Vec<u8>
}

impl PPU {
    pub fn new() -> PPU {
        PPU { ram: vec![0; RAM_SIZE], ..Default::default() }
    }
}
