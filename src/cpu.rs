#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    Implied,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Accumulator,
    Relative,
    NoneAddressing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mnemonic {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    SBC,
    LDA,
    TAX,
    INX,
    STA,
    BRK,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpCode {
    pub code: u8,
    pub mnemonic: Mnemonic,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, mnemonic: Mnemonic, len: u8, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            mnemonic,
            len,
            cycles,
            mode,
        }
    }
}

pub fn get_opcodes() -> Vec<OpCode> {
    vec![
        // ADC
        OpCode::new(0x69, Mnemonic::ADC, 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, Mnemonic::ADC, 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, Mnemonic::ADC, 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x6D, Mnemonic::ADC, 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7D, Mnemonic::ADC, 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x79, Mnemonic::ADC, 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x61, Mnemonic::ADC, 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x71, Mnemonic::ADC, 2, 5, AddressingMode::Indirect_Y),
        // AND
        OpCode::new(0x29, Mnemonic::AND, 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, Mnemonic::AND, 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, Mnemonic::AND, 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2D, Mnemonic::AND, 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3D, Mnemonic::AND, 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x39, Mnemonic::AND, 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x21, Mnemonic::AND, 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x31, Mnemonic::AND, 2, 5, AddressingMode::Indirect_Y),
        // ASL
        OpCode::new(0x0A, Mnemonic::ASL, 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x06, Mnemonic::ASL, 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, Mnemonic::ASL, 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0E, Mnemonic::ASL, 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1E, Mnemonic::ASL, 3, 7, AddressingMode::Absolute_X),
        // BCC
        OpCode::new(0x90, Mnemonic::BCC, 2, 2, AddressingMode::Relative),
        // BCS
        OpCode::new(0xB0, Mnemonic::BCS, 2, 2, AddressingMode::Relative),
        // SBC
        OpCode::new(0xE9, Mnemonic::SBC, 2, 2, AddressingMode::Immediate),
        OpCode::new(0xE5, Mnemonic::SBC, 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xF5, Mnemonic::SBC, 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xED, Mnemonic::SBC, 3, 4, AddressingMode::Absolute),
        OpCode::new(0xFD, Mnemonic::SBC, 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0xF9, Mnemonic::SBC, 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xE1, Mnemonic::SBC, 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xF1, Mnemonic::SBC, 2, 5, AddressingMode::Indirect_Y),
        // LDA
        OpCode::new(0xA9, Mnemonic::LDA, 2, 2, AddressingMode::Immediate),
        OpCode::new(0xA5, Mnemonic::LDA, 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xAD, Mnemonic::LDA, 3, 4, AddressingMode::Absolute),
        // TAX
        OpCode::new(0xAA, Mnemonic::TAX, 1, 2, AddressingMode::Implied),
        // BRK
        OpCode::new(0x00, Mnemonic::BRK, 1, 7, AddressingMode::Implied),
        // INX
        OpCode::new(0xE8, Mnemonic::INX, 1, 2, AddressingMode::Implied),
        // STA
        OpCode::new(0x85, Mnemonic::STA, 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, Mnemonic::STA, 2, 4, AddressingMode::ZeroPage_X),
    ]
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub instructions: [Option<OpCode>; 256],
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        let mut instructions = [None; 256];
        for op in get_opcodes() {
            instructions[op.code as usize] = Some(op);
        }
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            instructions: instructions,
            memory: [0; 65535],
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::Implied => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base: u8 = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                u16::from_le_bytes([
                    self.mem_read(ptr as u16),
                    self.mem_read(ptr.wrapping_add(1) as u16),
                ])
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let deref_base = u16::from_le_bytes([
                    self.mem_read(base as u16),
                    self.mem_read((base as u8).wrapping_add(1) as u16),
                ]);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::Relative => self.program_counter,
            AddressingMode::NoneAddressing | AddressingMode::Accumulator => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        u16::from_le_bytes([self.mem_read(pos), self.mem_read(pos + 1)])
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let bytes = data.to_le_bytes();
        self.mem_write(pos, bytes[0]);
        self.mem_write(pos + 1, bytes[1]);
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.add_with_carry(value);
    }

    fn add_with_carry(&mut self, value: u8) {
        let a = self.register_a;
        let c = if self.status & 0b0000_0001 != 0 { 1 } else { 0 };

        let sum = (a as u16) + (value as u16) + (c as u16);

        if sum > 0xFF {
            self.status |= 0b0000_0001;
        } else {
            self.status &= !0b0000_0001;
        }

        self.register_a = sum as u8;

        if ((a ^ self.register_a) & (value ^ self.register_a) & 0x80) != 0 {
            self.status = self.status | 0b0100_0000;
        } else {
            self.status = self.status | 0b0000_0000;
        }
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        if *mode == AddressingMode::Accumulator {
            let c = if self.register_a & 0x80 == 0x80 { 1 } else { 0 };

            self.register_a = self.register_a << 1;
            self.status |= c;
            self.update_zero_and_negative_flags(self.register_a);
        } else {
            let addr = self.get_operand_address(mode);
            let value = self.mem_read(addr);
            let c = if value & 0x80 == 0x80 { 1 } else { 0 };
            let result = value << 1;

            self.mem_write(addr, result);
            self.status |= c;
            self.update_zero_and_negative_flags(result)
        }
    }

    fn bcc(&mut self, mode: &AddressingMode) {
        if self.status & 0b0000_0001 != 0 {
            return;
        }

        let addr = self.get_operand_address(mode);
        let value = self.mem_read_u16(addr);
        self.program_counter += value;
    }

    fn bcs(&mut self, mode: &AddressingMode) {
        if self.status & 0b0000_0001 == 0 {
            return;
        }

        let addr = self.get_operand_address(mode);
        let value = self.mem_read_u16(addr);
        self.program_counter += value;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = !self.mem_read(addr);

        self.add_with_carry(value);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inx(&mut self) {
        if self.register_x == 0xff {
            self.register_x = 0;
        } else {
            self.register_x += 1;
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.update_zero_flag(result);
        self.update_negative_flag(result);
    }

    fn update_zero_flag(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }
    }

    fn update_negative_flag(&mut self, result: u8) {
        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    pub fn run(&mut self) {
        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;

            let opcode = match self.instructions[code as usize] {
                Some(op) => op,
                None => {
                    todo!();
                }
            };

            match opcode.mnemonic {
                Mnemonic::ADC => self.adc(&opcode.mode),
                Mnemonic::AND => self.and(&opcode.mode),
                Mnemonic::ASL => self.asl(&opcode.mode),
                Mnemonic::BCC => self.bcc(&opcode.mode),
                Mnemonic::SBC => self.sbc(&opcode.mode),
                Mnemonic::BCS => self.bcs(&opcode.mode),
                Mnemonic::LDA => self.lda(&opcode.mode),
                Mnemonic::TAX => self.tax(),
                Mnemonic::INX => self.inx(),
                Mnemonic::STA => self.sta(&opcode.mode),
                Mnemonic::BRK => return,
            }

            self.program_counter += (opcode.len - 1) as u16;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_txa_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();

        cpu.register_a = 10;
        cpu.run();

        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0xe8, 0x00]);
        cpu.reset();

        cpu.register_x = 0xff;
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }
    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55)
    }

    #[test]
    fn test_adc_immediate_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC #$05 (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x05]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_immediate_pos_pos_overflow() {
        // Test: 127 + 1 = 128 (-128 in signed) -> Overflow!
        // Opcode: ADC #$01 (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x01]);
        cpu.reset();

        cpu.register_a = 0x7f;
        cpu.run();

        assert_eq!(cpu.register_a, 0x80);
        // Flags: N=1, V=1, Z=0, C=0
        assert_eq!(cpu.status, 0b1100_0000);
    }

    #[test]
    fn test_adc_immediate_nega_nega() {
        // Test: -1 + -2 = -3 (0xFD) -> Carry occurs (unsigned 255+254 > 255)
        // Opcode: ADC #$FE (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0xFE]);
        cpu.reset();

        cpu.register_a = 0xFF;
        cpu.run();

        assert_eq!(cpu.register_a, 0xFD);
        // Flags: N=1, V=0, Z=0, C=1
        assert_eq!(cpu.status, 0b1000_0001);
    }

    #[test]
    fn test_adc_immediate_nega_nega_overflow() {
        // Test: -128 + -128 = -256 (0x00) -> Overflow!
        // Opcode: ADC #$80 (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x80]);
        cpu.reset();

        cpu.register_a = 0x80;
        cpu.run();

        assert_eq!(cpu.register_a, 0x00);
        // Flags: N=0, V=1, Z=1, C=1
        assert_eq!(cpu.status, 0b0100_0011);
    }

    #[test]
    fn test_adc_immediate_pos_nega_result_pos() {
        // Test: 5 + -1 = 4 (0x04) -> Carry occurs
        // Opcode: ADC #$FF (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0xFF]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
        // Flags: N=0, V=0, Z=0, C=1
        assert_eq!(cpu.status, 0b0000_0001);
    }

    #[test]
    fn test_adc_immediate_pos_nega_result_zero() {
        // Test: 1 + -1 = 0 (0x00) -> Carry occurs
        // Opcode: ADC #$FF (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0xFF]);
        cpu.reset();

        cpu.register_a = 0x01;
        cpu.run();

        assert_eq!(cpu.register_a, 0x00);
        // Flags: N=0, V=0, Z=1, C=1
        assert_eq!(cpu.status, 0b0000_0011);
    }

    #[test]
    fn test_adc_immediate_pos_nega_result_nega() {
        // Test: 1 + -2 = -1 (0xFF) -> No Carry
        // Opcode: ADC #$FE (Immediate)
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0xFE]);
        cpu.reset();

        cpu.register_a = 0x01;
        cpu.run();

        assert_eq!(cpu.register_a, 0xFF);
        // Flags: N=1, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_adc_zeropage_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC $10 (ZeroPage)
        let mut cpu = CPU::new();
        cpu.load(vec![0x65, 0x10]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.mem_write(0x10, 0x05); // Data at $10
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_zeropage_x_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC $10,X (ZeroPage, X)
        // Address: $10 + X($05) = $15
        let mut cpu = CPU::new();
        cpu.load(vec![0x75, 0x10]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.register_x = 0x05;
        cpu.mem_write(0x15, 0x05); // Data at $15
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_absolute_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC $1234 (Absolute)
        // Note: Little Endian means $34 then $12
        let mut cpu = CPU::new();
        cpu.load(vec![0x6D, 0x34, 0x12]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.mem_write(0x1234, 0x0005); // Data at $1234
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_absolute_x_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC $1000,X (Absolute, X)
        // Address: $1000 + X($05) = $1005
        let mut cpu = CPU::new();
        cpu.load(vec![0x7D, 0x00, 0x10]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.register_x = 0x05;
        cpu.mem_write(0x1005, 0x0005); // Data at $1005
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_absolute_y_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC $1000,Y (Absolute, Y)
        // Address: $1000 + Y($05) = $1005
        let mut cpu = CPU::new();
        cpu.load(vec![0x79, 0x00, 0x10]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.register_y = 0x05;
        cpu.mem_write(0x1005, 0x0005); // Data at $1005
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_indirect_x_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC ($20,X) (Indirect, X)
        // Pointer Addr: $20 + X($04) = $24
        // Pointer Value: $0074 (from $24 & $25)
        // Target Addr: $0074
        let mut cpu = CPU::new();
        cpu.load(vec![0x61, 0x20]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.register_x = 0x04;

        cpu.mem_write(0x24, 0x74); // Low byte of ptr
        cpu.mem_write(0x25, 0x00); // High byte of ptr
        cpu.mem_write(0x74, 0x05); // Data at target
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_adc_indirect_y_pos_pos() {
        // Test: 5 + 5 = 10
        // Opcode: ADC ($20),Y (Indirect, Y)
        // Base Addr: $1000 (from $20 & $21)
        // Target Addr: $1000 + Y($05) = $1005
        let mut cpu = CPU::new();
        cpu.load(vec![0x71, 0x20]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.register_y = 0x05;

        cpu.mem_write(0x20, 0x00); // Low byte of base
        cpu.mem_write(0x21, 0x10); // High byte of base
        cpu.mem_write(0x1005, 0x05); // Data at target
        cpu.run();

        assert_eq!(cpu.register_a, 0x0A);
        // Flags: N=0, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_and_immediate_no_flags() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0b0010_1101]);
        cpu.reset();

        cpu.register_a = 0b0001_1111;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0000_1101);
        // Flags: N=0, Z=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_and_immediate_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0b0010_1101]);
        cpu.reset();

        cpu.register_a = 0b0000_0010;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0000_0000);
        // Flags: N=0, Z=1
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn test_and_immediate_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0b1010_1101]);
        cpu.reset();

        cpu.register_a = 0b1000_0010;
        cpu.run();

        assert_eq!(cpu.register_a, 0b1000_0000);
        // Flags: N=1, Z=0
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_asl_accumulator_no_flags() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0A]);
        cpu.reset();

        cpu.register_a = 0b0000_0010;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0000_0100);
        // Flags: N=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_asl_accumulator_carry_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0A]);
        cpu.reset();

        cpu.register_a = 0b1000_0001;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0000_0010);
        // Flags: N=0, Z=0, C=1
        assert_eq!(cpu.status, 0b0000_0001);
    }

    #[test]
    fn test_asl_accumulator_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0A]);
        cpu.reset();

        cpu.register_a = 0b0100_0000;
        cpu.run();

        assert_eq!(cpu.register_a, 0b1000_0000);
        // Flags: N=1, Z=0, C=0
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_asl_zeropage_no_flags() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x06, 0x05]);
        cpu.reset();

        cpu.mem_write(0x05, 0x06);
        cpu.run();

        let result = cpu.mem_read(0x05);

        assert_eq!(result, 0b0000_1100);
        // Flags: N=0, Z=0, C=0
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[test]
    fn test_bcc_relative_success() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x90, 0x05]);
        cpu.reset();

        cpu.mem_write_u16(0x05, 0x06);
        let before = cpu.program_counter;
        cpu.run();

        assert_eq!(cpu.program_counter, before + 0x08);
    }

    #[test]
    fn test_bcc_relative_fail() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x90, 0x05]);
        cpu.reset();

        cpu.mem_write_u16(0x05, 0x06);
        let before = cpu.program_counter;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.program_counter, before + 3);
    }

    #[test]
    fn test_bcs_relative_success() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xB0, 0x05]);
        cpu.reset();

        cpu.mem_write_u16(0x05, 0x06);
        let before = cpu.program_counter;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.program_counter, before + 0x08);
    }

    #[test]
    fn test_bcs_relative_fail() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xB0, 0x05]);
        cpu.reset();

        cpu.mem_write_u16(0x05, 0x06);
        let before = cpu.program_counter;
        cpu.run();

        assert_eq!(cpu.program_counter, before + 3);
    }

    #[test]
    fn test_sbc_immediate_pos_pos_no_borrow() {
        // Test: 5 - 3 = 2
        // Opcode: SBC #$03
        let mut cpu = CPU::new();
        cpu.load(vec![0xE9, 0x03]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.register_a, 0x02);
        // Flags: N=0, V=0, Z=0, C=1
        assert_eq!(cpu.status, 0b0000_0001);
    }

    #[test]
    fn test_sbc_immediate_pos_pos_borrow() {
        // Test: 3 - 5 =  -2 (0xFE)
        // Opcode: SBC #$05
        let mut cpu = CPU::new();
        cpu.load(vec![0xE9, 0x05]);
        cpu.reset();

        cpu.register_a = 0x03;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.register_a, 0xFE);
        // Flags: N=1, V=0, Z=0, C=0
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_sbc_immediate_pos_neg_overflow() {
        // Test: 127 - (-2) = 129 (0x81) -> Overflow!
        // Opcode: SBC #$FE
        let mut cpu = CPU::new();
        cpu.load(vec![0xE9, 0xFE]);
        cpu.reset();

        cpu.register_a = 0x7F;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.register_a, 0x81);
        // Flags: N=1, V=1, Z=0, C=0
        assert_eq!(cpu.status, 0b1100_0000);
    }

    #[test]
    fn test_sbc_immediate_neg_pos_overflow() {
        // Test: -128 - 1 = -129 -> Overflow!
        // Opcode: SBC #$01
        let mut cpu = CPU::new();
        cpu.load(vec![0xE9, 0x01]);
        cpu.reset();

        cpu.register_a = 0x80;
        cpu.status = 0b0000_0001;
        cpu.run();

        assert_eq!(cpu.register_a, 0x7F);
        // Flags: N=0, V=1, Z=0, C=1
        assert_eq!(cpu.status, 0b0100_0001);
    }

    #[test]
    fn test_sbc_immediate_with_borrow() {
        // Test: 5 - 3 - 1 (borrow) = 1
        // Opcode: SBC #$03
        let mut cpu = CPU::new();
        cpu.load(vec![0xE9, 0x03]);
        cpu.reset();

        cpu.register_a = 0x05;
        cpu.status = 0b0000_0000;
        cpu.run();

        assert_eq!(cpu.register_a, 0x01);
        // Flags: N=0, V=0, Z=0, C=1
        assert_eq!(cpu.status, 0b0000_0001);
    }
}
