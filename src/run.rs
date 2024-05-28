//    | ‐0       | ‐1        | ‐2    | ‐3  | ‐4        | ‐5        | ‐6        | ‐7  | ‐8       | ‐9        | ‐A       | ‐B  | ‐C        | ‐D        | ‐E        | ‐F
// 0‐ | BRK impl | ORA X,ind | ---   | --- | ---       | ORA zpg   | ASL zpg   | --- | PHP impl | ORA #     | ASL A    | --- | ---       | ORA abs   | ASL abs   | ---
// 1‐ | BPL rel  | ORA ind,Y | ---   | --- | ---       | ORA zpg,X | ASL zpg,X | --- | CLC impl | ORA abs,Y | ---      | --- | ---       | ORA abs,X | ASL abs,X | ---
// 2‐ | JSR abs  | AND X,ind | ---   | --- | BIT zpg   | AND zpg   | ROL zpg   | --- | PLP impl | AND #     | ROL A    | --- | BIT abs   | AND abs   | ROL abs   | ---
// 3‐ | BMI rel  | AND ind,Y | ---   | --- | ---       | AND zpg,X | ROL zpg,X | --- | SEC impl | AND abs,Y | ---      | --- | ---       | AND abs,X | ROL abs,X | ---
// 4‐ | RTI impl | EOR X,ind | ---   | --- | ---       | EOR zpg   | LSR zpg   | --- | PHA impl | EOR #     | LSR A    | --- | JMP abs   | EOR abs   | LSR abs   | ---
// 5‐ | BVC rel  | EOR ind,Y | ---   | --- | ---       | EOR zpg,X | LSR zpg,X | --- | CLI impl | EOR abs,Y | ---      | --- | ---       | EOR abs,X | LSR abs,X | ---
// 6‐ | RTS impl | ADC X,ind | ---   | --- | ---       | ADC zpg   | ROR zpg   | --- | PLA impl | ADC #     | ROR A    | --- | JMP ind   | ADC abs   | ROR abs   | ---
// 7‐ | BVS rel  | ADC ind,Y | ---   | --- | ---       | ADC zpg,X | ROR zpg,X | --- | SEI impl | ADC abs,Y | ---      | --- | ---       | ADC abs,X | ROR abs,X | ---
// 8‐ | ---      | STA X,ind | ---   | --- | STY zpg   | STA zpg   | STX zpg   | --- | DEY impl | ---       | TXA impl | --- | STY abs   | STA abs   | STX abs   | ---
// 9‐ | BCC rel  | STA ind,Y | ---   | --- | STY zpg,X | STA zpg,X | STX zpg,Y | --- | TYA impl | STA abs,Y | TXS impl | --- | ---       | STA abs,X | ---       | ---
// A‐ | LDY #    | LDA X,ind | LDX # | --- | LDY zpg   | LDA zpg   | LDX zpg   | --- | TAY impl | LDA #     | TAX impl | --- | LDY abs   | LDA abs   | LDX abs   | ---
// B‐ | BCS rel  | LDA ind,Y | ---   | --- | LDY zpg,X | LDA zpg,X | LDX zpg,Y | --- | CLV impl | LDA abs,Y | TSX impl | --- | LDY abs,X | LDA abs,X | LDX abs,Y | ---
// C‐ | CPY #    | CMP X,ind | ---   | --- | CPY zpg   | CMP zpg   | DEC zpg   | --- | INY impl | CMP #     | DEX impl | --- | CPY abs   | CMP abs   | DEC abs   | ---
// D‐ | BNE rel  | CMP ind,Y | ---   | --- | ---       | CMP zpg,X | DEC zpg,X | --- | CLD impl | CMP abs,Y | ---      | --- | ---       | CMP abs,X | DEC abs,X | ---
// E‐ | CPX #    | SBC X,ind | ---   | --- | CPX zpg   | SBC zpg   | INC zpg   | --- | INX impl | SBC #     | NOP impl | --- | CPX abs   | SBC abs   | INC abs   | ---
// F‐ | BEQ rel  | SBC ind,Y | ---   | --- | ---       | SBC zpg,X | INC zpg,X | --- | SED impl | SBC abs,Y | ---      | --- | ---       | SBC abs,X | INC abs,X | ---

// Resources:
// - https://www.masswerk.at/6502/6502_instruction_set.html
// - https://www.pagetable.com/?p=410
// - https://www.pagetable.com/c64ref/6502/?tab=2
// - http://www.6502.org/tutorials/vflag.html
// - https://www.nesdev.org/wiki/Status_flags
// - https://skilldrick.github.io/easy6502/


const MEM_SIZE: usize = 0x10000;
const RESET_VECTOR_ADDR: usize = 0xFFFC;
const CODE_ADDR: usize = 0x8000;
const STACK_ADDR: usize = 0x0100;

pub struct Registers {
    pub a: u8,    // Accumulator
    pub x: u8,    // X index register
    pub y: u8,    // Y index register
    pub pc: u16,  // Program counter
    pub sp: u8,   // Stack pointer
    pub p: u8,    // Status flags
}

impl Registers {
    fn new() -> Self {
        Registers {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFF,
            p: 0x20,  // Set unused flag on power up
        }
    }
}


// Helper function

pub fn read_u16(memory: &[u8], addr: u16) -> u16 {
    let lo = memory[addr as usize] as u16;
    let hi = memory[(addr + 1) as usize] as u16;
    (hi << 8) | lo
}

fn is_negative(value: u8) -> bool { (value & 0x80) != 0 }

fn get_carry(regs: &Registers) -> bool { (regs.p & 0x01) != 0 }
fn get_zero(regs: &Registers) -> bool { (regs.p & 0x02) != 0 }
fn get_overflow(regs: &Registers) -> bool { (regs.p & 0x40) != 0 }
fn get_negative(regs: &Registers) -> bool { (regs.p & 0x80) != 0 }

fn set_carry(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0xFE) | ((value as u8) << 0) }
fn set_zero(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0xFD) | ((value as u8) << 1) }
fn set_interrupt(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0xFB) | ((value as u8) << 2) }
fn set_decimal(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0xF7) | ((value as u8) << 3) }
fn set_overflow(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0xBF) | ((value as u8) << 6) }
fn set_negative(value: bool, regs: &mut Registers) { regs.p = (regs.p & 0x7F) | ((value as u8) << 7) }

fn set_zero_and_negative(value: u8, regs: &mut Registers) {
    set_zero(value == 0, regs);
    set_negative(is_negative(value), regs);
}

fn should_branch(opcode: u8, regs: &Registers) -> bool {
    match opcode {
        0x10 => !get_negative(&regs),
        0x30 => get_negative(&regs),
        0x50 => !get_overflow(&regs),
        0x70 => get_overflow(&regs),
        0x90 => !get_carry(&regs),
        0xB0 => get_carry(&regs),
        0xD0 => !get_zero(&regs),
        0xF0 => get_zero(&regs),
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}

fn apply_math(opcode: u8, operand: u8, arg: u8) -> u8 {
    match opcode & 0xE0 {
        0x00 => operand | arg,  // ORA
        0x20 => operand & arg,  // AND
        0x40 => operand ^ arg,  // EOR
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}

fn apply_shift(opcode: u8, operand: u8, regs: &mut Registers) -> u8 {
    let left = opcode & 0x40 == 0;
    let result = match opcode & 0xE0 {
        0x00 => operand << 1,  // ASL
        0x20 => (operand << 1) | get_carry(regs) as u8,  // ROL
        0x40 => operand >> 1,  // LSR
        0x60 => (operand >> 1) | ((get_carry(regs) as u8) << 7),  // ROR
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    };
    let bit_to_carry = if left { operand & 0x80 } else { operand & 0x01 };
    set_carry(bit_to_carry != 0, regs);
    set_zero_and_negative(result, regs);
    result
}


// Read operands

fn get_movement_arg(opcode: u8, memory: &[u8], regs: &Registers) -> usize {
    let index = if opcode & 0x02 > 0 { regs.y as usize } else { regs.x as usize };  // LDX & STX use the Y register for indexing, LDY & STY use the X register
    match opcode & 0x1C {
        0x00 => regs.pc as usize,
        0x04 => memory[regs.pc as usize] as usize,
        0x14 => memory[regs.pc as usize] as usize + index,
        0x0C => read_u16(&memory, regs.pc) as usize,
        0x1C => read_u16(&memory, regs.pc) as usize + index,
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}

fn get_cmp_arg(opcode: u8, memory: &[u8], regs: &Registers) -> usize {
    match opcode & 0x0F {
        0x00 => regs.pc as usize,
        0x04 => memory[regs.pc as usize] as usize,
        0x0C => read_u16(&memory, regs.pc) as usize,
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}

fn get_shift_arg(opcode: u8, memory: &[u8], regs: &Registers) -> usize {
    match opcode & 0x1F {
        0x06 => memory[regs.pc as usize] as usize,
        0x16 => memory[regs.pc as usize] as usize + regs.x as usize,
        0x0E => read_u16(&memory, regs.pc) as usize,
        0x1E => read_u16(&memory, regs.pc) as usize + regs.x as usize,
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}

fn get_accumulator_arg(opcode: u8, memory: &[u8], regs: &Registers) -> usize {
    match opcode & 0x1F {
        0x01 => memory[memory[regs.pc as usize] as usize + regs.x as usize] as usize,
        0x11 => memory[memory[regs.pc as usize] as usize] as usize + regs.y as usize,
        0x05 => memory[regs.pc as usize] as usize,
        0x15 => memory[regs.pc as usize] as usize + regs.x as usize,
        0x09 => regs.pc as usize,
        0x19 => read_u16(&memory, regs.pc) as usize + regs.y as usize,
        0x0D => read_u16(&memory, regs.pc) as usize,
        0x1D => read_u16(&memory, regs.pc) as usize + regs.x as usize,
        _ => panic!("Invalid opcode: {:#04X}", opcode)
    }
}


// Run the program

pub fn run(bytecode: &Vec<u8>) -> ([u8; MEM_SIZE], Registers) {
    let mut memory = [0 as u8; MEM_SIZE];
    let mut regs = Registers::new();

    // Initialize the memory
    memory[RESET_VECTOR_ADDR] = (CODE_ADDR & 0xFF) as u8;
    memory[RESET_VECTOR_ADDR + 1] = (CODE_ADDR >> 8) as u8;
    memory[CODE_ADDR..CODE_ADDR + bytecode.len()].copy_from_slice(&bytecode);

    // Initialize the register file
    regs.pc = read_u16(&memory, RESET_VECTOR_ADDR as u16);

    // Main execution loop
    loop {
        let opcode = memory[regs.pc as usize];
        regs.pc += 1;

        match opcode {
            // Load & Store
            0x84 | 0x8C | 0x94 => {  // STY
                memory[get_movement_arg(opcode, &memory, &regs)] = regs.y;
                regs.pc += match opcode { 0x8C => 2, _ => 1 };
            },
            0xA0 | 0xA4 | 0xAC | 0xB4 | 0xBC => {  // LDY
                regs.y = memory[get_movement_arg(opcode, &memory, &regs)];
                regs.pc += match opcode { 0xAC | 0xBC => 2, _ => 1 };
                set_zero_and_negative(regs.y, &mut regs);
            },
            0x86 | 0x8E | 0x96 => {  // STX
                memory[get_movement_arg(opcode, &memory, &regs)] = regs.x;
                regs.pc += match opcode { 0x8E => 2, _ => 1 };
            },
            0xA2 | 0xA6 | 0xAE | 0xB6 | 0xBE => {  // LDX
                regs.x = memory[get_movement_arg(opcode, &memory, &regs)];
                regs.pc += match opcode { 0xAE | 0xBE => 2, _ => 1 };
                set_zero_and_negative(regs.x, &mut regs);
            },
            0x81 | 0x85 | 0x8D | 0x91 | 0x95 | 0x99 | 0x9D => {  // STA
                memory[get_accumulator_arg(opcode, &memory, &regs)] = regs.a;
                regs.pc += match opcode { 0x8D | 0x99 | 0x9D => 2, _ => 1 };
            },
            0xA1 | 0xA5 | 0xA9 | 0xAD | 0xB1 | 0xB5 | 0xB9 | 0xBD => {  // LDA
                regs.a = memory[get_accumulator_arg(opcode, &memory, &regs)];
                regs.pc += match opcode { 0xAD | 0xB9 | 0xBD => 2, _ => 1 };
                set_zero_and_negative(regs.a, &mut regs);
            },

            // Transfer
            0x98 => {  // TYA
                regs.a = regs.y;
                set_zero_and_negative(regs.a, &mut regs);
            },
            0xA8 => {  // TAY
                regs.y = regs.a;
                set_zero_and_negative(regs.y, &mut regs);
            },
            0x8A => {  // TXA
                regs.a = regs.x;
                set_zero_and_negative(regs.a, &mut regs);
            },
            0xAA => {  // TAX
                regs.x = regs.a;
                set_zero_and_negative(regs.x, &mut regs);
            },
            0x9A => {  // TXS
                regs.sp = regs.x;  // TXS doesn't affect any flags
            },
            0xBA => {  // TSX
                regs.x = regs.sp;
                set_zero_and_negative(regs.x, &mut regs);
            },

            // Stack
            0x08 => {  // PHP
                memory[STACK_ADDR + regs.sp as usize] = regs.p;
                regs.sp = regs.sp.wrapping_sub(1);
            },
            0x28 => {  // PLP
                regs.sp = regs.sp.wrapping_add(1);
                regs.p = memory[STACK_ADDR + regs.sp as usize];
            },
            0x48 => {  // PHA
                memory[STACK_ADDR + regs.sp as usize] = regs.a;
                regs.sp = regs.sp.wrapping_sub(1);
            },
            0x68 => {  // PLA
                regs.sp = regs.sp.wrapping_add(1);
                regs.a = memory[STACK_ADDR + regs.sp as usize];
                set_zero_and_negative(regs.a, &mut regs);
            },

            // Logic
            0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D |
            0x21 | 0x25 | 0x29 | 0x2D | 0x31 | 0x35 | 0x39 | 0x3D |
            0x41 | 0x45 | 0x49 | 0x4D | 0x51 | 0x55 | 0x59 | 0x5D => {  // ORA / AND / EOR
                let arg = memory[get_accumulator_arg(opcode, &memory, &regs)];
                regs.a = apply_math(opcode, regs.a, arg);
                regs.pc += match opcode & 0x1F { 0x0D | 0x19 | 0x1D => 2, _ => 1 };
                set_zero_and_negative(regs.a, &mut regs);
            },
            0x24 | 0x2C => {  // BIT
                let arg = match opcode {
                    0x24 => memory[memory[regs.pc as usize] as usize],
                    _ => memory[read_u16(&memory, regs.pc) as usize]
                };
                regs.pc += match opcode { 0x2C => 2, _ => 1 };
                set_zero((regs.a & arg) == 0, &mut regs);
                set_overflow((arg & 0x40) != 0, &mut regs);
                set_negative((arg & 0x80) != 0, &mut regs);
            },

            // Arithmetic
            0x61 | 0x65 | 0x69 | 0x6D | 0x71 | 0x75 | 0x79 | 0x7D => {  // ADC
                let arg = memory[get_accumulator_arg(opcode, &memory, &regs)];
                let result = regs.a.wrapping_add(arg).wrapping_add(regs.p & 0x01);
                set_carry(regs.a as u16 + arg as u16 + (regs.p & 0x01) as u16 > 0xFF, &mut regs);  // Carry is set if result is greater than 255
                set_overflow((regs.a & 0x80 == arg & 0x80) && (regs.a & 0x80 != result as u8 & 0x80), &mut regs);  // Overflow is set if sign bit changes
                set_zero_and_negative(result, &mut regs);
                regs.a = result;
                regs.pc += match opcode { 0x6D | 0x79 | 0x7D => 2, _ => 1 };
            },
            0xE1 | 0xE5 | 0xE9 | 0xED | 0xF1 | 0xF5 | 0xF9 | 0xFD => {  // SBC
                let arg = memory[get_accumulator_arg(opcode, &memory, &regs)];
                let result = regs.a.wrapping_sub(arg).wrapping_sub((regs.p & 0x01) ^ 1);  // Invert the carry bit
                set_carry(regs.a as i16 - arg as i16 - ((regs.p & 0x01) ^ 1) as i16 >= 0, &mut regs);  // Carry is set if result is non-negative
                set_overflow((regs.a & 0x80 == arg & 0x80) && (regs.a & 0x80 != result as u8 & 0x80), &mut regs);
                set_zero_and_negative(result, &mut regs);
                regs.a = result;
                regs.pc += match opcode { 0xED | 0xF9 | 0xFD => 2, _ => 1 };
            },
            0xC1 | 0xC5 | 0xC9 | 0xCD | 0xD1 | 0xD5 | 0xD9 | 0xDD => {  // CMP
                let arg = memory[get_accumulator_arg(opcode, &memory, &regs)];
                let result = regs.a.wrapping_sub(arg);
                set_carry(regs.a >= arg, &mut regs);
                set_zero_and_negative(result, &mut regs);
                regs.pc += match opcode { 0xCD | 0xD9 | 0xDD => 2, _ => 1 };
            },
            0xC0 | 0xC4 | 0xCC | 0xE0 | 0xE4 | 0xEC => {  // CPY / CPX
                let register = if opcode & 0x20 > 0 { regs.x } else { regs.y };
                let arg = memory[get_cmp_arg(opcode, &memory, &regs)];
                let result = register.wrapping_sub(arg);
                set_carry(register >= arg, &mut regs);
                set_zero_and_negative(result, &mut regs);
                regs.pc += match opcode { 0xCC | 0xEC => 2, _ => 1 };
            },

            // Shift
            0x06 | 0x0E | 0x16 | 0x1E |
            0x26 | 0x2E | 0x36 | 0x3E |
            0x46 | 0x4E | 0x56 | 0x5E |
            0x66 | 0x6E | 0x76 | 0x7E => {  // ASL / LSR / ROL / ROR
                let addr = get_shift_arg(opcode, &memory, &regs);
                memory[addr] = apply_shift(opcode, memory[addr], &mut regs);
                regs.pc += match opcode & 0x1F { 0x0E | 0x1E => 2, 0x06 | 0x16 => 1, _ => 0 };
            },
            0x0A | 0x2A | 0x4A | 0x6A => {  // ASL / LSR / ROL / ROR A
                regs.a = apply_shift(opcode, regs.a, &mut regs);
                regs.pc += 1;
            },

            // Increment & Decrement
            0xC6 | 0xCE | 0xD6 | 0xDE | 0xE6 | 0xEE | 0xF6 | 0xFE => {  // DEC / INC
                let addr = get_shift_arg(opcode, &memory, &regs);
                memory[addr] = if opcode & 0x20 > 0 { memory[addr].wrapping_add(1) } else { memory[addr].wrapping_sub(1) };
                regs.pc += match opcode { 0xCE | 0xDE | 0xEE | 0xFE => 2, _ => 1 };
                set_zero_and_negative(memory[addr], &mut regs);
            },
            0x88 | 0xC8 => {  // DEY / INY
                regs.y = if opcode == 0xC8 { regs.y.wrapping_add(1) } else { regs.y.wrapping_sub(1) };
                set_zero_and_negative(regs.y, &mut regs);
            },
            0xCA | 0xE8 => {  // DEX / INX
                regs.x = if opcode == 0xE8 { regs.x.wrapping_add(1) } else { regs.x.wrapping_sub(1) };
                set_zero_and_negative(regs.x, &mut regs);
            },

            // Control flow
            0x00 => { break; },  // BRK
            0xEA => { continue; },  // NOP
            0x4C => { regs.pc = read_u16(&memory, regs.pc); },  // JMP abs
            0x6C => { regs.pc = read_u16(&memory, read_u16(&memory, regs.pc)); },  // JMP ind
            0x20 => {  // JSR
                // NOTE: The address is only bumped by 2, so it points to the last byte of the JSR instruction
                // See https://retrocomputing.stackexchange.com/questions/19543/why-does-the-6502-jsr-instruction-only-increment-the-return-address-by-2-bytes for an explanation
                let addr = read_u16(&memory, regs.pc);
                regs.pc += 1;
                memory[STACK_ADDR + regs.sp as usize] = (regs.pc >> 8) as u8;
                memory[STACK_ADDR + regs.sp as usize - 1] = (regs.pc & 0xFF) as u8;
                regs.sp = regs.sp.wrapping_sub(2);
                regs.pc = addr;
            },
            0x40 => {  // RTI
                regs.sp = regs.sp.wrapping_add(1);
                regs.p = memory[STACK_ADDR + regs.sp as usize];    // NOTE: Bit 5 should be ignored here, but it's not used for anything so it doesn't matter
                regs.sp = regs.sp.wrapping_add(1);
                regs.pc = read_u16(&memory, STACK_ADDR as u16 + regs.sp as u16);
                regs.sp = regs.sp.wrapping_add(1);

            },
            0x60 => {  // RTS
                regs.sp = regs.sp.wrapping_add(1);
                regs.pc = read_u16(&memory, STACK_ADDR as u16 + regs.sp as u16) + 1;  // Add 1 to skip the last byte of the JSR instruction
                regs.sp = regs.sp.wrapping_add(1);
            },

            // Branching
            0x10 | 0x30 | 0x50 | 0x70 | 0x90 | 0xB0 | 0xD0 | 0xF0 => {  // BPL, BMI, BVC, BVS, BCC, BCS, BNE, BEQ
                let offset = memory[regs.pc as usize] as i8;
                regs.pc = 1 + regs.pc.wrapping_add_signed(if should_branch(opcode, &regs) { offset as i16 } else { 0 });
            },

            // Flags
            0x18 => { set_carry(false, &mut regs); },  // CLC
            0x38 => { set_carry(true, &mut regs); },  // SEC
            0x58 => { set_interrupt(false, &mut regs); },  // CLI
            0x78 => { set_interrupt(true, &mut regs); },  // SEI
            0xB8 => { set_overflow(false, &mut regs); },  // CLV
            0xD8 => { set_decimal(false, &mut regs); },  // CLD
            0xF8 => { set_decimal(true, &mut regs); },  // SED

            _ => { panic!("Unknown opcode: {:#04X}", opcode); }
        }
    }

    (memory, regs)
}