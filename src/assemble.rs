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


use std::fs::File;
use std::io::{BufRead, BufReader};


// Expression type checking

fn is_byte_literal(literal: &str) -> bool {
    literal.starts_with("#$")
}
fn is_zpg_address(addr: &str) -> bool {
    addr.len() == 3 && addr.starts_with("$")
}
fn is_address(addr: &str) -> bool {
    addr.len() == 5 && addr.starts_with("$")
}
fn is_indirect_address(addr: &str) -> bool {
    addr.starts_with("(") && addr.ends_with(")") && is_address(&addr[1..addr.len()-1])
}
fn is_indexed_zpg_address(addr: &str) -> bool {
    let parts: Vec<_> = addr.split(',').collect();
    parts.len() == 2 && is_zpg_address(parts[0])
}
fn is_indexed_address(addr: &str) -> bool {
    let parts: Vec<_> = addr.split(',').collect();
    parts.len() == 2 && is_address(parts[0])
}
fn is_indirect_indexed_zpg_address(addr: &str) -> bool {
    addr.len() > 4 && addr.starts_with("(") && is_zpg_address(&addr[1..4]) && &addr[4..5] == ","
}


// Expression parsing

fn parse_byte_literal(literal: &str) -> u8 {
    if literal.len() != 4 {
        panic!("Byte literal must be a single byte: {}", literal);
    } else {
        u8::from_str_radix(&literal[2..], 16)
            .expect(&format!("Invalid byte literal: {}", literal))
    }
}

fn parse_address(addr: &str) -> Vec<u8> {
    (1..addr.len()).step_by(2)
        .map(|i| u8::from_str_radix(&addr[i..i+2], 16).expect(&format!("Invalid address: {}", addr)))
        .rev()
        .collect()
}

fn parse_indirect_address(addr: &str) -> Vec<u8> {
    if addr.len() == 7 { parse_address(&addr[2..6]) } else { parse_address(&addr[2..4]) }
}

fn parse_indexed_address(addr: &str) -> (Vec<u8>, char) {
    let parts: Vec<_> = addr.split(',').collect();
    if parts.len() != 2 || (parts[1] != "X" && parts[1] != "Y") {
        panic!("Indexed address must be in the form $LL[HH],X|Y: {}", addr);
    } else {
        (parse_address(parts[0]), parts[1].chars().next().unwrap())
    }
}

fn parse_indirect_indexed_address(addr: &str) -> (Vec<u8>, char) {
    if addr.len() != 7 || (!addr.ends_with(",X)") && !addr.ends_with("),Y")){
        panic!("Indirect indexed address must be in the form ($LL,X) or the form ($LL),Y: {}", addr);
    } else {
        (parse_address(&addr[1..4]), if addr.ends_with(",X)") { 'X' } else { 'Y' })
    }
}


// Parsing helper functions

macro_rules! ivec {
    ($op:expr, $args:expr) => {{
        let mut result = vec![$op];
        result.extend($args);
        result
    }}
}

fn parse_abs_op(op: &str, args: &str, opcode: u8) -> Vec<u8> { parse_direct_op(op, args, Some(opcode), None, None) }
fn parse_rel_op(op: &str, args: &str, opcode: u8) -> Vec<u8> { parse_direct_op(op, args, None, Some(opcode), None) }

fn parse_direct_op(op: &str, args: &str, abs_opcode: Option<u8>, rel_opcode: Option<u8>, imm_opcode: Option<u8>) -> Vec<u8> {
    if is_address(args) && abs_opcode.is_some() {
        ivec![abs_opcode.unwrap(), parse_address(args)]
    } else if is_zpg_address(args) && rel_opcode.is_some() {
        ivec![rel_opcode.unwrap(), parse_address(args)]
    } else if is_byte_literal(args) && imm_opcode.is_some() {
        vec![imm_opcode.unwrap(), parse_byte_literal(args)]
    } else {
        panic!("Invalid addressing mode for op {}: {}", op, args);
    }
}

fn parse_jmp_op(args: &str) -> Vec<u8> {
    if is_address(args) {
        ivec![0x4C, parse_address(args)]
    } else if is_indirect_address(args) {
        ivec![0x6C, parse_indirect_address(args)]
    } else {
        panic!("Invalid addressing mode for op JMP: {}", args);
    }
}

fn parse_shift_op(op: &str, args: &str, offset: u8) -> Vec<u8> {
    if is_indexed_zpg_address(args) {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset + if index == 'X' { 0x16 } else { panic!("Indexing on Y not supported for op {}: {}", op, args) };
        ivec![opcode, address]
    } else if is_indexed_address(args) {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset + if index == 'X' { 0x1E } else { panic!("Indexing on Y not supported for op {}: {}", op, args) };
        ivec![opcode, address]
    } else if is_zpg_address(args) {
        ivec![offset + 0x06, parse_address(args)]
    } else if is_address(args) {
        ivec![offset + 0x0E, parse_address(args)]
    } else if args == "A" && op != "DEC" && op != "INC" {
        vec![offset + 0x0A]
    } else {
        panic!("Invalid addressing mode for op {}: {}", op, args);
    }
}

fn parse_accumulator_op(op: &str, args: &str, offset: u8) -> Vec<u8> {
    if is_indirect_indexed_zpg_address(args) {
        let (address, index) = parse_indirect_indexed_address(args);
        let opcode = offset + if index == 'X' { 0x01 } else { 0x11 };
        ivec![opcode, address]
    } else if is_indexed_zpg_address(args) {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset + if index == 'X' { 0x15 } else { panic!("Indexing on Y not supported for op {}: {}", op, args) };
        ivec![opcode, address]
    } else if is_indexed_address(args) {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset + if index == 'X' { 0x1D } else { 0x19 };
        ivec![opcode, address]
    } else if is_zpg_address(args) {
        ivec![offset + 0x05, parse_address(args)]
    } else if is_address(args) {
        let address = parse_address(args);
        ivec![offset + 0x0D, address]
    } else if is_byte_literal(args) && op != "STA" {
        vec![offset + 0x09, parse_byte_literal(args)]
    } else {
        panic!("Invalid addressing mode for op {}: {}", op, args);
    }
}

fn parse_movement_op(op: &str, args: &str, offset: u8) -> Vec<u8> {
    if is_indexed_zpg_address(args) {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset +
            if index == 'X' && (op == "LDY" || op == "STY") { 0x14 }
            else if index == 'Y' && (op == "LDX" || op == "STX") { 0x16 }
            else { panic!("Indexing on {} not supported for op {}: {}", index, op, args) };
        ivec![opcode, address]
    } else if is_indexed_address(args) && op != "STX" && op != "STY" {
        let (address, index) = parse_indexed_address(args);
        let opcode = offset +
            if index == 'X' && op == "LDY" { 0x1C }
            else if index == 'Y' && op == "LDX" { 0x1E }
            else { panic!("Indexing on {} not supported for op {}: {}", index, op, args) };
        ivec![opcode, address]
    } else if is_zpg_address(args) {
        ivec![offset + 0x04, parse_address(args)]
    } else if is_address(args) {
        ivec![offset + 0x0C, parse_address(args)]
    } else if is_byte_literal(args) && op != "STX" && op != "STY" {
        vec![offset, parse_byte_literal(args)]
    } else {
        panic!("Invalid addressing mode for op {}: {}", op, args);
    }
}


// Parse instruction

fn parse_line(line: &str) -> Vec<u8> {
    let line = line.split(';').next().unwrap().trim();
    let op = &line[0..3];
    let args = &line[4..];
    match op {
        // Load & Store
        "STY" => parse_movement_op("STY", args, 0x80),  // Load / store Y register
        "LDY" => parse_movement_op("LDY", args, 0xA0),
        "STX" => parse_movement_op("STX", args, 0x82),  // Load / store X register
        "LDX" => parse_movement_op("LDX", args, 0xA2),
        "STA" => parse_accumulator_op("STA", args, 0x80),  // Load / store accumulator
        "LDA" => parse_accumulator_op("LDA", args, 0xA0),

        // Transfer
        "TYA" => vec![0x98],  // Transfer Y / accumulator
        "TAY" => vec![0xA8],
        "TXA" => vec![0x8A],  // Transfer X / accumulator
        "TAX" => vec![0xAA],
        "TXS" => vec![0x9A],  // Transfer X / stack pointer
        "TSX" => vec![0xBA],

        // Stack
        "PHP" => vec![0x08],  // Push / pull processor register
        "PLP" => vec![0x28],
        "PHA" => vec![0x48],  // Push / pull accumulator
        "PLA" => vec![0x68],

        // Logic
        "ORA" => parse_accumulator_op("ORA", args, 0x00),
        "AND" => parse_accumulator_op("AND", args, 0x20),
        "EOR" => parse_accumulator_op("EOR", args, 0x40),
        "BIT" => parse_direct_op("BIT", args, Some(0x2C), Some(0x24), None),  // Test bits in memory with accumulator

        // Arithmetic
        "ADC" => parse_accumulator_op("ADC", args, 0x60),
        "SBC" => parse_accumulator_op("SBC", args, 0xE0),
        "CMP" => parse_accumulator_op("CMP", args, 0xC0),
        "CPY" => parse_direct_op("CPY", args, Some(0xCC), Some(0xC4), Some(0xC0)),  // Compare Y register
        "CPX" => parse_direct_op("CPX", args, Some(0xEC), Some(0xE4), Some(0xE0)),  // Compare X register

        // Shift
        "ASL" => parse_shift_op("INC", args, 0x00),
        "ROL" => parse_shift_op("ROL", args, 0x20),
        "LSR" => parse_shift_op("LSR", args, 0x40),
        "ROR" => parse_shift_op("ROR", args, 0x60),

        // Increment & Decrement
        "DEC" => parse_shift_op("DEC", args, 0xC0),  // Increment / decrement memory
        "INC" => parse_shift_op("INC", args, 0xE0),
        "DEY" => vec![0x88],  // Increment / decrement Y
        "INY" => vec![0xC8],
        "DEX" => vec![0xCA],  // Increment / decrement X
        "INX" => vec![0xE8],

        // Control flow
        "BRK" => vec![0x00],
        "NOP" => vec![0xEA],
        "JMP" => parse_jmp_op(args),
        "JSR" => parse_abs_op("JSR", args, 0x20),  // Jump to subroutine
        "RTI" => vec![0x40],  // Return from interrupt
        "RTS" => vec![0x60],  // Return from subroutine

        // Branching
        "BPL" => parse_rel_op("BPL", args, 0x10),  // Branch if plus / minus
        "BMI" => parse_rel_op("BMI", args, 0x30),
        "BVC" => parse_rel_op("BVC", args, 0x50),  // Branch if overflow flag is cleared / set
        "BVS" => parse_rel_op("BVS", args, 0x70),
        "BCC" => parse_rel_op("BCC", args, 0x90),  // Branch if carry flag is cleared / set
        "BCS" => parse_rel_op("BCS", args, 0xB0),
        "BNE" => parse_rel_op("BNE", args, 0xD0),  // Branch if the zero flag is cleared / set
        "BEQ" => parse_rel_op("BEQ", args, 0xF0),

        // Flags
        "CLC" => vec![0x18],  // Clear / set carry flag (C)
        "SEC" => vec![0x38],
        "CLI" => vec![0x58],  // Clear / set interrupt disable flag (I)
        "SEI" => vec![0x78],
        "CLV" => vec![0xB8],  // Clear overflow flag (V)
        "CLD" => vec![0xD8],  // Clear / set decimal mode flag (D)
        "SED" => vec![0xF8],

        _ => panic!("Invalid op: {}", op)
    }
}


// Assemble program

pub fn assemble(file_path: &String) -> Vec<u8> {
    let file = File::open(file_path).expect("Failed to open file");
    let buf_reader = BufReader::new(file);


    let mut program: Vec<u8> = Vec::new();
    for line in buf_reader.lines() {
        let line = line.expect("Failed to read line");
        program.extend(parse_line(&line));
    }
    program
}
