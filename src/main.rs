mod assemble;
mod run;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
    } else {
        let code = fs::read_to_string(&args[1]).expect("Unable to read file");
        let bytecode = assemble::assemble(&code);
        run::run(&bytecode);
    }
}


#[cfg(test)]
mod tests {
    use super::assemble::assemble;
    use super::run::{run, read_u16, Registers};

    fn binary_op(op: &str, a: u8, b: u8) -> u8 {
        match op {
            "ORA" => a | b,
            "AND" => a & b,
            "EOR" => a ^ b,
            "ADC" => a.wrapping_add(b),
            "SBC" => a.wrapping_sub(b).wrapping_sub(1),  // Subtract one since carry flag usually won't be set
            "CMP" => a,
            _ => panic!("Invalid operation")
        }
    }
    fn unary_op(op: &str, a: u8) -> u8 {
        match op {
            "ASL" | "ROL" => a << 1,
            "LSR" | "ROR" => a >> 1,
            "INC" => a.wrapping_add(1),
            "DEC" => a.wrapping_sub(1),
            _ => panic!("Invalid operation")
        }
    }
    fn get_reg(reg: &str, regs: &Registers) -> u8 {
        match reg {
            "A" => regs.a,
            "X" => regs.x,
            "Y" => regs.y,
            _ => panic!("Invalid register")
        }
    }

    #[test]
    fn test_load() {
        for (op, reg) in [("LDA", "A"), ("LDX", "X"), ("LDY", "Y")].iter() {
            // Immediate
            let (_, regs) = run(&assemble(&format!("{op} #$01", op=op)));
            assert_eq!(get_reg(reg, &regs), 0x01);

            // Zero page
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $10\n{op} $10", op=op)));
            assert_eq!(get_reg(reg, &regs), 0xAB);
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $12\nLD{ireg} #$02\n{op} $10,{ireg}", op=op, ireg=if *reg == "X" { "Y" } else { "X" })));
            assert_eq!(get_reg(reg, &regs), 0xAB);

            // Absolute
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0200\n{op} $0200", op=op)));
            assert_eq!(get_reg(reg, &regs), 0xAB);
            if *op != "LDX" {
                let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0202\nLDX #$02\n{op} $0200,X", op=op)));
                assert_eq!(get_reg(reg, &regs), 0xAB);
            }
            if *op != "LDY" {
                let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0202\nLDY #$02\n{op} $0200,Y", op=op)));
                assert_eq!(get_reg(reg, &regs), 0xAB);
            }
        }

        // Indirect
        let (_, regs) = run(&assemble(&format!("LDA #$14\nSTA $12\nLDA #$AB\nSTA $14\nLDX #$02\nLDA ($10,X)")));
        assert_eq!(regs.a, 0xAB);
        let (_, regs) = run(&assemble(&format!("LDA #$12\nSTA $10\nLDA #$AB\nSTA $14\nLDY #$02\nLDA ($10),Y")));
        assert_eq!(regs.a, 0xAB);
    }

    #[test]
    fn test_store() {
        for (op, reg) in [("STA", "A"), ("STX", "X"), ("STY", "Y")].iter() {
            // Zero page
            let (memory, _) = run(&assemble(&format!("LD{reg} #$AB\n{op} $10", reg=reg, op=op)));
            assert_eq!(memory[0x10], 0xAB);
            let (memory, _) = run(&assemble(&format!("LD{reg} #$AB\nLD{ireg} #$02\n{op} $10,{ireg}", reg=reg, op=op, ireg=if *reg == "X" { "Y" } else { "X" })));
            assert_eq!(memory[0x12], 0xAB);

            // Absolute
            let (memory, _) = run(&assemble(&format!("LD{reg} #$AB\n{op} $0200", reg=reg, op=op)));
            assert_eq!(memory[0x0200], 0xAB);
        }

        // Absolute
        let (memory, _) = run(&assemble(&format!("LDA #$AB\nLDX #$02\nSTA $0200,X")));
        assert_eq!(memory[0x0202], 0xAB);
        let (memory, _) = run(&assemble(&format!("LDA #$AB\nLDY #$02\nSTA $0200,Y")));
        assert_eq!(memory[0x0202], 0xAB);

        // Indirect
        let (memory, _) = run(&assemble(&format!("LDA #$14\nSTA $12\nLDA #$AB\nLDX #$02\nSTA ($10,X)")));
        assert_eq!(memory[0x14], 0xAB);
        let (memory, _) = run(&assemble(&format!("LDA #$12\nSTA $10\nLDA #$AB\nLDY #$02\nSTA ($10),Y")));
        assert_eq!(memory[0x14], 0xAB);
    }

    #[test]
    fn test_transfer() {
        let (_, regs) = run(&assemble(&format!("LDA #$01\nTAX\nLDA #$02\nTAY")));
        assert_eq!(regs.x, 0x01);
        assert_eq!(regs.y, 0x02);
        assert_eq!(regs.a, 0x02);

        let (_, regs) = run(&assemble(&format!("TSX\nTXA\nLDX #$01\nTXS")));
        assert_eq!(regs.sp, 0x01);
        assert_eq!(regs.a, 0xFF);

        let (_, regs) = run(&assemble(&format!("LDY #$01\nTYA")));
        assert_eq!(regs.a, 0x01);
    }

    #[test]
    fn test_stack() {
        let (memory, regs) = run(&assemble(&format!("LDA #$01\nPHA")));
        assert_eq!(regs.a, 0x01);
        assert_eq!(regs.sp, 0xFE);
        assert_eq!(memory[0x1FF], 0x01);

        let (_, regs) = run(&assemble(&format!("LDA #$FF\nPHA\nLDA #$01\nPLA")));
        assert_eq!(regs.a, 0xFF);
        assert_eq!(regs.sp, 0xFF);

        let (memory, regs) = run(&assemble(&format!("PHP")));
        assert_eq!(regs.sp, 0xFE);
        assert_eq!(regs.p, 0x20);
        assert_eq!(memory[0x1FF], 0x20);

        let (_, regs) = run(&assemble(&format!("LDA #$FF\nPHA\nPLP")));
        assert_eq!(regs.p, 0xFF);
        assert_eq!(regs.sp, 0xFF);
    }

    #[test]
    fn test_logic() {
        for op in ["ORA", "AND", "EOR"].iter() {
            // Immediate
            let (_, regs) = run(&assemble(&format!("LDA #$01\n{} #$02", op)));
            assert_eq!(regs.a, binary_op(op, 0x01, 0x02));
            assert_eq!(regs.p, if *op == "AND" { 0b0010_0010 } else { 0b0010_0000 });

            let (_, regs) = run(&assemble(&format!("LDA #$01\n{} #$03", op)));
            assert_eq!(regs.a, binary_op(op, 0x01, 0x03));
            assert_eq!(regs.p, 0b0010_0000);

            let (_, regs) = run(&assemble(&format!("LDA #${:02x}\n{} #$80", if *op == "AND" { 0x80 } else { 0x00}, op)));
            assert_eq!(regs.a, 0x80);
            assert_eq!(regs.p, 0b1010_0000);

            let (_, regs) = run(&assemble(&format!("LDA #$00\n{} #$00", op)));
            assert_eq!(regs.a, 0x00);
            assert_eq!(regs.p, 0b0010_0010);

            // Absolute + indexing
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0200\nLDA #$37\n{} $0200", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0202\nLDA #$37\nLDX #$02\n{} $0200,X", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $02FE\nLDA #$37\nLDY #$FE\n{} $0200,Y", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));

            // Zero page + indexing
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $10\nLDA #$37\n{} $10", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $12\nLDA #$37\nLDX #$02\n{} $10,X", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));

            // Indirect
            let (_, regs) = run(&assemble(&format!("LDA #$14\nSTA $12\nLDA #$AB\nSTA $14\nLDX #$02\nLDA #$37\n{} ($10,X)", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));
            let (_, regs) = run(&assemble(&format!("LDA #$12\nSTA $10\nLDA #$AB\nSTA $14\nLDY #$02\nLDA #$37\n{} ($10),Y", op)));
            assert_eq!(regs.a, binary_op(op, 0xAB, 0x37));
        }
    }

    #[test]
    fn test_arithmetic() {
        for op in ["ADC", "SBC", "CMP"].iter() {
            // Immediate
            let (_, regs) = run(&assemble(&format!("LDA #$01\n{} #$02", op)));
            assert_eq!(regs.a, binary_op(op, 0x01, 0x02));
            assert_eq!(regs.p, match *op { "ADC" => 0b0010_0000, "SBC" => 0b1110_0000, "CMP" => 0b1010_0000, _ => 0 });

            let (_, regs) = run(&assemble(&format!("SEC\nLDA #$03\n{} #$01", op)));
            assert_eq!(regs.a, binary_op(op, 0x03, 0x01) + if *op != "CMP" { 1 } else { 0 });  // Carry flag is set, so ADC will add 1 and SBC will NOT subtract 1
            assert_eq!(regs.p, match *op { "ADC" => 0b0010_0000, "SBC" => 0b0010_0001, "CMP" => 0b0010_0001, _ => 0 });

            let (_, regs) = run(&assemble(&format!("LDA #$FF\n{} #$02", op)));
            assert_eq!(regs.a, binary_op(op, 0xFF, 0x02));
            assert_eq!(regs.p, match *op { "ADC" => 0b0010_0001, "SBC" => 0b1010_0001, "CMP" => 0b1010_0001, _ => 0 });

            let (_, regs) = run(&assemble(&format!("{}\nLDA #$00\n{} #$00", if *op == "SBC" { "SEC" } else { "" }, op)));
            assert_eq!(regs.a, 0x00);
            assert_eq!(regs.p, 0b0010_0010 | if *op == "ADC" { 0x00 } else { 0x01 });

            // Absolute + indexing
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0200\nLDA #$37\n{} $0200", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $0202\nLDA #$37\nLDX #$02\n{} $0200,X", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $02FE\nLDA #$37\nLDY #$FE\n{} $0200,Y", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));

            // Zero page + indexing
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $10\nLDA #$37\n{} $10", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));
            let (_, regs) = run(&assemble(&format!("LDA #$AB\nSTA $12\nLDA #$37\nLDX #$02\n{} $10,X", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));

            // Indirect
            let (_, regs) = run(&assemble(&format!("LDA #$14\nSTA $12\nLDA #$AB\nSTA $14\nLDX #$02\nLDA #$37\n{} ($10,X)", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));
            let (_, regs) = run(&assemble(&format!("LDA #$12\nSTA $10\nLDA #$AB\nSTA $14\nLDY #$02\nLDA #$37\n{} ($10),Y", op)));
            assert_eq!(regs.a, binary_op(op, 0x37, 0xAB));
        }

        for (op, reg) in [("CPX", "X"), ("CPY", "Y")].iter() {
            // Immediate
            let (_, regs) = run(&assemble(&format!("{op} #$01", op=op)));
            assert_eq!(get_reg(reg, &regs), 0x00);
            assert_eq!(regs.p, 0b1010_0000);

            let (_, regs) = run(&assemble(&format!("LD{reg} #$01\n{op} #$01", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x01);
            assert_eq!(regs.p, 0b0010_0011);

            let (_, regs) = run(&assemble(&format!("LD{reg} #$02\n{op} #$01", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x02);
            assert_eq!(regs.p, 0b0010_0001);

            // Zero page
            let (_, regs) = run(&assemble(&format!("LD{reg} #$01\nST{reg} $10\n{op} $10", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x01);
            assert_eq!(regs.p, 0b0010_0011);

            let (_, regs) = run(&assemble(&format!("LD{reg} #$01\nST{reg} $10\nLD{reg} #$02\n{op} $10", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x02);
            assert_eq!(regs.p, 0b0010_0001);

            // Absolute
            let (_, regs) = run(&assemble(&format!("LD{reg} #$01\nST{reg} $0200\n{op} $0200", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x01);
            assert_eq!(regs.p, 0b0010_0011);

            let (_, regs) = run(&assemble(&format!("LD{reg} #$01\nST{reg} $0200\nLD{reg} #$02\n{op} $0200", reg=reg, op=op)));
            assert_eq!(get_reg(reg, &regs), 0x02);
            assert_eq!(regs.p, 0b0010_0001);
        }
    }

    #[test]
    fn test_shift() {
        for op in ["ASL", "LSR", "ROL", "ROR"].iter() {
            // Register
            let (_, regs) = run(&assemble(&format!("LDA #$01\n{} A", op)));
            assert_eq!(regs.a, unary_op(op, 0x01));
            assert_eq!(regs.p, match *op { "ASL" | "ROL" => 0b0010_0000, _ => 0b0010_0011 });

            let (_, regs) = run(&assemble(&format!("LDA #$80\n{} A", op)));
            assert_eq!(regs.a, unary_op(op, 0x80));
            assert_eq!(regs.p, match *op { "ASL" | "ROL" => 0b0010_0011, _ => 0b0010_0000 });

            let (_, regs) = run(&assemble(&format!("SEC\nLDA #$0F\n{} A", op)));
            assert_eq!(regs.a, unary_op(op, 0x0F) + match *op { "ROL" => 0x01, "ROR" => 0x80, _ => 0 });
            assert_eq!(regs.p, match *op { "ASL" | "ROL" => 0b0010_0000, "ROR" => 0b1010_0001, _ => 0b0010_0001 });

            let (_, regs) = run(&assemble(&format!("SEC\nLDA #$F0\n{} A", op)));
            assert_eq!(regs.a, unary_op(op, 0xF0) + match *op { "ROL" => 0x01, "ROR" => 0x80, _ => 0 });
            assert_eq!(regs.p, match *op { "ASL" | "ROL" => 0b1010_0001, "ROR" => 0b1010_0000, _ => 0b0010_0000 });

            // Zero page + indexing
            let (memory, _) = run(&assemble(&format!("LDA #$04\nSTA $10\n{} $10", op)));
            assert_eq!(memory[0x10], unary_op(op, 0x04));
            let (memory, _) = run(&assemble(&format!("LDA #$04\nSTA $12\nLDX #$02\n{} $10,X", op)));
            assert_eq!(memory[0x12], unary_op(op, 0x04));

            // Absolute + indexing
            let (memory, _) = run(&assemble(&format!("LDA #$04\nSTA $0200\n{} $0200", op)));
            assert_eq!(memory[0x0200], unary_op(op, 0x04));
            let (memory, _) = run(&assemble(&format!("LDA #$04\nSTA $0202\nLDX #$02\n{} $0200,X", op)));
            assert_eq!(memory[0x0202], unary_op(op, 0x04));
        }
    }

    #[test]
    fn test_inc_dec() {
        for op in ["INC", "DEC"].iter() {
            // Zero page
            let (memory, regs) = run(&assemble(&format!("LDA #$01\nSTA $10\n{} $10", op)));
            assert_eq!(memory[0x10], unary_op(op, 0x01));
            assert_eq!(regs.p, if *op == "INC" { 0b0010_0000 } else { 0b0010_0010 });

            let (memory, regs) = run(&assemble(&format!("LDA #$FF\nSTA $10\n{} $10", op)));
            assert_eq!(memory[0x10], unary_op(op, 0xFF));
            assert_eq!(regs.p, if *op == "INC" { 0b0010_0010 } else { 0b1010_0000 });

            // Zero page + indexing
            let (memory, _) = run(&assemble(&format!("LDA #$01\nSTA $10\n{} $10", op)));
            assert_eq!(memory[0x10], unary_op(op, 0x01));
            let (memory, _) = run(&assemble(&format!("LDA #$01\nSTA $12\nLDX #$02\n{} $10,X", op)));
            assert_eq!(memory[0x12], unary_op(op, 0x01));

            // Absolute + indexing
            let (memory, _) = run(&assemble(&format!("LDA #$01\nSTA $0200\n{} $0200", op)));
            assert_eq!(memory[0x0200], unary_op(op, 0x01));
            let (memory, _) = run(&assemble(&format!("LDA #$01\nSTA $0202\nLDX #$02\n{} $0200,X", op)));
            assert_eq!(memory[0x0202], unary_op(op, 0x01));
        }

        for (op, reg) in [("INX", "X"), ("INY", "Y")].iter() {
            let (_, regs) = run(&assemble(&format!("LD{} #$01\n{}", reg, op)));
            assert_eq!(get_reg(reg, &regs), 0x02);
            assert_eq!(regs.p, 0b0010_0000);

            let (_, regs) = run(&assemble(&format!("LD{} #$FF\n{}", reg, op)));
            assert_eq!(get_reg(reg, &regs), 0x00);
            assert_eq!(regs.p, 0b0010_0010);
        }

        for (op, reg) in [("DEX", "X"), ("DEY", "Y")].iter() {
            let (_, regs) = run(&assemble(&format!("LD{} #$01\n{}", reg, op)));
            assert_eq!(get_reg(reg, &regs), 0x00);
            assert_eq!(regs.p, 0b0010_0010);

            let (_, regs) = run(&assemble(&format!("LD{} #$00\n{}", reg, op)));
            assert_eq!(get_reg(reg, &regs), 0xFF);
            assert_eq!(regs.p, 0b1010_0000);
        }
    }

    #[test]
    fn test_control_flow() {
        let (_, regs) = run(&assemble(&format!("JMP $8004\nBRK\nBRK")));
        assert_eq!(regs.pc, 0x8005);

        let (_, regs) = run(&assemble(&format!("JMP ($8004)\nJMP $8007\nBRK")));
        assert_eq!(regs.pc, 0x8008);

        let (memory, regs) = run(&assemble(&format!("JSR $8004\nBRK\nBRK")));
        assert_eq!(regs.pc, 0x8005);
        assert_eq!(regs.sp, 0xFD);
        assert_eq!(read_u16(&memory, 0x1FE), 0x8002);

        let (_, regs) = run(&assemble(&format!("JSR $8004\nBRK\nLDA #$01\nRTS")));
        assert_eq!(regs.pc, 0x8004);
        assert_eq!(regs.sp, 0xFF);
        assert_eq!(regs.a, 0x01);

        let (_, regs) = run(&assemble(&format!("JSR $8004\nBRK\nLDA #$00\nSTA $8002\nLDA #$FF\nPHA\nRTI"))); // Insert a break instruction at $8002
        assert_eq!(regs.pc, 0x8003);
        assert_eq!(regs.sp, 0xFF);
        assert_eq!(regs.a, 0xFF);
        assert_eq!(regs.p, 0xFF);
    }

    #[test]
    fn test_branching() {
        for (op, neg, status) in [("BMI", "BPL", 0b1000_0000), ("BVS", "BVC", 0b0100_0000), ("BCS", "BCC", 0b0000_0001), ("BEQ", "BNE", 0b0000_0010)].iter() {
            for (op, status) in [(op, status), (neg, &!(*status as u8))].iter() {
                let bytecode = assemble(&format!("LDA #${status:02x}\nPHA\nPLP\n{op} $01\nBRK\nBRK", op=op, status=status));
                let (_, regs) = run(&bytecode);
                assert_eq!(regs.pc, 0x8000 + (bytecode.len() - 1) as u16);

                let bytecode = assemble(&format!("LDA #${status:02x}\nPHA\nPLP\n{op} $01\nBRK\nBRK", op=op, status=!(**status as u8)));
                let (_, regs) = run(&bytecode);
                assert_eq!(regs.pc, 0x8000 + (bytecode.len() - 2) as u16);
            }
        }
    }

    #[test]
    fn test_flags() {
        for (flag, status) in [("C", 0b0000_0001), ("I", 0b0000_0100), ("D", 0b0000_1000), ("V", 0b0100_0000)].iter() {
            let (_, regs) = run(&assemble(&format!("LDA #$FF\nPHA\nPLP\nCL{flag}", flag=flag)));
            assert_eq!(regs.p, !status);

            if *flag == "V" { continue; }  // SEV is not a valid instruction
            let (_, regs) = run(&assemble(&format!("LDA #$00\nPHA\nPLP\nSE{flag}", flag=flag)));
            assert_eq!(regs.p,  *status);
        }
    }
}