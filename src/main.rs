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
    use super::run::run;

    #[test]
    fn test_branching() {
        for (op, neg, status) in [("BMI", "BPL", 0b1000_0000), ("BVS", "BVC", 0b0100_0000), ("BCS", "BCC", 0b0000_0001), ("BEQ", "BNE", 0b0000_0010)].iter() {
            for (op, status) in [(op, status), (neg, &!(*status as u8))].iter() {
                let bytecode = assemble(&format!("LDA #${status:02x}\nPHA\nPLP\n{op} $01\nBRK\nBRK", op=op, status=status));
                let (_, registers) = run(&bytecode);
                assert_eq!(registers.pc, 0x8000 + (bytecode.len() - 1) as u16);

                let bytecode = assemble(&format!("LDA #${status:02x}\nPHA\nPLP\n{op} $01\nBRK\nBRK", op=op, status=!(**status as u8)));
                let (_, registers) = run(&bytecode);
                assert_eq!(registers.pc, 0x8000 + (bytecode.len() - 2) as u16);
            }
        }
    }

    #[test]
    fn test_flags() {
        for (flag, status) in [("C", 0b0000_0001), ("I", 0b0000_0100), ("D", 0b0000_1000), ("V", 0b0100_0000)].iter() {
            let (_, registers) = run(&assemble(&format!("LDA #$FF\nPHA\nPLP\nCL{flag}", flag=flag)));
            assert_eq!(registers.p, !status);

            if *flag == "V" { continue; }  // SEV is not a valid instruction
            let (_, registers) = run(&assemble(&format!("LDA #$00\nPHA\nPLP\nSE{flag}", flag=flag)));
            assert_eq!(registers.p,  *status);
        }
    }
}