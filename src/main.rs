mod assemble;
mod run;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
    } else {
        let bytecode = assemble::assemble(&args[1]);
        run::run(bytecode);
    }
}
