extern crate copperline;
mod lexer;
mod parser;
mod interpreter;
use copperline::Copperline;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let mut cl = Copperline::new();
    let mut args = env::args();
    args.next();
    let mut intrp = interpreter::Program::new();
    match args.next() {
        Some(filename) => {
            let mut f = File::open(filename).unwrap();
            let mut s = String::new();
            f.read_to_string(&mut s).unwrap();
            intrp.read(s);
        }
        None => {
            while let Ok(line) = cl.read_line(">", copperline::Encoding::Utf8) {
                println!(" ? {}", intrp.read(line.clone()));
                cl.add_history(line);
            }
        }
    };
}
