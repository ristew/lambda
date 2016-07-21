extern crate copperline;
mod lexer;
mod parser;
mod interpreter;
use copperline::Copperline;

fn main() {
    let mut cl = Copperline::new();
    let mut intrp = interpreter::Program::new();
    while let Ok(line) = cl.read_line(">", copperline::Encoding::Utf8) {
        println!(" ? {}", intrp.read(line.clone()));
        cl.add_history(line);
    }
}
