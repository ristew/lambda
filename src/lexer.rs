#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Lambda,
    Seperator,
    Assign,
    Equals,
    OpenParen,
    CloseParen,
    OpenList,
    CloseList,
    OpenBlock,
    CloseBlock,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Star,
    Slash,
    Arrow,
    Tilde,
    Symbol(String),
    Str(String),
}

// we use these
fn reserved_chars() -> Vec<char> {
    vec!('{', '}', '[', ']', '<', '>', '\\', '/', ':', '(', ')', '*', '+', '=', '-', ' ', '\n')
}
    

#[derive(Debug)]
pub struct Lexer {
    program: String,
    position: usize,
}

impl Lexer {
    pub fn new(program: String) -> Lexer {
        Lexer {
            program: program,
            position: 0
        }
    }

    fn get_next_char(&mut self) -> Option<char> {
        self.position += 1;
        if self.position > self.program.len() {
            None
        } else {
            self.program.chars().nth(self.position - 1)
        }
    }

    pub fn get_next_symbol(&mut self) -> Option<Token> {
        match self.get_next_char() {
            // diregard multiple spaces
            Some(' ') => self.get_next_symbol(),
            Some('\\') => Some(Token::Lambda),
            Some(':') => {
                match self.get_next_char() {
                    // :=
                    Some('=') => Some(Token::Assign),
                    _ => {
                        self.position -= 1;
                        Some(Token::Seperator)
                    }
                }
            },
            Some('-') => {
                match self.get_next_char() {
                    // -> 
                    Some('>') => Some(Token::Arrow),
                    Some(' ') => Some(Token::Minus),
                    Some(c) => {
                        let mut symbol = String::new();
                        symbol.push('-');
                        symbol.push(c);
                        loop {
                            let nc = self.get_next_char();
                            if !nc.is_some() || reserved_chars().contains(&nc.unwrap()) {
                                self.position -= 1; 
                                break;
                            }
                            symbol.push(nc.unwrap());
                        }
                        Some(Token::Symbol(symbol))
                    },
                    _ => None,
                }
            },
            Some('"') => {
                let mut string = String::new();
                while let Some(c) = self.get_next_char() {
                    if c == '"' {
                        break;
                    }
                    string.push(c);
                }
                Some(Token::Str(string))
            },
            Some('~') => Some(Token::Tilde),
            Some('(') => Some(Token::OpenParen),
            Some(')') => Some(Token::CloseParen),
            Some('[') => Some(Token::OpenList),
            Some(']') => Some(Token::CloseList),
            Some('{') => Some(Token::OpenBlock),
            Some('}') => Some(Token::CloseBlock),
            Some('<') => Some(Token::LessThan),
            Some('>') => Some(Token::GreaterThan),
            Some('+') => Some(Token::Plus),
            Some('*') => Some(Token::Star),
            Some('/') => Some(Token::Slash),
            Some('=') => Some(Token::Equals),
            Some('\n') => self.get_next_symbol(), 
            Some('\t') => self.get_next_symbol(), 
            Some(c) => {
                let mut symbol = String::new();
                symbol.push(c);
                loop {
                    let nc = self.get_next_char();
                    if !nc.is_some() || reserved_chars().contains(&nc.unwrap()) {
                        self.position -= 1; 
                        break;
                    }
                    symbol.push(nc.unwrap());
                }
                Some(Token::Symbol(symbol))
            },
            None => None
        }
    }
}
