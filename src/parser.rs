use lexer::*;

/*
 * a recursive descent parser
 */
pub struct Parser {
    tok: Option<Token>,
    last: Option<Token>,
    lex: Lexer,
    ast: Vec<ASTNode>
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Def,
    Fun,
    Eq,
    Lt,
    Gt,
    Apply,
    When,
}

#[derive(Debug, Clone)]
pub enum ValueType {
    String,
    Symbol,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Value(String, ValueType),
    List(Vec<ASTNode>),
    BinaryOperation(BinaryOp, Box<ASTNode>, Box<ASTNode>),
}

impl Parser {
    pub fn new(program: String) -> Parser {
        Parser {
            tok: None,
            last: None,
            lex: Lexer::new(program),
            ast: Vec::new() 
        }
    }

    fn nextsym(&mut self) -> bool {
        self.last = self.tok.clone();
        self.tok = self.lex.get_next_symbol();
        self.tok.is_some()
    }

    fn accept(&mut self, t: Token) -> bool {
        if match t {
            Token::Symbol(_) => {
                match self.tok {
                    Some(Token::Symbol(_)) => true,
                    _ => false
                }
            },
            Token::Str(_) => {
                match self.tok {
                    Some(Token::Str(_)) => true,
                    _ => false
                }
            },
            _ => Some(t.clone()) == self.tok
        } {
            self.nextsym();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, t: Token) -> bool {
        if  self.accept(t.clone()) {
            true
        } else {
            panic!("unexpected symbol: got {:?}, expected {:?}", self.tok.clone(), t);
        }
    }

    // the most basic unit - a number, symbol, lambda, or paren expr
    fn factor(&mut self) -> Option<ASTNode> {
        if self.accept(Token::Lambda) {
            let mut args = Vec::new();
            while let Some(Token::Symbol(arg)) = self.tok.clone() {
                args.push(ASTNode::Value(arg.clone(), ValueType::Symbol));
                self.nextsym();
            }
            self.expect(Token::Seperator);
            Some(ASTNode::BinaryOperation(
                BinaryOp::Fun,
                Box::new(ASTNode::List(args)),
                Box::new(self.expr().unwrap())))
        }
        else if self.accept(Token::Symbol("any".to_string())) {
            let t = self.last.clone();
            match t {
                Some(Token::Symbol(s)) => Some(ASTNode::Value(s, ValueType::Symbol)),
                _ => None
            }
        }
        else if self.accept(Token::Str("any".to_string())) {
            let t = self.last.clone();
            match t {
                Some(Token::Str(s)) => Some(ASTNode::Value(s, ValueType::String)),
                _ => None
            }
        }
        else if self.accept(Token::OpenParen) {
            let e = self.expr();
            self.expect(Token::CloseParen);
            e
        } else if self.accept(Token::OpenList) {
            let mut list = Vec::new();
            while self.tok.clone() != Some(Token::CloseList) {
                list.push(self.expr().unwrap());
            }
            self.nextsym();
            Some(ASTNode::List(list.clone()))
        } else {
            panic!("not a factor: {:?}", self.tok.clone());
        }
    }

    // *, /
    fn term(&mut self) -> Option<ASTNode> {
        let lhs = self.factor();
        if self.tok == Some(Token::Star) || self.tok == Some(Token::Slash) {
            let op = match self.tok {
                Some(Token::Star) => BinaryOp::Mul,
                Some(Token::Slash) => BinaryOp::Div,
                _ => panic!("bad op")
            };
            self.nextsym();
            Some(ASTNode::BinaryOperation(op, Box::new(lhs.unwrap()), Box::new(self.term().unwrap())))
        } else {
            lhs
        }
    }

    // +, -, :=, :, =
    fn expr(&mut self) -> Option<ASTNode> {
        if self.tok == Some(Token::Plus) || self.tok == Some(Token::Minus) {
            self.nextsym();
        }
        let lhs = self.term().unwrap();
        // recursive descent parsers have problems with right associativity
        if self.tok == Some(Token::Plus) || self.tok == Some(Token::Minus) {
            let mut astbase: Option<ASTNode> = Some(lhs);
            while self.tok == Some(Token::Plus) || self.tok == Some(Token::Minus) {
                let baseas = astbase.clone();
                if self.accept(Token::Plus) {
                    astbase = Some(ASTNode::BinaryOperation(
                        BinaryOp::Add, Box::new(baseas.unwrap()), Box::new(self.term().unwrap())));
                } else if self.accept(Token::Minus) {
                    astbase = Some(ASTNode::BinaryOperation(
                        BinaryOp::Sub, Box::new(baseas.unwrap()), Box::new(self.term().unwrap())));
                }
            }
            return astbase;
        }
        if self.accept(Token::Assign) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::Def, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else if self.accept(Token::Seperator) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::Apply, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else if self.accept(Token::Equals) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::Eq, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else if self.accept(Token::Arrow) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::When, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else if self.accept(Token::LessThan) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::Lt, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else if self.accept(Token::GreaterThan) {
            Some(ASTNode::BinaryOperation(
                BinaryOp::Gt, Box::new(lhs), Box::new(self.expr().unwrap())))
        } else {
            Some(lhs)
        }
    }

    pub fn statement(&mut self) -> Option<ASTNode> {
        // lexer gives none for first token
        if self.tok == None {
            None
        } else {
            self.expr()
        }
    }

    pub fn parse(&mut self) {
        loop {
            self.nextsym();
            let st = self.statement();
            if st.is_some() {
                self.ast.push(st.unwrap());
            } else {
                break;
            }
        }
    }

    pub fn ast(&self) -> Vec<ASTNode> {
        self.ast.clone()
    }
}
            
        

    


