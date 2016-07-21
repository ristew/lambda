use parser::*;
use std::collections::BTreeMap;
use std::fmt;

pub type Environment = BTreeMap<String, ASTNode>;

#[derive(Debug, Clone)]
pub enum Atom {
    Num(isize),
    Sym(String),
    // lambda takes a closure over its environment
    Lambda(String, ASTNode, Environment)
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            Atom::Num(i) => write!(f, "{}", i),
            Atom::Sym(s) => write!(f, "{}", s),
            // thanks for otb support
            Atom::Lambda(s, _, _) => write!(f, "λ {}", s)
        }
    }
}
    

pub struct Program {
    env: Environment,
    ast: Vec<ASTNode>
}

impl Program {
    pub fn new() -> Program {
        Program {
            env: BTreeMap::new(),
            ast: Vec::new()
        }
    }

    // extract a value out of an ast node
    pub fn value_node(&mut self, node: ASTNode) -> Atom {
        match node {
            ASTNode::Value(val) => {
                match val.parse::<isize>() {
                    Ok(n) => Atom::Num(n),
                    Err(_) => {
                        if self.env.contains_key(&val) {
                            let binding = self.env.get(&val).unwrap().clone();
                            self.interpret_node(binding.clone())
                        } else {
                            Atom::Sym(val)
                        }
                    }
                }
            },
            _ => self.interpret_node(node)
        }
    }

    fn node_num(&mut self, node: ASTNode) -> isize {
        match self.interpret_node(node)  {
            Atom::Num(i) => i,
            _ => panic!("expected Num")
        }
    }

    fn node_sym(&mut self, node: ASTNode) -> String {
        match node {
            ASTNode::Value(s) => s,
            _ => panic!("failed to parse sym")
        }
    }

    fn op_node(&mut self, node: ASTNode) -> Atom {
        match node.clone() {
            ASTNode::BinaryOperation(op, lhs, rhs) => {
                match op {
                    BinaryOp::Add => Atom::Num(
                        self.node_num(*lhs) + self.node_num(*rhs)),
                    BinaryOp::Sub => Atom::Num(
                        self.node_num(*lhs) - self.node_num(*rhs)),
                    BinaryOp::Mul => Atom::Num(
                        self.node_num(*lhs) * self.node_num(*rhs)),
                    BinaryOp::Div => {
                        let lower = self.node_num(*rhs);
                        if lower == 0 {
                            // could panic too
                            Atom::Sym("undef".to_string())
                        } else {
                            Atom::Num(self.node_num(*lhs) / lower)
                        }
                    },
                    BinaryOp::Eq => {
                        if self.node_num(*lhs) == self.node_num(*rhs) {
                            // fuck you george
                            Atom::Num(1)
                        } else {
                            Atom::Num(0)
                        }
                    },
                    BinaryOp::Def => {
                        let sym = self.node_sym(*lhs);
                        self.env.insert(sym.clone(), *rhs.clone());
                        println!("env: {:?}", self.env.clone());
                        self.interpret_node(*rhs)
                    },
                    BinaryOp::Fun => Atom::Lambda(self.node_sym(*lhs),
                                                  *rhs, self.env.clone()),
                    BinaryOp::Apply => {
                        // lhs evaluates to a lambda, rhs is an arg
                        match self.interpret_node(*lhs.clone()) {
                            Atom::Lambda(arg, lam, env) =>
                                self.apply_lambda(lam, (arg, *rhs), env.clone()),
                            n => n
                        }
                    }
                }
            },
            _ => Atom::Num(0)
        }
    }

    fn apply_lambda(&mut self, node: ASTNode, arg: (String, ASTNode),
                    env: Environment) -> Atom {
        let oldenv = self.env.clone();
        self.env = env;
        let (name, val) = arg;
        self.env.insert(name, val);
        let result = self.interpret_node(node.clone());
        self.env = oldenv;
        result
    }

    pub fn interpret_node(&mut self, node: ASTNode) -> Atom {
        match node {
            ASTNode::BinaryOperation(_, _, _) => self.op_node(node),
            ASTNode::Value(_) => self.value_node(node),
        }
    }

    pub fn interpret(&mut self) -> Atom {
        let mut last = Atom::Num(0);
        for node in self.ast.clone() {
            last = self.interpret_node(node)
        }
        last
    }
    pub fn read(&mut self, line: String) -> Atom {
        let mut parsy = Parser::new(line);
        parsy.parse();
        self.ast = parsy.ast();
        self.interpret()
    }
}
    
