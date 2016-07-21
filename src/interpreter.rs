use parser::*;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Atom {
    Num(isize),
    Sym(String),
    List(Vec<Atom>),
    // lambda takes a closure over its environment
    Lambda(Box<Atom>, ASTNode, Environment)
}

impl Atom {
    fn string(&self) -> Option<String> {
        match self.clone() {
            Atom::Sym(s) => Some(s),
            _ => None
        }
    }
}

pub type Environment = BTreeMap<String, Atom>;

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            Atom::Num(i) => write!(f, "{}", i),
            Atom::Sym(s) => write!(f, "{}", s),
            // thanks for otb support
            Atom::Lambda(_, _, _) => write!(f, "λ"),
            Atom::List(l) => {
                write!(f, "[").unwrap();
                for i in l {
                    write!(f, " {}", i).unwrap();
                }
                write!(f, " ]")
            }
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
            ASTNode::Value(val, typ) => {
                match typ {
                    ValueType::Symbol => {
                        match val.parse::<isize>() {
                            Ok(n) => Atom::Num(n),
                            Err(_) => {
                                if self.env.contains_key(&val) {
                                    let binding = self.env.get(&val).unwrap().clone();
                                    binding
                                } else {
                                    Atom::Sym(val)
                                }
                            }
                        }
                    },
                    ValueType::String => {
                        Atom::Sym(val)
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
            ASTNode::Value(s, _) => s,
            _ => panic!("failed to parse sym")
        }
    }

    fn list_val(&self, atom: Atom) -> Vec<Atom> {
        match atom {
            Atom::List(l) => l,
            n => vec!(n),
        }
    }

    fn add(&mut self, lhs: ASTNode, rhs: ASTNode) -> Atom {
        match (self.interpret_node(lhs.clone()), self.interpret_node(rhs.clone())) {
            (Atom::Num(x), Atom::Num(y)) => Atom::Num(x + y),
            (Atom::Sym(x), Atom::Sym(y)) => {
                Atom::Sym(format!("{}{}", x, y))
            },
            (Atom::List(v), Atom::List(w)) => {
                let mut vc = v.clone();
                let mut wc = w.clone();
                vc.append(&mut wc);
                Atom::List(vc)
            },
            (x, y) => panic!("bad add: {} + {}", x, y)
        }
    }

    fn eq(&mut self, lhs: ASTNode, rhs: ASTNode) -> Atom {
        match (self.interpret_node(lhs), self.interpret_node(rhs)) {
            (Atom::Num(x), Atom::Num(y)) => Atom::Num((x == y) as isize),
            (Atom::Sym(x), Atom::Sym(y)) => Atom::Num((x == y) as isize),
            (_, _) => panic!("bad eq")
        }
    }

    fn div(&mut self, lhs: ASTNode, rhs: ASTNode) -> Atom {
        match self.interpret_node(lhs) {
            Atom::List(l) => l.get(self.node_num(rhs) as usize).unwrap().clone(),
            Atom::Num(n) => Atom::Num(n / self.node_num(rhs)),
            Atom::Sym(s) => {
                let mut retstr = String::new();
                retstr.push(s.chars().nth(self.node_num(rhs) as usize).unwrap());
                Atom::Sym(retstr)
            },
            _ => panic!("bad div")
        }
    }

    fn op_node(&mut self, node: ASTNode) -> Atom {
        match node.clone() {
            ASTNode::BinaryOperation(op, lhs, rhs) => {
                match op {
                    BinaryOp::Add => self.add(*lhs, *rhs),
                    BinaryOp::Sub => Atom::Num(
                        self.node_num(*lhs) - self.node_num(*rhs)),
                    BinaryOp::Mul => Atom::Num(
                        self.node_num(*lhs) * self.node_num(*rhs)),
                    BinaryOp::Div => self.div(*lhs, *rhs),
                    BinaryOp::Eq => self.eq(*lhs, *rhs),
                    BinaryOp::Lt => {
                        if self.node_num(*lhs) < self.node_num(*rhs) {
                            Atom::Num(1)
                        } else {
                            Atom::Num(0)
                        }
                    },
                    BinaryOp::Gt => {
                        if self.node_num(*lhs) > self.node_num(*rhs) {
                            Atom::Num(1)
                        } else {
                            Atom::Num(0)
                        }
                    },
                    BinaryOp::Def => {
                        let sym = self.node_sym(*lhs);
                        let expanded = self.interpret_node(*rhs.clone());
                        self.env.insert(sym.clone(), expanded);
                        //self.interpret_node(*rhs)
                        Atom::Sym(sym)
                    },
                    BinaryOp::When => {
                        if self.node_num(*lhs) != 0 {
                            self.interpret_node(*rhs)
                        } else {
                            Atom::Num(0)
                        }
                    },
                    BinaryOp::Fun => Atom::Lambda(Box::new(self.list_node(*lhs)),
                                                  *rhs, self.env.clone()),
                    BinaryOp::Apply => {
                        // lhs evaluates to a lambda, rhs is an arg
                        match self.interpret_node(*lhs.clone()) {
                            Atom::Lambda(args, lam, env) => {
                                let expanded = self.interpret_node(*rhs);
                                self.apply_lambda(lam, *args, expanded, env.clone())
                            }
                            n => n
                        }
                    }
                }
            },
            _ => Atom::Num(0)
        }
    }

    fn apply_lambda(&mut self, node: ASTNode, argnames: Atom, args: Atom,
                    env: Environment) -> Atom {
        let oldenv = self.env.clone();
        self.env = env;
        let namelist = self.list_val(argnames);
        let arglist = self.list_val(args);
        for i in 0..namelist.len() {
            let name = namelist[i].clone();
            let val = arglist[i].clone();
            self.env.insert(name.string().unwrap(), val);
        }
        let result = self.interpret_node(node.clone());
        self.env = oldenv;
        result
    }

    fn list_node(&mut self, node: ASTNode) -> Atom {
        let mut alist = Vec::new();
        match node {
            ASTNode::List(l) => {
                for item in l {
                    alist.push(self.interpret_node(item));
                }
            },
            _ => {}
        };
        Atom::List(alist)
    }

    pub fn interpret_node(&mut self, node: ASTNode) -> Atom {
        match node {
            ASTNode::BinaryOperation(_, _, _) => self.op_node(node),
            ASTNode::Value(_, _) => self.value_node(node),
            ASTNode::List(_) => self.list_node(node)
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
    
