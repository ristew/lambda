use parser::*;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    Num(isize),
    Sym(String),
    List(Vec<Atom>),
    Builtin(String, fn(Atom) -> Atom),
    // lambda takes a closure over its environment
    Lambda(Box<Atom>, ASTNode)
}

impl Atom {
    fn string(&self) -> Option<String> {
        match self.clone() {
            Atom::Sym(s) => Some(s),
            _ => None
        }
    }
    fn node_val(&self) -> Option<ASTNode> {
        match self.clone() {
            Atom::Sym(s) => Some(ASTNode::Value(s, ValueType::Symbol)),
            Atom::Num(n) => Some(ASTNode::Value(format!("{}", n), ValueType::Symbol)),
            Atom::List(l) => {
                let mut lnew = Vec::new();
                for i in l {
                    lnew.push(i.node_val().unwrap());
                }
                Some(ASTNode::List(lnew))
            },
            _ => None
        }
    }
}

pub type Environment = BTreeMap<String, Atom>;

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            Atom::Nil => write!(f, "nil"),
            Atom::Builtin(name, _) => write!(f, "builtin {}", name),
            Atom::Num(i) => write!(f, "{}", i),
            Atom::Sym(s) => write!(f, "{}", s),
            // thanks for otb support
            Atom::Lambda(_, _) => write!(f, "λ"),
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
        let mut p = Program {
            env: BTreeMap::new(),
            ast: Vec::new()
        };
        p.add_builtin("print", Builtin::println);
        p.add_builtin("fmt", Builtin::fmt);
        p.add_builtin("debug", Builtin::debug);
        p.add_builtin("cons", Builtin::cons);
        p.add_builtin("cdr", Builtin::cdr);
        p.add_builtin("car", Builtin::car);
        p
    }

    fn add_builtin(&mut self, name: &str, fun: fn(Atom) -> Atom) {
        self.env.insert(name.to_string(), Atom::Builtin(name.to_string(), fun));
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
                                if val == "nil" {
                                    Atom::Nil
                                } else if self.env.contains_key(&val) {
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

    fn node_truth(&self, atom: Atom) -> bool {
        match atom {
            Atom::Nil => false,
            Atom::Num(0) => false,
            _ => true
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
            (x, y) => panic!("bad add: {:?} + {:?}", x, y)
        }
    }

        

    fn eq(&mut self, lhs: ASTNode, rhs: ASTNode) -> Atom {
        match (self.interpret_node(lhs), self.interpret_node(rhs)) {
            (Atom::Num(x), Atom::Num(y)) => {
                if x == y {
                    Atom::Num(1)
                } else {
                    Atom::Nil
                }
            },
            (Atom::Sym(x), Atom::Sym(y)) => {
                if x == y {
                    Atom::Num(1)
                } else {
                    Atom::Nil
                }
            },
            (Atom::Nil, Atom::Nil) => Atom::Num(1),
            (_, _) => Atom::Nil
        }
    }

    fn div(&mut self, lhs: ASTNode, rhs: ASTNode) -> Atom {
        match self.interpret_node(lhs.clone()) {
            Atom::List(l) => {
                let idx = self.node_num(rhs);
                let udx = if idx < 0 {
                    l.len() - (-idx as usize)
                } else {
                    idx as usize
                };
                l.get(udx).unwrap_or(&Atom::Nil).clone()
            },
            Atom::Num(n) => Atom::Num(n / self.node_num(rhs)),
            Atom::Sym(s) => {
                let mut retstr = String::new();
                retstr.push(s.chars().nth(self.node_num(rhs) as usize).unwrap());
                Atom::Sym(retstr)
            },
            _ => panic!("bad div {:?}", lhs)
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
                            Atom::Nil
                        }
                    },
                    BinaryOp::Gt => {
                        if self.node_num(*lhs) > self.node_num(*rhs) {
                            Atom::Num(1)
                        } else {
                            Atom::Nil
                        }
                    },
                    BinaryOp::Def => {
                        let sym = self.node_sym(*lhs);
                        let expanded = self.interpret_node(*rhs.clone());
                        self.env.insert(sym.clone(), expanded.clone());
                        //self.interpret_node(*rhs)
                        expanded
                    },
                    BinaryOp::Xor => {
                        let lhint = self.interpret_node(*lhs);
                        let rhint = self.interpret_node(*rhs);
                        if self.node_truth(lhint) ^ self.node_truth(rhint) {
                            Atom::Num(1)
                        } else {
                            Atom::Nil
                        }
                    },
                    BinaryOp::Fun => Atom::Lambda(Box::new(self.literal_list_node(*lhs)),
                                                  *rhs),
                    BinaryOp::Apply => {
                        // lhs evaluates to a lambda, rhs is an arg
                        match self.interpret_node(*lhs.clone()) {
                            Atom::Lambda(args, lam) => {
                                let expanded = self.interpret_node(*rhs);
                                self.apply_lambda(lam, *args, expanded)
                            }
                            Atom::Builtin(_, fun) => {
                                fun(self.interpret_node(*rhs))
                            },
                            n => n
                        }
                    }
                }
            },
            _ => Atom::Num(0)
        }
    }

    fn replace(node: ASTNode, sym: String, val: Atom) -> ASTNode {
        match node.clone() {
            ASTNode::Value(s, ValueType::Symbol) => {
                if s == sym {
                    //println!("replacing {} with {:?}", s, val.clone());
                    ASTNode::Literal(Box::new(val))
                } else {
                    node.clone()
                }
            },

            ASTNode::BinaryOperation(op, lhs, rhs) =>
                ASTNode::BinaryOperation(op,
                                         Box::new(Program::replace(*lhs, sym.clone(), val.clone())),
                                         Box::new(Program::replace(*rhs, sym.clone(), val.clone()))),
            ASTNode::TernaryOperation(op, lhs, rhs, ohs) => 
                ASTNode::TernaryOperation(op,
                                         Box::new(Program::replace(*lhs, sym.clone(), val.clone())),
                                         Box::new(Program::replace(*rhs, sym.clone(), val.clone())),
                                         Box::new(Program::replace(*ohs, sym.clone(), val.clone()))),
            ASTNode::List(l) => {
                let mut rl = Vec::new();
                for item in l {
                    rl.push(Program::replace(item.clone(), sym.clone(), val.clone()));
                }
                ASTNode::List(rl)
            }, 
            ASTNode::Block(l) => {
                let mut rl = Vec::new();
                for item in l {
                    rl.push(Program::replace(item.clone(), sym.clone(), val.clone()));
                }
                ASTNode::Block(rl)
            }, 
            n => n
        }
    }

    fn apply_lambda(&mut self, node: ASTNode, argnames: Atom, args: Atom) -> Atom {
        let oldenv = self.env.clone();
        let namelist = self.list_val(argnames);
        let arglist = self.list_val(args);
        let mut prog = node.clone();
        for i in 0..namelist.len() {
            let name = namelist[i].clone();
            let val = arglist[i].clone();
            prog = Program::replace(prog, name.string().unwrap(), val);
        }
        let result = self.interpret_node(prog.clone());
        for i in 0..namelist.len() {
            let name = namelist[i].clone();
            let val = oldenv.get(&name.string().unwrap());
            if val.is_some() {
                self.env.insert(name.string().unwrap(), val.unwrap().clone());
            }
        }
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

    fn block_node(&mut self, node: ASTNode) -> Atom {
        let mut last = Atom::Nil;
        match node {
            ASTNode::Block(l) => {
                for item in l {
                    last = self.interpret_node(item);
                }
            },
            _ => {}
        };
        last
    }
            

    fn literal_list_node(&mut self, node: ASTNode) -> Atom {
        let mut alist = Vec::new();
        match node {
            ASTNode::List(l) => {
                for item in l {
                    alist.push(Atom::Sym(self.node_sym(item)));
                }
            },
            _ => {}
        };
        Atom::List(alist)
    }

    pub fn interpret_node(&mut self, node: ASTNode) -> Atom {
        //println!("node :{:?}", node.clone());
        match node {
            ASTNode::BinaryOperation(_, _, _) => self.op_node(node),
            ASTNode::TernaryOperation(TernaryOp::IfElse, cond, lhs, rhs) => {
                let truth  = self.interpret_node(*cond);
                if self.node_truth(truth) {
                    self.interpret_node(*lhs)
                } else {
                    self.interpret_node(*rhs)
                }
            },
            ASTNode::Value(_, _) => self.value_node(node),
            ASTNode::List(_) => self.list_node(node),
            ASTNode::Block(_) => self.block_node(node),
            ASTNode::Literal(a) => *a.clone()
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
        let a = self.interpret();
        //println!("ast: {:?}", a);
        a
    }
}
    
struct Builtin;

impl Builtin {
    fn println(atom: Atom) -> Atom {
        println!("{}", atom);
        Atom::Nil
    }

    fn fmt(atom: Atom) -> Atom {
        Atom::Sym(format!("{}", atom))
    }

    fn debug(atom: Atom) -> Atom {
        println!("{:?}", atom);
        Atom::Nil
    }
    fn cons(atom: Atom) -> Atom {
        if let Atom::List(l) = atom {
            if let Atom::List(mut l2) = l[1].clone() {
                l2.insert(0, l[0].clone());
                Atom::List(l2)
            } else {
                Atom::Nil
            }
        } else {
            Atom::Nil
        }
    }
    fn car(atom: Atom) -> Atom {
        if let Atom::List(l) = atom {
            l.get(0).unwrap_or(&Atom::Nil).clone()
        } else {
            Atom::Nil
        }
    }
        
    fn cdr(atom: Atom) -> Atom {
        if let Atom::List(l) = atom {
            let mut l2 = l.clone();
            if l2.len() > 0 {
                l2.remove(0);
            }
            Atom::List(l2)
        } else {
            Atom::Nil
        }
    }
}
