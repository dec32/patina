use std::borrow::Borrow;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;
use std::str::FromStr;
use crate::Result;
use crate::error::Error;
use crate::Func;
use crate::Node;
use crate::Type::{self, *};
use crate::Syntax::*;
use crate::BinOperator::*;
use crate::UnOperator::*;
use crate::Declarer::*;
use crate::Variable;


pub fn analyze(ast: Vec<Node>) -> Result<TypedAst> {
    let mut env = Env::new();
    env.analyze_program(ast);
    let errors = env.errors.borrow();
    if !errors.is_empty() {
        if errors.len() == 1 {
            Err(errors.first().unwrap().clone())
        } else {
            Err(Error::Multiple(errors.clone().into_iter().collect()))
        }
    } else {
        let typed_ast = TypedAst {
            structs: env.structs,
            funcs: env.funcs,
        };
        Ok(typed_ast)
    }   
}


pub struct TypedAst {
    pub structs: HashMap<&'static str, Structure>,
    pub funcs: HashMap<&'static str, Func>,
}


struct Env {
    // symbol table
    structs: HashMap<&'static str, Structure>,
    funcs: HashMap<&'static str, Func>,
    signatures: HashMap<&'static str, Signature>,
    variables: Variables,
    // error collector
    errors: RefCell<Vec<Error>>,
    // state for analyzing
    expected_return_type: Type,
    looping: bool,
}

#[derive(Clone)]
pub struct Structure {
    name: &'static str,
    fields: HashMap<&'static str, Variable>,
}

struct Signature {
    return_type: Type,
    params: Vec<Variable>
}

struct Variables {
    scopes: Vec<HashMap<&'static str, Variable>>, // todo support shadowing
    cur: isize,
}

impl Variables {
    fn new() -> Variables {
        Variables {
            scopes: Vec::new(),
            cur: -1,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.cur += 1;
    }
    
    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.cur -= 1;
    }

    fn insert(&mut self, name: &'static str, var: Variable) {
        self.scopes[self.cur as usize].insert(name, var);
    }

    fn get(&self, name: &str) -> Option<&Variable> {
        for scope in self.scopes.iter().rev() {
            let var = scope.get(name);
            if var.is_some() {
                return var;
            }
        }
        None
    }
}



impl Env {
    fn new() -> Env {
        Env{
            structs: HashMap::new(),
            funcs: HashMap::new(),
            signatures: HashMap::new(),
            variables: Variables::new(),
            errors: RefCell::new(Vec::new()),
            expected_return_type: Void.into(),
            looping: false
        }
    }

    fn analyze_program(&mut self, nodes: Vec<Node>) {
        let mut struct_names = HashSet::new();
        for node in nodes.iter() {
            let Struct(struct_syntax) = &*node.syntax else {
                continue;
            };
            struct_names.insert(struct_syntax.name);
        }

        let mut funcs_to_analyze = Vec::with_capacity(nodes.len());
        for node in nodes {
            let line_col = node.line_col;
            match *node.syntax {
                Struct(struct_syntax) => {
                    if self.structs.contains_key(&struct_syntax.name) {
                        self.report("struct already defined", node.line_col)
                    }
                    let mut structure = Structure {name: struct_syntax.name, fields: HashMap::new()};
                    for field in struct_syntax.fields {
                        if structure.fields.contains_key(&field.name) {
                            self.report("field already defined", field.line_col);
                            continue;
                        }
                        if let Defined(struct_name) = field.ty.value_type() {
                            if !struct_names.contains(struct_name) {
                                self.report("type of field is not defined", field.line_col)
                            }
                        }
                        // todo : field type?
                        structure.fields.insert(field.name, field);
                    }
                    if self.structs.contains_key(&struct_syntax.name) {
                        self.report("struct already defined", node.line_col);
                    } else {
                        self.structs.insert(structure.name, structure);
                    }
                },

                Fn(func) => {
                    let mut param_names = HashSet::new();
                    for param in func.params.iter() {
                        if !param_names.insert(param.name) {
                            self.report("parameter already defined", param.line_col)
                        }
                        if let Defined(struct_name) = param.ty.value_type() {
                            if !struct_names.contains(struct_name) {
                                self.report("type of parameter is not not defined", param.line_col)
                            }
                        }
                    }
                    if self.signatures.contains_key(&func.name) {
                        self.report("function already defined.", node.line_col)
                    } else {
                        let signature = Signature {
                            return_type: func.return_type.clone(),
                            params: func.params.clone()
                        };
                        self.signatures.insert(func.name, signature);
                    }
                    funcs_to_analyze.push(func)
                },

                Declare { declarer, variable: _, value:_ } => {
                    match declarer {
                        Let    => self.report("not allowed here", line_col),
                        Static => self.report("not implemented yet", line_col),
                        Const  => self.report("not implemented yet", line_col),
                    }
                }
                _ => unreachable!()
            }
        }

        if !self.signatures.contains_key("main") {
            // todo there's no line_col
            self.report("main function is not defined", (0, 0));
        }

        for func in funcs_to_analyze.iter_mut() {
            self.analyze_fn(func);
        }

        // storing results in env is hella weird
        for func in funcs_to_analyze {
            self.funcs.insert(func.name, func);
        }

    }

    fn analyze_fn(&mut self, func: &mut Func) {
        self.expected_return_type = func.return_type.clone();
        self.variables.push_scope();
        for var in self.signatures.get(&func.name).unwrap().params.iter() {
            self.variables.insert(var.name, var.clone())   
        }
        self.analyze_body(&mut func.body);
        self.variables.pop_scope();
    }


    fn analyze_body(&mut self, body: &mut Vec<Node>) {
        self.variables.push_scope();
        for node in body {
            let line_col = node.line_col;
            match node.syntax.borrow_mut() {
                Declare { declarer, variable, value } => {
                    match declarer {
                        Const | Static => self.report("not allowed here", line_col),
                        Let => ()
                    }
                    self.infer_type(value);
                    if variable.ty == Unknown {
                        if value.ty == Unknown {
                            self.report("can not infer the type of value", line_col);
                        } else if value.ty == Void {
                            self.report("can not assign void to variable", line_col);
                        } else {
                            variable.ty = value.ty.clone();
                        }
                    } else {
                        if !value.ty.is(&variable.ty) {
                            self.report("mismatched types", value.line_col);
                        }
                    }
                    if self.variables.get(&variable.name).is_some() {
                        self.report("variable already defined", line_col)
                    }
                    self.variables.insert(variable.name, variable.clone());
                },
                Assign { lvalue: target, value } => {
                    self.infer_type(target);
                    self.infer_type(value);
                    if !value.ty.is(&target.ty) {
                        self.report("mismatched type", value.line_col)
                    }
                },
                
                Break(value) => {
                    if !self.looping {
                        self.report("not inside a loop", line_col)
                    }
                    if let Some(value) = value {
                        self.infer_type(value);                        
                    }
                },
                Return(value) => {
                    if let Some(value) = value {
                        self.infer_type(value);
                        if !value.ty.is(&self.expected_return_type) {
                            self.report("wrong type for return value", line_col)
                        }
                    } else if self.expected_return_type != Void.into() {
                        self.report("return value is required", line_col)
                    }
                },

                Call{..} | If{..} | Loop{..} => self.infer_type(node),
                
                Fn(_) | Struct(_) => {
                    self.report("not allowed here", line_col)
                }

                _ => {
                    self.report("not a statement", line_col)
                }
            }
        }
        self.variables.pop_scope()
    }

    fn infer_type(&mut self, node: &mut Node) {
        let line_col = node.line_col;
        let type_: Type = match &mut *node.syntax {
            True => Bool.into(),
            False => Bool.into(),
            // type inference is hella difficult. I need to read some tutorials or papers....
            IntLiteral{value, type_suffix} => {
                let (overflow, value_type) = match type_suffix {
                    Some("u8")  => (u8::from_str_radix(value, 10).is_err(),  U8),
                    Some("u16") => (u16::from_str_radix(value, 10).is_err(), U16),
                    Some("u32") => (u32::from_str_radix(value, 10).is_err(), U32),
                    Some("u64") => (u64::from_str_radix(value, 10).is_err(), U64),
                    Some("i8")  => (i8::from_str_radix(value, 10).is_err(),  I8),
                    Some("i16") => (i16::from_str_radix(value, 10).is_err(), I16),
                    Some("i32") => (i32::from_str_radix(value, 10).is_err(), I32),
                    Some("i64") => (i64::from_str_radix(value, 10).is_err(), I64),
                    None        => (i64::from_str_radix(value, 10).is_err(), I32),
                    _ => unreachable!()
                };
                if overflow {
                    self.report("overflow", line_col)
                }
                value_type.into()
            }
            FloatLiteral{value, type_suffix} => {
                let (overflow, value_type) = match type_suffix {
                    Some("f32") => (f32::from_str(value).is_err(), F32),
                    Some("f64") => (f64::from_str(value).is_err(), F64),
                    None        => (f64::from_str(value).is_err(), F64),
                    _ => unreachable!()
                };
                if overflow {
                    self.report("overflow", line_col)
                }
                value_type.into()
            }
            StructLiteral{ name, entries } => {
                // FIXME: clone is needed because if here we immutably borrows self
                if let Some(s) = self.structs.get(name).cloned() {
                    let mut declared_fields = HashSet::new();
                    for entry in entries {
                        let field_type = {
                            let Some(field) = s.fields.get(entry.0) else {
                                self.report("struct has no suche field", line_col);
                                continue;
                            };
                            field.ty.clone()
                        };
                        if !s.fields.contains_key(entry.0) {
                            self.report("struct has no suche field", line_col);
                            continue;
                        }
                        if !declared_fields.insert(entry.0) {
                            self.report("duplicate fields", line_col);
                            continue;
                        }
                        // here we can't mutablly borrow self anymore
                        self.infer_type(&mut entry.1);
                        if entry.1.ty != field_type {
                            self.report("mismatched type", line_col);
                        }
                    }
                    Defined(s.name).into()
                } else {
                    self.report("type is not defined", line_col);
                    Void.into()
                }
                
            },
            Ident(name) => {
                if let Some(var) = self.variables.get(name) {
                    var.ty.clone()
                } else {
                    Void.into()
                }
            }
            Member(node, field) => {
                auto_deref(node);
                self.infer_type(node);
                match node.ty.borrow() {
                    Defined(name) => {
                        if let Some(structure) = self.structs.get(name) {
                            if let Some(field) = structure.fields.get(field) {
                                field.ty.clone()
                            } else {
                                self.report("no such field", line_col);
                                Void.into()
                            }
                        } else {
                            self.report("struct is not defined", line_col);
                            Void.into()
                        }
                    }
                    _ => {
                        self.report("primitive types don't have no fields", line_col);
                        Void.into()
                    }
                }
            }
            BinOp(op, left, right) => {
                self.infer_type(left);
                self.infer_type(right);
                auto_deref(left);
                auto_deref(right);
                match op {
                    Ge | Gt | Le | Lt => {
                        match Type::supertype(&left.ty, &right.ty) {
                            None => {
                                self.report("compairson between incompatitable types", line_col);
                            },
                            Some(supertype) => {
                                if !supertype.is_number() {
                                    self.report("compairson between non-numeric types", line_col);
                                }
                            }
                        }
                        Bool.into()
                    }
                    Ne | Eq => {
                        match Type::supertype(&left.ty, &right.ty) {
                            None => {
                                self.report("equality check between incompatitable types", line_col);
                            }
                            Some(supertype) => { 
                                if !supertype.is_primitive() {
                                    self.report("equality check between non-primitive types", line_col);
                                }
                            }
                        }
                        Bool.into()
                    }
                    And | Or => {
                        if left.ty != Bool {
                            self.report("logical operation for non-bool types", line_col);
                        }
                        if right.ty != Bool {
                            self.report("logical operation for non-bool types", line_col);
                        }
                        Bool.into()
                    }
                    Mul | Div | Add | Sub => {
                        match Type::supertype(&left.ty, &right.ty) {
                            None => if left.ty.is_number() {
                                self.report("numeric operation for non-numeric types", line_col);
                                left.ty.clone()
                            } else if right.ty.is_number() {
                                self.report("numeric operation for non-numeric types", line_col);
                                right.ty.clone()
                            } else {
                                self.report("numeric operation for non-numeric types", line_col);
                                I32.into()
                            },
                            Some(supertype) => {
                                if !supertype.is_number() {
                                    self.report("numeric operation for non-numeric types", line_col);
                                    I32.into()   
                                } else {
                                    supertype.clone().into()
                                }
                            }
                        }
                    }
                }
            },
            UnOp(op, right) => {
                self.infer_type(right);
                match op {
                    Neg => {
                        auto_deref(right);
                        if !right.ty.is_signed() {
                            self.report("not a signed number", line_col);
                        }
                        right.ty.clone()
                    },
                    Not => {
                        auto_deref(right);
                        if right.ty != Bool {
                            self.report("not a bool", line_col);
                        }
                        Bool.into()
                    },
                    SharedRef | UniqueRef | BorrowRef => {
                        if right.ty.is_ref() {
                            self.report("high-order reference is not allowed", line_col);
                        }
                        match op {
                            SharedRef => Shared(Box::new(right.ty.clone())),
                            BorrowRef => Borrow(Box::new(right.ty.clone())),
                            UniqueRef => Unique(Box::new(right.ty.clone())),
                            _ => unreachable!()
                        }
                    },
                    Deref => {
                        if !right.ty.is_ref() {
                            self.report("dereference on value type", line_col);
                        }
                        right.ty.value_type()
                    },
                }
            }
            Call { func, args } => {
                if let Some(signature) = self.signatures.get(func) {
                    if args.len() != signature.params.len() {
                        self.report("number of arguments does not match with number of parameters", line_col);
                    } else {
                        for i in 0..args.len() {
                            if args[i].ty != signature.params[i].ty {
                                self.report("type of arguments does not match with type of parameters", line_col);
                            }
                        }
                    }
                    signature.return_type.clone()
                } else {
                    self.report("function not defined", line_col);
                    Void.into()
                }
            },
            If { branches, default } => {
                for branch in branches {
                    self.infer_type(&mut branch.predicate);
                    if branch.predicate.ty != Bool.into() {
                        self.report("bool is required for condition", line_col);
                    }
                    self.analyze_body(&mut branch.body);
                }
                self.analyze_body(default);
                Void.into()
            },
            Loop { body } => {
                let outer_loop = !self.looping;
                self.looping = true;
                self.analyze_body(body);
                if outer_loop {
                    self.looping = false;
                }
                Void.into()
            },
            _ => unreachable!()
        };
        node.ty = type_
    }


    fn report(&self, msg: &str, line_col: (usize, usize)) {
        let error = Error::General(msg.into(), line_col);
        self.errors.borrow_mut().push(error)
    }
}


fn auto_deref(node: &mut Node) {
    if !node.ty.is_ref() {
        return;
    }
    // fixme use some unsafe magic to get rid of the needless Box::new()
    let child_node = Node { 
        syntax: mem::replace(&mut node.syntax, Box::new(Moved)), 
        line_col: node.line_col,
        ty: node.ty.clone()
    };
    node.ty = child_node.ty.value_type();
    node.syntax = Box::new(UnOp(Deref, child_node));
}

