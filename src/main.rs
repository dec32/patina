mod syntax;
mod semantic;
mod ast;
mod test;
mod error;
use std::{env, fs, path::Path};
use error::Error;

pub type Result<T> = std::result::Result<T, Error>;

fn main(){
    let Some(path) = env::args().nth(1) else {
        println!("Path is needed.");
        return
    };
    let path = Path::new(&path);
    let src: &'static str = fs::read_to_string(path).unwrap().leak(); // mem leak goes brrrr bye bye lifetime lol
    if let Err(e) = compile(&src) {
        println!("{:?}", e)
    }
}

fn compile(src: &'static str) -> Result<()>{
    // lexical and syntax analysis
    let ast = syntax::parse(src)?;
    let tast = semantic::analyze(ast)?;
    Ok(())
}

#[derive(Debug)]
pub struct Node {
    syntax: Box<Syntax>,
    line_col: (usize, usize),
    ty: Type
}

#[derive(Debug)]
pub enum Syntax {
    // the top-level elements
    Fn (Func),
    Struct (StructSyntax),
    Declare { declarer: Declarer, variable: Variable, value: Node },
    // statements
    Assign { lvalue: Node, value: Node },
    Return(Option<Node>),
    Break(Option<Node>),
    // statements that can also be used as expressions in some cases
    If { branches: Vec<Branch>, default: Vec<Node> },
    Loop { body: Vec<Node> },
    Call { func: &'static str, args: Vec<Node> },
    // expressions
    True,
    False,
    IntLiteral {
        value: &'static str,
        type_suffix: Option<&'static str>,
    },
    FloatLiteral{
        value: &'static str,
        type_suffix: Option<&'static str>,
    },
    StructLiteral {
        name: &'static str,
        entries: Vec<(&'static str, Node)>
    },
    Member(Node, &'static str),
    Ident(&'static str),
    BinOp(BinOperator, Node, Node),
    UnOp(UnOperator, Node),
    // don't even ask why
    Moved,
}

impl Syntax {
    fn at(self, line_col:(usize, usize)) -> Node {
        Node {
            syntax: self.into(),
            line_col,
            ty: Type::Unknown
        }

    }    
}

#[derive(Debug)]
pub enum BinOperator{ Ge, Gt, Le, Lt, Mul, Div, Add, Sub, Ne, Eq, And, Or }

#[derive(Debug)]
pub enum UnOperator{ Neg, Deref, Not, SharedRef, UniqueRef, BorrowRef }

#[derive(Debug)]
pub struct Func {
    name: &'static str, 
    params: Vec<Variable>, 
    return_type: Type,
    body: Vec<Node>
}

#[derive(Debug)]
pub struct StructSyntax {
    name: &'static str,
    fields: Vec<Variable>,
}

#[derive(Debug)]
pub enum Declarer { Let, Static, Const }
impl Declarer {
    #[allow(unused)]
    fn as_str(&self) -> &str {
        use Declarer::*;
        match self {
            Const  => "const",
            Static => "static",
            Let    => "let",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: &'static str,
    ty: Type,
    line_col: (usize, usize)
}

#[derive(Debug)]
pub struct Branch {
    predicate: Node,
    body: Vec<Node>
}


#[derive(Debug, PartialEq, Eq, Clone)]
enum Type {
    Unknown,
    Void, Bool,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    Borrow(Box<Type>),
    Unique(Box<Type>), 
    Shared(Box<Type>),
    Defined(&'static str), 
}

impl Type {
    fn is(&self, other: &Type) -> bool {
        use Type::*;
        match (self, other) {
            (U8,  U8 | U16 | U32 | U64) => true,
            (U16,      U16 | U32 | U64) => true,
            (U32,            U32 | U64) => true,
            (U64,                  U64) => true,
            (I8,  I8 | I16 | I32 | I64) => true,
            (I16,      I16 | I32 | I64) => true,
            (I32,            I32 | I64) => true,
            (I64,                  I64) => true,
            (F32,            F32 | F64) => true,
            (F64,                  F64) => true,
            (Defined(s), Defined(o)) => s == o,
            (Borrow(s), Borrow(o)) => s.is(o),
            (Unique(s), Unique(o)) => s.is(o),
            (Shared(s), Shared(o)) => s.is(o),
            _ => false,
        }
    }

    fn supertype(t0: &Type, t1: &Type) -> Option<Type> {
        use Type::*;
        match (t0, t1) {
            (U8,  U8 | U16 | U32 | U64) => Some(t1.clone()),
            (U16,      U16 | U32 | U64) => Some(t1.clone()),
            (U32,            U32 | U64) => Some(t1.clone()),
            (U64,                  U64) => Some(t1.clone()),
            (I8,  I8 | I16 | I32 | I64) => Some(t1.clone()),
            (I16,      I16 | I32 | I64) => Some(t1.clone()),
            (I32,            I32 | I64) => Some(t1.clone()),
            (I64,                  I64) => Some(t1.clone()),
            (F32,            F32 | F64) => Some(t1.clone()),
            (F64,                  F64) => Some(t1.clone()),
            (Defined(s0), Defined(s1)) => if s0 == s1 { Some(t1.clone()) } else { None },
            (Borrow(t0), Borrow(t1)) => Type::supertype(t0, t1).map(|ty|Borrow(Box::new(ty))),
            (Unique(t0), Unique(t1)) => Type::supertype(t0, t1).map(|ty|Unique(Box::new(ty))),
            (Shared(t0), Shared(t1)) => Type::supertype(t0, t1).map(|ty|Shared(Box::new(ty))),
            _ => None,
        }

    }

    fn is_ref(&self) -> bool {
        use Type::*;
        matches!(self, Borrow(_) | Unique(_) | Shared(_))
    }

    fn is_primitive(&self) -> bool {
        use Type::*;
        matches!(self, Void | Bool | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F32 | F64)

    }
    fn is_number(&self) -> bool {
        use Type::*;
        matches!(self, U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F32 | F64)
    }

    fn is_signed(&self) -> bool {
        use Type::*;
        matches!(self, I8 | I16 | I32 | I64 | F32 | F64)
    }

    fn value_type(&self) -> Type {
        use Type::*;
        match self {
            Borrow(ty) | Unique(ty) | Shared(ty) => *ty.clone(),
            other => other.clone()
        }
    }
}

impl From<&'static str> for Type {
    fn from(name: &'static str) -> Self {
        match name {
            "void" => Type::Void,
            "bool" => Type::Bool,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            defined => Type::Defined(defined.into())
        }
    }
}

















