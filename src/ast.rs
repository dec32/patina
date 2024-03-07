use crate::BinOperator;
use crate::Node;
use crate::Syntax::*;
use crate::UnOperator::*;
use crate::BinOperator::*;
use crate::Type;
use crate::UnOperator;
use crate::Variable;

pub fn display(nodes: &Vec<Node>) {
    for node in nodes {
        display_node(node, 0);
        println!()
    }
}

fn display_node(node: &Node, identation: u8) {
    match node.syntax.as_ref() {
        Fn(func) => {
            print!("(fn {}", func.name);
            print!(" (");
            let mut first = true;
            for param in func.params.iter() {
                if first {
                    first = false
                } else {
                    print!(" ")
                }
                print!("{}", param.as_str())
            }
            print!(")");
            print!(" {}", func.return_type.as_str());
            display_nodes(&func.body, identation + 1);
            print!(")")
        },
        Struct(struct_syntax) => {
            print!("(struct {}", struct_syntax.name);
            for field in struct_syntax.fields.iter() {
                println!();
                print_identation(identation + 1);
                print!("{}", field.as_str())
            }
            print!(")")
        },
        Declare { declarer, variable, value } => {
            print!("({} {} ", declarer.as_str(), variable.as_str());
            display_node(value, identation);
            print!(")")
        },
        Assign { lvalue, value } => {
            print!("(assign ");
            display_node(lvalue, identation);
            print!(" ");
            display_node(value, identation);
            print!(")")
        },
        Return(value) => {
            print!("(return ");
            if let Some(value) = value {
                display_node(value, identation)
            }
            print!(")");
        },
        Break(value) => {
            print!("(break ");
            if let Some(value) = value {
                display_node(value, identation)
            }
            print!(")");
        },
        If { branches, default } => {
            // todo when cond is used as expressions identation is not needed
            print!("(if ");
            for branch in branches {
                println!();
                print_identation(identation + 1);
                print!("[");
                display_node(&branch.predicate, identation + 1);
                display_nodes(&branch.body, identation + 2);
                print!("]");
            }
            display_nodes(default, identation + 1);
            print!(")");
        },
        Loop { body } => {
            print!("(loop ");
            display_nodes(body, identation + 1);
            print!(")");
        },
        Call { func, args } => {
            print!("({}", func);
            for arg in args {
                print!(" ");
                display_node(arg, identation)
            }
            print!(")")
        },
        True => print!("true"),
        False => print!("false"),
        IntLiteral { value, type_suffix: _ } => print!("{}", value),
        FloatLiteral{ value, type_suffix:_ } => print!("{}", value),
        StructLiteral { name, entries } => {
            print!("({}", name);
            for entry in entries {
                println!();
                print_identation(identation + 1);
                print!("({} ", entry.0);
                display_node(&entry.1, identation + 1);
                print!(")")
            }
        },
        Member(node, member) => {
            display_node(node, identation);
            print!(".{}", member);
        },
        Ident(name) => print!("{}", name),
        BinOp(op, left, right) => {
            print!("({} ", op.as_str());
            display_node(left, identation);
            print!(" ");
            display_node(right, identation);
            print!(")");
        },
        UnOp(op, right) => {
            print!("({} ", op.as_str());
            display_node(right, identation);
            print!(")");
        },
        Moved => unreachable!(),
    }

    if node.ty != Type::Unknown {
        print!(":{}", node.ty.as_str())
    }
}

fn display_nodes(nodes: &Vec<Node>, identation: u8) {
    for node in nodes.iter() {
        println!();
        print_identation(identation);
        display_node(node, identation)
    }
}

fn print_identation(identation: u8) {
    for _ in 0..identation {
        print!("  ")
    }
}


impl Variable {
    fn as_str(&self) -> &str {
        if self.ty == Type::Unknown {
            self.name
        } else {
            format!("{}:{}", self.name, self.ty.as_str()).leak()
        }
    }
}

impl Type {
    fn as_str(&self) -> &str {
        match self {
            Type::Unknown => "unknown",
            Type::Void => "void",
            Type::Bool => "bool",
            Type::U8 => "u8",
            Type::U16 => "u16",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::I8 => "i8",
            Type::I16 => "i16",
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Ref(ty) => format!("&{}", ty.as_str()).leak(),
            Type::Defined(name) => name,
        }
    }
}

impl UnOperator {
    fn as_str(&self) -> &str{
        match self {
            Neg => "−",
            Deref => "deref",
            Not => "¬",
            Refer => "ref"
        }
    }
}

impl BinOperator {
    fn as_str(&self) -> &str{
        match self {
            Ge => "≥",
            Gt => ">",
            Le => "≤",
            Lt => "<",
            Mul => "×",
            Div => "÷",
            Add => "+",
            Sub => "−",
            Ne => "≠",
            Eq => "=",
            And => "∧",
            Or => "∨",
        }
    }
}