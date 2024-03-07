use pest::{iterators::Pair, pratt_parser::{Assoc, Op, PrattParser}, Parser};
use pest_derive::Parser;
use std::sync::OnceLock;
use crate::{ast, BinOperator, Branch, Declarer, Func, Node, Result, StructSyntax, Syntax, Type, UnOperator, Variable};


#[derive(Parser)]
#[grammar = "patina.pest"]
struct PatinaParser;
fn pratt_parser() -> &'static PrattParser<Rule> {
    static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();
    PRATT_PARSER.get_or_init(||{
        PrattParser::new()
            .op(Op::infix(Rule::And, Assoc::Left) | Op::infix(Rule::Or, Assoc::Left) )
            .op(Op::infix(Rule::Eq, Assoc::Left) | Op::infix(Rule::Ne, Assoc::Left) )
            .op(Op::infix(Rule::Ge, Assoc::Left) | Op::infix(Rule::Gt, Assoc::Left) | Op::infix(Rule::Le, Assoc::Left) | Op::infix(Rule::Lt, Assoc::Left))
            .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
            .op(Op::infix(Rule::Mul, Assoc::Left) | Op::infix(Rule::Div, Assoc::Left))
            .op(Op::prefix(Rule::Neg) | Op::prefix(Rule::Not) | Op::prefix(Rule::Ref) | Op::prefix(Rule::Deref))
            
    })
}

pub fn parse<'a>(src: &'static str) -> Result<Vec<Node>> {
    let mut nodes = Vec::new();
    for pair in PatinaParser::parse(Rule::Program, &src)?.next().unwrap().into_inner() {
        match pair.as_rule() {
            Rule::Fn => nodes.push(parse_fn(pair)),
            Rule::Struct => nodes.push(parse_struct(pair)),
            Rule::Declare => nodes.push(parse_declare(pair)),
            Rule::EOI => (),
            rule =>unreachable!("Reached {:?}", rule)
        }
    }
    ast::display(&nodes);
    Ok(nodes)
}

fn parse_struct(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let line_col = pair.line_col();
    let mut pairs = pair.into_inner();
    // todo support generic
    let name = pairs.next().unwrap().as_str();
    let mut fields = Vec::new();
    loop {
        let Some(pair) = pairs.next() else {
            break;
        };
        let name = pair.as_str();
        let type_ = parse_type(pairs.next().unwrap());
        let line_col = pair.line_col();
        let var = Variable{ name, ty: type_, line_col};
        fields.push(var);
    }
    Struct(StructSyntax{ name, fields }).at(line_col)
}

fn parse_declare(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let line_col = pair.line_col();
    let mut pairs = pair.into_inner();
    let declarer = match pairs.next().unwrap().as_rule() {
        Rule::Const => Declarer::Const,
        Rule::Static => Declarer::Static,
        Rule::Let => Declarer::Let,
        _ => unreachable!()
    };
    let mut variable = {
        let pair = pairs.next().unwrap();
        let name = pair.as_str();
        let line_col = pair.line_col();
        let type_= Type::Unknown.into();
        Variable {name, ty: type_, line_col}
    };
    let mut value = None;
    for pair in pairs {
        match pair.as_rule() {
            Rule::Type => variable.ty = parse_type(pair),
            Rule::Expr => value = Some(parse_expr(pair)),
            _ => unreachable!()
        }
    }
    let value = value.unwrap();
    Declare { declarer, variable, value }.at(line_col)
}

fn parse_fn(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let line_col = pair.line_col();
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str();
    let mut params: Vec<Variable> = Vec::new();
    let mut return_type = Type::Void;
    let mut body = Vec::new();
    loop {
        let Some(pair) = pairs.next() else {
            break;
        };
        match pair.as_rule() {
            Rule::Ident => {
                let name = pair.as_str();
                let type_ = parse_type(pairs.next().unwrap());
                let line_col = pair.line_col();
                params.push(Variable{name, ty: type_, line_col});
            }
            Rule::Type => return_type = parse_type(pair),
            Rule::Body => body = parse_body(pair),
            rule => unreachable!("Reached {:?}", rule)
        }
    };
    Fn(Func { name, params, return_type, body }).at(line_col)
}

fn parse_body(pair: Pair<'static, Rule>) -> Vec<Node> {
    pair.into_inner().map(parse_stmt).collect()
}

fn parse_stmt(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let line_col = pair.line_col();
    match pair.as_rule() {
        Rule::Declare => parse_declare(pair),
        Rule::Assign => {
            let mut pairs = pair.into_inner();
            let lvalue = parse_lvalue(pairs.next().unwrap());
            let value = parse_expr(pairs.next().unwrap());
            Assign{ lvalue, value }.at(line_col)
        }
        Rule::Return => Return(pair.into_inner().next().map(parse_expr)).at(line_col),
        Rule::Break => Break(pair.into_inner().next().map(parse_expr)).at(line_col),
        // statements that can also be used as expressions
        Rule::Call => {
            let mut pairs = pair.into_inner();
            let func = pairs.next().unwrap().as_str();
            let args = pairs.map(parse_expr).collect();
            Call{ func, args }.at(line_col)
        }
        Rule::If => {
            let mut pairs = pair.into_inner();
            let mut branches = Vec::new();
            let mut default = Vec::new();
            loop {
                let Some(pair) = pairs.next() else {
                    break;
                };
                match pair.as_rule() {
                    Rule::Expr => {
                        let predicate = parse_expr(pair);
                        let body = parse_body(pairs.next().unwrap());
                        branches.push(Branch{ predicate, body });
                    },
                    Rule::Body => {
                        default = parse_body(pair);
                    }
                    _ => unreachable!()
                }
            }
            If { branches, default }.at(line_col)
        },
        Rule::Loop => {
            let pair = pair.into_inner().next().unwrap();
            let body = parse_body(pair);
            Loop { body }.at(line_col)
        },
        _ => unreachable!()
    }
}


fn parse_expr(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let pairs = pair.into_inner();
    pratt_parser()
        .map_primary(parse_primary)
        .map_infix(|left, op, right| {
            let line_col = left.line_col;
            match op.as_rule() {
                Rule::Add => BinOp(BinOperator::Add, left, right),
                Rule::Sub => BinOp(BinOperator::Sub, left, right),
                Rule::Mul => BinOp(BinOperator::Mul, left, right),
                Rule::Div => BinOp(BinOperator::Div, left, right),
                Rule::Ge  => BinOp(BinOperator::Ge,  left, right),
                Rule::Gt  => BinOp(BinOperator::Gt,  left, right),
                Rule::Le  => BinOp(BinOperator::Le,  left, right),
                Rule::Lt  => BinOp(BinOperator::Lt,  left, right),
                Rule::And => BinOp(BinOperator::And, left, right),
                Rule::Or  => BinOp(BinOperator::Or,  left, right),
                Rule::Eq  => BinOp(BinOperator::Eq,  left, right),
                Rule::Ne  => BinOp(BinOperator::Ne,  left, right),
                rule => unreachable!("Reached {:?}", rule)
            }.at(line_col)
        })
        .map_prefix(|op, right| {
            let line_col = op.line_col();
            match op.as_rule() {
                Rule::Neg => UnOp(UnOperator::Neg, right),
                Rule::Deref => UnOp(UnOperator::Deref, right),
                Rule::Not => UnOp(UnOperator::Not, right),
                Rule::Ref => UnOp(UnOperator::Refer, right),
                rule => unreachable!("Reached {:?}", rule)
            }.at(line_col)
        })
        .parse(pairs)
}
 
fn parse_primary(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let line_col = pair.line_col();
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    let mut node = match pair.as_rule() {
        Rule::True => True.at(line_col),
        Rule::False => False.at(line_col),
        Rule::FloatLiteral => {
            let mut pairs = pair.into_inner();
            let value = pairs.next().unwrap().as_str();
            let type_suffix = pairs.next().map(|pair|pair.as_str());
            FloatLiteral{ value, type_suffix }.at(line_col)
        },
        Rule::IntLiteral => {
            let mut pairs = pair.into_inner();
            let value = pairs.next().unwrap().as_str();
            let type_suffix = pairs.next().map(|pair|pair.as_str());
            IntLiteral{ value, type_suffix }.at(line_col)
        },
        Rule::StructLiteral => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str();
            let mut entries = Vec::new();
            loop {
                let Some(field) = pairs.next() else {
                    break;
                };
                let field = field.as_str();
                let value = parse_expr(pairs.next().unwrap());
                entries.push((field, value));
            }
            StructLiteral{ name, entries }.at(line_col)
        }
        Rule::Ident => Ident(pair.as_str()).at(line_col),
        Rule::If | Rule::Loop | Rule::Call  => parse_stmt(pair),
        Rule::NestedExpr => parse_expr(pair.into_inner().next().unwrap()),
        _ => unreachable!()
    };

    for pair in pairs {
        node = Member(node, pair.as_str()).at(line_col)
    }
    node
}

fn parse_lvalue(pair: Pair<'static, Rule>) -> Node {
    use Syntax::*;
    let mut pairs = pair.into_inner();
    let mut deref = false;
    let mut pair = pairs.next().unwrap();
    if let Rule::Deref = pair.as_rule() {
        deref = true;
        pair = pairs.next().unwrap();
    }
    let mut node = Ident(pair.as_str()).at(pair.line_col());
    for pair in pairs {
        node = Member(node, pair.as_str()).at(pair.line_col())
    }
    if deref == true {
        node = UnOp(UnOperator::Deref, node).at(pair.line_col())
    }
    node
}

fn parse_type(pair: Pair<'static, Rule>) -> Type {
    let pairs = pair.into_inner();
    let mut is_ref = false;
    let mut ty = None;
    for pair in pairs {
        match pair.as_rule() {
            Rule::Ref => is_ref = true,
            Rule::Ident => ty = Some(Type::from(pair.as_str())),
            _ => unreachable!()
        }
    }
    let ty = ty.unwrap();
    if is_ref {
        Type::Ref(Box::new(ty))
    } else {
        ty
    }
}