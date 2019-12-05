use compiler::tokenizer::{Token, Delimiter};
use compiler::error::Return;
use compiler::stream_reader::GenericReader;
use compiler::stream_reader::StreamReader;

pub enum AST {
    Fn(Fn),
    Sentence(Sentence)
}

pub struct Decl {
    kind: String,
    name: String
}

pub struct Fn {
    name: String,
    params: Vec<Decl>,
    return_kind: String,
    sentences: Vec<AST>
}

pub struct Sentence {
    decl: Decl,
    expr: Expr
}

pub enum Expr {
    Ident(String),
    Literal(String),
    Op(Op)
}

pub struct Op {
    name: String,
    params: Vec<Expr>
}

pub fn parse(input: &Vec<Token>) -> Return<String> {
    let mut reader = GenericReader::new(input);
    let mut tree: Vec<AST> = Vec::new();
    while let Some(token) = reader.current() {
        match token {
            &Token::Word(ref w) => {
                match w.as_ref() {
                    "fn" => {
                        try!(parse_fn(&mut tree, &mut reader));
                    }
                    _ => {
                        bail!("cant define whatever in global scope")
                    }
                }
            },
            _ => {
                bail!("awaiting fn declaration")
            }
        }
        reader.advance();
    }
    Ok("something".into())
}

fn parse_fn(tree: &mut Vec<AST>, reader: &mut GenericReader<Token>) -> Return<()> {
    reader.advance();
    if let Some(&Token::Word(ref name)) = reader.current() {
        let func = Fn {
            name: name.clone(),
            params: Vec::new(),
            return_kind: "void".into(),
            sentences: Vec::new()
        };
        reader.advance();
        match reader.current() {
            Some(&Token::LeftDelimiter(Delimiter::Parenthesis)) => {
                reader.advance();
            }
            _ => {
                bail!("Expecting (");
            }
        }

        match reader.current() {
            Some(&Token::RightDelimiter(Delimiter::Parenthesis)) => {
                reader.advance();
            }
            Some(&Token::Word(ref w)) => {
                bail!("Uninmplemented!");
            }
            _ => {
                bail!("Expecting parameters");
            }
        }

        match reader.current() {
            Some(&Token::LeftDelimiter(Delimiter::Brace)) => {
                reader.advance();
            }
            _ => {
                bail!("Expecting body");
            }
        }

        match reader.current() {
            Some(&Token::RightDelimiter(Delimiter::Brace)) => {
                reader.advance();
            }
            _ => {
                bail!("Expecting end of body");
            }
        }

        tree.push(AST::Fn(func))
    } else {
        reader.rewind();
    }
    Ok(())
}