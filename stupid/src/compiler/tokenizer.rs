use failure::Error;
use std::str::Chars;
use std::slice::Iter;

use self::Token::Word;

pub struct StringReader<'a> {
    current: Option<char>,
    position: usize,
    iter: Chars<'a>,
    input: &'a str
}

impl<'a> StringReader<'a> {
    pub fn new(input: &'a str) -> StringReader {
        let mut iter = input.chars();
        let current = iter.next();
        StringReader {
            input: input,
            iter: iter,
            position: 0,
            current: current
        }
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn check_ahead(&self, offset: usize) -> Option<char> {
        self.iter.clone().nth(offset)
    }

    fn advance(&mut self) -> Option<char> {
        self.position = self.position + 1;
        self.current = self.iter.next();
        self.current()
    }

    #[allow(dead_code)]
    fn rewind(&mut self) -> Option<()> {
        if self.position == 0 { return None }
        self.position = self.position - 1;
        self.iter = self.input.chars();
        self.current = self.iter.nth(self.position);
        Some( () )
    }
}

#[cfg(test)]
mod tests_string_reader {
    use super::*;

    fn sut() -> StringReader<'static> {
        StringReader::new("wtf")
    }

    #[test]
    fn string_reader_with_empty_string_return_always_none() {
        let mut empty = StringReader::new("");
        assert_eq!(empty.current(), None);
        assert_eq!(empty.check_ahead(0), None);
        assert_eq!(empty.rewind(), None);
        assert_eq!(empty.advance(), None);
    }

    #[test]
    fn current_on_new_string_reader_returns_first_char() {
        assert_eq!(sut().current().unwrap(), 'w')
    }

    #[test]
    fn advance_when_string_reader_not_consumed_returns_char() {
        assert_eq!(sut().advance().unwrap(), 't')
    }

    #[test]
    fn check_ahead_when_offset_is_x_returns_same_as_x_minus_one_advance() {
        for x in [0, 1, 234].iter() {
            let mut reader = sut();
            for ad in 0 .. *x + 1 {
                reader.advance();
            }
            assert_eq!(sut().check_ahead(*x), reader.current())
        }
    }

    #[test]
    fn check_ahead_when_offset_is_too_big_for_stream_returns_none() {
        assert_eq!(sut().check_ahead(3), None)
    }

    #[test]
    fn advance_on_consumed_string_reader_returns_none() {
        assert_eq!(consumed_sut().advance(), None)
    }

    #[test]
    fn current_on_consumed_string_reader_returns_none() {
        assert_eq!(consumed_sut().current(), None)
    }

    fn consumed_sut() -> StringReader<'static> {
        let mut reader = sut();
        while let Some(_) = reader.advance() {}
        reader
    }

    #[test]
    fn rewind_when_never_advance_returns_none() {
        assert_eq!(sut().rewind(), None)
    }

    #[test]
    fn rewind_after_advance_returns_first_current() {
        let mut reader = sut();
        reader.advance();
        reader.rewind();
        assert_eq!(reader.current(), sut().current())
    }

    #[test]
    fn rewind_after_advance_returns_to_previous_states() {
        let mut sut = sut();
        sut.advance();
        sut.advance();
        assert_eq!(sut.current().unwrap(), 'f');
        sut.rewind();
        assert_eq!(sut.current().unwrap(), 't');
        sut.rewind();
        assert_eq!(sut.current().unwrap(), 'w');
        sut.advance();
        assert_eq!(sut.current().unwrap(), 't');
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(String),
    Char(char),
    String(String),
    Word(String),
    LeftDelimiter(Delimiter),
    RightDelimiter(Delimiter),
    MoreThan,
    LessThan,
    Comma,
    Dot,
    Semicolon,
    Equal,
    DoubleEqual,
    Slash,
    Mult,
    Minus,
    Plus,
    Mod,
    Power,
    Comment,
    Unknown(char)
}

#[derive(Debug, PartialEq)]
pub enum Delimiter {
    Parenthesis,
    Bracket,
    Brace
}

pub type TokenMatcher = fn(char) -> TokenProvider;
pub enum TokenProvider {
    Ignored,
    Literal(Token),
    Func(TokenProviderFn)
}
pub type TokenProviderFn = fn(&mut StringReader) -> TokenProviderFnResult;
pub type TokenProviderFnResult = Result<Token, Error>;

pub fn tokenize(mut reader: &mut StringReader, matcher: TokenMatcher) -> Result<Vec<Token>, Error> {
    let mut tokens: Vec<Token> = Vec::new();
    while let Some(c) = reader.current() {
        let consumer = matcher(c);
        match consumer {
            TokenProvider::Ignored => {},
            TokenProvider::Literal(t) => tokens.push(t),
            TokenProvider::Func(foo) => tokens.push(try!(foo(&mut reader)))
        }
        reader.advance();
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests_tokenize {
    use super::*;

    #[test]
    fn when_complex_input_returns_expected_tokens() {
        let input = r#"s
        // comment
let a = 3*6;'b'
"asdf"
/*
let a = 3*6;'b'
"asdf"
*//*ds
 */
print(13434),
// comment print(23434)
/ /
dostuff/* 34 mierda */3
s"#;
        let expected: Vec<Token> = vec!(
            Token::Word("s".into()),
            Token::Comment,
            Token::Word("let".into()),
            Token::Word("a".into()),
            Token::Equal,
            Token::Number("3".into()),
            Token::Mult,
            Token::Number("6".into()),
            Token::Semicolon,
            Token::Char('b'),
            Token::String("asdf".into()),
            Token::Comment,
            Token::Comment,
            Token::Word("print".into()),
            Token::LeftDelimiter(Delimiter::Parenthesis),
            Token::Number("13434".into()),
            Token::RightDelimiter(Delimiter::Parenthesis),
            Token::Comma,
            Token::Comment,
            Token::Slash,
            Token::Slash,
            Token::Word("dostuff".into()),
            Token::Comment,
            Token::Number("3".into()),
            Token::Word("s".into())
        );
        let mut reader: StringReader = StringReader::new(input);
        let actual = tokenize(&mut reader, global_token_matcher).unwrap();
        assert_eq!(actual, expected);
    }
}

pub fn global_token_matcher(c: char) -> TokenProvider {
    match c {
        '=' => TokenProvider::Func(tokenize_equal),
        '\'' => TokenProvider::Func(tokenize_single_quote),
        '"' => TokenProvider::Func(tokenize_double_quote),
        '0' ... '9' => TokenProvider::Func(tokenize_number),
        'a' ... 'z' |  'A'...'Z' => TokenProvider::Func(tokenize_word),
        '(' => TokenProvider::Literal(Token::LeftDelimiter(Delimiter::Parenthesis)),
        ')' => TokenProvider::Literal(Token::RightDelimiter(Delimiter::Parenthesis)),
        '[' => TokenProvider::Literal(Token::LeftDelimiter(Delimiter::Bracket)),
        ']' => TokenProvider::Literal(Token::RightDelimiter(Delimiter::Bracket)),
        '{' => TokenProvider::Literal(Token::LeftDelimiter(Delimiter::Brace)),
        '}' => TokenProvider::Literal(Token::RightDelimiter(Delimiter::Brace)),
        '<' => TokenProvider::Literal(Token::MoreThan),
        '>' => TokenProvider::Literal(Token::LessThan),
        ',' => TokenProvider::Literal(Token::Comma),
        '.' => TokenProvider::Literal(Token::Dot),
        ';' => TokenProvider::Literal(Token::Semicolon),
        ' ' | '\n' | '\t' | '\r' => TokenProvider::Ignored,
        '/' => TokenProvider::Func(tokenize_slash),
        '*' => TokenProvider::Literal(Token::Mult),
        '-' => TokenProvider::Literal(Token::Minus),
        '+' => TokenProvider::Literal(Token::Plus),
        '%' => TokenProvider::Literal(Token::Mod),
        '^' => TokenProvider::Literal(Token::Power),
        _ => TokenProvider::Literal(Token::Unknown(c))
    }
}

pub fn tokenize_single_quote(reader: &mut StringReader) -> TokenProviderFnResult {
    if let Some('\'') = reader.check_ahead(1) {
        let result = match reader.advance() {
            Some(c) => c,
            None => return Err(format_err!("Illogic!"))
        };
        reader.advance();
        Ok( Token::Char(result) )
    } else {
        Err(format_err!("Single quotes are for chars only"))
    }
}

pub fn tokenize_double_quote(reader: &mut StringReader) -> TokenProviderFnResult {
    let mut result = String::new();
    while let Some(x) = reader.check_ahead(0) {
        reader.advance();
        if x == '"' {break}
        result.push(x);
    }
    Ok( Token::String(result) )
}

pub fn tokenize_equal(reader: &mut StringReader) -> TokenProviderFnResult {
    if let Some('=') = reader.check_ahead(0) {
        reader.advance();
        Ok( Token::DoubleEqual )
    } else {
        Ok( Token::Equal )
    }
}

pub fn tokenize_slash(mut reader: &mut StringReader) -> TokenProviderFnResult {
    match reader.check_ahead(0) {
        Some('/') => {reader.advance(); tokenize_eol_comment(&mut reader)},
        Some('*') => {reader.advance(); tokenize_ml_comment(&mut reader)},
        _ => Ok( Token::Slash )
    }
}

pub fn tokenize_eol_comment(reader: &mut StringReader) -> TokenProviderFnResult {
    while let Some(c) = reader.advance() {
        if c == '\n' {break}
    }
    Ok( Token::Comment )
}

pub fn tokenize_ml_comment(reader: &mut StringReader) -> TokenProviderFnResult {
    let mut asterisk = false;
    while let Some(c) = reader.advance() {
        if asterisk && c == '/' {break}
        asterisk = c == '*';
    }
    Ok( Token::Comment )
}

pub fn tokenize_number(reader: &mut StringReader) -> TokenProviderFnResult {
    let mut result = match reader.current() {
        Some(c) => c.to_string(),
        None => return Err(format_err!("Empty number"))
    };
    while let Some(c @ '0' ... '9') = reader.check_ahead(0) {
        result.push(c);
        reader.advance();
    }
    Ok( Token::Number(result) )
}

#[cfg(test)]
mod tests_tokenize_number {
    use super::*;

    #[test]
    fn when_number_string_returns_number_token() {
        let mut reader: StringReader = StringReader::new("232");
        assert_eq!(tokenize_number(&mut reader).unwrap(), Token::Number("232".into()))
    }
}

pub fn tokenize_word(reader: &mut StringReader) -> TokenProviderFnResult {
    let mut result = match reader.current() {
        Some(c) => c.to_string(),
        None => return Err(format_err!("Empty word"))
    };
    while let Some(c @ 'a' ... 'z') = reader.check_ahead(0) {
        result.push(c);
        reader.advance();
    }
    Ok( Token::Word(result) )
}


#[cfg(test)]
mod tests_tokenize_word {
    use super::*;

    #[test]
    fn when_word_string_returns_word_token() {
        let mut reader: StringReader = StringReader::new("asdf");
        assert_eq!(tokenize_word(&mut reader).unwrap(), Token::Word("asdf".into()))
    }
}


pub struct Reader<'a, T: 'a> {
    current: Option<&'a T>,
    position: usize,
    iter: Iter<'a, T>,
    input: &'a Vec<T>
}

impl<'a, T> Reader<'a, T> {
    fn new(input: &'a Vec<T>) -> Reader<T> {
        let mut iter = input.iter();
        let current = iter.next();
        Reader {
            input: input,
            iter: iter,
            position: 0,
            current: current
        }
    }

    fn current(&self) -> Option<&'a T> {
        self.current
    }

    fn check_ahead(&self, offset: usize) -> Option<&'a T> {
        self.iter.clone().nth(offset)
    }

    fn advance(&mut self) -> Option<&'a T> {
        self.position = self.position + 1;
        self.current = self.iter.next();
        self.current()
    }

    #[allow(dead_code)]
    fn rewind(&mut self) -> Option<()> {
        if self.position == 0 { return None }
        self.position = self.position - 1;
        self.iter = self.input.iter();
        self.current = self.iter.nth(self.position);
        Some( () )
    }
}

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

pub fn parse(input: &Vec<Token>) -> Result<String, Error> {
    let mut reader = Reader::new(input);
    let mut tree: Vec<AST> = Vec::new();
    while let Some(token) = reader.current() {
        match token {
            &Token::Word(ref w) => {
                match w.as_ref() {
                    "fn" => {
                        try!(parse_fn(&mut tree, &mut reader));
                    }
                    _ => {
                        return Err(format_err!("cant define whatever in global scope"))
                    }
                }
            },
            _ => {
                return Err(format_err!("awaiting fn declaration"))
            }
        }
        reader.advance();
    }
    Ok("something".into())
}

fn parse_fn(tree: &mut Vec<AST>, reader: &mut Reader<Token>) -> Result<(), Error> {
    reader.advance();
    if let Some(&Word(ref name)) = reader.current() {
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
                return Err(format_err!("Expecting ("));
            }
        }

        match reader.current() {
            Some(&Token::RightDelimiter(Delimiter::Parenthesis)) => {
                reader.advance();
            }
            Some(&Token::Word(ref w)) => {
                return Err(format_err!("Uninmplemented!"));
            }
            _ => {
                return Err(format_err!("Expecting parameters"));
            }
        }

        match reader.current() {
            Some(&Token::LeftDelimiter(Delimiter::Brace)) => {
                reader.advance();
            }
            _ => {
                return Err(format_err!("Expecting body"));
            }
        }

        match reader.current() {
            Some(&Token::RightDelimiter(Delimiter::Brace)) => {
                reader.advance();
            }
            _ => {
                return Err(format_err!("Expecting end of body"));
            }
        }

        tree.push(AST::Fn(func))
    } else {
        reader.rewind();
    }
    Ok(())
}