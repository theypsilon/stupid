use compiler::error::Return;
use compiler::stream_reader::StreamReader;

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
pub type TokenProviderFn = fn(&mut StreamReader<char>) -> Return<Token>;

pub fn tokenize(mut reader: &mut StreamReader<char>) -> Return<Vec<Token>> {
    return tokenize_matcher(reader, global_token_matcher);
}

pub fn tokenize_matcher(mut reader: &mut StreamReader<char>, matcher: TokenMatcher) -> Return<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    while let Some(c) = reader.current() {
        let consumer = matcher(c);
        match consumer {
            TokenProvider::Ignored => {},
            TokenProvider::Literal(t) => tokens.push(t),
            TokenProvider::Func(foo) => tokens.push(foo(reader)?)
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
        let mut reader: StreamReader<char> = StringReader::new(input);
        let actual = tokenize_matcher(reader, global_token_matcher).unwrap();
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

pub fn tokenize_single_quote(reader: &mut StreamReader<char>) -> Return<Token> {
    if let Some('\'') = reader.check_ahead(1) {
        let result = match reader.advance() {
            Some(c) => c,
            None => bail!("Illogic!")
        };
        reader.advance();
        Ok( Token::Char(result) )
    } else {
        bail!("Single quotes are for chars only")
    }
}

pub fn tokenize_double_quote(reader: &mut StreamReader<char>) -> Return<Token> {
    let mut result = String::new();
    while let Some(x) = reader.check_ahead(0) {
        reader.advance();
        if x == '"' {break}
        result.push(x);
    }
    Ok( Token::String(result) )
}

pub fn tokenize_equal(reader: &mut StreamReader<char>) -> Return<Token> {
    if let Some('=') = reader.check_ahead(0) {
        reader.advance();
        Ok( Token::DoubleEqual )
    } else {
        Ok( Token::Equal )
    }
}

pub fn tokenize_slash(mut reader: &mut StreamReader<char>) -> Return<Token> {
    match reader.check_ahead(0) {
        Some('/') => {reader.advance(); tokenize_eol_comment(reader)},
        Some('*') => {reader.advance(); tokenize_ml_comment(reader)},
        _ => Ok( Token::Slash )
    }
}

pub fn tokenize_eol_comment(reader: &mut StreamReader<char>) -> Return<Token> {
    while let Some(c) = reader.advance() {
        if c == '\n' {break}
    }
    Ok( Token::Comment )
}

pub fn tokenize_ml_comment(reader: &mut StreamReader<char>) -> Return<Token> {
    let mut asterisk = false;
    while let Some(c) = reader.advance() {
        if asterisk && c == '/' {break}
        asterisk = c == '*';
    }
    Ok( Token::Comment )
}

pub fn tokenize_number(reader: &mut StreamReader<char>) -> Return<Token> {
    let mut result = match reader.current() {
        Some(c) => c.to_string(),
        None => bail!("Empty number")
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
        let mut reader: StreamReader<char> = StringReader::new("232");
        assert_eq!(tokenize_number(reader).unwrap(), Token::Number("232".into()))
    }
}

pub fn tokenize_word(reader: &mut StreamReader<char>) -> Return<Token> {
    let mut result = match reader.current() {
        Some(c) => c.to_string(),
        None => bail!("Empty word")
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
        let mut reader: StreamReader<char> = StringReader::new("asdf");
        assert_eq!(tokenize_word(reader).unwrap(), Token::Word("asdf".into()))
    }
}

