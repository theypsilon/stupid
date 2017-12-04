pub mod tokenizer;

use failure::Error;
use self::tokenizer::StringReader;
use self::tokenizer::tokenize;
use self::tokenizer::global_token_matcher;
use self::tokenizer::parse;


pub fn compile(input: &str) -> Result<String, Error> {
    let mut reader: StringReader = StringReader::new(input);
    let tokens = try!(tokenize(&mut reader, global_token_matcher));
    try!(parse(&tokens));
    Ok(format!("Tokens: {:?}", tokens))
}