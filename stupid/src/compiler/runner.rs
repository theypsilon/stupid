use failure::Error;
use compiler::stream_reader::StringReader;
use compiler::tokenizer::tokenize;
use compiler::tokenizer::global_token_matcher;
use compiler::ast::parse;

pub fn compile(input: &str) -> Result<String, Error> {
    let mut reader: StringReader = StringReader::new(input);
    let tokens = tokenize(&mut reader)?;
    parse(&tokens)?;
    Ok(format!("Tokens: {:?}", tokens))
}