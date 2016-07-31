extern crate stupid;
use stupid::tokenizer::compile;

fn main() {
    let input = r#"s
print(13434),
s"#;

    match compile(input) {
        Ok(s) => println!("{}", s),
        Err(e) => println!("ERROR {}", e)
    }
}