extern crate stupid;

use stupid::compiler::runner::compile;

fn main() {
    let input = r#"s
print(13434),
s"#;

    match compile(input) {
        Ok(s) => {
            println!("{}", s)
        },
        Err(e) => {
            println!("ERROR!\n{}\n{}", e, e.backtrace());
            std::process::exit(1)
        }
    }
}