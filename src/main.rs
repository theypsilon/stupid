extern crate stupid;

use stupid::compiler::compile;

fn main() {
    let input = r#"s
print(13434),
s"#;

    match compile(input) {
        Ok(s) => {
            println!("{}", s)
        },
        Err(e) => {
            println!("ERROR!\n{}\n{}", e.cause(), e.backtrace());
            std::process::exit(1)
        }
    }
}