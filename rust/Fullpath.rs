use std::env;
use std::process;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let mut pwd = "".to_string();
    match env::current_dir() {
        Ok(_pwd) =>
            pwd = format!("{}", _pwd.display()),
        Err(err) => {
            println!("{}", err);
            process::exit(1);
        }
    }
    println!("{}", pwd);
    for argument in env::args() {
        println!("{}", argument);
    }
}

