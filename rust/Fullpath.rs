use std::env;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let mut pwd = "".to_string();
    match env::current_dir() {
        Ok(_pwd) => pwd = format!("{}", _pwd.display()),
        Err(err) => println!("{}", err)
    }
    println!("{}", pwd);
    for argument in env::args() {
        println!("{}", argument);
    }
}

