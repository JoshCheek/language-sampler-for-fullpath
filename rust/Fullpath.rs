use std::env;
use std::process;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let pwd = get_pwd();
    for argument in env::args() {
        println!("{}/{}", pwd, argument);
    }
}

fn get_pwd() -> String {
    match env::current_dir() {
        Ok(pwd) =>
            return format!("{}", pwd.display()),
        Err(err) => {
            println!("{}", err);
            process::exit(1);
        }
    }
}
