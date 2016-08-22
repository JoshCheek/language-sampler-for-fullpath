use std::env;
use std::process;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let pwd = get_pwd();
    let mut args = env::args();
    args.next(); // first arg is the program name
    for argument in args {
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
