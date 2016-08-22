use std::env;
use std::process;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let pwd  = get_pwd();
    let args = get_args();
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

fn get_args() -> Vec<String> {
    let mut argv = env::args();
    argv.next(); // first arg is the program name
    return argv.collect();
}
