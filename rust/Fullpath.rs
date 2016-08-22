use std::env;
use std::process;
use std::io::{self, Read};

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let pwd = get_pwd();
    let mut paths:Vec<String> =
        get_args().map(|path| format!("{}/{}", pwd, path)).collect();
    if paths.len() == 0 {
        let mut buffer = String::new();
        match io::stdin().read_to_string(&mut buffer) {
            Ok(_)    => paths = vec![buffer],
            Err(err) => {
                println!("{}", err);
                process::exit(1);
            }
        }
    }


    if paths.len() == 1 {
        print!("{}", paths[0]);
    } else {
        for path in paths {
            println!("{}", path);
        }
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

fn get_args() -> env::Args {
    let mut argv = env::args();
    argv.next(); // first arg is the program name
    return argv;
}
