use std::env;
use std::process;
use std::io;
use std::io::prelude::*;

// This code is editable and runnable!
fn main() {
    // let pwd:collections::string::String = "".to_string();
    let pwd = get_pwd();
    let mut paths = get_args();
    if paths.len() == 0 {
        let stdin = io::stdin();
        for maybe_line in stdin.lock().lines() {
            match maybe_line {
                Ok(line) => paths.push(line),
                Err(_)   => {}
            }
        }
    }
    paths = paths.into_iter().map(|path| format!("{}/{}", pwd, path)).collect();
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

fn get_args() -> Vec<String>  {
    let mut argv = env::args();
    argv.next(); // first arg is the program name
    return argv.collect();
}
