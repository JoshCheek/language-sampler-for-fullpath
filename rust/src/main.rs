use std::env;
use std::process;
use std::io;
use std::io::prelude::*;

fn select_paths(paths: Vec<String>) -> Vec<String> {
    paths.into_iter()
         .filter(|path| path != "")
         .filter(|path| !path.starts_with("-"))
         .collect()
}

fn main() {
    let help_screen           =
"usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard";
    let pwd         = get_pwd();
    let args        = get_args();
    let print_help  = args.contains(&"-h".to_string()) || args.contains(&"--help".to_string());
    let copy_output = args.contains(&"-c".to_string()) || args.contains(&"--copy".to_string());
    let mut paths   = select_paths(args);

    if print_help {
        println!("{}", help_screen);
        process::exit(0);
    }

    if paths.is_empty() {
        let stdin = io::stdin();
        for maybe_line in stdin.lock().lines() {
            match maybe_line { Ok(line) => paths.push(line), Err(_) => (), }
        }
    }

    // Sigh, it's apparently not smart enough to let me return the iter,
    // so I have to convert back and forth.
    // Also tried making it a macro, but it doesn't have a way to tell me
    // if the iterator is empty or not, so in order to ask the size, (eg is_empty())
    // I have to convert back to a vector, too >.<
    paths = select_paths(paths)
                .into_iter()
                .map(|path| format!("{}/{}", pwd, path))
                .collect();

    write_paths(&mut std::io::stdout(), &paths);

    if copy_output {
        copy_paths(&paths);
    }
}

fn get_pwd() -> String {
    match env::current_dir() {
        Ok(pwd)  => format!("{}", pwd.display()),
        Err(err) => panic!(err),
    }
}

fn get_args() -> Vec<String>  {
    let mut argv = env::args();
    argv.next(); // first arg in argv is the program name
    argv.collect()
}

fn write_paths(stream:&mut std::io::Write, paths:&Vec<String>) {
    match paths.len() {
        1 => ignore_result(stream.write(&paths[0].as_bytes())),
        _ => for path in paths {
                 ignore_result(stream.write(&format!("{}\n", path).as_bytes()));
             },
    }
    ignore_result(stream.flush());
}


fn copy_paths(paths:&Vec<String>) {
    let maybe_pbcopy = process::Command::new("/usr/bin/pbcopy")
        .stdin(process::Stdio::piped())
        .spawn();
    match maybe_pbcopy {
        Ok(mut pbcopy) => {
            match pbcopy.stdin.as_mut() {
                Some(pbcopy_stdin) => write_paths(pbcopy_stdin, &paths),
                None => (),
            }
            ignore_result(pbcopy.wait());
        },
        Err(_) => (),
    }
}

fn ignore_result<X, Y>(result:std::result::Result<X, Y>) {
    match result {
        Ok(_)  => (),
        Err(_) => (),
    }
}
