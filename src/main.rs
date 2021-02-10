#![allow(non_snake_case)]
mod parser;
mod error;
mod context;
mod exec;
mod core;

use std::path::Path;
use clap::{App, Arg, crate_version};
use rustyline::{Editor, error::ReadlineError};

use exec::*;
use parser::*;
use error::*;
use context::*;

fn main() -> Result<()> {
    let matches = App::new("MyFP2")
        .version(crate_version!())
        .arg(Arg::with_name("interactive")
            .short("i")
            .takes_value(false)
            .help("open interactive mode after loading files")
        )
        .arg(Arg::with_name("library")
            .short("l")
            .long("lib")
            .multiple(true)
            .takes_value(true)
            .help("path for libraries to import")
        )
        .arg(Arg::with_name("files")
            .multiple(true)
            .takes_value(true)
        )
        .get_matches();
    let interactive = matches.is_present("interactive");
    let files = matches.values_of("files");
    let libs = matches.values_of("library");
    
    let mut ctx = Context::new();
    if let Some(libs) = libs {
        for libpath in libs {
            let libpath = Path::new(libpath);
            ctx.library.insert(
                libpath.file_stem().unwrap().to_str().unwrap().to_string(),
                libpath.to_path_buf()
            );
        }
    }
    if let Some(files) = files {
        for file in files {
            load_file(Path::new(file), &mut ctx)?;
        }
    }
    
    if interactive {
        let mut rl = Editor::<()>::new();
        let prompt = "myfp2> ".to_string();
        
        loop {
            let line = rl.readline(&prompt);
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);
                    let mut ts = TokenStream::new(line.chars());
                    if let Err(err) = parse_stmt(&mut ts).map(|stmt|execute(stmt, &mut ctx)) {
                        println!("{:?}", err);
                    }
                },
                Err(ReadlineError::Eof) => break,
                Err(err) => {
                    return Err(Error::new(ErrorKind::Other, format!("{:?}", err)));
                }
            }
        }
    } else {
        println!("All files checked. Exit.");
    }
    Ok(())
}
