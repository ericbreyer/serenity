use std::{
    env::args,
    io::{self},
};

use anyhow::Result;
use getopts::Options;
use serenity::{run_file, set_log_verbosity};

fn usage(prog: &str, opts: &Options) {
    let req = format!("{prog} path");
    let brief = opts.short_usage(&req);
    print!("{}", opts.usage(&brief));
}
fn main() -> Result<()> {
    // // usage
    // // serenity [path] - run file
    // // optional -v - verbose

    let args: Vec<String> = args().collect();
    let prog = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("v", "verbose", "enable verbose output", "LEVEL");
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("s", "scan", "scan only");
    opts.optflag("p", "parse", "parse only");
    opts.optflag("c", "compile", "compile only");
    opts.optflag("r", "run", "run (default)");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            usage(&prog, &opts);
            panic!("{}", f.to_string())
        }
    };

    let run = if !matches.opt_present("s") && !matches.opt_present("p") && !matches.opt_present("c")
    {
        true
    } else {
        matches.opt_present("r")
    };

    if matches.opt_present("h") {
        usage(&prog, &opts);
        return Ok(());
    }

    let verbose = matches.opt_get_default("v", 0).unwrap_or(0);

    let _guard = set_log_verbosity(verbose).map_err(|e| e.downcast::<io::Error>().unwrap())?;

    let file_name = if matches.free.len() == 1 {
        matches.free[0].clone()
    } else {
        anyhow::bail!("Usage: {prog} path [--v[v]]")
    };

    if matches.opt_present("s") {
        let tokens = serenity::scan(&file_name)?;
        for token in tokens {
            println!("{}", token);
        }
        return Ok(());
    };

    if matches.opt_present("p") {
        let ast = serenity::parse(&file_name)?;
        println!("{}", ast);
        return Ok(());
    };

    if matches.opt_present("c") {
        let ir = serenity::compile(&file_name)?;
        println!("{}", ir);
        return Ok(());
    };

    if run {
        let mut out = Vec::new();
        let code = run_file(&file_name, &mut out).unwrap();
        println!("{}", code);
        return Ok(());
    };

    Ok(())
}
