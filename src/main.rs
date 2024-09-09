use std::{env::args, io::{self, stdout, Write}};

use getopts::Options;
use serenity::{run_file, set_log_verbosity};
use anyhow::Result;

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

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            usage(&prog, &opts);
            panic!("{}", f.to_string())
        }
    };

    if matches.opt_present("h") {
        usage(&prog, &opts);
        return Ok(());
    }

    let verbose = matches.opt_get_default("v", 0).unwrap_or(0);

    let _guard = set_log_verbosity(verbose).map_err(|e| e.downcast::<io::Error>().unwrap())?;

    return if matches.free.len() == 1 {
        run_file(&matches.free[0].clone(), stdout().by_ref())?;
        Ok(())
    } else {
        anyhow::bail!("Usage: {prog} path [--v[v]]")
    }
}
