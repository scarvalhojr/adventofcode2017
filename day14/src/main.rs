use clap::{crate_description, App, Arg};
use day14::DiskMap;
use std::fs::File;
use std::io::Read;
use std::process::exit;

fn main() {
    let args = App::new(crate_description!())
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    println!(crate_description!());
    let key = read_input(args.value_of("INPUT").unwrap());
    let diskmap = DiskMap::new(&key);
    println!("Part 1: {}", diskmap.count_used());
    println!("Part 2: {}", diskmap.count_regions());
}

fn read_input(filename: &str) -> String {
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to open file '{}': {}", filename, err.to_string());
            exit(2);
        }
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => contents,
        Err(err) => {
            println!(
                "Failed to read input file '{}': {}",
                filename,
                err.to_string()
            );
            exit(3);
        }
    }
}
