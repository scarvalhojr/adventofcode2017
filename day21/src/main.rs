use clap::{crate_description, App, Arg};
use day21::{Image, Rules};
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
    let rules = read_input(args.value_of("INPUT").unwrap());
    let image = Image::default();
    println!("Part 1: {}", image.enhance(&rules, 5).count_on_pixels());
    println!("Part 1: {}", image.enhance(&rules, 18).count_on_pixels());
}

fn read_input(filename: &str) -> Rules {
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to open file '{}': {}", filename, err.to_string());
            exit(2);
        }
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => (),
        Err(err) => {
            println!(
                "Failed to read input file '{}': {}",
                filename,
                err.to_string()
            );
            exit(3);
        }
    };

    match contents.parse() {
        Ok(rules) => rules,
        Err(err) => {
            println!("Invalid input: {}", err.to_string());
            exit(4);
        }
    }
}
