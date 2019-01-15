use regex::Regex;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Image {
    pixels: Vec<Vec<bool>>,
}

impl Default for Image {
    fn default() -> Self {
        Self {
            pixels: vec![
                vec![false, true, false],
                vec![false, false, true],
                vec![true, true, true],
            ],
        }
    }
}

impl Image {
    pub fn size(&self) -> usize {
        self.pixels.len()
    }

    pub fn count_on_pixels(&self) -> usize {
        self.pixels
            .iter()
            .map(|row| row.iter().filter(|&p| *p).count())
            .sum()
    }

    pub fn enhance(&self, rules: &Rules, iterations: usize) -> Self {
        let mut image = self.clone();
        for _ in 1..=iterations {
            image = image.enhance_step(rules);
        }
        image
    }

    fn enhance_step(&self, rules: &Rules) -> Self {
        let pattern_size = match self.size() % 2 {
            0 => 2,
            _ => 3,
        };

        // Pixels of a square scanning the current image
        let mut pattern = Self {
            pixels: vec![vec![false; pattern_size]; pattern_size],
        };

        let squares = self.size() / pattern_size;
        let new_size = squares * (pattern_size + 1);

        // Pixels of resulting image
        let mut pixels = vec![vec![false; new_size]; new_size];

        for sq_row in 0..squares {
            for sq_col in 0..squares {
                // Capture a square pattern from current image
                let curr_row = sq_row * pattern_size;
                let curr_col = sq_col * pattern_size;
                for row in 0..pattern_size {
                    pattern.pixels[row][..pattern_size].clone_from_slice(
                        &self.pixels[curr_row + row]
                            [curr_col..(pattern_size + curr_col)],
                    )
                }

                if let Some(result) = rules.get_result(&pattern) {
                    // Copy result to new image
                    let new_row = sq_row * (pattern_size + 1);
                    let new_col = sq_col * (pattern_size + 1);
                    for row in 0..=pattern_size {
                        pixels[new_row + row]
                            [new_col..=(pattern_size + new_col)]
                            .clone_from_slice(
                                &result.pixels[row][..=pattern_size],
                            )
                    }
                } else {
                    panic!("Pattern not found!");
                }
            }
        }

        Self { pixels }
    }

    pub fn rotate_clockwise(&self) -> Self {
        let size = self.size();
        let mut pixels = vec![vec![false; size]; size];
        for (row_num, row) in pixels.iter_mut().enumerate().take(size) {
            for (col_num, cell) in row.iter_mut().enumerate().take(size) {
                *cell = self.pixels[size - col_num - 1][row_num];
            }
        }
        Self { pixels }
    }

    pub fn flip_vertical(&self) -> Self {
        let size = self.size();
        let mut pixels = vec![vec![false; size]; size];
        for (row_num, row) in pixels.iter_mut().enumerate().take(size) {
            row[..size]
                .clone_from_slice(&self.pixels[size - row_num - 1][..size])
        }
        Self { pixels }
    }
}

impl Display for Image {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let image = self
            .pixels
            .iter()
            .map(|row| {
                row.iter()
                    .map(|col| match col {
                        false => '.',
                        true => '#',
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", image)
    }
}

impl FromStr for Image {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pixels = s
            .split('/')
            .map(|line| {
                line.chars()
                    .map(|p| match p {
                        '.' => Ok(false),
                        '#' => Ok(true),
                        _ => Err("Invalid character in image".to_string()),
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;

        let size = pixels.len();
        if pixels.iter().any(|row| row.len() != size) {
            return Err("Image is not square".to_string());
        }

        Ok(Self { pixels })
    }
}

pub struct Rules {
    rules: HashMap<Image, Image>,
}

impl Rules {
    pub fn get_result(&self, pattern: &Image) -> Option<&Image> {
        self.rules.get(pattern)
    }
}

impl FromStr for Rules {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rule_re =
            Regex::new(r"(?P<pat>[\.#/]+) => (?P<res>[\.#/]+)$").unwrap();

        let mut rules = HashMap::new();
        for line in s.lines() {
            let captures =
                rule_re.captures(line).ok_or("Invalid rule format")?;
            let mut pattern: Image =
                captures.name("pat").unwrap().as_str().parse()?;
            let result: Image =
                captures.name("res").unwrap().as_str().parse()?;
            match (pattern.size(), result.size()) {
                (2, 3) => (),
                (3, 4) => (),
                _ => return Err("Invalid image sizes in rule".to_string()),
            };
            for _ in 1..=4 {
                rules.insert(pattern.clone(), result.clone());
                rules.insert(pattern.flip_vertical(), result.clone());
                pattern = pattern.rotate_clockwise();
            }
        }

        Ok(Self { rules })
    }
}
