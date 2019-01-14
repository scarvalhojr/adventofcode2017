use hex::encode;
use std::collections::HashSet;

const HASH_LEN: usize = 256;
const HASH_SUFFIX: &[u8] = &[17, 31, 73, 47, 23];
const TWIST_ROUNDS: usize = 64;
const CHUNK_SIZE: usize = 16;
const GRID_DIM: u8 = 128;

#[derive(Debug)]
pub struct KnotHash {
    list: Vec<u8>,
    head: usize,
    skip: usize,
}

impl Default for KnotHash {
    fn default() -> Self {
        Self {
            list: (0..HASH_LEN).map(|n| n as u8).collect(),
            head: 0,
            skip: 0,
        }
    }
}

impl KnotHash {
    pub fn update(&mut self, input: &str) {
        for _ in 0..TWIST_ROUNDS {
            for length in input.as_bytes().iter().chain(HASH_SUFFIX.iter()) {
                self.twist(*length as usize);
            }
        }
    }

    pub fn to_hex_string(&self) -> String {
        encode(self.to_byte_vec())
    }

    pub fn to_byte_vec(&self) -> Vec<u8> {
        self.list
            .chunks(CHUNK_SIZE)
            .map(|chunk| chunk.iter().fold(0, |acc, x| acc ^ *x))
            .collect::<Vec<_>>()
    }

    fn twist(&mut self, length: usize) {
        let mut head = self.head;
        let mut tail = (self.head + length - 1) % HASH_LEN;
        for _ in 0..length / 2 {
            self.list.swap(head, tail);
            head = (head + 1) % HASH_LEN;
            tail = (HASH_LEN + tail - 1) % HASH_LEN;
        }
        self.head = (self.head + self.skip + length) % HASH_LEN;
        self.skip = (self.skip + 1) % HASH_LEN;
    }
}

pub struct DiskMap {
    used: HashSet<(u8, u8)>,
}

impl DiskMap {
    pub fn new(key: &str) -> Self {
        let mut used = HashSet::new();
        for row in 0..GRID_DIM {
            let mut hash = KnotHash::default();
            hash.update(&format!("{}-{}", key, row));
            used.extend(
                hash.to_byte_vec()
                    .iter()
                    .flat_map(|&val| u8_to_bin_vec(val))
                    .enumerate()
                    .filter(|(_, bit)| *bit == 1)
                    .map(|(col, _)| (row, col as u8)),
            );
        }
        Self { used }
    }

    pub fn count_used(&self) -> usize {
        self.used.len()
    }

    pub fn count_regions(&self) -> usize {
        // Create a set with all used positions in signed integer coordinates
        // so we don't have to worry about boundaries
        let mut pending: HashSet<_> = self
            .used
            .iter()
            .map(|pos| (i16::from(pos.0), i16::from(pos.1)))
            .collect();

        let mut queue = Vec::new();
        let mut regions = 0;

        while !pending.is_empty() {
            // Start a queue with a random (pending) position
            queue.push(pending.iter().next().unwrap().clone());

            while let Some(pos) = queue.pop() {
                pending.remove(&pos);
                let neighbours = [
                    (pos.0 - 1, pos.1),
                    (pos.0 + 1, pos.1),
                    (pos.0, pos.1 - 1),
                    (pos.0, pos.1 + 1),
                ];
                for neighbour in &neighbours {
                    if pending.contains(neighbour) {
                        queue.push(*neighbour);
                    }
                }
            }

            regions += 1;
        }

        regions
    }
}

fn u8_to_bin_vec(val: u8) -> Vec<u8> {
    (0..8)
        .rev()
        .map(|exp| match val & 2_u8.pow(exp) {
            0 => 0,
            _ => 1,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_hashes() {
        let test_cases = [
            ("", "a2582a3a0e66e6e86e3812dcb672a272"),
            ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
            ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
            ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e"),
        ];

        for (input, output) in test_cases.iter() {
            let mut hash = KnotHash::default();
            hash.update(input);
            assert_eq!(hash.to_hex_string(), *output);
        }
    }
}
