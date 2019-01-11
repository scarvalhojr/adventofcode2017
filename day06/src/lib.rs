use std::collections::HashSet;
use std::str::FromStr;

pub struct Memory {
    banks: Vec<u8>,
}

impl Memory {
    pub fn count_unique_redistrib(&mut self) -> usize {
        let mut seen: HashSet<Vec<u8>> = HashSet::new();
        let mut count = 0;
        while seen.insert(self.banks.clone()) {
            count += 1;
            self.redistrib();
        }
        count
    }

    fn redistrib(&mut self) {
        let len = self.banks.len();
        if len < 2 {
            return;
        }
        let (max_idx, &max_val) = self
            .banks
            .iter()
            .enumerate()
            .rev()
            .max_by_key(|(_, val)| *val)
            .unwrap();
        self.banks[max_idx] = 0;
        let add = max_val / len as u8;
        let extra = max_val as usize % len;
        for idx in 0..len {
            if (len + idx - 1 - max_idx) % len < extra {
                self.banks[idx] += add + 1;
            } else {
                self.banks[idx] += add;
            }
        }
    }
}

impl FromStr for Memory {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let banks = s
            .split('\t')
            .filter(|num| !num.is_empty())
            .map(|num| num.parse::<u8>().map_err(|err| err.to_string()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Memory { banks })
    }
}
