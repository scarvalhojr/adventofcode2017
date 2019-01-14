use day14::DiskMap;

#[test]
fn example_part1() {
    let diskmap = DiskMap::new("flqrgnkx");
    assert_eq!(diskmap.count_used(), 8108);
}

#[test]
fn example_part2() {
    let diskmap = DiskMap::new("flqrgnkx");
    assert_eq!(diskmap.count_regions(), 1242);
}
