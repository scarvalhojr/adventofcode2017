use day06::Memory;

#[test]
fn examples() {
    let input = "0\t2\t0\t7";

    let mut memory: Memory = input.parse().unwrap();
    assert_eq!(memory.count_unique_redistrib(), 5);
    assert_eq!(memory.count_unique_redistrib(), 4);
}
