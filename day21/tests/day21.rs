use day21::{Image, Rules};

#[test]
fn example() {
    let rules: Rules = "../.# => ##./#../...\n\
                        .#./..#/### => #..#/..../..../#..#"
        .parse()
        .unwrap();
    let image = Image::default();
    assert_eq!(image.enhance(&rules, 2).count_on_pixels(), 12);
}
