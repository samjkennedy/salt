add_one(x: u16) => x + 1;

halve(f: f32) => f / 2;

negatives(): void {
    x := -1;

    println(x);
}

chars(): void {
    e := 69;

    println(e as char);
}

main(): void {
    x: u8 = 255;
    println(x);
    i := add_one(x);
    println(i);

    y := 360;
    println(add_one(y as u16));

    println(halve(33.25));

    println(17.5 as u8);

    negatives();

    chars();
}