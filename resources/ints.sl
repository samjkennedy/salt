add_one(x: u16) => x + 1;

halve(f: f32) => f / 2;

main(): void {
    x: u8 = 255;
    println(x);
    i := add_one(x);
    println(i);

    y := 360;
    println(add_one(y as u16));

    println(halve(33.25));
}