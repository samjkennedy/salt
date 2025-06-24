add_one(x: u16): u16 {
    return x + 1;
}

main(): void {
    x: u8 = 255;
    println(x);
    i := add_one(x);
    println(i);

    y := 360;
    println(add_one(y as u16));
}