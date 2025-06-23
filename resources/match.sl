enum Colour { Red, Green, Blue }

maybe_colour(): ?Colour {
    return Colour::Blue;
}

main(): void {
    c := maybe_colour();

    match c? {
        Colour::Red => println("Red!");
        Colour::Green => println("Green!");
        Colour::Blue => println("Blue!");
    }

    s := match c? {
        Colour::Red => "Red!",
        Colour::Green => "Green!",
        else => "Blue!",
    };

    println(s);

    x := 10;

    println(match x {
        5 * 2 => "ten!",
        else => "not ten!",
    });
}