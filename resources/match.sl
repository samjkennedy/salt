enum Colour { Red, Green, Blue }

maybe_colour(): ?Colour {
    return Colour::Red;
}

main(): void {
    c := maybe_colour();

    match c {
        Colour::Red => println("Red!");
        Colour::Green => println("Green!");
        Colour::Blue => println("Blue!");
    }
}