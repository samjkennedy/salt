enum Colour {
    Red,
    Green,
    Blue
}

main(): void {
    c := Colour::Red;

    print(c);
    print(Colour::Green);

    c = Colour::Blue;
    print(c);
}