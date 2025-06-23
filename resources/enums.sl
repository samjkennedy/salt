enum Colour {
    Red,
    Green,
    Blue
}

to_colour(i: i64): Colour {
    if i == 0 {
        return Colour::Red;
    } else if i == 1 {
        return Colour::Green;
    } else {
        return Colour::Blue;
    }
}

main(): void {
    c := Colour::Red;

    print(c);
    print(Colour::Green);

    c = Colour::Blue;
    print(c);

    for i in 0..5 {
        print(to_colour(i));
    }
}