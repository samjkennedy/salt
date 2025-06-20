maybe(b: bool): ?i64 {
    if b {
        return 10;
    } else {
        return;
    }
}

main(): void {
    x := guard maybe(true) else {
        print(0);
        return;
    };
    print(x);

    b := guard x < 25 else {
        return;
    };
    print(b);

    a := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    for i in a {
        print(i);
        guard i < 5 else {
            continue;
        }
        print(i);
    }

    guard maybe(false) else {
        print(0);
        return;
    }
}