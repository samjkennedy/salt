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

    guard maybe(false) else {
        print(0);
        return;
    }
}