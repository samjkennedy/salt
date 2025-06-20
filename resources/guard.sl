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

    b := guard x < 5 else {
        return;
    }
    print(b);
}