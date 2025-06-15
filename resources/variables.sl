main(): void {
    x: i64 = 10;
    y: i64 = x;
    b: bool = false;

    print(x);
    print(y);
    print(b);

    x = y = 5;
    b = true;

    print(x);
    print(y);
    print(b);
}