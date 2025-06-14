main(): void {
    a: i64 = 0;
    b: i64 = 1;
    c: i64 = 1;
    while (a < 1000000) {
        print(a);
        c = a + b;
        a = b;
        b = c;
    }
}