sum(a: i64, b: i64): void {
    print(a + b);
}

main(): void {
    sum(10, 11);

    x: i64 = 15;
    sum(x, x + 1);
}