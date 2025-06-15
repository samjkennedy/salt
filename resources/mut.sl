foo(x: mut i64): void {
    x = 10;
}

main(): void {
    i: i64 = 1;
    foo(i);
    print(i);
}