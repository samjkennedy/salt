sum(a: u32, b: u32) => a + b;

foo(): void {
    println(sum(10, 11));
}

main() => foo();