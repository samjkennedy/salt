foo(p: mut *i64): void {
    *p = 5;
}

print_pointer(p: *i64): void {
    print(*p);
}

main(): void {
    x: i64 = 15;
    ptr: *i64 = &x;

    print_pointer(ptr);
    foo(ptr);
    print_pointer(ptr);

    print(x);
}