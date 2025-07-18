ptr_set(p: mut *i64, val: i64): void {
    *p = val;
}

print_pointer(p: *i64): void {
    print(*p);
}

main(): void {
    x: i64 = 15;
    ptr: *i64 = &x;

    print_pointer(ptr);
    ptr_set(mut &x, 5);
    print_pointer(ptr);

    print(x);
}