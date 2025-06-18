halve(sl: mut []i64): void {
    i: i64 = 0;
   while i < sl.len {
        sl[i] = sl[i] / 2;
        i = i + 1;
    }
}

sl1(): void {
    nums: [5]i64 = [10, 20, 30, 40, 50];

    s: []i64 = &nums;

    i: i64 = 0;
    while i < s.len {
        print(s[i]);
        i = i + 1;
    }

    halve(mut s);

    i = 0;
    while i < s.len {
        print(s[i]);
        i = i + 1;
    }

    print(*s.data);
}

sl2(): void {
    
}

main(): void {
    sl1();
}