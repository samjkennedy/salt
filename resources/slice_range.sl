halve(sl: mut []i64): void {
    i: i64 = 0;
   while i < sl.len {
        sl[i] = sl[i] / 2;
        i = i + 1;
    }
}

main(): void {
    nums := [10, 20, 30, 40, 50, 60, 70, 80, 90];

    s := nums[3..6];

    halve(mut s);

    i := 0;
    while i < s.len {
        print(s[i]);
        i = i + 1;
    }
}