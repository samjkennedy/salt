main(): void {
    x: [3]i64 = [10, 20, 30];

    i: i64 = 0;
    while i < x.len {
        print(x[i]);
        i = i + 1;
    }

    x[1] = 5;
    j: i64 = 0;
    while j < x.len {
        print(x[j]);
        j = j + 1;
    }
}   