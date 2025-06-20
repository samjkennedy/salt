struct Vec2 { x: i64, y: i64 }

maybe(): ?Vec2 {
    return Vec2 { x: 3, y: 7 };
}

main(): void {
    v := maybe();
    print(v?.x);
    print(v?.y);

    a := [1, 2, v?.x, 4, 5, 6, v?.y, 8, 9];

    s := a[v?.x..v?.y];

    i := 0;
    while i < s.len {
        print(s[i]);
        i = i + 1;
    }

    if (v?.x > 0) {
        print(10);
    }
}