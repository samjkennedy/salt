maybe(x: i64): ?i64 {
    if (x < 10) {
        return 5;
    }
    return;
}

foo(): ?i64 {
    i := maybe(3)?;

    return i * 10;
}

main(): void {
    x := foo()?;

    println(x);

    i := 0;
    while i < 20 {
        println(i);
        m := maybe(i)?;
        println(m);

        i = i + 1;
    }
}