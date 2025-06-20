main(): void {
    s: String = "Hello from salt!";
    print(s);

    for i in 0..s.len {
        print(s[0..s.len - i]);
    }

    for i in 0..s.len {
        print(s[i..s.len]);
    }

    for i in 0..(s.len/2) {
        print(s[i..s.len - i]);
    }

    ss := [s, "World!"[1..4], "foo", "bar"];

    for s in ss {
        print(s);
        print(s.len);
    }

    d := "Hello".data;

    print(d);
}