struct Vec2 { x: i64, y: i64 }

sum(v: Vec2): i64 {
    return v.x + v.y;
}

zero_vec(v: mut *Vec2): void {
    v.x = 0;
    v.y = 0;
}

main(): void {
    v: Vec2 = Vec2 { y: 5, x: 4};

    print(sum(v));

    zero_vec(mut &v);

    print(sum(v));
}