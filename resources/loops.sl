struct Vec3 { x: i64, y: i64, z: i64 }

main(): void {

    i := 0;
    while i < 10 {
        i = i + 1;
        if (i < 3) {
            continue;
        }

        print(i);

        if (i > 6) {
            break;
        }
    }

    a := [10, 20, 30, 40, 50];
    for i in a {
        print(i);
    }

    s := a[2..4];
    for i in s {
        print(i);
    }

    vs := [
        Vec3 { y: 5, x: 4, z: 8},
        Vec3 { y: 6, x: 3, z: 9}
    ];

    for v in vs {
        print(v.x);
        print(v.y);
        print(v.z);
    }

    for x in a[0]..a[1] {
        print(x);
    }
}