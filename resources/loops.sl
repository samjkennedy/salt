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
}