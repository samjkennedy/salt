void main() {
    i64 a = 0;
    i64 b = 1;
    i64 c = 1;
    while (a < 1000000) {
        print(a);
        c = a + b;
        a = b;
        b = c;
    }
}