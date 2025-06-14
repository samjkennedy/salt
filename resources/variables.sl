void main() {
    i64 x = 10;
    i64 y = x;
    bool b = false;

    print(x);
    print(y);
    print(b);

    x = y = 5;
    b = true;

    print(x);
    print(y);
    print(b);
}