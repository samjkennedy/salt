void foo(mut i64 x) {
    x = 10;
}

void main() {
    i64 i = 1;
    foo(i);
    print(i);
}