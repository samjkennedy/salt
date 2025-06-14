i64 choice_return(i64 i) {
    if (i < 8) {
        return 0;
    } else if (i < 16) {
        return 1;
    }
    return 2;
}

void main() {
    i64 x = 10;

    print(choice_return(x));
    print(choice_return(x + x));
    print(choice_return(0));
}