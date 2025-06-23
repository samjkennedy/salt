choice_return(i: i64): i64 {
    if (i < 8) {
        return 0;
    } else if (i < 16) {
        return 1;
    }
    return 2;
}

main(): void {
    x := 10;

    println(choice_return(x));
    println(choice_return(x + x));
    println(choice_return(0));
}