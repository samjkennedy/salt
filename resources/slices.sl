main(args: str[]): void {
    nums: [5]i64 = [10, 20, 30, 40, 50];

    mid: []i64 = nums[1..4];  // slice: [20, 30, 40]

    for (i32 n in mid) {
        printf("%d\n", n);
    }
}