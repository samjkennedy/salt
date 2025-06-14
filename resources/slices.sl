void main(str[] args) {
    i32[5] nums = [10, 20, 30, 40, 50];

    i32[] mid = nums[1..4];  // slice: [20, 30, 40]

    for (i32 n in mid) {
        printf("%d\n", n);
    }
}