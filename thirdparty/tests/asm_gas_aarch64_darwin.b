foo() {
    __asm__(
    "MOV X0, #3",
    "LDP LR, FP, [SP], #16",
    "RET"
    );
}

main() {
    extrn printf;
    printf("%d*n", foo());
}
