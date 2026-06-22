foo() {
    __asm__(
    "MOV X0, #3",
    "LDP LR, FP, [SP], #16",
    "RET"
    );
}

main() {
    extrn printf2; // TODO: Replace printf2 after adding variadics
    printf2("%d*n", foo());
}
