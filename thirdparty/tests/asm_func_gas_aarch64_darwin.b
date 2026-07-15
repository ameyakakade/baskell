add __asm__(
    "add X0, X0, X1",
    "ret"
);

main() {
    extrn printf;
    printf("%d*n", add(34, 35));
}
