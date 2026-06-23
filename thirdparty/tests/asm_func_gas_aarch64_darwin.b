add __asm__(
    "add X0, X0, X1",
    "ret"
);

main() {
    extrn printf2; // TODO: Replace printf2
    printf2("%d*n", add(34, 35));
}
