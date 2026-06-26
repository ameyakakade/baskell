main() {
    extrn printf2; // TODO: Replace printf2 when working
    // It is important for the unary "not" to be applied to the nearest "0",
    // not to the whole "0 + 68". If the parser is implemented correctly the
    // output of this program is 69.
    printf2("%d*n", !0 + 68);
}