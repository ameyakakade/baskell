main() {
    extrn printf2; // TODO: Replace printf2 when working
    auto fmt;
    // This must be `llu` and not `lu` because on windows `long` is 32-bits
    printf2("%llu*n", 69);
    printf2("%llu*n", 1000000);
    printf2("%llu*n", 123456789987654321);
}
