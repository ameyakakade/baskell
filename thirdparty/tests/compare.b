assert_equal(actual, expected, message) {
    extrn printf2, abort; // TODO: Change back to printf when variadics works
    printf2("%s: ", message);
    if (actual != expected) {
        printf2("FAIL*n");
        abort();
    } else {
        printf2("OK*n");
    }
}

main() {
    extrn assert_equal;
    assert_equal(5 == 3, 0, "5 == 3");
    assert_equal(3 == 3, 1, "3 == 3");
    assert_equal(5 != 3, 1, "5 != 3");
    assert_equal(3 != 3, 0, "3 != 3");
    assert_equal(5 >= 3, 1, "5 >= 3");
    assert_equal(3 >= 5, 0, "3 >= 5");
    assert_equal(3 >= 3, 1, "3 >= 3");
    assert_equal(3 >  3, 0, "3 >  3");
    assert_equal(5 >  3, 1, "5 >  3");
}
