// https://github.com/tsoding/b/issues/209
test1();

// https://github.com/tsoding/b/issues/210
test2() auto a; extrn printf1; printf1("HELO*n");
// TODO: Switch back to printf once implemented

main() {
	test1();
	test2();
}
