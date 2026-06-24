test(n) {
	extrn printf3;
	printf3(
		"%d:*t%s*n*0", n,
		n == 69 ? "69*0" :
		n == 420 ? "420*0" :
		n < 69 ? "..69*0" :
		n >= 420 ?
        n > 1337 ? "1337..*0" : "420..=1337*0" // remember to add "n != 1337" condition after implementing '&' operator
        : "69..420*0"
	);
}
main(argc, argv) {
	test(0);
	test(42);
	test(69);
	test(96);
	test(420);
	test(690);
	test(1337);
	test(4269);
    return 0;
}
