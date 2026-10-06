main()
{
    extrn malloc, putchar, printword;
    auto name, a, b, c, d;
    name = malloc(0);
    *name = 56;
    putchar(*name);
    a = 5595;
    b = a*4;
    c = a + b/c;
    d = 434 + 33 * c / b;
    printword(a);
    putchar('*n');
    printword(b);
    putchar('*n');
    putchar('*n');
    printword(c);
    printword(d);
    return 0;
}
