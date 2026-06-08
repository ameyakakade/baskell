main()
{
    extrn write, exit, printword, putchar;
    auto a;
    a = 10;
    while(a)
    {
        auto b;
        b=10;
        while(b){
            b=b-1;
            printword((a+b));
            write(" ", 1);
        }
        a=a-1;
        putchar(10);
    }
    exit(0);
}
