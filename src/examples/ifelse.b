main()
{
    extrn write, exit, printword;
    auto a;
    a = 10;
    while(a)
    {
        printword(a);
        a=a-1;
    }
    exit(0);
}
