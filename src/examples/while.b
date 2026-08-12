main()
{
    extrn exit, printf, putchar;
    auto a;
    a = 10;
    while(a)
    {
        auto b;
        b=10;
        while(b){
            b=b-1;
            printf("%d ", a+b);
        }
        a=a-1;
        putchar(10);
    }
    exit(0);
}
