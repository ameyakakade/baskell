/* this program uses recursion
   to find out factorial of number */
main()
{
    extrn printword, exit, putchar;
    auto a;
    a=factorial(3);
    printword(a);
    putchar('*n');
    exit(0);
}
factorial(i)
{
    if(i>1){
        auto b;
        b=factorial(i-1);
        b=b*i;
        return b;
    }else{
        return 1;
    }
}
/*  */
