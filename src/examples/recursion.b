main()
{
    extrn printword,exit;
    auto a;
    a=factorial(5);
    printword(a);
    exit(0);
}
factorial(i)
{
    if(i-1){
        auto b;
        b=factorial((i-1));
        b=b*i;
        return b;
    }else{
        return (1);
    }
}
