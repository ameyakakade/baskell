main()
{
    extrn write, exit, putstr, putchar, getchar;
    auto a,b,c;
    a = 5;
    b = 0;
    c = 1;
    while(a){
        if(c){
            c=0;
            if(b){
                b=0;
                putstr("wow*0");
            }else{
                putstr("owo*0");
            }
        }else if(b){
            putstr("ooo*0");
            c=1;
        }else{
            putstr("www*0");
            c=1;
        }
        a=a-1;
    }
    exit(0);
}
