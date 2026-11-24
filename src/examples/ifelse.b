main()
{
    extrn write, exit, printf;
    auto a,b,c;
    a = 7;
    b = 1;
    c = 1;
    while(a){
        if(c){
            c=0;
            if(b){
                b=0;
                printf("wow*n");
            }else{
                printf("owo*n");
            }
        }else if(b){
            printf("ooo*n");
            c=1;
        }else{
            printf("www*n");
            c=1;
        }
        a=a-1;
    }
    exit(0);
}
