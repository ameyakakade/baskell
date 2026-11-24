main()
{
    auto a;
    extrn exit, printf;
    a = 1;
    switch a
    {
        case 0: printf("The number was zero*n");
        case 1: {
            printf("The number was one*n");
            printf("and this switch st holds a block*n");      
        }
        case 2: printf("The number was two*n");
    }
    exit(0);
}
