main()
{
    fn();
    fn();
}
fn()
{
    anotherone();
    anotherone();
    anotherone();
}
anotherone()
{
    extrn printf;
    printf("Another one*n");
}
