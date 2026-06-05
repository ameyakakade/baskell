#include <stdio.h>

void write(char* str, int len)
{
    for(int i=0; i<len; i++)
    {
        putchar(*(str+i));
    }
}
