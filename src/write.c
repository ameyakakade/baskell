#include <stdio.h>

void write(char* str, int len)
{
    for(int i=0; i<len; i++)
    {
        putchar(*(str+i));
    }
}

void printword(size_t n)
{
    printf("%zd", n);
}

void putstr(char* nullTerminatedStr)
{
    printf("%s", nullTerminatedStr);
}

size_t printf1(size_t a1) {return printf((void*)a1);}
size_t printf2(char* a1, size_t a2) {return printf(a1, (void*)a2);}

char gchar(char* str, int index)
{
    return str[index];
}
