#include <stdio.h>

void check(int v)
{
    if (v > 0)
        printf("True %d\n", v);
    else
        printf("False %d\n", v);
}

int main()
{
    check(1);
    check(0);
}
