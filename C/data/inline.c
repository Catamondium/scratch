#include <stdio.h>

struct {
    char *str;
    int type;
    int color;
} // anonymous struct table[] = {	// derived delcaration of it's type[]
    {
    "Cat", 1, 0xffffff}, {
    "Dog", 2, 0xff0000}, {
"Mouse", 3, 0x000000}};

int main()
{
    for (int i = 0; i < sizeof(table) / sizeof(table[0]); i++) {
	printf("%s:\t%d,%x\n", table[i].str, table[i].type,
	       table[i].color);
    }
}
