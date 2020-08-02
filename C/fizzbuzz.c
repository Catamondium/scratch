//#define _GNU_SOURCE
#include <string.h>		// strcat
#include <stdio.h>

struct {
    unsigned int key;
    char *data;
} dict[] = {
    {
    3, "Fizz"}, {
    5, "Buzz"}, {
7, "Baz"}};

const unsigned int DICTLEN = sizeof(dict) / sizeof(dict[0]);

// with GNU C asprintf, str & ret can have automatic length
const char *itoa(const unsigned int x)
{
    // adapt x to string and return it
    static char str[4];		// static omits automatic deallocation
    sprintf(str, "%u", x);
    return str;
}

int main()
{
    char ret[11];
    for (unsigned int i = 1; i <= 100; i++) {
	ret[0] = '\0';		// 'clear' string

	for (unsigned int j = 0; j < DICTLEN; j++) {
	    if (i % dict[j].key == 0)
		strcat(ret, dict[j].data);
	}

	printf("%s\n", strlen(ret) ? ret : itoa(i));
    }
}
