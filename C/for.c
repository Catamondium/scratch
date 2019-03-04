#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef char *string;
int increment = 0;
const string phases[] = {"Init", "Cond", "Iter", "Body"};

void mark(const string phase)
{
	printf("FOR[%d]:\t%s\n%s", increment++, phase,
		   strcmp(phase, phases[2]) ? "" : "\n");
	return;
}

int init()
{
	mark("Init");
	return 0;
}

bool condition(int i)
{
	mark("Cond");
	return (i < 2) ? true : false;
}

int iterate()
{
	mark("Iter");
	return 1;
}

int main()
{
	for (int i = init(); condition(i); i += iterate())
	{
		mark("Body");
	}
}
