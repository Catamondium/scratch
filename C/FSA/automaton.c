#include <stdio.h>
enum states
{
	before = 0,
	inside = 1,
	after = 2
};

void step(enum states *state, int c)
{
	// Automation step
	if (c == '\n')
	{
		putchar('\n');
		*state = before;
	}
	else
		switch (*state)
		{
		case before:
			if (c != ' ')
			{
				putchar(c);
				*state = inside;
			}
			break;

		case inside:
			if (c == ' ')
			{
				*state = after;
			}
			else
			{
				putchar(c);
			}
			break;

		case after:
			break;
		}
}

int main(void)
{
	/* Read stdin sentence
	 * Print first word
	 */
	enum states state = before;

	int c;
	while ((c = getchar()) != EOF)
	{
		step(&state, c);
	}

	if (state != before)
		putchar('\n');
	return 0;
}
