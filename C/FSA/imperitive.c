#include <stdio.h>
#include <ctype.h> // isspace
int main(void)
{
	/* Read stdin sentence
	 * Print first word
	 */
	int c;
	do
	{
		do
		{
			c = getchar();
		} while (c == ' ');

		while (c != EOF && !isspace(c) && c != '\n')
		{
			putchar(c);
			c = getchar();
		}

		putchar('\n');

		while (c != EOF && c != '\n')
			c = getchar();
	} while (c != EOF);

	return 0;
}
