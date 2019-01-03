#include <stdio.h>

typedef signed int numeric;

numeric linear(numeric x) {
	return x*2 + 20;
}

numeric quadratic(numeric x) {
	return 2*x*x + 20;
}

numeric (*gen[2])(numeric x) = {linear, quadratic};
int main() {
	char *in[] = {"linear.tsv", "quadratic.tsv"};
	int num = sizeof(in)/sizeof(in[0]);

	for(int i = 0; i < num; i++) {
		FILE *out = fopen(in[i], "w+");
		fprintf(out, "X\tY\n"); // Header

		for(numeric x = -20; x < +20; x++) { // Body
			fprintf(out, "%+04d\t%+04d\n", x, gen[i](x));
		}

		fclose(out);
	}
}
