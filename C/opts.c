#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    int c;
    int aflag = 0;
    char *out;

    while ((c = getopt(argc, argv, "ao:")) != -1) {
	switch (c) {
	case 'a':
	    aflag = 1;
	    break;
	case 'o':
	    out = optarg;
	    break;
	}
    }

    if (optind < argc) {
	printf("Opts:\ta:%d\tout:%s\nArgv:\t", aflag, out);
	while (optind < argc)
	    printf("%s ", argv[optind++]);
	printf("\n");
    }
}
