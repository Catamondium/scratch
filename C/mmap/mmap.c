#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#define iferr(call) onerr(call, __LINE__)
void onerr(int call, int line)
{
    if (call == -1) {
	perror("Error");
	printf("line: %d\n", line);
	exit(EXIT_FAILURE);
    }
}

int main(int argc, char const *argv[])
{
    // prepare scratch file
    remove("file");
    iferr(system("cp o_file file"));

    iferr(system("cat file"));

    int fd = open("file", O_RDWR);
    iferr(fd);

    struct stat statbuf;
    iferr(fstat(fd, &statbuf));
    char *cursor =
	mmap(NULL, statbuf.st_size, PROT_WRITE, MAP_SHARED, fd, 0);
    iferr(close(fd));
    if (cursor == MAP_FAILED)
	iferr(-1);
    printf("base: %p\n", cursor);
    for (size_t i = 0; i < statbuf.st_size; ++i)
	cursor[i] = 'X';
    iferr(msync(cursor, statbuf.st_size, MS_SYNC));
    iferr(munmap(cursor, statbuf.st_size));
    iferr(system("cat file"));
}
