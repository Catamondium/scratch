#include <stdbool.h>
#include <stdlib.h>             // atoi
#include <string.h>             // strcat
#include <stdio.h>

int main(int argc, char *argv[])
{
    unsigned int bound = (argc > 1) ? atoi(argv[1]) : 100;

    int primes[bound];
    primes[0] = 2;
    unsigned int iter = 1;
    for (unsigned int i = 2; i <= bound; i++) {
        bool current = true;
        for (unsigned int j = 0; j < iter; j++) {
            if (i % primes[j] == 0) {
                current = false;
                break;
            }
        }

        if (current) {
            primes[iter] = i;
            iter++;
        }
    }

    for (unsigned int i = 0; i < iter; i++) {
        printf("%d\n", primes[i]);
    }
}
