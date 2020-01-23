#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define kElements 125000

struct intList {
    int val;
    int index;
    struct intList *next;
};

int sumList(struct intList *list)
{                               // Sum all elements
    int sum = 0;

    while (list != NULL) {
        sum = sum + list->val;
        list = list->next;
    }
    return sum;
}

int main()
{
    clock_t clk;
    struct intList *mainll = NULL;      //, *tmpll;
    int sumTime = 0;

    printf("Initializing list");

    // Generate random numbers <100
    for (int i = 0; i < kElements; i++) {
        // create element
        // allocate element
        struct intList *tmpll = calloc(1, sizeof(struct intList));
        tmpll->val = random() % 100;    // set element value
        tmpll->index = i;

        // prepend element
        tmpll->next = mainll;   // next is previous segment
        mainll = tmpll;         // current segment to mainll
    }

    printf("\nTiming...\n");

    for (int i = 0; i < 100; i++) {
        clk = clock();

        int sum = sumList(mainll);

        clk = clock() - clk;
        sumTime += clk;
        printf("%04ld ticks,\tsum %d\n", clk, sum);
    }

    printf("Average time:\t%f ticks\n", ((double) sumTime) / 100.0);
    //getchar(); // exit on return
}
