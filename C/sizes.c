#include <assert.h>
#include <stdio.h>

// Basic for for sizeof test
typedef struct {
    int x, y;
} Vec;

// Form providing alternative 'views'
typedef union {
    // see Vec
    struct {
        int x, y;
    };
    // as seq of ints
    int raw[2];
} VecUn;

int main()
{
    assert(sizeof(Vec) == 2 * sizeof(int));
    assert(sizeof(VecUn) == 2 * sizeof(int));
    printf("Views example\n");

    VecUn v = {.x = 1,.y = 2 };
    printf(".x = %d, .y = %d\n", v.x, v.y);
    printf("[0] = %d, [1] = %d\n", v.raw[0], v.raw[1]);
}
