#include <stdio.h>
// Example maths defs
int add(int x, int y)
{
    return x + y;
}

int sub(int x, int y)
{
    return x - y;
}

int mul(int x, int y)
{
    return x * y;
}

int div(int x, int y)
{
    return x / y;
}

typedef struct Ords {
    int x;
    int y;
} Ords;
// Function array
int (*maths[4]) (int x, int y) = {
add, sub, mul, div};
char *ops[] = { "add", "sub", "mul", "div" };

void Ords_maths(Ords v)
{
    Ords vec = v;
    for (int i = 0; i < 4; i++) {
        int val = maths[i] (vec.x, vec.y);
        printf("Ords_maths[%s]:\t%+d\t*:%p\n", ops[i], val, maths[i]);
    }
}

typedef int (*doublemaths) (int, int);  // Simplify declaration

void do_maths(doublemaths f)
{
    int val = (*f) (4, 2);      // Derefence and apply
    printf("do_maths:\t\t%+d\t*:%p\n", val, f);
}

int main()
{
    Ords vec = {.x = 4,.y = 2 };
    Ords *pvec = &vec;
    printf(".\t\t%+d\t%+d\n", vec.x, vec.y);    // direct access
    printf("->\t\t%+d\t%+d\t*:%p\n\n", pvec->x, pvec->y, pvec); // -> indirect access
    Ords_maths(vec);
    do_maths(sub);
}
