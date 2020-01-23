#include <stdio.h>

enum states {
    before = 0,
    inside = 1,
    after = 2
};

struct branch {
    unsigned char new_state:2;  //BIT [1:0]
    unsigned char should_putchar:1;     //BIT [2]
};

struct branch the_table[3][3] = {
    /*                               ' '         '\n'        others */
    /* before */ {{before, 0}, {before, 1}, {inside, 1}},
    /* inside */ {{after, 1}, {before, 1}, {inside, 1}},
    /* after  */ {{after, 0}, {before, 1}, {after, 0}}
};

void step(enum states *state, int c)
{
    // Automation step
    int charCase = (c == ' ') ? 0 : (c == '\n') ? 1 : 2;

    struct branch *b = &the_table[*state][charCase];
    *state = (enum states) (b->new_state);
    if (b->should_putchar)
        putchar(c);
}

int main(void)
{
    /* Read stdin sentence
     * Print first word
     */
    enum states state = before;

    int c;
    while ((c = getchar()) != EOF) {
        step(&state, c);
    }

    if (state != before)
        putchar('\n');
    return 0;
}
