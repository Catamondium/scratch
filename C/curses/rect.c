/* triangle.c */
#include <curses.h>
#include <stdlib.h>
#include <time.h>

#define ITERMAX 10000

/*unsigned int getrandom_int()
{
    return rand();
}*/

int height, width;

void horline(int y, int ch)
{
    move(y, 0);
    for (int i = 0; i < width; i++)
    {
        addch(ch);
    }
}

int main(void)
{
    /* initialize curses */
    initscr();
    cbreak();
    noecho();
    start_color();
    init_pair(1, COLOR_WHITE, COLOR_BLACK);
    clear();
    getmaxyx(stdscr, height, width);
    /* initialize triangle */
    horline(0, '*' | COLOR_PAIR(1));
    for (int i = 1; i < height - 1; i++)
    {
        mvaddch(i, 0, '+' | COLOR_PAIR(0));
        mvaddch(i, width - 1, '+' | COLOR_PAIR(0));
    }
    horline(height - 1, '*' | COLOR_PAIR(1));
    refresh();
    // Blocking get, 'press any key to exit'
    getch();
    endwin();
    exit(0);
}