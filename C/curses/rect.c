#include <curses.h>
#include <stdlib.h>

int height, width;

void horline(int y, int ch)
{
    move(y, 0);
    for (int i = 0; i < width; i++) {
        addch(ch);
    }
}

int main(void)
{
    /* initialize curses */
    initscr();
    start_color();
    cbreak();
    noecho();
    clear();
    getmaxyx(stdscr, height, width);

    init_pair(1, COLOR_GREEN, COLOR_YELLOW);

    // Draw
    horline(0, '*' | COLOR_PAIR(0));
    for (int i = 1; i < height - 1; i++) {
        mvaddch(i, 0, '#' | COLOR_PAIR(1));
        mvaddch(i, width - 1, '#' | COLOR_PAIR(1));
    }
    horline(height - 1, '*' | COLOR_PAIR(0));
    refresh();
    //

    getch();                    // Blocking get, 'press any key to exit'
    endwin();
}
