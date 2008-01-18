#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <curses.h>

#include "config.h"

/* curses */
extern void nomacro_getyx(WINDOW *win, int *y, int *x);
extern int get_color_pair (int pair);

/* packed string IO */
FILE *openfd(int fd);
int getline(char *buf, FILE *hdl);
void forcenext(void);
