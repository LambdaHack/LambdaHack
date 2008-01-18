
#include "utils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in Yi/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x) {
    getyx(win, *y, *x);
}

/* A non-macro version of COLOR_PAIR(3)
 */
int get_color_pair (int pair) {
    return COLOR_PAIR (pair);
}

/*
 * Specialised packed hGetLine. The caller should copy out any string it
 * is interested in. Additionally, we drop redundant @F packets arriving --
 * there's too many anyway
 *
 * Note that mpg321 (only) provides --skip-printing-frames=N
 * I guess we could have used that.
 */
#define BUFLEN 1024

#define DROPRATE 10

int FRAME_COUNT = 0;    /* we count frame packets, and drop 9/10 of them */
                        /* setting this to 10 will force the next
                         * packet to be returned, no matter what */

/* when skipping frames, we want to ensure we don't drop any packets,
 * for reasonable performance on updates. This trick does that */
void forcenext(void) {
    FRAME_COUNT = DROPRATE ;
}

/* sometimes we write to the wrong spot after a refresh */
int getline(char *buf, FILE *hdl) { 
    char *p;
    int c;

    /* read first two bytes of packet, to work out if we drop it */
    getc(hdl);      /* should be '@' */
    c = getc(hdl);

    /* drop packet */
    if (c == 'F' && FRAME_COUNT < DROPRATE ) {
        FRAME_COUNT++;

        while (c != '\n') 
            c = getc(hdl);
        return getline(buf,hdl);        /* read another line */

    /* normal packet */
    } else {
        if (c == 'F') FRAME_COUNT = 0;    /* reset frame count */

        p = fgets(buf+1, BUFLEN-1, hdl);  /* read rest of line */
        if (p == NULL) {
        //  perror("getline failed\n");
            return (-1);
        }
        buf[0] = c;         /* drop the '@' */
        return strlen(buf); /* return length so we can realloc */
    }
}
            
/* given a file descriptor (presumably got from a Haskell Handle) , open
 * a FILE * stream onto that fd. Don't try to use the Handle after this 
 *
 * could be done in Haskell...
 */
FILE *openfd(int fd) {
    FILE *file = NULL;

    if ((file = fdopen(fd, "r")) != NULL) {
         return file;
    } else {
         perror("cbits.openfd failed\n\n");
         close(fd);
         return NULL;
    }
}
