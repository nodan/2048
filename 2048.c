/*
 * A 2048 player.
 *
 * (c) Kai Tomerius
 * GPLv2
 */

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timeb.h>
#include <time.h>
#include <unistd.h>

int verbose = 0;

// the board and directions to move
typedef unsigned board[16];
typedef enum dir { left, right, up, down } dir;

static unsigned char* dirs[4] = { "left", "right", "up", "down" };

// row, column index and increments into board for all directions to move
static int start[4] = { 0, 3, 0, 12 };
static int row_inc[4]  = { 4, 4, 1, 1 };
static int col_inc[4]  = { 1, -1, 4, -4 };

// player strategies: move up, biggest direct score gain, order numbers per row: left - right - left ...
typedef enum strategy { s_up, s_score, s_lr } strategy;

// print_row - print one row of the board
void print_row(unsigned* b, int i) {
    unsigned n;
    for (n=4; n--; )
        printf("%5d%c", b[n*i], n ? ' ' : '\n');
}

// print_board - print the board
void print_board(board b) {
    unsigned n;
    for (n=4; n--; )
        print_row(b+4*n, 1);
    printf("\n");
}

// fill - randomly put a 2 or 4 on the board
unsigned fill(unsigned* b) {
    unsigned n, f=0, r=rand()&~(1<<31);
    for (n=16; n--; )
        f += !b[n] ? 1 : 0;

    if (f)
        for (n=16; n--; )
            if (!b[n] && !(r--%f))
                b[n] = r%100<90 ? 2 : 4;

    return f;
}

// move_row - move a row
unsigned move_row(unsigned* b, int i, int* moved) {
    unsigned s=0;
    int to=4;
    while (to-->0) {
        int from=to;
        while (from-->0) {
            if (b[from*i]) {
                if (!b[to*i]) {
                    // skip over 0s
                    b[to*i] = b[from*i];
                    b[from*i] = 0;
                    *moved += 1;
                } else if (b[to*i]==b[from*i]) {
                    // field with the same number
                    b[to*i] += b[from*i];
                    s += b[to*i];
                    b[from*i] = 0;
                    *moved += 1;
                }
                break;
            }
        }
    }

    return s;
}

// move - move into the given direction
unsigned move(board b, dir d, int* moved) {
    unsigned i;
    unsigned s=0;

    *moved = 0;
    for (i=0; i<4; i++) {
        s += move_row(&b[start[d]+i*row_inc[d]], col_inc[d], moved);
    }

    return s;
}

// clone - create a copy of the board
unsigned* clone(board b) {
    unsigned* c = (unsigned*) malloc(sizeof(board));
    memcpy(c, b, sizeof(board));
    return c;
}

// evaluate - move according to the given strategy
dir evaluate(board b, strategy strategy) {
    int d;

    // prefered directions to move
    dir dd[] = { up, left, right, down };

    // prefered order of fields on the board
    unsigned i, o[] = { 15, 14, 13, 12, 8, 9, 10, 11, 7, 6, 5, 4, 0, 1, 2, 3 };

    dir bd = up;
    unsigned bs = 0;

    if (strategy==s_lr) {
        for (i=0; i<16; i++) {
            // find the first field affected by the move
            if (!b[o[i]] || (i%4!=3 && b[o[i]]==b[o[i+1]])) {
                if (i/4%2==1) {
                    // move the second, fourth row to the right
                    dd[1] = right;
                    dd[2] = left;
                }

                if (!b[o[i]]) {
                    // move up to fill up rows
                    dd[0] = dd[1];
                    dd[1] = up;
                }
                break;
            }
        }
    }

    // try to move in all directions
    for (d=0; d<4; d++) {
        unsigned* c = clone(b);
        int moved;
        unsigned s = move(c, dd[d], &moved);
        if (moved && fill(c)) {
            // evaluate the move
            switch (strategy) {
            case s_lr: {
                // check the order of numbers on the board
                for (i=0; i<16; i++) {
                    if (b[o[i]] >= b[o[i-1]])
                        s += b[o[i]];
                }
            } // fall through ...

            case s_score:
                // use the move with the best score
                if (s+1>bs) {
                    bd = dd[d];
                    bs = s+1;
                }
                break;
            }
        }

        free(c);
    }

    if (verbose)
        printf("move %s\n", dirs[bd]);

    return bd;
}

int main(int argc, const char** argv) {
    struct timeb t;
    ftime(&t);
    srand(1000*t.time+t.millitm);

    unsigned tries = 1, playouts = 0;
    int average = 0;
    strategy strategy = s_up;

    // parse command line options
    while (argc>1)
        if (!strcmp(argv[argc-1], "-v")) {
            argc--, verbose = 1;
        } else if (!strcmp(argv[argc-1], "--average")) {
            argc--, average = 1;
        } else if (!strcmp(argv[argc-1], "--highscore")) {
            argc--, tries = ~0;
        } else if (!strcmp(argv[argc-1], "--up")) {
            argc--, strategy = s_up;
        } else if (!strcmp(argv[argc-1], "--score")) {
            argc--, strategy = s_score;
        } else if (!strcmp(argv[argc-1], "--lr")) {
            argc--, strategy = s_lr;
        } else {
            if (!strcmp(argv[argc-1], "-h"))
                argc--;
            else
                printf("%s: unknown option %s\n", basename((char*) argv[0]), argv[argc-1]);

            printf("usage: %s [--average] [--highscore] [--lr|--score|--up] [-v]\n", basename((char*) argv[0]));
            return argc-1;
        }

    board b;
    unsigned hs = 0; // highscore
    unsigned as = 0, an = 0; // average score
    while (tries--) {
        unsigned s = 0;
        int moved;
        playouts++;

        // create a board
        memset(b, 0, sizeof(b));
        fill(b);
        fill(b);

        // move until the board is full
        do
            if (verbose)
                print_board(b);
        while (
            // simple strategy: move up, left, right or down, whatever works
            (strategy==s_up && (
                ((s += move(b, up,    &moved)), (moved && fill(b))) ||
                ((s += move(b, left,  &moved)), (moved && fill(b))) ||
                ((s += move(b, right, &moved)), (moved && fill(b))) ||
                ((s += move(b, down,  &moved)), (moved && fill(b))))) ||
            // other strategies: evaluate moves
            (strategy!=s_up && (
                ((s += move(b, evaluate(b, strategy), &moved)), (moved && fill(b))))));

        // calculate the average score
        as += s;
        an++;

        // print the score/board
        if (s>hs) {
            printf("score %u (%u)\n", s, playouts);
            print_board(b);
            hs = s;
        } else if (average && tries%16384==0) {
            printf("avg.  %u\n", as/an);
        }
    }

    return 0;
}
