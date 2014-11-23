/*
 * Play 2048
 *
 * 2048 is a single-player puzzle game, in which the objective is to slide
 * numbered tiles on a grid to combine them and create a tile with the number 2048.
 *
 * (c) 2014 by Kai Tomerius
 * License: GPLv2
 */

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timeb.h>
#include <time.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

int verbose = 0;

// the board and directions to move
typedef unsigned board[16];
typedef enum dir { left, right, up, down, last } dir;

static unsigned char* dirs[4] = { "left", "right", "up", "down" };

// row, column index and increments into board for all directions to move
static int start[4] = { 0, 3, 0, 12 };
static int row_inc[4]  = { 4, 4, 1, 1 };
static int col_inc[4]  = { 1, -1, 4, -4 };

// player strategies:
// - move up
// - biggest direct score gain
// - order numbers per row: first row left to right, second row right to left ...
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

// board_notation - generate board notation
// surround numbers by two levels of brackets, e.g.:
// [[2 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 2]]
const char* board_notation(board b) {
    static char notation[256];
    unsigned m, n;
    sprintf(notation, "[");
    for (n=4; n--; ) {
        sprintf(notation+strlen(notation), "[");
        for (m=4; m--; )
            sprintf(notation+strlen(notation), "%u%c", *(b+4*n+m), m ? ' ' : '\0');
        sprintf(notation+strlen(notation), "]%c", n ? ' ' : '\0');
    }
    sprintf(notation+strlen(notation), "]");
    return notation;
}

// connect_server - connect to a 2048 server through TCP/IP
static struct sockaddr_in addr;
static int fd = 0;
int connect_server(const char* ip) {
   bzero(&addr,sizeof(addr));
   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = inet_addr(ip);
   addr.sin_port = htons(2048);

   fd = socket(AF_INET, SOCK_STREAM, 0);
   connect(fd, (struct sockaddr*) &addr, sizeof(addr));
   return fd<0;
}

void disconnect_server() {
    if (fd>0)
        close(fd);

    fd = 0;
}

// send_server - send a command to the server an read the response
const char* send_server(const char* s) {
    static char cmd[256];
    sprintf(cmd, ":%s", s);
    sendto(fd, cmd, strlen(cmd), 0, (struct sockaddr*) &addr,sizeof(addr));
    int n = 0;
    do
        n += recvfrom(fd, cmd+n, sizeof(cmd)-n, 0, NULL, NULL);
    while (n==0 || n==1);
    cmd[n>0 ? n : 0] = 0;

    return cmd;
}

// parse_notation - parse a board notation from the server
int parse_notation(const char* s, board b) {
    unsigned m=0, n=16;
    while (n)
        switch (s[m]) {
        case '[': case ']': case ' ': case '\n': case '\r': m++;
            break;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            b[--n] = atoi(s+m);
            while (isdigit(s[m]))
                m++;
            break;
        default:
            return -1;
        }

    return (int) n;
}

// drop - randomly drop a 2 or 4 on an empty tile
unsigned drop(unsigned* b) {
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
                    // skip over empty tiles
                    b[to*i] = b[from*i];
                    b[from*i] = 0;
                    *moved += 1;
                } else if (b[to*i]==b[from*i]) {
                    // join tiles with the same number
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

    // prefered order of tiles on the board
    unsigned i, o[] = { 15, 14, 13, 12, 8, 9, 10, 11, 7, 6, 5, 4, 0, 1, 2, 3 };

    dir bd = up;
    unsigned bs = 0;

    if (strategy==s_lr) {
        for (i=0; i<16; i++) {
            // find the first tile affected by the move
            if (!b[o[i]] || (i%4!=3 && b[o[i]]==b[o[i+1]])) {
                if (i/4%2==1) {
                    // move the second, fourth row to the right
                    dd[1] = right;
                    dd[2] = left;
                }

                if (!b[o[i]]) {
                    // prefer to move left or right to fill empty tiles
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
        if (moved && drop(c)) {
            // evaluate the move according to strategy
            switch (strategy) {
            case s_lr: {
                // check the order of numbers on the board
                for (i=1; i<16; i++) {
                    if (b[o[i]] >= b[o[i-1]])
                        // add up numbers which are in proper order
                        s += b[o[i-1]];
                }
            } // fall through to find the best move ...

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
    const char* server = NULL;

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
        } else if (argc>2 && !strcmp(argv[argc-2], "--server")) {
            server = argv[argc-1];
            argc -= 2;
        } else {
            if (!strcmp(argv[argc-1], "-h"))
                argc--;
            else
                printf("%s: unknown option %s\n", basename((char*) argv[0]), argv[argc-1]);

            printf("usage: %s [--average] [--highscore] [--lr|--score|--up] [--server <ip-address>] [-v]\n", basename((char*) argv[0]));
            return argc-1;
        }

    board b; // board
    dir d = (dir) -1; // last move
    unsigned hs = 0; // highscore
    unsigned as = 0, an = 0; // average score
    while (tries--) {
        unsigned s = 0;
        int moved;
        playouts++;

        if (server) {
            // connect to server
            if (connect_server(server)) {
                printf("failure to communicate %s\n", server);
                return -1;
            }
        } else {
            // create a board
            memset(b, 0, sizeof(b));
            drop(b);
            drop(b);
        }

        // move until the board is full
        do {
            if (server &&
                ((d<last && !send_server(dirs[d])) ||
                 parse_notation(send_server("board"), b))) {
                printf("failure to communicate %s\n", server);
                return -1;
            }

            if (verbose)
                print_board(b);
        } while (
            // simple strategy: move up, left, right or down, whatever works
            (strategy==s_up && (
                ((s += move(b, d = up,    &moved)), (moved && drop(b))) ||
                ((s += move(b, d = left,  &moved)), (moved && drop(b))) ||
                ((s += move(b, d = right, &moved)), (moved && drop(b))) ||
                ((s += move(b, d = down,  &moved)), (moved && drop(b))))) ||
            // other strategies: evaluate moves
            (strategy!=s_up && (
                ((s += move(b, d = evaluate(b, strategy), &moved)), (moved && drop(b))))));

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

        // disconnect the server, if any
        if (server)
            send_server("gameover");
        disconnect_server();
    }

    return 0;
}
