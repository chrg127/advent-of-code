/* this is part 2 only, but part 1 is very similar anyway */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 70
#define START 35

static char cubes[MAX][MAX][MAX][MAX];
static char copy[MAX][MAX][MAX][MAX];

char *mygetline(FILE *f)
{
    size_t len = 0;
    int c, i;
    char *res;

    while (c = fgetc(f), c != '\n' && c != EOF) len++;
    if (len == 0 && c == EOF)
        return NULL;
    fseek(f, -len-1, SEEK_CUR);
    res = malloc(sizeof(char)*(len+1));
    for (i = 0; i <= len; i++)
        res[i] = fgetc(f);
    res[i-1] = '\0';
    return res;
}

int check_plane(char cubes[MAX][MAX], size_t y, size_t x)
{
    char a = cubes[y-1][x-1]; char b = cubes[y-1][x  ];
    char c = cubes[y-1][x+1]; char d = cubes[y  ][x-1];
    char e = cubes[y  ][x  ]; char f = cubes[y  ][x+1];
    char g = cubes[y+1][x-1]; char h = cubes[y+1][x  ];
    char i = cubes[y+1][x+1];
    return (a == '#') + (b == '#') + (c == '#') + (d == '#') +
           (e == '#') + (f == '#') + (g == '#') + (h == '#') + (i == '#');
}

int check3d(char cubes[MAX][MAX][MAX], size_t z, size_t y, size_t x)
{
    return check_plane(cubes[z], y, x) + check_plane(cubes[z-1], y, x) + check_plane(cubes[z+1], y, x);
}

int check4d(char cubes[MAX][MAX][MAX][MAX], size_t w, size_t z, size_t y, size_t x)
{
    return check3d(cubes[w], z, y, x) + check3d(cubes[w-1], z, y, x) + check3d(cubes[w+1], z, y, x);
}

void step(char src[MAX][MAX][MAX][MAX], char next[MAX][MAX][MAX][MAX])
{
    for (size_t w = 1; w < MAX-1; w++) {
        for (size_t z = 1; z < MAX-1; z++) {
            for (size_t y = 1; y < MAX-1; y++) {
                for (size_t x = 1; x < MAX-1; x++) {
                    int n = check4d(src, w, z, y, x) - (src[w][z][y][x] == '#');
                    switch (src[w][z][y][x]) {
                    case '#': next[w][z][y][x] = (n == 2 || n == 3) ? '#' : '.'; break;
                    case '.': /* FALL THROUGH */
                    case '\0': next[w][z][y][x] = n == 3 ? '#' : '.'; break;
                    }
                }
            }
        }
    }
}

size_t count(char cubes[MAX][MAX][MAX][MAX])
{
    size_t sum = 0;
    for (size_t x = 0; x < MAX; x++)
        for (size_t y = 0; y < MAX; y++)
            for (size_t z = 0; z < MAX; z++)
                for (size_t w = 0; w < MAX; w++)
                    sum += (cubes[x][y][z][w] == '#');
    return sum;
}

void print(char cubes[MAX][MAX][MAX][MAX], size_t w, size_t z, size_t start, size_t len)
{
    for (size_t x = start; x < len; x++) {
        for (size_t y = start; y < len; y++) {
            char c = cubes[w][z][x][y];
            putchar( c == '\0' ? '.' : c );
        }
        putchar('\n');
    }
}

int main()
{
    FILE *f = fopen("input17.txt", "r");
    char *line;
    size_t i = START;
    while (line = mygetline(f), line != NULL) {
        strcpy(cubes[START][START][i] + START, line);
        free(line);
        i++;
    }
    char (*c1)[MAX][MAX][MAX] = cubes, (*c2)[MAX][MAX][MAX] = copy;
    size_t len = START + strlen(cubes[START][START][START] + START);
    for (int i = 0; i < 6; i++) {
        step(c1, c2);
        char (*tmp)[MAX][MAX][MAX] = c1;
        c1 = c2;
        c2 = tmp;
        printf("%lu\n", count(c1));
    }
}

