#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#define MAXSIZE 5000

char *mygetline(FILE *f)
{
    size_t len = 0;
    int c, i;
    char *res;

    while (c = fgetc(f), c != '\n' && c != EOF) len++;
    if (len == 0 && c == EOF) return NULL;
    fseek(f, -len-1, SEEK_CUR);
    res = malloc(sizeof(char)*(len+1));
    for (i = 0; i <= len; i++) res[i] = fgetc(f);
    res[i-1] = '\0';
    return res;
}

int main()
{
    FILE *input = fopen("input1.txt", "r");
    int arr[MAXSIZE];
    char *line, *endptr;
    long num;
    size_t arrlen = 0;
    while (line = mygetline(input), line != NULL) {
        num = strtol(line, &endptr, 0);
        if (*endptr != 0) {
            fprintf(stderr, "error: not a number: %s\n", line);
            continue;
        }
        arr[arrlen++] = num;
    }

    for (size_t i = 0; i < arrlen; i++) {
        for (size_t j = i; j < arrlen; j++) {
            if (arr[i] + arr[j] == 2020)
                printf("%d * %d = %d\n", arr[i], arr[j], arr[i]*arr[j]);
        }
    }

    for (size_t i = 0; i < arrlen; i++) {
        for (size_t j = i; j < arrlen; j++) {
            for (size_t x = j; x < arrlen; x++) {
                if (arr[i] + arr[j] + arr[x] == 2020)
                    printf("%d * %d * %d = %d\n",
                            arr[i], arr[j], arr[x], arr[i]*arr[j]*arr[x]);
            }
        }
    }
}

