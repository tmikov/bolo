#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char buf[512];
    for(;;) {
        printf("idx:");
        if (!gets(buf))
            return 0;

        char *src = buf;
        while (char *tok = strtok(src, " \t\n\r\v\f")) {
            src = nullptr;
            unsigned num = strtol(tok, nullptr, 16);
            if (num < 10)
                printf("%c", '0' + num);
            else if (num < 36)
                printf("%c", 'A' + num - 10);
            else if (num < 39)
                printf("%c", "! c"[num - 36]);
            else
                printf("\nERR!\n");
        }
        printf("\n");
    }
    return 0;
}
