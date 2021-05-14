#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>
#include <vector>
#include <string>

static std::vector<uint8_t> readBytes(FILE *f, unsigned upto) {
    std::vector<uint8_t> res{};

    int ch = fgetc(f);
    while (ch != EOF) {
        if (!isxdigit(ch)) {
            ch = fgetc(f);
            continue;
        }
        std::string buf;
        do {
            buf.push_back((char)ch);
            ch = fgetc(f);
        } while (isxdigit(ch));
        auto b = (uint8_t)strtol(buf.c_str(), nullptr, 16);
        //printf("Got %u: %02x from %s\n", (unsigned)res.size(), b, buf.c_str());
        res.push_back(b);
        if (res.size() == upto)
            break;
    }

    return res;
}

int main(int argc, const char **argv) {
    unsigned width, height;
    if (argc != 3 || 
        sscanf(argv[1], "%u", &width) != 1 ||
        sscanf(argv[2], "%u", &height) != 1) {
        fprintf(stderr, "syntax: %s width_bytes height < bmp.lst\n", argv[0]);
        return 1;
    }

    auto bytes = readBytes(stdin, width * height);
    if (bytes.size() != width * height) {
        fprintf(stderr, "Not enough bytes\n");
        return 2;
    }

    for(unsigned row = 0; row != height; ++row) {
        for(unsigned bt = 0; bt != width; ++bt) {
            unsigned pixels = bytes[row * width + bt];
            for(unsigned mask = 0x80; mask != 0; mask >>= 1) {
                putchar(mask & pixels ? '*' : ' ');
            }
        }
        printf("\n");
    }

    return 0;
}
