#include "ppm.h"

#include <stdio.h>

bool ppm_write_planes(
    const char *path,
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette) {
  FILE *f = fopen(path, "wb");
  if (!f)
    return false;

  if (fprintf(f, "P6\n%d %d\n255\n", EGA_WIDTH, EGA_HEIGHT) < 0) {
    fclose(f);
    return false;
  }

  // One row of RGB triples, flushed per row to keep the buffer small.
  uint8_t row[EGA_WIDTH * 3];

  for (unsigned y = 0; y != EGA_HEIGHT; ++y) {
    uint8_t *out = row;
    for (unsigned byte = 0; byte != EGA_STRIDE; ++byte) {
      unsigned ofs = y * EGA_STRIDE + byte;
      uint8_t bits0 = planes[0][ofs];
      uint8_t bits1 = planes[1][ofs];
      uint8_t bits2 = planes[2][ofs];
      uint8_t bits3 = planes[3][ofs];

      for (unsigned bit = 0; bit != 8; ++bit) {
        unsigned index = ((bits0 & 0x80) >> 7) | ((bits1 & 0x80) >> 6) | ((bits2 & 0x80) >> 5) |
            ((bits3 & 0x80) >> 4);
        RGBA8 color = palette[index];
        *out++ = color.r;
        *out++ = color.g;
        *out++ = color.b;

        bits0 <<= 1;
        bits1 <<= 1;
        bits2 <<= 1;
        bits3 <<= 1;
      }
    }

    if (fwrite(row, 1, sizeof(row), f) != sizeof(row)) {
      fclose(f);
      return false;
    }
  }

  return fclose(f) == 0;
}
