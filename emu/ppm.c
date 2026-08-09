#include "ppm.h"

#include "ega_render.h"

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
  RGBA8 rowRgba[EGA_WIDTH];

  for (unsigned y = 0; y != EGA_HEIGHT; ++y) {
    ega_row_to_rgba(planes, palette, y, rowRgba);

    uint8_t *out = row;
    for (unsigned x = 0; x != EGA_WIDTH; ++x) {
      *out++ = rowRgba[x].r;
      *out++ = rowRgba[x].g;
      *out++ = rowRgba[x].b;
    }

    if (fwrite(row, 1, sizeof(row), f) != sizeof(row)) {
      fclose(f);
      return false;
    }
  }

  return fclose(f) == 0;
}
