// Unit test for the PPM writer.
#include "ppm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint8_t g_planes[EGA_PLANES][EGA_PAGE_VISIBLE];
static RGBA8 g_palette[16];

static int fail(const char *msg) {
  fprintf(stderr, "FAIL: %s\n", msg);
  return 1;
}

int main(void) {
  // A recognizable palette: color index i is (i, i * 2, i * 3).
  for (int i = 0; i != 16; ++i)
    g_palette[i] = (RGBA8){(uint8_t)i, (uint8_t)(i * 2), (uint8_t)(i * 3), 0xFF};

  // Pixel 0 gets all four plane bits set -> color 15.
  // Pixel 1 gets only plane 0 -> color 1.
  // Pixel 2 gets only plane 3 -> color 8.
  for (int p = 0; p != EGA_PLANES; ++p)
    g_planes[p][0] = 0x80;
  g_planes[0][0] |= 0x40;
  g_planes[3][0] |= 0x20;

  const uint8_t *planes[EGA_PLANES];
  for (int p = 0; p != EGA_PLANES; ++p)
    planes[p] = g_planes[p];

  const char *path = "test_ppm_out.ppm";
  if (!ppm_write_planes(path, planes, g_palette))
    return fail("ppm_write_planes returned false");

  FILE *f = fopen(path, "rb");
  if (!f)
    return fail("output file missing");

  char header[32];
  size_t headerLen = fread(header, 1, 15, f);
  if (headerLen != 15 || memcmp(header, "P6\n320 200\n255\n", 15) != 0) {
    fclose(f);
    return fail("bad PPM header");
  }

  uint8_t rgb[9];
  if (fread(rgb, 1, sizeof(rgb), f) != sizeof(rgb)) {
    fclose(f);
    return fail("short pixel data");
  }

  // Pixel 0 -> color 15, pixel 1 -> color 1, pixel 2 -> color 8.
  static const uint8_t expect[9] = {15, 30, 45, 1, 2, 3, 8, 16, 24};
  if (memcmp(rgb, expect, sizeof(expect)) != 0) {
    fclose(f);
    return fail("wrong pixels");
  }

  // The file must be exactly header + 320 * 200 * 3 bytes.
  if (fseek(f, 0, SEEK_END) != 0) {
    fclose(f);
    return fail("fseek failed");
  }
  long size = ftell(f);
  fclose(f);
  remove(path);

  if (size != 15 + (long)EGA_WIDTH * EGA_HEIGHT * 3) {
    fprintf(stderr, "FAIL: file is %ld bytes\n", size);
    return 1;
  }

  printf("ppm ok\n");
  return 0;
}
