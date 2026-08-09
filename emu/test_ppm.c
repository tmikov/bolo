// Unit test for the PPM writer.
#include "ppm.h"

#include <stdio.h>
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

  // A distinctive pixel on the last row, to catch a row-stride miscomputation
  // that pixels 0-2 of row 0 plus a total-size check would miss: byte 5, bit
  // 0 of plane 2 on row 199 -> column 40, color index 4.
  const int lastRow = EGA_HEIGHT - 1;
  const int lastRowByte = 5;
  const int lastRowColumn = lastRowByte * 8;
  g_planes[2][lastRow * EGA_STRIDE + lastRowByte] = 0x80;

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

  // Spot-check the last row: seek to its expected byte offset and confirm
  // color index 4 landed there. Both ppm.c and ega_to_rgb hand-roll the
  // row stride, so this catches a miscomputation that preserves total size.
  long lastOfs = 15 + (long)(lastRow * EGA_WIDTH + lastRowColumn) * 3;
  if (fseek(f, lastOfs, SEEK_SET) != 0) {
    fclose(f);
    return fail("fseek to last row failed");
  }
  uint8_t lastRgb[3];
  if (fread(lastRgb, 1, sizeof(lastRgb), f) != sizeof(lastRgb)) {
    fclose(f);
    return fail("short last-row pixel data");
  }
  static const uint8_t expectLast[3] = {4, 8, 12};
  if (memcmp(lastRgb, expectLast, sizeof(expectLast)) != 0) {
    fclose(f);
    return fail("wrong last-row pixel");
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
