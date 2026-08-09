// bolo - public domain Tzvetan Mikov 2021

#include "ega_render.h"

void ega_row_to_rgba(
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette,
    unsigned y,
    RGBA8 *out) {
  unsigned ofs = y * EGA_STRIDE;

  for (unsigned byte = 0; byte != EGA_STRIDE; ++byte) {
    uint8_t bits0 = planes[0][ofs];
    uint8_t bits1 = planes[1][ofs];
    uint8_t bits2 = planes[2][ofs];
    uint8_t bits3 = planes[3][ofs];
    ++ofs;

    unsigned bitcnt = 8;
    do {
      unsigned index = ((bits0 & 0x80) >> 7) | ((bits1 & 0x80) >> 6) | ((bits2 & 0x80) >> 5) |
          ((bits3 & 0x80) >> 4);

      *out++ = palette[index];

      bits0 <<= 1;
      bits1 <<= 1;
      bits2 <<= 1;
      bits3 <<= 1;
    } while (--bitcnt);
  }
}

void ega_screen_to_rgba(
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette,
    RGBA8 *out,
    unsigned outStridePixels) {
  for (unsigned y = 0; y != EGA_HEIGHT; ++y) {
    ega_row_to_rgba(planes, palette, y, out);
    out += outStridePixels;
  }
}
