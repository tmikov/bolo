// Unit test for the shared EGA plane -> RGBA conversion.
#include "ega_render.h"

#include <stdio.h>
#include <string.h>

static uint8_t g_planes[EGA_PLANES][EGA_PAGE_VISIBLE];
static RGBA8 g_palette[16];
static const uint8_t *g_planePtrs[EGA_PLANES];

static int fail(const char *msg) {
  fprintf(stderr, "FAIL: %s\n", msg);
  return 1;
}

static void clear_planes(void) {
  memset(g_planes, 0, sizeof(g_planes));
}

// Test 1: plane-to-bit mapping. A pixel with only plane 0 set is colour 1;
// only plane 1 -> 2; only plane 2 -> 4; only plane 3 -> 8; all four -> 15;
// none -> 0. Column 0 (byte 0, bit 7) is used throughout. Transposing the
// plane order in ega_render.c would make one of these assertions land on the
// wrong palette index.
static int test_plane_to_bit_mapping(void) {
  static const struct {
    int planeMask; // bit p set => plane p's bit is set
    int expectIndex;
  } cases[] = {
      {0x0, 0},
      {0x1, 1}, // only plane 0
      {0x2, 2}, // only plane 1
      {0x4, 4}, // only plane 2
      {0x8, 8}, // only plane 3
      {0xF, 15}, // all four
  };

  RGBA8 out[EGA_WIDTH];
  for (size_t i = 0; i != sizeof(cases) / sizeof(cases[0]); ++i) {
    clear_planes();
    for (int p = 0; p != EGA_PLANES; ++p) {
      if (cases[i].planeMask & (1 << p))
        g_planes[p][0] = 0x80;
    }
    ega_row_to_rgba(g_planePtrs, g_palette, 0, out);
    if (out[0].r != (uint8_t)cases[i].expectIndex)
      return fail("plane-to-bit mapping: wrong colour index at column 0");
  }
  return 0;
}

// Test 2: MSB-first bit order. Within one byte, a single distinctive bit
// (plane 0 only, so colour index 1) must land at the column matching its bit
// position counted from the most-significant bit: bit 7 -> column 0, bit 0
// -> column 7. Reversing the bit order in ega_render.c would shift every hit
// to the mirrored column.
static int test_msb_first_bit_order(void) {
  RGBA8 out[EGA_WIDTH];
  for (int bit = 0; bit != 8; ++bit) {
    clear_planes();
    g_planes[0][0] = (uint8_t)(1u << bit);
    ega_row_to_rgba(g_planePtrs, g_palette, 0, out);

    int expectColumn = 7 - bit;
    for (int col = 0; col != 8; ++col) {
      uint8_t expectIndex = (col == expectColumn) ? 1 : 0;
      if (out[col].r != expectIndex) {
        fprintf(
            stderr,
            "bit %d: column %d has index %u, expected %u\n",
            bit,
            col,
            out[col].r,
            expectIndex);
        return fail("MSB-first bit order violated");
      }
    }
  }
  return 0;
}

// Test 3: row indexing. Row 199's pixels must come from plane offset
// 199 * EGA_STRIDE, computed here independently of ega_render.c, not from
// row 0's data.
static int test_row_indexing(void) {
  clear_planes();

  const unsigned row = EGA_HEIGHT - 1; // 199
  const unsigned byteInRow = 5;
  const unsigned bit = 0; // bit 0 -> column 7 within the byte (MSB-first)
  const unsigned expectColumn = byteInRow * 8 + (7 - bit);

  // Compute the target offset independently of ega_render.c's own
  // `y * EGA_STRIDE + byte` arithmetic (same formula, but derived here from
  // first principles: 199 whole rows of EGA_STRIDE bytes, then the 6th byte).
  unsigned expectOfs = 0;
  for (unsigned r = 0; r != row; ++r)
    expectOfs += EGA_STRIDE;
  expectOfs += byteInRow;

  g_planes[3][expectOfs] = (uint8_t)(1u << bit); // plane 3 alone -> index 8

  RGBA8 out[EGA_WIDTH];

  // Row 0 must be unaffected (all zero).
  ega_row_to_rgba(g_planePtrs, g_palette, 0, out);
  for (unsigned col = 0; col != EGA_WIDTH; ++col) {
    if (out[col].r != 0)
      return fail("row indexing: row 0 was affected by row 199's data");
  }

  // Row 199 must show the bit at the expected column, and nowhere else.
  ega_row_to_rgba(g_planePtrs, g_palette, row, out);
  for (unsigned col = 0; col != EGA_WIDTH; ++col) {
    uint8_t expectIndex = (col == expectColumn) ? 8 : 0;
    if (out[col].r != expectIndex)
      return fail("row indexing: wrong pixel for row 199");
  }
  return 0;
}

// Test 4: stride padding in ega_screen_to_rgba. With outStridePixels >
// EGA_WIDTH, only the first EGA_WIDTH pixels of each row must be written;
// the padding pixels must retain a pre-filled sentinel, proving the loop
// skips rather than overwrites them.
static int test_stride_padding(void) {
  clear_planes();
  // Every pixel of every row -> colour index 1 (plane 0 all-ones).
  for (unsigned ofs = 0; ofs != EGA_PAGE_VISIBLE; ++ofs)
    g_planes[0][ofs] = 0xFF;

  const unsigned stride = EGA_WIDTH + 37;
  static RGBA8 out[EGA_HEIGHT * (EGA_WIDTH + 37)];
  const RGBA8 sentinel = {0xEE, 0xDD, 0xCC, 0xBB};
  for (size_t i = 0; i != sizeof(out) / sizeof(out[0]); ++i)
    out[i] = sentinel;

  ega_screen_to_rgba(g_planePtrs, g_palette, out, stride);

  for (unsigned y = 0; y != EGA_HEIGHT; ++y) {
    RGBA8 *row = out + (size_t)y * stride;
    for (unsigned x = 0; x != EGA_WIDTH; ++x) {
      if (row[x].r != 1)
        return fail("stride padding: converted pixel is wrong");
    }
    for (unsigned x = EGA_WIDTH; x != stride; ++x) {
      RGBA8 px = row[x];
      if (px.r != sentinel.r || px.g != sentinel.g || px.b != sentinel.b || px.a != sentinel.a)
        return fail("stride padding: padding pixel was overwritten");
    }
  }
  return 0;
}

// Test 5: all 16 colours. Every one of the 16 possible plane-bit
// combinations at column 0 must map to its own palette index.
static int test_all_16_colours(void) {
  RGBA8 out[EGA_WIDTH];
  for (int combo = 0; combo != 16; ++combo) {
    clear_planes();
    for (int p = 0; p != EGA_PLANES; ++p) {
      if (combo & (1 << p))
        g_planes[p][0] = 0x80;
    }
    ega_row_to_rgba(g_planePtrs, g_palette, 0, out);
    if (out[0].r != (uint8_t)combo)
      return fail("all-16-colours: wrong colour index");
    // Palette colour must be copied through faithfully, not just its index.
    if (out[0].g != g_palette[combo].g || out[0].b != g_palette[combo].b ||
        out[0].a != g_palette[combo].a)
      return fail("all-16-colours: palette entry not copied through");
  }
  return 0;
}

int main(void) {
  for (int i = 0; i != 16; ++i)
    g_palette[i] = (RGBA8){(uint8_t)i, (uint8_t)(i + 100), (uint8_t)(i + 200), 0xFF};
  for (int p = 0; p != EGA_PLANES; ++p)
    g_planePtrs[p] = g_planes[p];

  int rc;
  if ((rc = test_plane_to_bit_mapping()) != 0)
    return rc;
  if ((rc = test_msb_first_bit_order()) != 0)
    return rc;
  if ((rc = test_row_indexing()) != 0)
    return rc;
  if ((rc = test_stride_padding()) != 0)
    return rc;
  if ((rc = test_all_16_colours()) != 0)
    return rc;

  printf("ega_render ok\n");
  return 0;
}
