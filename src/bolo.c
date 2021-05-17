#include "sokol_app.h"
#include "sokol_gfx.h"
#include "sokol_glue.h"

#include "blit.h"

#include <stdalign.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef enum EGAColor {
  EGABlack = 0,
  EGALowBlue = 1,
  EGALowGreen = 2,
  EGALowCyan = 3,
  EGALowRed = 4,
  EGALowMagenta = 5,
  EGABrown = 6,
  EGALightGray = 7,
  EGADarkGray = 8,
  EGAHighBlue = 9,
  EGAHighGreen = 10,
  EGAHighCyan = 11,
  EGAHighRed = 12,
  EGAHighMagenta = 13,
  EGAYellow = 14,
  EGAWhite = 15,
} EGAColor;

typedef struct RGBA8 {
  uint8_t r, g, b, a;
} RGBA8;

#define EGA_WIDTH 320
#define EGA_STRIDE 40
#define EGA_HEIGHT 200
#define EGA_PAGE_SIZE 0x2000
#define EGA_PLANES 4

static alignas(uint32_t) uint8_t g_ega_screen[EGA_PLANES][EGA_PAGE_SIZE * 2];
static uint8_t g_ega_mask;
static unsigned g_ega_page;

static RGBA8 g_ega_palette[16] = {
    {0x00, 0x00, 0x00}, //  0
    {0x00, 0x00, 0xAA}, //  1
    {0x00, 0xAA, 0x00}, //  2
    {0x00, 0xAA, 0xAA}, //  3
    {0xAA, 0x00, 0x00}, //  4
    {0xAA, 0x00, 0xAA}, //  5
    {0xAA, 0x55, 0x00}, //  6
    {0xAA, 0xAA, 0xAA}, //  7
    {0x55, 0x55, 0x55}, //  8
    {0x55, 0x55, 0xFF}, //  9
    {0x55, 0xFF, 0x55}, // 10
    {0x55, 0xFF, 0xFF}, // 11
    {0xFF, 0x55, 0x55}, // 12
    {0xFF, 0x55, 0xFF}, // 13
    {0xFF, 0xFF, 0x55}, // 14
    {0xFF, 0xFF, 0xFF}, // 15
};

/// The EGA bitplanes converted to RGB here.
static RGBA8 g_rgb_screen[EGA_WIDTH * EGA_HEIGHT];

/// Convert the active EGA screen page to RGB into g_rgb_screen.
static void ega_to_rgb(void) {
  unsigned pixcnt = EGA_STRIDE * EGA_HEIGHT;
  const uint8_t *ptr = g_ega_screen[0] + g_ega_page * EGA_PAGE_SIZE;
  RGBA8 *out = g_rgb_screen;
  do {
    uint8_t bits0 = ptr[0];
    uint8_t bits1 = ptr[EGA_PAGE_SIZE * 2];
    uint8_t bits2 = ptr[EGA_PAGE_SIZE * 4];
    uint8_t bits3 = ptr[EGA_PAGE_SIZE * 6];

    unsigned bitcnt = 8;
    do {
      unsigned index = ((bits0 & 0x80) >> 7) | ((bits1 & 0x80) >> 6) | ((bits2 & 0x80) >> 5) |
          ((bits3 & 0x80) >> 4);

      *out++ = g_ega_palette[index];

      bits0 <<= 1;
      bits1 <<= 1;
      bits2 <<= 1;
      bits3 <<= 1;
    } while (--bitcnt);
  } while (++ptr, --pixcnt);
}

static inline uint8_t ega_read(unsigned offset) {
  return g_ega_screen[0][offset];
}

static inline void ega_or(unsigned offset, uint8_t value, uint8_t mask) {
  if (mask & 1)
    g_ega_screen[0][offset] |= value;
  if (mask & 2)
    g_ega_screen[1][offset] |= value;
  if (mask & 4)
    g_ega_screen[2][offset] |= value;
  if (mask & 8)
    g_ega_screen[3][offset] |= value;
}

/// Write to the specified address in EGA memory, obeying the mask
static inline void ega_write(unsigned offset, uint8_t value, uint8_t mask) {
  if (mask & 1)
    g_ega_screen[0][offset] = value;
  if (mask & 2)
    g_ega_screen[1][offset] = value;
  if (mask & 4)
    g_ega_screen[2][offset] = value;
  if (mask & 8)
    g_ega_screen[3][offset] = value;
}

/// Write to the specified address in EGA memory, obeying the mask
static inline unsigned
ega_write_bytes(unsigned offset, const uint8_t *src, unsigned len, uint8_t mask) {
  while (len--)
    ega_write(offset++, *src++, mask);
  return offset;
}

static inline unsigned ega_fill(unsigned offset, uint8_t value, unsigned len, uint8_t mask) {
  while (len--)
    ega_write(offset++, value, mask);
  return offset;
}

/// "SBOL" strings are a sequence of characters terminated by a negative, using
/// this encoding:
///    0-9: digits
///    10-35: letters
///    36: '!'
///    37: ' '
///    38: 'c' (copyright symbol)
///    extra icons
typedef int8_t SBOL;

#define SBOL_0 0
#define SBOL_A 10
#define SBOL_Z 35
#define SBOL_SPACE 37
#define SBOL_END -1

#define SB(x)                                          \
  ((x) == ' '              ? SBOL_SPACE                \
       : (x) <= '9'        ? (x) - '0'                 \
       : ((x) | 32) <= 'z' ? SBOL_A + ((x) | 32) - 'a' \
                           : -1)

/// A pair of SBOL string and a screen offset.
/// A sequence of these ends with a null string pointer.
typedef struct SBOLDesc {
  const SBOL *str;
  unsigned offset;
} SBOLDesc;

#define NUM_SPRITES 48
#define SPRITE_W 1
#define SPRITE_H 7

static uint8_t level;
static uint8_t density;
/// Score as a SBol string.
static SBOL strb_score[7] =
    {SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_0, SBOL_END};
/// High score as a string
static SBOL strb_hisco[7] =
    {SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_SPACE, SBOL_0, SBOL_END};
/// Level of the high score.
static uint8_t high_level;
/// Density of the high score
static uint8_t high_dens;
/// Increments every 54.94 ms
static uint8_t time_tick;
/// I believe this is used to keep the random generator state.
static uint8_t rnd_state[32] = {0x0B, 0xED, 0x5F, 0x8F, 0xB5, 0x3B, 0xE3, 0x73, 0x67, 0x23, 0x6D,
                                0x11, 0xBD, 0xA5, 0xF3, 0xDF, 0x71, 0x2B, 0x77, 0x85, 0xE5, 0x37,
                                0xAB, 0xEF, 0xF7, 0xD5, 0x0F, 0x51, 0xEB, 0xF3, 0x2B, 0xC3};

/// This value is always in the range 0..31. It is occasionally updated with
/// the current tick with some offset.
static uint8_t time_5bit;
#define TIME_5BIT_MASK 0x1F

static uint8_t coll_flags1; // 5043h
/// Ship velocity magnitude [0..5]
static uint8_t vel_magn; // 50B7h
/// Ship angle [0..7]. N, NE, E, SE, S, SW, W, NW.
static uint8_t ship_angle; // 50D7h
/// Absolute gun angle [0..7].
static uint8_t gun_angle; // 5177h
/// The angle of the ship velocity relative to the ship angle:
/// It is either 0 or 4 (when moving backwards).
static uint8_t vel_angle; // 5178h

static void draw_rect(unsigned seg, unsigned x, unsigned y, unsigned wm1, unsigned hm1);
static void draw_bolo_8x20(unsigned offset);
static void draw_mstrs(const SBOLDesc *strings);
static void draw_str(unsigned offset, const SBOL *str);
static void draw_char_1x7(unsigned offset, SBOL ch);
static uint8_t rnd_update(uint8_t limit);
static uint8_t rndnum(uint8_t limit);

static const uint8_t bmps_font_1x7[];
static const uint8_t sprites_1x7[];
static const uint8_t bmp_bolo_8x20[];
static const uint8_t bmp_comp_4x31[];
static const uint8_t *const gun_offs[8];
static const uint8_t *const ship_offs[8];

/// 2913:02C1                       ega_map_mask    proc    near
static inline void ega_map_mask(uint8_t mask) {
  g_ega_mask = mask;
}

static inline void ega_set_page(uint8_t page) {
  g_ega_page = page;
}

static inline void
draw_bmp(unsigned offset, const uint8_t *bmp, unsigned w, unsigned h, uint8_t mask) {
  ega_map_mask(mask);
  while (h--) {
    ega_write_bytes(offset, bmp, w, mask);
    bmp += w;
    offset += EGA_STRIDE;
  }
}

/// Clear both video pages and display page 0.
/// 2913:04EB                       clear_vp0       proc    near
static void clear_vp0() {
  ega_map_mask(EGAWhite);
  memset(g_ega_screen, 0, sizeof(g_ega_screen));
  ega_set_page(0);
}

#define VID_OFFSET(x, y) ((y)*EGA_STRIDE + (x) / 8)

/// Calculate offset in video memory
/// 2913:0525                       vid_offset      proc    near
static inline unsigned vid_offset(unsigned x, unsigned y) {
  return VID_OFFSET(x, y);
}
/// Calculate bitmask in video memory
/// 2913:0525                       vid_offset      proc    near
static inline uint8_t vid_mask(unsigned x) {
  return 0x80 >> (x % 8);
}

/// Draw a vertical line.
/// \p seg      base offset in EGA memory
/// \p lenm1    length  minus 1.
/// 2913:05F5                       vert_line       proc    near
static void vert_line(unsigned seg, unsigned x, unsigned y, unsigned lenm1) {
  unsigned ofs = seg + vid_offset(x, y);
  uint8_t bits = vid_mask(x);
  uint8_t ega_mask = g_ega_mask;
  ++lenm1;
  do {
    ega_or(ofs, bits, ega_mask);
    ofs += EGA_STRIDE;
  } while (--lenm1);
}

/// Draw a horizontal line.
/// 2913:0602                       horiz_line      proc    near
static void horiz_line(unsigned seg, unsigned x, unsigned y, unsigned lenm1) {
  unsigned ofs = seg + vid_offset(x, y);
  uint8_t bits = vid_mask(x);
  uint8_t ega_mask = g_ega_mask;
  ++lenm1;
  do {
    if (!lenm1)
      return;
    ega_or(ofs, bits, ega_mask);
    --lenm1;
  } while (bits >>= 1);
  ++ofs;
  if (!lenm1)
    return;
  ofs = ega_fill(ofs, 0xFF, lenm1 / 8, ega_mask);
  ega_or(ofs, 0xFF00 >> (lenm1 & 7), ega_mask);
}

/// Draw rotated ship and gun and perform collision detection.
/// Collision detection by counting screen bytes that already had
/// the same bit set.
/// 2913:0D22                       draw_ship       proc    near
static void draw_ship(unsigned pageSeg) {
  ega_map_mask(EGAHighCyan);

  unsigned offset = pageSeg + VID_OFFSET(104, 92);
  const uint8_t *gunbmp = gun_offs[gun_angle];
  const uint8_t *shipbmp = ship_offs[(ship_angle + vel_angle) & 7];
  unsigned collisions = 0;
  unsigned cnt = 7;

  do {
    uint8_t pix = *gunbmp++ ^ *shipbmp++;
    if (pix & ega_read(offset))
      ++collisions;
    ega_write(offset, pix, EGAHighCyan);
    offset += EGA_STRIDE;
  } while (--cnt);

  if (collisions) {
    // TODO: data_191e = 0xA;
    // Record the collision.
    coll_flags1 = 1;
    // Stop the ship.
    vel_magn = 0;
    // TODO: data_192e = 0xA;
  }
}

/// Draw q sequence of string/offset pairs terminated with a null string ptr.
/// 2913:0FFE                       draw_mstrs      proc    near
static void draw_mstrs(const SBOLDesc *strings) {
  for (; strings->str; ++strings)
    draw_str(strings->offset, strings->str);
}

/// Draw a B39 string at the specified offset in both screen pages.
/// 2913:100E                       draw_str        proc    near
static void draw_str(unsigned offset, const SBOL *str) {
  while (*str >= 0)
    draw_char_1x7(offset++, *str++);
}

/// Draw the specified SBOL character at the specified offset in both screen
/// pages.
/// 2913:1023                       draw_char_1x7   proc    near
static void draw_char_1x7(unsigned offset, SBOL ch) {
  draw_bmp(offset, bmps_font_1x7 + (uint8_t)ch * 7, 1, 7, g_ega_mask);
}

/// Display strb_score.
/// 2913:103A                       disp_score      proc    near
static void disp_score() {
  ega_map_mask(EGAYellow);
  draw_str(VID_OFFSET(232, 44) + 1, strb_score);
}

static const SBOL data_118[] = {
    0x0F,
    0x18,
    0x1B,
    0x25,
    0x12,
    0x0B,
    0x16,
    0x25,
    0x19,
    0x0C,
    0xFF,
};
static const SBOL data_119[] = {
    0x1F,
    0x01,
    0x2A,
    0x00,
    0x00,
    0xFF,
};
static const SBOL data_120[] = {
    0x0C, 0x18, 0x19, 0x22, 0x1B, 0x12, 0x10, 0x11, 0x1D, 0x25, 0x26, 0x25,
    0x01, 0x09, 0x09, 0x03, 0x25, 0x0B, 0x22, 0x25, 0x27, 0x28, 0x29, 0xFF,
};
static const SBOL data_121[] = {
    0x18, 0x1B, 0x12, 0x10, 0x12, 0x17, 0x0A, 0x15, 0x25, 0x10, 0x0A, 0x16, 0x0E,
    0x25, 0x26, 0x25, 0x01, 0x09, 0x08, 0x02, 0x25, 0x0B, 0x22, 0x25, 0x0E, 0x15,
    0x1F, 0x22, 0x17, 0x25, 0x1C, 0x18, 0x0F, 0x1D, 0x20, 0x0A, 0x1B, 0x0E, 0xFF,
};

static const SBOLDesc title_mstr[] =
    {{data_118, 0x0A0F}, {data_119, 0x0C92}, {data_120, 0x1189}, {data_121, 0x1901}, {NULL}};

/// Draw title screen, wait a little or until a key
/// 2913:1194                       title_screen    proc    near
static void title_screen(void) {
  ega_map_mask(EGADarkGray);

  for (unsigned row = 0; row < EGA_HEIGHT / (SPRITE_H + 1); ++row) {
    for (unsigned col = 0; col != EGA_STRIDE; ++col) {
      unsigned sprite = rndnum(NUM_SPRITES);
      draw_bmp(
          row * (SPRITE_H + 1) * EGA_STRIDE + col,
          sprites_1x7 + sprite * SPRITE_H * SPRITE_W,
          SPRITE_W,
          SPRITE_H,
          EGADarkGray);
    }
  }

  draw_bolo_8x20(0x538);
  draw_mstrs(title_mstr);

  // TODO: wait for 0x3C ticks or a key (which is consumed) and update
  //       lastkey_tick
}

static const SBOL str_level[] = {SB('l'), SB('e'), SB('v'), SB('e'), SB('l'), SBOL_END};
static const SBOL str_density[] =
    {SB('d'), SB('e'), SB('n'), SB('s'), SB('i'), SB('t'), SB('y'), SBOL_END};
static const SBOL str_new[] = {SB('n'), SB('e'), SB('w'), SBOL_END};
static const SBOL str_current[] =
    {SB('c'), SB('u'), SB('r'), SB('r'), SB('e'), SB('n'), SB('t'), SBOL_END};
static const SBOL str_score[] = {SB('s'), SB('c'), SB('o'), SB('r'), SB('e'), SBOL_END};
static const SBOL str_at[] = {SB('a'), SB('t'), SBOL_END};
static const SBOL str_high[] = {SB('h'), SB('i'), SB('g'), SB('h'), SBOL_END};

static SBOLDesc lsel_mstr[] = {
    {str_high, 0x551},
    {str_score, 0x556},
    {str_at, 0x624},
    {str_level, 0x560},
    {str_density, 0x6EF},
    {str_current, 0x0E3B},
    {str_level, 0x0D7D},
    {str_density, 0x0F0C},
    {str_new, 0x1685},
    {str_level, 0x15C3},
    {str_density, 0x1752},
    {NULL}};

/// Draw the hud to the right
/// 2913:11F9                       draw_hud        proc    near
static void draw_hud() {
  clear_vp0();

  unsigned pageOfs = 0;
  do {
    draw_bolo_8x20(0x1D);
    ega_map_mask(EGAWhite);

    draw_rect(pageOfs, 0xE7, 0x1C, 0x40, 0x1A);
    draw_rect(pageOfs, 0xE7, 0x47, 0x40, 0x0F);
    draw_rect(pageOfs, 0xE7, 0x7E, 0x40, 0x3D);
    draw_rect(pageOfs, 0xE7, 0x5B, 0x0F, 0x0F);
    draw_rect(pageOfs, 0xE7 + 0x0F, 0x5B, 0x0F, 0x0F);
    draw_rect(pageOfs, 0xE7 + 0x0F, 0x5B + 0x0F, 0x0F, 0x0F);
    draw_rect(pageOfs, 0xE7, 0x5B + 0x0F, 0x0F, 0x0F);
    draw_bmp(pageOfs + VID_OFFSET(264, 91), bmp_comp_4x31, 4, 31, EGAWhite);
  } while ((pageOfs += EGA_PAGE_SIZE) != EGA_PAGE_SIZE * 2);

  disp_score();
  draw_str(VID_OFFSET(240, 32), str_score);
}

/// Draw level selection on the left.
/// 2913:1263                       draw_levsel     proc    near
static void draw_levsel() {
  ega_map_mask(EGAWhite);
  draw_rect(0, 0x02, 0x1C, 0xCC, 0x1C);
  draw_rect(0, 0x02, 0x50, 0xCC, 0x1C);
  draw_rect(0, 0x02, 0x85, 0xCC, 0x1C);

  ega_map_mask(EGAHighCyan);
  draw_mstrs(lsel_mstr);

  ega_map_mask(EGAYellow);
  draw_char_1x7(VID_OFFSET(168, 86), (SBOL)(level + 1));
  draw_char_1x7(VID_OFFSET(168, 96), (SBOL)(density + 1));
  draw_str(VID_OFFSET(16, 44), strb_hisco);
  draw_char_1x7(VID_OFFSET(184, 34), (SBOL)(high_level + 1));
  draw_char_1x7(VID_OFFSET(184, 44), (SBOL)(high_dens + 1));
}

/// Draw a rectangle with 1-pixel lines.
/// \p wm1  width - 1
/// \p hm1  height - 1
/// 2913:1321                       draw_rect       proc    near
static void draw_rect(unsigned seg, unsigned x, unsigned y, unsigned wm1, unsigned hm1) {
  horiz_line(seg, x, y + hm1, wm1);
  horiz_line(seg, x, y, wm1);
  vert_line(seg, x, y, hm1);
  vert_line(seg, x + wm1, y, hm1);
}

/// Draw the 8x20 BOLO bitmap at the specified offset using bright green color.
/// 2913:13BC                       draw_bolo_8x20  proc    near
static void draw_bolo_8x20(unsigned offset) {
  draw_bmp(offset, bmp_bolo_8x20, 8, 20, 0xA);
}

/// This routine is not fully understood yet.
/// Update rnd_state and return a value.
/// 2913:2ED1                       rnd_update      proc    near
static uint8_t rnd_update(uint8_t limit) {
  uint8_t save_t5b = time_5bit;
  time_5bit = (time_5bit + 13) & TIME_5BIT_MASK;

  uint_fast16_t tmp = rnd_state[save_t5b] + rnd_state[time_5bit];
  tmp += (tmp >> 8) + 1;
  rnd_state[save_t5b] = (uint8_t)tmp;

  return (uint8_t)tmp & (limit - 1);
}

static uint8_t rndnum(uint8_t limit) {
  // Nothing to do if limit is 0.
  if (!limit)
    return limit;

  // One bit high than the highest set bit of limit.
  uint8_t highbit = 1;
  uint8_t tmp = limit;
  do
    highbit <<= 1;
  while (tmp >>= 1);

  // Loop until rnd_update returns something in range.
  uint8_t res;
  do
    res = rnd_update(highbit);
  while (res >= limit);

  return res;
}

/// "BOLO"
static const uint8_t bmp_bolo_8x20[] = {
    // 8 x 20 (160 bytes)
    0xff, 0xe0, 0x07, 0xc0, 0x3c, 0x00, 0x0f, 0x80, 0xff, 0xf0, 0x1f, 0xf0, 0x3c, 0x00, 0x3f, 0xe0,
    0xff, 0xf8, 0x3f, 0xf8, 0x3c, 0x00, 0x7f, 0xf0, 0xf0, 0xf8, 0x7c, 0x7c, 0x3c, 0x00, 0xf8, 0xf8,
    0xf0, 0x78, 0xf8, 0x3e, 0x3c, 0x01, 0xf0, 0x7c, 0xf0, 0x78, 0xf0, 0x1e, 0x3c, 0x01, 0xe0, 0x3c,
    0xf0, 0x79, 0xf0, 0x1f, 0x3c, 0x03, 0xe0, 0x3e, 0xf0, 0xf9, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e,
    0xff, 0xf1, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e, 0xff, 0xe1, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e,
    0xff, 0xf1, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e, 0xf0, 0xf9, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e,
    0xf0, 0x79, 0xe0, 0x0f, 0x3c, 0x03, 0xc0, 0x1e, 0xf0, 0x79, 0xf0, 0x1f, 0x3c, 0x03, 0xe0, 0x3e,
    0xf0, 0x78, 0xf0, 0x1e, 0x3c, 0x01, 0xe0, 0x3c, 0xf0, 0x78, 0xf8, 0x3e, 0x3c, 0x01, 0xf0, 0x7c,
    0xf0, 0xf8, 0x7c, 0x7c, 0x3c, 0x00, 0xf8, 0xf8, 0xff, 0xf8, 0x3f, 0xf8, 0x3f, 0xfc, 0x7f, 0xf0,
    0xff, 0xf0, 0x1f, 0xf0, 0x3f, 0xfc, 0x3f, 0xe0, 0xff, 0xe0, 0x07, 0xc0, 0x3f, 0xfc, 0x0f, 0x80,
};

/// Compass background.
static const uint8_t bmp_comp_4x31[] = {
    // 4 x 31 (124 bytes)
    0x7f, 0xff, 0xff, 0xff, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01,
    0x40, 0x00, 0x80, 0x01, 0x40, 0x00, 0x80, 0x01, 0x42, 0x00, 0x80, 0x21, 0x41, 0x00, 0x00, 0x41,
    0x40, 0x80, 0x00, 0x81, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01,
    0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x80, 0x01, 0x47, 0x01, 0xc0, 0x71,
    0x40, 0x00, 0x80, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01,
    0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x40, 0x80, 0x00, 0x81,
    0x41, 0x00, 0x80, 0x41, 0x42, 0x00, 0x80, 0x21, 0x40, 0x00, 0x80, 0x01, 0x40, 0x00, 0x00, 0x01,
    0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x01, 0x7f, 0xff, 0xff, 0xff,
};

// 43 SBol characters 1x7 each.
static const uint8_t bmps_font_1x7[] = {
    // 1 x 301 (301 bytes)
    0x3c, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x18, 0x78, 0x18, 0x18, 0x18, 0x18, 0x7e, 0x3c, 0x66,
    0x06, 0x0c, 0x18, 0x30, 0x7e, 0x3c, 0x66, 0x0c, 0x18, 0x0c, 0x66, 0x3c, 0x66, 0x66, 0x66, 0x7e,
    0x06, 0x06, 0x06, 0x7e, 0x60, 0x60, 0x3c, 0x06, 0x06, 0x7c, 0x3c, 0x60, 0x60, 0x7c, 0x66, 0x66,
    0x3c, 0x7e, 0x06, 0x0c, 0x18, 0x30, 0x30, 0x30, 0x3c, 0x66, 0x66, 0x3c, 0x66, 0x66, 0x3c, 0x3c,
    0x66, 0x66, 0x3e, 0x06, 0x06, 0x3c, 0x18, 0x3c, 0x66, 0x66, 0x7e, 0x66, 0x66, 0x7c, 0x66, 0x66,
    0x7c, 0x66, 0x66, 0x7c, 0x3c, 0x66, 0x60, 0x60, 0x60, 0x66, 0x3c, 0x7c, 0x66, 0x66, 0x66, 0x66,
    0x66, 0x7c, 0x7c, 0x66, 0x60, 0x7c, 0x60, 0x66, 0x7c, 0x7e, 0x60, 0x60, 0x7c, 0x60, 0x60, 0x60,
    0x3c, 0x60, 0x60, 0x6e, 0x66, 0x66, 0x3c, 0x66, 0x66, 0x66, 0x7e, 0x66, 0x66, 0x66, 0x7e, 0x18,
    0x18, 0x18, 0x18, 0x18, 0x7e, 0x06, 0x06, 0x06, 0x06, 0x06, 0x66, 0x3c, 0x66, 0x66, 0x6c, 0x78,
    0x6c, 0x66, 0x66, 0x60, 0x60, 0x60, 0x60, 0x60, 0x60, 0x7e, 0x66, 0x7e, 0x66, 0x66, 0x66, 0x66,
    0x66, 0x66, 0x66, 0x76, 0x6e, 0x66, 0x66, 0x66, 0x3c, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x7c,
    0x66, 0x66, 0x7c, 0x60, 0x60, 0x60, 0x3c, 0x66, 0x66, 0x66, 0x66, 0x6e, 0x3e, 0x7c, 0x66, 0x66,
    0x7c, 0x6c, 0x66, 0x66, 0x3c, 0x66, 0x60, 0x3c, 0x06, 0x66, 0x3c, 0x7e, 0x18, 0x18, 0x18, 0x18,
    0x18, 0x18, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x18,
    0x66, 0x66, 0x66, 0x66, 0x66, 0x7e, 0x66, 0x66, 0x66, 0x3c, 0x18, 0x3c, 0x66, 0x66, 0x66, 0x66,
    0x66, 0x3c, 0x18, 0x18, 0x18, 0x7e, 0x06, 0x0c, 0x18, 0x30, 0x60, 0x7e, 0x18, 0x18, 0x18, 0x18,
    0x18, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x42, 0x99, 0xa5, 0xa1, 0xa5, 0x99,
    0x42, 0x88, 0xd8, 0xaa, 0xab, 0x8a, 0x8a, 0x8a, 0x0f, 0x08, 0xc8, 0x0f, 0x0a, 0x09, 0x28, 0x00,
    0x80, 0xbc, 0x2a, 0x2a, 0x2a, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x0c,
};

// 48 1x7 sprites
static const uint8_t sprites_1x7[] = {
    // 1 x 336 (336 bytes)
    0x10, 0x10, 0x38, 0x6c, 0xc6, 0x82, 0x00, 0x02, 0xfc, 0x8c, 0x04, 0x04, 0x04, 0x0c, 0x60, 0x30,
    0x18, 0x0e, 0x18, 0x30, 0x60, 0x0c, 0x04, 0x04, 0x04, 0x8c, 0xfc, 0x02, 0x00, 0x82, 0xc6, 0x6c,
    0x38, 0x10, 0x10, 0x60, 0x40, 0x40, 0x40, 0x62, 0x7e, 0x80, 0x0c, 0x18, 0x30, 0xe0, 0x30, 0x18,
    0x0c, 0x80, 0x7e, 0x62, 0x40, 0x40, 0x40, 0x60, 0x10, 0x92, 0x92, 0xfe, 0x82, 0x82, 0x00, 0x20,
    0x44, 0xe8, 0x30, 0x1a, 0x0c, 0x08, 0x7c, 0x10, 0x10, 0x1e, 0x10, 0x10, 0x7c, 0x08, 0x0c, 0x1a,
    0x30, 0xe8, 0x44, 0x20, 0x00, 0x82, 0x82, 0xfe, 0x92, 0x92, 0x10, 0x20, 0x60, 0xb0, 0x18, 0x2e,
    0x44, 0x08, 0x7c, 0x10, 0x10, 0xf0, 0x10, 0x10, 0x7c, 0x08, 0x44, 0x2e, 0x18, 0xb0, 0x60, 0x20,
    0x10, 0x10, 0x38, 0x38, 0x7c, 0x6c, 0x44, 0x02, 0x1c, 0x7c, 0xfc, 0x18, 0x18, 0x10, 0x00, 0xe0,
    0x78, 0x3e, 0x78, 0xe0, 0x00, 0x10, 0x18, 0x18, 0xfc, 0x7c, 0x1c, 0x02, 0x44, 0x6c, 0x7c, 0x38,
    0x38, 0x10, 0x10, 0x10, 0x30, 0x30, 0x7e, 0x7c, 0x70, 0x80, 0x00, 0x0e, 0x3c, 0xf8, 0x3c, 0x0e,
    0x00, 0x80, 0x70, 0x7c, 0x7e, 0x30, 0x30, 0x10, 0x10, 0x38, 0x6c, 0x44, 0x44, 0x44, 0x44, 0x1e,
    0x32, 0x62, 0xc6, 0x0c, 0x18, 0x10, 0x00, 0xf8, 0x0c, 0x06, 0x0c, 0xf8, 0x00, 0x10, 0x18, 0x0c,
    0xc6, 0x62, 0x32, 0x1e, 0x44, 0x44, 0x44, 0x44, 0x6c, 0x38, 0x10, 0x10, 0x30, 0x60, 0xc6, 0x8c,
    0x98, 0xf0, 0x00, 0x3e, 0x60, 0xc0, 0x60, 0x3e, 0x00, 0xf0, 0x98, 0x8c, 0xc6, 0x60, 0x30, 0x10,
    0x10, 0x10, 0x38, 0x7c, 0x7c, 0x7c, 0x7c, 0x06, 0x3e, 0x7c, 0xfc, 0x7c, 0x38, 0x10, 0x00, 0xf0,
    0xf8, 0xfe, 0xf8, 0xf0, 0x00, 0x10, 0x38, 0x7c, 0xfc, 0x7c, 0x3e, 0x06, 0x7c, 0x7c, 0x7c, 0x7c,
    0x38, 0x10, 0x10, 0x10, 0x38, 0x7c, 0x7e, 0x7c, 0xf8, 0xc0, 0x00, 0x1e, 0x3e, 0xfe, 0x3e, 0x1e,
    0x00, 0xc0, 0xf8, 0x7c, 0x7e, 0x7c, 0x38, 0x10, 0x10, 0x38, 0x7c, 0x7c, 0x7c, 0x7c, 0x38, 0x1e,
    0x3e, 0x7e, 0xfe, 0xfc, 0x78, 0x30, 0x00, 0x78, 0xfc, 0xfe, 0xfc, 0x78, 0x00, 0x30, 0x78, 0xfc,
    0xfe, 0x7e, 0x3e, 0x1e, 0x38, 0x7c, 0x7c, 0x7c, 0x7c, 0x38, 0x10, 0x18, 0x3c, 0x7e, 0xfe, 0xfc,
    0xf8, 0xf0, 0x00, 0x3c, 0x7e, 0xfe, 0x7e, 0x3c, 0x00, 0xf0, 0xf8, 0xfc, 0xfe, 0x7e, 0x3c, 0x18,
};

static const uint8_t bmp_gun_n[7] = {0x10, 0x10, 0x10, 0x10, 0x00, 0x00, 0x00};
static const uint8_t bmp_gun_ne[7] = {0x02, 0x04, 0x08, 0x10, 0x00, 0x00, 0x00};
static const uint8_t bmp_gun_e[7] = {0, 0, 0, 0x1E, 0, 0, 0};
static const uint8_t bmp_gun_se[7] = {0, 0, 0, 0x10, 0x08, 0x04, 0x02};
static const uint8_t bmp_gun_s[7] = {0, 0, 0, 0x10, 0x10, 0x10, 0x10};
static const uint8_t bmp_gun_sw[7] = {0, 0, 0, 0x10, 0x20, 0x40, 0x80};
static const uint8_t bmp_gun_w[7] = {0, 0, 0, 0xF0, 0, 0, 0};
static const uint8_t bmp_gun_nw[7] = {0x80, 0x40, 0x20, 0x10, 0, 0, 0};

static const uint8_t *const gun_offs[8] = {
    bmp_gun_n,
    bmp_gun_ne,
    bmp_gun_e,
    bmp_gun_se,
    bmp_gun_s,
    bmp_gun_sw,
    bmp_gun_w,
    bmp_gun_nw,
};

static const uint8_t bmp_ship_n[7] = {0x00, 0x44, 0xFE, 0xFE, 0xFE, 0x44, 0x00};
static const uint8_t bmp_ship_ne[7] = {0x30, 0x70, 0xF8, 0xFE, 0x3E, 0x1C, 0x18};
static const uint8_t bmp_ship_e[7] = {0x38, 0x7C, 0x38, 0x38, 0x38, 0x7C, 0x38};
static const uint8_t bmp_ship_se[7] = {0x18, 0x1C, 0x3E, 0xFE, 0xF8, 0x70, 0x30};

static const uint8_t *const ship_offs[8] = {
    bmp_ship_n,
    bmp_ship_ne,
    bmp_ship_e,
    bmp_ship_se,
    bmp_ship_n,
    bmp_ship_ne,
    bmp_ship_e,
    bmp_ship_se,
};

static struct {
  sg_pass_action pass_action;
  sg_pipeline pip;
  sg_bindings bind;
} state;

static void bolo_init(void) {
  sg_setup(&(sg_desc){.context = sapp_sgcontext()});

  state.pass_action = (sg_pass_action){.colors[0] = {.action = SG_ACTION_DONTCARE}};

  state.bind.fs_images[SLOT_tex] = sg_make_image(&(sg_image_desc){
      .width = EGA_WIDTH,
      .height = EGA_HEIGHT,
      .usage = SG_USAGE_STREAM,
      .min_filter = SG_FILTER_LINEAR,
      .mag_filter = SG_FILTER_LINEAR,
      .label = "ega_image",
  });

  title_screen();
  draw_hud();
  draw_levsel();
  clear_vp0();
  draw_hud();
  for (unsigned i = 0; i != 8; ++i) {
    ship_angle = i;
    gun_angle = i;
    draw_ship(i * 2);
  }
  ega_to_rgb();

  sg_update_image(
      state.bind.fs_images[SLOT_tex],
      &(sg_image_data){.subimage[0][0] = {.ptr = g_rgb_screen, .size = sizeof(g_rgb_screen)}});

  /*
   * Triangle strip:
   *    2     0
   *
   *    3     1
   */
  static const float vertices[][4] = {
      {1, 1, 1, 0},
      {1, -1, 1, 1},
      {-1, 1, 0, 0},
      {-1, -1, 0, 1},
  };
  state.bind.vertex_buffers[0] = sg_make_buffer(&(sg_buffer_desc){
      .data = SG_RANGE(vertices),
      .label = "rect vertices",
  });

  sg_shader blit = sg_make_shader(blit_shader_desc(sg_query_backend()));

  state.pip = sg_make_pipeline(&(sg_pipeline_desc){
      .shader = blit,
      .layout =
          {.attrs =
               {
                   [ATTR_vs_pos].format = SG_VERTEXFORMAT_FLOAT2,
                   [ATTR_vs_texcoord0].format = SG_VERTEXFORMAT_FLOAT2,
               }},
      .primitive_type = SG_PRIMITIVETYPE_TRIANGLE_STRIP,
      .label = "rect pipeline",
  });
}

static void bolo_frame(void) {
  sg_begin_default_pass(&state.pass_action, sapp_width(), sapp_height());
  sg_apply_pipeline(state.pip);
  sg_apply_bindings(&state.bind);
  sg_draw(0, 4, 1);
  sg_end_pass();
  sg_commit();
}

static void bolo_cleanup(void) {
  sg_shutdown();
}

sapp_desc sokol_main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;
  return (sapp_desc){
      .init_cb = bolo_init,
      .frame_cb = bolo_frame,
      .cleanup_cb = bolo_cleanup,
      //.event_cb = __dbgui_event,
      .width = EGA_WIDTH * 2,
      .height = EGA_HEIGHT * 2,
      .window_title = "Bolo",
      .icon.sokol_default = true,
  };
}
