// bolo - public domain Tzvetan Mikov 2021
// A modern C/C++ reimplementation of the classic Apple II Game BOLO (1982)
// https://en.wikipedia.org/wiki/Bolo_(1982_video_game)
//
// The reimplementation is based on the unofficial 1993 IBM PC port of the
// original game by Mr.Rm. Unfortunately almost nothing is known about that
// port, or the mysterious "Mr.Rm", but it appears to be a completely faithful
// reimplementation.

#include "sokol_app.h"
#include "sokol_audio.h"
#include "sokol_gfx.h"
#include "sokol_glue.h"
#include "sokol_time.h"

#include "blit.h"

#include <assert.h>
#include <stdalign.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
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

#define EGA_WIDTH 320
#define EGA_STRIDE 40
#define EGA_HEIGHT 200
#define EGA_PAGE_SIZE 0x2000
#define EGA_PLANES 4

#define EGA_WIDTH_POT 512
#define EGA_HEIGHT_POT 256

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

typedef struct {
  int8_t x;
  int8_t y;
} StepXY;

/// Step for every magnitude [0..5] and every direction [0..7].
// clang-format off
static const StepXY step_xy[6][8] = {
  // N        NE       E       SE      S       SW        W        NW
  {{0,  0}, {0,  0}, {0, 0}, {0, 0}, {0, 0}, {0,  0}, { 0, 0}, { 0,  0}},
  {{0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}},
  {{0, -2}, {2, -2}, {2, 0}, {2, 2}, {0, 2}, {-2, 2}, {-2, 0}, {-2, -2}},
  {{0, -3}, {3, -3}, {3, 0}, {3, 3}, {0, 3}, {-3, 3}, {-3, 0}, {-3, -3}},
  {{0, -4}, {4, -4}, {4, 0}, {4, 4}, {0, 4}, {-4, 4}, {-4, 0}, {-4, -4}},
  {{0, -5}, {5, -5}, {5, 0}, {5, 5}, {0, 5}, {-5, 5}, {-5, 0}, {-5, -5}},
};
// clang-format on

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

#define MAZE_WIDTH 64
#define MAZE_HEIGHT 64
#define MAZE_STRIDE 64

// Wall flags: top, right, bottom, left
// Flags all always symmetrical in neighbouring cells: a right wall in cell X
// must be followed by a left wall in cell X + 1, etc.
#define W_L 1
#define W_T 2
#define W_R 4
#define W_B 8
#define W_NO 0
#define W_ALL 0x0F

static uint8_t _ext_alist_buf[2 + MAZE_HEIGHT * MAZE_WIDTH];
/// A 64x64 buffer containing the head of list of actors in each cell. 0 means
/// EOL.
/// It starts two bytes in to protect against negative indices.
#define alist_buf (_ext_alist_buf + 2)
/// The code relies on out of range access to maze_buf (indices -1 and -2), so
/// we reserve two extra bytes in the beginning.
static uint8_t _ext_maze_buf[2 + MAZE_HEIGHT * MAZE_WIDTH];
/// A 64x64 buffer. It starts two bytes in to protect against negative indices.
#define maze_buf (_ext_maze_buf + 2)

/// Destination segment for graphics writes.
static unsigned dest_seg; // 4F8Ah

/// The size in pixels of one cell.
#define CELL_SIZE 38

#define MAZE_SCREEN_W 208
#define MAZE_SCREEN_H 191

#define NUM_ACTORS 42

/// The cell of the ship.
static uint8_t ship_cellx[NUM_ACTORS];
/// The cell of the ship.
static uint8_t ship_celly[NUM_ACTORS];
/// Offset of the ship within the cell.
static int8_t ship_ofsx[NUM_ACTORS];
/// Offset of the ship within the cell.
static int8_t ship_ofsy[NUM_ACTORS];
/// Collision flags.
static uint8_t coll_flags1[NUM_ACTORS];
/// Unknown.
static uint8_t var_188e[NUM_ACTORS]; // 506Dh
/// Unknown.
static uint8_t var_189e[32]; // 5097h
/// Ship velocity magnitude [0..5]
static uint8_t vel_magn[32]; // 50B7h
/// Ship angle [0..7]. N, NE, E, SE, S, SW, W, NW.
static uint8_t ship_angle[32]; // 50D7h
/// This looks like an index of the next element. 0 means EOL.
static int8_t next_actor[32]; // 50F7h
/// Absolute gun angle [0..7].
static uint8_t gun_angle; // 5177h
/// The angle of the ship velocity relative to the ship angle:
/// It is either 0 or 4 (when moving backwards).
static uint8_t vel_angle; // 5178h
/// Bullet x-coordinate, screen-relative.
static uint8_t bullet_x[32]; // 5179h
/// Bullet 8-coordinate, screen-relative.
static uint8_t bullet_y[32]; // 5199h
/// Bullet flags.
/// 0xFF (or likely 0x80) if out of screen.
/// 0x06 if collision.
static uint8_t bullet_flags[32]; // 51B9h
/// Angle of the moving bullet.
static uint8_t bullet_angle[32]; // 51D9h

/// Not completely sure what this is yet, but it is a 32-byte array and element
/// 0 is incremented by 2 when we press the fire button. Theory: this has to do with
/// actors (0 being the ship) firing.
static uint8_t maybe_fire[32]; // 51F9h

static struct {
  uint8_t var_194e[6]; // 521Ah
  uint8_t var_195e[6]; // 5220h
  uint8_t var_196e[6]; // 5226h
  uint8_t var_197e[18]; // 522Ch
} un1;
static uint16_t var_198e; // 523Eh

static void draw_rect(unsigned seg, unsigned x, unsigned y, unsigned wm1, unsigned hm1);
static void draw_bolo_8x20(unsigned offset);
static void vert_line(unsigned seg, unsigned x, unsigned y, unsigned lenm1);
static void horiz_line(unsigned seg, unsigned x, unsigned y, unsigned lenm1);
static void draw_mstrs(const SBOLDesc *strings);
static void draw_str(unsigned offset, const SBOL *str);
static void draw_char_1x7(unsigned offset, SBOL ch);
static void shoot_bullet(unsigned actor);
static inline bool in_screen(uint8_t x, uint8_t y);

typedef struct {
  /// The "signedness" of the resulting 8-bit values is tricky. They are declared
  /// as unsigned, but their real values are in the following ranges:
  /// x = [-48..255]
  /// y = [-60..255]
  /// Obviously there is overlap. I believe the overlap is resolved by relying
  /// on the screen size 208x191. Values above 208 and 191 respectively are
  /// outside of the screen, regardless whether they are negative or positive.
  /// TODO: we should return 16-bit (or more integers) and eliminate all this
  /// confusion (which was necessary on Apple II).
  uint8_t x /*bl*/;
  uint8_t y /*bh*/;
  /// true if the result is valid.
  bool success /*!cf*/;
} XYFlag;
static XYFlag
abs_coords(uint8_t cellX /*bl*/, uint8_t cellY /*bh*/, uint8_t ofsX /*dl*/, uint8_t ofsY /*dh*/);

static void bullet_collide(uint8_t screenX, uint8_t screenY);
static void collide_cell(
    int8_t checkCellX,
    int8_t checkCellY,
    int8_t objCellX,
    int8_t objCellY,
    uint8_t objOffsX,
    uint8_t objOffsY);
static uint8_t rnd_update(uint8_t limit);
static uint8_t rndnum(uint8_t limit);

/// A tuple containing x, y, and corresponding pointer.
typedef struct {
  uint8_t x, y;
  uint8_t *ptr;
} XYPtr;
static XYPtr maze_rnd_xy();

static uint8_t *maze_xy(int8_t x, int8_t y);
static void inc_fuel(uint8_t dl);
static uint8_t *alist_xy(int8_t x, int8_t y);

static void playBoloSound(int ch_delay, int cl_length);

static const uint8_t bmps_font_1x7[];
static const uint8_t bmp_gmaze_22x7[];
static const uint8_t sprites_1x7[];
static const uint8_t bmp_bolo_8x20[];
static const uint8_t bmp_comp_4x31[];
static const uint8_t bmps_gun_1x7[8][7];
static const uint8_t bmps_ship_1x7[8][7];

typedef struct RGBA8 {
  uint8_t r, g, b, a;
} RGBA8;

static alignas(uint32_t) uint8_t g_ega_screen[EGA_PLANES][EGA_PAGE_SIZE * 2];
static uint8_t g_ega_mask;
static unsigned g_ega_page;

#define VID_OFFSET(x, y) ((y)*EGA_STRIDE + (x) / 8)

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
static RGBA8 g_rgb_screen[EGA_WIDTH_POT * EGA_HEIGHT_POT];

/// Convert the active EGA screen page to RGB into g_rgb_screen.
static void ega_to_rgb(void) {
  const uint8_t *ptr = g_ega_screen[0] + g_ega_page * EGA_PAGE_SIZE;
  RGBA8 *out = g_rgb_screen;
  for (unsigned row = 0; row != EGA_HEIGHT; ++row) {
    unsigned pixcnt = EGA_STRIDE;
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
    out += EGA_WIDTH_POT - EGA_WIDTH;
  }
}

static inline uint8_t ega_read(unsigned offset) {
  return g_ega_screen[3][offset];
}

static inline bool ega_test(unsigned offset, uint8_t val) {
  return (ega_read(offset) & val) != 0;
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

/// Like memset() but returns a pointer to the end.
static inline uint8_t *mempset(uint8_t *dst, uint8_t value, unsigned len) {
  memset(dst, value, len);
  return dst + len;
}

/// Draw generating maze, clear alist_buf, init maze_buf
/// 2913:0321                       init_maze       proc    near
static void init_maze() {
  ega_map_mask(EGAWhite);
  draw_bmp(dest_seg + VID_OFFSET(16, 5), bmp_gmaze_22x7, 22, 7, EGAWhite);
  memset(alist_buf, 0, MAZE_WIDTH * MAZE_HEIGHT);

  // This is the state of the buffer at the end (the middle has been collapsed):
  //
  // 00: 00000000000000000000000000000000
  // 01: 00000000000000000000000000000000
  // 02: 00080808080808080808080808080000
  // 03: 040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // 04: 040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // ..  040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // ..  040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // ..  040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // ..  040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // 61: 040f0f0f0f0f0f0f0f0f0f0f0f0f0100
  // 62: 00020202020202020202020202020000
  // 63: 00000000000000000000000000000000
  //
  // Wall flags
  // 04 - right wall
  // 08 - bottom wall
  // 01 - left wall
  // 02 - top wall
  // 0f - wall on 4 sides of cell

  uint8_t *b2 = maze_buf;
  // row 0, 1, 2:0
  b2 = mempset(b2, W_NO, 2 * MAZE_WIDTH + 1);
  // row [2:1..2:61]
  b2 = mempset(b2, W_B, MAZE_WIDTH - 1 - 2);
  // row 2:62, 2:63
  *b2++ = W_NO;
  *b2++ = W_NO;
  // row 3:0
  *b2++ = W_R;
  // row [3:1 .. 61:61]
  b2 = mempset(b2, W_ALL, MAZE_WIDTH - 1 + (MAZE_HEIGHT - 8) * MAZE_WIDTH + MAZE_WIDTH - 2);
  // row 61:62
  *b2++ = W_L;
  // row 61:63
  *b2++ = W_NO;
  // row 62:0
  *b2++ = W_NO;
  // row [62:1..62:61]
  b2 = mempset(b2, W_T, MAZE_WIDTH - 1 - 2);
  // row [62:62..63:63]
  b2 = mempset(b2, W_NO, 2 + MAZE_WIDTH * 2);
  assert(b2 == maze_buf + MAZE_WIDTH * MAZE_HEIGHT && "b2 must cover maze_buf");

  // Fill the middle sides: 01, 00 on the right, followed by 04 on the left.
  // Row 3:62
  b2 = maze_buf + 3 * MAZE_WIDTH + MAZE_WIDTH - 2;
  unsigned cnt = MAZE_HEIGHT - 7;
  do {
    b2[0] = W_L;
    b2[1] = W_NO;
    b2[2] = W_R;
    b2 += MAZE_WIDTH;
  } while (--cnt);
}

#ifndef NDEBUG
void dump_maze() {
  for (unsigned y = 0; y != MAZE_HEIGHT; ++y) {
    for (unsigned x = 0; x != MAZE_WIDTH; ++x) {
      printf("%02x", maze_buf[y * MAZE_WIDTH + x]);
    }
    printf("\n");
  }
}
#endif

/// Populate the maze using the random generator.
/// 2913:0377                       gen_maze        proc    near
static void gen_maze() {
  init_maze();

  unsigned cnt = (MAZE_HEIGHT - 6) * (MAZE_WIDTH - 3) - 1;

  XYPtr xyp = maze_rnd_xy();
  unsigned x = xyp.x;
  unsigned y = xyp.y;
  uint8_t *ptr = xyp.ptr;

  // Generating the maze. All cells initially start as unconnected cells with
  // four walls. The goal of the algorithm is to ensure that in the end all
  // cells are connected (there are no unreachable cells). It achieves that by
  // repeatedly connecting new cells to the existing path of connected cells.
  //
  // On every iteration (except the first one), we are positioned in a
  // cell with at least one opening, bordering at least one unconnected cell.
  // Then we randomly remove one of walls to an unconnected cell and move into
  // the into the newly connected cell. The process repeats.
  // In this way on every iteration we are guaranteed to connect a new
  // unconnected cell to the path of already connected cells. Since the loop
  // iterates the correct number of times, by the end it is guaranteed that all
  // the cells in the maze are connected.
  //
  // Occasionally we may end up with a cell that doesn't border any unconnected
  // cells - in that case we search for the next cell with at least one missing
  // wall and try again.

  for (;;) {
    // Bits are set here if we encounter a boxed cell (all four walls set)
    // in the corresponding direction.
    uint8_t wallMask = 0;

    if (y != MAZE_HEIGHT - 4 && *(ptr + MAZE_STRIDE) >= W_ALL)
      wallMask |= W_B;
    if (x != MAZE_WIDTH - 3 && *(ptr + 1) >= W_ALL)
      wallMask |= W_R;
    if (y >= 4 && *(ptr - MAZE_STRIDE) >= W_ALL)
      wallMask |= W_T;
    if (x >= 2 && *(ptr - 1) >= W_ALL)
      wallMask |= W_L;

    // If we didn't encounter boxes in any direction, scan to the right and bottom
    // until we encounter a cell with fewer than 4 walls and retry the loop.
    // I am not sure when this can happen, but seems like a reasonable
    // precaution.
    if (wallMask == 0) {
      do {
        ++x;
        if (x == MAZE_WIDTH - 2) {
          ++y;
          if (y == MAZE_HEIGHT - 3)
            y = 3;
          x = 1;
        }
        ptr = maze_xy(x, y);
      } while (*ptr == W_ALL);

      continue;
    }

    // Directions for drilling.
    enum Drill { DBottom = 1, DRight = 2, DTop = 3, DLeft = 4 };
    // clang-format off
    static const uint8_t tab_gen_maze[15 * 4] = {
        // 0001: W_L
        04, 04, 04, 04,
        // 0010: W_T
        03, 03, 03, 03,
        // 0011: W_T, W_L
        03, 04, 04, 03,
        // 0100: W_R
        02, 02, 02, 02,
        // 0101: W_R, W_L
        04, 02, 02, 04,
        // 0110: W_R, W_T
        02, 03, 03, 02,
        // 0111: W_R, W_T, W_L
        02, 03, 04, 00,
        // 1000: W_B
        01, 01, 01, 01,
        // 1001: W_B, W_L
        01, 04, 01, 04,
        // 1010: W_B, W_T
        03, 01, 01, 03,
        // 1011: W_B, W_T, W_L
        01, 00, 03, 04,
        // 1100: W_B, W_R
        02, 01, 01, 02,
        // 1101, W_B, W_R, W_L
        04, 02, 00, 01,
        // 1110: W_B, W_R, W_T
        03, 01, 02, 00,
        // 1111: W_B, W_R, W_T, W_L
        01, 02, 03, 04,
    };
    // clang-format on
    const uint8_t *tabPtr = tab_gen_maze + (wallMask - 1) * 4;
    unsigned rndVal;
    do
      rndVal = tabPtr[rnd_update(4)];
    while (rndVal == 0);
    switch (rndVal) {
    case DBottom:
      // Remove bottom wall (and top wall in cell y + 1)
      *ptr &= ~W_B;
      ptr += MAZE_STRIDE;
      ++y;
      *ptr &= ~W_T;
      break;
    case DRight:
      // Remove right wall
      *ptr &= ~W_R;
      ++ptr;
      ++x;
      *ptr &= ~W_L;
      break;
    case DTop:
      // Remove top.
      *ptr &= ~W_T;
      ptr -= MAZE_STRIDE;
      --y;
      *ptr &= ~W_B;
      break;
    case DLeft:
      // Remove left.
      *ptr &= ~W_L;
      --ptr;
      --x;
      *ptr &= ~W_R;
      break;
    default:
      assert(false && "unreachable");
    }

    if (--cnt == 0)
      break;
  }

  // After all cells in the maze are connected, the second stage of the
  // algorithm randomly removes walls. The number of iterations depends on the
  // density.
  static const unsigned rep_table[5] = {0x1B8A, 0x1608, 0x1086, 0x0B04, 0x0582};
  unsigned rep = rep_table[density];

  do {
    xyp = maze_rnd_xy();
    x = xyp.x;
    y = xyp.y;
    ptr = xyp.ptr;
    switch (rnd_update(4)) {
    case 0:
      // Remove bottom wall.
      if (y != MAZE_HEIGHT - 4) {
        *ptr &= ~W_B;
        *(ptr + MAZE_STRIDE) &= ~W_T;
      }
      break;
    case 1:
      // Remove right wall.
      if (x != MAZE_WIDTH - 3) {
        *ptr &= ~W_R;
        *(ptr + 1) &= ~W_L;
      }
      break;
    case 2:
      // Remove top wall.
      if (y != 3) {
        *ptr &= ~W_T;
        *(ptr - MAZE_STRIDE) &= ~W_B;
      }
      break;
    case 3:
      // Remove left wall.
      if (x != 1) {
        *ptr &= ~W_L;
        *(ptr - 1) &= ~W_R;
      }
      break;
    }
  } while (--rep);
}

/// Clear both video pages and display page 0.
/// 2913:04EB                       clear_vp0       proc    near
static void clear_vp0() {
  ega_map_mask(EGAWhite);
  memset(g_ega_screen, 0, sizeof(g_ega_screen));
  dest_seg = 0;
  ega_set_page(0);
}

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

/// Draw the maze around the ship at coordinates
/// ship_cellx:ship_ofsx, ship_celly:ship_ofsy.
/// 2913:054E                       sub_10          proc    near
static void draw_maze(unsigned seg) {
  ega_map_mask(EGAWhite);
  // Cell coordinates of the top left screen corner. Note that these can
  // get negative (by design, since there is only one extra cell on the left
  // in the map).
  int screenCellX, screenCellY;
  // Pixel offset of the top left screen corner relative to the top left
  // screen cell.
  unsigned screenOfsX, screenOfsY;
  /// Pointer to the top left screen corner cell in the maze.
  const uint8_t *screenCellPtr;

  // Calculate topmost visible cell: 2 cells + 19 pixels from center.
  screenCellY = ship_celly[0] - 3;
  screenOfsY = 19 - ship_ofsy[0];
  if ((int)screenOfsY < 0) {
    screenOfsY += CELL_SIZE;
    ++screenCellY;
  }

  // Calculate leftmost visible cell: 2 cells + 31 pixels from center.
  screenCellX = ship_cellx[0] - 3;
  screenOfsX = 31 - ship_ofsx[0];
  if ((int)screenOfsX < 0) {
    screenOfsX += CELL_SIZE;
    ++screenCellX;
  }

  screenCellPtr = maze_xy(screenCellX, screenCellY);

  // Draw horizontal lines.
  const uint8_t *mazePtr = screenCellPtr;
  unsigned wallLength = screenOfsX;
  unsigned ofsX = 0;
  if (wallLength == 0) {
    wallLength = CELL_SIZE;
    ++mazePtr;
  }
  do {
    unsigned curY = screenOfsY;
    const uint8_t *curMazePtr = mazePtr;
    do {
      if (*curMazePtr & 8)
        horiz_line(seg, ofsX, curY, wallLength);

      curY += CELL_SIZE;
      curMazePtr += MAZE_STRIDE;
    } while (curY < 192);

    ofsX += wallLength;
    wallLength = CELL_SIZE;
    if (ofsX >= 170)
      wallLength = MAZE_SCREEN_W - ofsX;

    ++mazePtr;
  } while (ofsX < MAZE_SCREEN_W);

  // Draw vertical lines.
  mazePtr = screenCellPtr;
  wallLength = screenOfsY;
  unsigned ofsY = 0;
  if (wallLength == 0) {
    wallLength = CELL_SIZE;
    mazePtr += MAZE_STRIDE;
  }
  do {
    unsigned curX = screenOfsX;
    const uint8_t *curMazePtr = mazePtr;
    do {
      if (*curMazePtr & 4)
        vert_line(seg, curX, ofsY, wallLength);

      curX += CELL_SIZE;
      ++curMazePtr;
    } while (curX <= 205);

    ofsY += wallLength;
    wallLength = CELL_SIZE;

    if (ofsY >= 154)
      wallLength = 192 - ofsY;

    mazePtr += MAZE_STRIDE;
  } while (ofsY <= MAZE_SCREEN_H);
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
  const uint8_t *gunbmp = bmps_gun_1x7[gun_angle];
  const uint8_t *shipbmp = bmps_ship_1x7[(ship_angle[0] + vel_angle) & 7];
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
    // Record the collision.
    var_188e[0] = 0xA;
    coll_flags1[0] = 1;
    // Stop the ship.
    vel_magn[0] = 0;
    var_189e[0] = 0xA;
  }
}

/// Draw a sequence of string/offset pairs terminated with a null string ptr.
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

/// Move all bullets while checking if they are in screen and for collision.
/// 2913:1623                       sub_1623h       proc    near
static void update_bullets(unsigned seg) {
  ega_map_mask(EGAYellow);

  unsigned i = 30;
  do {
    if (bullet_flags[i] != 0)
      continue;

    StepXY step = step_xy[1][bullet_angle[i]];

    uint8_t x = bullet_x[i];
    uint8_t y = bullet_y[i];

    // Check the bullet for collision and in-screen 12 steps ahead.
    uint8_t curX = x;
    uint8_t curY = y;
    unsigned steps = 12;
    do {
      if (ega_test(seg + vid_offset(curX, curY), vid_mask(curX)) == 0) {
        curX += step.x;
        curY += step.y;

        if (!in_screen(curX, curY)) {
          bullet_flags[i] = 0xFF;
          goto nextBullet;
        }
      } else {
        bullet_flags[i] = 6;
        bullet_x[i] = curX;
        bullet_y[i] = curY;
        bullet_collide(curX, curY);
        goto nextBullet;
      }
    } while (--steps);

    // Draw the bullet 8 steps ahead.
    x = x + step.x * 8;
    y = y + step.y * 8;

    if (in_screen(x, y)) {
      ega_or(seg + vid_offset(x, y), vid_mask(x), EGAYellow);
      bullet_x[i] = x;
      bullet_y[i] = y;
    } else {
      bullet_flags[i] = 0xFF;
    }

  nextBullet:;
  } while (i--);
}

/// Bullets coordinates are screen-relative, so they have to be adjusted as we
/// move the ship/screen. We adjust them in the opposite direction of the ship.
/// 2913:169C                       sub_20          proc    near
static void adjust_bullets() {
  if (vel_magn[0] == 0)
    return;
  StepXY step = step_xy[vel_magn[0]][(vel_angle + ship_angle[0]) & 7];
  unsigned i = 30;
  do {
    if (bullet_flags[i] & 0x80)
      continue;
    if (in_screen(bullet_x[i], bullet_y[i])) {
      bullet_x[i] -= step.x;
      bullet_y[i] -= step.y;
    } else {
      bullet_flags[i] = 0xFF;
    }
  } while (i--);
}

/// Start position of the bullet depending on the shooting angle.
static const int8_t bullet_start[8][2] = {
    {3, -1},
    {7, -1},
    {7, 3},
    {7, 7},
    {3, 7},
    {-1, 7},
    {-1, 3},
    {-1, -1},
};

/// Shoot a bullet from the specified actor, considering the actor's direction.
/// If the actor is 0, use gun_angle.
/// 2913:16F0                       proc_19          proc    near
static void shoot_bullet(unsigned actor) {
  XYFlag xyf = abs_coords(ship_cellx[actor], ship_celly[actor], ship_ofsx[actor], ship_ofsy[actor]);
  if (xyf.success) {
    uint8_t x = xyf.x;
    uint8_t y = xyf.y;
    // Look for an available bullet spot.
    int i = 30;
    do {
      if (bullet_flags[i] & 0x80)
        break;
    } while (--i >= 0);
    // Couldn't find one?
    if (i < 0)
      return;

    bullet_flags[i] = 0;
    uint8_t angle = actor == 0 ? gun_angle : ship_angle[actor];
    bullet_angle[i] = angle;
    x += bullet_start[angle][0];
    y += bullet_start[angle][1];
    bullet_x[i] = x;
    bullet_y[i] = y;
  }
  playBoloSound(32, 30);
}

// \return true if x < MAZE_SCREEN_W and y < MAZE_SCREEN_H;
// 2913:1762                       sub_22          proc    near
static inline bool in_screen(uint8_t x, uint8_t y) {
  return x < MAZE_SCREEN_W && y < MAZE_SCREEN_H;
}

/// Convert cell coordinate and offset into screen-relative coordinates if they
/// approximately fit in the screen. Return the screen-relative coordinates and
/// flag=true on success, -1,-1, false if the coordinates don't fit.
///
/// The "signedness" of the resulting 8-bit values is tricky. They are declared
/// as unsigned, but their real values are in the following ranges:
/// x = [-48..255]
/// y = [-60..255]
/// Obviously there is overlap. I believe the overlap is resolved by relying
/// on the screen size 208x191. Values above 208 and 191 respectively are
/// outside of the screen, regardless whether they are negative or positive.
/// TODO: we should return 16-bit (or more integers) and eliminate all this
/// confusion (which was necessary on Apple II).
///
/// 2913:1770                       proc_20          proc    near
static XYFlag
abs_coords(uint8_t cellX /*bl*/, uint8_t cellY /*bh*/, uint8_t ofsX /*dl*/, uint8_t ofsY /*dh*/) {
  // ship_cellx[0] - 3 is "start of screen" cell.
  // x is distance of the ship from start of screen.
  int8_t relCellX = cellX - (ship_cellx[0] - 3);
  // At most 7 cells are visible horizontally.
  if (relCellX >= 0 && relCellX < 7) {
    // The original logic looks like this:
    //   int8_t tmp = relCellX << 5; // Only positive if relCellX < 4.
    //   int8_t x = ofsX + relCellX * CELL_SIZE;
    //   if (x < 0 || tmp >= 0) {
    // What does it do???
    //
    // This appears like an elaborate way of checking whether the result of the
    // calculation fits in 8 unsigned bits. I suspect it is a remnant of the
    // original 8-bit Apple II logic.
    // The input values have these ranges:
    //   ofsX = [0..37]
    //   relCellX = [0..6]
    // These are the possible values for every relCellX:
    //   [0..37] + 0*38 = [  0.. 37]
    //   [0..37] + 1*38 = [ 38.. 75]
    //   [0..37] + 2*38 = [ 76..113]
    //   [0..37] + 3*38 = [114..151]             [114..127,-128..-105]
    //   [0..37] + 4*38 = [152..189]             [-104..-67]
    //   [0..37] + 5*38 = [190..227]             [-66..-29]
    //   [0..37] + 6*38 = [228..255, 0..9]       [-28..-1, 0..9]
    // Observe two things:
    // - Values get the sign bit set in the middle of relCellX = 3
    // - The sign bit resets back to zero at the end relCellX = 6
    // The code is trying to distinguish between two cases. If the sign bit of
    // "x" is set, its real value is between 127 and 255, so it fits. If it
    // isn't set, its real value could be [256..265] or it could be [0..127].
    // To find out which, just check the value of relCellX. if it is less than
    // 4, then the real value is [0..151], so it fits.
    //
    // If we use more than 16-bit arithmetic, the condition looks like this:
    //   x > 127 && x < 256 || x < 152
    // or simply:
    //   x < 256
    unsigned x = ofsX + relCellX * CELL_SIZE;
    if (x < 256) {
      x -= ship_ofsx[0] + 10;

      int8_t relCellY = cellY - (ship_celly[0] - 3);
      if (relCellY >= 0 && relCellY < 7) {
        unsigned y = ofsY + relCellY * CELL_SIZE;
        y -= ship_ofsy[0] + 22;
        return (XYFlag){.x = x, .y = y, .success = true};
      }
    }
  }

  return (XYFlag){.x = -1, .y = -1, .success = false};
}

typedef struct {
  uint8_t x_quot, y_quot;
  uint8_t x_rem, y_rem;
} XYDiv;

/// Divide x and y by cell size and return the quotiens and remainders.
/// 2913:17CC                       proc_21         proc    near
static XYDiv div_cell_size(int x, int y) {
  if (0) {
    uint8_t xcells = -1;
    do {
      ++xcells;
      x -= CELL_SIZE;
      // NOTE: originally this condition used clever unsigned 8-bit arithmetic in
      // order to handle inputs in range [0..207]: ((uint8_t)bl_x < (uint8_t)-CELL_SIZE)
    } while (x >= 0);
    x += CELL_SIZE;

    uint8_t ycells = -1;
    do {
      ++ycells;
      y -= CELL_SIZE;
    } while (y >= 0);
    y += CELL_SIZE;

    return (XYDiv){.x_quot = xcells, .y_quot = ycells, .x_rem = x, .y_rem = y};
  } else {
    div_t xd = div(x, CELL_SIZE);
    div_t yd = div(y, CELL_SIZE);
    return (XYDiv){.x_quot = xd.quot, .y_quot = yd.quot, .x_rem = yd.rem, .y_rem = yd.rem};
  }
}

/// Invoked upon a bullet collision. Coordinates are screen-relative.
/// 2913:17EF                       bullet_collide  proc    near
static void bullet_collide(uint8_t screenX, uint8_t screenY) {
  if ((screenX >= 107 - 7 && screenX < 107 + 8) && (screenY >= 95 - 7 && screenY < 95 + 8)) {
    var_188e[0] = 0x0A;
    coll_flags1[0] = 1;
  }

  XYDiv xydiv = div_cell_size(screenX + 13 + ship_ofsx[0], screenY + 19 + ship_ofsy[0]);
  int8_t cellX = xydiv.x_quot - 3 + ship_cellx[0];
  int8_t cellY = xydiv.y_quot - 3 + ship_celly[0];
  uint8_t offsX = xydiv.x_rem;
  uint8_t offsY = xydiv.y_rem;

  collide_cell(cellX, cellY, cellX, cellY, offsX, offsY);

  /// If not 0, the horizontal offset of the cell that must also be checked.
  int8_t alsoCellX = offsX < 6 ? -1 : (offsX >= 32 ? 1 : 0);
  if (alsoCellX)
    collide_cell(cellX + alsoCellX, cellY, cellX, cellY, offsX, offsY);

  if (offsY < 6) {
    collide_cell(cellX, cellY - 1, cellX, cellY, offsX, offsY);
    if (alsoCellX)
      collide_cell(cellX + alsoCellX, cellY - 1, cellX, cellY, offsX, offsY);
    return;
  }

  if (offsY >= 32) {
    collide_cell(cellX, cellY + 1, cellX, cellY, offsX, offsY);
    if (alsoCellX)
      collide_cell(cellX + alsoCellX, cellY + 1, cellX, cellY, offsX, offsY);
  }
}

/// Check the list of actors in the specified cell for collision with a
/// colliding object.
/// \param checkCellX, checkCellY  coordinates of the cell to check.
/// 2913:1893                       proc_22         proc    near
static void collide_cell(
    int8_t checkCellX,
    int8_t checkCellY,
    int8_t objCellX,
    int8_t objCellY,
    uint8_t objOffsX,
    uint8_t objOffsY) {
  for (int act = *alist_xy(checkCellX, checkCellY); act != 0; act = next_actor[act]) {
    if (coll_flags1[act]) {
      if (coll_flags1[act] == 1 || (coll_flags1[act] & 0x70) == 0x10)
        continue;
    }

    int xadj =
        objCellX == ship_cellx[act] ? 0 : (objCellX < ship_cellx[act] ? -CELL_SIZE : CELL_SIZE);
    int dist = xadj + objOffsX - ship_ofsx[act];
    if (dist < -6 || dist >= 7)
      continue;

    int yadj =
        objCellY == ship_celly[act] ? 0 : (objCellY < ship_celly[act] ? -CELL_SIZE : CELL_SIZE);
    dist = yadj + objOffsY - ship_ofsy[act];
    if (dist < -6 || dist >= 7)
      continue;

    uint8_t coll70 = coll_flags1[act] & 70;
    if (coll70 == 0x60)
      continue;

    if (coll70)
      un1.var_194e[coll_flags1[act] & 0x0F] = 0;

    var_188e[act] = 0x0A;
    coll_flags1[act] = 1;

    inc_fuel(1);
  }
}

/// 2913:1CC7                       inc_fuel        proc    near
static void inc_fuel(uint8_t dl) {}

/// Calculate and return a pointer to alist_buf element.
/// Note that the parameters are deliberately signed. It works exactly like
/// maze_xy().
/// TODO: change the parameters to int and have the caller take care of the
///       expansion.
/// 2913:2305                       alist_readxy     proc    near
static uint8_t *alist_xy(int8_t x, int8_t y) {
  int ofs = y * MAZE_WIDTH + x;
  assert(ofs >= -2);
  return alist_buf + ofs;
}

/// Return a pseudo random value in the range [0..power-of-two).
/// \param limit must be a power of 2. It is used to mask the result.
/// The random generator state is updated.
/// 2913:2ED1                       rnd_update      proc    near
static uint8_t rnd_update(uint8_t limit) {
  uint8_t save_t5b = time_5bit;
  time_5bit = (time_5bit + 13) & TIME_5BIT_MASK;

  uint_fast16_t tmp = rnd_state[save_t5b] + rnd_state[time_5bit];
  tmp += (tmp >> 8) + 1;
  rnd_state[save_t5b] = (uint8_t)tmp;

  return (uint8_t)tmp & (limit - 1);
}

/// Generate a random number in the range [0..limit).
/// 2913:2EFA                       rndnum          proc    near
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

/// Output: DL: x, DH: y, AL: maze_buf[x,y]
/// Generate random numbers x=[1..MAZE_WIDTH-3], y=[3..MAZE_HEIGHT-4] until maze_buf[x,y] >= 0.
/// 2913:2F18                       maze_rnd_xy     proc    near
static XYPtr maze_rnd_xy() {
  uint8_t x, y;
  uint8_t *ptr;
  do {
    do
      x = rnd_update(MAZE_WIDTH);
    while (x >= MAZE_WIDTH - 2 || x == 0);
    do
      y = rnd_update(MAZE_HEIGHT);
    while (y >= MAZE_HEIGHT - 3 || y < 3);
    ptr = maze_xy(x, y);
  } while (*ptr & 0x80);
  return (XYPtr){.x = x, .y = y, .ptr = ptr};
}

/// Calculate and return a pointer to a cell maze.
/// (This function used to be called maze_readxy() and used to return a value
/// as well as a pointer.)
/// Note that the parameters are deliberately signed. draw_maze occasionally
/// generates negative coords, and they need to work correctly.
/// TODO: change the parameters to int and have the caller take care of the
///       expansion.
/// 2913:2F3C                       maze_readxy     proc    near
static uint8_t *maze_xy(int8_t x, int8_t y) {
  int ofs = y * MAZE_WIDTH + x;
  assert(ofs >= -2);
  return maze_buf + ofs;
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

static const uint8_t bmp_gmaze_22x7[] = {
    // 22 x 7 (154 bytes)
    0x3f, 0x0f, 0xfc, 0xc0, 0xcf, 0xfc, 0xff, 0x00, 0xc0, 0xff, 0xcf, 0xfc, 0xc0, 0xc3, 0xf0, 0x00,
    0x30, 0x30, 0x30, 0x3f, 0xf3, 0xff, 0xc0, 0xcc, 0x00, 0xf0, 0xcc, 0x00, 0xc0, 0xc3, 0x30, 0x0c,
    0x00, 0xc0, 0xf0, 0xcc, 0x0c, 0x00, 0x3c, 0xf0, 0xcc, 0x00, 0x33, 0x00, 0xc0, 0x0c, 0x00, 0xd8,
    0xcc, 0x00, 0xc0, 0xcc, 0x0c, 0x0c, 0x00, 0xc0, 0xd8, 0xcc, 0x00, 0x00, 0x33, 0x33, 0x03, 0x00,
    0xc3, 0x00, 0xcf, 0xcf, 0xf0, 0xcc, 0xcf, 0xf0, 0xff, 0x0f, 0xfc, 0x0c, 0x00, 0xc0, 0xcc, 0xcc,
    0xfc, 0x00, 0x30, 0x33, 0xff, 0x03, 0x03, 0xfc, 0xc0, 0xcc, 0x00, 0xc6, 0xcc, 0x00, 0xc0, 0xcc,
    0x0c, 0x0c, 0x00, 0xc0, 0xc6, 0xcc, 0x0c, 0x00, 0x30, 0x33, 0x03, 0x0c, 0x03, 0x00, 0xc0, 0xcc,
    0x00, 0xc3, 0xcc, 0x00, 0xc0, 0xcc, 0x0c, 0x0c, 0x00, 0xc0, 0xc3, 0xcc, 0x0c, 0x00, 0x30, 0x33,
    0x03, 0x30, 0x03, 0x00, 0x3f, 0x0f, 0xfc, 0xc0, 0xcf, 0xfc, 0xc0, 0xcc, 0x0c, 0x0c, 0x0f, 0xfc,
    0xc0, 0xc3, 0xf0, 0x00, 0x30, 0x33, 0x03, 0x3f, 0xf3, 0xff,
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

static const uint8_t bmps_gun_1x7[8][7] = {
    {0x10, 0x10, 0x10, 0x10, 0x00, 0x00, 0x00},
    {0x02, 0x04, 0x08, 0x10, 0x00, 0x00, 0x00},
    {0, 0, 0, 0x1E, 0, 0, 0},
    {0, 0, 0, 0x10, 0x08, 0x04, 0x02},
    {0, 0, 0, 0x10, 0x10, 0x10, 0x10},
    {0, 0, 0, 0x10, 0x20, 0x40, 0x80},
    {0, 0, 0, 0xF0, 0, 0, 0},
    {0x80, 0x40, 0x20, 0x10, 0, 0, 0},
};

static const uint8_t bmps_ship_1x7[8][7] = {
    {0x00, 0x44, 0xFE, 0xFE, 0xFE, 0x44, 0x00},
    {0x30, 0x70, 0xF8, 0xFE, 0x3E, 0x1C, 0x18},
    {0x38, 0x7C, 0x38, 0x38, 0x38, 0x7C, 0x38},
    {0x18, 0x1C, 0x3E, 0xFE, 0xF8, 0x70, 0x30},
    // NOTE: bottom part is the same as top part.
    {0x00, 0x44, 0xFE, 0xFE, 0xFE, 0x44, 0x00},
    {0x30, 0x70, 0xF8, 0xFE, 0x3E, 0x1C, 0x18},
    {0x38, 0x7C, 0x38, 0x38, 0x38, 0x7C, 0x38},
    {0x18, 0x1C, 0x3E, 0xFE, 0xF8, 0x70, 0x30},
};

#define SOUND_QUEUE_CAPACITY 8192

typedef struct {
  /// Index of next element to read. Accessed only by the reader thread.
  int head;
  /// Index of next element write. Accessed only by the writer thread.
  int tail;
  /// Number of elements in queue. Shared by the reader and writer thread.
  atomic_int count;
  /// The actual data.
  float samples[SOUND_QUEUE_CAPACITY];
} sound_queue_t;

static struct {
  sg_pass_action pass_action;
  sg_pipeline pip;
  sg_bindings bind;

  double lastUpdate;
  sound_queue_t fx;
} state;

static void sound_queue_init(sound_queue_t *q) {
  q->head = 0;
  q->tail = 0;
  atomic_init(&q->count, 0);
}

static int sound_queue_expect(sound_queue_t *q) {
  return SOUND_QUEUE_CAPACITY - atomic_load_explicit(&q->count, memory_order_acquire);
}

static int sound_queue_count(sound_queue_t *q) {
  return atomic_load_explicit(&q->count, memory_order_acquire);
}

typedef struct {
  float *part1;
  int size1;
  float *part2;
  int size2;
} queue_parts_t;

static queue_parts_t sound_queue_writeparts(sound_queue_t *q, int len) {
  int expect = sound_queue_expect(q);
  if (len > expect)
    len = expect;

  if (len <= SOUND_QUEUE_CAPACITY - q->tail) {
    return (queue_parts_t){.part1 = q->samples + q->tail, .size1 = len};
  } else {
    int toCopy = SOUND_QUEUE_CAPACITY - q->tail;
    return (queue_parts_t){
        .part1 = q->samples + q->tail, .size1 = toCopy, .part2 = q->samples, .size2 = len - toCopy};
  }
}

static queue_parts_t sound_queue_readparts(sound_queue_t *q, int len) {
  int count = sound_queue_count(q);
  if (len > count)
    len = count;

  if (len <= SOUND_QUEUE_CAPACITY - q->head) {
    return (queue_parts_t){.part1 = q->samples + q->head, .size1 = len};
  } else {
    int toCopy = SOUND_QUEUE_CAPACITY - q->head;
    return (queue_parts_t){
        .part1 = q->samples + q->head, .size1 = toCopy, .part2 = q->samples, .size2 = len - toCopy};
  }
}

static void sound_queue_adv_tail(sound_queue_t *q, int len) {
  q->tail = (q->tail + len) & (SOUND_QUEUE_CAPACITY - 1);
  atomic_fetch_add_explicit(&q->count, len, memory_order_release);
}

static void sound_queue_adv_head(sound_queue_t *q, int len) {
  q->head = (q->head + len) & (SOUND_QUEUE_CAPACITY - 1);
  atomic_fetch_add_explicit(&q->count, -len, memory_order_release);
}

static int sound_queue_push(sound_queue_t *q, const float *data, int len) {
  queue_parts_t parts = sound_queue_writeparts(q, len);
  memcpy(parts.part1, data, parts.size1 * sizeof(float));
  if (parts.size2)
    memcpy(parts.part2, data + parts.size1, parts.size2 * sizeof(float));
  sound_queue_adv_tail(q, parts.size1 + parts.size2);
  return parts.size1 + parts.size2;
}

static int sound_queue_pop(sound_queue_t *q, float *data, int len) {
  queue_parts_t parts = sound_queue_readparts(q, len);
  memcpy(data, parts.part1, sizeof(float) * parts.size1);
  if (parts.size2)
    memcpy(data + parts.size1, parts.part2, sizeof(float) * parts.size2);
  sound_queue_adv_head(q, parts.size1 + parts.size2);
  return parts.size1 + parts.size2;
}

static void bolo_sound_cb(float *buffer, int num_frames, int num_channels) {
  int popped;
  if (num_channels == 1) {
    popped = sound_queue_pop(&state.fx, buffer, num_frames);
  } else if (num_channels == 2) {
    queue_parts_t parts = sound_queue_readparts(&state.fx, num_frames);
    int i;
    for (i = 0; i < parts.size1; ++i) {
      buffer[0] = buffer[1] = parts.part1[i];
      buffer += 2;
    }
    for (i = 0; i < parts.size2; ++i) {
      buffer[0] = buffer[1] = parts.part2[i];
      buffer += 2;
    }
    popped = parts.size1 + parts.size2;
    sound_queue_adv_head(&state.fx, popped);
  } else {
    popped = 0;
  }

  if (popped < num_frames) {
    // Fill the rest of the frame with zeroes.
    memset(buffer + popped * num_channels, 0, sizeof(float) * (num_frames - popped) * num_channels);
  }
}

static void bolo_update_screen() {
  ega_to_rgb();

  sg_update_image(
      state.bind.fs_images[SLOT_tex],
      &(sg_image_data){.subimage[0][0] = {.ptr = g_rgb_screen, .size = sizeof(g_rgb_screen)}});
}

static inline uint32_t nearest_pow2(uint32_t x) {
  --x;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return x + 1;
}

static void bolo_init(void) {
  saudio_setup(&(saudio_desc){
      //.sample_rate = 44100,
      //.buffer_frames = 2048,
      .stream_cb = bolo_sound_cb,
      .num_channels = 1,
  });

  stm_setup();
  state.lastUpdate = 0;

  sg_setup(&(sg_desc){.context = sapp_sgcontext()});

  state.pass_action = (sg_pass_action){.colors[0] = {.action = SG_ACTION_CLEAR}};

  state.bind.fs_images[SLOT_tex] = sg_make_image(&(sg_image_desc){
      .width = nearest_pow2(EGA_WIDTH),
      .height = nearest_pow2(EGA_HEIGHT),
      .usage = SG_USAGE_STREAM,
      .min_filter = SG_FILTER_LINEAR,
      .mag_filter = SG_FILTER_LINEAR,
      .label = "ega_image",
  });

  // title_screen();
  // draw_hud();
  // draw_levsel();
  // clear_vp0();
  // draw_hud();
  // draw_levsel();
  density = 4;
  gen_maze();
  ship_cellx[0] = 1;
  ship_celly[0] = 3;
  ship_ofsx[0] = CELL_SIZE / 2;
  ship_ofsy[0] = CELL_SIZE / 2;

  // Mark all bullets as out of screen.
  memset(bullet_flags, 0xFF, sizeof(bullet_flags));

  /*
   * Triangle strip:
   *    2  |  0
   * ------+------
   *    3  |  1
   */
  static const float U = (float)EGA_WIDTH / EGA_WIDTH_POT;
  static const float V = (float)EGA_HEIGHT / EGA_HEIGHT_POT;
  static const float vertices[][4] = {
      {1, 1, U, 0},
      {1, -1, U, V},
      {-1, 1, 0, 0},
      {-1, -1, 0, V},
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

/*
  This is how BOLO generates sound. CH is the inner loop delay, CL is how many
  times to flip the speaker. 8088 instruction cycles are written after every
  instruction.
                  mov     cl,30

  locloop_146:
                  mov     ch,32          [ 4]
                  in      al,61h         [14]     ; port 61h, 8255 port B, read
                  xor     al,data_180    [19]     ; (2913:2F66=2)
                  out     61h,al         [14]     ; port 61h, 8255 B - spkr, etc
  loc_147:
                  dec     ch             [ 3]
                  jnz     loc_147        [16 or 4]
                  loop    locloop_146    [17 or 5]

  Sound flip: 4+14+19+14 = 51
  Delay loop: (CH-1)*19 + 7 = 19*CH - 19 + 7 = 19*CH - 12
  Loop period: 51 + 19*CH - 12 + 17 = 19*CH + 56 cycles
  Sound freq: 4.77MHz / (loop period * 2)
  One cycle at 4.77MHz is 210ns (210e-9).
  Loop period:

  In this case: 19*32 + 56 = 664;  4.77e6 / 664 = 3592Hz for 4.1 ms
 */

/// Play a BOLO sound.
static void playBoloSound(int ch_delay, int cl_length) {
  static const int kCycleNs = 210;
  static const int kCPUFreq = 4770000;
  int loopPeriodCyc = (19 * ch_delay + 56) * 2;
  int loopRate = kCPUFreq / loopPeriodCyc;

  enum { kBufSize = 2048 };
  static float buffer[kBufSize];

  const int sampRate = saudio_sample_rate();
  int outLen = sampRate * cl_length / loopRate;
  // In the unlikely even that the buffer is not sufficient, truncate.
  if (outLen > kBufSize)
    outLen = kBufSize;

  float *p = buffer;
  float input = -0.1f;
  int acc = 0;
  int cnt = outLen;
  while (cnt--) {
    *p++ = input;
    acc += loopRate;
    while (acc >= sampRate) {
      acc -= sampRate;
      input = -input;
    }
  }

  sound_queue_push(&state.fx, buffer, outLen);
}

static int ship_dx = 0;
static int ship_dy = 0;
static bool fire_req = false;

static void render_test_frame(void) {
  clear_vp0();
  draw_hud();
  draw_maze(0);

  draw_ship(0);

  if (coll_flags1[0]) {
    coll_flags1[0] = 0;
    ship_cellx[0] = 1;
    ship_celly[0] = 3;
    ship_ofsx[0] = ship_ofsy[0] = CELL_SIZE / 2;
    ship_dx = 0;
    ship_dy = 0;
  } else {
    ship_ofsx[0] += ship_dx;
    if (ship_ofsx[0] < 0) {
      ship_ofsx[0] += 38;
      ship_cellx[0] -= 1;
    } else if (ship_ofsx[0] >= 38) {
      ship_ofsx[0] -= 38;
      ship_cellx[0] += 1;
    }
    ship_ofsy[0] += ship_dy;
    if (ship_ofsy[0] < 0) {
      ship_ofsy[0] += 38;
      ship_celly[0] -= 1;
    } else if (ship_ofsy[0] >= 38) {
      ship_ofsy[0] -= 38;
      ship_celly[0] += 1;
    }
  }

  if (fire_req) {
    fire_req = false;
    shoot_bullet(0);
  }
  update_bullets(0);
  adjust_bullets();

  // Temp hack. Clear bullets that are supposed to explode.
  for (unsigned i = 0; i != 31; ++i) {
    if (bullet_flags[i] == 6)
      bullet_flags[i] = 0xFF;
  }
}

static void bolo_frame(void) {
  double newTime = stm_sec(stm_now());

  if (state.lastUpdate == 0)
    state.lastUpdate = newTime - 1. / 18.2;
  if (newTime - state.lastUpdate >= 1. / 18.2) {
    do {
      state.lastUpdate += 1. / 18.2;
      render_test_frame();
    } while (newTime - state.lastUpdate >= 1. / 18.2);
    bolo_update_screen();
  }

  sg_begin_default_pass(&state.pass_action, sapp_width(), sapp_height());

  {
    // We always preserve the 320x200 aspect ratio. We don't care about the
    // 1.2x1 pixel aspect ratio of the original CRT monitors.
    int w = sapp_width();
    int h = sapp_height();
    int desiredW, desiredH;

    if (w * EGA_HEIGHT / h >= EGA_WIDTH) {
      desiredH = h;
      desiredW = h * EGA_WIDTH / EGA_HEIGHT;
    } else {
      desiredW = w;
      desiredH = w * EGA_HEIGHT / EGA_WIDTH;
    }
    sg_apply_viewport((w - desiredW) / 2, (h - desiredH) / 2, desiredW, desiredH, true);
  }

  sg_apply_pipeline(state.pip);
  sg_apply_bindings(&state.bind);
  sg_draw(0, 4, 1);
  sg_end_pass();
  sg_commit();
}

static void bolo_cleanup(void) {
  sg_shutdown();
  saudio_shutdown();
}

static int angleVel[][2] = {
    {0, -1},
    {1, -1},
    {1, 0},
    {1, 1},
    {0, 1},
    {-1, 1},
    {-1, 0},
    {-1, -1},
};
static bool keys[512];
static void bolo_event(const sapp_event *ev) {
  if (ev->type == SAPP_EVENTTYPE_KEY_DOWN) {
    keys[ev->key_code] = true;
  } else if (ev->type == SAPP_EVENTTYPE_KEY_UP) {
    keys[ev->key_code] = false;
  }
  if (ev->type == SAPP_EVENTTYPE_KEY_DOWN) {
    switch (ev->key_code) {
    case SAPP_KEYCODE_1:
      gun_angle = (gun_angle - 1) & 7;
      break;
    case SAPP_KEYCODE_2:
      gun_angle = (gun_angle + 1) & 7;
      break;
    case SAPP_KEYCODE_LEFT:
      ship_angle[0] = (ship_angle[0] - 1) & 7;
      gun_angle = (gun_angle - 1) & 7;
      break;
    case SAPP_KEYCODE_RIGHT:
      ship_angle[0] = (ship_angle[0] + 1) & 7;
      gun_angle = (gun_angle + 1) & 7;
      break;
    default:
      break;
    }
  }

  if (keys[SAPP_KEYCODE_UP]) {
    ship_dx = angleVel[ship_angle[0]][0];
    ship_dy = angleVel[ship_angle[0]][1];
  } else if (keys[SAPP_KEYCODE_DOWN]) {
    ship_dx = -angleVel[ship_angle[0]][0];
    ship_dy = -angleVel[ship_angle[0]][1];
  } else {
    ship_dx = ship_dy = 0;
  }

  if (keys[SAPP_KEYCODE_SPACE]) {
    fire_req = true;
  }
}

sapp_desc sokol_main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;
  return (sapp_desc){
      .init_cb = bolo_init,
      .frame_cb = bolo_frame,
      .cleanup_cb = bolo_cleanup,
      .event_cb = bolo_event,
      .width = EGA_WIDTH * 2,
      .height = EGA_HEIGHT * 2,
      .window_title = "Bolo",
      .icon.sokol_default = true,
  };
}
