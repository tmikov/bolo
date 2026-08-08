// bolo - public domain Tzvetan Mikov 2021
//
// The interface between the game logic in bolo.c and its front ends:
// shell_sokol.c drives it as a windowed app, emu/bolotest.c drives it
// headlessly.

#ifndef BOLO_H
#define BOLO_H

#include <stdint.h>

/// The period of the original's timer interrupt (INT 08h), 18.2 Hz.
#define TIMER_PERIOD_US 54925

#define EGA_WIDTH 320
#define EGA_STRIDE 40
#define EGA_HEIGHT 200
#define EGA_PAGE_SIZE 0x2000
#define EGA_PLANES 4

#define EGA_WIDTH_POT 512
#define EGA_HEIGHT_POT 256

/// Visible bytes in one page of one plane. The page is EGA_PAGE_SIZE (8192)
/// bytes, but only the first 8000 are on screen.
#define EGA_PAGE_VISIBLE (EGA_HEIGHT * EGA_STRIDE)

enum ScanCode {
  SC_ESC = 1,
  SC_1 = 2,
  SC_9 = 0x0A,
  SC_0 = 0x0B,
  SC_W = 0x11,
  SC_ENTER = 0x1C,
  SC_A = 0x1E,
  SC_S = 0x1F,
  SC_D = 0x20,
  SC_X = 0x2D,
  SC_B = 0x30,
  SC_SPACE = 0x39,
  SC_F1 = 0x3B,
  SC_F2 = 0x3C,
  SC_F3 = 0x3D,
  SC_UP = 0x48,
  SC_LEFT = 0x4B,
  SC_RIGHT = 0x4D,
  SC_DOWN = 0x50,
};

typedef struct RGBA8 {
  uint8_t r, g, b, a;
} RGBA8;

#endif // BOLO_H
