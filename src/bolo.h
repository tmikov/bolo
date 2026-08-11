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

/// Must be called once before any other bolo_* function.
///
/// Aborts if called twice. The game's state lives in file-scope statics that
/// the C runtime initializes once at program start, so a second run in the
/// same process would silently inherit the first run's state. Use a new
/// process (or fork()) per run.
void bolo_reset(void);

/// Deliver one timer interrupt, as the original's INT 08h handler did.
void bolo_timer_tick(void);

/// Run one slice of the game's async state machine, returning when it yields.
void bolo_step(void);

/// bolo_timer_tick() followed by `pump` calls to bolo_step().
///
/// The gameplay loop is gated on the timer tick, so any `pump >= 1` produces
/// identical gameplay; larger values only advance the non-tick-gated intro
/// states (title screen, maze generation) faster. The windowed app effectively
/// uses ~3-4, being a 60Hz render loop over an 18.2Hz tick.
void bolo_run_tick(int pump);

/// Deliver a key, as the original's INT 09h handler did.
void bolo_key(uint8_t scanCode);

/// EGA_PAGE_VISIBLE bytes of plane `plane` (0..3), as they stand after the
/// count'th crossing of the frame gate (see bolo_frame_count()).
///
/// This is the frame drawn *during* that gate crossing's iteration, not the
/// frame that was on screen when the gate was crossed: `async_start` case 12
/// increments the frame count at the point the original calls `flip_vp`, but
/// control does not return to the caller there — it falls through and draws
/// an entirely new frame into page 0 before finally yielding. So when
/// bolo_run_tick() returns with bolo_frame_count() == k, this buffer holds
/// the frame drawn in iteration k, while the original's k-th capture at
/// `029A` shows the frame drawn in iteration k-1. To compare the two,
/// align the port's planes at bolo_frame_count() == k with the original's
/// capture k+1.
const uint8_t *bolo_plane(int plane);

/// The 16 EGA colors.
const RGBA8 *bolo_palette(void);

/// Number of completed gameplay frames.
///
/// Incremented where the game consumes a tick at its frame gate, which is the
/// point at which the original calls flip_vp. Intro screens never reach that
/// gate, so this stays 0 until gameplay begins.
unsigned bolo_frame_count(void);

/// Called when the game requests a sound, carrying the original's PC speaker
/// loop delay and length. NULL (the default) discards sounds.
extern void (*bolo_sound_sink)(int ch_delay, int cl_length);

/// One piece of game state that the port and the original both keep, named by
/// the address the original keeps it at.
///
/// The port mirrors the original's data segment (see "Game state layout" in
/// CLAUDE.md), so a variable annotated `// 4F9Bh` in bolo.c holds the same
/// bytes as the original's 2913:4F9B. This makes that annotation checkable
/// rather than merely documentary, and lets the comparison harness answer
/// "which variable diverged first", which pixels cannot: game state can differ
/// for many frames before it reaches the screen.
typedef struct BoloStateVar {
  /// Offset in the original's data segment.
  uint16_t addr;
  /// The port's name for it, for reporting.
  const char *name;
  /// The port's storage.
  const void *data;
  /// Number of elements.
  uint16_t count;
  /// Bytes per element in the port's storage.
  uint8_t stride;
  /// Bytes per element to compare, which is the original's width. Less than
  /// `stride` where the port widened a variable the original kept in 8 bits;
  /// then only the low byte is comparable, and the port carrying a value the
  /// original could not hold is a divergence this cannot see.
  uint8_t width;
} BoloStateVar;

/// The table of comparable state, ordered by address; `*count` receives its
/// length.
///
/// Deliberately partial. It omits state whose representation differs on
/// purpose (dest_seg is a byte offset here and a segment there, pactor_list
/// holds host pointers) and everything derived from the timer, because the
/// harness delivers ticks on its own schedule -- see IDLE_THRESHOLD in
/// emu/runner.h. Those would report a difference that is designed in.
const BoloStateVar *bolo_state_table(unsigned *count);

#endif // BOLO_H
