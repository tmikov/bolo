#include "machine.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Machine {
  I8086 cpu;
  uint8_t mem[MACHINE_MEM_SIZE];
  char errorBuf[128];
  bool hasError;
  bool exited;

  /// Two pages of four planes. EGA_PAGE_SIZE bytes each, of which the first
  /// EGA_PAGE_VISIBLE are displayed.
  uint8_t plane[EGA_PLANES][EGA_PAGE_SIZE * 2];
  uint8_t latch[EGA_PLANES];
  uint8_t mapMask; ///< sequencer index 2, low 4 bits
  uint8_t readMapSelect; ///< graphics index 4
  uint8_t dataRotate; ///< graphics index 3: bits 4-3 function, 2-0 rotate
  uint8_t crtcStartHigh; ///< CRTC index 0Ch
  uint8_t seqIndex, gfxIndex, crtcIndex;
  /* Ports and BIOS/DOS services arrive in Task 3. */
};

static void machine_fail(Machine *m, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(m->errorBuf, sizeof(m->errorBuf), fmt, ap);
  va_end(ap);
  m->hasError = true;
  m->cpu.error = m->errorBuf;
}

/// Handles reads to the EGA window at A0000: returns the plane named by read
/// map select, and loads all four latches -- the original's read-modify-write
/// sequences depend on every read doing this, not just ones that "look like"
/// a draw.
static uint8_t ega_read(Machine *m, uint32_t linear) {
  uint32_t offset = (linear - MACHINE_EGA_BASE) & (MACHINE_EGA_WINDOW - 1);
  if (offset >= EGA_PAGE_SIZE * 2) {
    machine_fail(m, "EGA read beyond backed page storage: offset %04Xh", offset);
    return 0xFF;
  }
  for (int p = 0; p != EGA_PLANES; ++p)
    m->latch[p] = m->plane[p][offset];
  return m->plane[m->readMapSelect & 3][offset];
}

/// Handles writes to the EGA window at A0000: for each plane selected by the
/// map mask, combines the CPU byte with that plane's latch (loaded by the
/// most recent read) through the function selected by data rotate, and
/// stores the result. Planes whose map mask bit is clear keep their value.
static void ega_write(Machine *m, uint32_t linear, uint8_t value) {
  uint32_t offset = (linear - MACHINE_EGA_BASE) & (MACHINE_EGA_WINDOW - 1);
  if (offset >= EGA_PAGE_SIZE * 2) {
    machine_fail(m, "EGA write beyond backed page storage: offset %04Xh", offset);
    return;
  }

  unsigned rotate = m->dataRotate & 0x07;
  uint8_t rotated = (uint8_t)((value >> rotate) | (value << ((8 - rotate) & 7)));
  unsigned function = (m->dataRotate >> 3) & 0x03;

  for (int p = 0; p != EGA_PLANES; ++p) {
    if (!(m->mapMask & (1 << p)))
      continue;
    uint8_t result;
    switch (function) {
    case 0: // replace
      result = rotated;
      break;
    case 1: // AND
      result = (uint8_t)(rotated & m->latch[p]);
      break;
    case 2: // OR
      result = (uint8_t)(rotated | m->latch[p]);
      break;
    default: // XOR
      result = (uint8_t)(rotated ^ m->latch[p]);
      break;
    }
    m->plane[p][offset] = result;
  }
}

static uint8_t machine_read8(void *ctx, uint32_t linear) {
  Machine *m = (Machine *)ctx;
  linear &= 0xFFFFF;
  if (linear >= MACHINE_EGA_BASE && linear < MACHINE_EGA_BASE + MACHINE_EGA_WINDOW)
    return ega_read(m, linear);
  return m->mem[linear];
}

static void machine_write8(void *ctx, uint32_t linear, uint8_t value) {
  Machine *m = (Machine *)ctx;
  linear &= 0xFFFFF;
  if (linear >= MACHINE_EGA_BASE && linear < MACHINE_EGA_BASE + MACHINE_EGA_WINDOW) {
    ega_write(m, linear, value);
    return;
  }
  m->mem[linear] = value;
}

// EGA I/O ports. Sequencer (3C4h/3C5h) and graphics controller (3CEh/3CFh)
// registers are index+data pairs on adjacent even/odd ports; the original
// writes both halves with a single 16-bit "out dx,ax", which the plan-2
// executor decomposes into out8(index port) then out8(index port + 1). The
// odd port therefore always acts on the register most recently selected on
// the even port.
#define MACHINE_PORT_SEQ_INDEX 0x3C4
#define MACHINE_PORT_SEQ_DATA 0x3C5
#define MACHINE_PORT_GFX_INDEX 0x3CE
#define MACHINE_PORT_GFX_DATA 0x3CF
#define MACHINE_PORT_CRTC_INDEX 0x3D4
#define MACHINE_PORT_CRTC_DATA 0x3D5
#define MACHINE_PORT_INPUT_STATUS1 0x3DA

static uint8_t machine_in8(void *ctx, uint16_t port) {
  Machine *m = (Machine *)ctx;
  if (port == MACHINE_PORT_INPUT_STATUS1) {
    // Bit 3 is vertical retrace; the original spins on it in flip_vp. Always
    // reporting it set means the spin exits immediately.
    return 0x08;
  }
  machine_fail(m, "unimplemented port read %04Xh", port);
  return 0xFF;
}

static void machine_out8(void *ctx, uint16_t port, uint8_t value) {
  Machine *m = (Machine *)ctx;
  switch (port) {
  case MACHINE_PORT_SEQ_INDEX:
    m->seqIndex = value;
    return;
  case MACHINE_PORT_SEQ_DATA:
    if (m->seqIndex == 2) {
      m->mapMask = value & 0x0F;
      return;
    }
    machine_fail(m, "unimplemented sequencer register %02Xh <- %02Xh", m->seqIndex, value);
    return;
  case MACHINE_PORT_GFX_INDEX:
    m->gfxIndex = value;
    return;
  case MACHINE_PORT_GFX_DATA:
    switch (m->gfxIndex) {
    case 3:
      m->dataRotate = value & 0x1F;
      return;
    case 4:
      m->readMapSelect = value & 0x03;
      return;
    default:
      machine_fail(
          m, "unimplemented graphics controller register %02Xh <- %02Xh", m->gfxIndex, value);
      return;
    }
  case MACHINE_PORT_CRTC_INDEX:
    m->crtcIndex = value;
    return;
  case MACHINE_PORT_CRTC_DATA:
    if (m->crtcIndex == 0x0C) {
      m->crtcStartHigh = value;
      return;
    }
    machine_fail(m, "unimplemented CRTC register %02Xh <- %02Xh", m->crtcIndex, value);
    return;
  default:
    machine_fail(m, "unimplemented port write %04Xh <- %02Xh", port, value);
    return;
  }
}

static bool machine_intercept(void *ctx, uint8_t vec) {
  Machine *m = (Machine *)ctx;
  machine_fail(m, "unimplemented interrupt %02Xh", vec);
  return false;
}

Machine *machine_create(void) {
  // 1MB of guest memory is embedded in Machine, so Machine itself must be
  // heap-allocated (and zeroed) rather than a stack or static object.
  Machine *m = (Machine *)calloc(1, sizeof(Machine));
  if (!m)
    return NULL;

  // EGA power-on state: all planes writable, replace function, plane 0
  // readable, page 0 displayed. calloc already zeroed dataRotate,
  // readMapSelect and crtcStartHigh; only mapMask needs a nonzero reset.
  m->mapMask = 0x0F;

  return m;
}

void machine_destroy(Machine *m) {
  free(m);
}

bool machine_load_com(Machine *m, const char *path, uint16_t seg) {
  FILE *f = fopen(path, "rb");
  if (!f)
    return false;

  // A .COM image is loaded at seg:0100, so it must fit in the 64K segment
  // starting there, leaving room for at least the FFFE-byte stack pointer
  // DOS sets up (0x10000 - 0x100 = 0xFF00 bytes available).
  uint8_t buf[0x10000 - 0x100];
  size_t n = fread(buf, 1, sizeof(buf), f);
  bool tooLarge = fgetc(f) != EOF;
  fclose(f);
  if (tooLarge)
    return false;

  i8086_reset(&m->cpu);
  m->cpu.sreg[I8086_CS] = seg;
  m->cpu.sreg[I8086_DS] = seg;
  m->cpu.sreg[I8086_ES] = seg;
  m->cpu.sreg[I8086_SS] = seg;
  m->cpu.ip = 0x0100;
  m->cpu.reg[I8086_SP] = 0xFFFE;
  m->cpu.flags |= I8086_IF;

  m->cpu.ctx = m;
  m->cpu.read8 = machine_read8;
  m->cpu.write8 = machine_write8;
  m->cpu.in8 = machine_in8;
  m->cpu.out8 = machine_out8;
  m->cpu.intercept = machine_intercept;

  uint32_t base = i8086_linear(seg, 0x0100);
  memcpy(m->mem + base, buf, n);

  // BIOS data area: video option byte (nonzero => EGA/VGA present) and the
  // 18.2Hz tick counter, fixed at 0 for reproducibility.
  m->mem[0x0487] = 0x60;
  m->mem[0x046C] = 0;
  m->mem[0x046D] = 0;
  m->mem[0x046E] = 0;
  m->mem[0x046F] = 0;

  // A tiny INT 08h/09h stub, planted in the unused tail of the BIOS data
  // area (0000:0500), far from both the IVT and the loaded guest:
  //   mov al,20h / out 20h,al / iret
  static const uint8_t stub[] = {0xB0, 0x20, 0xE6, 0x20, 0xCF};
  memcpy(m->mem + 0x0500, stub, sizeof(stub));
  m->mem[8 * 4 + 0] = 0x00;
  m->mem[8 * 4 + 1] = 0x05;
  m->mem[8 * 4 + 2] = 0x00;
  m->mem[8 * 4 + 3] = 0x00;
  m->mem[9 * 4 + 0] = 0x00;
  m->mem[9 * 4 + 1] = 0x05;
  m->mem[9 * 4 + 2] = 0x00;
  m->mem[9 * 4 + 3] = 0x00;

  return true;
}

I8086 *machine_cpu(Machine *m) {
  return &m->cpu;
}

uint8_t machine_peek(const Machine *m, uint32_t linear) {
  return m->mem[linear & 0xFFFFF];
}

uint16_t machine_peek16(const Machine *m, uint32_t linear) {
  uint8_t lo = machine_peek(m, linear);
  uint8_t hi = machine_peek(m, linear + 1);
  return (uint16_t)(lo | (hi << 8));
}

const char *machine_error(const Machine *m) {
  return m->hasError ? m->errorBuf : NULL;
}

bool machine_exited(const Machine *m) {
  return m->exited;
}

const uint8_t *machine_plane(const Machine *m, int plane, int page) {
  return m->plane[plane] + (page ? EGA_PAGE_SIZE : 0);
}

int machine_draw_page(const Machine *m) {
  // dest_seg_e is at 2913:4F8A; flip_vp tests bit 1 of its high byte.
  uint8_t high = machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x4F8B));
  return (high & 0x02) ? 1 : 0;
}

int machine_display_page(const Machine *m) {
  return (m->crtcStartHigh & 0x20) ? 1 : 0;
}
