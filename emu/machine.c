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
  /* EGA state arrives in Task 2, ports and services in Task 3. */
};

/// Handles reads to the EGA window at A0000; for now, plain RAM. Task 2
/// replaces the body of this function and ega_write, and nothing else.
static uint8_t ega_read(Machine *m, uint32_t linear) {
  return m->mem[linear];
}

/// Handles writes to the EGA window at A0000; for now, plain RAM. Task 2
/// replaces the body of this function and ega_read, and nothing else.
static void ega_write(Machine *m, uint32_t linear, uint8_t value) {
  m->mem[linear] = value;
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

static void machine_fail(Machine *m, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(m->errorBuf, sizeof(m->errorBuf), fmt, ap);
  va_end(ap);
  m->hasError = true;
  m->cpu.error = m->errorBuf;
}

static uint8_t machine_in8(void *ctx, uint16_t port) {
  Machine *m = (Machine *)ctx;
  machine_fail(m, "unimplemented port read %04Xh", port);
  return 0xFF;
}

static void machine_out8(void *ctx, uint16_t port, uint8_t value) {
  Machine *m = (Machine *)ctx;
  machine_fail(m, "unimplemented port write %04Xh <- %02Xh", port, value);
}

static bool machine_intercept(void *ctx, uint8_t vec) {
  Machine *m = (Machine *)ctx;
  machine_fail(m, "unimplemented interrupt %02Xh", vec);
  return false;
}

Machine *machine_create(void) {
  // 1MB of guest memory is embedded in Machine, so Machine itself must be
  // heap-allocated (and zeroed) rather than a stack or static object.
  return (Machine *)calloc(1, sizeof(Machine));
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
