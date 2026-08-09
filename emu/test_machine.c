// Unit tests for the machine: the loader, the BIOS data area, and (in Task 2)
// the EGA write and read paths.
//
// These run without the guest wherever possible -- a test that needs BOLO.COM
// to fail is a test that cannot tell you which of ten things broke.

#include "machine.h"

#include <stdio.h>
#include <string.h>

static int g_failures;

static void expect_u8(const char *what, uint8_t got, uint8_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-40s got %02X, expected %02X\n", what, got, want);
  ++g_failures;
}

static void expect_u16(const char *what, uint16_t got, uint16_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-40s got %04X, expected %04X\n", what, got, want);
  ++g_failures;
}

int main(void) {
  Machine *m = machine_create();
  if (!m) {
    fprintf(stderr, "FAIL: machine_create returned NULL\n");
    return 1;
  }

  if (!machine_load_com(m, BOLO_COM_PATH, MACHINE_LOAD_SEG)) {
    fprintf(stderr, "FAIL: machine_load_com\n");
    return 1;
  }

  I8086 *cpu = machine_cpu(m);

  // --- DOS-like startup state ---
  expect_u16("CS", cpu->sreg[I8086_CS], MACHINE_LOAD_SEG);
  expect_u16("DS", cpu->sreg[I8086_DS], MACHINE_LOAD_SEG);
  expect_u16("ES", cpu->sreg[I8086_ES], MACHINE_LOAD_SEG);
  expect_u16("SS", cpu->sreg[I8086_SS], MACHINE_LOAD_SEG);
  expect_u16("IP", cpu->ip, 0x0100);
  // SP must be >= 555Ah or the program prints "Not enough memory" and exits.
  expect_u16("SP", cpu->reg[I8086_SP], 0xFFFE);
  // DOS hands a .COM its FLAGS with IF set, and BOLO never executes cli or sti.
  if (!(cpu->flags & I8086_IF)) {
    fprintf(stderr, "FAIL: IF must start set\n");
    ++g_failures;
  }

  // --- the image landed at seg:0100 ---
  // BOLO.COM begins "8B C4" -- mov ax,sp.
  expect_u8("image[0]", machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x0100)), 0x8B);
  expect_u8("image[1]", machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x0101)), 0xC4);

  // --- BIOS data the program checks before it will run ---
  // 0000:0487 must be nonzero or it prints "EGA/VGA required".
  expect_u8("video options 0:0487", machine_peek(m, 0x0487), 0x60);
  // 0000:046C is the 18.2Hz tick counter, fixed at 0 for reproducibility.
  expect_u16("timer low 0:046C", machine_peek16(m, 0x046C), 0x0000);

  // --- the INT 08h/09h vectors must point at something real, because
  //     int_08h_entry chains to the saved INT 08h with a far jmp ---
  uint16_t int8Off = machine_peek16(m, 8 * 4);
  uint16_t int8Seg = machine_peek16(m, 8 * 4 + 2);
  uint16_t int9Off = machine_peek16(m, 9 * 4);
  uint16_t int9Seg = machine_peek16(m, 9 * 4 + 2);
  if ((int8Seg == 0 && int8Off == 0) || (int9Seg == 0 && int9Off == 0)) {
    fprintf(stderr, "FAIL: INT 08h/09h vectors must not be null\n");
    ++g_failures;
  }
  // The stub they point at must return: an iret (CFh) has to be reachable
  // within a few bytes.
  {
    uint32_t at = i8086_linear(int8Seg, int8Off);
    bool sawIret = false;
    for (unsigned i = 0; i != 8; ++i)
      if (machine_peek(m, at + i) == 0xCF)
        sawIret = true;
    if (!sawIret) {
      fprintf(stderr, "FAIL: the INT 08h stub never reaches an iret\n");
      ++g_failures;
    }
  }

  // --- the startup checks actually pass: run to the EGA check and past it ---
  // 0100 mov ax,sp / 0102 cmp ax,555Ah / 0105 jae loc_1 -> 010D
  for (unsigned i = 0; i != 3; ++i) {
    if (!i8086_step(cpu)) {
      fprintf(stderr, "FAIL: step %u: %s\n", i, cpu->error ? cpu->error : "?");
      ++g_failures;
      break;
    }
  }
  expect_u16("took the jae (memory check passed)", cpu->ip, 0x010D);

  // ---------------------------------------------------------------- EGA ----
  //
  // Drive the registers exactly as the original does: an "out dx,ax" to an
  // index port writes the index in AL and the data in AH.
  {
    I8086 *c = machine_cpu(m);
    void *ctx = c->ctx;

// Helper shorthand: the 16-bit index+data write the original always uses.
#define OUT16(port, ax)                             \
  do {                                              \
    c->out8(ctx, (port), (uint8_t)((ax) & 0xFF));   \
    c->out8(ctx, (port) + 1, (uint8_t)((ax) >> 8)); \
  } while (0)

    // Map mask = all four planes, function = replace.
    OUT16(0x3C4, 0x0F02);
    OUT16(0x3CE, 0x0003);

    c->write8(ctx, MACHINE_EGA_BASE + 0, 0xA5);
    for (int p = 0; p != EGA_PLANES; ++p)
      expect_u8("write to all planes", machine_plane(m, p, 0)[0], 0xA5);

    // Map mask = plane 1 only: the others must keep their value.
    OUT16(0x3C4, 0x0202);
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0x3C);
    expect_u8("masked write, plane 0 untouched", machine_plane(m, 0, 0)[0], 0xA5);
    expect_u8("masked write, plane 1 written", machine_plane(m, 1, 0)[0], 0x3C);
    expect_u8("masked write, plane 2 untouched", machine_plane(m, 2, 0)[0], 0xA5);

    // Reads return only the plane named by read map select, and load all four
    // latches. Select plane 3 -- the value the original uses throughout.
    OUT16(0x3CE, 0x0304);
    expect_u8("read returns read-map-select plane", c->read8(ctx, MACHINE_EGA_BASE + 0), 0xA5);

    // The OR function combines the CPU byte with the latch loaded by that read.
    // Latches now hold A5/3C/A5/A5. Write 0x0F to all planes with function=OR.
    OUT16(0x3C4, 0x0F02);
    OUT16(0x3CE, 0x1003); // index 3, data 10h -> function OR
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0x0F);
    expect_u8("OR against latch, plane 0", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 | 0x0F));
    expect_u8("OR against latch, plane 1", machine_plane(m, 1, 0)[0], (uint8_t)(0x3C | 0x0F));

    // XOR, the other function the game uses for erasing. Writes never reload
    // the latches (only reads do, and the last one happened above, before
    // the OR write) so this XORs against the original A5 latch, not against
    // the AF the OR write just stored.
    OUT16(0x3CE, 0x1803); // function 11b -> XOR
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0xFF);
    expect_u8("XOR against latch, plane 0", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 ^ 0xFF));

    // Back to replace so later tests are not surprised.
    OUT16(0x3CE, 0x0003);

    // The two pages are distinct storage.
    OUT16(0x3C4, 0x0F02);
    c->write8(ctx, MACHINE_EGA_BASE + EGA_PAGE_SIZE, 0x77);
    expect_u8("page 1 written", machine_plane(m, 0, 1)[0], 0x77);
    expect_u8("page 0 unaffected", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 ^ 0xFF));

    // CRTC start address high selects the displayed page: 0 or 20h.
    OUT16(0x3D4, 0x000C);
    expect_u8("display page 0", (uint8_t)machine_display_page(m), 0);
    OUT16(0x3D4, 0x200C);
    expect_u8("display page 1", (uint8_t)machine_display_page(m), 1);

    // 3DAh bit 3 reads as always set, so flip_vp's retrace spin exits at once.
    if (!(c->in8(ctx, 0x3DA) & 0x08)) {
      fprintf(stderr, "FAIL: 3DAh bit 3 must read set\n");
      ++g_failures;
    }
#undef OUT16
  }

  // -------------------------------------------------------------- ports ----
  {
    I8086 *c = machine_cpu(m);
    void *ctx = c->ctx;

    // Port 61h is the speaker gate: writes are recorded, reads return the last
    // value written. Sound is out of scope for the comparison, but the port
    // must not be an error.
    c->out8(ctx, 0x61, 0x03);
    expect_u8("speaker port round trip", c->in8(ctx, 0x61) & 0x03, 0x03);

    // Port 20h is the PIC end-of-interrupt: accepted and ignored.
    c->out8(ctx, 0x20, 0x20);
    if (machine_error(m)) {
      fprintf(stderr, "FAIL: EOI to port 20h raised \"%s\"\n", machine_error(m));
      ++g_failures;
    }

    // Port 60h returns the injected scan code.
    machine_press_key(m, 0x39); // space
    expect_u8("keyboard data port", c->in8(ctx, 0x60), 0x39);
  }

  // ------------------------------------------------------ BIOS services ----
  {
    I8086 *c = machine_cpu(m);

    // INT 21h AH=25h sets an interrupt vector from DS:DX. This is how BOLO
    // installs its own INT 08h and INT 09h handlers, so it must actually write
    // the guest's IVT -- the CPU later vectors through it.
    c->reg[I8086_AX] = 0x2508;
    c->sreg[I8086_DS] = MACHINE_LOAD_SEG;
    c->reg[I8086_DX] = 0x02C8; // int_08h_entry
    if (!c->intercept(c->ctx, 0x21)) {
      fprintf(stderr, "FAIL: INT 21h AH=25h was not serviced\n");
      ++g_failures;
    }
    expect_u16("IVT 8 offset", machine_peek16(m, 8 * 4), 0x02C8);
    expect_u16("IVT 8 segment", machine_peek16(m, 8 * 4 + 2), MACHINE_LOAD_SEG);

    // INT 10h AH=00h sets the video mode; BOLO asks for 0Dh.
    c->reg[I8086_AX] = 0x000D;
    if (!c->intercept(c->ctx, 0x10)) {
      fprintf(stderr, "FAIL: INT 10h AH=00h was not serviced\n");
      ++g_failures;
    }

    // INT 10h AH=05h selects the active display page.
    c->reg[I8086_AX] = 0x0501;
    if (!c->intercept(c->ctx, 0x10)) {
      fprintf(stderr, "FAIL: INT 10h AH=05h was not serviced\n");
      ++g_failures;
    }
    expect_u8("INT 10h AH=05h set page 1", (uint8_t)machine_display_page(m), 1);

    // INT 20h terminates.
    if (!c->intercept(c->ctx, 0x20)) {
      fprintf(stderr, "FAIL: INT 20h was not serviced\n");
      ++g_failures;
    }
    if (!machine_exited(m)) {
      fprintf(stderr, "FAIL: INT 20h did not mark the machine exited\n");
      ++g_failures;
    }
  }

  // An unimplemented service must be a hard error, never a silent success.
  {
    Machine *m2 = machine_create();
    machine_load_com(m2, BOLO_COM_PATH, MACHINE_LOAD_SEG);
    I8086 *c2 = machine_cpu(m2);
    c2->reg[I8086_AX] = 0x4C00; // INT 21h AH=4Ch, which BOLO never calls
    c2->intercept(c2->ctx, 0x21);
    if (!machine_error(m2)) {
      fprintf(stderr, "FAIL: an unimplemented DOS function must set an error\n");
      ++g_failures;
    }
    machine_destroy(m2);
  }

  machine_destroy(m);

  if (g_failures) {
    fprintf(stderr, "\n%d machine check(s) failed\n", g_failures);
    return 1;
  }
  printf("machine ok\n");
  return 0;
}
