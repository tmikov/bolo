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

  machine_destroy(m);

  if (g_failures) {
    fprintf(stderr, "\n%d machine check(s) failed\n", g_failures);
    return 1;
  }
  printf("machine ok\n");
  return 0;
}
