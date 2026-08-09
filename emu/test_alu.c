// Flag semantics for every ALU operation the interpreter implements.
//
// Each case names the operation, its operands and the exact flags expected.
// The 8086's flag rules are the classic source of emulator bugs, and BOLO is
// unusually exposed to them: it passes the carry flag between routines as an
// argument, so an inc that clobbers CF changes gameplay rather than crashing.

#include "i8086.h"

#include <stdio.h>

/// The six arithmetic flags. TF/IF/DF are control flags and not under test.
#define TESTED_FLAGS (I8086_CF | I8086_PF | I8086_AF | I8086_ZF | I8086_SF | I8086_OF)

static int g_failures;

static void flag_string(uint16_t flags, char *buf) {
  buf[0] = (flags & I8086_OF) ? 'O' : '-';
  buf[1] = (flags & I8086_SF) ? 'S' : '-';
  buf[2] = (flags & I8086_ZF) ? 'Z' : '-';
  buf[3] = (flags & I8086_AF) ? 'A' : '-';
  buf[4] = (flags & I8086_PF) ? 'P' : '-';
  buf[5] = (flags & I8086_CF) ? 'C' : '-';
  buf[6] = '\0';
}

static void check(
    const char *what,
    uint16_t gotResult,
    uint16_t gotFlags,
    uint16_t wantResult,
    uint16_t wantFlags) {
  bool resultOk = gotResult == wantResult;
  bool flagsOk = (gotFlags & TESTED_FLAGS) == (wantFlags & TESTED_FLAGS);
  if (resultOk && flagsOk)
    return;
  char g[8], w[8];
  flag_string(gotFlags, g);
  flag_string(wantFlags, w);
  fprintf(
      stderr,
      "FAIL %-28s result %04X flags %s   expected result %04X flags %s\n",
      what,
      gotResult,
      g,
      wantResult,
      w);
  ++g_failures;
}

int main(void) {
  uint16_t f;
  uint16_t r;

  // Every case below binds the ALU call's result to `r` in its own full
  // expression before calling check(). This is not cosmetic: writing
  // check(name, i8086_alu(..., &f), f, ...) invokes undefined behavior --
  // the write to *flags inside the call is unsequenced relative to the
  // separate read of `f` as another argument to check(), and the two access
  // the same object. GCC on this box evaluates check()'s arguments
  // right-to-left, so `f` was observed to be read *before* the call that
  // updates it, silently checking the stale, pre-call flags on every case.
  // Binding to `r` first inserts a sequence point (the `=`'s full
  // expression ends) between the call and the later read of `f`.

  // ---- add ----
  // 8-bit wraparound: FF + 01 = 00, carry out, half carry, zero, even parity.
  f = 0;
  r = i8086_alu(I8086_ADD, false, 0xFF, 0x01, &f);
  check("add8 FF+01", r, f, 0x00, I8086_CF | I8086_AF | I8086_ZF | I8086_PF);
  // Signed overflow: 7F + 01 = 80.
  f = 0;
  r = i8086_alu(I8086_ADD, false, 0x7F, 0x01, &f);
  check("add8 7F+01", r, f, 0x80, I8086_OF | I8086_SF | I8086_AF);
  // Half carry alone: 0F + 01 = 10.
  f = 0;
  r = i8086_alu(I8086_ADD, false, 0x0F, 0x01, &f);
  check("add8 0F+01", r, f, 0x10, I8086_AF);
  // No half carry: 01 + 01 = 02.
  f = 0;
  r = i8086_alu(I8086_ADD, false, 0x01, 0x01, &f);
  check("add8 01+01", r, f, 0x02, 0);
  // 16-bit carry out.
  f = 0;
  r = i8086_alu(I8086_ADD, true, 0xFFFF, 0x0001, &f);
  check("add16 FFFF+0001", r, f, 0x0000, I8086_CF | I8086_AF | I8086_ZF | I8086_PF);

  // ---- adc ----
  // Carry in participates.
  f = I8086_CF;
  r = i8086_alu(I8086_ADC, false, 0xFE, 0x01, &f);
  check("adc8 FE+01+C", r, f, 0x00, I8086_CF | I8086_AF | I8086_ZF | I8086_PF);

  // ---- sub / cmp ----
  // Borrow: 00 - 01 = FF.
  f = 0;
  r = i8086_alu(I8086_SUB, false, 0x00, 0x01, &f);
  check("sub8 00-01", r, f, 0xFF, I8086_CF | I8086_AF | I8086_SF | I8086_PF);
  // Signed overflow: 80 - 01 = 7F.
  f = 0;
  r = i8086_alu(I8086_SUB, false, 0x80, 0x01, &f);
  check("sub8 80-01", r, f, 0x7F, I8086_OF | I8086_AF);
  // cmp of equal values sets ZF and returns its first operand untouched.
  f = 0;
  r = i8086_alu(I8086_CMP, true, 0x1000, 0x1000, &f);
  check("cmp16 1000,1000", r, f, 0x1000, I8086_ZF | I8086_PF);

  // ---- sbb ----
  f = I8086_CF;
  r = i8086_alu(I8086_SBB, false, 0x00, 0x00, &f);
  check("sbb8 00-00-C", r, f, 0xFF, I8086_CF | I8086_AF | I8086_SF | I8086_PF);

  // ---- logic: CF and OF always cleared ----
  f = I8086_CF | I8086_OF;
  r = i8086_alu(I8086_AND, false, 0xF0, 0x0F, &f);
  check("and8 F0&0F", r, f, 0x00, I8086_ZF | I8086_PF);
  f = I8086_CF;
  r = i8086_alu(I8086_OR, false, 0xF0, 0x0F, &f);
  check("or8 F0|0F", r, f, 0xFF, I8086_SF | I8086_PF);
  f = I8086_CF;
  r = i8086_alu(I8086_XOR, false, 0xFF, 0x0F, &f);
  check("xor8 FF^0F", r, f, 0xF0, I8086_SF | I8086_PF);

  // ---- inc / dec must preserve CF ----
  f = I8086_CF;
  r = i8086_inc(false, 0xFF, &f);
  check("inc8 FF (CF set)", r, f, 0x00, I8086_CF | I8086_AF | I8086_ZF | I8086_PF);
  f = 0;
  r = i8086_inc(false, 0x7F, &f);
  check("inc8 7F", r, f, 0x80, I8086_OF | I8086_SF | I8086_AF);
  f = I8086_CF;
  r = i8086_dec(false, 0x80, &f);
  check("dec8 80 (CF set)", r, f, 0x7F, I8086_CF | I8086_OF | I8086_AF);
  f = 0;
  r = i8086_dec(false, 0x01, &f);
  check("dec8 01", r, f, 0x00, I8086_ZF | I8086_PF);

  // ---- neg ----
  f = I8086_CF;
  r = i8086_neg(false, 0x00, &f);
  check("neg8 00", r, f, 0x00, I8086_ZF | I8086_PF);
  f = 0;
  r = i8086_neg(false, 0x01, &f);
  check("neg8 01", r, f, 0xFF, I8086_CF | I8086_AF | I8086_SF | I8086_PF);
  f = 0;
  r = i8086_neg(false, 0x80, &f);
  check("neg8 80", r, f, 0x80, I8086_CF | I8086_OF | I8086_SF);

  // ---- shifts ----
  f = 0;
  r = i8086_shift(4, false, 0x80, 1, &f);
  check("shl8 80,1", r, f, 0x00, I8086_CF | I8086_OF | I8086_ZF | I8086_PF);
  f = 0;
  r = i8086_shift(4, false, 0x01, 1, &f);
  check("shl8 01,1", r, f, 0x02, 0);
  f = 0;
  r = i8086_shift(5, false, 0x01, 1, &f);
  check("shr8 01,1", r, f, 0x00, I8086_CF | I8086_ZF | I8086_PF);
  f = 0;
  r = i8086_shift(5, false, 0x80, 1, &f);
  check("shr8 80,1", r, f, 0x40, I8086_OF);
  f = 0;
  r = i8086_shift(7, true, 0x8000, 1, &f);
  check("sar16 8000,1", r, f, 0xC000, I8086_SF | I8086_PF);
  // A count of 0 must leave every flag alone.
  f = I8086_CF | I8086_OF | I8086_ZF;
  r = i8086_shift(4, false, 0xFF, 0, &f);
  check("shl8 FF,0", r, f, 0xFF, I8086_CF | I8086_OF | I8086_ZF);

  // ---- rotates ----
  f = 0;
  r = i8086_shift(2, false, 0x80, 1, &f);
  check("rcl8 80,1 (CF=0)", r, f, 0x00, I8086_CF | I8086_OF);
  f = I8086_CF;
  r = i8086_shift(3, false, 0x01, 1, &f);
  check("rcr8 01,1 (CF=1)", r, f, 0x80, I8086_CF | I8086_OF);
  f = 0;
  r = i8086_shift(0, true, 0x8000, 1, &f);
  check("rol16 8000,1", r, f, 0x0001, I8086_CF | I8086_OF);

  // ---- mul ----
  f = 0;
  {
    uint32_t p = i8086_mul(false, 0x10, 0x10, &f);
    check("mul8 10*10", (uint16_t)p, f, 0x0100, I8086_CF | I8086_OF | I8086_PF);
  }
  f = 0;
  {
    uint32_t p = i8086_mul(false, 0x02, 0x03, &f);
    check("mul8 02*03", (uint16_t)p, f, 0x0006, I8086_PF);
  }

  if (g_failures != 0) {
    fprintf(stderr, "\n%d ALU check(s) failed\n", g_failures);
    return 1;
  }
  printf("alu ok\n");
  return 0;
}
