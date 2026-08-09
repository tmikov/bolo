// Test for the BOLO.LST parser.
//
// The strong check is the last one: every instruction the parser claims to
// have found must match the bytes actually in BOLO.COM at that address. That
// makes a parsing mistake essentially impossible to miss, and it is what earns
// the LST the right to be used as the decoder's oracle.

#include "lst.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Measured during planning. If a future BOLO.DEF edit changes this, update the
/// number deliberately after checking why -- do not "fix" the test blindly.
#define EXPECTED_INSNS 3439
#define EXPECTED_FIRST_ADDR 0x0100
#define EXPECTED_LAST_ADDR 0x2F53
#define COM_LOAD_ADDR 0x0100

static int fail(const char *msg) {
  fprintf(stderr, "FAIL: %s\n", msg);
  return 1;
}

int main(void) {
  LstFile lst;
  if (!lst_load(BOLO_LST_PATH, &lst))
    return fail("lst_load failed");

  if (lst.count != EXPECTED_INSNS) {
    fprintf(stderr, "FAIL: parsed %zu instructions, expected %d\n", lst.count, EXPECTED_INSNS);
    return 1;
  }

  if (lst.insns[0].addr != EXPECTED_FIRST_ADDR)
    return fail("first instruction is not at 0100h");
  if (lst.insns[lst.count - 1].addr != EXPECTED_LAST_ADDR)
    return fail("last instruction is not at 2F53h");

  // Sorted and unique.
  for (size_t i = 1; i != lst.count; ++i) {
    if (lst.insns[i].addr <= lst.insns[i - 1].addr) {
      fprintf(
          stderr,
          "FAIL: addresses not strictly ascending at index %zu: %04X after %04X\n",
          i,
          lst.insns[i].addr,
          lst.insns[i - 1].addr);
      return 1;
    }
  }

  // Lengths in range.
  for (size_t i = 0; i != lst.count; ++i) {
    if (lst.insns[i].len < 1 || lst.insns[i].len > LST_MAX_INSN_BYTES) {
      fprintf(
          stderr,
          "FAIL: %04X (LST:%d) has length %u\n",
          lst.insns[i].addr,
          lst.insns[i].line,
          lst.insns[i].len);
      return 1;
    }
    if (lst.insns[i].mnemonic[0] == '\0') {
      fprintf(
          stderr,
          "FAIL: %04X (LST:%d) has an empty mnemonic\n",
          lst.insns[i].addr,
          lst.insns[i].line);
      return 1;
    }
  }

  // Spot-check specific lines, including the two prefix forms that a naive
  // parser drops.
  const LstInsn *ins = lst_find(&lst, 0x0100);
  if (!ins || ins->len != 2 || strcmp(ins->mnemonic, "mov") != 0)
    return fail("0100h should be a 2-byte mov");
  // "cmp ax,555Ah" is 3D 5A 55 -- the LST prints the word big-endian.
  ins = lst_find(&lst, 0x0102);
  if (!ins || ins->len != 3 || ins->bytes[0] != 0x3D || ins->bytes[1] != 0x5A ||
      ins->bytes[2] != 0x55)
    return fail("0102h word immediate is not little-endian in bytes[]");
  ins = lst_find(&lst, 0x0126);
  if (!ins || ins->len != 2 || strcmp(ins->mnemonic, "rep movsw") != 0)
    return fail("0126h should be a 2-byte \"rep movsw\"");
  ins = lst_find(&lst, 0x02C8);
  if (!ins || ins->len != 5 || strcmp(ins->mnemonic, "inc") != 0 || ins->bytes[0] != 0x2E)
    return fail("02C8h should be a 5-byte cs:-prefixed inc");
  // A labeled data line must not be mistaken for an instruction.
  if (lst_find(&lst, 0x2F54) != NULL)
    return fail("2F54h is the data label \"level\", not an instruction");

  // The real check: bytes must equal BOLO.COM.
  FILE *f = fopen(BOLO_COM_PATH, "rb");
  if (!f)
    return fail("cannot open BOLO.COM");
  static uint8_t com[0x10000];
  size_t comSize = fread(com, 1, sizeof(com), f);
  fclose(f);
  if (comSize != 11912) {
    fprintf(stderr, "FAIL: BOLO.COM is %zu bytes, expected 11912\n", comSize);
    return 1;
  }

  for (size_t i = 0; i != lst.count; ++i) {
    const LstInsn *in = &lst.insns[i];
    size_t off = (size_t)in->addr - COM_LOAD_ADDR;
    if (off + in->len > comSize) {
      fprintf(stderr, "FAIL: %04X (LST:%d) runs past the end of BOLO.COM\n", in->addr, in->line);
      return 1;
    }
    if (memcmp(in->bytes, com + off, in->len) != 0) {
      fprintf(stderr, "FAIL: %04X (LST:%d) bytes differ from BOLO.COM\n", in->addr, in->line);
      fprintf(stderr, "  LST: ");
      for (unsigned b = 0; b != in->len; ++b)
        fprintf(stderr, "%02X ", in->bytes[b]);
      fprintf(stderr, "\n  COM: ");
      for (unsigned b = 0; b != in->len; ++b)
        fprintf(stderr, "%02X ", com[off + b]);
      fprintf(stderr, "\n");
      return 1;
    }
  }

  printf("lst ok: %zu instructions, all bytes match BOLO.COM\n", lst.count);
  lst_free(&lst);
  return 0;
}
