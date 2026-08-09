// The decoder oracle: decode at every address BOLO.LST names, and require the
// length and mnemonic to match what Sourcer produced.
//
// This is exhaustive over exactly the input that matters -- the 3439
// instructions in the program the harness has to run -- and needs no execution
// logic at all, so a decode bug is caught before it can be misdiagnosed as an
// execution bug.

#include "i8086.h"
#include "lst.h"

#include <stdio.h>
#include <string.h>

#define COM_LOAD_ADDR 0x0100

/// Sourcer spells four conditional jumps two ways; the decoder picks one. Map
/// both spellings onto the same canonical name before comparing.
static const char *canonical(const char *m) {
  if (strcmp(m, "jc") == 0)
    return "jb";
  if (strcmp(m, "jnc") == 0)
    return "jae";
  if (strcmp(m, "je") == 0)
    return "jz";
  if (strcmp(m, "jne") == 0)
    return "jnz";
  return m;
}

int main(void) {
  LstFile lst;
  if (!lst_load(BOLO_LST_PATH, &lst)) {
    fprintf(stderr, "FAIL: lst_load\n");
    return 1;
  }

  FILE *f = fopen(BOLO_COM_PATH, "rb");
  if (!f) {
    fprintf(stderr, "FAIL: cannot open BOLO.COM\n");
    return 1;
  }
  static uint8_t com[0x10000];
  size_t comSize = fread(com, 1, sizeof(com), f);
  fclose(f);

  size_t lenErrors = 0, mnemErrors = 0, unknown = 0;

  for (size_t i = 0; i != lst.count; ++i) {
    const LstInsn *want = &lst.insns[i];
    size_t off = (size_t)want->addr - COM_LOAD_ADDR;
    size_t avail = comSize - off;
    if (avail > I8086_MAX_INSN_BYTES)
      avail = I8086_MAX_INSN_BYTES;

    I8086Insn got;
    int len = i8086_decode(com + off, avail, &got);

    if (len == 0) {
      ++unknown;
      if (unknown <= 20)
        fprintf(
            stderr,
            "UNKNOWN opcode %02Xh at 2913:%04X: \"%s\"  (BOLO.LST:%d)\n",
            got.opcode,
            want->addr,
            want->mnemonic,
            want->line);
      continue;
    }

    if (len != want->len) {
      ++lenErrors;
      if (lenErrors <= 20)
        fprintf(
            stderr,
            "LENGTH 2913:%04X \"%s\": decoded %d, LST says %u  (BOLO.LST:%d)\n",
            want->addr,
            want->mnemonic,
            len,
            want->len,
            want->line);
      continue;
    }

    if (strcmp(canonical(got.mnemonic), canonical(want->mnemonic)) != 0) {
      ++mnemErrors;
      if (mnemErrors <= 20)
        fprintf(
            stderr,
            "MNEMONIC 2913:%04X: decoded \"%s\", LST says \"%s\"  (BOLO.LST:%d)\n",
            want->addr,
            got.mnemonic,
            want->mnemonic,
            want->line);
    }
  }

  size_t total = unknown + lenErrors + mnemErrors;
  if (total != 0) {
    fprintf(
        stderr,
        "FAIL: %zu unknown, %zu wrong length, %zu wrong mnemonic out of %zu\n",
        unknown,
        lenErrors,
        mnemErrors,
        lst.count);
    return 1;
  }

  printf("decode ok: %zu instructions\n", lst.count);
  lst_free(&lst);
  return 0;
}
