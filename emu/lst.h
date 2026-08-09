// bolo - public domain Tzvetan Mikov 2021
//
// Parser for disasm/BOLO.LST, the Sourcer disassembly of BOLO.COM. It is the
// oracle the 8086 decoder is validated against: 3439 instructions with their
// addresses, encodings and mnemonics.
//
// This module knows nothing about decoding. It only reads what Sourcer wrote.

#ifndef BOLO_LST_H
#define BOLO_LST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// The longest 8086 instruction is 6 bytes.
#define LST_MAX_INSN_BYTES 6

typedef struct LstInsn {
  /// Offset within the COM segment, i.e. the NNNN of the LST's "2913:NNNN".
  uint16_t addr;
  uint8_t len;
  uint8_t bytes[LST_MAX_INSN_BYTES];
  /// Sourcer's mnemonic. Prefixed instructions are stored joined, as in
  /// "rep stosb".
  char mnemonic[24];
  /// 1-based line number in the LST, so errors can name a line to go read.
  int line;
} LstInsn;

typedef struct LstFile {
  /// Sorted ascending by addr.
  LstInsn *insns;
  size_t count;
} LstFile;

/// Parse the LST at `path`. Returns false and prints to stderr on I/O failure.
/// On success `out` owns memory that lst_free() releases.
bool lst_load(const char *path, LstFile *out);

void lst_free(LstFile *lst);

/// The instruction starting exactly at `addr`, or NULL if none does.
const LstInsn *lst_find(const LstFile *lst, uint16_t addr);

#endif // BOLO_LST_H
