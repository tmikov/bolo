# BOLO 8086 Interpreter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build an 8086 interpreter that can execute the original `disasm/BOLO.COM`, validated exhaustively against the Sourcer disassembly before any execution logic is trusted.

**Architecture:** Two standalone modules under `emu/`, neither depending on the game. `lst.{c,h}` parses `disasm/BOLO.LST` into an array of `{address, length, bytes, mnemonic}`. `i8086.{c,h}` is the CPU: a decoder producing an `I8086Insn`, an ALU with exact 8086 flag semantics, and an executor reaching memory and ports only through caller-supplied callbacks. The decoder is built and validated first — the LST gives 3439 known-correct instructions to check it against, and the check needs no execution at all.

**Tech Stack:** C11, CMake (Ninja generator), CTest, no new dependencies.

This is plan 2 of 3 from `docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md`:

1. ~~The headless seam~~ — done, see `docs/superpowers/plans/2026-08-08-bolo-headless-seam.md`.
2. **This plan** — LST parser, decoder, ALU, execution engine.
3. The machine and comparison driver — EGA emulation, ports, interrupt services, sync, plane diffing, the baseline ratchet.

## Global Constraints

- C11 (`CMAKE_C_STANDARD 11`, `CMAKE_C_STANDARD_REQUIRED ON`), already set in the root `CMakeLists.txt`.
- CMake minimum 3.13. **Always configure with `-G Ninja`.**
- **No new external dependencies.**
- **Nothing in this plan touches `src/`.** No game logic changes, no shell changes. `emu/i8086.c` and `emu/lst.c` must not link against `bologame`.
- **Never modify `disasm/BOLO.COM`, `disasm/BOLO.LST`, `disasm/BOLO.DEF` or `disasm/BOLO.REM`.** They are the ground truth and are round-tripped through a DOS tool.
- New `emu/` targets are compiled with `-Wall -Wextra` and must be warning-free — add each new target to the `foreach` list in `emu/CMakeLists.txt`.
- Formatting follows the repo's `.clang-format`. Check with:
  `clang-format --dry-run -Werror emu/*.c emu/*.h` — judge by **exit status**, not by grepping output (`-Werror` prints `error:`, not `warning:`).
- Project is public domain; do not paste in licensed CPU-core code.

---

## Measured facts this plan is built on

All measured directly from `disasm/BOLO.LST` (6437 lines) and `disasm/BOLO.COM` (11912 bytes) during planning. **These numbers supersede the spec's estimates** (the spec said "3302 instructions, ~60 distinct opcodes" — that was an early estimate that missed prefixed instructions and counted opcode families rather than byte values):

- **3439 instruction lines**, spanning offsets `0100h`..`2F53h`, no duplicate addresses.
- **Every one of those lines' bytes matches `BOLO.COM` exactly** under the parsing rule in Task 1. This is the property that makes the LST trustworthy as an oracle.
- Instruction length histogram: 1 byte × 516, 2 × 1682, 3 × 768, 4 × 411, 5 × 59, 6 × 3.
- **169 distinct opcode keys** (opcode byte, plus the ModRM `reg` field for group opcodes). Full table in Task 2.
- Prefixes used: `rep` (F3) × 21, `es:` (26) × 31, `cs:` (2E) × 4. **No `repne`, no `lock`, no `ss:`/`ds:` override.**
- ModRM `mod` field distribution: mod=0 × 237, mod=1 × 47, mod=2 × 297, mod=3 × 849.

---

## File Structure

**Created:**

| File | Responsibility |
| --- | --- |
| `emu/lst.h`, `emu/lst.c` | Parse `BOLO.LST` into `{addr, len, bytes, mnemonic, line}` records. Knows nothing about decoding. |
| `emu/test_lst.c` | Parser test: 3439 records, sorted, unique, bytes equal to `BOLO.COM`. |
| `emu/i8086.h` | CPU types: `I8086`, `I8086Insn`, flag masks, the bus callback signatures, the API. |
| `emu/i8086.c` | Decoder, ALU, executor. |
| `emu/test_decode.c` | Decoder oracle: decode at all 3439 LST addresses, compare length and mnemonic. |
| `emu/test_alu.c` | Table-driven flag semantics for every ALU operation. |
| `emu/test_exec.c` | Execution tests over a flat-RAM toy machine: control flow, string ops, prefixes, interrupts. |

**Modified:**

| File | Change |
| --- | --- |
| `emu/CMakeLists.txt` | Add the `bolo_lst` and `bolo_i8086` libraries, four test targets, and the compiled-in paths to `BOLO.LST`/`BOLO.COM`. |

`emu/machine.{c,h}`, `emu/baseline.txt` and the comparison driver are **plan 3**, not this plan.

---

## Interface summary

Written out once here so every task's implementer sees the same names and types. Each task repeats the parts it needs.

```c
/* emu/lst.h */
typedef struct LstInsn {
  uint16_t addr;        /* offset in the COM segment; the LST's 2913:NNNN NNNN part */
  uint8_t len;          /* 1..6 */
  uint8_t bytes[6];
  char mnemonic[24];    /* "mov", "jae", "rep stosb", ... */
  int line;             /* 1-based line number in BOLO.LST, for error messages */
} LstInsn;

typedef struct LstFile {
  LstInsn *insns;       /* sorted ascending by addr */
  size_t count;
} LstFile;

bool lst_load(const char *path, LstFile *out);
void lst_free(LstFile *lst);
const LstInsn *lst_find(const LstFile *lst, uint16_t addr);

/* emu/i8086.h */
int i8086_decode(const uint8_t *code, size_t avail, I8086Insn *out);
void i8086_reset(I8086 *cpu);
bool i8086_step(I8086 *cpu);
void i8086_interrupt(I8086 *cpu, uint8_t vec);
```

---

### Task 1: The LST parser

Turn `disasm/BOLO.LST` into an array of instruction records. This task builds the oracle; nothing decodes yet. Its test cross-checks the parse against `BOLO.COM`, which is what makes every later task's use of the LST trustworthy.

**Files:**
- Create: `emu/lst.h`, `emu/lst.c`, `emu/test_lst.c`
- Modify: `emu/CMakeLists.txt`

**Interfaces:**
- Consumes: nothing.
- Produces: `LstInsn`, `LstFile`, `lst_load`, `lst_free`, `lst_find` (signatures above); CMake target `bolo_lst` with compile definitions `BOLO_LST_PATH` and `BOLO_COM_PATH`.

#### The line format

Sourcer emits instruction lines in fixed columns. Example lines, verbatim:

```
2913:0100  8B C4                                mov     ax,sp
2913:0102  3D 555A                              cmp     ax,555Ah
2913:0107 .BA 02FA                              mov     dx,offset msg_nomem     ; (2913:02FA='Not enough memor')
2913:0126  F3/ A5                               rep     movsw
2913:01A1  C6 06 4F9A 28                        mov     byte ptr ds:var_186e,28h ; (2913:4F9A=0) '('
2913:02C8  2E: FE 06 2F67                       inc     cs:time_tick            ; (2913:2F67=0)
2913:2F54  00                   level           db      0                       ;  xref 29
```

The rules, exactly:

1. The line is at least 12 characters, columns 0-8 match `[0-9A-F]{4}:[0-9A-F]{4}`, and column 9 is a space. The offset is columns 5-8.
2. Column 10 is Sourcer's reference marker — one of space, `.` or `,`. **Ignore it.** The byte field starts at column 11.
3. The byte field runs from column 11 up to the **first run of two or more spaces**. Getting this wrong is the trap: `F3/ A5` and `2E: FE 06 2F67` contain single spaces inside the field, so terminating at the first single space silently drops all 56 prefixed instructions.
4. The byte field must consist only of hex groups separated by ` `, `:` or `/` — i.e. it matches `(?:[0-9A-F]{2,4}[ :/]*)+`. If it does not, skip the line.
5. Each hex group is either 2 characters (one byte) or 4 characters (**one little-endian word — low byte first**). `3D 555A` is the three bytes `3D 5A 55`. `C6 06 4F9A 28` is the five bytes `C6 06 9A 4F 28`.
6. The first whitespace-separated token after the byte field is the mnemonic candidate. Skip the line unless it appears in the mnemonic whitelist below.
7. Skip the line if the **second** token after the byte field is `db` or `dw` — that is a labeled data line such as `level db 0`, whose first token is the label.
8. If the mnemonic is `rep`, `repe`, `repne`, `repz`, `repnz` or `lock`, the real instruction is the next token: store `"rep stosb"`, `"rep movsw"` and so on.

The mnemonic whitelist (a plain sorted `static const char *const` array, matched with `bsearch` or a linear scan):

```
aaa aad aam aas adc add and call cbw clc cld cli cmc cmp cmps cmpsb cmpsw cwd daa das
dec div esc hlt idiv imul in inc int into iret ja jae jb jbe jc jcxz je jg jge jl jle
jmp jna jnae jnb jnbe jnc jne jng jnge jnl jnle jno jnp jns jnz jo jp jpe jpo js jz
lahf lds lea les lock lodsb lodsw loop loope loopne loopnz loopz mov movs movsb movsw
mul neg nop not or out pop popf push pushf rcl rcr rep repe repne repnz repz ret retf
retn rol ror sahf sal sar sbb scas scasb scasw shl shr stc std sti stosb stosw sub
test wait xchg xlat xor
```

- [ ] **Step 1: Write `emu/lst.h`**

```c
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
```

- [ ] **Step 2: Write the failing test `emu/test_lst.c`**

```c
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
      fprintf(stderr, "FAIL: %04X (LST:%d) has an empty mnemonic\n",
              lst.insns[i].addr, lst.insns[i].line);
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
```

- [ ] **Step 3: Add the targets and run the test to see it fail**

Add to `emu/CMakeLists.txt`, immediately after the existing `add_test(NAME ega_render ...)` line:

```cmake
# The 8086 interpreter and its LST oracle. Standalone: no bologame dependency.
add_library(bolo_lst STATIC lst.c)
target_include_directories(bolo_lst PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(bolo_lst PUBLIC
    BOLO_LST_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.LST"
    BOLO_COM_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.COM")

add_executable(test_lst test_lst.c)
target_link_libraries(test_lst bolo_lst)
add_test(NAME lst COMMAND test_lst)
```

Then extend the warnings `foreach` list in the same file from

```cmake
  foreach(target test_link bolo_ppm test_ppm bolotest test_ega_render)
```

to

```cmake
  foreach(target test_link bolo_ppm test_ppm bolotest test_ega_render
                 bolo_lst test_lst)
```

Run:

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -5
```

Expected: FAIL at the configure step — `Cannot find source file: lst.c`.

- [ ] **Step 4: Write `emu/lst.c`**

```c
#include "lst.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Sourcer mnemonics, sorted for bsearch. Anything not here is not treated as
/// an instruction, which is how data lines and labels are filtered out.
static const char *const kMnemonics[] = {
    "aaa",   "aad",   "aam",   "aas",   "adc",   "add",   "and",   "call",  "cbw",   "clc",
    "cld",   "cli",   "cmc",   "cmp",   "cmps",  "cmpsb", "cmpsw", "cwd",   "daa",   "das",
    "dec",   "div",   "esc",   "hlt",   "idiv",  "imul",  "in",    "inc",   "int",   "into",
    "iret",  "ja",    "jae",   "jb",    "jbe",   "jc",    "jcxz",  "je",    "jg",    "jge",
    "jl",    "jle",   "jmp",   "jna",   "jnae",  "jnb",   "jnbe",  "jnc",   "jne",   "jng",
    "jnge",  "jnl",   "jnle",  "jno",   "jnp",   "jns",   "jnz",   "jo",    "jp",    "jpe",
    "jpo",   "js",    "jz",    "lahf",  "lds",   "lea",   "les",   "lock",  "lodsb", "lodsw",
    "loop",  "loope", "loopne","loopnz","loopz", "mov",   "movs",  "movsb", "movsw", "mul",
    "neg",   "nop",   "not",   "or",    "out",   "pop",   "popf",  "push",  "pushf", "rcl",
    "rcr",   "rep",   "repe",  "repne", "repnz", "repz",  "ret",   "retf",  "retn",  "rol",
    "ror",   "sahf",  "sal",   "sar",   "sbb",   "scas",  "scasb", "scasw", "shl",   "shr",
    "stc",   "std",   "sti",   "stosb", "stosw", "sub",   "test",  "wait",  "xchg",  "xlat",
    "xor",
};

static int cmp_mnemonic(const void *key, const void *elem) {
  return strcmp((const char *)key, *(const char *const *)elem);
}

static bool is_mnemonic(const char *word) {
  return bsearch(
             word,
             kMnemonics,
             sizeof(kMnemonics) / sizeof(kMnemonics[0]),
             sizeof(kMnemonics[0]),
             cmp_mnemonic) != NULL;
}

/// True for the prefix mnemonics, whose operand is the real instruction.
static bool is_prefix_mnemonic(const char *word) {
  return strcmp(word, "rep") == 0 || strcmp(word, "repe") == 0 || strcmp(word, "repne") == 0 ||
      strcmp(word, "repz") == 0 || strcmp(word, "repnz") == 0 || strcmp(word, "lock") == 0;
}

static int hex_digit(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

static bool is_hex4(const char *s) {
  for (unsigned i = 0; i != 4; ++i)
    if (hex_digit(s[i]) < 0)
      return false;
  return true;
}

/// Copy the next whitespace-delimited word from *p into buf, advancing *p.
/// Returns false at end of string.
static bool next_word(const char **p, char *buf, size_t bufSize) {
  const char *s = *p;
  while (*s == ' ' || *s == '\t')
    ++s;
  if (*s == '\0' || *s == ';')
    return false;
  size_t n = 0;
  while (*s != '\0' && *s != ' ' && *s != '\t') {
    if (n + 1 < bufSize)
      buf[n++] = *s;
    ++s;
  }
  buf[n] = '\0';
  *p = s;
  return n != 0;
}

/// Parse one LST line into `out`. Returns false if the line is not an
/// instruction (a comment, a label, a data directive, a page header, ...).
static bool parse_line(const char *line, int lineNo, LstInsn *out) {
  size_t len = strlen(line);
  if (len < 12)
    return false;

  // Columns 0-8 must be SEG:OFF, column 9 a space.
  for (unsigned i = 0; i != 4; ++i)
    if (hex_digit(line[i]) < 0 || hex_digit(line[i + 5]) < 0)
      return false;
  if (line[4] != ':' || line[9] != ' ')
    return false;

  uint16_t addr = 0;
  for (unsigned i = 5; i != 9; ++i)
    addr = (uint16_t)(addr * 16 + hex_digit(line[i]));

  // Column 10 is Sourcer's reference marker (' ', '.' or ','); the byte field
  // starts at column 11 and runs to the first run of two or more spaces.
  const char *field = line + 11;
  const char *end = field;
  for (;;) {
    if (*end == '\0')
      break;
    if (end[0] == ' ' && end[1] == ' ')
      break;
    ++end;
  }

  // The field must be hex groups separated by ' ', ':' or '/'. Groups are two
  // hex digits (one byte) or four (one little-endian word).
  uint8_t bytes[LST_MAX_INSN_BYTES];
  unsigned nbytes = 0;
  const char *p = field;
  while (p < end) {
    if (*p == ' ' || *p == ':' || *p == '/') {
      ++p;
      continue;
    }
    if (hex_digit(p[0]) < 0 || p + 1 >= end || hex_digit(p[1]) < 0)
      return false;
    if (p + 4 <= end && is_hex4(p)) {
      unsigned v = 0;
      for (unsigned i = 0; i != 4; ++i)
        v = v * 16 + (unsigned)hex_digit(p[i]);
      if (nbytes + 2 > LST_MAX_INSN_BYTES)
        return false;
      bytes[nbytes++] = (uint8_t)(v & 0xFF);
      bytes[nbytes++] = (uint8_t)(v >> 8);
      p += 4;
    } else {
      unsigned v = (unsigned)(hex_digit(p[0]) * 16 + hex_digit(p[1]));
      if (nbytes + 1 > LST_MAX_INSN_BYTES)
        return false;
      bytes[nbytes++] = (uint8_t)v;
      p += 2;
    }
  }
  if (nbytes == 0)
    return false;

  // First token after the byte field is the mnemonic; a second token of "db"
  // or "dw" means the first was actually a data label.
  const char *rest = end;
  // 12 each, so the joined "rep stosb" form is at worst 11 + 1 + 11 + 1 = 24
  // bytes -- exactly mnemonic[24], which lets the compiler prove the snprintf
  // below cannot truncate. next_word() truncates anything longer, which is
  // harmless: the longest real mnemonic is "rep movsw" (9), and an
  // over-length data label still fails the is_mnemonic() exact match.
  char word[12], word2[12];
  if (!next_word(&rest, word, sizeof(word)))
    return false;
  if (!is_mnemonic(word))
    return false;

  bool haveSecond = next_word(&rest, word2, sizeof(word2));
  if (haveSecond && (strcmp(word2, "db") == 0 || strcmp(word2, "dw") == 0))
    return false;

  out->addr = addr;
  out->len = (uint8_t)nbytes;
  memcpy(out->bytes, bytes, nbytes);
  out->line = lineNo;

  if (is_prefix_mnemonic(word) && haveSecond)
    snprintf(out->mnemonic, sizeof(out->mnemonic), "%s %s", word, word2);
  else
    snprintf(out->mnemonic, sizeof(out->mnemonic), "%s", word);

  return true;
}

static int cmp_insn(const void *a, const void *b) {
  uint16_t x = ((const LstInsn *)a)->addr, y = ((const LstInsn *)b)->addr;
  return x < y ? -1 : x > y ? 1 : 0;
}

bool lst_load(const char *path, LstFile *out) {
  out->insns = NULL;
  out->count = 0;

  FILE *f = fopen(path, "rb");
  if (!f) {
    fprintf(stderr, "lst_load: cannot open %s\n", path);
    return false;
  }

  size_t capacity = 4096;
  LstInsn *insns = malloc(capacity * sizeof(*insns));
  if (!insns) {
    fclose(f);
    fprintf(stderr, "lst_load: out of memory\n");
    return false;
  }
  size_t count = 0;

  char line[512];
  int lineNo = 0;
  while (fgets(line, sizeof(line), f)) {
    ++lineNo;
    // Strip CR/LF; the LST is a DOS-era file.
    size_t n = strlen(line);
    while (n != 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
      line[--n] = '\0';

    LstInsn ins;
    if (!parse_line(line, lineNo, &ins))
      continue;

    if (count == capacity) {
      capacity *= 2;
      LstInsn *grown = realloc(insns, capacity * sizeof(*insns));
      if (!grown) {
        free(insns);
        fclose(f);
        fprintf(stderr, "lst_load: out of memory\n");
        return false;
      }
      insns = grown;
    }
    insns[count++] = ins;
  }
  fclose(f);

  qsort(insns, count, sizeof(*insns), cmp_insn);
  out->insns = insns;
  out->count = count;
  return true;
}

void lst_free(LstFile *lst) {
  free(lst->insns);
  lst->insns = NULL;
  lst->count = 0;
}

const LstInsn *lst_find(const LstFile *lst, uint16_t addr) {
  size_t lo = 0, hi = lst->count;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    if (lst->insns[mid].addr == addr)
      return &lst->insns[mid];
    if (lst->insns[mid].addr < addr)
      lo = mid + 1;
    else
      hi = mid;
  }
  return NULL;
}
```

- [ ] **Step 5: Run the test to verify it passes**

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -3
cd build && ctest --output-on-failure -R lst; cd ..
```

Expected: `lst ok: 3439 instructions, all bytes match BOLO.COM` and `100% tests passed`.

**If the count is 3383 rather than 3439**, the byte field is being terminated at a single space instead of a run of two — see rule 3 above. That drops every `rep`- and segment-override-prefixed instruction.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/lst.c emu/lst.h emu/test_lst.c && echo "format clean"
git add emu/lst.c emu/lst.h emu/test_lst.c emu/CMakeLists.txt
git commit -m "Add a BOLO.LST parser validated against BOLO.COM"
```

---

### Task 2: The decoder

Decode 8086 instructions into an `I8086Insn`, and check it at all 3439 LST addresses. No execution yet — this task is purely "how long is this instruction and what is it".

**Files:**
- Create: `emu/i8086.h`, `emu/i8086.c`, `emu/test_decode.c`
- Modify: `emu/CMakeLists.txt`

**Interfaces:**
- Consumes: `LstFile`, `lst_load`, `lst_free` from Task 1.
- Produces: `I8086Insn`, the `I8086_*` flag masks, `i8086_decode(const uint8_t *code, size_t avail, I8086Insn *out)` returning the instruction length or 0 for an unknown encoding. CMake target `bolo_i8086`.

#### The complete opcode inventory

Measured from `BOLO.COM` at all 3439 LST addresses. `NN/r` means opcode byte `NN` with ModRM `reg` field `r`. The decoder must handle **exactly these**; anything else returns 0 so the caller can abort with a precise message.

```
 OPCODE    N  MNEMONIC        OPCODE    N  MNEMONIC        OPCODE    N  MNEMONIC
 00        4  add             73       80  jae/jnc         AB        8  stosw
 01        1  add             74      116  je/jz           AC       12  lodsb
 02       67  add             75      131  jne/jnz         AD        7  lodsw
 03       16  add             76        6  jbe             AE        1  scasb
 04       22  add             77       10  ja              B0       61  mov
 06        7  push es         78       44  js              B1       23  mov
 07       12  pop es          79       33  jns             B2        9  mov
 08       18  or              80/0     23  add             B3       14  mov
 0A        9  or              80/1      8  or              B4       35  mov
 0B        5  or              80/2      1  adc             B5        2  mov
 0C        5  or              80/4     28  and             B6        8  mov
 0E        4  push cs         80/5     23  sub             B7        9  mov
 14        1  adc             80/6      5  xor             B8       18  mov
 1E        3  push ds         80/7     88  cmp             B9       42  mov
 1F        3  pop ds          81/0      9  add             BA       17  mov
 22       14  and             81/5      3  sub             BB       19  mov
 23        1  and             81/7      1  cmp             BC        1  mov
 24       20  and             83/0     21  add             BD        4  mov
 28        2  sub             83/5      2  sub             BE       31  mov
 29        1  sub             83/7      1  cmp             BF       38  mov
 2A       60  sub             84       53  test            C3      118  retn
 2B        3  sub             85        4  test            C5        2  lds
 2C        7  sub             86       12  xchg            C6       29  mov
 30       12  xor             87        1  xchg            C7        2  mov
 32       61  xor             88       95  mov             CD       11  int
 33       16  xor             89        6  mov             CF        1  iret
 34        1  xor             8A      271  mov             D0/2      4  rcl
 38       34  cmp             8B       91  mov             D0/3      2  rcr
 3A       44  cmp             8C        1  mov (sreg)      D0/4     40  shl
 3B        2  cmp             8D        2  lea             D0/5     21  shr
 3C       63  cmp             8E       11  mov (sreg)      D1/0      1  rol
 3D        1  cmp             90       31  nop             D1/2      1  rcl
 40-47   64  inc reg16        92        1  xchg ax,dx      D1/3      1  rcr
 48-4F   40  dec reg16        95        1  xchg ax,bp      D1/4     38  shl
 50-57   56  push reg16       97        1  xchg ax,di      D1/5      7  shr
 58-5F   58  pop reg16        98       42  cbw             D1/7      4  sar
 72       79  jb/jc           9E        1  sahf            D2/4      2  shl
                              9F        1  lahf            D2/5      1  shr
 A0       27  mov al,[imm16]  A1        1  mov ax,[imm16]  D3/5      1  shr
 A2       32  mov [imm16],al  A3        1  mov [imm16],ax  D7        4  xlat
 A4        5  movsb           A5        1  movsw           E2       36  loop
 A8       10  test al,imm8    AA       32  stosb           E3        4  jcxz
                                                           E4        5  in al,imm8
 F6/0     36  test rm8,imm8   F8       10  clc             E6        6  out imm8,al
 F6/3      5  neg rm8         F9        4  stc             E8      254  call rel16
 F6/4      2  mul rm8         FC        2  cld             E9       34  jmp rel16
 F7/0      1  test rm16,imm16 FD        1  std             EB      101  jmp rel8
                              FE/0     56  inc rm8         EC        1  in al,dx
 Prefixes: 26 (es:) x31, 2E (cs:) x4, F3 (rep) x21
                              FE/1     37  dec rm8         EF        5  out dx,ax
                              FF/2      4  call rm16
                              FF/4      2  jmp rm16
                              FF/5      1  jmp far [rm]
```

Notes the implementer needs:

- **Implement the full 8-register range for the family opcodes** (`00`-`3D` ALU forms, `40`-`4F`, `50`-`5F`, `B0`-`BF`, `70`-`7F`, `8x` groups) even where the table shows only some values used. It is the same amount of code and removes a class of "works until it doesn't" failure.
- `FF/5` is `jmp far [mem]` — a 4-byte indirect far jump. Its only use is `2913:02CD  2E: FF 2E 4F8D  jmp dword ptr cs:orig_int8_e`, the timer ISR chaining to the original handler.
- `C5` is `lds`; both uses load a far pointer to a saved interrupt vector.
- Sourcer spells four conditional jumps two ways: `72` is `jb` or `jc`, `73` is `jae` or `jnc`, `74` is `je` or `jz`, `75` is `jne` or `jnz`. The **test** canonicalizes both sides; the decoder always emits `jb`, `jae`, `jz`, `jnz`.
- For prefixed instructions the decoder's mnemonic must be the joined form: `"rep stosb"`, `"rep movsw"`, `"rep movsb"`, `"rep stosw"`. Segment overrides do **not** change the mnemonic.

#### ModRM

`mod` in bits 7-6, `reg` in 5-3, `rm` in 2-0. Effective address by `rm` when `mod != 3`:

| rm | base+index | default segment |
| --- | --- | --- |
| 0 | BX+SI | DS |
| 1 | BX+DI | DS |
| 2 | BP+SI | **SS** |
| 3 | BP+DI | **SS** |
| 4 | SI | DS |
| 5 | DI | DS |
| 6 | BP (but a direct 16-bit address when mod=0) | **SS** (DS for the mod=0 direct form) |
| 7 | BX | DS |

Displacement: none for `mod=0` (except `rm=6`, which takes a 16-bit direct address), a **sign-extended** 8-bit for `mod=1`, a 16-bit for `mod=2`. `mod=3` means the `rm` field names a register instead.

- [ ] **Step 1: Write `emu/i8086.h`**

```c
// bolo - public domain Tzvetan Mikov 2021
//
// A targeted 8086 interpreter, written to execute the original BOLO.COM
// against which src/bolo.c is being validated. It implements only what that
// program uses -- see docs/superpowers/plans/2026-08-08-bolo-8086-interpreter.md
// for the measured opcode inventory -- and aborts loudly on anything else.
//
// The CPU knows nothing about EGA, DOS or the PC. All memory and port access
// goes through the callbacks in I8086.

#ifndef BOLO_I8086_H
#define BOLO_I8086_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// FLAGS bits. Bit 1 always reads as 1 on an 8086; bits 3 and 5 always as 0.
enum {
  I8086_CF = 0x0001,
  I8086_PF = 0x0004,
  I8086_AF = 0x0010,
  I8086_ZF = 0x0040,
  I8086_SF = 0x0080,
  I8086_TF = 0x0100,
  I8086_IF = 0x0200,
  I8086_DF = 0x0400,
  I8086_OF = 0x0800,
};

/// Bits that are hardwired to 1 in an 8086's FLAGS register.
#define I8086_FLAGS_ALWAYS_SET 0xF002

/// Register indices, matching the 8086's own encoding.
enum { I8086_AX, I8086_CX, I8086_DX, I8086_BX, I8086_SP, I8086_BP, I8086_SI, I8086_DI };
enum { I8086_ES, I8086_CS, I8086_SS, I8086_DS };

/// The longest 8086 instruction is 6 bytes.
#define I8086_MAX_INSN_BYTES 6

typedef struct I8086Insn {
  uint8_t len;
  /// The opcode byte, with any prefixes already consumed.
  uint8_t opcode;
  uint8_t modrm;
  bool hasModrm;
  /// 0 if none, otherwise the prefix byte 0x26, 0x2E, 0x36 or 0x3E.
  uint8_t segOverride;
  /// 0 if none, otherwise 0xF2 or 0xF3.
  uint8_t repPrefix;
  /// Displacement, already sign-extended for the mod=1 form.
  uint16_t disp;
  /// Immediate, relative branch target offset, port number or interrupt
  /// vector, depending on the opcode. Already sign-extended for the rel8 and
  /// imm8-to-imm16 forms.
  uint16_t imm;
  /// Sourcer's spelling, e.g. "mov", "jae", "rep stosb". Static storage.
  const char *mnemonic;
} I8086Insn;

typedef struct I8086 I8086;

struct I8086 {
  uint16_t reg[8];  ///< Indexed by I8086_AX .. I8086_DI.
  uint16_t sreg[4]; ///< Indexed by I8086_ES .. I8086_DS.
  uint16_t ip;
  uint16_t flags;
  /// Set by i8086_step() when it cannot decode or execute; the caller aborts.
  const char *error;
  /// Address of the instruction that produced `error`, as a linear address.
  uint32_t errorAddr;

  void *ctx;
  uint8_t (*read8)(void *ctx, uint32_t linear);
  void (*write8)(void *ctx, uint32_t linear, uint8_t value);
  uint8_t (*in8)(void *ctx, uint16_t port);
  void (*out8)(void *ctx, uint16_t port, uint8_t value);
};

/// Decode one instruction from `code` (at most `avail` bytes readable).
/// Returns its length in bytes, or 0 if the encoding is not implemented, in
/// which case `out->opcode` still holds the offending opcode byte.
int i8086_decode(const uint8_t *code, size_t avail, I8086Insn *out);

/// Zero the registers, set FLAGS to its power-on value and clear `error`.
/// Does not touch memory; the caller loads the program and sets CS:IP and SS:SP.
void i8086_reset(I8086 *cpu);

/// Execute one instruction. Returns false and sets `cpu->error` on failure.
bool i8086_step(I8086 *cpu);

/// Push FLAGS, CS and IP, clear IF and TF, and vector through the guest's
/// interrupt table at 0000:0000. Required rather than optional: BOLO installs
/// its own INT 08h and INT 09h handlers through DOS AH=25h, so the harness must
/// reach them the same way the hardware did.
void i8086_interrupt(I8086 *cpu, uint8_t vec);

/// Convenience: seg:off to a 20-bit linear address.
static inline uint32_t i8086_linear(uint16_t seg, uint16_t off) {
  return (((uint32_t)seg << 4) + off) & 0xFFFFF;
}

#endif // BOLO_I8086_H
```

- [ ] **Step 2: Write the failing test `emu/test_decode.c`**

```c
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
#include <stdlib.h>
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
```

- [ ] **Step 3: Add the targets and run to see it fail**

Add to `emu/CMakeLists.txt`, after the `test_lst` block:

```cmake
add_library(bolo_i8086 STATIC i8086.c)
target_include_directories(bolo_i8086 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(test_decode test_decode.c)
target_link_libraries(test_decode bolo_i8086 bolo_lst)
add_test(NAME decode COMMAND test_decode)
```

and extend the warnings `foreach` list to include `bolo_i8086 test_decode`.

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -5
```

Expected: FAIL at configure — `Cannot find source file: i8086.c`.

- [ ] **Step 4: Write the decoder half of `emu/i8086.c`**

Create the file with the decoder only. Execution lands in Task 4; this file grows.

```c
#include "i8086.h"

#include <string.h>

/// Table entry describing how to decode one opcode byte.
typedef enum OpForm {
  OF_NONE,      ///< No operands beyond the opcode.
  OF_MODRM,     ///< ModRM byte with any displacement.
  OF_MODRM_I8,  ///< ModRM plus an 8-bit immediate.
  OF_MODRM_I16, ///< ModRM plus a 16-bit immediate.
  OF_MODRM_IS8, ///< ModRM plus a sign-extended 8-bit immediate (the 83h group).
  OF_I8,        ///< 8-bit immediate.
  OF_I16,       ///< 16-bit immediate.
  OF_REL8,      ///< Sign-extended 8-bit relative branch.
  OF_REL16,     ///< 16-bit relative branch.
  OF_MOFFS,     ///< 16-bit direct memory offset (the A0-A3 accumulator moves).
  OF_BAD,       ///< Not implemented.
} OpForm;

typedef struct OpInfo {
  OpForm form;
  /// NULL when the mnemonic depends on the ModRM reg field; see kGroup*.
  const char *mnemonic;
} OpInfo;

/// Mnemonics selected by the ModRM reg field for the group opcodes.
static const char *const kGroup80[8] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"};
static const char *const kGroupD0[8] = {"rol", "ror", "rcl", "rcr", "shl", "shr", NULL, "sar"};
static const char *const kGroupF6[8] = {"test", NULL, "not", "neg", "mul", "imul", "div", "idiv"};
static const char *const kGroupFE[8] = {"inc", "dec", NULL, NULL, NULL, NULL, NULL, NULL};
static const char *const kGroupFF[8] = {
    "inc", "dec", "call", "call", "jmp", "jmp", "push", NULL};

static OpInfo kOps[256];
static bool g_tableReady;

static void set_op(unsigned op, OpForm form, const char *mnemonic) {
  kOps[op].form = form;
  kOps[op].mnemonic = mnemonic;
}

static void build_table(void) {
  for (unsigned i = 0; i != 256; ++i)
    set_op(i, OF_BAD, NULL);

  // The eight ALU families at 00-3F. Each occupies 8 bytes:
  //   +0 rm8,r8   +1 rm16,r16   +2 r8,rm8   +3 r16,rm16
  //   +4 al,imm8  +5 ax,imm16   +6 push sreg  +7 pop sreg
  static const char *const kAlu[8] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"};
  for (unsigned i = 0; i != 8; ++i) {
    unsigned base = i * 8;
    set_op(base + 0, OF_MODRM, kAlu[i]);
    set_op(base + 1, OF_MODRM, kAlu[i]);
    set_op(base + 2, OF_MODRM, kAlu[i]);
    set_op(base + 3, OF_MODRM, kAlu[i]);
    set_op(base + 4, OF_I8, kAlu[i]);
    set_op(base + 5, OF_I16, kAlu[i]);
  }
  // 26/2E/36/3E are segment override prefixes, not push/pop sreg, and 27/2F/
  // 37/3F are the BCD adjusts, none of which BOLO uses. Overwrite them.
  set_op(0x06, OF_NONE, "push");
  set_op(0x07, OF_NONE, "pop");
  set_op(0x0E, OF_NONE, "push");
  set_op(0x0F, OF_BAD, NULL);
  set_op(0x16, OF_NONE, "push");
  set_op(0x17, OF_NONE, "pop");
  set_op(0x1E, OF_NONE, "push");
  set_op(0x1F, OF_NONE, "pop");
  set_op(0x26, OF_BAD, NULL);
  set_op(0x27, OF_BAD, NULL);
  set_op(0x2E, OF_BAD, NULL);
  set_op(0x2F, OF_BAD, NULL);
  set_op(0x36, OF_BAD, NULL);
  set_op(0x37, OF_BAD, NULL);
  set_op(0x3E, OF_BAD, NULL);
  set_op(0x3F, OF_BAD, NULL);

  for (unsigned r = 0; r != 8; ++r) {
    set_op(0x40 + r, OF_NONE, "inc");
    set_op(0x48 + r, OF_NONE, "dec");
    set_op(0x50 + r, OF_NONE, "push");
    set_op(0x58 + r, OF_NONE, "pop");
  }

  // Conditional jumps 70-7F. The decoder emits one spelling per opcode; the
  // decode test maps Sourcer's alternatives onto these.
  static const char *const kJcc[16] = {
      "jo", "jno", "jb", "jae", "jz", "jnz", "jbe", "ja",
      "js", "jns", "jp", "jnp", "jl", "jge", "jle", "jg"};
  for (unsigned i = 0; i != 16; ++i)
    set_op(0x70 + i, OF_REL8, kJcc[i]);

  set_op(0x80, OF_MODRM_I8, NULL);
  set_op(0x81, OF_MODRM_I16, NULL);
  set_op(0x82, OF_MODRM_I8, NULL);
  set_op(0x83, OF_MODRM_IS8, NULL);
  set_op(0x84, OF_MODRM, "test");
  set_op(0x85, OF_MODRM, "test");
  set_op(0x86, OF_MODRM, "xchg");
  set_op(0x87, OF_MODRM, "xchg");
  set_op(0x88, OF_MODRM, "mov");
  set_op(0x89, OF_MODRM, "mov");
  set_op(0x8A, OF_MODRM, "mov");
  set_op(0x8B, OF_MODRM, "mov");
  set_op(0x8C, OF_MODRM, "mov");
  set_op(0x8D, OF_MODRM, "lea");
  set_op(0x8E, OF_MODRM, "mov");
  set_op(0x8F, OF_MODRM, "pop");

  set_op(0x90, OF_NONE, "nop");
  for (unsigned r = 1; r != 8; ++r)
    set_op(0x90 + r, OF_NONE, "xchg");

  set_op(0x98, OF_NONE, "cbw");
  set_op(0x99, OF_NONE, "cwd");
  set_op(0x9C, OF_NONE, "pushf");
  set_op(0x9D, OF_NONE, "popf");
  set_op(0x9E, OF_NONE, "sahf");
  set_op(0x9F, OF_NONE, "lahf");

  set_op(0xA0, OF_MOFFS, "mov");
  set_op(0xA1, OF_MOFFS, "mov");
  set_op(0xA2, OF_MOFFS, "mov");
  set_op(0xA3, OF_MOFFS, "mov");
  set_op(0xA4, OF_NONE, "movsb");
  set_op(0xA5, OF_NONE, "movsw");
  set_op(0xA6, OF_NONE, "cmpsb");
  set_op(0xA7, OF_NONE, "cmpsw");
  set_op(0xA8, OF_I8, "test");
  set_op(0xA9, OF_I16, "test");
  set_op(0xAA, OF_NONE, "stosb");
  set_op(0xAB, OF_NONE, "stosw");
  set_op(0xAC, OF_NONE, "lodsb");
  set_op(0xAD, OF_NONE, "lodsw");
  set_op(0xAE, OF_NONE, "scasb");
  set_op(0xAF, OF_NONE, "scasw");

  for (unsigned r = 0; r != 8; ++r) {
    set_op(0xB0 + r, OF_I8, "mov");
    set_op(0xB8 + r, OF_I16, "mov");
  }

  set_op(0xC2, OF_I16, "retn");
  set_op(0xC3, OF_NONE, "retn");
  set_op(0xC4, OF_MODRM, "les");
  set_op(0xC5, OF_MODRM, "lds");
  set_op(0xC6, OF_MODRM_I8, "mov");
  set_op(0xC7, OF_MODRM_I16, "mov");
  set_op(0xCA, OF_I16, "retf");
  set_op(0xCB, OF_NONE, "retf");
  set_op(0xCD, OF_I8, "int");
  set_op(0xCF, OF_NONE, "iret");

  set_op(0xD0, OF_MODRM, NULL);
  set_op(0xD1, OF_MODRM, NULL);
  set_op(0xD2, OF_MODRM, NULL);
  set_op(0xD3, OF_MODRM, NULL);
  set_op(0xD7, OF_NONE, "xlat");

  set_op(0xE0, OF_REL8, "loopne");
  set_op(0xE1, OF_REL8, "loope");
  set_op(0xE2, OF_REL8, "loop");
  set_op(0xE3, OF_REL8, "jcxz");
  set_op(0xE4, OF_I8, "in");
  set_op(0xE5, OF_I8, "in");
  set_op(0xE6, OF_I8, "out");
  set_op(0xE7, OF_I8, "out");
  set_op(0xE8, OF_REL16, "call");
  set_op(0xE9, OF_REL16, "jmp");
  set_op(0xEB, OF_REL8, "jmp");
  set_op(0xEC, OF_NONE, "in");
  set_op(0xED, OF_NONE, "in");
  set_op(0xEE, OF_NONE, "out");
  set_op(0xEF, OF_NONE, "out");

  set_op(0xF5, OF_NONE, "cmc");
  set_op(0xF6, OF_MODRM, NULL);
  set_op(0xF7, OF_MODRM, NULL);
  set_op(0xF8, OF_NONE, "clc");
  set_op(0xF9, OF_NONE, "stc");
  set_op(0xFA, OF_NONE, "cli");
  set_op(0xFB, OF_NONE, "sti");
  set_op(0xFC, OF_NONE, "cld");
  set_op(0xFD, OF_NONE, "std");
  set_op(0xFE, OF_MODRM, NULL);
  set_op(0xFF, OF_MODRM, NULL);

  g_tableReady = true;
}

/// Bytes of displacement the ModRM byte implies.
static unsigned modrm_disp_size(uint8_t modrm) {
  unsigned mod = (unsigned)(modrm >> 6), rm = (unsigned)(modrm & 7);
  if (mod == 0)
    return rm == 6 ? 2 : 0;
  if (mod == 1)
    return 1;
  if (mod == 2)
    return 2;
  return 0;
}

/// The joined mnemonic for a rep-prefixed string instruction. Static strings so
/// I8086Insn::mnemonic can stay a plain const char *.
static const char *rep_mnemonic(const char *base) {
  if (strcmp(base, "movsb") == 0)
    return "rep movsb";
  if (strcmp(base, "movsw") == 0)
    return "rep movsw";
  if (strcmp(base, "stosb") == 0)
    return "rep stosb";
  if (strcmp(base, "stosw") == 0)
    return "rep stosw";
  if (strcmp(base, "lodsb") == 0)
    return "rep lodsb";
  if (strcmp(base, "lodsw") == 0)
    return "rep lodsw";
  if (strcmp(base, "scasb") == 0)
    return "rep scasb";
  if (strcmp(base, "scasw") == 0)
    return "rep scasw";
  if (strcmp(base, "cmpsb") == 0)
    return "rep cmpsb";
  if (strcmp(base, "cmpsw") == 0)
    return "rep cmpsw";
  return base;
}

int i8086_decode(const uint8_t *code, size_t avail, I8086Insn *out) {
  if (!g_tableReady)
    build_table();

  memset(out, 0, sizeof(*out));
  if (avail == 0)
    return 0;

  size_t pos = 0;

  // Prefixes. BOLO uses only 26h, 2Eh and F3h, but accepting the whole set
  // costs nothing and keeps the decoder honest.
  for (;;) {
    if (pos >= avail)
      return 0;
    uint8_t b = code[pos];
    if (b == 0x26 || b == 0x2E || b == 0x36 || b == 0x3E) {
      out->segOverride = b;
      ++pos;
    } else if (b == 0xF2 || b == 0xF3) {
      out->repPrefix = b;
      ++pos;
    } else if (b == 0xF0) {
      ++pos; // lock: no effect in a single-threaded interpreter
    } else {
      break;
    }
  }

  out->opcode = code[pos++];
  const OpInfo *info = &kOps[out->opcode];
  if (info->form == OF_BAD)
    return 0;

  const char *mnemonic = info->mnemonic;

  // ModRM and displacement.
  if (info->form == OF_MODRM || info->form == OF_MODRM_I8 || info->form == OF_MODRM_I16 ||
      info->form == OF_MODRM_IS8) {
    if (pos >= avail)
      return 0;
    out->modrm = code[pos++];
    out->hasModrm = true;

    unsigned dispSize = modrm_disp_size(out->modrm);
    if (pos + dispSize > avail)
      return 0;
    if (dispSize == 1)
      out->disp = (uint16_t)(int16_t)(int8_t)code[pos];
    else if (dispSize == 2)
      out->disp = (uint16_t)(code[pos] | ((uint16_t)code[pos + 1] << 8));
    pos += dispSize;

    // Group opcodes take their mnemonic from the reg field.
    unsigned reg = (unsigned)((out->modrm >> 3) & 7);
    if (!mnemonic) {
      switch (out->opcode) {
      case 0x80:
      case 0x81:
      case 0x82:
      case 0x83:
        mnemonic = kGroup80[reg];
        break;
      case 0xD0:
      case 0xD1:
      case 0xD2:
      case 0xD3:
        mnemonic = kGroupD0[reg];
        break;
      case 0xF6:
      case 0xF7:
        mnemonic = kGroupF6[reg];
        break;
      case 0xFE:
        mnemonic = kGroupFE[reg];
        break;
      case 0xFF:
        mnemonic = kGroupFF[reg];
        break;
      default:
        break;
      }
      if (!mnemonic)
        return 0;
    }

    // F6/F7 reg=0 carry an immediate; the other subops do not.
    if ((out->opcode == 0xF6 || out->opcode == 0xF7) && reg == 0) {
      unsigned immSize = out->opcode == 0xF6 ? 1 : 2;
      if (pos + immSize > avail)
        return 0;
      out->imm = immSize == 1 ? code[pos] : (uint16_t)(code[pos] | ((uint16_t)code[pos + 1] << 8));
      pos += immSize;
    }
  }

  // Immediates and relative targets.
  switch (info->form) {
  case OF_MODRM_I8:
    if (pos + 1 > avail)
      return 0;
    out->imm = code[pos++];
    break;
  case OF_MODRM_IS8:
    if (pos + 1 > avail)
      return 0;
    out->imm = (uint16_t)(int16_t)(int8_t)code[pos++];
    break;
  case OF_MODRM_I16:
  case OF_I16:
  case OF_REL16:
  case OF_MOFFS:
    if (pos + 2 > avail)
      return 0;
    out->imm = (uint16_t)(code[pos] | ((uint16_t)code[pos + 1] << 8));
    pos += 2;
    break;
  case OF_I8:
    if (pos + 1 > avail)
      return 0;
    out->imm = code[pos++];
    break;
  case OF_REL8:
    if (pos + 1 > avail)
      return 0;
    out->imm = (uint16_t)(int16_t)(int8_t)code[pos++];
    break;
  default:
    break;
  }

  if (out->repPrefix)
    mnemonic = rep_mnemonic(mnemonic);

  out->mnemonic = mnemonic;
  out->len = (uint8_t)pos;
  return (int)pos;
}
```

**One thing to fix while writing this:** the `set_op(0x91 + r - 1 + 1, OF_BAD, NULL);` line inside the `0x40`-`0x5F` loop above is dead weight that the `0x90`-block immediately overwrites — delete it rather than copying it in.

- [ ] **Step 5: Run the decode test**

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -3
cd build && ctest --output-on-failure -R decode; cd ..
```

Expected: `decode ok: 3439 instructions`.

The test prints the first 20 of each failure kind with the LST line number, so work through them by reading the named lines. Do **not** relax the test to make it pass; a length mismatch is a real decode bug and every later task depends on this being exact.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/i8086.c emu/i8086.h emu/test_decode.c && echo "format clean"
git add emu/i8086.c emu/i8086.h emu/test_decode.c emu/CMakeLists.txt
git commit -m "Add an 8086 decoder validated against all 3439 LST instructions"
```

---

### Task 3: The ALU and flag semantics

Flags are where 8086 emulators go wrong, and this program is unusually exposed: it passes the carry flag as a function argument. This task builds the ALU as a standalone, exhaustively tested unit before anything calls it.

**Files:**
- Create: `emu/test_alu.c`
- Modify: `emu/i8086.h` (append the ALU declarations), `emu/i8086.c` (append the ALU), `emu/CMakeLists.txt`

**Interfaces:**
- Consumes: the `I8086_*` flag masks from Task 2.
- Produces:

```c
typedef enum I8086AluOp {
  I8086_ADD, I8086_OR, I8086_ADC, I8086_SBB,
  I8086_AND, I8086_SUB, I8086_XOR, I8086_CMP,
} I8086AluOp;

uint16_t i8086_alu(I8086AluOp op, bool wide, uint16_t a, uint16_t b, uint16_t *flags);
uint16_t i8086_inc(bool wide, uint16_t a, uint16_t *flags);
uint16_t i8086_dec(bool wide, uint16_t a, uint16_t *flags);
uint16_t i8086_neg(bool wide, uint16_t a, uint16_t *flags);
uint16_t i8086_not(bool wide, uint16_t a);
uint16_t i8086_shift(uint8_t subop, bool wide, uint16_t a, uint8_t count, uint16_t *flags);
uint32_t i8086_mul(bool wide, uint16_t a, uint16_t b, uint16_t *flags);
```

#### Flag rules

- **PF** is the even parity of the **low 8 bits only**, set when the count of set bits is even.
- **ZF** set when the whole result (8 or 16 bits) is zero.
- **SF** = the result's sign bit (bit 7 or 15).
- **AF** = carry or borrow out of bit 3: `((a ^ b ^ result) & 0x10) != 0`.
- **CF** for add = carry out of the top bit; for sub/cmp = borrow, i.e. `a < b` unsigned (`a < b + carryIn` for `sbb`).
- **OF** for add = `(~(a ^ b) & (a ^ result) & signBit) != 0`; for sub = `((a ^ b) & (a ^ result) & signBit) != 0`.
- **`and`/`or`/`xor`/`test`** clear CF and OF, set SF/ZF/PF from the result, and leave AF **undefined** — this implementation clears it.
- **`inc`/`dec` must not touch CF.** This is the single most consequential rule in the file. They set OF, SF, ZF, AF and PF only.
- **`neg`**: CF = `(operand != 0)`, OF set only when the operand is the sign bit alone (`0x80`/`0x8000`), other flags from the result.
- **Shifts and rotates:** a count of 0 changes no flags at all. For a count of 1: `shl`/`rol`/`rcl` set OF = MSB(result) XOR CF(result); `shr` sets OF = MSB(original operand); `sar` sets OF = 0; `ror`/`rcr` set OF = MSB(result) XOR bit(MSB-1)(result). For counts above 1 OF is architecturally undefined — this implementation computes it with the same rule as count 1, applied to the final result. `shl`/`shr`/`sar` set SF/ZF/PF from the result and leave AF undefined (cleared here); the rotates leave SF/ZF/PF/AF untouched.
- **`mul`**: CF = OF = (the high half is nonzero). SF, ZF, AF and PF are architecturally undefined — this implementation sets SF/ZF/PF from the full 16-bit product and clears AF.

Every "undefined" choice above is a deliberate decision, recorded in a comment at the definition. BOLO uses `mul` twice and multi-bit shifts four times; if one of these choices is wrong, plan 3's screen diff is what surfaces it.

- [ ] **Step 1: Append the ALU declarations to `emu/i8086.h`**

Insert immediately before `#endif // BOLO_I8086_H`:

```c
/// ALU operations, numbered as the 8086 encodes them in the 80h group's reg
/// field and in the 00-3Fh opcode families.
typedef enum I8086AluOp {
  I8086_ADD,
  I8086_OR,
  I8086_ADC,
  I8086_SBB,
  I8086_AND,
  I8086_SUB,
  I8086_XOR,
  I8086_CMP,
} I8086AluOp;

/// Compute `a op b` at 8 or 16 bits, updating *flags. For I8086_ADC and
/// I8086_SBB the incoming CF in *flags is part of the operation. I8086_CMP
/// returns `a` unchanged -- the caller discards the result and keeps the flags.
uint16_t i8086_alu(I8086AluOp op, bool wide, uint16_t a, uint16_t b, uint16_t *flags);

/// inc/dec deliberately preserve CF; the original passes the carry flag between
/// routines, so clobbering it here would corrupt game logic silently.
uint16_t i8086_inc(bool wide, uint16_t a, uint16_t *flags);
uint16_t i8086_dec(bool wide, uint16_t a, uint16_t *flags);

uint16_t i8086_neg(bool wide, uint16_t a, uint16_t *flags);
uint16_t i8086_not(bool wide, uint16_t a);

/// Shift or rotate. `subop` is the ModRM reg field of the D0-D3 group:
/// 0 rol, 1 ror, 2 rcl, 3 rcr, 4 shl, 5 shr, 7 sar. A count of 0 leaves every
/// flag untouched, as on real hardware.
uint16_t i8086_shift(uint8_t subop, bool wide, uint16_t a, uint8_t count, uint16_t *flags);

/// Unsigned multiply. Returns the full product: AX for the 8-bit form,
/// DX:AX (high half in the top 16 bits) for the 16-bit form.
uint32_t i8086_mul(bool wide, uint16_t a, uint16_t b, uint16_t *flags);
```

- [ ] **Step 2: Write the failing test `emu/test_alu.c`**

> **Defect in the code below, found during execution — do not transcribe it
> literally.** Every case is written as
> `check("...", i8086_alu(I8086_ADD, false, 0xFF, 0x01, &f), f, ...)`. The call
> writes `f` through its `&f` argument while a sibling argument reads `f`, and
> those are unsequenced: it is undefined behavior, and under GCC (which
> evaluates arguments right to left) the *stale* `f` is read, failing 26 of the
> 30 vectors against a correct ALU. Bind the result to a local first:
>
> ```c
> f = 0;
> uint16_t r = i8086_alu(I8086_ADD, false, 0xFF, 0x01, &f);
> check("add8 FF+01", r, f, 0x00, I8086_CF | I8086_AF | I8086_ZF | I8086_PF);
> ```
>
> **No expected value changes** — only the sequencing. The committed
> `emu/test_alu.c` is the corrected reference.

```c
// Flag semantics for every ALU operation the interpreter implements.
//
// Each case names the operation, its operands and the exact flags expected.
// The 8086's flag rules are the classic source of emulator bugs, and BOLO is
// unusually exposed to them: it passes the carry flag between routines as an
// argument, so an inc that clobbers CF changes gameplay rather than crashing.

#include "i8086.h"

#include <stdio.h>
#include <string.h>

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

  // ---- add ----
  // 8-bit wraparound: FF + 01 = 00, carry out, half carry, zero, even parity.
  f = 0;
  check("add8 FF+01", i8086_alu(I8086_ADD, false, 0xFF, 0x01, &f), f, 0x00,
        I8086_CF | I8086_AF | I8086_ZF | I8086_PF);
  // Signed overflow: 7F + 01 = 80.
  f = 0;
  check("add8 7F+01", i8086_alu(I8086_ADD, false, 0x7F, 0x01, &f), f, 0x80,
        I8086_OF | I8086_SF | I8086_AF);
  // Half carry alone: 0F + 01 = 10.
  f = 0;
  check("add8 0F+01", i8086_alu(I8086_ADD, false, 0x0F, 0x01, &f), f, 0x10, I8086_AF);
  // No half carry: 01 + 01 = 02.
  f = 0;
  check("add8 01+01", i8086_alu(I8086_ADD, false, 0x01, 0x01, &f), f, 0x02, 0);
  // 16-bit carry out.
  f = 0;
  check("add16 FFFF+0001", i8086_alu(I8086_ADD, true, 0xFFFF, 0x0001, &f), f, 0x0000,
        I8086_CF | I8086_AF | I8086_ZF | I8086_PF);

  // ---- adc ----
  // Carry in participates.
  f = I8086_CF;
  check("adc8 FE+01+C", i8086_alu(I8086_ADC, false, 0xFE, 0x01, &f), f, 0x00,
        I8086_CF | I8086_AF | I8086_ZF | I8086_PF);

  // ---- sub / cmp ----
  // Borrow: 00 - 01 = FF.
  f = 0;
  check("sub8 00-01", i8086_alu(I8086_SUB, false, 0x00, 0x01, &f), f, 0xFF,
        I8086_CF | I8086_AF | I8086_SF | I8086_PF);
  // Signed overflow: 80 - 01 = 7F.
  f = 0;
  check("sub8 80-01", i8086_alu(I8086_SUB, false, 0x80, 0x01, &f), f, 0x7F,
        I8086_OF | I8086_AF);
  // cmp of equal values sets ZF and returns its first operand untouched.
  f = 0;
  check("cmp16 1000,1000", i8086_alu(I8086_CMP, true, 0x1000, 0x1000, &f), f, 0x1000,
        I8086_ZF | I8086_PF);

  // ---- sbb ----
  f = I8086_CF;
  check("sbb8 00-00-C", i8086_alu(I8086_SBB, false, 0x00, 0x00, &f), f, 0xFF,
        I8086_CF | I8086_AF | I8086_SF | I8086_PF);

  // ---- logic: CF and OF always cleared ----
  f = I8086_CF | I8086_OF;
  check("and8 F0&0F", i8086_alu(I8086_AND, false, 0xF0, 0x0F, &f), f, 0x00,
        I8086_ZF | I8086_PF);
  f = I8086_CF;
  check("or8 F0|0F", i8086_alu(I8086_OR, false, 0xF0, 0x0F, &f), f, 0xFF,
        I8086_SF | I8086_PF);
  f = I8086_CF;
  check("xor8 FF^0F", i8086_alu(I8086_XOR, false, 0xFF, 0x0F, &f), f, 0xF0,
        I8086_SF | I8086_PF);

  // ---- inc / dec must preserve CF ----
  f = I8086_CF;
  check("inc8 FF (CF set)", i8086_inc(false, 0xFF, &f), f, 0x00,
        I8086_CF | I8086_AF | I8086_ZF | I8086_PF);
  f = 0;
  check("inc8 7F", i8086_inc(false, 0x7F, &f), f, 0x80, I8086_OF | I8086_SF | I8086_AF);
  f = I8086_CF;
  check("dec8 80 (CF set)", i8086_dec(false, 0x80, &f), f, 0x7F,
        I8086_CF | I8086_OF | I8086_AF);
  f = 0;
  check("dec8 01", i8086_dec(false, 0x01, &f), f, 0x00, I8086_ZF | I8086_PF);

  // ---- neg ----
  f = I8086_CF;
  check("neg8 00", i8086_neg(false, 0x00, &f), f, 0x00, I8086_ZF | I8086_PF);
  f = 0;
  check("neg8 01", i8086_neg(false, 0x01, &f), f, 0xFF,
        I8086_CF | I8086_AF | I8086_SF | I8086_PF);
  f = 0;
  check("neg8 80", i8086_neg(false, 0x80, &f), f, 0x80, I8086_CF | I8086_OF | I8086_SF);

  // ---- shifts ----
  f = 0;
  check("shl8 80,1", i8086_shift(4, false, 0x80, 1, &f), f, 0x00,
        I8086_CF | I8086_OF | I8086_ZF | I8086_PF);
  f = 0;
  check("shl8 01,1", i8086_shift(4, false, 0x01, 1, &f), f, 0x02, 0);
  f = 0;
  check("shr8 01,1", i8086_shift(5, false, 0x01, 1, &f), f, 0x00,
        I8086_CF | I8086_ZF | I8086_PF);
  f = 0;
  check("shr8 80,1", i8086_shift(5, false, 0x80, 1, &f), f, 0x40, I8086_OF);
  f = 0;
  check("sar16 8000,1", i8086_shift(7, true, 0x8000, 1, &f), f, 0xC000, I8086_SF | I8086_PF);
  // A count of 0 must leave every flag alone.
  f = I8086_CF | I8086_OF | I8086_ZF;
  check("shl8 FF,0", i8086_shift(4, false, 0xFF, 0, &f), f, 0xFF,
        I8086_CF | I8086_OF | I8086_ZF);

  // ---- rotates ----
  f = 0;
  check("rcl8 80,1 (CF=0)", i8086_shift(2, false, 0x80, 1, &f), f, 0x00,
        I8086_CF | I8086_OF);
  f = I8086_CF;
  check("rcr8 01,1 (CF=1)", i8086_shift(3, false, 0x01, 1, &f), f, 0x80,
        I8086_CF | I8086_OF);
  f = 0;
  check("rol16 8000,1", i8086_shift(0, true, 0x8000, 1, &f), f, 0x0001,
        I8086_CF | I8086_OF);

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
```

- [ ] **Step 3: Add the target and run to see it fail**

Add to `emu/CMakeLists.txt`:

```cmake
add_executable(test_alu test_alu.c)
target_link_libraries(test_alu bolo_i8086)
add_test(NAME alu COMMAND test_alu)
```

and add `test_alu` to the warnings `foreach` list.

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -8
```

Expected: FAIL at link — undefined references to `i8086_alu`, `i8086_inc`, `i8086_dec`, `i8086_neg`, `i8086_shift`, `i8086_mul`.

- [ ] **Step 4: Append the ALU to `emu/i8086.c`**

```c
/* ---------------------------------------------------------------- ALU ---- */

static uint16_t mask_of(bool wide) {
  return wide ? 0xFFFF : 0x00FF;
}

static uint16_t sign_bit_of(bool wide) {
  return wide ? 0x8000 : 0x0080;
}

/// PF reflects the parity of the low 8 bits only, on both 8- and 16-bit
/// operations. Set when the number of set bits is even.
static bool parity8(uint8_t v) {
  v ^= (uint8_t)(v >> 4);
  v ^= (uint8_t)(v >> 2);
  v ^= (uint8_t)(v >> 1);
  return (v & 1) == 0;
}

static void set_flag(uint16_t *flags, uint16_t bit, bool on) {
  if (on)
    *flags |= bit;
  else
    *flags &= (uint16_t)~bit;
}

/// SF, ZF and PF, which every result-producing operation sets the same way.
static void set_szp(uint16_t *flags, bool wide, uint16_t result) {
  set_flag(flags, I8086_SF, (result & sign_bit_of(wide)) != 0);
  set_flag(flags, I8086_ZF, (result & mask_of(wide)) == 0);
  set_flag(flags, I8086_PF, parity8((uint8_t)result));
}

uint16_t i8086_alu(I8086AluOp op, bool wide, uint16_t a, uint16_t b, uint16_t *flags) {
  uint16_t mask = mask_of(wide), sign = sign_bit_of(wide);
  a &= mask;
  b &= mask;

  switch (op) {
  case I8086_AND:
  case I8086_OR:
  case I8086_XOR: {
    uint16_t r = op == I8086_AND ? (uint16_t)(a & b)
        : op == I8086_OR        ? (uint16_t)(a | b)
                                : (uint16_t)(a ^ b);
    r &= mask;
    // The logic operations always clear CF and OF. AF is architecturally
    // undefined here; we clear it.
    set_flag(flags, I8086_CF, false);
    set_flag(flags, I8086_OF, false);
    set_flag(flags, I8086_AF, false);
    set_szp(flags, wide, r);
    return r;
  }

  case I8086_ADD:
  case I8086_ADC: {
    unsigned carryIn = (op == I8086_ADC && (*flags & I8086_CF)) ? 1u : 0u;
    uint32_t full = (uint32_t)a + b + carryIn;
    uint16_t r = (uint16_t)(full & mask);
    set_flag(flags, I8086_CF, (full & ((uint32_t)mask + 1)) != 0);
    set_flag(flags, I8086_AF, ((a ^ b ^ r) & 0x10) != 0);
    set_flag(flags, I8086_OF, ((uint16_t)(~(a ^ b) & (a ^ r)) & sign) != 0);
    set_szp(flags, wide, r);
    return r;
  }

  case I8086_SUB:
  case I8086_SBB:
  case I8086_CMP: {
    unsigned borrowIn = (op == I8086_SBB && (*flags & I8086_CF)) ? 1u : 0u;
    uint32_t full = (uint32_t)a - b - borrowIn;
    uint16_t r = (uint16_t)(full & mask);
    set_flag(flags, I8086_CF, (full & ((uint32_t)mask + 1)) != 0);
    set_flag(flags, I8086_AF, ((a ^ b ^ r) & 0x10) != 0);
    set_flag(flags, I8086_OF, ((uint16_t)((a ^ b) & (a ^ r)) & sign) != 0);
    set_szp(flags, wide, r);
    // cmp keeps the flags and throws the difference away; returning `a` lets
    // the caller store the result unconditionally without special-casing.
    return op == I8086_CMP ? a : r;
  }
  }
  return 0;
}

uint16_t i8086_inc(bool wide, uint16_t a, uint16_t *flags) {
  uint16_t mask = mask_of(wide), sign = sign_bit_of(wide);
  a &= mask;
  uint16_t r = (uint16_t)((a + 1) & mask);
  // CF is deliberately untouched: the original passes carry between routines.
  set_flag(flags, I8086_AF, ((a ^ 1 ^ r) & 0x10) != 0);
  set_flag(flags, I8086_OF, r == sign);
  set_szp(flags, wide, r);
  return r;
}

uint16_t i8086_dec(bool wide, uint16_t a, uint16_t *flags) {
  uint16_t mask = mask_of(wide), sign = sign_bit_of(wide);
  a &= mask;
  uint16_t r = (uint16_t)((a - 1) & mask);
  // CF is deliberately untouched; see i8086_inc().
  set_flag(flags, I8086_AF, ((a ^ 1 ^ r) & 0x10) != 0);
  set_flag(flags, I8086_OF, a == sign);
  set_szp(flags, wide, r);
  return r;
}

uint16_t i8086_neg(bool wide, uint16_t a, uint16_t *flags) {
  uint16_t mask = mask_of(wide), sign = sign_bit_of(wide);
  a &= mask;
  uint16_t r = (uint16_t)((0u - a) & mask);
  set_flag(flags, I8086_CF, a != 0);
  set_flag(flags, I8086_OF, a == sign);
  set_flag(flags, I8086_AF, ((a ^ r) & 0x10) != 0);
  set_szp(flags, wide, r);
  return r;
}

uint16_t i8086_not(bool wide, uint16_t a) {
  // not affects no flags at all.
  return (uint16_t)(~a & mask_of(wide));
}

uint16_t i8086_shift(uint8_t subop, bool wide, uint16_t a, uint8_t count, uint16_t *flags) {
  uint16_t mask = mask_of(wide), sign = sign_bit_of(wide);
  a &= mask;

  // A count of zero is a true no-op on the 8086: not one flag changes.
  if (count == 0)
    return a;

  uint16_t r = a;
  bool cf = (*flags & I8086_CF) != 0;

  for (unsigned n = 0; n != count; ++n) {
    switch (subop) {
    case 0: // rol
      cf = (r & sign) != 0;
      r = (uint16_t)(((r << 1) | (cf ? 1u : 0u)) & mask);
      break;
    case 1: // ror
      cf = (r & 1) != 0;
      r = (uint16_t)(((r >> 1) | (cf ? sign : 0u)) & mask);
      break;
    case 2: { // rcl
      bool msb = (r & sign) != 0;
      r = (uint16_t)(((r << 1) | (cf ? 1u : 0u)) & mask);
      cf = msb;
      break;
    }
    case 3: { // rcr
      bool lsb = (r & 1) != 0;
      r = (uint16_t)(((r >> 1) | (cf ? sign : 0u)) & mask);
      cf = lsb;
      break;
    }
    case 4: // shl
      cf = (r & sign) != 0;
      r = (uint16_t)((r << 1) & mask);
      break;
    case 5: // shr
      cf = (r & 1) != 0;
      r = (uint16_t)((r >> 1) & mask);
      break;
    case 7: { // sar
      cf = (r & 1) != 0;
      uint16_t keep = (uint16_t)(r & sign);
      r = (uint16_t)(((r >> 1) | keep) & mask);
      break;
    }
    default:
      return a; // subop 6 is not an 8086 instruction
    }
  }

  set_flag(flags, I8086_CF, cf);

  // OF is architecturally defined only for a count of 1. For larger counts we
  // apply the same rule to the final result; the choice is documented rather
  // than correct, and plan 3's screen diff is what would expose it. BOLO uses a
  // variable count in four places (D2h/D3h).
  switch (subop) {
  case 0: // rol
  case 2: // rcl
  case 4: // shl
    set_flag(flags, I8086_OF, (((r & sign) != 0) != cf));
    break;
  case 5: // shr
    set_flag(flags, I8086_OF, (a & sign) != 0);
    break;
  case 7: // sar
    set_flag(flags, I8086_OF, false);
    break;
  case 1: // ror
  case 3: // rcr
    set_flag(flags, I8086_OF, (((r & sign) != 0) != ((r & (sign >> 1)) != 0)));
    break;
  default:
    break;
  }

  // The shifts set SF/ZF/PF and leave AF undefined (cleared here); the rotates
  // leave all four untouched.
  if (subop == 4 || subop == 5 || subop == 7) {
    set_szp(flags, wide, r);
    set_flag(flags, I8086_AF, false);
  }

  return r;
}

uint32_t i8086_mul(bool wide, uint16_t a, uint16_t b, uint16_t *flags) {
  uint32_t product;
  bool highNonzero;

  if (wide) {
    product = (uint32_t)a * b;
    highNonzero = (product >> 16) != 0;
  } else {
    product = (uint32_t)(a & 0xFF) * (b & 0xFF);
    highNonzero = (product & 0xFF00) != 0;
  }

  set_flag(flags, I8086_CF, highNonzero);
  set_flag(flags, I8086_OF, highNonzero);
  // SF, ZF, AF and PF are architecturally undefined after mul. We set SF/ZF/PF
  // from the low 16 bits of the product and clear AF, so the interpreter is at
  // least deterministic. BOLO uses `mul cl` twice, at 2913:1026 and 2913:102A.
  set_szp(flags, true, (uint16_t)product);
  set_flag(flags, I8086_AF, false);

  return product;
}
```

- [ ] **Step 5: Run the ALU test**

```bash
cmake --build build 2>&1 | tail -3 && cd build && ctest --output-on-failure -R alu; cd ..
```

Expected: `alu ok`.

Each failure line prints the operation, the result and both flag strings in `OSZAPC` order, so a mismatch names the exact flag. If a case here disagrees with the rules in this task's preamble, **the rules win** — they are the 8086's documented behavior, and the test encodes them.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/i8086.c emu/i8086.h emu/test_alu.c && echo "format clean"
git add emu/i8086.c emu/i8086.h emu/test_alu.c emu/CMakeLists.txt
git commit -m "Add the 8086 ALU with tested flag semantics"
```

---

### Task 4: The execution engine

Execute every opcode in the inventory. Memory and ports are reached only through the callbacks, so this task can be tested against a flat-RAM toy machine with no devices at all — the real machine with EGA and ports is plan 3.

**Files:**
- Create: `emu/test_exec.c`
- Modify: `emu/i8086.c` (append the executor), `emu/CMakeLists.txt`

**Interfaces:**
- Consumes: `I8086Insn`, `i8086_decode` from Task 2; the ALU from Task 3.
- Produces: `i8086_reset(I8086 *cpu)`, `bool i8086_step(I8086 *cpu)`, `void i8086_interrupt(I8086 *cpu, uint8_t vec)` (declared in Task 2's header).

#### Rules the executor must follow

- **Fetch through the bus.** `i8086_step` reads up to 6 bytes at `CS:IP` with `cpu->read8` into a local buffer, then calls `i8086_decode` on it. Reading a few bytes past the instruction is harmless here: the program lives in RAM, well away from the A0000 EGA window.
- **Segment defaults.** The effective-address table in Task 2 gives the default segment per `rm`. A segment override prefix replaces it — **except for the string destination**, which is always `ES:DI` and cannot be overridden. `SS:SP` for stack operations is likewise fixed.
- **`rep` on a string instruction** repeats while `CX != 0`, decrementing `CX` each iteration. For `movs`/`stos`/`lods` that is the whole story — the `repe`/`repne` ZF condition applies only to `cmps`/`scas`, which BOLO never uses with a prefix. Implement the ZF condition anyway; it is three lines.
- **Direction flag.** `DF` set means the string pointers decrement.
- **IP is advanced past the instruction before executing it**, so relative branches are computed from the address of the *next* instruction. `E8`/`E9`/`EB`/`70`-`7F`/`E0`-`E3` all add their sign-extended `imm` to the post-increment IP.
- **`i8086_interrupt`** pushes FLAGS, then CS, then IP; clears IF and TF; and loads IP from `0000:vec*4` and CS from `0000:vec*4+2`. `iret` pops IP, CS, then FLAGS. This is not optional: BOLO installs its own INT 08h and INT 09h handlers through DOS AH=25h, so the harness reaches them only by vectoring through the guest's own table.
- **`FLAGS` bookkeeping.** After any write to FLAGS (`popf`, `iret`, `sahf`), re-apply `I8086_FLAGS_ALWAYS_SET` and clear bits 3 and 5.
- **Errors are hard.** On an unknown opcode or an unimplemented sub-operation, set `cpu->error` to a static string and `cpu->errorAddr` to the linear address of the instruction, and return false. Never guess.

- [ ] **Step 1: Write the failing test `emu/test_exec.c`**

```c
// Execution tests over a toy machine: 1MB of flat RAM, no devices.
//
// Each case hand-assembles a short program, runs it to a halt marker, and
// checks the resulting registers and memory. The point is to exercise the
// machinery the decode and ALU tests cannot reach -- effective addresses,
// the stack, string instructions, prefixes and interrupt vectoring -- before
// plan 3 wires up real hardware.

#include "i8086.h"

#include <stdio.h>
#include <string.h>

#define MEM_SIZE 0x100000
static uint8_t g_mem[MEM_SIZE];
static uint8_t g_ports[0x10000];

static int g_failures;

static uint8_t mem_read8(void *ctx, uint32_t linear) {
  (void)ctx;
  return g_mem[linear & 0xFFFFF];
}

static void mem_write8(void *ctx, uint32_t linear, uint8_t value) {
  (void)ctx;
  g_mem[linear & 0xFFFFF] = value;
}

static uint8_t port_in8(void *ctx, uint16_t port) {
  (void)ctx;
  return g_ports[port];
}

static void port_out8(void *ctx, uint16_t port, uint8_t value) {
  (void)ctx;
  g_ports[port] = value;
}

/// Reset the CPU, attach the toy bus, and point every segment at 1000h so the
/// program at linear 10000h is addressable as 1000:0000.
static void setup_cpu(I8086 *cpu) {
  i8086_reset(cpu);
  cpu->ctx = NULL;
  cpu->read8 = mem_read8;
  cpu->write8 = mem_write8;
  cpu->in8 = port_in8;
  cpu->out8 = port_out8;
  cpu->sreg[I8086_CS] = 0x1000;
  cpu->sreg[I8086_DS] = 0x1000;
  cpu->sreg[I8086_ES] = 0x1000;
  cpu->sreg[I8086_SS] = 0x1000;
  cpu->reg[I8086_SP] = 0xFFFE;
  cpu->ip = 0;
}

/// Step until IP reaches `stopIp`, or fail. Memory must already be prepared.
static bool run_loaded(I8086 *cpu, uint16_t stopIp, int maxSteps) {
  for (int i = 0; i != maxSteps; ++i) {
    if (cpu->ip == stopIp)
      return true;
    if (!i8086_step(cpu)) {
      fprintf(stderr, "  step failed at %05X: %s\n", cpu->errorAddr,
              cpu->error ? cpu->error : "(no message)");
      return false;
    }
  }
  fprintf(stderr, "  never reached stopIp %04X (IP is %04X)\n", stopIp, cpu->ip);
  return false;
}

/// Clear memory, load `code` at 1000:0000, and run it.
static bool run(I8086 *cpu, const uint8_t *code, size_t codeLen, uint16_t stopIp, int maxSteps) {
  memset(g_mem, 0, sizeof(g_mem));
  memcpy(g_mem + 0x10000, code, codeLen);
  setup_cpu(cpu);
  return run_loaded(cpu, stopIp, maxSteps);
}

static void expect_u16(const char *what, uint16_t got, uint16_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-32s got %04X, expected %04X\n", what, got, want);
  ++g_failures;
}

static void expect_u8(const char *what, uint8_t got, uint8_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-32s got %02X, expected %02X\n", what, got, want);
  ++g_failures;
}

int main(void) {
  I8086 cpu;

  // --- mov immediates, register-to-register, and cbw ---
  {
    static const uint8_t code[] = {
        0xB8, 0x34, 0x12,       // mov ax,1234h
        0x8B, 0xD8,             // mov bx,ax
        0xB0, 0xF0,             // mov al,0F0h
        0x98,                   // cbw
    };
    if (!run(&cpu, code, sizeof(code), 8, 20)) {
      ++g_failures;
    } else {
      expect_u16("mov bx,ax", cpu.reg[I8086_BX], 0x1234);
      expect_u16("cbw of F0h", cpu.reg[I8086_AX], 0xFFF0);
    }
  }

  // --- push / pop / call / retn round trip ---
  {
    //   0000 mov ax,2211h        B8 11 22
    //   0003 push ax             50
    //   0004 call 000Ah          E8 03 00   (next IP 0007, + 3)
    //   0007 pop bx              5B
    //   0008 jmp 000Eh           EB 04      (next IP 000A, + 4)
    //   000A mov cx,0099h        B9 99 00
    //   000D retn                C3
    //   000E stop
    // Control flows 0000 -> 0004 (call) -> 000A -> 000D (retn) -> 0007 -> 0008
    // (jmp) -> 000E, so both the call target and the return path are exercised.
    static const uint8_t code[] = {
        0xB8, 0x11, 0x22, 0x50, 0xE8, 0x03, 0x00, 0x5B,
        0xEB, 0x04, 0xB9, 0x99, 0x00, 0xC3,
    };
    if (!run(&cpu, code, sizeof(code), 0x000E, 40)) {
      ++g_failures;
    } else {
      expect_u16("call/retn set cx", cpu.reg[I8086_CX], 0x0099);
      expect_u16("push/pop round trip", cpu.reg[I8086_BX], 0x2211);
      expect_u16("sp restored", cpu.reg[I8086_SP], 0xFFFE);
    }
  }

  // --- rep stosb forward, then again with DF set ---
  {
    //   0000 mov di,0100h        BF 00 01
    //   0003 mov cx,0004h        B9 04 00
    //   0006 mov al,0AAh         B0 AA
    //   0008 cld                 FC
    //   0009 rep stosb           F3 AA
    //   000B stop
    static const uint8_t code[] = {
        0xBF, 0x00, 0x01, 0xB9, 0x04, 0x00, 0xB0, 0xAA, 0xFC, 0xF3, 0xAA,
    };
    if (!run(&cpu, code, sizeof(code), 0x000B, 40)) {
      ++g_failures;
    } else {
      for (unsigned i = 0; i != 4; ++i)
        expect_u8("rep stosb byte", g_mem[0x10100 + i], 0xAA);
      expect_u8("rep stosb stopped", g_mem[0x10104], 0x00);
      expect_u16("rep stosb left cx=0", cpu.reg[I8086_CX], 0);
      expect_u16("rep stosb advanced di", cpu.reg[I8086_DI], 0x0104);
    }
  }

  // --- loop and jcxz ---
  {
    //   0000 mov cx,0005h        B9 05 00
    //   0003 mov bx,0000h        BB 00 00
    //   0006 inc bx              43
    //   0007 loop 0006h          E2 FD
    //   0009 stop
    static const uint8_t code[] = {0xB9, 0x05, 0x00, 0xBB, 0x00, 0x00, 0x43, 0xE2, 0xFD};
    if (!run(&cpu, code, sizeof(code), 0x0009, 60)) {
      ++g_failures;
    } else {
      expect_u16("loop ran 5 times", cpu.reg[I8086_BX], 5);
      expect_u16("loop left cx=0", cpu.reg[I8086_CX], 0);
    }
  }

  // --- ModRM addressing: mod=1 (disp8) with BX+SI, and mod=0 rm=6 direct ---
  {
    //   0000 mov bx,0100h        BB 00 01
    //   0003 mov si,0002h        BE 02 00
    //   0006 mov al,55h          B0 55
    //   0008 mov [bx+si+10h],al  88 40 10
    //   000B mov ah,[0112h]      8A 26 12 01
    //   000F stop
    static const uint8_t code[] = {
        0xBB, 0x00, 0x01, 0xBE, 0x02, 0x00, 0xB0, 0x55,
        0x88, 0x40, 0x10, 0x8A, 0x26, 0x12, 0x01,
    };
    if (!run(&cpu, code, sizeof(code), 0x000F, 40)) {
      ++g_failures;
    } else {
      expect_u8("mod=1 BX+SI+disp8 store", g_mem[0x10112], 0x55);
      expect_u8("mod=0 rm=6 direct load", (uint8_t)(cpu.reg[I8086_AX] >> 8), 0x55);
    }
  }

  // --- es: segment override ---
  {
    //   0000 mov ax,2000h        B8 00 20
    //   0003 mov es,ax           8E C0
    //   0005 mov di,0000h        BF 00 00
    //   0008 mov al,7Eh          B0 7E
    //   000A mov es:[di],al      26 88 05
    //   000D stop
    static const uint8_t code[] = {
        0xB8, 0x00, 0x20, 0x8E, 0xC0, 0xBF, 0x00, 0x00, 0xB0, 0x7E, 0x26, 0x88, 0x05,
    };
    if (!run(&cpu, code, sizeof(code), 0x000D, 40)) {
      ++g_failures;
    } else {
      expect_u8("es: override target", g_mem[0x20000], 0x7E);
      expect_u8("ds: was not written", g_mem[0x10000 + 0], 0xB8);
    }
  }

  // --- xlat, with the translation table written by the program itself so the
  //     case also covers "mov rm8,imm8" in the mod=0 rm=6 direct form ---
  {
    //   0000 mov byte ptr [0103h],9Ch   C6 06 03 01 9C
    //   0005 mov bx,0100h               BB 00 01
    //   0008 mov al,03h                 B0 03
    //   000A xlat                       D7
    //   000B stop
    static const uint8_t code[] = {
        0xC6, 0x06, 0x03, 0x01, 0x9C, 0xBB, 0x00, 0x01, 0xB0, 0x03, 0xD7,
    };
    if (!run(&cpu, code, sizeof(code), 0x000B, 30)) {
      ++g_failures;
    } else {
      expect_u8("xlat", (uint8_t)cpu.reg[I8086_AX], 0x9C);
    }
  }

  // --- int / iret through the guest vector table ---
  {
    //   0000 int 40h            CD 40
    //   0002 stop
    // Handler planted at 1000:0020:  mov bx,0BEEFh (BB EF BE) then iret (CF).
    static const uint8_t code[] = {0xCD, 0x40};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));

    // Vector 40h -> 1000:0020, in the guest's own table at 0000:0000.
    g_mem[0x40 * 4 + 0] = 0x20;
    g_mem[0x40 * 4 + 1] = 0x00;
    g_mem[0x40 * 4 + 2] = 0x00;
    g_mem[0x40 * 4 + 3] = 0x10;
    g_mem[0x10020] = 0xBB;
    g_mem[0x10021] = 0xEF;
    g_mem[0x10022] = 0xBE;
    g_mem[0x10023] = 0xCF;

    setup_cpu(&cpu);
    if (!run_loaded(&cpu, 0x0002, 30)) {
      ++g_failures;
    } else {
      expect_u16("int 40h ran the handler", cpu.reg[I8086_BX], 0xBEEF);
      expect_u16("iret restored sp", cpu.reg[I8086_SP], 0xFFFE);
      expect_u16("iret returned to 0002", cpu.ip, 0x0002);
    }
  }

  // --- lahf / sahf round trip, and that inc leaves CF alone ---
  {
    //   0000 stc                 F9
    //   0001 mov al,0FFh         B0 FF
    //   0003 inc al              FE C0     (CF must survive)
    //   0005 lahf                9F
    //   0006 mov bl,ah           8A DC
    //   0008 stop
    static const uint8_t code[] = {0xF9, 0xB0, 0xFF, 0xFE, 0xC0, 0x9F, 0x8A, 0xDC};
    if (!run(&cpu, code, sizeof(code), 0x0008, 30)) {
      ++g_failures;
    } else {
      expect_u8("inc preserved CF into lahf", (uint8_t)(cpu.reg[I8086_BX] & I8086_CF), I8086_CF);
      expect_u8("inc al of FFh wrapped", (uint8_t)cpu.reg[I8086_AX], 0x00);
    }
  }

  // --- in / out ---
  {
    //   0000 mov dx,03DAh        BA DA 03
    //   0003 in al,dx            EC
    //   0004 mov ah,al           8A E0
    //   0006 out 61h,al          E6 61
    //   0008 stop
    static const uint8_t code[] = {0xBA, 0xDA, 0x03, 0xEC, 0x8A, 0xE0, 0xE6, 0x61};
    memset(g_ports, 0, sizeof(g_ports));
    g_ports[0x3DA] = 0x08;
    if (!run(&cpu, code, sizeof(code), 0x0008, 30)) {
      ++g_failures;
    } else {
      expect_u8("in al,dx", (uint8_t)cpu.reg[I8086_AX], 0x08);
      expect_u8("out imm8,al", g_ports[0x61], 0x08);
    }
  }

  if (g_failures != 0) {
    fprintf(stderr, "\n%d execution check(s) failed\n", g_failures);
    return 1;
  }
  printf("exec ok\n");
  return 0;
}
```

Every hand-assembled program above carries its address layout in a comment, and
the branch displacements were computed against those addresses. If a case fails,
check the displacement arithmetic against the comment before suspecting the
executor — a relative branch is measured from the address of the *next*
instruction, not the branch itself.

- [ ] **Step 2: Add the target and run to see it fail**

Add to `emu/CMakeLists.txt`:

```cmake
add_executable(test_exec test_exec.c)
target_link_libraries(test_exec bolo_i8086)
add_test(NAME exec COMMAND test_exec)
```

and add `test_exec` to the warnings `foreach` list.

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -8
```

Expected: FAIL at link — undefined references to `i8086_reset`, `i8086_step`, `i8086_interrupt`.

- [ ] **Step 3: Append the executor to `emu/i8086.c`**

Write the executor covering every opcode in Task 2's inventory. Structure it as:

1. **Bus helpers** — `read16`/`write16` composed from the byte callbacks, little-endian; `push16`/`pop16` on `SS:SP`; `fetch` of the instruction bytes at `CS:IP`.
2. **Register accessors** — `get_reg8`/`set_reg8`, `get_reg16`/`set_reg16` (below).
3. **Effective address** — `effective_addr` (below).
4. **Operand read/write** — `rm8`/`rm16` getters and setters that branch on `mod == 3` to the register accessors and otherwise to `effective_addr`.
5. **The dispatch switch** on `insn.opcode`, calling the ALU from Task 3 for arithmetic and doing the rest inline.

The byte-register encoding and the effective-address table are the two places a
silent mistake is hardest to catch — `test_exec` covers them, but write them from
here rather than from memory:

```c
/// The 8086 numbers byte registers AL CL DL BL AH CH DH BH: the low two bits
/// pick the word register, and bit 2 selects its high half.
static uint8_t get_reg8(const I8086 *cpu, unsigned r) {
  uint16_t w = cpu->reg[r & 3];
  return (uint8_t)(r & 4 ? w >> 8 : w);
}

static void set_reg8(I8086 *cpu, unsigned r, uint8_t v) {
  uint16_t *w = &cpu->reg[r & 3];
  if (r & 4)
    *w = (uint16_t)((*w & 0x00FF) | ((uint16_t)v << 8));
  else
    *w = (uint16_t)((*w & 0xFF00) | v);
}

/// Resolve a memory operand. Must not be called with mod == 3, which names a
/// register rather than an address.
///
/// The default segment is SS for any form based on BP -- that is the 8086's
/// rule and the reason a stack frame reachable through BP does not need an
/// override -- except for the mod=0 rm=6 form, which is a direct address in DS
/// rather than a BP-relative one. A segment override prefix replaces whatever
/// this picks.
static uint32_t effective_addr(const I8086 *cpu, const I8086Insn *insn) {
  unsigned mod = (unsigned)(insn->modrm >> 6), rm = (unsigned)(insn->modrm & 7);
  uint16_t off;
  unsigned defSeg = I8086_DS;

  switch (rm) {
  case 0: off = (uint16_t)(cpu->reg[I8086_BX] + cpu->reg[I8086_SI]); break;
  case 1: off = (uint16_t)(cpu->reg[I8086_BX] + cpu->reg[I8086_DI]); break;
  case 2: off = (uint16_t)(cpu->reg[I8086_BP] + cpu->reg[I8086_SI]); defSeg = I8086_SS; break;
  case 3: off = (uint16_t)(cpu->reg[I8086_BP] + cpu->reg[I8086_DI]); defSeg = I8086_SS; break;
  case 4: off = cpu->reg[I8086_SI]; break;
  case 5: off = cpu->reg[I8086_DI]; break;
  case 6:
    if (mod == 0) {
      off = insn->disp; // direct address, DS
    } else {
      off = cpu->reg[I8086_BP];
      defSeg = I8086_SS;
    }
    break;
  default: off = cpu->reg[I8086_BX]; break;
  }

  if (!(mod == 0 && rm == 6))
    off = (uint16_t)(off + insn->disp); // disp is 0 when mod == 0

  unsigned seg = defSeg;
  switch (insn->segOverride) {
  case 0x26: seg = I8086_ES; break;
  case 0x2E: seg = I8086_CS; break;
  case 0x36: seg = I8086_SS; break;
  case 0x3E: seg = I8086_DS; break;
  default: break;
  }

  return i8086_linear(cpu->sreg[seg], off);
}
```

Set `cpu->error` and `cpu->errorAddr` and return false for anything not in the inventory, with a message naming the opcode, e.g.:

```c
static bool fail_insn(I8086 *cpu, uint32_t addr, const char *msg) {
  cpu->error = msg;
  cpu->errorAddr = addr;
  return false;
}
```

`i8086_reset` zeroes `reg`, `sreg` and `ip`, sets `flags = I8086_FLAGS_ALWAYS_SET`, and clears `error`/`errorAddr`.

`i8086_interrupt` is:

```c
void i8086_interrupt(I8086 *cpu, uint8_t vec) {
  push16(cpu, cpu->flags);
  push16(cpu, cpu->sreg[I8086_CS]);
  push16(cpu, cpu->ip);
  cpu->flags &= (uint16_t)~(I8086_IF | I8086_TF);
  uint32_t entry = (uint32_t)vec * 4;
  cpu->ip = read16(cpu, entry);
  cpu->sreg[I8086_CS] = read16(cpu, entry + 2);
}
```

- [ ] **Step 4: Run the execution test**

```bash
cmake --build build 2>&1 | tail -3 && cd build && ctest --output-on-failure -R exec; cd ..
```

Expected: `exec ok`.

- [ ] **Step 5: Run the whole suite**

```bash
cd build && ctest --output-on-failure; cd ..
```

Expected: 10 tests pass — the 6 from plan 1 (`link`, `ppm`, `ega_render`, `headless_frames`, `determinism`, `pump_invariance`) plus `lst`, `decode`, `alu`, `exec`.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/i8086.c emu/i8086.h emu/test_exec.c && echo "format clean"
git add emu/i8086.c emu/test_exec.c emu/CMakeLists.txt
git commit -m "Add the 8086 execution engine"
```

---

## Definition of Done

- `ctest` from the build directory reports 10 passing tests.
- `test_lst` parses 3439 instructions and confirms every byte against `BOLO.COM`.
- `test_decode` decodes all 3439 with matching length and mnemonic, with zero unknown opcodes.
- `test_alu` and `test_exec` pass.
- `clang-format --dry-run -Werror emu/*.c emu/*.h` exits 0.
- `emu/i8086.c` and `emu/lst.c` do not reference `bolo.h` or link `bologame`.
- Nothing under `src/` or `disasm/` changed **by this plan**:
  `git diff --stat 6afe328..HEAD -- src disasm` is empty, where `6afe328` is
  this plan's base commit. Do **not** compare against `master`: branch `work`
  also carries plan 1, which legitimately changed `src/`, so that comparison
  reports 9 files and can never be empty.

## Notes for plan 3

- `i8086_decode` reads up to 6 bytes past `CS:IP` through `read8`. Once EGA is memory-mapped at A0000, confirm that a fetch never lands there — it does not for BOLO, whose code sits at `2913:0100`-`2F53`, but the machine's `read8` should not have side effects for reads in the code region regardless.
- The decoder's `mnemonic` is for diagnostics; the executor dispatches on `opcode`, so the four `jb`/`jc`-style spelling choices have no effect on behavior.
- **The undefined-flag decisions cannot affect BOLO, and are not worth
  revisiting on a divergence.** An earlier draft of this section named them as
  the first thing to check near `2913:1026`, `102A`, `0548`, `0627` and `1660`.
  That was wrong, and following it would cost a debugging session and invite a
  "fix" to flag code that was never at fault. Measured from the binary:
  **overflow is write-only in this program.** BOLO contains no signed
  conditional jump — opcodes `70`/`71` (`jo`/`jno`) and `7C`-`7F`
  (`jl`/`jge`/`jle`/`jg`) do not appear among its 3439 instructions — no
  `pushf`/`popf` (`9C`/`9D` are absent), and `OF` is bit 11, so the single
  `lahf` at `2913:178E` cannot observe it. Independently, all five sites have
  dead flags: `0548` is followed by `xor ah,ah`, `0627` by `or es:[di],al`,
  `1660` by `shl dh,cl` then `add bl,dl`, and both `mul` sites feed `add si,ax`
  with the flags unread. The choices remain documented at their definitions in
  `emu/i8086.c` for correctness, not as a debugging lead.
- `emu/machine.c` will need the runaway guards from the spec's "Error handling" section: abort if the guest executes outside the loaded image, and a per-frame instruction budget of about 5M.
