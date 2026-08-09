#include "i8086.h"

#include <string.h>

/// Table entry describing how to decode one opcode byte.
typedef enum OpForm {
  OF_NONE, ///< No operands beyond the opcode.
  OF_MODRM, ///< ModRM byte with any displacement.
  OF_MODRM_I8, ///< ModRM plus an 8-bit immediate.
  OF_MODRM_I16, ///< ModRM plus a 16-bit immediate.
  OF_MODRM_IS8, ///< ModRM plus a sign-extended 8-bit immediate (the 83h group).
  OF_I8, ///< 8-bit immediate.
  OF_I16, ///< 16-bit immediate.
  OF_REL8, ///< Sign-extended 8-bit relative branch.
  OF_REL16, ///< 16-bit relative branch.
  OF_MOFFS, ///< 16-bit direct memory offset (the A0-A3 accumulator moves).
  OF_BAD, ///< Not implemented.
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
static const char *const kGroupFF[8] = {"inc", "dec", "call", "call", "jmp", "jmp", "push", NULL};

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
      "jo",
      "jno",
      "jb",
      "jae",
      "jz",
      "jnz",
      "jbe",
      "ja",
      "js",
      "jns",
      "jp",
      "jnp",
      "jl",
      "jge",
      "jle",
      "jg"};
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
