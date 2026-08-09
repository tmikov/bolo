#include "i8086.h"

#include <stdio.h>
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
        : op == I8086_OR         ? (uint16_t)(a | b)
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

/* ----------------------------------------------------------- executor ---- */

/// Messages that name an opcode are formatted here. The interpreter stops dead
/// at the first failure, so a single buffer is enough and I8086::error can stay
/// a plain const char *.
static char g_errorMsg[64];

static bool fail_insn(I8086 *cpu, uint32_t addr, const char *msg) {
  cpu->error = msg;
  cpu->errorAddr = addr;
  return false;
}

/// Fail naming the opcode, plus the ModRM reg field when `sub` is not negative.
static bool fail_op(I8086 *cpu, uint32_t addr, const char *what, unsigned op, int sub) {
  if (sub < 0)
    snprintf(g_errorMsg, sizeof(g_errorMsg), "%s %02Xh", what, op);
  else
    snprintf(g_errorMsg, sizeof(g_errorMsg), "%s %02Xh /%d", what, op, sub);
  return fail_insn(cpu, addr, g_errorMsg);
}

/* -- the bus -- */

static uint16_t read16(I8086 *cpu, uint32_t linear) {
  uint8_t lo = cpu->read8(cpu->ctx, linear);
  uint8_t hi = cpu->read8(cpu->ctx, (linear + 1) & 0xFFFFF);
  return (uint16_t)(lo | ((uint16_t)hi << 8));
}

static void write16(I8086 *cpu, uint32_t linear, uint16_t value) {
  cpu->write8(cpu->ctx, linear, (uint8_t)value);
  cpu->write8(cpu->ctx, (linear + 1) & 0xFFFFF, (uint8_t)(value >> 8));
}

/// The stack is always SS:SP; a segment override prefix does not reach it.
static void push16(I8086 *cpu, uint16_t value) {
  cpu->reg[I8086_SP] = (uint16_t)(cpu->reg[I8086_SP] - 2);
  write16(cpu, i8086_linear(cpu->sreg[I8086_SS], cpu->reg[I8086_SP]), value);
}

static uint16_t pop16(I8086 *cpu) {
  uint16_t value = read16(cpu, i8086_linear(cpu->sreg[I8086_SS], cpu->reg[I8086_SP]));
  cpu->reg[I8086_SP] = (uint16_t)(cpu->reg[I8086_SP] + 2);
  return value;
}

static uint16_t in16(I8086 *cpu, uint16_t port) {
  uint8_t lo = cpu->in8(cpu->ctx, port);
  uint8_t hi = cpu->in8(cpu->ctx, (uint16_t)(port + 1));
  return (uint16_t)(lo | ((uint16_t)hi << 8));
}

static void out16(I8086 *cpu, uint16_t port, uint16_t value) {
  cpu->out8(cpu->ctx, port, (uint8_t)value);
  cpu->out8(cpu->ctx, (uint16_t)(port + 1), (uint8_t)(value >> 8));
}

/* -- registers -- */

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

/// FLAGS bits 1 and 12-15 read as 1 on an 8086, bits 3 and 5 as 0. Applied
/// after every wholesale write to FLAGS: popf, iret and sahf.
static uint16_t normalize_flags(uint16_t flags) {
  return (uint16_t)((flags | I8086_FLAGS_ALWAYS_SET) & ~(uint16_t)0x28);
}

/* -- effective addresses -- */

/// The offset part of a memory operand, which is also exactly what `lea`
/// produces. `*defSeg` receives the segment the 8086 would use without an
/// override prefix. Must not be called with mod == 3, which names a register
/// rather than an address.
///
/// The default segment is SS for any form based on BP -- that is the 8086's
/// rule and the reason a stack frame reachable through BP does not need an
/// override -- except for the mod=0 rm=6 form, which is a direct address in DS
/// rather than a BP-relative one.
static uint16_t effective_offset(const I8086 *cpu, const I8086Insn *insn, unsigned *defSeg) {
  unsigned mod = (unsigned)(insn->modrm >> 6), rm = (unsigned)(insn->modrm & 7);
  uint16_t off;

  *defSeg = I8086_DS;
  switch (rm) {
  case 0:
    off = (uint16_t)(cpu->reg[I8086_BX] + cpu->reg[I8086_SI]);
    break;
  case 1:
    off = (uint16_t)(cpu->reg[I8086_BX] + cpu->reg[I8086_DI]);
    break;
  case 2:
    off = (uint16_t)(cpu->reg[I8086_BP] + cpu->reg[I8086_SI]);
    *defSeg = I8086_SS;
    break;
  case 3:
    off = (uint16_t)(cpu->reg[I8086_BP] + cpu->reg[I8086_DI]);
    *defSeg = I8086_SS;
    break;
  case 4:
    off = cpu->reg[I8086_SI];
    break;
  case 5:
    off = cpu->reg[I8086_DI];
    break;
  case 6:
    if (mod == 0) {
      off = insn->disp; // direct address, DS
    } else {
      off = cpu->reg[I8086_BP];
      *defSeg = I8086_SS;
    }
    break;
  default:
    off = cpu->reg[I8086_BX];
    break;
  }

  if (!(mod == 0 && rm == 6))
    off = (uint16_t)(off + insn->disp); // disp is 0 when mod == 0

  return off;
}

/// Apply a segment override prefix, if any, to a default segment.
static unsigned override_seg(const I8086Insn *insn, unsigned defSeg) {
  switch (insn->segOverride) {
  case 0x26:
    return I8086_ES;
  case 0x2E:
    return I8086_CS;
  case 0x36:
    return I8086_SS;
  case 0x3E:
    return I8086_DS;
  default:
    return defSeg;
  }
}

/// Resolve a memory operand to a linear address. Must not be called with
/// mod == 3.
static uint32_t effective_addr(const I8086 *cpu, const I8086Insn *insn) {
  unsigned defSeg = I8086_DS;
  uint16_t off = effective_offset(cpu, insn, &defSeg);
  return i8086_linear(cpu->sreg[override_seg(insn, defSeg)], off);
}

/* -- operands -- */

static bool is_reg_operand(const I8086Insn *insn) {
  return (insn->modrm >> 6) == 3;
}

static uint16_t rm_read(I8086 *cpu, const I8086Insn *insn, bool wide) {
  if (is_reg_operand(insn)) {
    unsigned r = (unsigned)(insn->modrm & 7);
    return wide ? cpu->reg[r] : get_reg8(cpu, r);
  }
  uint32_t addr = effective_addr(cpu, insn);
  return wide ? read16(cpu, addr) : cpu->read8(cpu->ctx, addr);
}

static void rm_write(I8086 *cpu, const I8086Insn *insn, bool wide, uint16_t value) {
  if (is_reg_operand(insn)) {
    unsigned r = (unsigned)(insn->modrm & 7);
    if (wide)
      cpu->reg[r] = value;
    else
      set_reg8(cpu, r, (uint8_t)value);
    return;
  }
  uint32_t addr = effective_addr(cpu, insn);
  if (wide)
    write16(cpu, addr, value);
  else
    cpu->write8(cpu->ctx, addr, (uint8_t)value);
}

static uint16_t reg_read(const I8086 *cpu, const I8086Insn *insn, bool wide) {
  unsigned r = (unsigned)((insn->modrm >> 3) & 7);
  return wide ? cpu->reg[r] : get_reg8(cpu, r);
}

static void reg_write(I8086 *cpu, const I8086Insn *insn, bool wide, uint16_t value) {
  unsigned r = (unsigned)((insn->modrm >> 3) & 7);
  if (wide)
    cpu->reg[r] = value;
  else
    set_reg8(cpu, r, (uint8_t)value);
}

/* -- conditions -- */

/// Evaluate one of the sixteen 70-7F conditions. The even opcode is the
/// condition and the odd one its negation.
static bool cond_true(uint16_t flags, unsigned cc) {
  bool cf = (flags & I8086_CF) != 0;
  bool zf = (flags & I8086_ZF) != 0;
  bool sf = (flags & I8086_SF) != 0;
  bool of = (flags & I8086_OF) != 0;
  bool pf = (flags & I8086_PF) != 0;
  bool r;

  switch (cc >> 1) {
  case 0:
    r = of;
    break;
  case 1:
    r = cf;
    break;
  case 2:
    r = zf;
    break;
  case 3:
    r = cf || zf;
    break;
  case 4:
    r = sf;
    break;
  case 5:
    r = pf;
    break;
  case 6:
    r = sf != of;
    break;
  default:
    r = zf || sf != of;
    break;
  }
  return (cc & 1) ? !r : r;
}

/* -- string instructions -- */

/// DF set means the pointers walk backwards.
static void string_advance(I8086 *cpu, bool wide, bool si, bool di) {
  int delta = wide ? 2 : 1;
  if (cpu->flags & I8086_DF)
    delta = -delta;
  if (si)
    cpu->reg[I8086_SI] = (uint16_t)(cpu->reg[I8086_SI] + delta);
  if (di)
    cpu->reg[I8086_DI] = (uint16_t)(cpu->reg[I8086_DI] + delta);
}

/// One iteration of a string instruction. The source is DS:SI and honors a
/// segment override; the destination is ES:DI and cannot be overridden.
static void string_step(I8086 *cpu, const I8086Insn *insn) {
  bool wide = (insn->opcode & 1) != 0;
  uint32_t src = i8086_linear(cpu->sreg[override_seg(insn, I8086_DS)], cpu->reg[I8086_SI]);
  uint32_t dst = i8086_linear(cpu->sreg[I8086_ES], cpu->reg[I8086_DI]);

  switch (insn->opcode & 0xFE) {
  case 0xA4: // movs
    if (wide)
      write16(cpu, dst, read16(cpu, src));
    else
      cpu->write8(cpu->ctx, dst, cpu->read8(cpu->ctx, src));
    string_advance(cpu, wide, true, true);
    break;

  case 0xA6: { // cmps: [SI] - [DI]
    uint16_t a = wide ? read16(cpu, src) : cpu->read8(cpu->ctx, src);
    uint16_t b = wide ? read16(cpu, dst) : cpu->read8(cpu->ctx, dst);
    i8086_alu(I8086_CMP, wide, a, b, &cpu->flags);
    string_advance(cpu, wide, true, true);
    break;
  }

  case 0xAA: // stos
    if (wide)
      write16(cpu, dst, cpu->reg[I8086_AX]);
    else
      cpu->write8(cpu->ctx, dst, (uint8_t)cpu->reg[I8086_AX]);
    string_advance(cpu, wide, false, true);
    break;

  case 0xAC: // lods
    if (wide)
      cpu->reg[I8086_AX] = read16(cpu, src);
    else
      set_reg8(cpu, 0, cpu->read8(cpu->ctx, src));
    string_advance(cpu, wide, true, false);
    break;

  default: { // AE/AF scas: acc - [DI]
    uint16_t a = wide ? cpu->reg[I8086_AX] : get_reg8(cpu, 0);
    uint16_t b = wide ? read16(cpu, dst) : cpu->read8(cpu->ctx, dst);
    i8086_alu(I8086_CMP, wide, a, b, &cpu->flags);
    string_advance(cpu, wide, false, true);
    break;
  }
  }
}

/// A string instruction, repeated in full when it carries a rep prefix. Doing
/// the whole run inside one step is invisible to the guest as long as nothing
/// interrupts it, which is true here: interrupts are delivered between steps.
static void exec_string(I8086 *cpu, const I8086Insn *insn) {
  if (!insn->repPrefix) {
    string_step(cpu, insn);
    return;
  }

  // Only cmps and scas end the repetition on ZF; BOLO never prefixes either,
  // but leaving the condition out would be a trap for plan 3.
  unsigned base = (unsigned)(insn->opcode & 0xFE);
  bool checksZF = base == 0xA6 || base == 0xAE;

  while (cpu->reg[I8086_CX] != 0) {
    string_step(cpu, insn);
    cpu->reg[I8086_CX] = (uint16_t)(cpu->reg[I8086_CX] - 1);
    if (checksZF) {
      bool zf = (cpu->flags & I8086_ZF) != 0;
      // F3 is repe here (continue while equal), F2 repne.
      if (insn->repPrefix == 0xF3 ? !zf : zf)
        break;
    }
  }
}

/* -- the machine -- */

void i8086_reset(I8086 *cpu) {
  memset(cpu->reg, 0, sizeof(cpu->reg));
  memset(cpu->sreg, 0, sizeof(cpu->sreg));
  cpu->ip = 0;
  cpu->flags = I8086_FLAGS_ALWAYS_SET;
  cpu->error = NULL;
  cpu->errorAddr = 0;
}

void i8086_interrupt(I8086 *cpu, uint8_t vec) {
  push16(cpu, cpu->flags);
  push16(cpu, cpu->sreg[I8086_CS]);
  push16(cpu, cpu->ip);
  cpu->flags &= (uint16_t) ~(I8086_IF | I8086_TF);
  uint32_t entry = (uint32_t)vec * 4;
  cpu->ip = read16(cpu, entry);
  cpu->sreg[I8086_CS] = read16(cpu, entry + 2);
}

bool i8086_step(I8086 *cpu) {
  uint32_t addr = i8086_linear(cpu->sreg[I8086_CS], cpu->ip);

  // Fetching a few bytes past the instruction is harmless: BOLO's code sits far
  // from any memory-mapped device, and the machine's read8 has no side effects
  // in the code region.
  uint8_t bytes[I8086_MAX_INSN_BYTES];
  for (unsigned i = 0; i != I8086_MAX_INSN_BYTES; ++i)
    bytes[i] = cpu->read8(cpu->ctx, i8086_linear(cpu->sreg[I8086_CS], (uint16_t)(cpu->ip + i)));

  I8086Insn insn;
  if (i8086_decode(bytes, sizeof(bytes), &insn) == 0)
    return fail_op(cpu, addr, "cannot decode opcode", insn.opcode, -1);

  // IP is past the instruction before it executes, so a relative branch adds
  // its displacement to the address of the *next* instruction.
  cpu->ip = (uint16_t)(cpu->ip + insn.len);

  unsigned op = insn.opcode;

  // The eight ALU families at 00-3D. Within a family: +0 rm8,r8  +1 rm16,r16
  // +2 r8,rm8  +3 r16,rm16  +4 al,imm8  +5 ax,imm16. The +6/+7 slots are
  // push/pop sreg or prefixes and are excluded here.
  if (op < 0x40 && (op & 7) < 6) {
    I8086AluOp aluOp = (I8086AluOp)(op >> 3);
    unsigned form = op & 7;
    bool wide = (form & 1) != 0;
    uint16_t a, b, r;

    if (form >= 4) {
      a = wide ? cpu->reg[I8086_AX] : get_reg8(cpu, 0);
      b = insn.imm;
    } else if (form < 2) {
      a = rm_read(cpu, &insn, wide);
      b = reg_read(cpu, &insn, wide);
    } else {
      a = reg_read(cpu, &insn, wide);
      b = rm_read(cpu, &insn, wide);
    }

    r = i8086_alu(aluOp, wide, a, b, &cpu->flags);

    // cmp must not write anything back: on the real machine a store to video
    // memory has side effects, so "store the unchanged value" is not a no-op.
    if (aluOp != I8086_CMP) {
      if (form >= 4) {
        if (wide)
          cpu->reg[I8086_AX] = r;
        else
          set_reg8(cpu, 0, (uint8_t)r);
      } else if (form < 2) {
        rm_write(cpu, &insn, wide, r);
      } else {
        reg_write(cpu, &insn, wide, r);
      }
    }
    return true;
  }

  switch (op) {
  // --- push/pop segment register ---
  case 0x06:
  case 0x0E:
  case 0x16:
  case 0x1E:
    push16(cpu, cpu->sreg[(op >> 3) & 3]);
    break;
  case 0x07:
  case 0x17:
  case 0x1F:
    cpu->sreg[(op >> 3) & 3] = pop16(cpu);
    break;

  // --- inc/dec reg16 ---
  case 0x40:
  case 0x41:
  case 0x42:
  case 0x43:
  case 0x44:
  case 0x45:
  case 0x46:
  case 0x47:
    cpu->reg[op & 7] = i8086_inc(true, cpu->reg[op & 7], &cpu->flags);
    break;
  case 0x48:
  case 0x49:
  case 0x4A:
  case 0x4B:
  case 0x4C:
  case 0x4D:
  case 0x4E:
  case 0x4F:
    cpu->reg[op & 7] = i8086_dec(true, cpu->reg[op & 7], &cpu->flags);
    break;

  // --- push/pop reg16 ---
  case 0x50:
  case 0x51:
  case 0x52:
  case 0x53:
  case 0x54:
  case 0x55:
  case 0x56:
  case 0x57: {
    uint16_t v = cpu->reg[op & 7];
    // An 8086 pushes the already-decremented value for `push sp`; the 286 and
    // later push the original. Match the 8086.
    if ((op & 7) == I8086_SP)
      v = (uint16_t)(v - 2);
    push16(cpu, v);
    break;
  }
  case 0x58:
  case 0x59:
  case 0x5A:
  case 0x5B:
  case 0x5C:
  case 0x5D:
  case 0x5E:
  case 0x5F:
    cpu->reg[op & 7] = pop16(cpu);
    break;

  // --- conditional jumps ---
  case 0x70:
  case 0x71:
  case 0x72:
  case 0x73:
  case 0x74:
  case 0x75:
  case 0x76:
  case 0x77:
  case 0x78:
  case 0x79:
  case 0x7A:
  case 0x7B:
  case 0x7C:
  case 0x7D:
  case 0x7E:
  case 0x7F:
    if (cond_true(cpu->flags, op & 15))
      cpu->ip = (uint16_t)(cpu->ip + insn.imm);
    break;

  // --- the immediate-ALU groups ---
  case 0x80:
  case 0x81:
  case 0x82:
  case 0x83: {
    bool wide = op == 0x81 || op == 0x83;
    I8086AluOp aluOp = (I8086AluOp)((insn.modrm >> 3) & 7);
    uint16_t a = rm_read(cpu, &insn, wide);
    uint16_t r = i8086_alu(aluOp, wide, a, insn.imm, &cpu->flags);
    if (aluOp != I8086_CMP)
      rm_write(cpu, &insn, wide, r);
    break;
  }

  // --- test rm,reg ---
  case 0x84:
  case 0x85: {
    bool wide = (op & 1) != 0;
    uint16_t a = rm_read(cpu, &insn, wide);
    uint16_t b = reg_read(cpu, &insn, wide);
    i8086_alu(I8086_AND, wide, a, b, &cpu->flags);
    break;
  }

  // --- xchg rm,reg ---
  case 0x86:
  case 0x87: {
    bool wide = (op & 1) != 0;
    uint16_t a = rm_read(cpu, &insn, wide);
    uint16_t b = reg_read(cpu, &insn, wide);
    rm_write(cpu, &insn, wide, b);
    reg_write(cpu, &insn, wide, a);
    break;
  }

  // --- mov ---
  case 0x88:
  case 0x89: {
    bool wide = (op & 1) != 0;
    uint16_t v = reg_read(cpu, &insn, wide);
    rm_write(cpu, &insn, wide, v);
    break;
  }
  case 0x8A:
  case 0x8B: {
    bool wide = (op & 1) != 0;
    uint16_t v = rm_read(cpu, &insn, wide);
    reg_write(cpu, &insn, wide, v);
    break;
  }
  case 0x8C: // mov rm16,sreg
    rm_write(cpu, &insn, true, cpu->sreg[(insn.modrm >> 3) & 3]);
    break;
  case 0x8E: // mov sreg,rm16
    cpu->sreg[(insn.modrm >> 3) & 3] = rm_read(cpu, &insn, true);
    break;

  case 0x8D: { // lea: the offset only, with no memory access and no segment
    if (is_reg_operand(&insn))
      return fail_insn(cpu, addr, "lea with a register operand");
    unsigned defSeg;
    reg_write(cpu, &insn, true, effective_offset(cpu, &insn, &defSeg));
    break;
  }

  case 0x8F: // pop rm16
    rm_write(cpu, &insn, true, pop16(cpu));
    break;

  case 0x90: // nop, i.e. xchg ax,ax
    break;
  case 0x91:
  case 0x92:
  case 0x93:
  case 0x94:
  case 0x95:
  case 0x96:
  case 0x97: {
    uint16_t t = cpu->reg[I8086_AX];
    cpu->reg[I8086_AX] = cpu->reg[op & 7];
    cpu->reg[op & 7] = t;
    break;
  }

  case 0x98: // cbw
    cpu->reg[I8086_AX] = (uint16_t)(int16_t)(int8_t)get_reg8(cpu, 0);
    break;
  case 0x99: // cwd
    cpu->reg[I8086_DX] = (cpu->reg[I8086_AX] & 0x8000) ? 0xFFFF : 0x0000;
    break;

  case 0x9C: // pushf
    push16(cpu, cpu->flags);
    break;
  case 0x9D: // popf
    cpu->flags = normalize_flags(pop16(cpu));
    break;
  case 0x9E: // sahf: AH replaces the low byte of FLAGS
    cpu->flags = normalize_flags((uint16_t)((cpu->flags & 0xFF00) | get_reg8(cpu, 4)));
    break;
  case 0x9F: // lahf
    set_reg8(cpu, 4, (uint8_t)cpu->flags);
    break;

  // --- accumulator moves to and from a direct offset ---
  case 0xA0:
  case 0xA1:
  case 0xA2:
  case 0xA3: {
    bool wide = (op & 1) != 0;
    uint32_t target = i8086_linear(cpu->sreg[override_seg(&insn, I8086_DS)], insn.imm);
    if (op < 0xA2) {
      if (wide)
        cpu->reg[I8086_AX] = read16(cpu, target);
      else
        set_reg8(cpu, 0, cpu->read8(cpu->ctx, target));
    } else {
      if (wide)
        write16(cpu, target, cpu->reg[I8086_AX]);
      else
        cpu->write8(cpu->ctx, target, (uint8_t)cpu->reg[I8086_AX]);
    }
    break;
  }

  // --- string instructions ---
  case 0xA4:
  case 0xA5:
  case 0xA6:
  case 0xA7:
  case 0xAA:
  case 0xAB:
  case 0xAC:
  case 0xAD:
  case 0xAE:
  case 0xAF:
    exec_string(cpu, &insn);
    break;

  // --- test acc,imm ---
  case 0xA8:
  case 0xA9: {
    bool wide = (op & 1) != 0;
    uint16_t a = wide ? cpu->reg[I8086_AX] : get_reg8(cpu, 0);
    i8086_alu(I8086_AND, wide, a, insn.imm, &cpu->flags);
    break;
  }

  // --- mov reg,imm ---
  case 0xB0:
  case 0xB1:
  case 0xB2:
  case 0xB3:
  case 0xB4:
  case 0xB5:
  case 0xB6:
  case 0xB7:
    set_reg8(cpu, op & 7, (uint8_t)insn.imm);
    break;
  case 0xB8:
  case 0xB9:
  case 0xBA:
  case 0xBB:
  case 0xBC:
  case 0xBD:
  case 0xBE:
  case 0xBF:
    cpu->reg[op & 7] = insn.imm;
    break;

  // --- returns ---
  case 0xC2: // retn imm16
    cpu->ip = pop16(cpu);
    cpu->reg[I8086_SP] = (uint16_t)(cpu->reg[I8086_SP] + insn.imm);
    break;
  case 0xC3: // retn
    cpu->ip = pop16(cpu);
    break;
  case 0xCA: // retf imm16
    cpu->ip = pop16(cpu);
    cpu->sreg[I8086_CS] = pop16(cpu);
    cpu->reg[I8086_SP] = (uint16_t)(cpu->reg[I8086_SP] + insn.imm);
    break;
  case 0xCB: // retf
    cpu->ip = pop16(cpu);
    cpu->sreg[I8086_CS] = pop16(cpu);
    break;

  // --- les/lds: a far pointer straight out of memory ---
  case 0xC4:
  case 0xC5: {
    if (is_reg_operand(&insn))
      return fail_op(cpu, addr, "les/lds with a register operand", op, -1);
    uint32_t p = effective_addr(cpu, &insn);
    reg_write(cpu, &insn, true, read16(cpu, p));
    cpu->sreg[op == 0xC4 ? I8086_ES : I8086_DS] = read16(cpu, (p + 2) & 0xFFFFF);
    break;
  }

  // --- mov rm,imm ---
  case 0xC6:
  case 0xC7:
    rm_write(cpu, &insn, (op & 1) != 0, insn.imm);
    break;

  case 0xCD: // int imm8
    i8086_interrupt(cpu, (uint8_t)insn.imm);
    break;
  case 0xCF: // iret
    cpu->ip = pop16(cpu);
    cpu->sreg[I8086_CS] = pop16(cpu);
    cpu->flags = normalize_flags(pop16(cpu));
    break;

  // --- shifts and rotates ---
  case 0xD0:
  case 0xD1:
  case 0xD2:
  case 0xD3: {
    unsigned sub = (unsigned)((insn.modrm >> 3) & 7);
    if (sub == 6)
      return fail_op(cpu, addr, "unimplemented shift group", op, (int)sub);
    bool wide = (op & 1) != 0;
    uint8_t count = (op & 2) ? (uint8_t)cpu->reg[I8086_CX] : 1;
    uint16_t a = rm_read(cpu, &insn, wide);
    uint16_t r = i8086_shift((uint8_t)sub, wide, a, count, &cpu->flags);
    rm_write(cpu, &insn, wide, r);
    break;
  }

  case 0xD7: { // xlat: AL = [BX + AL]
    uint16_t off = (uint16_t)(cpu->reg[I8086_BX] + get_reg8(cpu, 0));
    uint32_t p = i8086_linear(cpu->sreg[override_seg(&insn, I8086_DS)], off);
    set_reg8(cpu, 0, cpu->read8(cpu->ctx, p));
    break;
  }

  // --- loop and jcxz ---
  case 0xE0: // loopne
  case 0xE1: // loope
  case 0xE2: { // loop
    uint16_t cx = (uint16_t)(cpu->reg[I8086_CX] - 1);
    cpu->reg[I8086_CX] = cx;
    bool zf = (cpu->flags & I8086_ZF) != 0;
    bool take = cx != 0;
    if (op == 0xE0)
      take = take && !zf;
    else if (op == 0xE1)
      take = take && zf;
    if (take)
      cpu->ip = (uint16_t)(cpu->ip + insn.imm);
    break;
  }
  case 0xE3: // jcxz
    if (cpu->reg[I8086_CX] == 0)
      cpu->ip = (uint16_t)(cpu->ip + insn.imm);
    break;

  // --- ports ---
  case 0xE4: // in al,imm8
    set_reg8(cpu, 0, cpu->in8(cpu->ctx, insn.imm));
    break;
  case 0xE5: // in ax,imm8
    cpu->reg[I8086_AX] = in16(cpu, insn.imm);
    break;
  case 0xE6: // out imm8,al
    cpu->out8(cpu->ctx, insn.imm, get_reg8(cpu, 0));
    break;
  case 0xE7: // out imm8,ax
    out16(cpu, insn.imm, cpu->reg[I8086_AX]);
    break;
  case 0xEC: // in al,dx
    set_reg8(cpu, 0, cpu->in8(cpu->ctx, cpu->reg[I8086_DX]));
    break;
  case 0xED: // in ax,dx
    cpu->reg[I8086_AX] = in16(cpu, cpu->reg[I8086_DX]);
    break;
  case 0xEE: // out dx,al
    cpu->out8(cpu->ctx, cpu->reg[I8086_DX], get_reg8(cpu, 0));
    break;
  case 0xEF: // out dx,ax
    out16(cpu, cpu->reg[I8086_DX], cpu->reg[I8086_AX]);
    break;

  // --- unconditional transfers ---
  case 0xE8: // call rel16
    push16(cpu, cpu->ip);
    cpu->ip = (uint16_t)(cpu->ip + insn.imm);
    break;
  case 0xE9: // jmp rel16
  case 0xEB: // jmp rel8
    cpu->ip = (uint16_t)(cpu->ip + insn.imm);
    break;

  // --- flag manipulation ---
  case 0xF5: // cmc
    cpu->flags ^= I8086_CF;
    break;
  case 0xF8: // clc
    cpu->flags &= (uint16_t)~I8086_CF;
    break;
  case 0xF9: // stc
    cpu->flags |= I8086_CF;
    break;
  case 0xFA: // cli
    cpu->flags &= (uint16_t)~I8086_IF;
    break;
  case 0xFB: // sti
    cpu->flags |= I8086_IF;
    break;
  case 0xFC: // cld
    cpu->flags &= (uint16_t)~I8086_DF;
    break;
  case 0xFD: // std
    cpu->flags |= I8086_DF;
    break;

  // --- the F6/F7 group ---
  case 0xF6:
  case 0xF7: {
    bool wide = (op & 1) != 0;
    unsigned sub = (unsigned)((insn.modrm >> 3) & 7);
    // imul, div and idiv: BOLO uses none of them. Checked before the operand is
    // read, so a failing instruction touches nothing.
    if (sub == 1 || sub >= 5)
      return fail_op(cpu, addr, "unimplemented group", op, (int)sub);
    uint16_t a = rm_read(cpu, &insn, wide);
    switch (sub) {
    case 0: // test rm,imm
      i8086_alu(I8086_AND, wide, a, insn.imm, &cpu->flags);
      break;
    case 2: // not
      rm_write(cpu, &insn, wide, i8086_not(wide, a));
      break;
    case 3: { // neg
      uint16_t r = i8086_neg(wide, a, &cpu->flags);
      rm_write(cpu, &insn, wide, r);
      break;
    }
    case 4: { // mul
      uint16_t acc = wide ? cpu->reg[I8086_AX] : get_reg8(cpu, 0);
      uint32_t product = i8086_mul(wide, acc, a, &cpu->flags);
      cpu->reg[I8086_AX] = (uint16_t)product;
      if (wide)
        cpu->reg[I8086_DX] = (uint16_t)(product >> 16);
      break;
    }
    default:
      return fail_op(cpu, addr, "unimplemented group", op, (int)sub);
    }
    break;
  }

  // --- inc/dec rm8 ---
  case 0xFE: {
    unsigned sub = (unsigned)((insn.modrm >> 3) & 7);
    if (sub > 1)
      return fail_op(cpu, addr, "unimplemented group", op, (int)sub);
    uint16_t a = rm_read(cpu, &insn, false);
    uint16_t r = sub == 0 ? i8086_inc(false, a, &cpu->flags) : i8086_dec(false, a, &cpu->flags);
    rm_write(cpu, &insn, false, r);
    break;
  }

  // --- the FF group ---
  case 0xFF: {
    unsigned sub = (unsigned)((insn.modrm >> 3) & 7);
    switch (sub) {
    case 0: { // inc rm16
      uint16_t r = i8086_inc(true, rm_read(cpu, &insn, true), &cpu->flags);
      rm_write(cpu, &insn, true, r);
      break;
    }
    case 1: { // dec rm16
      uint16_t r = i8086_dec(true, rm_read(cpu, &insn, true), &cpu->flags);
      rm_write(cpu, &insn, true, r);
      break;
    }
    case 2: { // call rm16
      uint16_t target = rm_read(cpu, &insn, true);
      push16(cpu, cpu->ip);
      cpu->ip = target;
      break;
    }
    case 3: { // call far [rm]
      if (is_reg_operand(&insn))
        return fail_op(cpu, addr, "far call through a register", op, (int)sub);
      uint32_t p = effective_addr(cpu, &insn);
      uint16_t off = read16(cpu, p);
      uint16_t seg = read16(cpu, (p + 2) & 0xFFFFF);
      push16(cpu, cpu->sreg[I8086_CS]);
      push16(cpu, cpu->ip);
      cpu->ip = off;
      cpu->sreg[I8086_CS] = seg;
      break;
    }
    case 4: // jmp rm16
      cpu->ip = rm_read(cpu, &insn, true);
      break;
    case 5: { // jmp far [rm]
      if (is_reg_operand(&insn))
        return fail_op(cpu, addr, "far jump through a register", op, (int)sub);
      uint32_t p = effective_addr(cpu, &insn);
      uint16_t off = read16(cpu, p);
      cpu->sreg[I8086_CS] = read16(cpu, (p + 2) & 0xFFFFF);
      cpu->ip = off;
      break;
    }
    case 6: // push rm16
      push16(cpu, rm_read(cpu, &insn, true));
      break;
    default:
      return fail_op(cpu, addr, "unimplemented group", op, (int)sub);
    }
    break;
  }

  default:
    return fail_op(cpu, addr, "unimplemented opcode", op, -1);
  }

  return true;
}
