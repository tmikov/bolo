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
  uint16_t reg[8]; ///< Indexed by I8086_AX .. I8086_DI.
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

#endif // BOLO_I8086_H
