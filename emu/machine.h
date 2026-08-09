// bolo - public domain Tzvetan Mikov 2021
//
// A minimal PC/XT: 1MB of memory, EGA planes at A0000, the seven I/O ports the
// original BOLO.COM touches, and the five BIOS/DOS services it calls. Exists so
// the original can be executed alongside the C port and their video memory
// compared -- see docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md
//
// This models what BOLO uses and nothing else. Every unimplemented port,
// interrupt or BIOS function is a hard error naming itself, never a silent
// no-op: a harness that guesses produces a divergence that means nothing.

#ifndef BOLO_MACHINE_H
#define BOLO_MACHINE_H

#include "i8086.h"

#include <stdbool.h>
#include <stdint.h>

#define MACHINE_MEM_SIZE 0x100000

/// The segment Sourcer assumed for BOLO.COM. Using it means every address in
/// disasm/BOLO.LST is directly comparable with this machine's addresses.
#define MACHINE_LOAD_SEG 0x2913

/// Where the EGA planes are mapped.
#define MACHINE_EGA_BASE 0xA0000
#define MACHINE_EGA_WINDOW 0x10000

typedef struct Machine Machine;

Machine *machine_create(void);
void machine_destroy(Machine *m);

/// Load a .COM image at `seg:0100` and set the CPU the way DOS would: all
/// segment registers to `seg`, SP to FFFEh, IP to 0100h, IF set. Seeds the BIOS
/// data area and the interrupt vector table. Returns false on I/O failure.
bool machine_load_com(Machine *m, const char *path, uint16_t seg);

/// The CPU, with bus callbacks already wired to this machine.
I8086 *machine_cpu(Machine *m);

/// Read one byte of guest memory without disturbing EGA latches.
uint8_t machine_peek(const Machine *m, uint32_t linear);

/// Read a little-endian word of guest memory.
uint16_t machine_peek16(const Machine *m, uint32_t linear);

/// Non-NULL once the machine has hit something it does not implement.
const char *machine_error(const Machine *m);

/// True once the guest has executed INT 20h.
bool machine_exited(const Machine *m);

#endif // BOLO_MACHINE_H
