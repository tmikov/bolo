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

#include "bolo.h"

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

/// Total memory and port writes the guest has performed. The runner uses this
/// to detect a pure-read spin without knowing which loop it is in.
uint64_t machine_write_count(const Machine *m);

/// True once the guest has executed INT 20h.
bool machine_exited(const Machine *m);

/// EGA_PAGE_VISIBLE bytes of `plane` (0..3) from `page` (0 or 1).
///
/// A page is EGA_PAGE_SIZE (8192) bytes but only the first 8000 are displayed;
/// this returns the displayed part, which is what gets compared.
const uint8_t *machine_plane(const Machine *m, int plane, int page);

/// The page the guest is currently drawing into.
///
/// dest_seg_e at 2913:4F8A is an EGA segment (A000h or A200h) and bit 1 of its
/// high byte is a page number -- but it is the page flip_vp is about to make
/// visible, i.e. the one holding the *previous* frame. The guest draws into the
/// other one: clr_alt_box (2913:04CA), first thing in every frame, loads
/// ES with dest_seg_e XOR 0200h, clears the maze area through it and returns
/// without restoring ES, so every draw routine for the rest of the frame writes
/// that page. flip_vp then shows dest_seg_e's page and toggles the bit, which
/// makes the page just drawn the next one shown.
///
/// So this is the complement of bit 1 of 4F8Bh -- verified against the guest:
/// on its very first frame dest_seg_e names page 0 and the freshly drawn maze
/// is in page 1.
int machine_draw_page(const Machine *m);

/// The page currently being displayed, from CRTC register 0Ch.
int machine_display_page(const Machine *m);

/// Make `scanCode` the next byte port 60h returns, and raise INT 09h.
///
/// Unused by the attract-demo comparison -- the demo needs no input -- but the
/// keyboard path is part of the machine and cheap to expose.
void machine_press_key(Machine *m, uint8_t scanCode);

#endif // BOLO_MACHINE_H
