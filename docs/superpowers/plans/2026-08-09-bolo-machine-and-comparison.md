# BOLO Machine and Comparison Driver Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run the original 1993 `disasm/BOLO.COM` under the plan-2 interpreter alongside the C port, compare their EGA planes frame by frame, and report the first tick at which they diverge — ratcheted into a committed baseline.

**Architecture:** `emu/machine.{c,h}` is a minimal XT: 1MB of memory with EGA planes mapped at A0000, the seven I/O ports the original touches, and native servicing of the BIOS/DOS calls it makes. It plugs into the plan-2 CPU purely through the `I8086` bus callbacks. `emu/bolotest.c` grows from a frame dumper into the driver: it boots the original, delivers timer ticks, captures the original's completed frames at `2913:029A`, drives the port through `bolo.h`, and diffs the two.

**Tech Stack:** C11, CMake (Ninja generator), CTest, no new dependencies.

This is plan 3 of 3 from `docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md`:

1. ~~The headless seam~~ — done, `docs/superpowers/plans/2026-08-08-bolo-headless-seam.md`.
2. ~~The 8086 interpreter~~ — done, `docs/superpowers/plans/2026-08-08-bolo-8086-interpreter.md`.
3. **This plan** — the machine, the run loop, the comparison, the ratchet.

## Global Constraints

- C11 (`CMAKE_C_STANDARD 11`), CMake minimum 3.13. **Always configure with `-G Ninja`.**
- **No new external dependencies.** Images are binary PPM (P6) to avoid zlib.
- **Nothing in this plan changes game logic.** `src/bolo.c` is the thing under test; changing it to make the comparison pass would defeat the entire exercise. If the comparison reveals a port bug, **record it, do not fix it here** — fixing is separate, deliberate work, and the baseline number is what tracks it.
- **Never modify `disasm/BOLO.COM`, `disasm/BOLO.LST`, `disasm/BOLO.DEF`, `disasm/BOLO.REM`.**
- **Do not modify `emu/i8086.{c,h}` or `emu/lst.{c,h}`** unless a task here explicitly says to. They are validated against 3439 instructions; changes there invalidate that.
- **New `emu/` targets are compiled with `-Wall -Wextra` and must be warning-free.** GCC is the default `cc` here and is stricter than clang. Verify with a count, not a grep for one word: `cmake --build <dir> 2>&1 | grep -c warning` must print `0`. Add each new target to the `foreach` list in `emu/CMakeLists.txt`.
- Formatting follows `.clang-format`, judged by **exit status** of `clang-format --dry-run -Werror`.
- Project is public domain; no licensed code.
- `emu/` is POSIX-only and excluded from the Emscripten build.

---

## Measured facts this plan is built on

Measured from `disasm/BOLO.LST` and `disasm/BOLO.COM` during planning. Where these differ from the design spec, **these win** — the spec was written before the binary was instrumented this closely.

### The hardware surface is exactly seven ports

| Port | Direction | Use |
| --- | --- | --- |
| `3C4h` | `out dx,ax` | Sequencer index+data. Only ever index 2 (map mask). `ega_map_mask` at `2913:02C1` is the sole writer: `AL=2`, `AH=`the mask. |
| `3CEh` | `out dx,ax` | Graphics controller index+data. Three sites: `AX=0304h` (index 4, read map select = plane 3) at `02DA`; `AX=1003h` (index 3, data rotate/function = OR) at `023E`; `AX=0003h` (index 3, function = replace) at `025B`. |
| `3D4h` | `out dx,ax` | CRTC index+data. One site, inside `flip_vp` at `2913:051E`: `AL=0Ch` (start address high), `AH=0` or `20h`. |
| `3DAh` | `in al,dx` | Input status 1. One site, `2913:0515`: `flip_vp` spins on `test al,8` until bit 3 sets. |
| `60h` | `in al,imm` | Keyboard data, in the `INT 09h` handler at `2913:062E`. |
| `61h` | `in`/`out` | PC speaker gate. Five sites; sound only. |
| `20h` | `out imm,al` | PIC end-of-interrupt, `2913:0644`. Ignorable. |

**Watch the half-register write.** `flip_vp` sets `DX=3DAh`, spins, then does `mov dl,0D4h` — turning DX into `3D4h`. A port scan that tracks only `mov dx,` misses this and wrongly concludes the CRTC is never touched. It is.

### `out dx,ax` to an index port writes index in AL and data in AH

Every EGA write here is the 16-bit index+data form. `out 3CEh, 1003h` means "graphics controller index 3, data `10h`". Data `10h` in the data-rotate register is function = OR (bits 4-3 = `01`); data `00h` is function = replace.

### BIOS/DOS calls: five functions, not the spec's three

| Site | Call | Reached in the compared run? |
| --- | --- | --- |
| `0148`, `014E` | `INT 21h` AH=25h — set interrupt vector 08h, then 09h | **Yes**, during startup |
| `02D2` (`call_int10`) | `INT 10h` AH=00h — set video mode, `AL` from caller (`0Dh` at startup) | **Yes** |
| `0503` (`clear_vp0`) | `INT 10h` AH=05h — select active display page | **Yes** |
| `02A7`, `02AF` | `INT 21h` AH=25h — restore vectors 08h/09h | Only on clean exit |
| `02B9` | `INT 10h` AX=0003h — back to text mode | Only on clean exit |
| `02BD` | `INT 21h` AH=09h — print `$`-terminated string | Only on clean exit / error |
| `02BF` | `INT 20h` — terminate | Only on clean exit |
| `06C2` | `INT 10h` AX=0502h — select page 2 | Help screen only |
| `06F0` | `INT 10h` AH=0Eh — teletype character | Help screen only |

Implement all of them. The exit and help paths are cheap and turn a "harness died mysteriously" into a clear message.

### The program never sets the interrupt flag

There are **zero** `cli` (`FA`) and `sti` (`FB`) instructions among the 3439. DOS hands a `.COM` its FLAGS with IF already set, so the machine must **start FLAGS with IF set** (`I8086_FLAGS_ALWAYS_SET | I8086_IF`). The harness delivers `INT 08h` by calling `i8086_interrupt()` directly and must **not** gate that on IF — but starting with IF clear would still be wrong, because `iret` restores the pushed FLAGS and the state would be observable.

### Startup preconditions the program checks

1. `2913:0100  mov ax,sp / cmp ax,555Ah / jae` — **SP must be ≥ 555Ah** or it prints "Not enough memory" and exits. `SP = FFFEh` satisfies this.
2. `2913:0111  cmp ds:video_options_,cl` with `DS = 0` — reads `0000:0487` and requires it **nonzero** ("EGA/VGA required"). Seed it with `60h`, the value the LST records.
3. `2913:011D  mov si,0020h / mov di,4F8Dh / mov cl,4 / rep movsw` — copies **4 words from `0000:0020`**, i.e. the `INT 08h` and `INT 09h` vectors, into `orig_int8_e`/`orig_int9_e`. Those vectors must point at something real, because `int_08h_entry` chains to the saved one:
   ```
   2913:02C8  inc cs:time_tick
   2913:02CD  jmp dword ptr cs:orig_int8_e
   ```
   So the machine must plant a **stub original INT 08h handler** the guest can jump into and return from. `out 20h,al` + `iret` (bytes `B0 20 E6 20 CF`) in unused low memory is enough.
4. `2913:0132`/`0138` read `0000:046C` (the BIOS 18.2Hz tick counter) to seed `rnd_state` and `time_5bit_e`. Fix it at 0 — the values are overwritten at the sync point.

### Determinism, and why the sync point is reached

`title_screen` (`2913:1194`) ends with:

```
11D9  mov bl,time_tick
11DD  add bl,3Ch          ; deadline = time_tick + 60
11E2  mov kbdin_key_e,0
11E5  cmp kbdin_key_e,al  ; a key arrived?
11E9  jne loc_107         ;   yes -> leave lastkey_tick alone
11EB  cmp bl,time_tick
11EF  jae loc_106         ; spin until time_tick passes the deadline
11F1  mov lastkey_tick,bl ; TIMED OUT: lastkey_tick becomes nonzero
```

Then at `2913:0185`:

```
0185  cmp lastkey_tick,ch  ; ch = 0
0189  je  loc_6            ; lastkey_tick == 0 -> SKIP the reset
018B  mov di,offset rnd_state ; rnd_state[i] = i  <-- determinism starts here
```

No keys are injected, so the title screen times out, `lastkey_tick` becomes `3Ch`, the `je` is **not** taken, and `018B` runs. That is what makes the attract demo reproducible. (Had a key been pressed, `lastkey_tick` would still be 0, the reset would be skipped, and `rnd_state` would keep its BIOS-seeded values.)

### **The spec's tick-delivery rule is incomplete — this is the plan's one real design decision**

The spec says: *"fire `INT 08h` when the guest is spinning at `028E`"*. That is the **gameplay** frame gate. But the title screen spins at `11E5`, waiting on the very same `time_tick`. Deliver ticks only at `028E` and the guest **never leaves the title screen** — the harness hangs before it reaches the code it exists to compare.

**Resolution: deliver a tick whenever the guest goes idle, where idle is defined generically as "N consecutive instructions with no memory write and no port write."** Both wait loops qualify — `028E` is `mov`/`cmp`/`je` and `11E5` is `cmp`/`jne`/`cmp`/`jae`, neither writing anything — and no real work loop in this program runs 64 instructions without storing something. This needs no hardcoded addresses, no cycle counting, and no per-phase special-casing.

Two consequences to accept deliberately:

- `flip_vp`'s `3DAh` retrace spin also writes nothing, so it can trip the idle detector. Harmless: the handler increments `time_tick` and returns, and modelling `3DAh` bit 3 as *always set* (below) makes that spin exit on its first read anyway.
- The count of ticks consumed during the intro is then whatever the guest asks for — and it **self-aligns with the port**, because both run the same `deadline = time_tick + 3Ch` logic from `time_tick = 0`. Neither side needs a tick budget.

### `3DAh` bit 3 should read as always set

`flip_vp` spins `in al,3DAh / test al,8 / jz`. The spec suggests alternating the bit per read. **Return it always set instead.** The spin then exits immediately and deterministically, there is no retrace timing to model, and every frame is still complete because the page flip that follows is what matters. Alternating adds a second state variable and a wasted iteration for no fidelity gain.

### Frame alignment — the correction that must not be lost

`bolo_frame_count()` increments where the original calls `flip_vp` (`2913:029A`), **but control does not return there**: `async_start` case 12 falls through and draws an entirely new frame into page 0 before yielding. So the port's planes at `bolo_frame_count() == k` correspond to the original's capture **k+1**, not k. This is documented in `src/bolo.h`'s `bolo_plane()` comment. An earlier spec draft had it backwards.

### What the original draws into, and what to capture

`dest_seg_e` at `2913:4F8A` holds the **segment** being drawn into (`A000h`, set at `04F5`), and `flip_vp` tests **bit 1 of its high byte** (`dest_seg_e+1`, i.e. `4F8Bh`) to choose the CRTC start-address-high value of `0` or `20h`. Read the page being drawn out of guest memory at `4F8Bh` at capture time rather than tracking it separately. `time_tick` at `2F67h` labels the frame.

The port pins `dest_seg` to 0 and `bolo_plane()` returns page 0 always. **The original will genuinely be double-buffering while the port is not.** Compare the original's *drawn* page (the one named by `dest_seg_e`) against the port's page 0.

---

## File Structure

**Created:**

| File | Responsibility |
| --- | --- |
| `emu/machine.h`, `emu/machine.c` | The XT: memory, BIOS data, EGA planes and registers, seven ports, BIOS/DOS services, the loader. Owns no policy about *when* to run. |
| `emu/test_machine.c` | Unit tests for the EGA write/read paths and the port registers, with no guest involved. |
| `emu/runner.h`, `emu/runner.c` | The original-side run loop: idle detection, tick delivery, capture at `029A`, runaway guards. Separated from `bolotest.c` so it is testable and so the CLI stays argument parsing plus reporting. |
| `emu/test_runner.c` | Boots the real `BOLO.COM` to the title screen and to the sync point; asserts the guest got there. |
| `emu/baseline.txt` | The ratchet: the highest tick count known to match. |

**Modified:**

| File | Change |
| --- | --- |
| `emu/bolotest.c` | Gains a `--compare` mode: run both sides, diff planes, write artifacts, enforce the baseline. The existing frame-dumping behavior stays as the default. |
| `emu/CMakeLists.txt` | New library and test targets. |
| `CLAUDE.md` | A short section on the comparison harness and how to read a divergence. |

---

## Interface summary

Written once here; each task repeats what it needs.

```c
/* emu/machine.h */
typedef struct Machine Machine;

Machine *machine_create(void);
void machine_destroy(Machine *m);

/// Load a .COM image at `seg:0100` and set the CPU up the way DOS would.
bool machine_load_com(Machine *m, const char *path, uint16_t seg);

/// The CPU, with its bus callbacks already wired to this machine.
I8086 *machine_cpu(Machine *m);

/// The EGA plane bytes for `page` (0 or 1). EGA_PAGE_VISIBLE bytes.
const uint8_t *machine_plane(const Machine *m, int plane, int page);

/// The page the guest is currently drawing into, read from dest_seg_e.
int machine_draw_page(const Machine *m);

/// Guest memory, for reading variables like time_tick.
uint8_t machine_peek(const Machine *m, uint32_t linear);

/// Set by the machine on an unimplemented port/service; the driver aborts.
const char *machine_error(const Machine *m);

/* emu/runner.h */
typedef struct Runner Runner;

Runner *runner_create(const char *comPath);
void runner_destroy(Runner *r);

/// Run until the guest completes its next frame (reaches 2913:029A) or fails.
/// Delivers timer ticks whenever the guest goes idle.
typedef enum RunResult { RUN_FRAME, RUN_SYNCED, RUN_ERROR, RUN_EXITED } RunResult;
RunResult runner_run_to_frame(Runner *r);

/// Run until the guest reaches the determinism sync point (2913:018B).
RunResult runner_run_to_sync(Runner *r);

const uint8_t *runner_plane(const Runner *r, int plane);
unsigned runner_frame_count(const Runner *r);
uint8_t runner_time_tick(const Runner *r);
const char *runner_error(const Runner *r);
```

---

## Guest address reference

| Address | What |
| --- | --- |
| `2913:0100` | Entry; `mov ax,sp / cmp ax,555Ah` memory check |
| `2913:018B` | Determinism sync point — `rnd_state[i] = i` |
| `2913:028E` | Gameplay frame gate — spins on `time_tick` vs `last_tick` |
| `2913:029A` | `call flip_vp` — **the capture trigger** |
| `2913:02C8` | `int_08h_entry` — increments `time_tick`, chains to the saved vector |
| `2913:0506` | `flip_vp` |
| `2913:2F67` | `time_tick` (byte) |
| `2913:4F8A` | `dest_seg_e` (word); bit 1 of `4F8Bh` selects the page |
| `2913:4F8C` | `last_tick` (byte) |
| `2913:0973` | `lastkey_tick` (byte) |

Load segment: use **`2913h`**, the segment Sourcer assumed, so every address in `BOLO.LST` is directly usable in a debugger session and in error messages. Code then occupies linear `0x29230`..`0x2B083` — nowhere near the `A0000` EGA window, so an instruction fetch can never disturb the latches.

---

### Task 1: The machine — memory, BIOS data, and the loader

Get the original booting far enough to prove the startup checks pass. No EGA yet: writes to the `A0000` window land in plain RAM for now, and unknown ports are hard errors.

**Files:**
- Create: `emu/machine.h`, `emu/machine.c`, `emu/test_machine.c`
- Modify: `emu/CMakeLists.txt`

**Interfaces:**
- Consumes: `I8086`, `i8086_reset`, `i8086_step`, `i8086_interrupt`, `i8086_linear` from `emu/i8086.h`.
- Produces: `Machine`, `machine_create`, `machine_destroy`, `machine_load_com`, `machine_cpu`, `machine_peek`, `machine_error`.

- [ ] **Step 1: Write `emu/machine.h`**

```c
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
```

- [ ] **Step 2: Write the failing test `emu/test_machine.c`**

```c
// Unit tests for the machine: the loader, the BIOS data area, and (in Task 2)
// the EGA write and read paths.
//
// These run without the guest wherever possible -- a test that needs BOLO.COM
// to fail is a test that cannot tell you which of ten things broke.

#include "machine.h"

#include <stdio.h>
#include <string.h>

static int g_failures;

static void expect_u8(const char *what, uint8_t got, uint8_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-40s got %02X, expected %02X\n", what, got, want);
  ++g_failures;
}

static void expect_u16(const char *what, uint16_t got, uint16_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-40s got %04X, expected %04X\n", what, got, want);
  ++g_failures;
}

int main(void) {
  Machine *m = machine_create();
  if (!m) {
    fprintf(stderr, "FAIL: machine_create returned NULL\n");
    return 1;
  }

  if (!machine_load_com(m, BOLO_COM_PATH, MACHINE_LOAD_SEG)) {
    fprintf(stderr, "FAIL: machine_load_com\n");
    return 1;
  }

  I8086 *cpu = machine_cpu(m);

  // --- DOS-like startup state ---
  expect_u16("CS", cpu->sreg[I8086_CS], MACHINE_LOAD_SEG);
  expect_u16("DS", cpu->sreg[I8086_DS], MACHINE_LOAD_SEG);
  expect_u16("ES", cpu->sreg[I8086_ES], MACHINE_LOAD_SEG);
  expect_u16("SS", cpu->sreg[I8086_SS], MACHINE_LOAD_SEG);
  expect_u16("IP", cpu->ip, 0x0100);
  // SP must be >= 555Ah or the program prints "Not enough memory" and exits.
  expect_u16("SP", cpu->reg[I8086_SP], 0xFFFE);
  // DOS hands a .COM its FLAGS with IF set, and BOLO never executes cli or sti.
  if (!(cpu->flags & I8086_IF)) {
    fprintf(stderr, "FAIL: IF must start set\n");
    ++g_failures;
  }

  // --- the image landed at seg:0100 ---
  // BOLO.COM begins "8B C4" -- mov ax,sp.
  expect_u8("image[0]", machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x0100)), 0x8B);
  expect_u8("image[1]", machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x0101)), 0xC4);

  // --- BIOS data the program checks before it will run ---
  // 0000:0487 must be nonzero or it prints "EGA/VGA required".
  expect_u8("video options 0:0487", machine_peek(m, 0x0487), 0x60);
  // 0000:046C is the 18.2Hz tick counter, fixed at 0 for reproducibility.
  expect_u16("timer low 0:046C", machine_peek16(m, 0x046C), 0x0000);

  // --- the INT 08h/09h vectors must point at something real, because
  //     int_08h_entry chains to the saved INT 08h with a far jmp ---
  uint16_t int8Off = machine_peek16(m, 8 * 4);
  uint16_t int8Seg = machine_peek16(m, 8 * 4 + 2);
  uint16_t int9Off = machine_peek16(m, 9 * 4);
  uint16_t int9Seg = machine_peek16(m, 9 * 4 + 2);
  if ((int8Seg == 0 && int8Off == 0) || (int9Seg == 0 && int9Off == 0)) {
    fprintf(stderr, "FAIL: INT 08h/09h vectors must not be null\n");
    ++g_failures;
  }
  // The stub they point at must return: an iret (CFh) has to be reachable
  // within a few bytes.
  {
    uint32_t at = i8086_linear(int8Seg, int8Off);
    bool sawIret = false;
    for (unsigned i = 0; i != 8; ++i)
      if (machine_peek(m, at + i) == 0xCF)
        sawIret = true;
    if (!sawIret) {
      fprintf(stderr, "FAIL: the INT 08h stub never reaches an iret\n");
      ++g_failures;
    }
  }

  // --- the startup checks actually pass: run to the EGA check and past it ---
  // 0100 mov ax,sp / 0102 cmp ax,555Ah / 0105 jae loc_1 -> 010D
  for (unsigned i = 0; i != 3; ++i) {
    if (!i8086_step(cpu)) {
      fprintf(stderr, "FAIL: step %u: %s\n", i, cpu->error ? cpu->error : "?");
      ++g_failures;
      break;
    }
  }
  expect_u16("took the jae (memory check passed)", cpu->ip, 0x010D);

  machine_destroy(m);

  if (g_failures) {
    fprintf(stderr, "\n%d machine check(s) failed\n", g_failures);
    return 1;
  }
  printf("machine ok\n");
  return 0;
}
```

- [ ] **Step 3: Add the targets and run to see it fail**

Add to `emu/CMakeLists.txt`, after the `test_exec` block:

```cmake
add_library(bolo_machine STATIC machine.c)
target_include_directories(bolo_machine PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(bolo_machine bolo_i8086)
target_compile_definitions(bolo_machine PUBLIC
    BOLO_COM_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.COM")

add_executable(test_machine test_machine.c)
target_link_libraries(test_machine bolo_machine)
add_test(NAME machine COMMAND test_machine)
```

and add `bolo_machine test_machine` to the warnings `foreach` list.

Note that `bolo_lst` already defines `BOLO_COM_PATH` as a PUBLIC compile definition. No target links both `bolo_lst` and `bolo_machine` today, and the two definitions are the same string, so this is harmless — but if a future target does link both and the compiler objects to the duplicate `-D`, move the definition to a shared interface library rather than deleting it from one side.

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -5
```

Expected: FAIL at configure — `Cannot find source file: machine.c`.

- [ ] **Step 4: Write `emu/machine.c` (memory and loader only)**

Structure:

```c
struct Machine {
  I8086 cpu;
  uint8_t mem[MACHINE_MEM_SIZE];
  char errorBuf[128];
  bool hasError;
  bool exited;
  /* EGA state arrives in Task 2, ports and services in Task 3. */
};
```

Requirements:

- `machine_create` allocates and zeroes (1MB — use `calloc`, not a stack array).
- Bus callbacks: `read8`/`write8` index `mem` after masking to 20 bits. **Route the `A0000` window through a helper even now** (`ega_read`/`ega_write`) so Task 2 has one place to change; for now the helper is a plain array access.
- `in8`/`out8` and `intercept` set a hard error naming the port/vector and return. Task 3 fills them in. Write the error with `snprintf` into `errorBuf` so the message can name the value, and set `cpu.error` to `errorBuf` so `i8086_step` stops.
- `machine_load_com`: `fopen`/`fread` the image (it is 11912 bytes; reject anything that would not fit below the load segment), copy to `linear(seg, 0x0100)`, then:
  - `i8086_reset(&m->cpu)`; set `sreg[CS]=sreg[DS]=sreg[ES]=sreg[SS]=seg`, `ip=0x0100`, `reg[SP]=0xFFFE`, and `cpu.flags |= I8086_IF`.
  - Seed BIOS data: `mem[0x0487] = 0x60`; `mem[0x046C..0x046F] = 0`.
  - Plant the INT 08h/09h stub. Put it somewhere harmless and fixed — `0000:0500` is inside the BIOS data area's unused tail and far from both the IVT and the guest. Write `B0 20 E6 20 CF` (`mov al,20h / out 20h,al / iret`), then point vectors 8 and 9 at it: `mem[8*4] = 0x00; mem[8*4+1] = 0x05; mem[8*4+2..3] = 0` and the same for vector 9.
  - Set `cpu.ctx = m` and install the callbacks. (`i8086_reset` deliberately leaves callbacks alone, so either order works — do it explicitly anyway.)
- `machine_peek`/`machine_peek16` read `mem` directly, **bypassing the EGA path**, so a test or the driver can inspect guest variables without loading latches.

- [ ] **Step 5: Run the test**

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build 2>&1 | tail -3
cd build && ctest --output-on-failure -R machine; cd ..
```

Expected: `machine ok`.

If "took the jae" fails with `cpu->ip == 0x0107`, the memory check failed — `SP` is below `555Ah`.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/machine.c emu/machine.h emu/test_machine.c && echo clean
git add emu/machine.c emu/machine.h emu/test_machine.c emu/CMakeLists.txt
git commit -m "Add the machine: memory, BIOS data area and the COM loader"
```

---

### Task 2: EGA planes

Model mode 0Dh planar memory the way the original uses it. This is the part the whole comparison rests on: if the write path is wrong, every frame differs and the tool reports garbage.

**Files:**
- Modify: `emu/machine.h`, `emu/machine.c`, `emu/test_machine.c`

**Interfaces:**
- Produces: `machine_plane(const Machine *m, int plane, int page)`, `machine_draw_page(const Machine *m)`, and the EGA register state behind `out8`.

#### The EGA subset to implement

Only what the original touches. Registers and their effect on a **write** to `A000:offset`:

- **Sequencer index 2 — map mask** (`3C4h`). Low 4 bits: which planes the write lands in. A plane whose bit is clear keeps its existing byte. This is the only sequencer register used.
- **Graphics controller index 3 — data rotate / function** (`3CEh`). Bits 4-3 select the function applied between the CPU byte and the **read latch** for that plane: `00` replace, `01` AND, `10` OR, `11` XOR. Bits 2-0 are a rotate count — the original always writes 0 there, so implement rotate as well (it is three lines) but expect it unused.
- **Graphics controller index 4 — read map select** (`3CEh`). Which single plane a **read** returns. The original sets it to 3 once, at startup, and never changes it — which is exactly why the port's `ega_read()` returns plane 3 only.
- **CRTC index `0Ch` — start address high** (`3D4h`). `0` or `20h`, selecting which `0x2000`-byte page is displayed.

**Read behavior:** a read from the window returns `plane[readMapSelect][offset]` **and loads all four latches** with `plane[i][offset]`. Loading the latches on every read is not optional — the read-modify-write sequences in the original depend on it, and the plan-2 executor was deliberately built so that a read-modify-write instruction issues a real bus read before its write.

**Write behavior:**

```
for each plane p in 0..3:
    if !(mapMask & (1 << p)): continue
    v = rotate_right(cpuByte, rotateCount)
    switch (function):
      case 0: result = v;                       break  // replace
      case 1: result = v & latch[p];            break
      case 2: result = v | latch[p];            break
      case 3: result = v ^ latch[p];            break
    plane[p][offset] = result
```

- [ ] **Step 1: Extend `emu/machine.h`**

Add before the closing `#endif`:

```c
/// EGA_PAGE_VISIBLE bytes of `plane` (0..3) from `page` (0 or 1).
///
/// A page is EGA_PAGE_SIZE (8192) bytes but only the first 8000 are displayed;
/// this returns the displayed part, which is what gets compared.
const uint8_t *machine_plane(const Machine *m, int plane, int page);

/// The page the guest is currently drawing into, decoded from dest_seg_e at
/// 2913:4F8A: flip_vp tests bit 1 of its high byte to choose the CRTC start
/// address, so bit 1 of 4F8Bh is the page number.
int machine_draw_page(const Machine *m);

/// The page currently being displayed, from CRTC register 0Ch.
int machine_display_page(const Machine *m);
```

`machine.h` includes `i8086.h` but not `bolo.h`; add `#include "bolo.h"` for `EGA_PLANES`, `EGA_PAGE_SIZE` and `EGA_PAGE_VISIBLE` rather than redefining them, and link `bolo_machine` against `bologame` in CMake. Reusing those constants is the point — the two sides must agree on the geometry by construction, not by coincidence.

- [ ] **Step 2: Add the failing EGA tests to `emu/test_machine.c`**

Append before the final `machine_destroy`. These drive the EGA through the **CPU's own bus callbacks**, not through a private back door, so they test the path the guest actually uses.

```c
  // ---------------------------------------------------------------- EGA ----
  //
  // Drive the registers exactly as the original does: an "out dx,ax" to an
  // index port writes the index in AL and the data in AH.
  {
    I8086 *c = machine_cpu(m);
    void *ctx = c->ctx;

    // Helper shorthand: the 16-bit index+data write the original always uses.
    #define OUT16(port, ax)                          \
      do {                                           \
        c->out8(ctx, (port), (uint8_t)((ax)&0xFF));  \
        c->out8(ctx, (port) + 1, (uint8_t)((ax) >> 8)); \
      } while (0)

    // Map mask = all four planes, function = replace.
    OUT16(0x3C4, 0x0F02);
    OUT16(0x3CE, 0x0003);

    c->write8(ctx, MACHINE_EGA_BASE + 0, 0xA5);
    for (int p = 0; p != EGA_PLANES; ++p)
      expect_u8("write to all planes", machine_plane(m, p, 0)[0], 0xA5);

    // Map mask = plane 1 only: the others must keep their value.
    OUT16(0x3C4, 0x0202);
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0x3C);
    expect_u8("masked write, plane 0 untouched", machine_plane(m, 0, 0)[0], 0xA5);
    expect_u8("masked write, plane 1 written", machine_plane(m, 1, 0)[0], 0x3C);
    expect_u8("masked write, plane 2 untouched", machine_plane(m, 2, 0)[0], 0xA5);

    // Reads return only the plane named by read map select, and load all four
    // latches. Select plane 3 -- the value the original uses throughout.
    OUT16(0x3CE, 0x0304);
    expect_u8("read returns read-map-select plane", c->read8(ctx, MACHINE_EGA_BASE + 0), 0xA5);

    // The OR function combines the CPU byte with the latch loaded by that read.
    // Latches now hold A5/3C/A5/A5. Write 0x0F to all planes with function=OR.
    OUT16(0x3C4, 0x0F02);
    OUT16(0x3CE, 0x1003); // index 3, data 10h -> function OR
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0x0F);
    expect_u8("OR against latch, plane 0", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 | 0x0F));
    expect_u8("OR against latch, plane 1", machine_plane(m, 1, 0)[0], (uint8_t)(0x3C | 0x0F));

    // XOR, the other function the game uses for erasing. Note the expected
    // value is computed against the LATCH (still A5h from the read above), not
    // against the AFh the OR write stored: only a read reloads the latches, so
    // they are unchanged by the intervening write.
    OUT16(0x3CE, 0x1803); // function 11b -> XOR
    c->write8(ctx, MACHINE_EGA_BASE + 0, 0xFF);
    expect_u8("XOR against latch, plane 0", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 ^ 0xFF));

    // Back to replace so later tests are not surprised.
    OUT16(0x3CE, 0x0003);

    // The two pages are distinct storage.
    OUT16(0x3C4, 0x0F02);
    c->write8(ctx, MACHINE_EGA_BASE + EGA_PAGE_SIZE, 0x77);
    expect_u8("page 1 written", machine_plane(m, 0, 1)[0], 0x77);
    expect_u8("page 0 unaffected", machine_plane(m, 0, 0)[0], (uint8_t)(0xA5 ^ 0xFF));

    // CRTC start address high selects the displayed page: 0 or 20h.
    OUT16(0x3D4, 0x000C);
    expect_u8("display page 0", (uint8_t)machine_display_page(m), 0);
    OUT16(0x3D4, 0x200C);
    expect_u8("display page 1", (uint8_t)machine_display_page(m), 1);

    // 3DAh bit 3 reads as always set, so flip_vp's retrace spin exits at once.
    if (!(c->in8(ctx, 0x3DA) & 0x08)) {
      fprintf(stderr, "FAIL: 3DAh bit 3 must read set\n");
      ++g_failures;
    }
    #undef OUT16
  }
```

- [ ] **Step 3: Run to see the EGA tests fail**

```bash
cmake --build build 2>&1 | tail -3 && cd build && ctest --output-on-failure -R machine; cd ..
```

Expected: FAIL — `machine_plane` and `machine_display_page` are undeclared, or the port writes hit Task 1's hard-error path.

- [ ] **Step 4: Implement the EGA in `emu/machine.c`**

Add to `struct Machine`:

```c
  /// Two pages of four planes. EGA_PAGE_SIZE bytes each, of which the first
  /// EGA_PAGE_VISIBLE are displayed.
  uint8_t plane[EGA_PLANES][EGA_PAGE_SIZE * 2];
  uint8_t latch[EGA_PLANES];
  uint8_t mapMask;        ///< sequencer index 2, low 4 bits
  uint8_t readMapSelect;  ///< graphics index 4
  uint8_t dataRotate;     ///< graphics index 3: bits 4-3 function, 2-0 rotate
  uint8_t crtcStartHigh;  ///< CRTC index 0Ch
  uint8_t seqIndex, gfxIndex, crtcIndex;
```

Write the read and write paths exactly as specified in the preamble above. Two details that are easy to get wrong:

- **Offset masking.** The window is 64KB but only `EGA_PAGE_SIZE * 2` is backed. Mask the offset with `(MACHINE_EGA_WINDOW - 1)` and then reject or wrap anything beyond `EGA_PAGE_SIZE * 2` — the original never goes there, so make it a hard error rather than silent wrapping, and you will find out immediately if that assumption is wrong.
- **The latches load on every read**, including reads the guest makes for reasons unrelated to drawing.

Initialize `mapMask = 0x0F`, `dataRotate = 0`, `readMapSelect = 0`, `crtcStartHigh = 0` in `machine_create`.

`machine_draw_page` reads `dest_seg_e+1` out of guest memory:

```c
int machine_draw_page(const Machine *m) {
  // dest_seg_e is at 2913:4F8A; flip_vp tests bit 1 of its high byte.
  uint8_t high = machine_peek(m, i8086_linear(MACHINE_LOAD_SEG, 0x4F8B));
  return (high & 0x02) ? 1 : 0;
}
```

- [ ] **Step 5: Run the tests**

Expected: `machine ok`.

- [ ] **Step 6: Check formatting and commit**

```bash
clang-format --dry-run -Werror emu/machine.c emu/machine.h emu/test_machine.c && echo clean
git add emu/machine.c emu/machine.h emu/test_machine.c emu/CMakeLists.txt
git commit -m "Add EGA plane emulation to the machine"
```

---

### Task 3: Ports and BIOS services

Fill in the remaining ports and the five BIOS/DOS functions, so the original can boot without hitting a hard error.

**Files:**
- Modify: `emu/machine.c`, `emu/machine.h`, `emu/test_machine.c`

**Interfaces:**
- Produces: complete `in8`/`out8`/`intercept` handling; `machine_exited`; `machine_set_key`.

- [ ] **Step 1: Add the key-injection entry point to `emu/machine.h`**

```c
/// Make `scanCode` the next byte port 60h returns, and raise INT 09h.
///
/// Unused by the attract-demo comparison -- the demo needs no input -- but the
/// keyboard path is part of the machine and cheap to expose.
void machine_press_key(Machine *m, uint8_t scanCode);
```

- [ ] **Step 2: Write the failing tests**

Append to `emu/test_machine.c`:

```c
  // -------------------------------------------------------------- ports ----
  {
    I8086 *c = machine_cpu(m);
    void *ctx = c->ctx;

    // Port 61h is the speaker gate: writes are recorded, reads return the last
    // value written. Sound is out of scope for the comparison, but the port
    // must not be an error.
    c->out8(ctx, 0x61, 0x03);
    expect_u8("speaker port round trip", c->in8(ctx, 0x61) & 0x03, 0x03);

    // Port 20h is the PIC end-of-interrupt: accepted and ignored.
    c->out8(ctx, 0x20, 0x20);
    if (machine_error(m)) {
      fprintf(stderr, "FAIL: EOI to port 20h raised \"%s\"\n", machine_error(m));
      ++g_failures;
    }

    // Port 60h returns the injected scan code.
    machine_press_key(m, 0x39); // space
    expect_u8("keyboard data port", c->in8(ctx, 0x60), 0x39);
  }

  // ------------------------------------------------------ BIOS services ----
  {
    I8086 *c = machine_cpu(m);

    // INT 21h AH=25h sets an interrupt vector from DS:DX. This is how BOLO
    // installs its own INT 08h and INT 09h handlers, so it must actually write
    // the guest's IVT -- the CPU later vectors through it.
    c->reg[I8086_AX] = 0x2508;
    c->sreg[I8086_DS] = MACHINE_LOAD_SEG;
    c->reg[I8086_DX] = 0x02C8; // int_08h_entry
    if (!c->intercept(c->ctx, 0x21)) {
      fprintf(stderr, "FAIL: INT 21h AH=25h was not serviced\n");
      ++g_failures;
    }
    expect_u16("IVT 8 offset", machine_peek16(m, 8 * 4), 0x02C8);
    expect_u16("IVT 8 segment", machine_peek16(m, 8 * 4 + 2), MACHINE_LOAD_SEG);

    // INT 10h AH=00h sets the video mode; BOLO asks for 0Dh.
    c->reg[I8086_AX] = 0x000D;
    if (!c->intercept(c->ctx, 0x10)) {
      fprintf(stderr, "FAIL: INT 10h AH=00h was not serviced\n");
      ++g_failures;
    }

    // INT 10h AH=05h selects the active display page.
    c->reg[I8086_AX] = 0x0501;
    if (!c->intercept(c->ctx, 0x10)) {
      fprintf(stderr, "FAIL: INT 10h AH=05h was not serviced\n");
      ++g_failures;
    }
    expect_u8("INT 10h AH=05h set page 1", (uint8_t)machine_display_page(m), 1);

    // INT 20h terminates.
    if (!c->intercept(c->ctx, 0x20)) {
      fprintf(stderr, "FAIL: INT 20h was not serviced\n");
      ++g_failures;
    }
    if (!machine_exited(m)) {
      fprintf(stderr, "FAIL: INT 20h did not mark the machine exited\n");
      ++g_failures;
    }
  }

  // An unimplemented service must be a hard error, never a silent success.
  {
    Machine *m2 = machine_create();
    machine_load_com(m2, BOLO_COM_PATH, MACHINE_LOAD_SEG);
    I8086 *c2 = machine_cpu(m2);
    c2->reg[I8086_AX] = 0x4C00; // INT 21h AH=4Ch, which BOLO never calls
    c2->intercept(c2->ctx, 0x21);
    if (!machine_error(m2)) {
      fprintf(stderr, "FAIL: an unimplemented DOS function must set an error\n");
      ++g_failures;
    }
    machine_destroy(m2);
  }
```

- [ ] **Step 3: Run to see them fail**, then implement.

- [ ] **Step 4: Implement ports and services in `emu/machine.c`**

**Ports.** `out8`:

| Port | Behavior |
| --- | --- |
| `3C4h` | index latch. `3C5h`: if index == 2, `mapMask = value & 0x0F`; other indices are a hard error. |
| `3CEh` | index latch. `3CFh`: index 3 → `dataRotate = value`; index 4 → `readMapSelect = value & 3`; others hard error. |
| `3D4h` | index latch. `3D5h`: index `0Ch` → `crtcStartHigh = value`; others hard error. |
| `3DAh` | Feature control on write — accept and ignore. |
| `61h` | store in `speakerPort`. |
| `20h` | accept and ignore (EOI). |
| anything else | hard error naming the port and value. |

`in8`: `60h` returns the pending scan code; `61h` returns `speakerPort`; `3DAh` returns `0x08` (bit 3 always set — see the measured facts); anything else is a hard error.

Note the guest reaches the data ports only via the 16-bit `out dx,ax` form, which the plan-2 executor decomposes into `out8(port, AL)` then `out8(port+1, AH)`. That is exactly why the index/data split above works.

**Services** (`intercept`, returning true when handled):

- `INT 10h`: `AH=00h` set mode (accept `0Dh` and `03h`; anything else a hard error naming the mode); `AH=05h` set display page from `AL` (also update `crtcStartHigh` so `machine_display_page` agrees); `AH=0Eh` teletype — write `AL` to `stdout`; anything else a hard error.
- `INT 21h`: `AH=25h` set vector `AL` to `DS:DX`, writing the guest IVT; `AH=09h` print the `$`-terminated string at `DS:DX` to `stdout`; anything else a hard error.
- `INT 20h`: set `exited`, return true.
- Any other vector: **return false**, so it vectors through the guest IVT. That is what makes BOLO's own `INT 08h`/`INT 09h` handlers reachable — the harness raises those with `i8086_interrupt()` and they must go through the guest's table.

- [ ] **Step 5: Run the tests, check formatting, and commit**

```bash
git add emu/machine.c emu/machine.h emu/test_machine.c
git commit -m "Add the seven I/O ports and the BIOS/DOS services BOLO uses"
```

---

### Task 4: The run loop — boot the original to the title screen

Make the original actually run. This is the milestone where you look at a picture and see BOLO.

**Files:**
- Create: `emu/runner.h`, `emu/runner.c`, `emu/test_runner.c`
- Modify: `emu/machine.h`, `emu/machine.c` (add the write counter), `emu/CMakeLists.txt`, `emu/bolotest.c`

**Interfaces:**
- Consumes: everything from `machine.h`.
- Produces: `Runner`, `runner_create`, `runner_destroy`, `runner_run_to_sync`, `runner_run_to_frame`, `runner_plane`, `runner_frame_count`, `runner_time_tick`, `runner_error`; `bolotest --original DIR`.

#### The run loop

The design decision from the measured facts, restated because it is the heart of this task:

**Deliver `INT 08h` whenever the guest goes idle**, where idle means *N consecutive instructions with no memory write and no port write*. The spec's "fire at `028E`" is the gameplay gate only; the title screen waits on `time_tick` at `11E5` and would spin forever. A generic idle detector covers both without hardcoding either address.

Use `N = 64`. The two wait loops are 3 and 4 instructions of pure reads; no real work loop in this program goes 64 instructions without a store.

```c
/// Instructions with no store before the guest is considered idle.
#define IDLE_THRESHOLD 64

/// Instructions to run in one frame before declaring the guest hung. BOLO does
/// roughly 250k per frame; 5M is a factor of 20 of headroom.
#define FRAME_INSN_BUDGET 5000000
```

The machine must count stores. Add a `writeCount` to `Machine`, bumped in `write8` and in `out8`, and expose it:

```c
/// Total memory and port writes the guest has performed. The runner uses this
/// to detect a pure-read spin without knowing which loop it is in.
uint64_t machine_write_count(const Machine *m);
```

`runner_run_to_frame`:

```
loop:
  if instructions executed this frame > FRAME_INSN_BUDGET: RUN_ERROR "guest never completed a frame"
  if the guest is outside the loaded image: RUN_ERROR (see below)
  before = machine_write_count()
  ip = cpu->ip, cs = cpu->sreg[CS]
  if !i8086_step(cpu): RUN_ERROR (cpu->error names it)
  if machine_error(): RUN_ERROR          // see below -- do not omit this
  if machine_exited(): RUN_EXITED
  if machine_write_count() == before: ++idleRun; else idleRun = 0
  if idleRun >= IDLE_THRESHOLD:
      i8086_interrupt(cpu, 8); idleRun = 0
  if cs == MACHINE_LOAD_SEG && ip == 0x029A:   // the instruction just executed was the call
      ++frameCount; capture; return RUN_FRAME
```

Capture **before** stepping the `call` or after — either is fine as long as it is consistent, because `flip_vp` does not draw. Checking `ip` *before* the step and acting after it (as above) means the capture happens with the completed frame in place and the call already taken.

**Check `machine_error()` after every step, not just `i8086_step()`'s return value.** This was found during Task 1 and is a genuine silent-failure hole: the machine reports an unimplemented port or BIOS service by setting `cpu->error` from inside `out8`/`in8`/`intercept`, but those callbacks have no way to fail the instruction, so `i8086_step()` returns **true** and the guest runs on. Without this check the harness would sail past the first unimplemented port and produce a divergence that blames the port for the machine's gap — exactly the misattribution this tool exists to prevent.

**Runaway guards**, both from the spec:

- The guest executing outside the loaded image: `linear(CS,IP)` outside `[0x29230, 0x2B083]` is an error. Note the interrupt stubs at `0000:0500` are legitimately outside — allow that range too, or check only when `CS == MACHINE_LOAD_SEG`. Prefer the latter: simple and it still catches a wild jump within the code segment.
- The per-frame instruction budget above.

`runner_run_to_sync` is the same loop with the stop condition `cs == MACHINE_LOAD_SEG && ip == 0x018B`, and no capture. It exists so a test can assert the determinism point is reached — which also proves the title screen timed out rather than hanging.

- [ ] **Step 1: Write `emu/runner.h`**

Declare the types and functions from the Interface summary, with doc comments explaining the idle rule and the two guards. Include `machine.h`.

- [ ] **Step 2: Write the failing test `emu/test_runner.c`**

```c
// Boot the real BOLO.COM and prove it gets where the comparison needs it to.
//
// This is the first test that runs the original end to end, so its failures are
// the most informative in the suite: reaching the sync point at 2913:018B means
// the memory and EGA checks passed, the interrupt vectors were installed, the
// title screen rendered and timed out, and rnd_state was reset -- i.e. the
// attract demo is deterministic from here on.

#include "runner.h"

#include <stdio.h>

int main(void) {
  Runner *r = runner_create(BOLO_COM_PATH);
  if (!r) {
    fprintf(stderr, "FAIL: runner_create\n");
    return 1;
  }

  // Reaching 018B proves the title screen timed out into the demo. It cannot
  // time out unless timer ticks are being delivered while the guest spins at
  // 2913:11E5 -- which is the whole reason tick delivery is idle-triggered
  // rather than tied to the gameplay gate at 028E.
  RunResult res = runner_run_to_sync(r);
  if (res != RUN_SYNCED) {
    fprintf(stderr, "FAIL: never reached the sync point: %s\n",
            runner_error(r) ? runner_error(r) : "(no error message)");
    return 1;
  }

  // time_tick must have advanced past the title screen's 3Ch-tick deadline.
  if (runner_time_tick(r) < 0x3C) {
    fprintf(stderr, "FAIL: time_tick is %u, expected at least 3Ch by the sync point\n",
            runner_time_tick(r));
    return 1;
  }

  // Then the demo must actually produce frames.
  for (unsigned i = 0; i != 10; ++i) {
    res = runner_run_to_frame(r);
    if (res != RUN_FRAME) {
      fprintf(stderr, "FAIL: frame %u: %s\n", i,
              runner_error(r) ? runner_error(r) : "(no error message)");
      return 1;
    }
  }

  if (runner_frame_count(r) != 10) {
    fprintf(stderr, "FAIL: frame count is %u, expected 10\n", runner_frame_count(r));
    return 1;
  }

  // The captured frame must not be blank -- a uniformly zero plane means the
  // EGA write path is not reaching the planes.
  {
    bool anySet = false;
    for (int p = 0; p != EGA_PLANES && !anySet; ++p) {
      const uint8_t *plane = runner_plane(r, p);
      for (unsigned i = 0; i != EGA_PAGE_VISIBLE; ++i)
        if (plane[i]) { anySet = true; break; }
    }
    if (!anySet) {
      fprintf(stderr, "FAIL: all four captured planes are blank\n");
      return 1;
    }
  }

  printf("runner ok: reached sync at tick %u, captured %u frames\n",
         runner_time_tick(r), runner_frame_count(r));
  runner_destroy(r);
  return 0;
}
```

- [ ] **Step 3: Add the targets, run to see it fail, then implement `emu/runner.c`**

```cmake
add_library(bolo_runner STATIC runner.c)
target_include_directories(bolo_runner PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(bolo_runner bolo_machine)

add_executable(test_runner test_runner.c)
target_link_libraries(test_runner bolo_runner)
add_test(NAME runner COMMAND test_runner)
```

plus both targets in the warnings `foreach`.

**If the sync point is never reached, do not raise the instruction budget.** Print `CS:IP` and the last few addresses when the budget expires and go read those addresses in `BOLO.LST` — the guest is stuck somewhere specific, and the address names the missing piece.

- [ ] **Step 4: Add `--original DIR` to `emu/bolotest.c` and look at the output**

Add a mode that runs the original alone and dumps its captured frames as PPMs, reusing `ppm_write_planes` and the port's palette (`bolo_palette()`); rendering both sides through the same palette means any visible difference later is a real plane difference.

```bash
./build/emu/bolotest --original /tmp/orig --frames 60
python3 -c "from PIL import Image; Image.open('/tmp/orig/frame-00000.ppm').save('/tmp/o0.png')"
```

**Open the PNG and look at it.** This is the milestone: the original, running under an interpreter written from its own disassembly, drawing BOLO. A byte-count heuristic is not a substitute — the point is to confirm it is a maze and a ship, not plausible-looking noise.

- [ ] **Step 5: Lock the original's behavior with a golden trace**

This is the spec's "Testing the harness" item 3, and it is worth doing the moment the original runs: it pins the *interpreter's* behavior independently of the port, so a later refactor of `i8086.c` or `machine.c` cannot silently change what the original does. Without it, the only signal is the comparison — which would blame the port.

Do **not** commit 60 frames of planes (32000 bytes each). Commit a checksum per frame:

- Add `--golden FILE` to `bolotest`. In `--original` mode it writes one line per captured frame: the frame index, the guest's `time_tick`, and a checksum over all four planes' `EGA_PAGE_VISIBLE` bytes.
- Use FNV-1a 64-bit — eight lines, no dependency, and far better spread than a sum:

```c
static uint64_t fnv1a(const uint8_t *data, size_t len, uint64_t hash) {
  for (size_t i = 0; i != len; ++i) {
    hash ^= data[i];
    hash *= 0x100000001B3ULL;
  }
  return hash;
}
```
  seeded with `0xCBF29CE484222325ULL` and chained across the four planes in order.

- Generate `emu/golden-original.txt` for the first 60 frames and commit it.
- Register a test that regenerates and diffs:

```cmake
add_test(NAME golden COMMAND bolotest --original ${CMAKE_CURRENT_BINARY_DIR}/golden-run
                             --frames 60
                             --golden ${CMAKE_CURRENT_BINARY_DIR}/golden-out.txt
                             --check-golden ${CMAKE_CURRENT_SOURCE_DIR}/golden-original.txt)
```

  where `--check-golden` compares against the committed file and exits nonzero on the first differing line, naming the frame.

**If this test ever fails, the interpreter's behavior changed.** That is either a bug you just introduced or a fix you just made — decide which, and only then regenerate the file. Regenerating it to make the test pass is the same mistake as relaxing the decode oracle.

- [ ] **Step 6: Commit**

```bash
git add emu/runner.c emu/runner.h emu/test_runner.c emu/bolotest.c \
        emu/golden-original.txt emu/CMakeLists.txt
git commit -m "Add the run loop and boot the original to its attract demo"
```

---

### Task 5: The comparison and the ratchet

Run both sides and answer the question the whole harness exists for.

**Files:**
- Create: `emu/baseline.txt`
- Modify: `emu/bolotest.c`, `emu/CMakeLists.txt`, `CLAUDE.md`

**Interfaces:**
- Consumes: `runner.h` and `bolo.h`.
- Produces: `bolotest --compare`, artifacts on divergence, and the `compare` CTest target.

#### The alignment, one more time

The port's planes at `bolo_frame_count() == k` correspond to the original's capture **k+1**. So the original runs one capture ahead of the port:

```c
// Capture 1 has no port frame to match: the port's frame 1 corresponds to the
// original's capture 2. Pull capture 1 and drop it, then stay one ahead.
runner_run_to_frame(r);                       // the original's capture 1, discarded

for (long k = 1; k <= limit; ++k) {
  advance_port_to_frame(k);                   // the port's frame k
  if (runner_run_to_frame(r) != RUN_FRAME)    // the original's capture k + 1
    break;
  compare(port_planes, original_planes, k);
}
```

Check the indices against the invariant before writing the loop: on the first iteration `k == 1`, the pre-loop pull already consumed capture 1, so the pull inside the loop yields capture 2 — which is `k + 1`. Restate that in a comment at the comparison site. This is the single easiest thing in the plan to get backwards, and getting it backwards manufactures a divergence at frame 0 that does not exist.

#### What to compare

`EGA_PLANES * EGA_PAGE_VISIBLE` = 32000 bytes: the original's **drawn** page (`machine_draw_page()`), against the port's page 0 (`bolo_plane()`).

- [ ] **Step 1: Add `--compare` to `emu/bolotest.c`**

Flags: `--compare`, `--ticks N` (default 1000), `--out DIR`, `--continue-past-diff`, `--baseline FILE`.

On divergence report, to stdout:

```
DIVERGENCE at frame 137 (guest time_tick 8Ch)
  32000 bytes compared, 214 differ
  bounding box of differing pixels: x 96..135, y 40..47
  wrote /tmp/cmp/orig-00137.ppm, port-00137.ppm, diff-00137.ppm
```

The bounding box is what makes a divergence diagnosable — "214 bytes differ" says nothing, "a 40x8 box at (96,40)" is a sprite. Compute it by walking the plane bytes, converting each differing byte's index to `(x, y)` via `EGA_STRIDE`, and taking min/max. The diff PPM should mark differing pixels in a single high-contrast color over a dimmed copy of the original, so the eye goes straight to them.

- [ ] **Step 2: Add the baseline ratchet**

`emu/baseline.txt` holds one integer: the highest frame count known to match. `bolotest --compare` reads it and:

- diverges **before** the baseline → exit nonzero, print `REGRESSION: matched N frames, baseline is M`.
- diverges **at or after** the baseline → exit 0, print `matched N frames (baseline M)`; if `N > M`, also print `IMPROVED: update emu/baseline.txt to N`.
- reaches `--ticks` with no divergence → exit 0, print the full match.

**Do not have the tool rewrite `baseline.txt` itself.** A number that updates silently is not a ratchet — it is a file that always agrees with whatever just happened. Updating it is a deliberate commit.

Seed the initial value by running the comparison once and committing what it actually reports. **Expect it to be low.** The port is incomplete, carries `HACK`/`HACK2`/`HACK3` toggles and has `flip_vp` disabled; the spec says so explicitly. A low first number is the correct starting state, and it is the number that improves.

- [ ] **Step 3: Register the CTest target**

```cmake
add_test(NAME compare COMMAND bolotest --compare --ticks 200
                              --out ${CMAKE_CURRENT_BINARY_DIR}/cmp
                              --baseline ${CMAKE_CURRENT_SOURCE_DIR}/baseline.txt)
```

- [ ] **Step 4: Run it and record the result honestly**

```bash
cd build && ctest --output-on-failure -R compare; cd ..
```

Whatever number comes out, that is the number. If the two sides diverge at frame 0, **that is a finding, not a failure of this plan** — write down what differs (the bounding box and the two PPMs will say), commit the baseline as 0, and report it. Do not adjust the comparison to make a match appear.

- [ ] **Step 5: Document it in `CLAUDE.md` and commit**

Add a short section under "Build and run" covering `bolotest --compare`, what the baseline means, and the rule that a divergence is investigated rather than tuned away.

```bash
git add emu/bolotest.c emu/baseline.txt emu/CMakeLists.txt CLAUDE.md
git commit -m "Add the port-vs-original comparison and the baseline ratchet"
```

---

## Definition of Done

- `ctest` passes, including the new `machine`, `runner` and `compare` tests.
- `cmake --build build 2>&1 | grep -c warning` prints `0`.
- `clang-format --dry-run -Werror emu/*.c emu/*.h` exits 0.
- `bolotest --original` writes recognizable BOLO frames — **confirmed by looking at one**, not by a byte-count heuristic.
- `bolotest --compare` reports a frame count and exits 0 against a committed `emu/baseline.txt`.
- `git diff --stat 6afe328..HEAD -- src` is empty: no game logic changed.
- Nothing under `disasm/` changed.

## Known limitations, carried forward

- **One run per process.** `bolo_reset()` aborts if called twice, so the CLI runs one trace per invocation.
- **Sound is not compared.** The original's pitch comes from a cycle-counted bit-banged loop; comparing it would require cycle accounting, which the tick-on-idle design deliberately avoids.
- **The original double-buffers and the port does not.** Handled by comparing the original's drawn page against the port's page 0, but it means a bug in the port's page handling is invisible to this tool.
- **`rep` executes atomically inside one `i8086_step()`**, so the instruction budget under-counts by up to 65535 per string instruction. `CX` bounds it, so nothing hangs.
