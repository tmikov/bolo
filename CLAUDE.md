# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project is

A C re-implementation of the 1982 Apple II game BOLO, ported from the unofficial 1993 IBM PC
DOS version by "Mr.Rm" (a 9KB `.COM` file). The port is driven by an offline disassembly of that
binary, kept in `disasm/`.

The goal stated in README.md is **faithfulness, not merely equivalent output**: the original 8086
logic, routine structure, and data layout are ported over, rather than reimplemented from scratch.
This is why the code looks the way it does — parallel arrays instead of structs, byte-sized
counters that intentionally wrap, reads back from video memory, and so on. When changing code,
preserve the original's semantics (including its overflow and aliasing behavior) rather than
"cleaning it up".

Work in progress on a fidelity harness is described in
`docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md`, with current state and
next steps in `HANDOFF.md`. Read both before starting on `emu/`.

## Build and run

There is no linter beyond `.clang-format` and no CI. Tests are CTest targets under `emu/`,
run with `ctest` from the build directory. Check formatting without rewriting anything:

```sh
clang-format --dry-run -Werror src/bolo.c src/bolo.h src/ega_render.c src/ega_render.h \
    src/shell_sokol.c emu/*.c emu/*.h
```

Never run it over `src/sokol_*.h` or `src/blit.h` — vendored and generated respectively, and
neither matches this project's style. `src/bolo.c` reports two pre-existing violations on the
`VID_OFFSET` macro (clang-format 18 wants `(y) * EGA_STRIDE`); everything else is clean.
Judge the result by exit status: with `-Werror` clang-format emits `error:`, not `warning:`,
so grepping for warnings reports success on a dirty file.

**Always configure with the Ninja generator** — `-G Ninja`. Don't use the Makefile default.

**Native (macOS and Linux).**

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build build
```

**Headless.** `emu/bolotest` runs the attract demo with no window and writes one PPM per
completed frame: `./build/emu/bolotest --frames 100 --out /tmp/frames`. `ctest` from the build
directory runs the harness tests. Excluded from the Emscripten build.

**Comparing the port against the original.** `bolotest --compare` runs both sides at once — the C
port, and `disasm/BOLO.COM` under the 8086 machine in `emu/` — and compares their EGA planes frame
by frame. It is the only thing in the repo that actually checks the project's stated goal.

```sh
./build/emu/bolotest --compare --ticks 1000 --out /tmp/cmp --baseline emu/baseline.txt
```

On a divergence it prints the frame, the guest's `time_tick`, how many of the 32000 bytes differ
and the **bounding box of the differing pixels** — a box is diagnosable, a byte count is not — and
writes `orig-NNNNN.ppm`, `port-NNNNN.ppm` and `diff-NNNNN.ppm` (the original, dimmed, with
differing pixels in magenta) into the output directory. `--continue-past-diff` keeps going instead
of stopping at the first one.

`emu/baseline.txt` holds one integer: the highest frame count known to match. The `compare` test
fails only when the port matches *fewer* frames than that. When it matches more, the tool prints
`IMPROVED: update emu/baseline.txt to N` and a human commits the new number — the tool never
rewrites it, because a file that always agrees with the last run is not a ratchet.

**A divergence is investigated, never tuned away.** Do not adjust the comparison, the alignment or
the baseline to make a difference disappear; the difference is the finding. Two things to know
before chasing one: the port's planes at `bolo_frame_count() == k` line up with the original's
capture **k+1** (see `bolo_plane()` in `src/bolo.h`), and if a divergence correlates with tick
counts, `IDLE_THRESHOLD` in `emu/runner.h` is the first suspect.

`emu/` code is ordinary C compiled with `-Wall -Wextra` and must stay warning-free. GCC is
stricter here than clang — check with a count, not a grep for one word, since a build that
emits warnings still exits 0:

```sh
cmake --build build 2>&1 | grep -c warning   # must print 0
```

`src/CMakeLists.txt` has three branches — Emscripten, `APPLE`, and everything else. macOS compiles
`sokol.m` (the sokol headers are Objective-C there) and links Cocoa/QuartzCore/OpenGL/AudioToolbox.
Linux compiles `sokol.c` and needs `libx11-dev libxi-dev libxcursor-dev libgl-dev libasound2-dev`;
CMake reports which one is missing at configure time. Both use `SOKOL_GLCORE33` — sokol_app only
supports GL on Linux. `-pthread` comes from `Threads::Threads` and is mandatory: sokol_app.h calls
`pthread_attr_init` purely so that omitting it becomes a link error rather than a runtime mystery.

**Wasm.** The Emscripten branch of `src/CMakeLists.txt` sets `SOKOL_GLES2`, an `.html` suffix, and
`--shell-file src/shell.html`; `.gitignore` lists `embuild/`, so the build directory was
conventionally named that. The exact command is not recorded anywhere in the repo; the standard
form is `emcmake cmake -S . -B embuild -G Ninja -DCMAKE_BUILD_TYPE=Release && cmake --build embuild`.
The published build lives on the orphan `gh-pages` branch (build artifacts only — `bolo.js`,
`bolo.wasm`, `index.html` — with no shared history with `master`).

**Disassembly helper tools:** `cd disasm && make` builds `prbmap` and `decode-str`.

**Shader.** `src/blit.h` is generated, do not edit it by hand. Regenerate with the command recorded
in its header: `sokol-shdc -i blit.glsl -o blit.h -l glsl100:glsl330:metal_macos`.

**Sokol** headers under `src/` are vendored; `src/sokol_version.txt` pins the upstream commit.
Don't modify them.

## Architecture

`src/bolo.c` (~4.1k lines) is the entire game logic in one translation unit, built as the
`bologame` static library and driven through `src/bolo.h`. `src/shell_sokol.c` is the windowed
front end (rendering, audio, keyboard); `emu/bolotest.c` is the headless one. `sokol.c`/`sokol.m`
exist only to instantiate `SOKOL_IMPL`.

### Frame loop and DOS interrupt emulation

`sokol_main` registers `bolo_init` / `bolo_frame` / `bolo_event`. Each frame:

1. Catch-up loop advancing simulated time in `TIMER_PERIOD_US` (54.925 ms) steps, calling
   `int_08h_entry()` per step — the DOS timer interrupt, which just increments `time_tick`.
   Game code paces itself by comparing against `time_tick`, exactly as the original did.
2. `async_start()` — one slice of the game (see below).
3. `bolo_update_screen()` → `ega_screen_to_rgba()` → `sg_update_image` → fullscreen textured quad drawn
   with the `blit` shader into an aspect-preserving viewport.

Keyboard input goes `sapp_event` → `to_scan_code()` → `int_09h_entry()`, i.e. it is converted to
**IBM PC scan codes** (`enum ScanCode`) and fed to a port of the original keyboard ISR.

### The async/unwind state machine (most important constraint)

The original is a blocking DOS game loop; the browser requires returning control after each frame.
Rather than threads (see README.md — SharedArrayBuffer was unavailable) the blocking routines were
manually converted into resumable state machines:

- Any routine that can block is named `async_*` and returns `AsyncResult`.
- Its resume point lives in a **global** `async_state.<routine>.state` field, not on the stack.
- The body is a `switch` on that field with deliberate `// FALL` fallthrough between cases.
- Returning `AS_UNWIND` yields to the browser, and every caller must propagate it immediately:
  `if (async_foo() == AS_UNWIND) return AS_UNWIND;`
- A caller resets the callee's `.state` to 0 before first entering it.

Consequences when editing: these routines are **not reentrant**, so a given `async_*` function can
only be active once at a time; adding a new wait point requires adding a `case` (and possibly a
`wait_until` field) rather than writing a loop; and no local variable survives an `AS_UNWIND`, so
anything that must persist across a yield has to become a global.

### EGA emulation

`g_ega_screen[EGA_PLANES][EGA_PAGE_SIZE * 2]` models EGA mode 0Dh planar memory: 4 bitplanes, two
320x200 pages of 0x2000 bytes each. The planar layout is preserved deliberately because **the game
does collision detection by reading back video memory** — `ega_read()` returns plane 3 only, and
routines like the ship/gun draw test the bits they are about to write against what is already
there.

- `ega_write` / `ega_or` / `ega_xor` take a 4-bit plane mask; the mask values are the `EGAColor`
  enum. `ega_write_bytes`, `ega_fill`, `draw_bmp` are the bulk helpers.
- `VID_OFFSET(x, y)` converts pixel coordinates to a byte offset (`EGA_STRIDE` = 40 bytes/row).
- `dest_seg` is a byte offset (0 or `EGA_PAGE_SIZE`) added to video offsets — named "seg" after the
  original's segment register. Page flipping is wired up (`ega_set_page`, `g_ega_page`) but
  currently pinned to page 0, and several draw routines write both pages explicitly by adding
  `EGA_PAGE_SIZE`.
- `src/ega_render.c` unpacks planes into RGBA through the 16-entry palette, and is the only
  copy of that logic: `ega_row_to_rgba()` does one row, `ega_screen_to_rgba()` does the screen
  at a caller-chosen stride. The shell renders at `EGA_WIDTH_POT` stride (leaving the texture's
  power-of-two padding untouched); `emu/ppm.c` renders row by row. Tested directly by
  `emu/test_ega_render.c`.

### The 8086 interpreter (`emu/`)

Built to execute the original `disasm/BOLO.COM` so the port can be diffed against it. Both
libraries are standalone — neither links `bologame` nor includes `bolo.h`.

- `emu/lst.{c,h}` parses `disasm/BOLO.LST` into 3439 `{addr, len, bytes, mnemonic}` records.
  The parsing rule has one trap: the byte field ends at a run of **two or more** spaces, not
  the first single space — terminating early silently drops all 56 `rep`- and
  segment-override-prefixed instructions. `test_lst` catches this by cross-checking every
  record's bytes against `BOLO.COM`, which is also what makes the LST trustworthy as an oracle.
- `emu/i8086.{c,h}` is the CPU: decoder, ALU, executor, in that order behind banner comments.
  It reaches memory and ports only through the callbacks on `struct I8086`, so it knows
  nothing about EGA or DOS. `i8086_reset()` deliberately leaves those callbacks alone.
- Flag semantics are the bug farm, because **the original passes the carry flag between
  routines as an argument** — so `inc`/`dec` must not touch `CF`. `test_alu` pins 30 vectors.
- Unknown encodings are hard errors naming the opcode, never silent no-ops.

Tests: `lst`, `decode`, `alu`, `exec`. The `decode` test is exhaustive over all 3439
instructions; **never relax it to make a case pass** — a length mismatch is a real bug.

### Game state layout

State mirrors the original's memory, so it is **parallel arrays indexed by actor**, not structs:
`ship_cellx[]`, `ship_celly[]`, `ship_ofsx[]`, `ship_ofsy[]`, `ship_angle[]`, `vel_magn[]`,
`bullet_x[]`, `bullet_flags[]`, and so on. Two sizes are in play — `NUM_ACTORS42` (42) for the
per-position arrays and `NUM_ACTORS32` (32) for the rest — matching the original's tables. Index 0
is not a valid actor in the linked lists (0 means end-of-list).

- **Maze:** `maze_buf` is a 64x64 grid of wall flags (`W_L`/`W_T`/`W_R`/`W_B`, `CELL_FL`). It is a
  macro pointing *into* `_ext_maze_buf` with a guard row in front, because the original relies on
  out-of-range accesses. `alist_buf` is the same trick with a 2-byte guard.
- **Actor lists:** `alist_buf` holds a per-cell list head; `next_actor[]`/`prev_actor[]`/
  `pactor_list[]` thread actors through their cell. `add_actor()`/`remove_actor()` maintain them.
- **Angles** are 0..7 (N, NE, E, ...) and velocity magnitudes 0..5; `step_xy[magnitude][angle]`
  gives the per-tick delta.
- **Text** uses the original's `SBOL` encoding (digits 0-9, letters 10-35, `!`, space, icons,
  terminated by a negative). Build literals with the `SB()` macro; `disasm/decode-str` decodes
  captured hex sequences.

### Naming and comment conventions in bolo.c

- `/// 2913:0100` above a function is the routine's segment:offset in the original binary.
- `// 4F89h` after a variable is its address in the original's data segment.
- `var_NNNN`, `proc_NN`, `arr_Ne`, `buf49` are entities lifted from the disassembly whose purpose
  is not yet understood. Renaming one to something meaningful is a normal part of the work.
- `DEBUG_SHOW_BASES`, `CLAMP_ACTOR_TO_MAZE`, `HACK2`, `HACK3`, `VERBOSE` at the top of the file
  are compile-time debug toggles, each documented where it is defined. Leave them at their
  committed values unless debugging — `bolotest --compare` is measured with these settings, so
  changing one changes what "matching" means.

  Only `CLAMP_ACTOR_TO_MAZE` is on, and it is a known, deliberate infidelity: it **changes game
  logic**, guarding against an out-of-range `maze_buf` access the original makes on purpose. It
  does not appear to fire during the attract demo, so the comparison cannot tell you what
  turning it off would cost — which is not the same as saying it is safe to remove.

## The disasm/ workflow

Reverse engineering is offline and iterative, using the vintage MS-DOS *Sourcer* disassembler
(README.md explains the rationale):

`BOLO.COM` + `BOLO.DEF` (hand-edited: what to disassemble, labels, comments) + `BOLO.REM`
(remark text) → Sourcer → improved `BOLO.LST` and a new `BOLO.DEF` → repeat, committing each
stage. `.gitattributes` forces CRLF on `BOLO.DEF`/`BOLO.REM` — keep it; these files are round-tripped
through a DOS tool.

The `*.lst` files with dimensions in the name (`bmp_bolo_8x20.lst`, `sprites_1x7x48.lst`, ...) are
hex dumps of the original bitmaps. `prbmap width_bytes height < bmp.lst` renders one as ASCII art
to eyeball it; passing any third argument instead emits a C array ready to paste into `bolo.c`.
`helpers.py` has two REPL helpers converting raw offsets into `VID_OFFSET(x, y)` or maze
coordinates.
