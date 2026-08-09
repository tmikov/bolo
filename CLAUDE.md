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

## Build and run

There is no test suite, no linter config beyond `.clang-format`, and no CI.

**Native (macOS and Linux).**

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build
```

**Headless.** `emu/bolotest` runs the attract demo with no window and writes one PPM per
completed frame: `./build/emu/bolotest --frames 100 --out /tmp/frames`. `ctest` from the build
directory runs the harness tests. Excluded from the Emscripten build.

`src/CMakeLists.txt` has three branches — Emscripten, `APPLE`, and everything else. macOS compiles
`sokol.m` (the sokol headers are Objective-C there) and links Cocoa/QuartzCore/OpenGL/AudioToolbox.
Linux compiles `sokol.c` and needs `libx11-dev libxi-dev libxcursor-dev libgl-dev libasound2-dev`;
CMake reports which one is missing at configure time. Both use `SOKOL_GLCORE33` — sokol_app only
supports GL on Linux. `-pthread` comes from `Threads::Threads` and is mandatory: sokol_app.h calls
`pthread_attr_init` purely so that omitting it becomes a link error rather than a runtime mystery.

**Wasm.** The Emscripten branch of `src/CMakeLists.txt` sets `SOKOL_GLES2`, an `.html` suffix, and
`--shell-file src/shell.html`; `.gitignore` lists `embuild/`, so the build directory was
conventionally named that. The exact command is not recorded anywhere in the repo; the standard
form is `emcmake cmake -S . -B embuild -DCMAKE_BUILD_TYPE=Release && cmake --build embuild`.
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
3. `bolo_update_screen()` → `ega_to_rgb()` → `sg_update_image` → fullscreen textured quad drawn
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
- `ega_to_rgb()` unpacks the active page's four planes into `g_rgb_screen` (padded to
  `EGA_WIDTH_POT` x `EGA_HEIGHT_POT` for the texture) through the 16-entry `g_ega_palette`.

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
- `HACK`, `HACK2`, `HACK3`, `VERBOSE` at the top of the file are compile-time debug toggles
  (skip the title screen, force a level, extra logging). Leave them at their committed values
  unless debugging.

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
