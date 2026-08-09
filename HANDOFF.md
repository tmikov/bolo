# HANDOFF

Written 2026-08-08, at the end of the session that implemented plan 1 of the fidelity
harness. This describes a point in time — replace or delete it once plan 2 lands.

## Where things stand

Branch `work` at `979a144`, pushed (`origin/work` matches), 20 commits ahead of `master`.
Nothing is local-only. `ctest` is 6/6 green from a Ninja build.

**Plan 1 (the headless seam) is complete.** `src/bolo.c` is now game logic only, built as
the `bologame` library and driven through `src/bolo.h`. `src/shell_sokol.c` is the windowed
front end. `emu/bolotest` runs the game's built-in attract demo with no window and writes
one PPM per completed gameplay frame.

## Read these first, in this order

1. `docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md` — the design for all
   three plans. **This is the authority.** Its "What was verified about the original",
   "Alternatives considered" and "Known limitations" sections record decisions that were
   argued through once with evidence; don't re-litigate them without new information.
2. `docs/superpowers/plans/2026-08-08-bolo-headless-seam.md` — what plan 1 did and why.
3. `CLAUDE.md` — build commands, architecture, the `disasm/` workflow.

## Where the built tree differs from the spec

The spec's `## Layout and build` section predates plan 1. Actual state:

- `emu/` already contains `ppm.{c,h}`, `bolotest.c`, `compare_runs.sh`, and the tests
  `test_link.c`, `test_ppm.c`, `test_ega_render.c`. Plan 2 adds `lst.{c,h}`,
  `i8086.{c,h}`, `test_decode.c`, `test_alu.c`. Plan 3 adds `machine.{c,h}`,
  `baseline.txt`, and grows `bolotest.c` from a frame dumper into the comparison driver.
- `src/ega_render.{c,h}` is not in the spec at all. It is the single copy of the EGA
  planes → RGBA conversion, shared by the shell and `emu/ppm.c` and tested directly by
  `emu/test_ega_render.c`. It exists because that logic was duplicated and the shell's
  copy was untestable.
- `bolo.h`'s API as built: `bolo_reset()`, `bolo_timer_tick()`, `bolo_step()`,
  `bolo_run_tick(int pump)`, `bolo_key(uint8_t)`, `bolo_plane(int)`, `bolo_palette()`,
  `bolo_frame_count()`, and the `bolo_sound_sink` function pointer.

## The one correction plan 3 must not lose

`bolo_frame_count()` increments at the game's frame gate — the instant the original calls
`flip_vp` at `2913:029A` — but control does not return there. It falls through and draws
another whole frame before `bolo_run_tick()` returns. So the port's planes at
`bolo_frame_count() == k` correspond to the original's capture **k+1**, not k.

This is stated in `src/bolo.h`'s `bolo_plane()` doc comment, in the spec's "Capture and
comparison" section, and in plan 1's "Notes for the next plan". An earlier draft of the
spec had it wrong; plan 3 would have inherited a phantom divergence at tick 0.

## Next up: plan 2 — the 8086 interpreter

Scope: `emu/lst.{c,h}`, `emu/i8086.{c,h}`, `emu/test_decode.c`, `emu/test_alu.c`.

**Build the decoder first and validate it against `disasm/BOLO.LST`.** The LST holds 3302
disassembled instructions with their addresses and byte encodings, so the decoder can be
checked exhaustively over exactly the input that matters, before any execution logic is
trusted. Compare instruction length and mnemonic; reproducing Sourcer's operand syntax is
not worth it.

The measured target, all from the spec: 3302 instructions, ~60 distinct opcodes,
8086/8088 only (no 186+), three interrupt vectors (`INT 10h`, `INT 21h`, `INT 20h`), and
seven I/O ports. Flag semantics are the known bug farm — this program passes the carry
flag as a function argument.

## Environment facts

- **Always configure CMake with `-G Ninja`.** Required; see `CLAUDE.md`.
- **This box is headless** — no `DISPLAY`, no Xvfb. `./build/src/bolo` reaches X11 init and
  aborts with `XOpenDisplay() failed!`. The windowed app cannot be verified here; the user
  verified it working at `98cff14` by hand.
- **`clang-format` 18.1.3 is installed.** Every file this project owns is clean except two
  pre-existing violations on `src/bolo.c`'s `VID_OFFSET` macro. Use the command in
  `CLAUDE.md`, and never run it over `src/sokol_*.h` or `src/blit.h`.
- **Python PIL is available**, which is the fastest way to actually look at a frame:
  `python3 -c "from PIL import Image; Image.open('frames/frame-00040.ppm').save('/tmp/f.png')"`
  then open the PNG. Do this rather than trusting a byte-count heuristic.

## Verification habits that paid off

1. **Baseline-and-diff around refactors.** Run `bolotest --frames 60 --out before`, make the
   change, run it again, `diff -r`. This caught nothing because nothing broke — but it is
   what made "behaviour preserved" a fact rather than a hope, twice.
2. **Look at the output.** Opening frame 40 as an image is what confirmed the harness
   produces real gameplay rather than plausible-looking noise.
3. **Ignore the LSP for `emu/`.** There is no `compile_commands.json`, so clangd reports
   spurious "file not found" and "undeclared identifier" errors for every file that
   includes `bolo.h`. Trust the build, not the diagnostics.
4. **Treat subagent reports as claims.** One task's TDD evidence turned out to be a
   paraphrase presented as a terminal capture; a reviewer caught it by reproducing the
   failure independently. Re-verify the things that matter.
5. **Judge tools by exit status, not by grepping their output.** `clang-format -Werror`
   emits `error:`, so a `grep -c warning:` check reports a dirty file as clean. Likewise
   `clang-format` finds `.clang-format` relative to the file's real path, so checking a
   copy in a scratch directory silently falls back to LLVM style and invents violations.

## Open items

None parked — every finding from plan 1's reviews is closed.

One standing caveat: nothing in this environment can verify the **windowed app**. After any
change to `src/shell_sokol.c` or `src/ega_render.c`, ask the user to run it once and confirm
the title screen appears, the demo self-starts, and arrows/space respond.
