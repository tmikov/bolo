# HANDOFF

Written 2026-08-10. **All three plans of the fidelity harness are done, the first divergence
it found has been closed, and the harness now compares game state as well as pixels.** This
describes a point in time — replace it when the baseline moves again.

## The headline

The harness works, and it has an answer.

`disasm/BOLO.COM` — the original 1993 binary — now boots and plays under an 8086 interpreter
written from its own disassembly, alongside the C port, with their EGA planes compared frame
by frame.

**The port is frame-exact against the original for the first 147 frames**, out of the 440 the
attract demo runs. `emu/baseline.txt` reads `147`.

That is the whole screen — maze, ship, HUD, gauge, radar, compass — matching byte for byte,
frame after frame, against the original binary executing under the interpreter.

## Next: an actor at the bottom maze edge, frame 148

The state now matches through frame 133 and the pixels through 147. Two frontiers, in order:

**Frame 134, state.** Actor 20 goes its own way: `ship_cellx/celly/ofsx/ofsy`, `coll_flags1`,
`var_188e` and `ship_kind` all differ for that one actor, and `rnd_state` and `time_5bit` with
them. `ship_kind[20]` differing (original 11h, port 10h) is the interesting part — the two
sides disagree about *what that actor is*, not just where it is, which points at actor
creation or destruction rather than movement. The partial `FIXME` routines below are the
suspects.

**Frame 148, pixels.** 14 bytes, `x 56..105, y 189..191` — a few pixel fragments on the bottom
edge of the maze, an actor clipped at the border. Small and self-contained; probably a
worthwhile one to take first, since it is a drawing difference rather than a state one.

## What is still missing

The port has 6 routines carrying `FIXME`. Two are entirely empty, four partial:

| state | routine |
| --- | --- |
| empty | `inc_fuel`, `update_hisco` |
| partial | `is_actor_close`, `explode_bullets`, `draw_enemy_base`, `draw_enemies` |

`update_fuel` and `proc_60` were two of the empty ones and are now done: `update_fuel` was
worth 131 frames on its own, `proc_60` another 16. The harness is the tool for prioritising the
rest: implement one, re-measure, see what it buys.

Two constants were wrong and are now fixed. `proc_58` ended `while (dh & 80)` — decimal 80,
i.e. `50h` — where `2913:2C35` is `test dh,dh` / `js`, a test of bit 7; `dh` is only ever `FFh`
or `00h` during the demo so both spellings happened to agree, but it would have diverged the
moment it wasn't. The real one was in `proc_60`'s caller: `2913:2775` does
`mov al,dl / sub dl,vel_magn[si] / jns loc_302`, and the port never did that subtraction, so an
actor that had just chosen a new count stored it undecremented and ran one step long. That was
the frame-12 divergence.

## Comparing state, not just pixels

`bolo_state_table()` in `src/bolo.c` lists the port's variables against the address the
original keeps each at, which turns the `// 4F9Bh` annotations from documentation into
something checkable. `--compare` walks it every frame and reports each variable the first time
it differs; `--watch NAME[:ELEMENT]` prints both sides' value of one of them every frame, which
is what shows a sequence going out of step.

```sh
./build/emu/bolotest --compare --ticks 400 --out /tmp/cmp
./build/emu/bolotest --compare --ticks 400 --out /tmp/cmp --watch var_188e:31
```

Two cautions, both learned the hard way:

- **A wrong address or size in that table invents a divergence.** It compares the port's
  variable against the original's *neighbouring* one and reports a difference that does not
  exist. `bullet_x`/`bullet_y` were entered at the right addresses with the port's widened
  `int16_t` sizes, so each read 64 bytes where the original keeps 32, and the tool duly
  reported a difference at frame 1. `test_state_table` now fails on any entry that overlaps
  the next.
- **The table is deliberately partial.** `time_tick` and `last_tick` are left out because the
  harness delivers ticks on its own schedule; `dest_seg` and `pactor_list` because the port
  represents them differently on purpose.

## What to do next

**Take the frame-148 pixel divergence**, then the frame-134 state one — both described above.

When it moves, commit the new baseline. The tool prints `IMPROVED: update emu/baseline.txt to
N`; a human commits it. **The tool never rewrites that file**, because a number that always
agrees with the last run is not a ratchet.

**Do not change the comparison, the alignment or the baseline to make a difference disappear.**
The difference is the finding. That rule is in `CLAUDE.md` too.

## Read these first, in this order

1. `CLAUDE.md` — build commands, the architecture, the `disasm/` workflow, and a section on
   running the comparison.
2. `docs/superpowers/plans/2026-08-09-bolo-machine-and-comparison.md` — what plan 3 built and
   why. Its "Measured facts this plan is built on" section is the reference for the original's
   hardware surface.
3. `docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md` — the original design
   for all three plans. Still useful for the rationale, but **several of its specifics were
   corrected by measurement** — see "Where the spec is wrong" below.

## Where things stand

Branch `work`, nothing pushed. `ctest` is 15/15 green, the build is warning-free under
`-Wall -Wextra`, and `clang-format --dry-run -Werror` exits 0 on everything but the two known
pre-existing `VID_OFFSET` violations in `src/bolo.c`. Nothing under `disasm/` has ever changed.
`src/bolo.c` was untouched for the whole of plan 3; every change to it since has been
implementing a missing routine or fixing a wrong constant, never adjusting the port to flatter
the comparison.

| Component | What it is |
| --- | --- |
| `emu/lst.{c,h}` | Parses `disasm/BOLO.LST` into 3439 instruction records, each verified byte-for-byte against `BOLO.COM`. |
| `emu/i8086.{c,h}` | The CPU: decoder (validated at all 3439 addresses), ALU with exact 8086 flag semantics, executor. |
| `emu/machine.{c,h}` | The XT: 1MB memory, BIOS data area, EGA planes at A0000, seven I/O ports, five BIOS/DOS services. |
| `emu/runner.{c,h}` | The run loop: tick delivery, frame capture, runaway guards. |
| `emu/bolotest.c` | Three modes — dump the port's frames, dump the original's, or compare them. Comparison covers planes and game state. |
| `emu/baseline.txt` | The ratchet. |
| `emu/golden-original.txt` | 60 FNV-1a checksums pinning the *original* side, independent of the port. |

Tests: `link`, `ppm`, `ega_render`, `state_table`, `lst`, `decode`, `alu`, `exec`, `machine`,
`runner`, `golden`, `headless_frames`, `determinism`, `pump_invariance`, `compare`.

## Where the spec is wrong

The design spec predates the binary being instrumented this closely. Measured corrections:

- **Tick delivery.** The spec says fire `INT 08h` when the guest spins at the gameplay frame
  gate `2913:028E`. That hangs the harness: the title screen waits on the same `time_tick` at
  `2913:11E5` and never exits. Ticks are delivered on a generic idle rule instead — see
  `IDLE_THRESHOLD` in `emu/runner.h`, which documents the deviation and its measured cost.
- **`flip_vp` and the CRTC.** The spec is right that the CRTC is written, but note `flip_vp`
  sets `DX = 3DAh`, spins, then does `mov dl,0D4h` — a *half-register* write turning DX into
  `3D4h`. A port scan tracking only `mov dx,` concludes the CRTC is never touched.
- **BIOS functions.** Five, not three: `INT 10h` AH=00h/05h/0Eh and `INT 21h` AH=25h/09h, plus
  `INT 20h`.
- **Instruction counts.** 3439 instructions and 169 distinct opcode keys, not the spec's
  estimated "3302 and ~60" — that undercount missed the 56 prefixed instructions.

## Things that cost a session to learn

- **`dest_seg_e` names the page being *displayed*, not the one being drawn.** `clr_alt_box` at
  `2913:04CF` does `mov ax,dest_seg_e / xor ah,2 / mov es,ax` and never restores ES, so the
  guest draws into the *other* page. `machine_draw_page()` returns the complement, and there is
  a test pinning it. Get this backwards and every frame carries the previous frame's drawing.
- **The `INT 08h` stub must preserve AX.** A hardware interrupt lands between arbitrary
  instructions; a stub that clobbers AL made the title screen exit as if a key had been
  pressed, which skipped the `rnd_state` reset and hung the level editor at `2913:12F4`.
- **A 30-tick gap around frame 150 is a level transition, not a stall.** There are exactly two
  tick-waits outside the frame gate — `2913:01B9  add al,14h` (20 ticks, level start) and
  `2913:0215  add al,32h` (50 ticks, level complete) — and neither crosses the frame gate, so a
  level boundary necessarily costs 20-50 ticks while producing no frames.
- **Frame alignment: the port's frame `k` pairs with the original's capture `k+1`.** The port's
  counter increments where the original calls `flip_vp`, but control falls through and draws an
  entirely new frame before yielding — see `bolo_plane()` in `src/bolo.h`. Beware: **at frame 1
  alone the wrong alignment looks better** (20 differing bytes versus 34). Only the trend from
  frame 5 on distinguishes them — thousands of differing bytes for same-index pairing versus
  dozens for `k+1`.
- **Overflow is write-only in this program.** BOLO has no signed conditional jump (`70`/`71`,
  `7C`-`7F` are all absent), no `pushf`/`popf`, and `OF` is bit 11 so its single `lahf` cannot
  see it. The interpreter's undefined-flag choices therefore cannot affect it — do not chase
  them on a divergence.

## Environment facts

- **Always configure CMake with `-G Ninja`.**
- **This box is headless** — no `DISPLAY`, no Xvfb. The windowed app (`./build/src/bolo`)
  cannot be verified here; it aborts at `XOpenDisplay()`. Plan 3 did not touch `src/`, so that
  risk is unchanged, but it remains true.
- **GCC is the default `cc` and is stricter than clang.** Check warnings with a count, not a
  grep for one word: `cmake --build build 2>&1 | grep -c warning` must print `0`.
- **Judge `clang-format` by exit status** — with `-Werror` it emits `error:`, not `warning:`,
  so grepping for warnings reports a dirty file as clean. `src/bolo.c` has two known
  pre-existing violations on the `VID_OFFSET` macro; everything else is clean.
- **Ignore the LSP for `emu/`.** There is no `compile_commands.json`, so clangd reports
  spurious "file not found" and "undeclared identifier" errors, including for CMake-provided
  macros like `BOLO_COM_PATH` and for `bolo.h`. Trust the build.
- **Python PIL is available**, which is how you look at a frame.

## Verification habits that paid off

1. **Build the oracle before the thing it judges.** Validating the decoder against 3439 real
   instructions before writing any execution logic means a decode bug can never be
   misdiagnosed as an execution bug later.
2. **Mutation-test a new test before trusting it.** Several tests in `emu/` were checked by
   deliberately breaking the code they cover and confirming they failed. One case
   (`shr F000h,1`) leaves the correct low byte, so only a separate high-byte assertion catches
   it — that would otherwise have been a silently vacuous test.
3. **Look at the picture.** Opening the diff image is what turned "40 bytes differ" into "a
   gauge segment and six radar dots".
4. **Measure the binary instead of reasoning about it.** Every correction in "Where the spec is
   wrong" came from counting opcodes or reading addresses, not from argument. Two of them
   contradicted confident prose.
5. **Treat every report as a claim.** Each task's build, tests and formatting were re-run
   independently before review. Six defects in the plans themselves surfaced that way,
   including an EGA latch arithmetic error and an interrupt stub that corrupted a register.
