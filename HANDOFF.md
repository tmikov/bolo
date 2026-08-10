# HANDOFF

Written 2026-08-09, at the end of the session that implemented plan 3 of the fidelity
harness. **All three plans are done.** This describes a point in time — replace or delete it
once the first divergence is closed and the baseline moves off 0.

## The headline

The harness works, and it has an answer.

`disasm/BOLO.COM` — the original 1993 binary — now boots and plays under an 8086 interpreter
written from its own disassembly, alongside the C port, with their EGA planes compared frame
by frame.

**At the first comparable frame, 31,972 of 32,000 bytes match.**

The 28 that differ are one thing: the original fills a 2x14 light-blue segment at the left of
the gauge bar (`x 232..233, y 72..85`) and the port leaves it empty. That is **the first bug to
chase**, and it points at `update_fuel` (`src/bolo.c:1443`) or `draw_hud` (`:1766`).

The maze, the ship, the title, the score, the ship icons, the 2x2 base indicator and the
compass needle match **byte for byte** on that frame. All 440 frames of the attract demo were
compared, ending cleanly at the guest's own `INT 20h`.

**The divergence grows, and the gauge is not the whole story.** Measured byte counts per frame:

| frame | 1 | 50 | 100 | 140 | 200 |
| --- | --- | --- | --- | --- | --- |
| bytes differing | 28 | 196 | 224 | 2569 | 3625 |

So something beyond the gauge starts differing well before frame 50, and around frame 140 the
two sides diverge wholesale — by then they have scrolled the view differently, and everything
after that is downstream of an earlier cause. Fix the gauge first, then re-measure; the
frame-50 growth is the next thread, not the frame-140 explosion.

(An earlier draft of this file said "131 frames diverge only inside that panel." That was
wrong — it is already 196 bytes at frame 50.)

**`emu/baseline.txt` therefore reads `0`, and that number badly understates the state of the
port.** It means "the port diverges at the first frame it is possible to compare" — not
"nothing matches". Read the paragraph above before drawing any conclusion from the file.

Reproduce it with:

```sh
./build/emu/bolotest --compare --ticks 400 --out /tmp/cmp --continue-past-diff
python3 -c "from PIL import Image; Image.open('/tmp/cmp/diff-00001.ppm').save('/tmp/d.png')"
```

## What to do next

**Close the two panel differences.** They are the smallest, best-characterised bugs in the
port, they are the only thing standing between the project and a nonzero fidelity number, and
the harness will tell you the moment you have fixed them. Start with the radar dots — the port
draws something the original does not, which is usually the easier direction.

Then commit the new baseline. The tool prints `IMPROVED: update emu/baseline.txt to N`; a
human commits it. **The tool never rewrites that file**, because a number that always agrees
with the last run is not a ratchet.

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

Branch `work`, 38 commits ahead of `master`, nothing pushed. `ctest` is 14/14 green, the build
is warning-free under `-Wall -Wextra`, and `clang-format --dry-run -Werror emu/*.c emu/*.h`
exits 0. Across all of plan 3, **nothing under `src/` or `disasm/` changed** — the thing under
test was never touched to flatter the result.

| Component | What it is |
| --- | --- |
| `emu/lst.{c,h}` | Parses `disasm/BOLO.LST` into 3439 instruction records, each verified byte-for-byte against `BOLO.COM`. |
| `emu/i8086.{c,h}` | The CPU: decoder (validated at all 3439 addresses), ALU with exact 8086 flag semantics, executor. |
| `emu/machine.{c,h}` | The XT: 1MB memory, BIOS data area, EGA planes at A0000, seven I/O ports, five BIOS/DOS services. |
| `emu/runner.{c,h}` | The run loop: tick delivery, frame capture, runaway guards. |
| `emu/bolotest.c` | Three modes — dump the port's frames, dump the original's, or compare them. |
| `emu/baseline.txt` | The ratchet. |
| `emu/golden-original.txt` | 60 FNV-1a checksums pinning the *original* side, independent of the port. |

Tests: `link`, `ppm`, `ega_render`, `lst`, `decode`, `alu`, `exec`, `machine`, `runner`,
`golden`, `headless_frames`, `determinism`, `pump_invariance`, `compare`.

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
