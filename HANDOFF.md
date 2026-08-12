# HANDOFF

Written 2026-08-11. **All three plans of the fidelity harness are done, and the port now
reproduces the original's entire attract demo byte for byte.** This describes a point in time —
replace it when the baseline moves again.

## The headline

The harness works, and it has an answer.

`disasm/BOLO.COM` — the original 1993 binary — now boots and plays under an 8086 interpreter
written from its own disassembly, alongside the C port, with their EGA planes compared frame
by frame.

**The port is frame-exact against the original for all 440 frames of the attract demo** — every
plane, every pixel, and every mirrored variable. `emu/baseline.txt` reads `440`, and the
comparison now ends because the original's recorded key script reaches its ESC, not because the
two sides differ.

That is the whole screen — maze, ship, HUD, gauge, radar, compass — matching byte for byte,
frame after frame, against the original binary executing under the interpreter.

## Next: past the attract demo

The demo no longer diverges, so the ratchet has nothing left to catch. That does **not** mean
the port is faithful — it means the demo has stopped exercising the difference. The demo never
fires under human control, never loses a life, never finishes a level, and never reaches the
high-score entry.

The two cheap avenues are now closed, both by measurement:

- **The read-modify-write sweep is done.** Every `or es:[di]` in the binary is
  either converted to `ega_or_rmw` or carries a comment saying why it does not need to be.
  The four left alone -- `draw_maze`'s three and the fuel gauge's `xor` -- run outside the
  2913:0238..0255 OR-mode window and are provably indistinguishable from the simple form,
  because their masks include plane 3 and keep every masked plane equal to it.
- **`CLAMP_ACTOR_TO_MAZE` never fires.** Instrumented over all 440 frames: zero triggers, and
  turning it off changes no frame. It stays on anyway, because removing it properly means
  giving `maze_buf` the neighbours the original has -- the original's runs 3F89h..4F89h and an
  out-of-range access lands on `var_182e` and the state after it, deterministically, where the
  port would run off the end of an array. That is a real change, not a deletion.

So the only way forward is **input**. `machine_press_key()` exists on the guest and the
comparison does not use it; the port has `bolo_key()`. The wrinkle to know before starting:
both sides already replay the same recorded key script baked into the binary -- `reckeys` in
bolo.c, from `getkey` at 2913:0648 -- and `getkey` turns *any* real keypress into `SC_F3` and
abandons the script. So injecting a key does not extend the demo, it ends it and starts a real
game. Driving gameplay means authoring a script from scratch and deciding what it should
exercise: firing, dying, finishing a level, the high-score entry.

The natural injection point is a frame boundary rather than a tick, since the two sides run on
deliberately different tick schedules but the comparison already aligns on completed frames.

## How these were found

The method that worked for every divergence in this round:

1. `--compare` names the first variable to differ and the frame.
2. If `rnd_state` and `time_5bit` are among them, the two sides made a different *number* of
   random draws that frame, so the cause is upstream of the RNG rather than in it. Trace every
   draw on both sides and diff the sequences; the first differing row names the routine, and on
   the guest side the return address on the stack names the call site outright.
3. If they are not among them, it is plain logic: trace the routine's arguments on both sides
   and walk inward until they stop agreeing.
4. For a pixel difference with no state difference, log every write to the offending byte on
   both sides — the guest's `cpu.ip` at the write names the instruction outright.
5. Only then read the disassembly.

Reasoning from the disassembly first was slower every time, and twice pointed at the wrong
routine.

## What is still missing

The port has 5 routines carrying `FIXME`. Two are entirely empty, three partial:

| state | routine |
| --- | --- |
| empty | `update_hisco` |
| partial | `explode_bullets`, `draw_enemy_base`, `draw_enemies` |

`inc_fuel` has three call sites in the original (2913:191C, 1E81, 2596) and the port so far has
only the first. The other two live in routines that are still partial, so they are waiting on
those rather than missing outright.

`update_fuel`, `proc_60` and `inc_fuel` were three of the empty ones and are now done, and the
baseline went 0 -> 131 -> 147 -> 160 -> 163 -> 208 -> 269 -> 296 -> 366 -> 440 across this
work. The harness is the tool for prioritising the rest: implement one, re-measure, see what it
buys. Note that a routine being `FIXME` no longer means the demo will catch it -- the demo is
exhausted, so the next round needs input (see above).

Four constants were wrong and are now fixed. `proc_58` ended `while (dh & 80)` — decimal 80,
i.e. `50h` — where `2913:2C35` is `test dh,dh` / `js`, a test of bit 7; `dh` is only ever `FFh`
or `00h` during the demo so both spellings happened to agree, but it would have diverged the
moment it wasn't. The real one was in `proc_60`'s caller: `2913:2775` does
`mov al,dl / sub dl,vel_magn[si] / jns loc_302`, and the port never did that subtraction, so an
actor that had just chosen a new count stored it undecremented and ran one step long. That was
the frame-12 divergence.

The other two were both `15h` read as decimal 31. `spawn_enemy` (2913:238F, 2913:239D) will
recycle an actor only for a base within ten cells of the player; the port allowed fifteen, so
it spawned enemies the original refused to. And `is_actor_close` (2913:158B) wanted `ofsy >= 15h`
where the port had `>= 31`. The same routine had one more: at 2913:15A5 a `jb` with a
displacement of *zero* lands on the instruction it falls through to, so the `cmp al,0EBh` above
it decides nothing and the case is just `ofsy < 0` -- the port had encoded the dead comparison
as `< -21`, and its own FIXME comment had already guessed as much.

Frame 161 took a fourth, of a different kind: `div_cell_size` built its result with
`.x_rem = yd.rem, .y_rem = yd.rem`, so the x offset within the cell was the y division's
remainder. A bullet that should have hit an enemy missed by seventeen pixels. Nothing about the
disassembly would have suggested it -- the arguments simply stopped agreeing partway down.

## Comparing state, not just pixels

`bolo_state_table()` in `src/bolo.c` lists the port's variables against the address the
original keeps each at, which turns the `// 4F9Bh` annotations from documentation into
something checkable. `--compare` walks it every frame and reports each variable the first time
it differs; `--watch NAME[:ELEMENT]` prints both sides' value of one of them every frame, which
is what shows a sequence going out of step.

```sh
./build/emu/bolotest --compare --ticks 800 --out /tmp/cmp
./build/emu/bolotest --compare --ticks 800 --out /tmp/cmp --watch var_188e:31
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

**Give the comparison some input**, as described above. Until then the ratchet is pinned at the
full length of the demo and cannot move.

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
