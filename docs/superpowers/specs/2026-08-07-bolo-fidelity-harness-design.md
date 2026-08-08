# BOLO Fidelity Harness — Design

Date: 2026-08-07
Status: approved, not yet implemented

## Goal

Prove that the C re-implementation behaves identically to the original 1993 IBM PC
`BOLO.COM`, by running both and comparing their EGA video memory tick by tick.

The project's stated purpose is faithfulness, not merely equivalent output. Nothing in
the repo currently verifies that. The deliverable is a tool that answers one question
precisely: **at which tick does the port first diverge from the original?**

That number doubles as a progress metric — "the port matches the original for the first
N ticks" — enforced by a committed baseline so it can only go up.

## Decisions

| Decision | Choice | Why |
| --- | --- | --- |
| Ground truth | Run the original under an in-process 8086 interpreter | Reusable, deeply inspectable, no capture or alignment guesswork, and both sides run in one process so divergence is localized to a tick |
| CPU core | Write a targeted interpreter | ~60 opcodes; the LST validates the decoder for free; no license friction with a public-domain project; instrumentation hooks are native rather than retrofitted |
| Comparison depth | Screen-level (EGA planes) to start | Smallest thing that is still a real oracle; write-stream and routine-level diffing layer on later without rework |
| Headless seam | Extract the sokol shell out of `bolo.c` | Native, Wasm, and headless become peer front-ends over one game core |

Alternatives considered and rejected: DOSBox lossless video capture (alignment is
heuristic — dedup silently merges ticks whose screen does not change); patching
`BOLO.COM` to dump planes per tick (exact and cheaper, but a dead-end artifact rather
than a reusable tool); vendoring a CPU core (`libx86emu` permissive but library-shaped
around someone else's hook granularity; `fake86`/`XTulator`/Unicorn are GPLv2, which
conflicts with this repo's public-domain licensing; `8086tiny` is MIT but deliberately
unhackable).

## What was verified about the original

Measured from `disasm/BOLO.LST` (6437 lines) and `disasm/BOLO.DEF`:

- **3302 instructions** using **~60 distinct opcodes**. `BOLO.DEF` sets `uP = 8088`, so
  pure 8086/8088 — no 186+ instructions.
- Only three interrupt vectors are called: `INT 10h` (5 sites), `INT 21h` (5), `INT 20h` (1).
  `INT 21h` use is essentially AH=25h set-interrupt-vector; the game installs its own
  `INT 08h` and `INT 09h` handlers.
- The entire hardware surface is seven ports: `3C4h` (sequencer map mask), `3CEh`
  (graphics controller — data rotate, read map select), `3D4h` (CRTC start address high),
  `3DAh` (retrace status), `60h` (keyboard data), `61h` (PC speaker), `20h` (PIC EOI).
  No PIT, DMA, disk, or BIOS blob.
- The original **double-buffers**: `flip_vp` at `2913:0506` waits for vertical retrace
  (`3DAh` bit 3), writes CRTC start-address-high (`3D4h` reg `0Ch`), and toggles bit 2 of
  `dest_seg_e+1` — a `0x2000`-byte page flip matching `EGA_PAGE_SIZE` in the port. It has
  exactly one call site, `2913:029A`. Every displayed frame is therefore complete.
- The per-tick frame gate is `loc_19` at `2913:028E`, which is the port's `async_start`
  case 12 line for line:

  ```
  2913:028E  loc_19:  mov  al, time_tick       ; time_tick @ 2F67h
  2913:0291           cmp  ds:last_tick, al    ; last_tick @ 4F8Ch
  2913:0295           je   loc_19              ; spin
  2913:0297           mov  ds:last_tick, al
  2913:029A           call flip_vp             ; (0506)
  ```

- Determinism begins at `2913:018B`. `2913:0185` is `cmp lastkey_tick,ch`; when the demo
  path is taken, `018B` starts the `rnd_state[i] = i` reset. The port does the same thing
  in `async_start` case 4. Before that point the original seeds `rnd_state` from the BIOS
  24-hour counter and is not reproducible; after it, it is.

Relevant guest addresses: `lastkey_tick` `2913:0973`, `rnd_state` `2913:2F68`,
`time_tick` `2913:2F67`, `last_tick` `2913:4F8C`, `dest_seg_e` `2913:4F8A`.

## Architecture

Four components. Only the first touches existing code.

### A. The seam — `src/bolo.h`, `src/shell_sokol.c`

`bolo.c` keeps all game logic. Its tail moves to `shell_sokol.c`: `bolo_init`,
`bolo_frame`, `bolo_cleanup`, `bolo_event`, `sokol_main`, `to_scan_code`, the sound
queue, the `state` struct, `ega_to_rgb`, and the sokol includes. After the move `bolo.c`
has no sokol dependency and links standalone.

```c
void bolo_reset(void);
void bolo_run_tick(int pump);           /* one timer tick, then `pump` async_start calls */
void bolo_key(uint8_t scancode);       /* == int_09h_entry */
const uint8_t *bolo_plane(int plane);  /* 8000 bytes of the completed frame; page 0,
                                          since the port pins dest_seg to 0 */
extern void (*bolo_sound_sink)(int ch_delay, int cl_length);
```

**`playBoloSound` splits along the right line.** Its PCM synthesis needs
`saudio_sample_rate()` and moves to the shell; what stays in `bolo.c` is a call to
`bolo_sound_sink(ch_delay, cl_length)`. The `(ch_delay, cl_length)` pair is the original
PC-speaker semantic — loop delay and duration — so the seam yields the right sound
artifact as a by-product rather than as extra machinery.

**`bolo_run_tick()` pumping.** In the shipping app `async_start()` is called once per
rendered frame (~60Hz) while ticks arrive at 18.2Hz, so roughly 3-4 calls per tick. Of the
13 `return AS_UNWIND` sites, all but one are tick-gated and therefore self-limiting; the
sole plain yield is `async_init_maze` state 0, which draws the GENERATING MAZE bitmap and
unwinds so the message appears before generation runs.

This does not affect the comparison, for two reasons:

- **Gameplay is already tick-locked.** Case 12 is a hard gate. The first call after a tick
  advances runs exactly one iteration (case 9 -> 11 -> 12) and returns with state 12; every
  later call that tick re-enters at case 12 and returns immediately. Extra calls are exact
  no-ops — including `handle_kbd` -> `getkey` -> `--reckey_delay`, so recorded demo input is
  consumed once per tick, not once per frame.
- **The intro is never compared.** `flip_vp` has one call site, inside the gameplay loop.
  Title screen, level select and maze generation produce no flips; they draw straight to
  visible page 0. Capture triggers on `029A`, so comparison starts at the first gameplay
  frame. Maze generation is synchronous in the port too — `async_gen_maze`'s `for(;;)` runs
  to completion in one call, matching the original.

So:

```c
int_08h_entry();
for (int i = 0; i < pump; ++i) async_start();
```

`pump` defaults to 4 (the app's ~60/18.2 ratio), exposed as `--pump N`. During gameplay any
`N >= 1` is byte-identical; in the intro `N` only changes how many ticks pass before
gameplay begins. A test asserts this: the same trace at `--pump 1` and `--pump 8` must
produce identical output.

Note: an earlier draft proposed pumping until `last_tick == time_tick`. That is wrong —
`last_tick` is assigned only in case 12, which is reached only during gameplay, so the loop
would spin until its guard expired on every intro tick.

### B. `emu/i8086.[ch]` — the CPU

Registers, flags, ModRM decoding, ~60 opcodes, segment-override and `REP` prefixes. All
bus access through callbacks (`read8`, `write8`, `in8`, `out8`). Knows nothing about EGA.

- `i8086_step()` executes one instruction.
- `i8086_interrupt(vec)` pushes FLAGS/CS/IP and vectors *through the guest IVT* —
  required, since BOLO installs its own `INT 08h`/`INT 09h` handlers via DOS AH=25h.

### C. `emu/machine.[ch]` — the minimal XT

1MB memory with EGA planes `uint8_t plane[4][0x10000]` mapped at A0000, honoring:

- sequencer map mask (`3C4h` idx 2) — which planes a write lands in
- graphics controller data rotate/function (`3CEh` idx 3) and read map select (`3CEh` idx 4),
  plus the four read latches
- CRTC start address high (`3D4h` idx `0Ch`) — the visible page
- `3DAh` retrace status — `flip_vp` spins on `test al,8 / jz`, so bit 3 must become set;
  simplest correct model is to alternate it on each read
- `60h` keyboard data (injected scancode), `61h` speaker (toggles logged, unused for now),
  `20h` EOI (ignored)

Interrupt services: `INT 10h` AH=00h (set mode 0Dh) and AH=05h (set display page),
`INT 21h` AH=25h, `INT 20h` terminate. Anything outside this set is a hard error.

### D. `emu/bolotest.c` — the driver

Loads `disasm/BOLO.COM` at 0100h, runs to the sync point, then advances both sides one
tick at a time and compares. The port runs natively in the same process via `bolo.h`.

## Run loop

### Tick delivery without cycle counting

Counting 8088 cycles to 262,000 per tick would need a per-opcode cycle table and buys
nothing at screen granularity. Instead, **fire `INT 08h` when the guest is spinning at
`028E`** — the game announces its own readiness for the next tick. One harness step is
one game frame on both sides, with no timing model.

This defers sound: the original's pitch comes from a bit-banged cycle-counted loop, so
audio comparison would bring cycle accounting back. Consistent with screen-level-only scope.

### Startup and synchronization

1. Load `BOLO.COM` at 0100h; `CS=DS=ES=SS`, `SP=FFFEh`. BIOS tick counter at 40:6Ch fixed to 0.
2. Run free. The original checks memory/EGA, seeds `rnd_state`, hooks INT 08h/09h, sets
   mode 0Dh, and shows the title screen. All nondeterministically seeded and irrelevant —
   no keys are injected, so it times out into the attract demo.
3. **Arm the harness at `018Bh`.** Both sides now hold identical deterministic state.
   No captures happen yet: the intro phases produce no `flip_vp` calls, so the first
   captured frame is the first gameplay frame.

### Capture and comparison

Trigger on the guest reaching `029A`. At that instant the page being drawn — named by
`dest_seg_e` at `4F8Ah`, read directly out of guest memory — holds the completed frame
about to be flipped in. The port's equivalent is page 0 after `bolo_run_tick()` returns
(the port pins `dest_seg` to 0 and has `flip_vp` commented out).

Compare 4 planes x 8000 bytes = 32000 bytes. The guest's `time_tick` at `2F67h` labels
the frame.

### Artifacts

On divergence, report the tick number, the differing byte count, and the bounding box of
differing pixels; write `orig-NNNN.ppm`, `port-NNNN.ppm`, `diff-NNNN.ppm`, and the raw
`.planes` for scripted diffing. PPM keeps this dependency-free — a short header plus RGB
bytes, no zlib or CRC work. Both sides render through the port's `g_ega_palette`, so any
visible difference is a real plane difference rather than a palette artifact.

### The ratchet

`bolotest` exits 0 if it reaches the tick limit clean, nonzero on divergence. The current
known-good tick count lives in `emu/baseline.txt`; the test fails if the number regresses
and reports when it improves.

```
bolotest [--ticks N] [--dump-every N] [--out DIR] [--continue-past-diff]
```

## Error handling

Nothing degrades gracefully — the tool's entire value is that a difference means
something. Every unknown aborts, and because the LST is available the aborts name the
fix:

```
unimplemented opcode D5h at 2913:1A2C: "aad 0Ah"  (BOLO.LST:3421)
```

Same for an unhandled port, `INT` vector, or BIOS function. Two runaway guards on the
interpreter: the guest executing outside the loaded image aborts, and a per-frame
instruction budget (~5M) aborts with "guest never reached `028E`". The port side needs no
guard — `bolo_run_tick` pumps a fixed count, so it cannot spin.

The harness also aborts if the port stops producing frames while the original keeps
going (or the reverse), rather than silently comparing a stale buffer.

## Testing the harness

1. **Decoder oracle — build first.** Parse `BOLO.LST` and assert the decoder's instruction
   length and mnemonic at every one of the 3302 addresses. No execution required,
   exhaustive over exactly the input that matters. Compare length and mnemonic only;
   reproducing Sourcer's operand syntax is not worth it, and length alone catches nearly
   every decode error.
2. **ALU and flag unit tests.** Table-driven cases across
   `add/sub/cmp/and/or/xor/test/inc/dec/neg/shl/shr/sar/rol/rcl/rcr/mul/div`, checking all
   six flags. Where the 8086 is genuinely undefined (flags after `mul`, `OF` on multi-bit
   shifts), document the choice; if BOLO depends on it, the screen diff surfaces it.
3. **Golden trace of the interpreter itself.** Once the original runs, commit its first N
   frames so interpreter refactors cannot silently change behavior, independent of the port.
4. **Pump invariance.** The same trace run at `--pump 1` and `--pump 8` must produce
   identical output, asserting that the port's gameplay loop is frame-rate independent.
5. **The thing itself.** The original must reach the title screen and demo and render
   recognizable BOLO. Eyeball the PPMs once; the golden trace locks it thereafter.

Plain assert-based C programs under CTest (`enable_testing()` + `add_test`) — no
framework, matching the repo's zero-dependency habit.

## Layout and build

```
emu/  i8086.c/.h      CPU, no hardware knowledge
      machine.c/.h    EGA planes, 7 ports, interrupt services
      lst.c/.h        BOLO.LST parser (decoder test + error messages)
      bolotest.c      driver / CLI
      test_decode.c   decoder oracle
      test_alu.c      flag semantics
      baseline.txt    the ratchet
src/  bolo.c          game logic, tail removed
      bolo.h          the seam
      shell_sokol.c   native + Wasm front end
```

`bolotest` links `bolo.c` plus the emu sources and is excluded under Emscripten.
`BOLO.COM`'s path is compiled in from `${CMAKE_SOURCE_DIR}/disasm/BOLO.COM` so tests run
from any build directory.

## Milestones

1. Extract the shell — pure refactor; native and Wasm still build and run.
2. LST parser and decoder; `test_decode` green over all 3302 instructions.
3. Execution: ALU, flags, `test_alu`.
4. Machine: EGA, ports, interrupt services. Original boots to the title screen; eyeball a PPM.
5. Driver: sync at `018Bh`, capture at `029A`, compare, artifacts, baseline ratchet.

## Known limitations

- **One run per process.** `bolo.c`'s state lives in file-scope statics initialized once at
  program start. Measured from the object file, that is 7 non-zero-initialized symbols
  (`strb_score`, `rnd_state`, `lsel_mstr`, `strb_hisco`, `reckey_delay`, `reckey_offset`,
  `g_ega_palette`; 311 bytes total) plus zero-initialized state that, once the shell is
  extracted, is `g_ega_screen` and the game arrays — roughly 75KB.

  A hand-written `bolo_reset()` would therefore be short (~30 lines), but the objection is
  silent drift, not size: a global added later with an initializer and not mirrored into
  the reset makes run 2 differ from run 1 with nothing to announce it, corrupting exactly
  the measurement this tool produces. So `bolo_reset()` aborts if called twice and the CLI
  runs one trace per invocation. If multiple runs per invocation are ever wanted, `fork()`
  per run is the answer — the OS snapshots everything including state nobody enumerated,
  it is ~15 lines, and drift becomes impossible. The interpreter side needs none of this;
  its state is one struct, reset by `memset` plus reloading `BOLO.COM`.

  Scope note: the initial deliverable needs a single run. The attract demo is one canonical
  scenario — `reckeys` opens with `0x0306` (scancode 6, `'5'`, after a 3-tick delay), so the
  recorded script types its own level and density selection.
- **The port will diverge early at first.** It is incomplete, carries `HACK`/`HACK2`/`HACK3`
  toggles, and has `flip_vp` disabled. Early divergence is the expected starting state, and
  the baseline number is the thing that improves.
- **8086 undefined flag behavior** is a judgement call in a few places; documented in code
  where it arises.

## Out of scope

Sound comparison (needs cycle accounting), routine-level differential testing, EGA
write-stream diffing, cycle-accurate timing, and anything the program does not use —
186+ instructions, PIT, DMA, disk, real DOS.
