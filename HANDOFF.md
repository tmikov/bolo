# HANDOFF

Written 2026-08-08, at the end of the session that implemented plan 2 of the fidelity
harness. This describes a point in time — replace or delete it once plan 3 lands.

## Where things stand

Branch `work` at `f51b03e`, 26 commits ahead of `master`. `ctest` is 10/10 green from a
Ninja build, the build is warning-free under `-Wall -Wextra`, and
`clang-format --dry-run -Werror emu/*.c emu/*.h` exits 0.

**Plans 1 and 2 are complete.**

- **Plan 1** (the headless seam) split the sokol front end out of `src/bolo.c`. The game
  is the `bologame` library driven through `src/bolo.h`; `src/shell_sokol.c` is the
  windowed front end and `emu/bolotest.c` the headless one.
- **Plan 2** (this session) built the 8086 interpreter that will execute the original
  `disasm/BOLO.COM`. Nothing under `src/` or `disasm/` changed.

## Read these first, in this order

1. `docs/superpowers/specs/2026-08-07-bolo-fidelity-harness-design.md` — the design for
   all three plans. **This is the authority.** Its "What was verified about the original",
   "Alternatives considered" and "Known limitations" sections record decisions that were
   argued through once with evidence; don't re-litigate them without new information.
2. `docs/superpowers/plans/2026-08-08-bolo-8086-interpreter.md` — what plan 2 did and why.
   Its "Measured facts this plan is built on" section **supersedes the spec's estimates**
   (the spec's "3302 instructions, ~60 opcodes" was an early undercount).
3. `CLAUDE.md` — build commands, architecture, the `disasm/` workflow.

## What plan 2 delivered

| File | Lines | What it is |
| --- | --- | --- |
| `emu/lst.{c,h}` | 288 | Parser for `disasm/BOLO.LST`, the Sourcer disassembly. Yields 3439 `LstInsn` records. |
| `emu/i8086.{c,h}` | 1712 | The CPU: decoder, ALU, executor. Knows nothing about EGA or DOS. |
| `emu/test_lst.c` | 130 | Parser test, including a byte-for-byte check against `BOLO.COM`. |
| `emu/test_decode.c` | 114 | The decoder oracle. |
| `emu/test_alu.c` | 199 | 30 hand-derived flag vectors. |
| `emu/test_exec.c` | 577 | Execution tests over a flat-RAM toy machine. |

CMake targets `bolo_lst` (carrying `BOLO_LST_PATH` and `BOLO_COM_PATH` as PUBLIC compile
definitions) and `bolo_i8086`. Neither links `bologame`; only `test_decode` links both.

### Measured facts about the original, verified during plan 2

All measured from `disasm/BOLO.LST` and `disasm/BOLO.COM`, not estimated:

- **3439 instructions**, spanning offsets `0100h`..`2F53h`, no duplicate addresses, and
  every one's bytes match `BOLO.COM` exactly. That last property is what earns the LST the
  right to be the decoder's oracle, and `test_lst` asserts it.
- **169 distinct opcode keys** (opcode byte plus the ModRM `reg` field for group opcodes).
- Prefixes used: `rep` (F3) ×21, `es:` (26) ×31, `cs:` (2E) ×4. No `repne`, no `lock`,
  no `ss:`/`ds:` override.
- **Overflow is write-only in this program.** BOLO contains no signed conditional jump
  (`70`/`71` and `7C`-`7F` are absent), no `pushf`/`popf`, and `OF` is bit 11 so the single
  `lahf` cannot observe it. This retires a whole class of concern — see below.

## Two corrections plan 3 must not lose

**1. The frame alignment is off by one.** `bolo_frame_count()` increments at the game's
frame gate — the instant the original calls `flip_vp` at `2913:029A` — but control does not
return there. It falls through and draws another whole frame before `bolo_run_tick()`
returns. So the port's planes at `bolo_frame_count() == k` correspond to the original's
capture **k+1**, not k. This is stated in `src/bolo.h`'s `bolo_plane()` doc comment and in
the spec's "Capture and comparison" section. An earlier spec draft had it wrong.

**2. Do not chase the undefined-flag choices on a divergence.** An earlier draft of plan 2
named the interpreter's undefined-flag decisions (multi-bit shift `OF`, `mul`'s
`SF`/`ZF`/`AF`/`PF`) as the first thing to check if the comparison diverges near
`2913:1026`, `102A`, `0548`, `0627` or `1660`. That advice was wrong and would cost a
debugging session — worse, it invites a "fix" to flag code that was never at fault. As
measured above, `OF` is unobservable in this program, and all five of those sites have dead
flags besides. The choices stay documented at their definitions in `emu/i8086.c` for
correctness, not as a debugging lead.

## Next up: plan 3 — the machine and the comparison driver

Scope: `emu/machine.{c,h}`, `emu/baseline.txt`, and growing `emu/bolotest.c` from a frame
dumper into the comparison driver.

The final review of plan 2 walked each plan-3 requirement against the delivered API and
found it fit. Specifically:

- **EGA read latches work.** Every data read reaches `read8`, and read-modify-write does a
  genuine bus read at the operand address before its write — which is how the latches load.
  `cmp` and `test` correctly read *without* writing back, because on real hardware storing
  an unchanged value to video memory is not a no-op.
- **A 16-bit access decomposes into two byte accesses, low then high**, which is what an
  8-bit-bus EGA actually sees.
- **`i8086_interrupt()` vectors through the guest's own IVT**, which is required: BOLO
  installs its `INT 08h`/`09h` handlers via DOS AH=25h.
- **`bool (*intercept)(void *ctx, uint8_t vec)`** on `struct I8086` lets the machine service
  `INT 10h`/`21h`/`20h` natively — return true and the CPU resumes after the `INT` with no
  frame pushed. Added specifically so plan 3 does not have to put "how an INT is encoded"
  inside `machine.c`.
- **`i8086_reset()` deliberately does not touch the bus callbacks**, so a caller may install
  them before or after resetting. This is a documented guarantee, not an accident.
- **The runaway guards are implementable from the caller's loop.** `cpu->sreg[CS]`/`cpu->ip`
  settle before each step, so both the spec's guards — the guest executing outside the loaded
  image (linear `0x29230`..`0x2B083`), and "never reached `028E`" — are just checks in the
  driver loop. One caveat: a `rep` executes its entire run inside a single `i8086_step()`, so
  an instruction budget under-counts by up to 65535 per string instruction. `CX` bounds it, so
  nothing hangs; the budget is just coarser than it looks.
- **A code fetch cannot pollute the EGA latches.** `i8086_step` reads 6 bytes at `CS:IP`, and
  BOLO's code occupies linear `0x29230`-`0x2B083`, so the overrun reaches at most `0x2B089` —
  nowhere near the A0000 window.

Nothing else should require reopening `emu/i8086.{c,h}`. The one thing that might: every
`i8086_step` issues six `read8` callbacks regardless of instruction length and computes a
mnemonic the executor never reads. At BOLO's ~250k instructions per tick, a long ratchet run
is a lot of wasted callbacks. **Don't act on this until it's measured** — if it does bite,
the fix (skip the mnemonic unless asked; add a direct-pointer fetch fast path) is local.

## Known gaps, deliberately left

- `test_exec` covers ~20 scenarios against ~60 opcode families, so most dispatch paths have
  no committed test. The exhaustive decode oracle covers *decoding* of all 3439 instructions;
  the execution tests deliberately target the machinery the decode and ALU tests cannot reach,
  plus the wiring judged riskiest (`xchg`, the `D0`-`D3` shifts, `mul`). Still uncovered and
  judged low-risk: the BP→SS segment default (BOLO is a `.COM` where `DS == SS == CS`, so it
  cannot manifest), `lds`/`les` (`les` has zero uses; `lds` runs twice, on the clean-exit path
  only), and the string-source override.
- `read16`/`write16` wrap at the 1 MB linear boundary rather than within the 64 KB segment.
  Requires a word access at offset `FFFF` to matter. Recorded in a comment at the definition
  so a future divergence hunt finds it rather than rediscovering it.
- `i8086_not` has no test. Trivial, no flags, and BOLO contains zero `not` instructions.
- `g_errorMsg` is one process-wide static buffer. Fine for one `I8086`; revisit if plan 3
  ever instantiates two.

## Environment facts

- **Always configure CMake with `-G Ninja`.** Required; see `CLAUDE.md`.
- **This box is headless** — no `DISPLAY`, no Xvfb. `./build/src/bolo` reaches X11 init and
  aborts with `XOpenDisplay() failed!`. The windowed app cannot be verified here.
- **GCC is the default `cc` and is stricter than clang here.** Plan 2's first task shipped
  two `-Wformat-truncation` warnings that clang did not emit. Check with a clean build and a
  count, not a grep for one word:
  `cmake --build <dir> 2>&1 | grep -c warning` should print `0`.
- **`clang-format` 18.1.3 is installed.** Judge it by **exit status** — with `-Werror` it
  emits `error:`, not `warning:`, so grepping for warnings reports a dirty file as clean.
  Also, it finds `.clang-format` relative to the file's real path, so checking a copy in a
  scratch directory silently falls back to LLVM style and invents violations.
- **Python PIL is available**, the fastest way to actually look at a frame:
  `python3 -c "from PIL import Image; Image.open('frames/frame-00040.ppm').save('/tmp/f.png')"`
- **Ignore the LSP for `emu/`.** There is no `compile_commands.json`, so clangd reports
  spurious "file not found" and "undeclared identifier" errors — including for
  `BOLO_LST_PATH`/`BOLO_COM_PATH`, which come from CMake. Trust the build.

## Verification habits that paid off

1. **Build the oracle before the thing it judges.** Validating the decoder against 3439 real
   instructions before writing any execution logic means a decode bug can never be
   misdiagnosed as an execution bug later. The LST's byte-for-byte cross-check against
   `BOLO.COM` is what earns it that authority.
2. **Mutation-test a new test before trusting it.** Every execution test added late in plan 2
   was checked by deliberately breaking the code it covers and confirming it failed. One case
   (`shr F000h,1`) happens to leave the correct low byte, so only the separate high-byte
   assertion catches it — that would have been a silently vacuous test.
3. **Treat subagent reports as claims, and re-verify what matters.** Every task's build,
   tests and formatting were re-run independently before review. Two plan defects surfaced
   this way — an undefined-behavior test pattern, and a comment promising coverage that did
   not exist.
4. **Judge tools by exit status, not by grepping their output.** See `clang-format` above.
5. **Measure the binary instead of reasoning about it.** "Does BOLO ever observe `OF`?" was
   answered by counting opcodes, and the answer retired a whole class of concern.
