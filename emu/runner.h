// bolo - public domain Tzvetan Mikov 2021
//
// The loop that drives the original BOLO.COM under the machine: it steps the
// CPU, delivers timer ticks, watches for a runaway guest, and captures a copy
// of the EGA planes every time the game completes a frame.
//
// Nothing here knows anything about the C port; the comparison driver puts the
// two side by side.

#ifndef BOLO_RUNNER_H
#define BOLO_RUNNER_H

#include "machine.h"

/// Instructions with no store before the guest is considered idle.
///
/// There is no cycle model and no programmable interval timer here, so INT 08h
/// cannot be delivered on a schedule. Instead it is delivered whenever the
/// guest stops doing work, which is exactly when it is waiting for a tick.
///
/// The guest has two such waits and they are at different addresses: the title
/// screen spins on time_tick at 2913:11E5 (3 instructions) and gameplay spins
/// on it at 2913:028E (3 instructions), both reading only. Firing at a single
/// hardcoded address would leave the other one hung forever, so the rule is
/// generic: 64 consecutive instructions that store nothing means idle. No real
/// work loop in this program runs that long without writing memory or a port.
#define IDLE_THRESHOLD 64

/// Instructions to run in one frame before declaring the guest hung. BOLO does
/// roughly 250k per frame; 5M is a factor of 20 of headroom.
///
/// Raising this is almost never the right fix. When it trips, the reported
/// CS:IP names an address in disasm/BOLO.LST, and that address names whatever
/// the machine is not implementing.
#define FRAME_INSN_BUDGET 5000000

/// 2913:018B, the instruction after the title screen's "did a key end it?"
/// test. Reaching it means the title screen timed out rather than being
/// dismissed, and the rnd_state reset that follows makes the attract demo
/// deterministic from there on.
#define RUNNER_SYNC_IP 0x018B

/// 2913:029A, the "call flip_vp" that ends a gameplay frame. Everything drawn
/// for the frame is already in the planes when this executes; flip_vp itself
/// only changes which page the CRTC displays.
#define RUNNER_FRAME_IP 0x029A

typedef enum RunResult {
  /// runner_run_to_frame: the guest completed a frame and it has been captured.
  RUN_FRAME,
  /// runner_run_to_sync: the guest reached RUNNER_SYNC_IP.
  RUN_SYNCED,
  /// The guest executed INT 20h. Not expected during the attract demo.
  RUN_EXITED,
  /// Something went wrong; runner_error() says what.
  RUN_ERROR,
} RunResult;

typedef struct Runner Runner;

/// Create a machine and load `comPath` at MACHINE_LOAD_SEG. Returns NULL if
/// the machine could not be allocated or the image could not be loaded.
Runner *runner_create(const char *comPath);
void runner_destroy(Runner *r);

/// Run until the guest reaches RUNNER_SYNC_IP. Captures nothing.
///
/// Exists so a test can assert the determinism point is reached, which also
/// proves the title screen timed out rather than hanging.
RunResult runner_run_to_sync(Runner *r);

/// Run until the guest completes a frame, then copy the four planes of the
/// page it just drew into the runner. Returns RUN_FRAME on success.
RunResult runner_run_to_frame(Runner *r);

/// EGA_PAGE_VISIBLE bytes of `plane` (0..3) as of the last captured frame.
/// The copy is the runner's own, so it survives the guest drawing the next
/// frame; before the first capture it is all zeroes.
const uint8_t *runner_plane(const Runner *r, int plane);

/// Frames captured by runner_run_to_frame so far.
unsigned runner_frame_count(const Runner *r);

/// The guest's time_tick byte at 2913:2F67 -- the counter its own INT 08h
/// handler increments and both wait loops compare against.
unsigned runner_time_tick(const Runner *r);

/// Non-NULL once a run has failed. Every RUN_ERROR sets it.
const char *runner_error(const Runner *r);

#endif // BOLO_RUNNER_H
