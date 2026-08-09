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
    fprintf(
        stderr,
        "FAIL: never reached the sync point: %s\n",
        runner_error(r) ? runner_error(r) : "(no error message)");
    return 1;
  }

  // time_tick must have advanced past the title screen's 3Ch-tick deadline.
  if (runner_time_tick(r) < 0x3C) {
    fprintf(
        stderr,
        "FAIL: time_tick is %u, expected at least 3Ch by the sync point\n",
        runner_time_tick(r));
    return 1;
  }

  // Then the demo must actually produce frames.
  for (unsigned i = 0; i != 10; ++i) {
    res = runner_run_to_frame(r);
    if (res != RUN_FRAME) {
      fprintf(
          stderr,
          "FAIL: frame %u: %s\n",
          i,
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
        if (plane[i]) {
          anySet = true;
          break;
        }
    }
    if (!anySet) {
      fprintf(stderr, "FAIL: all four captured planes are blank\n");
      return 1;
    }
  }

  printf(
      "runner ok: reached sync at tick %u, captured %u frames\n",
      runner_time_tick(r),
      runner_frame_count(r));
  runner_destroy(r);
  return 0;
}
