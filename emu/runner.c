#include "runner.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Where a .COM image is loaded inside its segment.
#define RUNNER_IMAGE_ORIGIN 0x0100

struct Runner {
  Machine *machine;
  I8086 *cpu;

  unsigned frameCount;
  /// Consecutive instructions that have stored nothing. See IDLE_THRESHOLD.
  unsigned idleRun;
  /// One past the last byte of the loaded image, as an offset in the load
  /// segment. Executing outside [0100h, this) means the guest went wild.
  uint32_t imageEnd;

  char errorBuf[256];
  bool hasError;

  /// The last completed frame, copied out of the page the guest had been
  /// drawing into so that it survives the guest drawing the next one.
  uint8_t plane[EGA_PLANES][EGA_PAGE_VISIBLE];
};

static RunResult runner_fail(Runner *r, const char *fmt, ...) {
  // Keep the first failure: it is the one that explains the rest.
  if (!r->hasError) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(r->errorBuf, sizeof(r->errorBuf), fmt, ap);
    va_end(ap);
    r->hasError = true;
  }
  return RUN_ERROR;
}

/// Size of `path` in bytes, or 0 if it cannot be measured.
static long file_size(const char *path) {
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  long size = fseek(f, 0, SEEK_END) == 0 ? ftell(f) : 0;
  fclose(f);
  return size > 0 ? size : 0;
}

Runner *runner_create(const char *comPath) {
  long size = file_size(comPath);
  if (size <= 0 || size > 0x10000 - RUNNER_IMAGE_ORIGIN)
    return NULL;

  Runner *r = (Runner *)calloc(1, sizeof(Runner));
  if (!r)
    return NULL;

  r->machine = machine_create();
  if (!r->machine || !machine_load_com(r->machine, comPath, MACHINE_LOAD_SEG)) {
    runner_destroy(r);
    return NULL;
  }
  r->cpu = machine_cpu(r->machine);
  r->imageEnd = (uint32_t)(RUNNER_IMAGE_ORIGIN + size);
  return r;
}

void runner_destroy(Runner *r) {
  if (!r)
    return;
  machine_destroy(r->machine);
  free(r);
}

/// Copy the page the guest has just finished drawing. Called with the CPU
/// inside flip_vp but before flip_vp's body has run, so dest_seg_e is still
/// what it was for the whole frame and machine_draw_page() still names the
/// page that frame went into.
static void runner_capture(Runner *r) {
  int page = machine_draw_page(r->machine);
  for (int p = 0; p != EGA_PLANES; ++p)
    memcpy(r->plane[p], machine_plane(r->machine, p, page), EGA_PAGE_VISIBLE);
}

/// What runner_run() is waiting for.
typedef enum StopKind {
  STOP_SYNC, ///< RUNNER_SYNC_IP, no capture.
  STOP_FRAME, ///< RUNNER_FRAME_IP, capture the planes.
} StopKind;

static RunResult runner_run(Runner *r, StopKind stop) {
  if (r->hasError)
    return RUN_ERROR;

  uint16_t stopIp = stop == STOP_SYNC ? RUNNER_SYNC_IP : RUNNER_FRAME_IP;

  for (unsigned long executed = 0;; ++executed) {
    if (executed >= FRAME_INSN_BUDGET) {
      return runner_fail(
          r,
          "guest never reached %04Xh: %lu instructions, now at %04X:%04X",
          stopIp,
          executed,
          r->cpu->sreg[I8086_CS],
          r->cpu->ip);
    }

    uint16_t cs = r->cpu->sreg[I8086_CS];
    uint16_t ip = r->cpu->ip;

    // A wild jump inside the code segment. Other segments are legitimate: the
    // INT 08h/09h stubs live at 0000:0500 and the EGA window is entered
    // through ES, never CS.
    if (cs == MACHINE_LOAD_SEG && (ip < RUNNER_IMAGE_ORIGIN || ip >= r->imageEnd)) {
      return runner_fail(
          r,
          "guest left the loaded image: CS:IP = %04X:%04X, image is %04Xh..%04Xh",
          cs,
          ip,
          (unsigned)RUNNER_IMAGE_ORIGIN,
          (unsigned)r->imageEnd - 1);
    }

    uint64_t writesBefore = machine_write_count(r->machine);

    if (!i8086_step(r->cpu)) {
      return runner_fail(
          r,
          "cpu: %s at %05Xh (CS:IP was %04X:%04X)",
          r->cpu->error ? r->cpu->error : "(no message)",
          r->cpu->errorAddr,
          cs,
          ip);
    }

    // The machine reports an unimplemented port or service by setting
    // cpu->error from inside out8/in8/intercept, and those callbacks have no
    // way to fail the instruction -- i8086_step() returned true above. Without
    // this check the guest would run on past the machine's first gap and the
    // resulting divergence would be blamed on the port under test.
    if (machine_error(r->machine)) {
      return runner_fail(r, "machine: %s (at %04X:%04X)", machine_error(r->machine), cs, ip);
    }

    if (machine_exited(r->machine))
      return RUN_EXITED;

    if (machine_write_count(r->machine) == writesBefore)
      ++r->idleRun;
    else
      r->idleRun = 0;

    if (r->idleRun >= IDLE_THRESHOLD) {
      i8086_interrupt(r->cpu, 8);
      r->idleRun = 0;
    }

    // `ip` is the address of the instruction just executed, so the stop point
    // is observed with the frame complete and the call already taken.
    if (cs == MACHINE_LOAD_SEG && ip == stopIp) {
      if (stop == STOP_SYNC)
        return RUN_SYNCED;
      ++r->frameCount;
      runner_capture(r);
      return RUN_FRAME;
    }
  }
}

RunResult runner_run_to_sync(Runner *r) {
  return runner_run(r, STOP_SYNC);
}

RunResult runner_run_to_frame(Runner *r) {
  return runner_run(r, STOP_FRAME);
}

const uint8_t *runner_plane(const Runner *r, int plane) {
  return r->plane[plane];
}

unsigned runner_frame_count(const Runner *r) {
  return r->frameCount;
}

unsigned runner_time_tick(const Runner *r) {
  // time_tick, 2913:2F67.
  return machine_peek(r->machine, i8086_linear(MACHINE_LOAD_SEG, 0x2F67));
}

const Machine *runner_machine(const Runner *r) {
  return r->machine;
}

const char *runner_error(const Runner *r) {
  return r->hasError ? r->errorBuf : NULL;
}
