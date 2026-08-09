// Run the game headlessly and dump each completed frame as a PPM.
//
// The game's attract demo needs no input: the title screen times out, and the
// recorded key script in bolo.c drives level selection and play.
//
// With --original the same is done for the original BOLO.COM running under the
// emulated machine instead of the C port. Both sides render through the same
// palette, so any visible difference between their PPMs is a real difference
// in the EGA planes.
//
// With --compare both are run at once and their planes are compared frame by
// frame, which is the question the whole harness exists to answer: at which
// frame does the port first stop matching the original?

#include "bolo.h"
#include "ega_render.h"
#include "ppm.h"
#include "runner.h"

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static void usage(const char *argv0) {
  fprintf(
      stderr,
      "usage: %s [--frames N] [--pump N] [--out DIR] [--max-ticks N]\n"
      "       %s --original DIR [--frames N] [--golden FILE] [--check-golden FILE]\n"
      "       %s --compare [--ticks N] [--pump N] [--out DIR] [--baseline FILE]\n"
      "                    [--continue-past-diff]\n"
      "\n"
      "  --frames N          stop after N completed frames (default 100)\n"
      "  --pump N            async_start() calls per tick (default 4)\n"
      "  --out DIR           directory for frame-NNNNN.ppm (default \"frames\")\n"
      "  --max-ticks N       give up after N ticks (default 100000)\n"
      "\n"
      "  --original DIR      run the original BOLO.COM instead of the port,\n"
      "                      writing its frames to DIR\n"
      "  --golden FILE       with --original, write one checksum line per frame\n"
      "  --check-golden FILE compare those lines against a committed trace\n"
      "\n"
      "  --compare           run both sides and compare their planes per frame\n"
      "  --ticks N           with --compare, run at most N timer ticks (default 1000)\n"
      "  --baseline FILE     with --compare, the highest frame count known to match\n"
      "  --continue-past-diff  keep comparing after the first divergence\n",
      argv0,
      argv0,
      argv0);
}

/// Parse a positive integer argument, exiting on anything malformed.
static long parse_positive(const char *what, const char *text) {
  char *end;
  long value = strtol(text, &end, 10);
  if (*text == '\0' || *end != '\0' || value <= 0 || value > INT_MAX) {
    fprintf(stderr, "%s: expected a positive integer, got \"%s\"\n", what, text);
    exit(2);
  }
  return value;
}

/// Create `dir` if it is not already a directory. Returns false on failure.
///
/// An existing directory is reused as-is and never cleared, so stale frames
/// from a previous run can remain alongside the new ones.
static bool ensure_directory(const char *dir) {
  if (mkdir(dir, 0777) == 0)
    return true;
  struct stat st;
  return stat(dir, &st) == 0 && S_ISDIR(st.st_mode);
}

/// FNV-1a, 64-bit. Chained across the four planes so that one number covers
/// the whole frame, with enough spread that a single flipped pixel shows.
static uint64_t fnv1a(const uint8_t *data, size_t len, uint64_t hash) {
  for (size_t i = 0; i != len; ++i) {
    hash ^= data[i];
    hash *= 0x100000001B3ULL;
  }
  return hash;
}

#define FNV1A_SEED 0xCBF29CE484222325ULL

/// Compare one generated golden line against the next line of `expected`.
/// Returns false, having reported the difference, if they disagree.
static bool golden_line_matches(FILE *expected, const char *path, long frame, const char *line) {
  char want[256];
  if (!fgets(want, sizeof(want), expected)) {
    fprintf(stderr, "%s: ended at frame %ld, but the run produced more\n", path, frame);
    return false;
  }
  want[strcspn(want, "\r\n")] = '\0';
  if (strcmp(want, line) == 0)
    return true;
  fprintf(
      stderr, "%s: frame %ld differs\n  expected: %s\n  got:      %s\n", path, frame, want, line);
  return false;
}

/// Run the original under the machine, dumping frames and optionally a
/// checksum trace. Returns a process exit code.
static int
run_original(const char *dir, long frames, const char *goldenPath, const char *checkPath) {
  Runner *r = runner_create(BOLO_COM_PATH);
  if (!r) {
    fprintf(stderr, "cannot load %s\n", BOLO_COM_PATH);
    return 1;
  }

  // Everything before the sync point is the title screen and the level
  // editor replaying recorded keys; the demo proper starts here.
  // RUN_EXITED leaves no error message, so it needs its own text: passing the
  // NULL from runner_error() to %s would be undefined.
  RunResult sync = runner_run_to_sync(r);
  if (sync != RUN_SYNCED) {
    fprintf(
        stderr,
        "the original never reached its sync point: %s\n",
        sync == RUN_EXITED ? "it exited" : runner_error(r));
    runner_destroy(r);
    return 1;
  }

  FILE *golden = NULL;
  if (goldenPath && !(golden = fopen(goldenPath, "w"))) {
    fprintf(stderr, "cannot write %s\n", goldenPath);
    runner_destroy(r);
    return 1;
  }
  FILE *expected = NULL;
  if (checkPath && !(expected = fopen(checkPath, "r"))) {
    fprintf(stderr, "cannot read %s\n", checkPath);
    if (golden)
      fclose(golden);
    runner_destroy(r);
    return 1;
  }

  int status = 0;
  for (long captured = 0; captured != frames; ++captured) {
    RunResult res = runner_run_to_frame(r);
    if (res != RUN_FRAME) {
      fprintf(
          stderr,
          "the original stopped after %ld frames: %s\n",
          captured,
          res == RUN_EXITED ? "it exited" : runner_error(r));
      status = 1;
      break;
    }

    const uint8_t *planes[EGA_PLANES];
    uint64_t hash = FNV1A_SEED;
    for (int plane = 0; plane != EGA_PLANES; ++plane) {
      planes[plane] = runner_plane(r, plane);
      hash = fnv1a(planes[plane], EGA_PAGE_VISIBLE, hash);
    }

    char path[1024];
    snprintf(path, sizeof(path), "%s/frame-%05ld.ppm", dir, captured);
    if (!ppm_write_planes(path, planes, bolo_palette())) {
      fprintf(stderr, "failed to write %s\n", path);
      status = 1;
      break;
    }

    char line[256];
    snprintf(
        line,
        sizeof(line),
        "%05ld %02X %016llX",
        captured,
        runner_time_tick(r),
        (unsigned long long)hash);
    if (golden)
      fprintf(golden, "%s\n", line);
    if (expected && !golden_line_matches(expected, checkPath, captured, line)) {
      status = 1;
      break;
    }
  }

  if (golden)
    fclose(golden);
  if (expected)
    fclose(expected);
  if (status == 0)
    printf("captured %ld frames from the original\n", frames);
  runner_destroy(r);
  return status;
}

/// What one frame's comparison found.
typedef struct FrameDiff {
  /// Differing bytes, counted across all four planes.
  long bytes;
  /// Bounding box of the differing *pixels*, valid only when bytes != 0.
  int minX, maxX, minY, maxY;
} FrameDiff;

/// Bytes compared per frame: four planes of one page's visible area.
#define COMPARE_BYTES ((long)EGA_PLANES * EGA_PAGE_VISIBLE)

/// Pixels of byte offset `off` that differ between the two sides: bit 7 is the
/// leftmost pixel, matching the EGA's own layout. A pixel differs if any plane
/// disagrees about it.
static uint8_t
diff_mask(const uint8_t *const a[EGA_PLANES], const uint8_t *const b[EGA_PLANES], unsigned off) {
  uint8_t mask = 0;
  for (int plane = 0; plane != EGA_PLANES; ++plane)
    mask = (uint8_t)(mask | (a[plane][off] ^ b[plane][off]));
  return mask;
}

/// Compare two sets of planes, counting differing bytes and bounding the
/// differing pixels. A byte offset maps to (x, y) through EGA_STRIDE, which is
/// what turns "214 bytes differ" into "a 40x8 box at (96,40)".
static FrameDiff compare_planes(
    const uint8_t *const orig[EGA_PLANES],
    const uint8_t *const port[EGA_PLANES]) {
  FrameDiff d = {0, EGA_WIDTH, -1, EGA_HEIGHT, -1};

  for (unsigned off = 0; off != EGA_PAGE_VISIBLE; ++off) {
    for (int plane = 0; plane != EGA_PLANES; ++plane) {
      if (orig[plane][off] != port[plane][off])
        ++d.bytes;
    }

    uint8_t mask = diff_mask(orig, port, off);
    if (!mask)
      continue;

    int y = (int)(off / EGA_STRIDE);
    int xBase = (int)(off % EGA_STRIDE) * 8;
    for (int bit = 7; bit >= 0; --bit) {
      if (!(mask & (1u << bit)))
        continue;
      int x = xBase + (7 - bit);
      if (x < d.minX)
        d.minX = x;
      if (x > d.maxX)
        d.maxX = x;
      if (y < d.minY)
        d.minY = y;
      if (y > d.maxY)
        d.maxY = y;
    }
  }

  return d;
}

/// How much of the original's own color survives in the diff image.
#define DIFF_DIM_NUMERATOR 3
#define DIFF_DIM_DENOMINATOR 10

/// Write the diff image: the original, dimmed, with every differing pixel in
/// one high-contrast color so the eye goes straight to it.
static bool write_diff_ppm(
    const char *path,
    const uint8_t *const orig[EGA_PLANES],
    const uint8_t *const port[EGA_PLANES],
    const RGBA8 *palette) {
  FILE *f = fopen(path, "wb");
  if (!f)
    return false;

  if (fprintf(f, "P6\n%d %d\n255\n", EGA_WIDTH, EGA_HEIGHT) < 0) {
    fclose(f);
    return false;
  }

  uint8_t row[EGA_WIDTH * 3];
  RGBA8 rowRgba[EGA_WIDTH];

  for (unsigned y = 0; y != EGA_HEIGHT; ++y) {
    ega_row_to_rgba(orig, palette, y, rowRgba);

    uint8_t *out = row;
    for (unsigned x = 0; x != EGA_WIDTH; ++x) {
      unsigned off = y * EGA_STRIDE + x / 8;
      bool differs = (diff_mask(orig, port, off) >> (7 - x % 8)) & 1;
      if (differs) {
        *out++ = 255;
        *out++ = 0;
        *out++ = 255;
      } else {
        *out++ = (uint8_t)(rowRgba[x].r * DIFF_DIM_NUMERATOR / DIFF_DIM_DENOMINATOR);
        *out++ = (uint8_t)(rowRgba[x].g * DIFF_DIM_NUMERATOR / DIFF_DIM_DENOMINATOR);
        *out++ = (uint8_t)(rowRgba[x].b * DIFF_DIM_NUMERATOR / DIFF_DIM_DENOMINATOR);
      }
    }

    if (fwrite(row, 1, sizeof(row), f) != sizeof(row)) {
      fclose(f);
      return false;
    }
  }

  return fclose(f) == 0;
}

/// Write orig-NNNNN.ppm, port-NNNNN.ppm and diff-NNNNN.ppm into `dir`.
static bool write_divergence_images(
    const char *dir,
    long frame,
    const uint8_t *const orig[EGA_PLANES],
    const uint8_t *const port[EGA_PLANES]) {
  char path[1024];
  const RGBA8 *palette = bolo_palette();

  snprintf(path, sizeof(path), "%s/orig-%05ld.ppm", dir, frame);
  if (!ppm_write_planes(path, orig, palette))
    return false;
  snprintf(path, sizeof(path), "%s/port-%05ld.ppm", dir, frame);
  if (!ppm_write_planes(path, port, palette))
    return false;
  snprintf(path, sizeof(path), "%s/diff-%05ld.ppm", dir, frame);
  return write_diff_ppm(path, orig, port, palette);
}

/// Read the ratchet's single integer. Returns false, having reported why, if
/// the file cannot be read or does not hold exactly one non-negative number.
///
/// The trailing check matters more than it looks: this file is the project's
/// fidelity metric, so "5abc" quietly parsing as 5 would silently lower the
/// bar the whole harness is measured against.
static bool read_baseline(const char *path, long *value) {
  FILE *f = fopen(path, "r");
  if (!f) {
    fprintf(stderr, "cannot read baseline \"%s\"\n", path);
    return false;
  }
  long parsed = -1;
  bool ok = fscanf(f, "%ld", &parsed) == 1 && parsed >= 0;
  for (int c; ok && (c = fgetc(f)) != EOF;)
    ok = isspace(c) != 0;
  fclose(f);
  if (!ok) {
    fprintf(stderr, "\"%s\": expected a single non-negative integer\n", path);
    return false;
  }
  *value = parsed;
  return true;
}

/// Run the port until bolo_frame_count() == frame, spending at most the
/// remaining tick budget. Returns false if the budget ran out first.
///
/// `*spent` receives the ticks this call consumed, which is what separates
/// "the budget was simply too small" from "the port stopped producing frames":
/// the former spends a frame's worth of ticks, the latter the whole remainder.
static bool
advance_port_to_frame(unsigned frame, long pump, long *ticks, long maxTicks, long *spent) {
  long start = *ticks;
  while (bolo_frame_count() < frame && *ticks < maxTicks) {
    bolo_run_tick((int)pump);
    ++*ticks;
  }
  *spent = *ticks - start;
  return bolo_frame_count() >= frame;
}

/// Run both sides and compare their planes frame by frame. Returns a process
/// exit code: nonzero only on a regression against the baseline or on a
/// failure to run either side.
static int run_compare(
    const char *dir,
    long maxTicks,
    long pump,
    const char *baselinePath,
    bool continuePastDiff) {
  long baseline = 0;
  if (baselinePath && !read_baseline(baselinePath, &baseline))
    return 1;

  Runner *r = runner_create(BOLO_COM_PATH);
  if (!r) {
    fprintf(stderr, "cannot load %s\n", BOLO_COM_PATH);
    return 1;
  }

  // Same starting point as --original: everything before the sync point is the
  // title screen and the level editor replaying recorded keys.
  RunResult sync = runner_run_to_sync(r);
  if (sync != RUN_SYNCED) {
    fprintf(
        stderr,
        "the original never reached its sync point: %s\n",
        sync == RUN_EXITED ? "it exited" : runner_error(r));
    runner_destroy(r);
    return 1;
  }

  bolo_reset();

  // The alignment, which is the one thing here that is easy to get backwards.
  // async_start case 12 increments the port's frame count where the original
  // calls flip_vp, but control does not return there: it falls through and
  // draws an entirely new frame before yielding. So the port's planes at
  // bolo_frame_count() == k hold what the original draws for its capture k+1
  // (see bolo.h's bolo_plane comment). Capture 1 therefore has no port frame
  // to match: it is pulled and dropped here, which leaves every pull inside
  // the loop yielding capture k+1 for the port's frame k.
  // RUN_EXITED leaves no error message, so it needs its own text: passing the
  // NULL from runner_error() to %s would be undefined.
  RunResult first = runner_run_to_frame(r);
  if (first != RUN_FRAME) {
    fprintf(
        stderr,
        "the original produced no frames: %s\n",
        first == RUN_EXITED ? "it exited" : runner_error(r));
    runner_destroy(r);
    return 1;
  }

  int status = 0;
  long ticks = 0;
  long compared = 0;
  long firstDiff = -1;
  char stoppedBuf[160];
  const char *stopped = "nothing ran";

  for (long k = 1;; ++k) {
    long spent = 0;
    if (!advance_port_to_frame((unsigned)k, pump, &ticks, maxTicks, &spent)) {
      snprintf(
          stoppedBuf,
          sizeof(stoppedBuf),
          "the tick budget ran out, %ld of them spent waiting for the port's frame %ld",
          spent,
          k);
      stopped = stoppedBuf;
      break;
    }
    if (bolo_frame_count() != (unsigned)k) {
      fprintf(
          stderr, "the port jumped from frame %ld to %u in one tick\n", k - 1, bolo_frame_count());
      status = 1;
      break;
    }

    RunResult res = runner_run_to_frame(r); // the original's capture k + 1
    if (res != RUN_FRAME) {
      if (res == RUN_EXITED) {
        // The recorded key script ends with ESC, which reaches INT 20h. That
        // is the end of the trace, not a failure.
        stopped = "the original's demo ended";
      } else {
        fprintf(stderr, "the original failed at frame %ld: %s\n", k, runner_error(r));
        status = 1;
      }
      break;
    }

    const uint8_t *orig[EGA_PLANES];
    const uint8_t *port[EGA_PLANES];
    for (int plane = 0; plane != EGA_PLANES; ++plane) {
      orig[plane] = runner_plane(r, plane);
      port[plane] = bolo_plane(plane);
    }

    ++compared;
    FrameDiff d = compare_planes(orig, port);
    if (d.bytes == 0)
      continue;

    printf("DIVERGENCE at frame %ld (guest time_tick %02Xh)\n", k, runner_time_tick(r));
    printf("  %ld bytes compared, %ld differ\n", COMPARE_BYTES, d.bytes);
    printf(
        "  bounding box of differing pixels: x %d..%d, y %d..%d\n", d.minX, d.maxX, d.minY, d.maxY);
    if (firstDiff < 0)
      firstDiff = k;

    // A divergence that cannot be looked at is not a report. Failing here
    // rather than carrying on keeps an unwritable output directory from
    // reading as a successful comparison.
    if (!write_divergence_images(dir, k, orig, port)) {
      fprintf(stderr, "failed to write the divergence images into %s\n", dir);
      status = 1;
      break;
    }
    printf("  wrote %s/orig-%05ld.ppm, port-%05ld.ppm, diff-%05ld.ppm\n", dir, k, k, k);

    if (!continuePastDiff) {
      stopped = "they diverged";
      break;
    }
  }

  runner_destroy(r);
  if (status != 0)
    return status;

  // A run that compared nothing has measured nothing, and "matched 0 frames"
  // against a baseline of 0 would report that as success. Whatever kept the
  // frames from arriving is the finding; it is not a passing comparison.
  if (compared == 0) {
    fprintf(stderr, "no frames were compared in %ld ticks: %s\n", ticks, stopped);
    return 1;
  }

  long matched = firstDiff < 0 ? compared : firstDiff - 1;
  if (firstDiff < 0)
    printf("no divergence in %ld frames; stopped after %ld ticks: %s\n", compared, ticks, stopped);
  else
    printf(
        "compared %ld frames, first divergence at %ld; stopped after %ld ticks: %s\n",
        compared,
        firstDiff,
        ticks,
        stopped);

  if (!baselinePath) {
    printf("matched %ld frames\n", matched);
    return 0;
  }
  if (matched < baseline) {
    printf("REGRESSION: matched %ld frames, baseline is %ld\n", matched, baseline);
    return 1;
  }
  printf("matched %ld frames (baseline %ld)\n", matched, baseline);
  if (matched > baseline)
    printf("IMPROVED: update emu/baseline.txt to %ld\n", matched);
  return 0;
}

int main(int argc, char **argv) {
  long frames = 100;
  long pump = 4;
  long maxTicks = 100000;
  long compareTicks = 1000;
  const char *out = "frames";
  const char *original = NULL;
  const char *golden = NULL;
  const char *checkGolden = NULL;
  const char *baseline = NULL;
  bool compare = false;
  bool continuePastDiff = false;
  bool framesSet = false;
  bool maxTicksSet = false;
  bool ticksSet = false;

  for (int i = 1; i < argc; ++i) {
    bool last = i + 1 >= argc;
    if (strcmp(argv[i], "--compare") == 0) {
      compare = true;
    } else if (strcmp(argv[i], "--continue-past-diff") == 0) {
      continuePastDiff = true;
    } else if (strcmp(argv[i], "--ticks") == 0 && !last) {
      compareTicks = parse_positive("--ticks", argv[++i]);
      ticksSet = true;
    } else if (strcmp(argv[i], "--baseline") == 0 && !last) {
      baseline = argv[++i];
    } else if (strcmp(argv[i], "--frames") == 0 && !last) {
      frames = parse_positive("--frames", argv[++i]);
      framesSet = true;
    } else if (strcmp(argv[i], "--pump") == 0 && !last) {
      pump = parse_positive("--pump", argv[++i]);
    } else if (strcmp(argv[i], "--max-ticks") == 0 && !last) {
      maxTicks = parse_positive("--max-ticks", argv[++i]);
      maxTicksSet = true;
    } else if (strcmp(argv[i], "--out") == 0 && !last) {
      out = argv[++i];
    } else if (strcmp(argv[i], "--original") == 0 && !last) {
      original = argv[++i];
    } else if (strcmp(argv[i], "--golden") == 0 && !last) {
      golden = argv[++i];
    } else if (strcmp(argv[i], "--check-golden") == 0 && !last) {
      checkGolden = argv[++i];
    } else {
      usage(argv[0]);
      return 2;
    }
  }

  if (!original && (golden || checkGolden)) {
    fprintf(stderr, "--golden and --check-golden only apply to --original\n");
    return 2;
  }
  if (compare && original) {
    fprintf(stderr, "--compare runs both sides; it cannot be combined with --original\n");
    return 2;
  }
  // A flag that applies to another mode is rejected rather than ignored, in
  // both directions: silently running a different number of frames than was
  // asked for is how a harness comes to measure something nobody intended.
  if (!compare && (baseline || continuePastDiff || ticksSet)) {
    const char *flag = baseline ? "--baseline"
        : continuePastDiff      ? "--continue-past-diff"
                                : "--ticks";
    fprintf(stderr, "%s only applies to --compare\n", flag);
    return 2;
  }
  if (compare && (framesSet || maxTicksSet)) {
    fprintf(
        stderr,
        "%s does not apply to --compare; bound the run with --ticks\n",
        framesSet ? "--frames" : "--max-ticks");
    return 2;
  }

  const char *dir = original ? original : out;
  if (!ensure_directory(dir)) {
    fprintf(stderr, "cannot create output directory \"%s\"\n", dir);
    return 1;
  }

  if (compare)
    return run_compare(out, compareTicks, pump, baseline, continuePastDiff);
  if (original)
    return run_original(original, frames, golden, checkGolden);

  bolo_reset();

  long captured = 0;
  long ticks = 0;
  while (captured < frames && ticks < maxTicks) {
    unsigned before = bolo_frame_count();
    bolo_run_tick((int)pump);
    ++ticks;

    if (bolo_frame_count() == before)
      continue;

    const uint8_t *planes[EGA_PLANES];
    for (int plane = 0; plane != EGA_PLANES; ++plane)
      planes[plane] = bolo_plane(plane);

    char path[1024];
    snprintf(path, sizeof(path), "%s/frame-%05ld.ppm", out, captured);
    if (!ppm_write_planes(path, planes, bolo_palette())) {
      fprintf(stderr, "failed to write %s\n", path);
      return 1;
    }
    ++captured;
  }

  if (captured < frames) {
    fprintf(
        stderr,
        "only captured %ld of %ld frames in %ld ticks; the game never reached "
        "its frame gate\n",
        captured,
        frames,
        ticks);
    return 1;
  }

  printf("captured %ld frames in %ld ticks\n", captured, ticks);
  return 0;
}
