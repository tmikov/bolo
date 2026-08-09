// Run the game headlessly and dump each completed frame as a PPM.
//
// The game's attract demo needs no input: the title screen times out, and the
// recorded key script in bolo.c drives level selection and play.
//
// With --original the same is done for the original BOLO.COM running under the
// emulated machine instead of the C port. Both sides render through the same
// palette, so any visible difference between their PPMs is a real difference
// in the EGA planes.

#include "bolo.h"
#include "ppm.h"
#include "runner.h"

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
      "\n"
      "  --frames N          stop after N completed frames (default 100)\n"
      "  --pump N            async_start() calls per tick (default 4)\n"
      "  --out DIR           directory for frame-NNNNN.ppm (default \"frames\")\n"
      "  --max-ticks N       give up after N ticks (default 100000)\n"
      "\n"
      "  --original DIR      run the original BOLO.COM instead of the port,\n"
      "                      writing its frames to DIR\n"
      "  --golden FILE       with --original, write one checksum line per frame\n"
      "  --check-golden FILE compare those lines against a committed trace\n",
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

int main(int argc, char **argv) {
  long frames = 100;
  long pump = 4;
  long maxTicks = 100000;
  const char *out = "frames";
  const char *original = NULL;
  const char *golden = NULL;
  const char *checkGolden = NULL;

  for (int i = 1; i < argc; ++i) {
    bool last = i + 1 >= argc;
    if (strcmp(argv[i], "--frames") == 0 && !last) {
      frames = parse_positive("--frames", argv[++i]);
    } else if (strcmp(argv[i], "--pump") == 0 && !last) {
      pump = parse_positive("--pump", argv[++i]);
    } else if (strcmp(argv[i], "--max-ticks") == 0 && !last) {
      maxTicks = parse_positive("--max-ticks", argv[++i]);
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

  const char *dir = original ? original : out;
  if (!ensure_directory(dir)) {
    fprintf(stderr, "cannot create output directory \"%s\"\n", dir);
    return 1;
  }

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
