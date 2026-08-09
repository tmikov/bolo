// Run the game headlessly and dump each completed frame as a PPM.
//
// The game's attract demo needs no input: the title screen times out, and the
// recorded key script in bolo.c drives level selection and play.

#include "bolo.h"
#include "ppm.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static void usage(const char *argv0) {
  fprintf(
      stderr,
      "usage: %s [--frames N] [--pump N] [--out DIR] [--max-ticks N]\n"
      "\n"
      "  --frames N     stop after N completed frames (default 100)\n"
      "  --pump N       async_start() calls per tick (default 4)\n"
      "  --out DIR      directory for frame-NNNNN.ppm (default \"frames\")\n"
      "  --max-ticks N  give up after N ticks (default 100000)\n",
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

int main(int argc, char **argv) {
  long frames = 100;
  long pump = 4;
  long maxTicks = 100000;
  const char *out = "frames";

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
    } else {
      usage(argv[0]);
      return 2;
    }
  }

  if (mkdir(out, 0777) != 0) {
    // Reusing an existing directory is fine; anything else is not. Note that
    // an existing directory is reused as-is and never cleared, so stale
    // frames from a previous run can remain alongside the new ones.
    struct stat st;
    if (stat(out, &st) != 0 || !S_ISDIR(st.st_mode)) {
      fprintf(stderr, "cannot create output directory \"%s\"\n", out);
      return 1;
    }
  }

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
