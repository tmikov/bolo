// Proves bolo.c has no sokol dependency: this links against bologame alone.
#include "bolo.h"

#include <stdio.h>

int main(void) {
  bolo_reset();

  if (bolo_frame_count() != 0) {
    fprintf(stderr, "expected 0 frames before any tick, got %u\n", bolo_frame_count());
    return 1;
  }
  for (int plane = 0; plane != EGA_PLANES; ++plane) {
    if (!bolo_plane(plane)) {
      fprintf(stderr, "bolo_plane(%d) returned NULL\n", plane);
      return 1;
    }
  }
  if (!bolo_palette()) {
    fprintf(stderr, "bolo_palette() returned NULL\n");
    return 1;
  }
  printf("bolo.c links without sokol\n");
  return 0;
}
