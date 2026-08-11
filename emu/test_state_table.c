// The mirrored-state table in bolo.c names each of the port's variables by the
// address the original keeps it at, and the comparison reads the original's
// memory at exactly those addresses. A wrong address or size there does not
// fail loudly: it compares the port's variable against the original's
// *neighbouring* one and reports a divergence that does not exist. That
// happened -- bullet_x and bullet_y were entered at their true addresses but
// with the port's widened sizes, so each read 64 bytes where the original
// keeps 32, and the comparison invented a difference at frame 1.
//
// Ordering and non-overlap catch that whole class: the original's variables
// are laid out end to end, so an entry that runs into the next one is wrong
// about one of them.

#include "bolo.h"

#include <stdio.h>

static int failures;

static void fail(const char *fmt, const char *name, unsigned a, unsigned b) {
  fprintf(stderr, fmt, name, a, b);
  ++failures;
}

int main(void) {
  unsigned count;
  const BoloStateVar *table = bolo_state_table(&count);

  if (count == 0) {
    fprintf(stderr, "the state table is empty\n");
    return 1;
  }

  unsigned prevEnd = 0;
  const char *prevName = "(start)";
  for (unsigned i = 0; i != count; ++i) {
    const BoloStateVar *v = &table[i];

    if (!v->name || !v->data) {
      fprintf(stderr, "entry %u has no name or no storage\n", i);
      ++failures;
      continue;
    }
    if (v->count == 0)
      fail("%s: has %u elements of %u bytes\n", v->name, v->count, v->width);
    if (v->width == 0 || v->width > v->stride)
      fail("%s: compares %u of every %u bytes\n", v->name, v->width, v->stride);

    // The original's variables live in one data segment, end to end. An entry
    // starting before the previous one ended is reading the previous
    // variable's bytes.
    if (v->addr < prevEnd)
      fprintf(
          stderr,
          "%s at %04Xh starts inside %s, which ends at %04Xh\n",
          v->name,
          v->addr,
          prevName,
          prevEnd),
          ++failures;

    unsigned end = (unsigned)v->addr + (unsigned)v->count * v->width;
    if (end > 0x10000u)
      fail("%s: ends past the segment at %u (0x%X)\n", v->name, end, end);

    prevEnd = end;
    prevName = v->name;
  }

  if (failures != 0) {
    fprintf(stderr, "%d problem(s) in the %u-entry state table\n", failures, count);
    return 1;
  }
  printf("state table: %u entries, ordered and non-overlapping\n", count);
  return 0;
}
