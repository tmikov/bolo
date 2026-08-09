// Write EGA planes as a binary PPM (P6). Chosen over PNG to avoid a zlib
// dependency; every image viewer and ImageMagick reads PPM.

#ifndef BOLO_PPM_H
#define BOLO_PPM_H

#include "bolo.h"

#include <stdbool.h>
#include <stdint.h>

/// Write an EGA_WIDTH x EGA_HEIGHT image built from the four bit planes.
///
/// Each pixel's color index is its bit from plane 0 in bit 0, plane 1 in bit 1,
/// and so on, matching the EGA's own layout. Bits run most-significant first.
/// Every planes[i] must point to at least EGA_PAGE_VISIBLE bytes, and palette
/// to 16 entries. Returns false on any I/O error.
bool ppm_write_planes(
    const char *path,
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette);

#endif // BOLO_PPM_H
