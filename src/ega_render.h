// bolo - public domain Tzvetan Mikov 2021
//
// Conversion from the game's EGA bit planes to RGBA pixels. Shared by the
// windowed front end (which renders into a power-of-two texture) and the
// headless harness (which writes image files).

#ifndef BOLO_EGA_RENDER_H
#define BOLO_EGA_RENDER_H

#include "bolo.h"

/// Convert row `y` of the four EGA bit planes into EGA_WIDTH RGBA pixels.
///
/// A pixel's colour index is its bit from plane 0 in bit 0, plane 1 in bit 1,
/// plane 2 in bit 2 and plane 3 in bit 3 — the EGA's own layout. Within each
/// byte, bits run most-significant first, so the leftmost pixel of a byte
/// comes from bit 7.
///
/// Each `planes[i]` must hold at least EGA_PAGE_VISIBLE bytes, `palette` must
/// have 16 entries, and `out` must have room for EGA_WIDTH pixels.
void ega_row_to_rgba(
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette,
    unsigned y,
    RGBA8 *out);

/// Convert the whole EGA_WIDTH x EGA_HEIGHT screen into `out`, advancing
/// `outStridePixels` pixels per row.
///
/// A stride wider than EGA_WIDTH leaves the pixels past each row's end
/// untouched, which is what the GL front end wants when rendering into a
/// power-of-two texture.
void ega_screen_to_rgba(
    const uint8_t *const planes[EGA_PLANES],
    const RGBA8 *palette,
    RGBA8 *out,
    unsigned outStridePixels);

#endif // BOLO_EGA_RENDER_H
