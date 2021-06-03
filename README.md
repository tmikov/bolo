# Re-implementation of the Vintage Game BOLO

![](https://raw.github.com/tmikov/bolo/master/bolo-screenshot.png)

This project is dedicated to reimplementing the classic Apple II game "BOLO" in modern C/C++.
- https://en.wikipedia.org/wiki/Bolo_(1982_video_game)
- https://archive.org/details/a2_Bolo_1982_Synergistic_Software

Live demo of the current work in progress (very rough, not playable) in Wasm:
- https://tmikov.github.io/bolo/

Based on the unofficial IBM PC port of the original game by MrRm, 1993.

## Current Status

In just a few days we have been able to re-implement:
- title screen
- level-selection screen
- main ship rendering
- maze rendering
- bullet rendering and movement
- collision detection
- explosions  
- some other stuff.

There is working code in the [src/](https://github.com/tmikov/bolo/tree/master/src) directory. Here is a [live demo](https://tmikov.github.io/bolo/).

To exercise the working code paths, the app controls ship around the maze with collision detection:
- Arrow keys control the ship.
- 1 & 2 rotate the gun.
- Space fires.
- R resets the game.

Note that when we say "re-implement", we don't mean recreate the output (which in some cases could have been accomplished by just rendering a png), but rather port the *original* logic to C and get that logic do the work. For example, this is the code that draws multiple strings to the screen:
```c
/// Draw a sequence of string/offset pairs terminated with a null string ptr.
static void draw_mstrs(const SBOLDesc *strings) {
  for (; strings->str; ++strings)
    draw_str(strings->offset, strings->str);
}
```
Here are a couple of screenshots from the re-implementation (note the blurriness caused by OpenGL filering when upscaling from the native 320x200).

### Level selection
![](https://raw.github.com/tmikov/bolo/master/images/levsel.png)

### Maze and Ship Firing
![](https://raw.github.com/tmikov/bolo/master/images/maze.png)

## The Re-implementation

Our goal is to faithfully re-implement the game in modern C/C++, while preserving its core structure - key routines, data structures, etc. The result should play exactly like the original, down to every little detail. Once that is accomplished, we may explore some improvements of graphics, sound, etc - we have some ideas how to approach the problem without affecting gameplay.

The application uses modern graphics APIs to render the screen, but internally it preserves the original structure of the EGA memory, bitplanes and all. We are doing this for maximum faithfulness to the original. Once we are confident in the quality of our re-implementation, we will probably transition to packed byte-per-pixel representation. We cannot deviate too much, since video memory is also used for collision detection.

As an example, this is what rendering the rotated ship and gun looks like ported to C:
```c
  do {
    uint8_t pix = *gunbmp++ ^ *shipbmp++;
    if (pix & ega_read(offset))
      ++collisions;
    ega_write(offset, pix, EGAHighCyan);
    offset += EGA_STRIDE;
  } while (--cnt);
```
This is what the original assembler looked like:
```asm
nextrow:
                lodsb
                xor     al,[bx]
                test    al,es:[di]
                jz      no_collision
                inc     ah
no_collision:
                stosb
                add     di,27h
                inc     bx
                loop    nextrow
```

The finished application will work on major operating systems and web (using WebGL) and will have minimal dependencies.

## Web Platform

Running on the web platform with Emscripten presents an interesting challenge because it is not compatible with the classic DOS game loop. We need to be able to relinquish control to the browser after rendering every frame.

In a native app we plan to solve this by using threads - the classic game logic runs in its own thread - while the native rendering, sound, keyboard handling is in the map application thread. This approach unfortunately doesn't seem to work in browsers for now, because sharing memory between web worker threads has been temporarily disabled (though hopefully [coming back](https://developer.chrome.com/blog/enabling-shared-array-buffer/)).

One very tempting solution we are looking into is [Asyncify](https://kripken.github.io/blog/wasm/2019/07/16/asyncify.html), which augments linear code to enable it to suspend the callstack and re-wind it.

A perhaps better solution is to refactor the game into a more modern frame-based approach. We will have to see whether that is possible while preserving the original spirit of the game.

## Origins

The project is based on the unofficial IBM PC port of the original game by MrRm, 1993.

We chose the MS DOS port instead of the Apple II original, because we expected it to be easier to understand and work with. It is a tiny 9KB MS-DOS .COM file - how hard can it be? Also, we happened to come across https://github.com/begoon/bolo - a project with the same goal, graciously hosting the binary, and even a patch for unlimited lives, making it even easier to get started.

Not much is known about the MS DOS port or the mysterious "Mr.Rm", but it appears to be a completely faithful reimplementation of the original, with some minor graphical changes (the colors, the compass, etc). Did he reverse engineer the original binary? We would like to think so and imagine that we are continuing a great tradition.

Examining the binary tells us that:
1. It is not just a mechanical translation of the original 6502 assembly: the code fully utilizes the 8086 CPU's registers (the 6502 only had three 8-bit registers, 8086 is abundant in comparison), performs multiplication, and so on. It also cleverly uses EGA mode 0Dh to perform fewer graphics writes (just one bit per pixel, instead of four), and so on. However, I also feel that it occasionally overuses 8-bit registers, perhaps showing its 6502 ancestry.
2. It was coded in 8086 assembler, no high level language compiler was used. Even the carry flag is passed as an extra argument in some functions, to extend a value from 8 to 9 bits!
3. The author seemed like he was enjoying himself writing it, sometimes doing things not because they are necessarily much faster or smaller, but because they are fun. For example, the bitmaps of the tank gun partially overlap to save a few bytes. Each bitmap is 7 bytes and there are 8 of them, for a grand total of 56 bytes! By partially overlapping them, around 10 bytes were saved! Similarly, the tank has 8 7-byte bitmaps, but because it is symmetric, only 4 are needed, thus saving a whopping 26 bytes (well, actually somewhat less, because of additional 8 pointers needed)! Tell that to my 100MB Electron app...

## Reverse Engineering Approach

Our reverse engineering approach involves no debugging. We disassembled the .COM file using a vintage copy of the Sourcer disassembler we had lying around. Sourcer itself is a MS DOS application, abandonware these days, but is very powerful and high quality, despite its age and idiosyncrasies, especially when reverse engineering a 30 year old .COM file.

We prefer Sourcer's iterative offline approach to modern interactive tools, since it relies entirely on manipulating and saving text files. Sourcer generates a definition file, which can be edited to control what it disassembles, names of locations, even comments. When that file is fed back to it, it produces a better disassemly and a new definition file, and so on. Each file at every stage can be committed separately to source control.

Once we have the partially disassembled source (the quality of the disassembly improves incrementally as we work through it), we look for simple things we can recognize and understand, like writes to video memory, or handling keyboard events, etc. Then we start building around them, looking how these pieces of code are used, who calls them, and so on. It is painful initially, but it gets easier as the corpus of understood code grows.

The "unlimited lives" patch file also provided useful hints for where collision detection occurs and other things.

Once we have meaningful peaces of code more or less understood, we re-implement them in C. Frequently the re-implementation itself helps us to really understand what is going on and fill the last missing pieces.

## License

The source of re-implementation is put in the public domain, see the LICENSE file.
External libraries that have been used have their own licenses, present in the same directory.