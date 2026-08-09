// Execution tests over a toy machine: 1MB of flat RAM, no devices.
//
// Each case hand-assembles a short program, runs it to a halt marker, and
// checks the resulting registers and memory. The point is to exercise the
// machinery the decode and ALU tests cannot reach -- effective addresses,
// the stack, string instructions, prefixes and interrupt vectoring -- before
// plan 3 wires up real hardware.

#include "i8086.h"

#include <stdio.h>
#include <string.h>

#define MEM_SIZE 0x100000
static uint8_t g_mem[MEM_SIZE];
static uint8_t g_ports[0x10000];

static int g_failures;

static uint8_t mem_read8(void *ctx, uint32_t linear) {
  (void)ctx;
  return g_mem[linear & 0xFFFFF];
}

static void mem_write8(void *ctx, uint32_t linear, uint8_t value) {
  (void)ctx;
  g_mem[linear & 0xFFFFF] = value;
}

static uint8_t port_in8(void *ctx, uint16_t port) {
  (void)ctx;
  return g_ports[port];
}

static void port_out8(void *ctx, uint16_t port, uint8_t value) {
  (void)ctx;
  g_ports[port] = value;
}

/// Every vector the intercept hook was offered, in order.
static uint8_t g_interceptVec[4];
static unsigned g_interceptCount;

/// Claims INT 21h and declines everything else, the way plan 3's machine will
/// service DOS and BIOS natively while leaving BOLO's own INT 08h and INT 09h
/// handlers to be reached through the guest's own table.
static bool claim_21h(void *ctx, uint8_t vec) {
  (void)ctx;
  if (g_interceptCount < sizeof(g_interceptVec) / sizeof(g_interceptVec[0]))
    g_interceptVec[g_interceptCount] = vec;
  ++g_interceptCount;
  return vec == 0x21;
}

/// Reset the CPU, attach the toy bus, and point every segment at 1000h so the
/// program at linear 10000h is addressable as 1000:0000.
static void setup_cpu(I8086 *cpu) {
  i8086_reset(cpu);
  cpu->ctx = NULL;
  cpu->read8 = mem_read8;
  cpu->write8 = mem_write8;
  cpu->in8 = port_in8;
  cpu->out8 = port_out8;
  // i8086_reset() deliberately leaves the callbacks alone, so every one of them
  // has to be set here -- including the optional hook, which is otherwise
  // whatever was on the stack.
  cpu->intercept = NULL;
  cpu->sreg[I8086_CS] = 0x1000;
  cpu->sreg[I8086_DS] = 0x1000;
  cpu->sreg[I8086_ES] = 0x1000;
  cpu->sreg[I8086_SS] = 0x1000;
  cpu->reg[I8086_SP] = 0xFFFE;
  cpu->ip = 0;
}

/// Step until IP reaches `stopIp`, or fail. Memory must already be prepared.
static bool run_loaded(I8086 *cpu, uint16_t stopIp, int maxSteps) {
  for (int i = 0; i != maxSteps; ++i) {
    if (cpu->ip == stopIp)
      return true;
    if (!i8086_step(cpu)) {
      fprintf(
          stderr,
          "  step failed at %05X: %s\n",
          cpu->errorAddr,
          cpu->error ? cpu->error : "(no message)");
      return false;
    }
  }
  fprintf(stderr, "  never reached stopIp %04X (IP is %04X)\n", stopIp, cpu->ip);
  return false;
}

/// Clear memory, load `code` at 1000:0000, and run it.
static bool run(I8086 *cpu, const uint8_t *code, size_t codeLen, uint16_t stopIp, int maxSteps) {
  memset(g_mem, 0, sizeof(g_mem));
  memcpy(g_mem + 0x10000, code, codeLen);
  setup_cpu(cpu);
  return run_loaded(cpu, stopIp, maxSteps);
}

static void expect_u16(const char *what, uint16_t got, uint16_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-32s got %04X, expected %04X\n", what, got, want);
  ++g_failures;
}

static void expect_u8(const char *what, uint8_t got, uint8_t want) {
  if (got == want)
    return;
  fprintf(stderr, "FAIL %-32s got %02X, expected %02X\n", what, got, want);
  ++g_failures;
}

int main(void) {
  I8086 cpu;

  // --- mov immediates, register-to-register, and cbw ---
  {
    //   0000 mov ax,1234h        B8 34 12
    //   0003 mov bx,ax           8B D8
    //   0005 mov al,0F0h         B0 F0
    //   0007 cbw                 98
    //   0008 stop
    static const uint8_t code[] = {0xB8, 0x34, 0x12, 0x8B, 0xD8, 0xB0, 0xF0, 0x98};
    if (!run(&cpu, code, sizeof(code), 8, 20)) {
      ++g_failures;
    } else {
      expect_u16("mov bx,ax", cpu.reg[I8086_BX], 0x1234);
      expect_u16("cbw of F0h", cpu.reg[I8086_AX], 0xFFF0);
    }
  }

  // --- push / pop / call / retn round trip ---
  {
    //   0000 mov ax,2211h        B8 11 22
    //   0003 push ax             50
    //   0004 call 000Ah          E8 03 00   (next IP 0007, + 3)
    //   0007 pop bx              5B
    //   0008 jmp 000Eh           EB 04      (next IP 000A, + 4)
    //   000A mov cx,0099h        B9 99 00
    //   000D retn                C3
    //   000E stop
    // Control flows 0000 -> 0004 (call) -> 000A -> 000D (retn) -> 0007 -> 0008
    // (jmp) -> 000E, so both the call target and the return path are exercised.
    static const uint8_t code[] = {
        0xB8, 0x11, 0x22, 0x50, 0xE8, 0x03, 0x00, 0x5B, 0xEB, 0x04, 0xB9, 0x99, 0x00, 0xC3};
    if (!run(&cpu, code, sizeof(code), 0x000E, 40)) {
      ++g_failures;
    } else {
      expect_u16("call/retn set cx", cpu.reg[I8086_CX], 0x0099);
      expect_u16("push/pop round trip", cpu.reg[I8086_BX], 0x2211);
      expect_u16("sp restored", cpu.reg[I8086_SP], 0xFFFE);
    }
  }

  // --- rep stosb forward, then again with DF set ---
  {
    //   0000 mov di,0100h        BF 00 01
    //   0003 mov cx,0004h        B9 04 00
    //   0006 mov al,0AAh         B0 AA
    //   0008 cld                 FC
    //   0009 rep stosb           F3 AA
    //   000B stop
    static const uint8_t code[] = {
        0xBF, 0x00, 0x01, 0xB9, 0x04, 0x00, 0xB0, 0xAA, 0xFC, 0xF3, 0xAA};
    if (!run(&cpu, code, sizeof(code), 0x000B, 40)) {
      ++g_failures;
    } else {
      for (unsigned i = 0; i != 4; ++i)
        expect_u8("rep stosb byte", g_mem[0x10100 + i], 0xAA);
      expect_u8("rep stosb stopped", g_mem[0x10104], 0x00);
      expect_u16("rep stosb left cx=0", cpu.reg[I8086_CX], 0);
      expect_u16("rep stosb advanced di", cpu.reg[I8086_DI], 0x0104);
    }

    // The same fill with DF set. DI now walks down, so the four bytes land at
    // 0110h down to 010Dh and DI ends one below the last one written. The run
    // finishes with cld so it cannot leak DF into any later case.
    //   0000 mov di,0110h        BF 10 01
    //   0003 mov cx,0004h        B9 04 00
    //   0006 mov al,55h          B0 55
    //   0008 std                 FD
    //   0009 rep stosb           F3 AA
    //   000B cld                 FC
    //   000C stop
    static const uint8_t back[] = {
        0xBF, 0x10, 0x01, 0xB9, 0x04, 0x00, 0xB0, 0x55, 0xFD, 0xF3, 0xAA, 0xFC};
    if (!run(&cpu, back, sizeof(back), 0x000C, 40)) {
      ++g_failures;
    } else {
      for (unsigned i = 0; i != 4; ++i)
        expect_u8("std rep stosb byte", g_mem[0x10110 - i], 0x55);
      expect_u8("std rep stosb stopped", g_mem[0x1010C], 0x00);
      expect_u16("std rep stosb left cx=0", cpu.reg[I8086_CX], 0);
      expect_u16("std rep stosb walked di down", cpu.reg[I8086_DI], 0x010C);
      expect_u16("cld cleared DF", cpu.flags & I8086_DF, 0);
    }
  }

  // --- backward lodsb: the shape BOLO uses at 2913:20BE, where std is followed
  //     by a backward lodsb loop and a matching cld ---
  {
    //   0000 mov si,0105h        BE 05 01
    //   0003 std                 FD
    //   0004 lodsb               AC        (AL <- [0105], SI -> 0104)
    //   0005 lodsb               AC        (AL <- [0104], SI -> 0103)
    //   0006 cld                 FC
    //   0007 stop
    static const uint8_t code[] = {0xBE, 0x05, 0x01, 0xFD, 0xAC, 0xAC, 0xFC};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));
    g_mem[0x10105] = 0x11;
    g_mem[0x10104] = 0x22;

    setup_cpu(&cpu);
    if (!run_loaded(&cpu, 0x0007, 30)) {
      ++g_failures;
    } else {
      expect_u8("std lodsb read the lower byte", (uint8_t)cpu.reg[I8086_AX], 0x22);
      expect_u16("std lodsb walked si down", cpu.reg[I8086_SI], 0x0103);
      expect_u16("cld cleared DF", cpu.flags & I8086_DF, 0);
    }
  }

  // --- backward rep movsw: the wide form has to step by two ---
  {
    //   0000 mov si,0106h        BE 06 01
    //   0003 mov di,0206h        BF 06 02
    //   0006 mov cx,0004h        B9 04 00
    //   0009 std                 FD
    //   000A rep movsw           F3 A5
    //   000C cld                 FC
    //   000D stop
    // Eight source bytes at 0100h..0107h are copied down to 0200h..0207h, so
    // the destination reads identically even though the copy ran backwards.
    static const uint8_t code[] = {
        0xBE, 0x06, 0x01, 0xBF, 0x06, 0x02, 0xB9, 0x04, 0x00, 0xFD, 0xF3, 0xA5, 0xFC};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));
    for (unsigned i = 0; i != 8; ++i)
      g_mem[0x10100 + i] = (uint8_t)(0x10 + i);

    setup_cpu(&cpu);
    if (!run_loaded(&cpu, 0x000D, 40)) {
      ++g_failures;
    } else {
      for (unsigned i = 0; i != 8; ++i)
        expect_u8("std rep movsw byte", g_mem[0x10200 + i], (uint8_t)(0x10 + i));
      expect_u16("std rep movsw walked si down", cpu.reg[I8086_SI], 0x00FE);
      expect_u16("std rep movsw walked di down", cpu.reg[I8086_DI], 0x01FE);
      expect_u16("std rep movsw left cx=0", cpu.reg[I8086_CX], 0);
      expect_u16("cld cleared DF", cpu.flags & I8086_DF, 0);
    }
  }

  // --- loop and jcxz ---
  {
    //   0000 mov cx,0005h        B9 05 00
    //   0003 mov bx,0000h        BB 00 00
    //   0006 inc bx              43
    //   0007 loop 0006h          E2 FD
    //   0009 stop
    static const uint8_t code[] = {0xB9, 0x05, 0x00, 0xBB, 0x00, 0x00, 0x43, 0xE2, 0xFD};
    if (!run(&cpu, code, sizeof(code), 0x0009, 60)) {
      ++g_failures;
    } else {
      expect_u16("loop ran 5 times", cpu.reg[I8086_BX], 5);
      expect_u16("loop left cx=0", cpu.reg[I8086_CX], 0);
    }
  }

  // --- ModRM addressing: mod=1 (disp8) with BX+SI, and mod=0 rm=6 direct ---
  {
    //   0000 mov bx,0100h        BB 00 01
    //   0003 mov si,0002h        BE 02 00
    //   0006 mov al,55h          B0 55
    //   0008 mov [bx+si+10h],al  88 40 10
    //   000B mov ah,[0112h]      8A 26 12 01
    //   000F stop
    static const uint8_t code[] = {
        0xBB, 0x00, 0x01, 0xBE, 0x02, 0x00, 0xB0, 0x55, 0x88, 0x40, 0x10, 0x8A, 0x26, 0x12, 0x01};
    if (!run(&cpu, code, sizeof(code), 0x000F, 40)) {
      ++g_failures;
    } else {
      expect_u8("mod=1 BX+SI+disp8 store", g_mem[0x10112], 0x55);
      expect_u8("mod=0 rm=6 direct load", (uint8_t)(cpu.reg[I8086_AX] >> 8), 0x55);
    }
  }

  // --- es: segment override ---
  {
    //   0000 mov ax,2000h        B8 00 20
    //   0003 mov es,ax           8E C0
    //   0005 mov di,0000h        BF 00 00
    //   0008 mov al,7Eh          B0 7E
    //   000A mov es:[di],al      26 88 05
    //   000D stop
    static const uint8_t code[] = {
        0xB8, 0x00, 0x20, 0x8E, 0xC0, 0xBF, 0x00, 0x00, 0xB0, 0x7E, 0x26, 0x88, 0x05};
    if (!run(&cpu, code, sizeof(code), 0x000D, 40)) {
      ++g_failures;
    } else {
      expect_u8("es: override target", g_mem[0x20000], 0x7E);
      expect_u8("ds: was not written", g_mem[0x10000 + 0], 0xB8);
    }
  }

  // --- xlat, with the translation table written by the program itself so the
  //     case also covers "mov rm8,imm8" in the mod=0 rm=6 direct form ---
  {
    //   0000 mov byte ptr [0103h],9Ch   C6 06 03 01 9C
    //   0005 mov bx,0100h               BB 00 01
    //   0008 mov al,03h                 B0 03
    //   000A xlat                       D7
    //   000B stop
    static const uint8_t code[] = {
        0xC6, 0x06, 0x03, 0x01, 0x9C, 0xBB, 0x00, 0x01, 0xB0, 0x03, 0xD7};
    if (!run(&cpu, code, sizeof(code), 0x000B, 30)) {
      ++g_failures;
    } else {
      expect_u8("xlat", (uint8_t)cpu.reg[I8086_AX], 0x9C);
    }
  }

  // --- int / iret through the guest vector table ---
  {
    //   0000 int 40h            CD 40
    //   0002 stop
    // Handler planted at 1000:0020:  mov bx,0BEEFh (BB EF BE) then iret (CF).
    static const uint8_t code[] = {0xCD, 0x40};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));

    // Vector 40h -> 1000:0020, in the guest's own table at 0000:0000.
    g_mem[0x40 * 4 + 0] = 0x20;
    g_mem[0x40 * 4 + 1] = 0x00;
    g_mem[0x40 * 4 + 2] = 0x00;
    g_mem[0x40 * 4 + 3] = 0x10;
    g_mem[0x10020] = 0xBB;
    g_mem[0x10021] = 0xEF;
    g_mem[0x10022] = 0xBE;
    g_mem[0x10023] = 0xCF;

    setup_cpu(&cpu);
    if (!run_loaded(&cpu, 0x0002, 30)) {
      ++g_failures;
    } else {
      expect_u16("int 40h ran the handler", cpu.reg[I8086_BX], 0xBEEF);
      expect_u16("iret restored sp", cpu.reg[I8086_SP], 0xFFFE);
      expect_u16("iret returned to 0002", cpu.ip, 0x0002);
    }
  }

  // --- lahf / sahf round trip, and that inc leaves CF alone ---
  {
    //   0000 stc                 F9
    //   0001 mov al,0FFh         B0 FF
    //   0003 inc al              FE C0     (CF must survive)
    //   0005 lahf                9F
    //   0006 mov bl,ah           8A DC
    //   0008 stop
    static const uint8_t code[] = {0xF9, 0xB0, 0xFF, 0xFE, 0xC0, 0x9F, 0x8A, 0xDC};
    if (!run(&cpu, code, sizeof(code), 0x0008, 30)) {
      ++g_failures;
    } else {
      expect_u8("inc preserved CF into lahf", (uint8_t)(cpu.reg[I8086_BX] & I8086_CF), I8086_CF);
      expect_u8("inc al of FFh wrapped", (uint8_t)cpu.reg[I8086_AX], 0x00);
    }
  }

  // --- in / out ---
  {
    //   0000 mov dx,03DAh        BA DA 03
    //   0003 in al,dx            EC
    //   0004 mov ah,al           8A E0
    //   0006 out 61h,al          E6 61
    //   0008 stop
    static const uint8_t code[] = {0xBA, 0xDA, 0x03, 0xEC, 0x8A, 0xE0, 0xE6, 0x61};
    memset(g_ports, 0, sizeof(g_ports));
    g_ports[0x3DA] = 0x08;
    if (!run(&cpu, code, sizeof(code), 0x0008, 30)) {
      ++g_failures;
    } else {
      expect_u8("in al,dx", (uint8_t)cpu.reg[I8086_AX], 0x08);
      expect_u8("out imm8,al", g_ports[0x61], 0x08);
    }
  }

  // --- jmp far [mem], the FF /5 form. BOLO's timer ISR uses exactly this
  //     encoding at 2913:002E to chain to the INT 08h handler it displaced, so
  //     plan 3's tick delivery rides on it ---
  {
    //   0000 jmp dword ptr [0100h]   FF 2E 00 01
    // with the far pointer 2000:0050 planted at DS:0100.
    static const uint8_t code[] = {0xFF, 0x2E, 0x00, 0x01};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));
    g_mem[0x10100] = 0x50;
    g_mem[0x10101] = 0x00;
    g_mem[0x10102] = 0x00;
    g_mem[0x10103] = 0x20;

    setup_cpu(&cpu);
    if (!i8086_step(&cpu)) {
      fprintf(stderr, "  jmp far failed: %s\n", cpu.error ? cpu.error : "(no message)");
      ++g_failures;
    } else {
      expect_u16("jmp far [mem] loaded ip", cpu.ip, 0x0050);
      expect_u16("jmp far [mem] loaded cs", cpu.sreg[I8086_CS], 0x2000);
    }
  }

  // --- an encoding the decoder accepts but the executor does not implement
  //     must fail loudly and name where. Plan 3 turns this into a precise
  //     abort rather than a wrong answer ---
  {
    //   0000 nop                 90
    //   0001 div al              F6 F0     (F6 /6 is not implemented)
    // The nop is there so the reported address is the failing instruction's,
    // not merely the start of the program.
    static const uint8_t code[] = {0x90, 0xF6, 0xF0};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));

    setup_cpu(&cpu);
    if (!i8086_step(&cpu)) {
      fprintf(stderr, "  the nop before div failed to execute\n");
      ++g_failures;
    } else if (i8086_step(&cpu)) {
      fprintf(stderr, "FAIL %-32s div al executed instead of failing\n", "hard error on div");
      ++g_failures;
    } else {
      if (!cpu.error) {
        fprintf(stderr, "FAIL %-32s step failed with no message\n", "hard error on div");
        ++g_failures;
      }
      if (cpu.errorAddr != 0x10001) {
        fprintf(
            stderr,
            "FAIL %-32s got %05X, expected %05X\n",
            "hard error addr",
            cpu.errorAddr,
            0x10001);
        ++g_failures;
      }
    }
  }

  // --- the intercept hook: the host claims one vector, the guest's own table
  //     still serves the other ---
  {
    //   0000 int 21h             CD 21     (claimed by the host)
    //   0002 int 40h             CD 40     (declined, vectors through the IVT)
    //   0004 stop
    // Handler planted at 1000:0020:  mov bx,0BEEFh (BB EF BE) then iret (CF).
    static const uint8_t code[] = {0xCD, 0x21, 0xCD, 0x40};
    memset(g_mem, 0, sizeof(g_mem));
    memcpy(g_mem + 0x10000, code, sizeof(code));
    g_mem[0x40 * 4 + 0] = 0x20;
    g_mem[0x40 * 4 + 1] = 0x00;
    g_mem[0x40 * 4 + 2] = 0x00;
    g_mem[0x40 * 4 + 3] = 0x10;
    g_mem[0x10020] = 0xBB;
    g_mem[0x10021] = 0xEF;
    g_mem[0x10022] = 0xBE;
    g_mem[0x10023] = 0xCF;

    setup_cpu(&cpu);
    cpu.intercept = claim_21h;
    g_interceptCount = 0;

    // Step the claimed one on its own: the frame it must not push is only
    // observable before the declined one pushes a frame of its own.
    if (!i8086_step(&cpu)) {
      fprintf(stderr, "  int 21h step failed: %s\n", cpu.error ? cpu.error : "(no message)");
      ++g_failures;
    } else {
      expect_u16("claimed int pushed no frame", cpu.reg[I8086_SP], 0xFFFE);
      expect_u16("claimed int resumed after it", cpu.ip, 0x0002);
      expect_u16("claimed int left cs alone", cpu.sreg[I8086_CS], 0x1000);
      expect_u8("hook was offered 21h", g_interceptVec[0], 0x21);
    }

    if (!run_loaded(&cpu, 0x0004, 30)) {
      ++g_failures;
    } else {
      expect_u8("hook was offered 40h", g_interceptVec[1], 0x40);
      expect_u8("hook was offered both, once each", (uint8_t)g_interceptCount, 2);
      expect_u16("declined int ran the guest handler", cpu.reg[I8086_BX], 0xBEEF);
      expect_u16("declined int's iret restored sp", cpu.reg[I8086_SP], 0xFFFE);
    }
  }

  // --- xchg with a memory operand, both widths. The wiring risk is which of
  //     the two written values lands where ---
  {
    //   0000 mov bx,1234h              BB 34 12
    //   0003 mov word ptr [0100h],5678h  C7 06 00 01 78 56
    //   0009 xchg [0100h],bx           87 1E 00 01
    //   000D mov ah,9Ah                B4 9A
    //   000F mov byte ptr [0102h],5Ch  C6 06 02 01 5C
    //   0014 xchg [0102h],ah           86 26 02 01
    //   0018 stop
    static const uint8_t code[] = {0xBB, 0x34, 0x12, 0xC7, 0x06, 0x00, 0x01, 0x78,
                                   0x56, 0x87, 0x1E, 0x00, 0x01, 0xB4, 0x9A, 0xC6,
                                   0x06, 0x02, 0x01, 0x5C, 0x86, 0x26, 0x02, 0x01};
    if (!run(&cpu, code, sizeof(code), 0x0018, 40)) {
      ++g_failures;
    } else {
      expect_u16("xchg rm16 -> register", cpu.reg[I8086_BX], 0x5678);
      expect_u8("xchg rm16 -> memory low", g_mem[0x10100], 0x34);
      expect_u8("xchg rm16 -> memory high", g_mem[0x10101], 0x12);
      expect_u8("xchg rm8 -> register", (uint8_t)(cpu.reg[I8086_AX] >> 8), 0x5C);
      expect_u8("xchg rm8 -> memory", g_mem[0x10102], 0x9A);
    }
  }

  // --- D2/D3 shifts with a CL count and a memory operand. i8086_shift is unit
  //     tested at count 1, so what is under test here is the dispatch wiring:
  //     where the count comes from, the operand width, and the writeback ---
  {
    //   0000 mov byte ptr [0100h],03h    C6 06 00 01 03
    //   0005 mov cx,0103h                B9 03 01
    //   0008 shl byte ptr [0100h],cl     D2 26 00 01
    //   000C mov word ptr [0104h],0F000h C7 06 04 01 00 F0
    //   0012 shr word ptr [0104h],cl     D3 2E 04 01
    //   0016 stop
    // CX is 0103h so that CL is 3 while CH is not: a count taken from the whole
    // of CX would be 259, which shifts both operands to zero.
    static const uint8_t code[] = {0xC6, 0x06, 0x00, 0x01, 0x03, 0xB9, 0x03, 0x01,
                                   0xD2, 0x26, 0x00, 0x01, 0xC7, 0x06, 0x04, 0x01,
                                   0x00, 0xF0, 0xD3, 0x2E, 0x04, 0x01};
    if (!run(&cpu, code, sizeof(code), 0x0016, 40)) {
      ++g_failures;
    } else {
      expect_u8("shl rm8,cl wrote back", g_mem[0x10100], 0x18);
      expect_u8("shl rm8,cl left the next byte", g_mem[0x10101], 0x00);
      expect_u8("shr rm16,cl wrote back low", g_mem[0x10104], 0x00);
      expect_u8("shr rm16,cl wrote back high", g_mem[0x10105], 0x1E);
      expect_u16("shift did not consume cx", cpu.reg[I8086_CX], 0x0103);
    }
  }

  // --- F6 /4 and F7 /4 mul: where the halves of the product land ---
  {
    //   0000 mov al,10h                 B0 10
    //   0002 mov byte ptr [0100h],24h   C6 06 00 01 24
    //   0007 mul byte ptr [0100h]       F6 26 00 01   (AX <- 10h * 24h = 0240h)
    //   000B mov bx,ax                  8B D8
    //   000D mov ax,1000h               B8 00 10
    //   0010 mov word ptr [0102h],2000h C7 06 02 01 00 20
    //   0016 mul word ptr [0102h]       F7 26 02 01   (DX:AX <- 0200_0000h)
    //   001A stop
    // The byte form's product needs both halves of AX, and the word form's
    // needs DX, so neither passes if the result is truncated or misplaced.
    static const uint8_t code[] = {0xB0, 0x10, 0xC6, 0x06, 0x00, 0x01, 0x24, 0xF6, 0x26,
                                   0x00, 0x01, 0x8B, 0xD8, 0xB8, 0x00, 0x10, 0xC7, 0x06,
                                   0x02, 0x01, 0x00, 0x20, 0xF7, 0x26, 0x02, 0x01};
    if (!run(&cpu, code, sizeof(code), 0x001A, 40)) {
      ++g_failures;
    } else {
      expect_u16("mul rm8 filled all of ax", cpu.reg[I8086_BX], 0x0240);
      expect_u16("mul rm16 low half in ax", cpu.reg[I8086_AX], 0x0000);
      expect_u16("mul rm16 high half in dx", cpu.reg[I8086_DX], 0x0200);
      expect_u16("mul set CF on a wide product", cpu.flags & I8086_CF, I8086_CF);
    }
  }

  if (g_failures != 0) {
    fprintf(stderr, "\n%d execution check(s) failed\n", g_failures);
    return 1;
  }
  printf("exec ok\n");
  return 0;
}
