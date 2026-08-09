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

/// Reset the CPU, attach the toy bus, and point every segment at 1000h so the
/// program at linear 10000h is addressable as 1000:0000.
static void setup_cpu(I8086 *cpu) {
  i8086_reset(cpu);
  cpu->ctx = NULL;
  cpu->read8 = mem_read8;
  cpu->write8 = mem_write8;
  cpu->in8 = port_in8;
  cpu->out8 = port_out8;
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

  if (g_failures != 0) {
    fprintf(stderr, "\n%d execution check(s) failed\n", g_failures);
    return 1;
  }
  printf("exec ok\n");
  return 0;
}
