#include "lst.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Sourcer mnemonics, sorted for bsearch. Anything not here is not treated as
/// an instruction, which is how data lines and labels are filtered out.
static const char *const kMnemonics[] = {
    "aaa",  "aad",   "aam",    "aas",    "adc",   "add",   "and",   "call",  "cbw",   "clc",
    "cld",  "cli",   "cmc",    "cmp",    "cmps",  "cmpsb", "cmpsw", "cwd",   "daa",   "das",
    "dec",  "div",   "esc",    "hlt",    "idiv",  "imul",  "in",    "inc",   "int",   "into",
    "iret", "ja",    "jae",    "jb",     "jbe",   "jc",    "jcxz",  "je",    "jg",    "jge",
    "jl",   "jle",   "jmp",    "jna",    "jnae",  "jnb",   "jnbe",  "jnc",   "jne",   "jng",
    "jnge", "jnl",   "jnle",   "jno",    "jnp",   "jns",   "jnz",   "jo",    "jp",    "jpe",
    "jpo",  "js",    "jz",     "lahf",   "lds",   "lea",   "les",   "lock",  "lodsb", "lodsw",
    "loop", "loope", "loopne", "loopnz", "loopz", "mov",   "movs",  "movsb", "movsw", "mul",
    "neg",  "nop",   "not",    "or",     "out",   "pop",   "popf",  "push",  "pushf", "rcl",
    "rcr",  "rep",   "repe",   "repne",  "repnz", "repz",  "ret",   "retf",  "retn",  "rol",
    "ror",  "sahf",  "sal",    "sar",    "sbb",   "scas",  "scasb", "scasw", "shl",   "shr",
    "stc",  "std",   "sti",    "stosb",  "stosw", "sub",   "test",  "wait",  "xchg",  "xlat",
    "xor",
};

static int cmp_mnemonic(const void *key, const void *elem) {
  return strcmp((const char *)key, *(const char *const *)elem);
}

static bool is_mnemonic(const char *word) {
  return bsearch(
             word,
             kMnemonics,
             sizeof(kMnemonics) / sizeof(kMnemonics[0]),
             sizeof(kMnemonics[0]),
             cmp_mnemonic) != NULL;
}

/// True for the prefix mnemonics, whose operand is the real instruction.
static bool is_prefix_mnemonic(const char *word) {
  return strcmp(word, "rep") == 0 || strcmp(word, "repe") == 0 || strcmp(word, "repne") == 0 ||
      strcmp(word, "repz") == 0 || strcmp(word, "repnz") == 0 || strcmp(word, "lock") == 0;
}

static int hex_digit(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

static bool is_hex4(const char *s) {
  for (unsigned i = 0; i != 4; ++i)
    if (hex_digit(s[i]) < 0)
      return false;
  return true;
}

/// Copy the next whitespace-delimited word from *p into buf, advancing *p.
/// Returns false at end of string.
static bool next_word(const char **p, char *buf, size_t bufSize) {
  const char *s = *p;
  while (*s == ' ' || *s == '\t')
    ++s;
  if (*s == '\0' || *s == ';')
    return false;
  size_t n = 0;
  while (*s != '\0' && *s != ' ' && *s != '\t') {
    if (n + 1 < bufSize)
      buf[n++] = *s;
    ++s;
  }
  buf[n] = '\0';
  *p = s;
  return n != 0;
}

/// Parse one LST line into `out`. Returns false if the line is not an
/// instruction (a comment, a label, a data directive, a page header, ...).
static bool parse_line(const char *line, int lineNo, LstInsn *out) {
  size_t len = strlen(line);
  if (len < 12)
    return false;

  // Columns 0-8 must be SEG:OFF, column 9 a space.
  for (unsigned i = 0; i != 4; ++i)
    if (hex_digit(line[i]) < 0 || hex_digit(line[i + 5]) < 0)
      return false;
  if (line[4] != ':' || line[9] != ' ')
    return false;

  uint16_t addr = 0;
  for (unsigned i = 5; i != 9; ++i)
    addr = (uint16_t)(addr * 16 + hex_digit(line[i]));

  // Column 10 is Sourcer's reference marker (' ', '.' or ','); the byte field
  // starts at column 11 and runs to the first run of two or more spaces.
  const char *field = line + 11;
  const char *end = field;
  for (;;) {
    if (*end == '\0')
      break;
    if (end[0] == ' ' && end[1] == ' ')
      break;
    ++end;
  }

  // The field must be hex groups separated by ' ', ':' or '/'. Groups are two
  // hex digits (one byte) or four (one little-endian word).
  uint8_t bytes[LST_MAX_INSN_BYTES];
  unsigned nbytes = 0;
  const char *p = field;
  while (p < end) {
    if (*p == ' ' || *p == ':' || *p == '/') {
      ++p;
      continue;
    }
    if (hex_digit(p[0]) < 0 || p + 1 >= end || hex_digit(p[1]) < 0)
      return false;
    if (p + 4 <= end && is_hex4(p)) {
      unsigned v = 0;
      for (unsigned i = 0; i != 4; ++i)
        v = v * 16 + (unsigned)hex_digit(p[i]);
      if (nbytes + 2 > LST_MAX_INSN_BYTES)
        return false;
      bytes[nbytes++] = (uint8_t)(v & 0xFF);
      bytes[nbytes++] = (uint8_t)(v >> 8);
      p += 4;
    } else {
      unsigned v = (unsigned)(hex_digit(p[0]) * 16 + hex_digit(p[1]));
      if (nbytes + 1 > LST_MAX_INSN_BYTES)
        return false;
      bytes[nbytes++] = (uint8_t)v;
      p += 2;
    }
  }
  if (nbytes == 0)
    return false;

  // First token after the byte field is the mnemonic; a second token of "db"
  // or "dw" means the first was actually a data label.
  const char *rest = end;
  char word[12], word2[12];
  if (!next_word(&rest, word, sizeof(word)))
    return false;
  if (!is_mnemonic(word))
    return false;

  bool haveSecond = next_word(&rest, word2, sizeof(word2));
  if (haveSecond && (strcmp(word2, "db") == 0 || strcmp(word2, "dw") == 0))
    return false;

  out->addr = addr;
  out->len = (uint8_t)nbytes;
  memcpy(out->bytes, bytes, nbytes);
  out->line = lineNo;

  if (is_prefix_mnemonic(word) && haveSecond)
    snprintf(out->mnemonic, sizeof(out->mnemonic), "%s %s", word, word2);
  else
    snprintf(out->mnemonic, sizeof(out->mnemonic), "%s", word);

  return true;
}

static int cmp_insn(const void *a, const void *b) {
  uint16_t x = ((const LstInsn *)a)->addr, y = ((const LstInsn *)b)->addr;
  return x < y ? -1 : x > y ? 1 : 0;
}

bool lst_load(const char *path, LstFile *out) {
  out->insns = NULL;
  out->count = 0;

  FILE *f = fopen(path, "rb");
  if (!f) {
    fprintf(stderr, "lst_load: cannot open %s\n", path);
    return false;
  }

  size_t capacity = 4096;
  LstInsn *insns = malloc(capacity * sizeof(*insns));
  if (!insns) {
    fclose(f);
    fprintf(stderr, "lst_load: out of memory\n");
    return false;
  }
  size_t count = 0;

  char line[512];
  int lineNo = 0;
  while (fgets(line, sizeof(line), f)) {
    ++lineNo;
    // Strip CR/LF; the LST is a DOS-era file.
    size_t n = strlen(line);
    while (n != 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
      line[--n] = '\0';

    LstInsn ins;
    if (!parse_line(line, lineNo, &ins))
      continue;

    if (count == capacity) {
      capacity *= 2;
      LstInsn *grown = realloc(insns, capacity * sizeof(*insns));
      if (!grown) {
        free(insns);
        fclose(f);
        fprintf(stderr, "lst_load: out of memory\n");
        return false;
      }
      insns = grown;
    }
    insns[count++] = ins;
  }
  fclose(f);

  qsort(insns, count, sizeof(*insns), cmp_insn);
  out->insns = insns;
  out->count = count;
  return true;
}

void lst_free(LstFile *lst) {
  free(lst->insns);
  lst->insns = NULL;
  lst->count = 0;
}

const LstInsn *lst_find(const LstFile *lst, uint16_t addr) {
  size_t lo = 0, hi = lst->count;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    if (lst->insns[mid].addr == addr)
      return &lst->insns[mid];
    if (lst->insns[mid].addr < addr)
      lo = mid + 1;
    else
      hi = mid;
  }
  return NULL;
}
