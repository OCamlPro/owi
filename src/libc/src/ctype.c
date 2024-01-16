#include <ctype.h>

int isascii(int ch) { return (unsigned int)ch < 128u; }

int isblank(int ch) { return ch == ' ' || ch == '\t'; }

int isalnum(int ch) {
  return (unsigned int)((ch | 0x20) - 'a') < 26u ||
         (unsigned int)(ch - '0') < 10u;
}

int isalpha(int ch) { return (unsigned int)((ch | 0x20) - 'a') < 26u; }

int isdigit(int ch) { return (unsigned int)(ch - '0') < 10u; }

int isspace(int ch) { return (unsigned int)(ch - 9) < 5u || ch == ' '; }

int isupper(int ch) { return (unsigned int)(ch - 'A') < 26u; }

int islower(int ch) { return (unsigned int)(ch - 'a') < 26u; }

int tolower(int ch) {
  if ((unsigned int)(ch - 'A') < 26u)
    ch += 'a' - 'A';
  return ch;
}

inline int toupper(int ch) {
  if ((unsigned int)(ch - 'a') < 26u)
    ch += 'A' - 'a';
  return ch;
}

int toascii(int c) { return (c & 0x7f); }

int isprint(int ch) {
  ch &= 0x7f;
  return (ch >= 32 && ch < 127);
}

int ispunct(int ch) { return isprint(ch) && !isalnum(ch) && !isspace(ch); }

int iscntrl(int ch) { return (unsigned int)ch < 32u || ch == 127; }

int isxdigit(int ch) {
  return (unsigned int)(ch - '0') < 10u ||
         (unsigned int)((ch | 0x20) - 'a') < 6u;
}

int isgraph(int ch) { return (unsigned int)(ch - '!') < 127u - '!'; }
