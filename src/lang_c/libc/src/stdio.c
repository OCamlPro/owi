#include <stdio.h>

int owi_int(void);

FILE *stdin = &(FILE){0};

int puts(const char *str) { return 0; }

int fputs(const char *s, FILE *stream) { return 0; }

int putchar(int c) { return 0; }

int printf(const char *format, ...) { return 0; }

int fprintf(FILE *stream, const char *format, ...) { return 0; }

int sprintf(char *str, const char *format, ...) { return 0; }

int swprintf(wchar_t *ptr1, const wchar_t *ptr2, ...) { return 0; }

int snprintf(char *str, size_t size, const char *format, ...) { return 0; }

int asprintf(char **ptr, const char *format, ...) { return 0; }

int scanf(const char *format, ...) { return 0; }
int fscanf(FILE *stream, const char *format, ...) { return 0; }
int sscanf(const char *str, const char *format, ...) { return 0; }

int fflush(FILE *stream) { return 0; }
int ferror(FILE *stream) { return 0; }
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *restrict stream) {
  return 0;
}

void perror(const char *s) {}

int fclose(FILE *stream) { return 0; }
FILE *fopen(const char *path, const char *mode) { return 0; }
int feof(FILE *stream) { return 0; }
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
  return 0;
}
char *fgets(char *s, int size, FILE *stream) { return 0; }
long ftell(FILE *stream) { return 0; }

FILE *popen(const char *command, const char *type) { return 0; }
int pclose(FILE *stream) { return 0; }

int vfprintf(FILE *restrict stream, const char *restrict format, va_list ap) {
  return 0;
}
int vsnprintf(char *str, size_t size, const char *format, va_list ap) {
  return 0;
}

int vasprintf(char **s, const char *fmt, va_list ap) { return 0; }

FILE memstream = {0};
FILE *open_memstream(char **ptr, size_t *sizeloc) { return &memstream; }

void setbuf(FILE *stream, char *buf) {
  // nop
  (void)stream;
  (void)buf;
}

int fseek(FILE *stream, long offset, int whence) {
  (void)stream;
  (void)offset;
  (void)whence;
  return 0;
}

int fgetc(FILE *stream) {
  (void)stream;
  int c = owi_int();
  return c & 0xFF;
}

int fputs_unlocked(const char *s, FILE *stream) {
  (void)s;
  (void)stream;
  return 0;
}
