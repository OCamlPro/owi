#include <stdio.h>

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

int fclose(FILE *stream) { return 0; }
FILE *fopen(const char *path, const char *mode) { return 0; }
int feof(FILE *stream) { return 0; }
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
  return 0;
}
char *fgets(char *s, int size, FILE *stream) { return 0; }

FILE *popen(const char *command, const char *type) { return 0; }
int pclose(FILE *stream) { return 0; }

int vsnprintf(char *str, size_t size, const char *format, va_list ap) {
  return 0;
}
