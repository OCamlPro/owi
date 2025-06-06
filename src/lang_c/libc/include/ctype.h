#ifndef CTYPE_H
#define CTYPE_H

extern int isascii(int c);
extern int isblank(int c);
extern int isalnum(int c);
extern int isalpha(int c);
extern int isdigit(int c);
extern int isspace(int c);

extern int isupper(int c);
extern int islower(int c);

extern int toascii(int c);
extern int tolower(int c);
extern int toupper(int c);

extern int isprint(int c);
extern int ispunct(int c);
extern int iscntrl(int c);

/* fscking GNU extensions! */
extern int isxdigit(int c);

extern int isgraph(int c);

#endif
