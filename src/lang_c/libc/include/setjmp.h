#ifndef _SETJMP_H_
#define _SETJMP_H_

struct __jmp_buf_tag {};

int setjmp(struct __jmp_buf_tag *);

#endif
