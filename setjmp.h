/*********************************************************************/
/*                                                                   */
/*  This Program Written by Paul Edwards.                            */
/*  Released to the Public Domain                                    */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  setjmp.h - setjmp header file.                                   */
/*                                                                   */
/*********************************************************************/

#ifndef __SETJMP_INCLUDED
#define __SETJMP_INCLUDED

typedef struct {
#if defined(__MVS__) || defined(__CMS__)
    int saveptr;   /* pointer to stack savearea */
    int savelng;  /* length of save area */
    int savestk;  /* where to put it */
    int saver13; /* Where to leave it pointing to */
    int saver14; /* and return address */
#else
    int eax;
    int ebx;
    int ecx;
#endif
    int longj;
    int ret;
} jmp_buf[1];

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

#endif
