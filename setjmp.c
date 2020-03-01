/*********************************************************************/
/*                                                                   */
/*  This Program Written by Paul Edwards.                            */
/*  Released to the Public Domain                                    */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  setjmp.c - implementation of stuff in setjmp.h                   */
/*                                                                   */
/*********************************************************************/

#include "setjmp.h"

#if defined(__MVS__) || defined(__CMS__)
int __saver(jmp_buf env);
int __loadr(jmp_buf env);
#endif

int setjmp(jmp_buf env)
{
#if defined(__MVS__) || defined(__CMS__)
    env[0].longj = 0;
    __saver(env);
#else
    env[0].eax = 0;
    env[0].ebx = 0;
    env[0].ecx = 0;
    env[0].longj = 0;
    /* env[0].sp = sp */
#endif
    if (env[0].longj == 1)
    {
        if (env[0].ret == 0)
        {
            env[0].ret = 1;
        }
        return (env[0].ret);
    }
    else
    {
        return (0);
    }
}

void longjmp(jmp_buf env, int val)
{
    env[0].longj = 1;
    env[0].ret = val;
    /* load regs */
#if defined(__MVS__) || defined(__CMS__)
    __loadr(env);
#endif
    return;
}
