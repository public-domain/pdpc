/*********************************************************************/
/*                                                                   */
/*  This Program Written by Paul Edwards.                            */
/*  Released to the Public Domain                                    */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  stdlib.c - implementation of stuff in stdlib.h                   */
/*                                                                   */
/*********************************************************************/

#include "stdlib.h"
#include "signal.h"
#include "string.h"
#include "ctype.h"

/* PDOS and MSDOS use the same interface most of the time */
#ifdef __PDOS__
#define __MSDOS__
#endif

#ifdef __OS2__
#define INCL_DOSMISC
#define INCL_DOSPROCESS
#include <os2.h>
#endif

#ifdef __WIN32__
#include <windows.h>
#endif

#if defined(__MVS__) || defined(__CMS__)
#include "mvssupa.h"
#endif

#ifdef __MVS__
extern int __tso;
#endif

#if USE_MEMMGR
#include "__memmgr.h"
#define MAX_CHUNK 30000000 /* maximum size we will store in memmgr */
    /* Note that you can set MAX_CHUNK to less than REQ_CHUNK */
    /* But don't do this until MVS/380 has been fixed */
#define REQ_CHUNK 30000000 /* size that we request from OS */
void *__lastsup = NULL; /* last thing supplied to memmgr */
#endif

#ifdef __MSDOS__
#ifdef __WATCOMC__
#define CTYP __cdecl
#else
#define CTYP
#endif
void CTYP __allocmem(size_t size, void **ptr);
void CTYP __freemem(void *ptr);
extern unsigned char *__envptr;
void CTYP __exec(char *cmd, void *env);
#endif

void (*__userExit[__NATEXIT])(void);

void *malloc(size_t size)
{
#ifdef __OS2__
    PVOID BaseAddress;
    ULONG ulObjectSize;
    ULONG ulAllocationFlags;
    APIRET rc;

    ulObjectSize = size + sizeof(size_t);
    ulAllocationFlags = PAG_COMMIT | PAG_WRITE | PAG_READ;
    rc = DosAllocMem(&BaseAddress, ulObjectSize, ulAllocationFlags);
    if (rc != 0) return (NULL);
    *(size_t *)BaseAddress = size;
    BaseAddress = (char *)BaseAddress + sizeof(size_t);
    return ((void *)BaseAddress);
#endif
#ifdef __MSDOS__
    void *ptr;

    __allocmem(size, &ptr);
    return (ptr);
#endif
#if USE_MEMMGR
    void *ptr;

    if (size > MAX_CHUNK)
    {
#if defined(__MVS__) || defined(__CMS__)
        /* don't allow this until MVS/380 is fixed */
        /* ptr = __getm(size); */
        ptr = NULL;
#elif defined(__WIN32__)
        ptr = GlobalAlloc(0, size + sizeof(size_t));
        if (ptr != NULL)
        {
            *(size_t *)ptr = size;
            ptr = (char *)ptr + sizeof(size_t);
        }
#endif
    }
    else
    {
        ptr = memmgrAllocate(&__memmgr, size, 0);
        if (ptr == NULL)
        {
            void *ptr2;

#if defined(__MVS__) || defined(__CMS__)
            /* until MVS/380 is fixed, don't do an additional request */
            if (__memmgr.start == NULL)
            {
                ptr2 = __getm(REQ_CHUNK);
            }
            else
            {
                ptr2 = NULL;
            }
#elif defined(__WIN32__)
            ptr2 = GlobalAlloc(0, REQ_CHUNK);
            if (ptr2 != NULL)
            {
                *(size_t *)ptr2 = size;
                ptr2 = (char *)ptr2 + sizeof(size_t);
            }
#endif
            if (ptr2 == NULL)
            {
                return (NULL);
            }
            __lastsup = ptr2;
            memmgrSupply(&__memmgr, ptr2, REQ_CHUNK);
            ptr = memmgrAllocate(&__memmgr, size, 0);
        }
    }
    return (ptr);
#else /* not MEMMGR */
#if defined(__MVS__) || defined(__CMS__)
    return (__getm(size));
#endif
#ifdef __WIN32__
    void *ptr;

    ptr = GlobalAlloc(0, size + sizeof(size_t));
    if (ptr != NULL)
    {
        *(size_t *)ptr = size;
        ptr = (char *)ptr + sizeof(size_t);
    }
    return (ptr);
#endif
#endif /* not MEMMGR */
}

void *calloc(size_t nmemb, size_t size)
{
    void *ptr;
    size_t total;

    if (nmemb == 1)
    {
        total = size;
    }
    else if (size == 1)
    {
        total = nmemb;
    }
    else
    {
        total = nmemb * size;
    }
    ptr = malloc(total);
    if (ptr != NULL)
    {
        memset(ptr, '\0', total);
    }
    return (ptr);
}

void *realloc(void *ptr, size_t size)
{
    char *newptr;
    size_t oldsize;

    if (size == 0)
    {
        free(ptr);
        return (NULL);
    }
#if USE_MEMMGR
    if (memmgrRealloc(&__memmgr, ptr, size) == 0)
    {
        return (ptr);
    }
#endif
    newptr = malloc(size);
    if (newptr == NULL)
    {
        return (NULL);
    }
    if (ptr != NULL)
    {
        oldsize = *((size_t *)ptr - 1);
        if (oldsize < size)
        {
            size = oldsize;
        }
        memcpy(newptr, ptr, size);
        free(ptr);
    }
    return (newptr);
}

void free(void *ptr)
{
#ifdef __OS2__
    if (ptr != NULL)
    {
        ptr = (char *)ptr - sizeof(size_t);
        DosFreeMem((PVOID)ptr);
    }
#endif
#ifdef __MSDOS__
    if (ptr != NULL)
    {
        __freemem(ptr);
    }
#endif
#if USE_MEMMGR
    if (ptr != NULL)
    {
        size_t size;

        size = *((size_t *)ptr - 1);
        if (size > MAX_CHUNK)
        {
#if defined(__MVS__) || defined(__CMS__)
            /* Just ignore this until MVS/380 is fixed */
            /* __freem(ptr); */
#elif defined(__WIN32__)
            GlobalFree(ptr);
#endif
        }
        else
        {
            memmgrFree(&__memmgr, ptr);
        }
    }
#else /* not using MEMMGR */
#if defined(__MVS__) || defined(__CMS__)
    if (ptr != NULL)
    {
        __freem(ptr);
    }
#endif
#ifdef __WIN32__
    if (ptr != NULL)
    {
        GlobalFree(ptr);
    }
#endif
#endif /* not USE_MEMMGR */
    return;
}

void abort(void)
{
    raise(SIGABRT);
    exit(EXIT_FAILURE);
#if !defined(__EMX__) && !defined(__GCC__) && !defined(__WIN32__)
    return;
#endif
}

#if !defined(__EMX__) && !defined(__GCC__) && !defined(__WIN32__)
void __exit(int status);
#else
void __exit(int status) __attribute__((noreturn));
#endif

void exit(int status)
{
    __exit(status);
#if !defined(__EMX__) && !defined(__GCC__) && !defined(__WIN32__)
    return;
#endif
}

/******************************************************************/
/* qsort.c  --  ISO C qsort() function                            */
/*                                                                */
/* Public domain by Raymond Gardner, Englewood CO  February 1991  */
/* Minor mods by Paul Edwards also public domain                  */
/* Mods by Martin Baute also public domain                        */
/*                                                                */
/* Usage:                                                         */
/*     qsort(base, nbr_elements, width_bytes, compare_function);  */
/*        void *base;                                             */
/*        size_t nbr_elements, width_bytes;                       */
/*        int (*compare_function)(const void *, const void *);    */
/*                                                                */
/* Sorts an array starting at base, of length nbr_elements, each  */
/* element of size width_bytes, ordered via compare_function,     */
/* which is called as  (*compare_function)(ptr_to_element1,       */
/* ptr_to_element2) and returns < 0 if element1 < element2,       */
/* 0 if element1 = element2, > 0 if element1 > element2.          */
/* Most refinements are due to R. Sedgewick. See "Implementing    */
/* Quicksort Programs", Comm. ACM, Oct. 1978, and Corrigendum,    */
/* Comm. ACM, June 1979.                                          */
/******************************************************************/

static void memswp(char *i, char *j, size_t size)
{
     char tmp;

     while (size-- > 0)
     {
         tmp = *i;
         *i++ = *j;
         *j++ = tmp;
     };
     return;
}

/* For small sets, insertion sort is faster than quicksort.
   T is the threshold below which insertion sort will be used.
   Must be 3 or larger.
*/
#define T 7

void qsort(void *base,
           size_t nmemb,
           size_t size,
           int (*compar)(const void *, const void *))
{
    char * i;
    char * j;
    size_t thresh     = T * size;
    char * base_      = (char *)base;
    char * limit      = base_ + nmemb * size;

    if ( ( nmemb == 0 ) || ( size == 0 ) || ( base == NULL ) )
    {
        return;
    }

    for ( ;; )
    {
        if ( limit - base_ > thresh ) /* QSort for more than T elements. */
        {
            /* We work from second to last - first will be pivot element. */
            i = base_ + size;
            j = limit - size;
            /* We swap first with middle element, then sort that with second
            and last element so that eventually first element is the median
            of the three - avoiding pathological pivots.
            */
            memswp( ( ( ( (size_t)( limit - base_ ) ) / size ) / 2 )
                    * size + base_, base_, size );
            if ( compar( i, j ) > 0 ) memswp( i, j, size );
            if ( compar( base_, j ) > 0 ) memswp( base_, j, size );
            if ( compar( i, base_ ) > 0 ) memswp( i, base_, size );
            /* Now we have the median for pivot element, entering main
               Quicksort. */
            for ( ;; )
            {
                do
                {
                    /* move i right until *i >= pivot */
                    i += size;
                } while ( compar( i, base_ ) < 0 );
                do
                {
                    /* move j left until *j <= pivot */
                    j -= size;
                } while ( compar( j, base_ ) > 0 );
                if ( i > j )
                {
                    /* break loop if pointers crossed */
                    break;
                }
                /* else swap elements, keep scanning */
                memswp( i, j, size );
            }
            /* move pivot into correct place */
            memswp( base_, j, size );
            /* recurse into larger subpartition, iterate on smaller */
            if ( j - base_ > limit - i )
            {
                /* left is larger */
                qsort( base, ( j - base_ ) / size, size, compar );
                base_ = i;
            }
            else
            {
                /* right is larger */
                qsort( i, ( limit - i ) / size, size, compar );
                limit = j;
            }
        }
        else /* insertion sort for less than T elements */
        {
            for ( j = base_, i = j + size; i < limit; j = i, i += size )
            {
                for ( ; compar( j, j + size ) > 0; j -= size )
                {
                    memswp( j, j + size, size );
                    if ( j == base_ )
                    {
                        break;
                    }
                }
            }
            break;
        }
    }
    return;
}


static unsigned long myseed = 1;

void srand(unsigned int seed)
{
    myseed = seed;
    return;
}

int rand(void)
{
    int ret;

#if defined(__MVS__) || defined(__CMS__)
    /* this is a hack that should be removed. It is to get
       around a bug in the gcc 3.2.3 MVS 3.0 optimizer. */
    myseed = myseed * 1103515245UL;
    ret = (int)(((myseed + 12345) >> 16) & 0x8fff);
#else
    myseed = myseed * 1103515245UL + 12345;
    ret = (int)((myseed >> 16) & 0x8fff);
#endif
    return (ret);
}

double atof(const char *nptr)
{
    return (strtod(nptr, (char **)NULL));
}

double strtod(const char *nptr, char **endptr)
{
    double x = 0.0;
    double xs= 1.0;
    double es = 1.0;
    double xf = 0.0;
    double xd = 1.0;

    while( isspace( (unsigned char)*nptr ) ) ++nptr;
    if(*nptr == '-')
    {
        xs = -1;
        nptr++;
    }
    else if(*nptr == '+')
    {
        nptr++;
    }


    while (1)
    {
        if (isdigit((unsigned char)*nptr))
        {
            x = x * 10 + (*nptr - '0');
            nptr++;
        }
        else
        {
            x = x * xs;
            break;
        }
    }
    if (*nptr == '.')
    {
        nptr++;
        while (1)
        {
            if (isdigit((unsigned char)*nptr))
            {
                xf = xf * 10 + (*nptr - '0');
                xd = xd * 10;
            }
            else
            {
                x = x + xs * (xf / xd);
                break;
            }
            nptr++;
        }
    }
    if ((*nptr == 'e') || (*nptr == 'E'))
    {
        nptr++;
        if (*nptr == '-')
        {
            es = -1;
            nptr++;
        }
        xd = 1;
        xf = 0;
        while (1)
        {
            if (isdigit((unsigned char)*nptr))
            {
                xf = xf * 10 + (*nptr - '0');
                nptr++;
            }
            else
            {
                while (xf > 0)
                {
                    xd *= 10;
                    xf--;
                }
                if (es < 0.0)
                {
                    x = x / xd;
                }
                else
                {
                    x = x * xd;
                }
                break;
            }
        }
    }
    if (endptr != NULL)
    {
        *endptr = (char *)nptr;
    }
    return (x);
}

int atoi(const char *nptr)
{
    return ((int)strtol(nptr, (char **)NULL, 10));
}

long int atol(const char *nptr)
{
    return (strtol(nptr, (char **)NULL, 10));
}

/* this logic is also in vvscanf - if you update this, update
   that one too */

unsigned long int strtoul(const char *nptr, char **endptr, int base)
{
    unsigned long x = 0;
    int undecided = 0;

    if (base == 0)
    {
        undecided = 1;
    }
    while (1)
    {
        if (isdigit((unsigned char)*nptr))
        {
            if (base == 0)
            {
                if (*nptr == '0')
                {
                    base = 8;
                }
                else
                {
                    base = 10;
                    undecided = 0;
                }
            }
            x = x * base + (*nptr - '0');
            nptr++;
        }
        else if (isalpha((unsigned char)*nptr))
        {
            if ((*nptr == 'X') || (*nptr == 'x'))
            {
                if ((base == 0) || ((base == 8) && undecided))
                {
                    base = 16;
                    undecided = 0;
                    nptr++;
                }
                else if (base == 16)
                {
                    /* hex values are allowed to have an optional 0x */
                    nptr++;
                }
                else
                {
                    break;
                }
            }
            else if (base <= 10)
            {
                break;
            }
            else
            {
                x = x * base + (toupper((unsigned char)*nptr) - 'A') + 10;
                nptr++;
            }
        }
        else
        {
            break;
        }
    }
    if (endptr != NULL)
    {
        *endptr = (char *)nptr;
    }
    return (x);
}

long int strtol(const char *nptr, char **endptr, int base)
{
    unsigned long y;
    long x;
    int neg = 0;

    if (*nptr == '-')
    {
        neg = 1;
        nptr++;
    }
    else if (*nptr == '+')
    {
        nptr++;
    }
    y = strtoul(nptr, endptr, base);
    if (neg)
    {
        x = (long)-y;
    }
    else
    {
        x = (long)y;
    }
    return (x);
}

int mblen(const char *s, size_t n)
{
    if (s == NULL)
    {
        return (0);
    }
    if (n == 1)
    {
        return (1);
    }
    else
    {
        return (-1);
    }
}

int mbtowc(wchar_t *pwc, const char *s, size_t n)
{
    if (s == NULL)
    {
        return (0);
    }
    if (n == 1)
    {
        if (pwc != NULL)
        {
            *pwc = *s;
        }
        return (1);
    }
    else
    {
        return (-1);
    }
}

int wctomb(char *s, wchar_t wchar)
{
    if (s != NULL)
    {
        *s = wchar;
        return (1);
    }
    else
    {
        return (0);
    }
}

size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n)
{
    strncpy((char *)pwcs, s, n);
    if (strlen(s) >= n)
    {
        return (n);
    }
    return (strlen((char *)pwcs));
}

size_t wcstombs(char *s, const wchar_t *pwcs, size_t n)
{
    strncpy(s, (const char *)pwcs, n);
    if (strlen((const char *)pwcs) >= n)
    {
        return (n);
    }
    return (strlen(s));
}

#ifdef abs
#undef abs
#endif
int abs(int j)
{
    if (j < 0)
    {
        j = -j;
    }
    return (j);
}

div_t div(int numer, int denom)
{
    div_t x;

    x.quot = numer / denom;
    x.rem = numer % denom;
    return (x);
}

#ifdef labs
#undef labs
#endif
long int labs(long int j)
{
    if (j < 0)
    {
        j = -j;
    }
    return (j);
}

ldiv_t ldiv(long int numer, long int denom)
{
    ldiv_t x;

    x.quot = numer / denom;
    x.rem = numer % denom;
    return (x);
}

int atexit(void (*func)(void))
{
    int x;

    for (x = 0; x < __NATEXIT; x++)
    {
        if (__userExit[x] == 0)
        {
            __userExit[x] = func;
            return (0);
        }
    }
    return (-1);
}

char *getenv(const char *name)
{
#ifdef __OS2__
    PSZ result;

    if (DosScanEnv((void *)name, (void *)&result) == 0)
    {
        return ((char *)result);
    }
#endif
#if defined(__MSDOS__) || defined(__WIN32__)
    char *env;
    size_t lenn;

#ifdef __WIN32__
    env = GetEnvironmentStrings();
#else
    env = (char *)__envptr;
#endif
    lenn = strlen(name);
    while (*env != '\0')
    {
        if (strncmp(env, name, lenn) == 0)
        {
            if (env[lenn] == '=')
            {
                return (&env[lenn + 1]);
            }
        }
        env = env + strlen(env) + 1;
    }
#endif
    return (NULL);
}

/* The following code was taken from Paul Markham's "EXEC" program,
   and adapted to create a system() function.  The code is all
   public domain */

int system(const char *string)
{
#ifdef __OS2__
    char err_obj[100];
    APIRET rc;
    RESULTCODES results;

    if (string == NULL)
    {
        return (1);
    }
    rc = DosExecPgm(err_obj, sizeof err_obj, EXEC_SYNC,
                    (PSZ)string, NULL, &results, (PSZ)string);
    if (rc != 0)
    {
        return (rc);
    }
    return ((int)results.codeResult);
#endif
#ifdef __WIN32__
    BOOL rc;
    PROCESS_INFORMATION pi;
    STARTUPINFO si;
    DWORD ExitCode;

    memset(&si, 0, sizeof si);
    si.cb = sizeof si;
    memset(&pi, 0, sizeof pi);
    rc = CreateProcess(NULL,
                       (char *)string,
                       NULL,
                       NULL,
                       FALSE,
                       0,
                       NULL,
                       NULL,
                       &si,
                       &pi);
    if (!rc)
    {
        return (GetLastError());
    }
    WaitForSingleObject(pi.hProcess, INFINITE);
    GetExitCodeProcess(pi.hProcess, &ExitCode);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    return (ExitCode);
#endif
#ifdef __MSDOS__
    static unsigned char cmdt[140];
    static struct {
        int env;
        unsigned char *cmdtail;
        char *fcb1;
        char *fcb2;
    } parmblock = { 0, cmdt, NULL, NULL };
    size_t len;
    char *cmd;

    if (string == NULL)
    {
        return (1);
    }
    len = strlen(string);
    cmdt[0] = (unsigned char)(len + 3);
    memcpy(&cmdt[1], "/c ", 3);
    memcpy(&cmdt[4], string, len);
    memcpy(&cmdt[len + 4], "\r", 2);
    cmd = getenv("COMSPEC");
    if (cmd == NULL)
    {
        cmd = "\\command.com";
    }
    __exec(cmd, &parmblock);
    return (0);
#endif
#if defined(__MVS__)
    char pgm[9];
    size_t pgm_len;
    size_t cnt;
    char *p;
    
    p = strchr(string, ' ');
    if (p == NULL)
    {
        p = strchr(string, '\0');
    }
    
    pgm_len = p - string;
    /* don't allow a program name greater than 8 */
    
    if (pgm_len > 8)
    {
        return (-1);
    }
    memcpy(pgm, string, pgm_len);
    pgm[pgm_len] = '\0';
    
    /* uppercase the program name */
    for (cnt = 0; cnt < pgm_len; cnt++)
    {
        pgm[cnt] = toupper((unsigned char)pgm[cnt]);
    }
    
    /* point to parms */
    p++;
    
    /* all parms now available */
    /* we use 1 = batch or 2 = tso */
    return (__system(__tso ? 2: 1, pgm_len, pgm, strlen(p), p));
#endif
#if defined(__CMS__)
    /* not implemented yet */
    return (0);
#endif
}

void *bsearch(const void *key, const void *base,
              size_t nmemb, size_t size,
              int (*compar)(const void *, const void *))
{
    size_t try;
    int res;
    const void *ptr;

    while (nmemb > 0)
    {
        try = nmemb / 2;
        ptr = (void *)((char *)base + try * size);
        res = compar(ptr, key);
        if (res == 0)
        {
            return ((void *)ptr);
        }
        else if (res < 0)
        {
            nmemb = nmemb - try - 1;
            base = (const void *)((const char *)ptr + size);
        }
        else
        {
            nmemb = try;
        }
    }
    return (NULL);
}
