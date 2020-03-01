/*********************************************************************/
/*                                                                   */
/*  This Program Written By Paul Edwards.                            */
/*  Released to the public domain.                                   */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  This program reads from an input file and writes to an output    */
/*  file.                                                            */
/*                                                                   */
/*********************************************************************/

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    FILE *fp;
    FILE *fq;
    int c;
    int off = 0;
    char *in = "r";
    char *out = "w";
    
    if (argc < 3)
    {
        printf("usage: copyfile [-bb/-tt/-tb/-bt] <infile> <outfile>\n");
        printf("default is text to text copy\n");
        return (EXIT_FAILURE);
    }

    if (argc > 3)
    {
        if (argv[1][0] == '-')
        {
            if (argv[1][1] == 'b')
            {
                in = "rb";
            }
            if (argv[1][2] == 'b')
            {
                out = "wb";
            }
            off++;
        }
    }
    fp = fopen(*(argv + off + 1), in);
    if (fp == NULL)
    {
        printf("failed to open %s for reading\n", *(argv + off + 1));
        return (EXIT_FAILURE);
    }

    fq = fopen(*(argv + off + 2), out);
    if (fq == NULL)
    {
        printf("failed to open %s for writing\n", *(argv + off + 2));
        return (EXIT_FAILURE);
    }

    while ((c = fgetc(fp)) != EOF)
    {
        fputc(c, fq);
    }

    if (ferror(fp) || ferror(fq))
    {
        printf("i/o error\n");
        return (EXIT_FAILURE);
    }
    
    return (0);
}
