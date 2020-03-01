del pdpvse.zip
gcccms -D__VSE__ -S -I . start.c
gcccms -D__VSE__ -S -I . stdio.c
gcccms -D__VSE__ -S -I . stdlib.c
gcccms -D__VSE__ -S -I . ctype.c
gcccms -D__VSE__ -S -I . string.c
gcccms -D__VSE__ -S -I . time.c
gcccms -D__VSE__ -S -I . errno.c
gcccms -D__VSE__ -S -I . assert.c
gcccms -D__VSE__ -S -I . locale.c
gcccms -D__VSE__ -S -I . math.c
gcccms -D__VSE__ -S -I . setjmp.c
gcccms -D__VSE__ -S -I . signal.c
gcccms -D__VSE__ -S -I . __memmgr.c
gcccms -D__VSE__ -S -I . pdptest.c
zip -0X pdpvse *.s *.exec *.asm *.mac
