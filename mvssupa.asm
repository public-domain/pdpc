***********************************************************************
*
*  This program written by Paul Edwards.
*  Released to the public domain
*
*  Extensively modified by others
*
***********************************************************************
*
*  MVSSUPA - Support routines for PDPCLIB under MVS
*
*  It is currently coded for GCC, but C/370 functionality is
*  still there, it's just being tested after any change.
*
***********************************************************************
*
* Note that the VBS support may not be properly implemented.
* Note that this code issues WTOs. It should be changed to just
* set a return code an exit gracefully instead. I'm not talking
* about that dummy WTO. But on the subject of that dummy WTO - it
* should be made consistent with the rest of PDPCLIB which doesn't
* use that to set the RMODE/AMODE. It should be consistent one way
* or the other.
*
* Here are some of the errors reported:
*
*  OPEN input failed return code is: -37
*  OPEN output failed return code is: -39
*
* FIND input member return codes are:
* Original, before the return and reason codes had
* negative translations added refer to copyrighted:
* DFSMS Macro Instructions for Data Sets
* RC = 0 Member was found.
* RC = -1024 Member not found.
* RC = -1028 RACF allows PDSE EXECUTE, not PDSE READ.
* RC = -1032 PDSE share not available.
* RC = -1036 PDSE is OPENed output to a different member.
* RC = -2048 Directory I/O error.
* RC = -2052 Out of virtual storage.
* RC = -2056 Invalid DEB or DEB not on TCB or TCBs DEB chain.
* RC = -2060 PDSE I/O error flushing system buffers.
* RC = -2064 Invalid FIND, no DCB address.
*
***********************************************************************
*   Changes by Gerhard Postpischil:
*     EQU * for entry points deleted (placed labels on SAVE) to avoid
*       0C6 abends when Equ follows a LTORG
*     Fixed 0C4 abend in RECFM=Vxxx processing; fixed PUT length error.
*     Deleted unnecessary and duplicated instructions
*     Added @@SYSTEM and @@DYNAL routines                2008-06-10
*     Added @@IDCAMS non-reentrant, non-refreshable      2008-06-17
***********************************************************************
*
         MACRO ,             COMPILER DEPENDENT LOAD INTEGER
&NM      LDINT &R,&A         LOAD INTEGER VALUE FROM PARM LIST
         GBLC  &COMP         COMPILER GCC OR C/370
&NM      L     &R,&A         LOAD PARM VALUE
         AIF ('&COMP' EQ 'GCC').MEND
* THIS LINE IS FOR ANYTHING NOT GCC: C/370
         L     &R,0(,&R)     LOAD INTEGER VALUE
.MEND    MEND  ,
         SPACE 1
         MACRO ,             PATTERN FOR @@DYNAL'S DYNAMIC WORK AREA
&NM      DYNPAT &P=MISSING-PFX
.*   NOTE THAT EXTRA FIELDS ARE DEFINED FOR FUTURE EXPANSION
.*
&NM      DS    0D            ALLOCATION FIELDS
&P.ARBP  DC    0F'0',X'80',AL3(&P.ARB) RB POINTER
&P.ARB   DC    0F'0',AL1(20,S99VRBAL,0,0)
         DC    A(0,&P.ATXTP,0,0)       SVC 99 REQUEST BLOCK
&P.ATXTP DC    10A(0)
&P.AXVOL DC    Y(DALVLSER,1,6)
&P.AVOL  DC    CL6' '
&P.AXDSN DC    Y(DALDSNAM,1,44)
&P.ADSN  DC    CL44' '
&P.AXMEM DC    Y(DALMEMBR,1,8)
&P.AMEM  DC    CL8' '
&P.AXDSP DC    Y(DALSTATS,1,1)
&P.ADSP  DC    X'08'         DISP=SHR
&P.AXFRE DC    Y(DALCLOSE,0)   FREE=CLOSE
&P.AXDDN DC    Y(DALDDNAM,1,8)    DALDDNAM OR DALRTDDN
&P.ADDN  DC    CL8' '        SUPPLIED OR RETURNED DDNAME
&P.ALEN  EQU   *-&P.ARBP       LENGTH OF REQUEST BLOCK
         SPACE 1
&P.URBP  DC    0F'0',X'80',AL3(&P.URB) RB POINTER
&P.URB   DC    0F'0',AL1(20,S99VRBUN,0,0)
         DC    A(0,&P.UTXTP,0,0)       SVC 99 REQUEST BLOCK
&P.UTXTP DC    X'80',AL3(&P.UXDDN)
&P.UXDDN DC    Y(DUNDDNAM,1,8)
&P.UDDN  DC    CL8' '        RETURNED DDNAME
&P.ULEN  EQU   *-&P.URBP       LENGTH OF REQUEST BLOCK
&P.DYNLN EQU   *-&P.ARBP     LENGTH OF ALL DATA
         MEND  ,
         SPACE 2
         COPY  PDPTOP
*
         CSECT ,
         PRINT GEN
* YREGS IS NOT AVAILABLE WITH IFOX
*         YREGS
SUBPOOL  EQU   0
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
* External variables. Note that these variables will eventually need
* to be moved into a CRAB or something to allow reentrancy. Note that
* GCC doesn't currently produce reentrant code, so you will need to
* solve that problem first.
*
* MANSTK contains the address of the main stack. MANSTL has the length
* of that stack. This is used for setjmp/longjmp so that the stack can
* be copied.
*
         ENTRY   @@MANSTK
@@MANSTK DS    F
         ENTRY   @@MANSTL
@@MANSTL DS    F
*
* PGMPRM contains the R1 that was passed to the main program, ie
* when you go EXEC PGM=xxx,PARM='hello there' - that "hello there"
* is passed to the program, although not directly, I think R1
* points to a list of parameters that you'll need to map via
* some appropriate macro. Note that the address stored here will
* be 31-bit clean.
*
         ENTRY   @@PGMPRM
@@PGMPRM DS    F
*
* SYSANC contains an anchor for the use of @@SYSTEM which
* needs to figure out its operating environment.
*
         ENTRY   @@SYSANC
@@SYSANC DC    A(0)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  AOPEN- Open a data set
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@AOPEN
@@AOPEN  SAVE  (14,12),,@@AOPEN  Save caller's regs.
         LR    R12,R15
         USING @@AOPEN,R12
         LR    R11,R1
*
         GETMAIN RU,LV=WORKLEN,SP=SUBPOOL
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
         LR    R1,R11
         USING WORKAREA,R13
*
         L     R3,00(,R1)         R3 POINTS TO DDNAME
         L     R4,04(,R1)         R4 is the MODE.  0=input 1=output
         L     R5,08(,R1)         R5 POINTS TO RECFM
         L     R8,12(,R1)         R8 POINTS TO LRECL
         L     R9,16(,R1)         R9 POINTS TO MEMBER NAME (OF PDS)
         LA    R9,00(,R9)         Strip off high-order bit or byte
*
* S/370 can't handle LOC=BELOW
*
         AIF   ('&SYS' NE 'S370').MVS8090  If not S/370 then 380 or 390
         GETMAIN RU,LV=ZDCBLEN,SP=SUBPOOL  No LOC= for S/370
         AGO   .GETOEND
.MVS8090 ANOP  ,                  S/380 or S/390
         GETMAIN RU,LV=ZDCBLEN,SP=SUBPOOL,LOC=BELOW
.GETOEND ANOP
*
         LR    R2,R1              Addr.of storage obtained to its base
         USING IHADCB,R2          Give assembler DCB area base register
         LR    R0,R2              Load output DCB area address
         LA    R1,ZDCBLEN         Load output length of DCB area
         LA    R11,0              Pad of X'00' and no input length
         MVCL  R0,R10             Clear DCB area to binary zeroes
*
         CH    R4,H4              Call with value?
         BL    *+8                Yes; else pointer
         L     R4,0(,R4)          Load C/370 MODE.  0=input 1=output
*
*   Do as much common code for input and output before splitting
*
         LTR   R4,R4              In or Out
         BNZ   OPREPOUT
         MVC   ZDCBAREA(INDCBLN),INDCB  Move input DCB template to work
         MVI   OPENCLOS,X'90'     INPUT/REREAD MODE=24 OPEN/CLOSE list
         B     OPREPCOM
OPREPOUT MVC   ZDCBAREA(OUTDCBLN),OUTDCB
         MVI   OPENCLOS,X'8F'     Initialize MODE=24 OPEN/CLOSE list
OPREPCOM LA    R10,JFCB
* EXIT TYPE 07 + 80 (END OF LIST INDICATOR)
         ICM   R10,B'1000',=X'87'
         ST    R10,DCBXLST
         LA    R10,DCBXLST
         STCM  R10,B'0111',DCBEXLSA
         MVC   DCBDDNAM,0(R3)
         LA    R3,37(R4,R4)       Preset OPEN error code
         DEVTYPE DCBDDNAM,DEVINFO   Check device type
         BXH   R15,R15,FREEDCB    No DD card or ?
         RDJFCB ((R2)),MF=(E,OPENCLOS)  Read JOB File Control Blk
         CLI   DEVINFO+2,X'20' UCB3DACC   Is it a DASD device?
         BNE   OPNODSCB           No; no member name supported
* CAMLST CAMLST SEARCH,DSNAME,VOLSER,DSCB+44
*
         MVC   CAMLST(4),CAMDUM   Copy CAMLST flags to work area
         LA    R15,JFCBDSNM       Load address of output data set name
         LA    R0,JFCBVOLS        Load addr. of output data set volser
         LA    R1,DS1FMTID        Load address of where to put DSCB
         STM   R15,R1,CAMLST+4    Complete CAMLST addresses
         OBTAIN CAMLST            Read the VTOC record
OPNODSCB DS    0H
* The member name may not be below the line, which may stuff up
* the "FIND" macro, so make sure it is in 24-bit memory.
         LTR   R9,R9              See if an address for the member name
         BZ    NOMEM              No member name, skip copying
         MVC   MEMBER24,0(R9)
         LA    R9,MEMBER24
NOMEM    DS    0H
*
         LTR   R4,R4              See if OPEN input or output
         BNZ   WRITING
*
* READING
         MVC   EOFR24(EOFRLEN),ENDFILE   Put EOF code below the line
         MVC   DECB(READLEN),READDUM  MF=L READ MACRO to work area
         LA    R1,EOFR24
         STCM  R1,B'0111',DCBEODA
*   N.B. moved RDJFCB prior to member test to allow uniform OPEN and
*        other code. Makes debugging and maintenance easier
*
         OI    JFCBTSDM,JFCNWRIT  Don't mess with DSCB
         CLI   DEVINFO+2,X'20' UCB3DACC   Is it a DASD device?
         BNE   OPENVSEQ           No; no member name supported
* See if DSORG=PO but no member so set LRECL&BLKSIZE=256 read directory
         TM    DS1DSORG,DS1DSGPO  See if DSORG=PO
         BZ    OPENVSEQ           Not PDS, don't read PDS directory
         LTR   R9,R9              See if an address for the member name
         BNZ   OPENMEM            Is member name - BPAM access
         TM    JFCBIND1,JFCPDS    See if a member name in JCL
         BZ    OPENDIR            No; read directory
         MVC   MEMBER24,JFCBELNM  Save the member name
         NI    JFCBIND1,255-JFCPDS    Reset it
         XC    JFCBELNM,JFCBELNM  Delete it
         LA    R9,MEMBER24        Force FIND to prevent 013 abend
         B     OPENMEM            Change DCB to BPAM PO
* At this point, we have a PDS but no member name requested.
* Request must be to read the PDS directory
OPENDIR  MVC   DCBBLKSI,=H'256'   Set DCB BLKSIZE to 256
         MVC   DCBLRECL,=H'256'   Set DCB LRECL to 256
         MVI   DCBRECFM,DCBRECF   Set DCB RECFM to RECFM=F (notU?)
         B     OPENIN
OPENMEM  MVI   DCBDSRG1,DCBDSGPO  Replace DCB DSORG=PS with PO
         OI    JFCBTSDM,JFCVSL    Force OPEN analysis of JFCB
         B     OPENIN
OPENVSEQ LTR   R9,R9              Member name for sequential?
         BNZ   BADOPIN            Yes, fail
         SPACE 1
OPENIN   DS    0H
         OPEN  MF=(E,OPENCLOS),TYPE=J  Open the data set
         TM    DCBOFLGS,DCBOFOPN  Did OPEN work?
         BZ    BADOPIN            OPEN failed, go return error code -37
         MVC   LRECL+2(2),DCBLRECL  Copy LRECL to a fullword
         MVC   BLKSIZE+2(2),DCBBLKSI  Copy BLKSIZE to a fullword
         LTR   R9,R9              See if an address for the member name
         BZ    GETBUFF            No member name, skip finding it
*
         FIND  (R2),(R9),D        Point to the requested member
*
         LTR   R15,R15            See if member found
         BZ    GETBUFF            Member found, go get an input buffer
* If FIND return code not zero, process return and reason codes and
* return to caller with a negative return code.
         SLL   R15,8              Shift return code for reason code
         OR    R15,R0             Combine return code and reason code
         LR    R3,R15             Number to generate return and reason
         CLOSE MF=(E,OPENCLOS)    Close, FREEPOOL not needed
FREEDCB  DS    0H
         FREEMAIN RU,LV=ZDCBLEN,A=(R2),SP=SUBPOOL  Free DCB area
         LCR   R2,R3              Set return and reason code
         B     RETURNOP           Go return to caller with negative RC
BADOPIN  DS    0H
         LA    R3,37              Load OPEN input error return code
         B     FREEDCB            Go free the DCB area
BADOPOUT DS    0H
         LA    R3,39              Load OPEN output error return code
         B     FREEDCB            Go free the DCB area
         SPACE 1
GETBUFF  DS    0H
         L     R6,BLKSIZE         Load the input blocksize
         LA    R6,4(,R6)          Add 4 in case RECFM=U buffer
         GETMAIN RU,LV=(R6),SP=SUBPOOL  Get input buffer storage
         ST    R1,BUFFADDR        Save the buffer address for READ
         XC    0(4,R1),0(R1)      Clear the RECFM=U Record Desc. Word
         TM    DCBRECFM,DCBRECU   RECFM=V possible?
         BNM   DONEOPEN           No; done
         TM    DCBRECFM,DCBRECV+DCBRECSB  See if spanned records
         BNO   DONEOPEN           Not RECFM=VS, VBS, etc. spanned, go
         L     R6,LRECL           Load the input VBS LRECL
         CL    R6,F32760          See if LRECL=X
         BNH   GETVBS             Not LRECL=X, just get LRECL
         L     R6,F65536          Allow records up to 64K input
GETVBS   DS    0H
         GETMAIN RU,LV=(R6),SP=SUBPOOL  Get VBS build record area
         ST    R1,VBSADDR         Save the VBS record build area addr.
         AR    R1,R6              Add size GETMAINed to find end
         ST    R1,VBSEND          Save address after VBS rec.build area
         B     DONEOPEN           Go return to caller with DCB info
*
WRITING  DS    0H
         LTR   R9,R9
         BZ    WNOMEM
         CLI   DEVINFO+2,X'20'    UCB3DACC
         BNE   BADOPOUT           Member name invalid
         TM    DS1DSORG,DS1DSGPO  See if DSORG=PO
         BZ    BADOPOUT           Is not PDS, fail request
         MVC   JFCBELNM,0(R9)
         OI    JFCBIND1,JFCPDS
         OI    JFCBTSDM,JFCVSL    Just in case
         B     WNOMEM2            Go to move DCB info
WNOMEM   DS    0H
         TM    JFCBIND1,JFCPDS    See if a member name in JCL
         BO    WNOMEM2            Is member name, go to continue OPEN
* See if DSORG=PO but no member so OPEN output would destroy directory
         TM    DS1DSORG,DS1DSGPO  See if DSORG=PO
         BZ    WNOMEM2            Is not PDS, go OPEN
         WTO   'MVSSUPA - No member name for output PDS',ROUTCDE=11
         WTO   'MVSSUPA - Refuses to write over PDS directory',        C
               ROUTCDE=11
         ABEND 123                Abend without a dump
         DC    H'0'               Ensure that abend took
WNOMEM2  DS    0H
         OPEN  MF=(E,OPENCLOS),TYPE=J
         TM    DCBOFLGS,DCBOFOPN  Did OPEN work?
         BZ    BADOPOUT           OPEN failed, go return error code -39
         MVC   LRECL+2(2),DCBLRECL  Copy LRECL to a fullword
         MVC   BLKSIZE+2(2),DCBBLKSI  Copy BLKSIZE to a fullword
         AIF   ('&OUTM' NE 'M').NMM4
         L     R6,=F'32768'
* Give caller an internal buffer to write to. Below the line!
*
* S/370 can't handle LOC=BELOW
*
         AIF   ('&SYS' NE 'S370').MVT8090  If not S/370 then 380 or 390
         GETMAIN RU,LV=(R6),SP=SUBPOOL  No LOC= for S/370
         AGO   .GETOENE
.MVT8090 ANOP  ,                  S/380 or S/390
         GETMAIN RU,LV=(R6),SP=SUBPOOL,LOC=BELOW
.GETOENE ANOP
         ST    R1,ASMBUF
* In move move mode, we will return this two fullword control
* block instead of the DCB area
         ST    R2,BEGINDCB
         LA    R7,BEGINDCB
         B     DONEOPEW
.NMM4    ANOP
         SPACE 1
*   Lots of code tests DCBRECFM twice, to distinguish among F, V, and
*     U formats. We set the index byte to 0,4,8 to allow a single test
*     with a three-way branch.
DONEOPEN LR    R7,R2
DONEOPEW DS    0H
         LA    R0,8
         TM    DCBRECFM,DCBRECU   Undefined ?
         BO    SETINDEX           Yes
         BM    GETINDFV           No
         TM    DCBRECFM,DCBRECTO  RECFM=D
         BZ    SETINDEX           No; treat as U
         LA    R0,4               Yes; treat as V
         B     SETINDEX
GETINDFV LA    R0,4               Preset for V
         TM    DCBRECFM,DCBRECV   Undefined ?
         BO    SETINDEX           Yes
         SR    R0,R0              Else set for F
SETINDEX STC   R0,RECFMIX         Save for the duration
         L     R1,LRECL           Load RECFM F or V max. record length
         TM    DCBRECFM,DCBRECU   See if RECFM=U
         BNO   NOTU               Not RECFM=U, go leave LRECL as LRECL
         L     R1,BLKSIZE         Load RECFM U maximum record length
         LTR   R4,R4              See if OPEN input or output
         BNZ   NOTU               For output, skip adding RDW bytes
         LA    R1,4(,R1)          Add four for fake RECFM=U RDW
NOTU     DS    0H
         ST    R1,0(,R8)          Return record length back to caller
         SR    R1,R1              Set F
         CLI   RECFMIX,4          See if RECFM=F, V, or U
         BL    SETRECFM           Is RECFM=F, go return "0" to caller
         LA    R1,1               Set V for V and U
         BE    SETRECFM           if V
*        LTR   R4,R4              In or Out ?
*        BZ    SETRECFM           Input - set V
*        SR    R1,R1              Out - set F
         LA    R1,2
* RECFM=U on input  will tell caller that it is RECFM=V
* RECFM=U on output will tell caller that it is RECFM=F
* This logic now moved to the C code
SETRECFM DS    0H
         ST    R1,0(,R5)          Pass either RECFM F or V to caller
         B     RETURNOQ
*
RETURNOP DS    0H
         LR    R7,R2
RETURNOQ DS    0H
         LR    R1,R13
         L     R13,SAVEAREA+4
         FREEMAIN RU,LV=WORKLEN,A=(1),SP=SUBPOOL
*
         LR    R15,R7             Return neg.RC or GETMAINed area addr.
         RETURN (14,12),RC=(15)   Return to caller
*
* This is not executed directly, but copied into 24-bit storage
ENDFILE  LA    R6,1               Indicate @@AREAD reached end-of-file
         BR    R14                Return to instruction after the GET
EOFRLEN  EQU   *-ENDFILE
*
         LTORG ,
INDCB    DCB   MACRF=R,DSORG=PS   If member name, will be changed to PO
INDCBLN  EQU   *-INDCB
F32760   DC    F'32760'           Constant for compare
F65536   DC    F'65536'           Maximum VBS record GETMAIN length
*
* OUTDCB changes depending on whether we are in LOCATE mode or
* MOVE mode
         AIF   ('&OUTM' NE 'L').NLM1
OUTDCB   DCB   MACRF=PL,DSORG=PS
.NLM1    ANOP
         AIF   ('&OUTM' NE 'M').NMM1
OUTDCB   DCB   MACRF=PM,DSORG=PS
.NMM1    ANOP
OUTDCBLN EQU   *-OUTDCB
*
READDUM  READ  NONE,              Read record Data Event Control Block C
               SF,                Read record Sequential Forward       C
               ,       (R2),      Read record DCB address              C
               ,       (R4),      Read record input buffer             C
               ,       (R5),      Read BLKSIZE or 256 for PDS.DirectoryC
               MF=L               List type MACRO
READLEN  EQU   *-READDUM
*
*
* CAMDUM CAMLST SEARCH,DSNAME,VOLSER,DSCB+44
CAMDUM   CAMLST SEARCH,*-*,*-*,*-*
CAMLEN   EQU   *-CAMDUM           Length of CAMLST Template
         ORG   CAMDUM+4           Don't need rest
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  AREAD - Read from an open data set
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@AREAD
@@AREAD  SAVE  (14,12),,@@AREAD
         LR    R12,R15
         USING @@AREAD,R12
         L     R2,0(,R1)          R2 contains GETMAINed address/handle
         USING IHADCB,R2
         L     R3,4(,R1)  R3 points to where to store record pointer
         USING ZDCBAREA,R2
*        GETMAIN RU,LV=WORKLEN,SP=SUBPOOL
         LA    R1,SAVEADCB
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
         USING WORKAREA,R13
         LA    R6,1               Prepare
         TM    IOFLAGS,IOFLEOF    Prior EOF ?
         BNZ   READEXIT           Yes; don't abend
*   Return here for empty record or end-of-block
*
REREAD   SLR   R6,R6              Clear default end-of-file indicator
         ICM   R5,B'1111',BUFFCURR  Load address of next record
         BNZ   DEBLOCK            Block in memory, go de-block it
         L     R4,BUFFADDR        Load address of input buffer
         TM    DCBRECFM,DCBRECU   See if RECFM=U
         BNO   READ               Not RECFM=U, go read a block
         LA    R4,4(,R4)          Read RECFM=U four bytes into buffer
         L     R5,BLKSIZE         Load block size to read
READ     DS    0H
*
         AIF   ('&SYS' NE 'S380').N380RD1
         CALL  @@SETM24
.N380RD1 ANOP
*
         READ  DECB,              Read record Data Event Control Block C
               SF,                Read record Sequential Forward       C
               (R2),              Read record DCB address              C
               (R4),              Read record input buffer             C
               (R5),              Read BLKSIZE or 256 for PDS.DirectoryC
               MF=E               Execute a MF=L MACRO
*                                 If EOF, R6 will be set to F'1'
         CHECK DECB               Wait for READ to complete
         AIF   ('&SYS' NE 'S380').N380RD2
         CALL  @@SETM31
.N380RD2 ANOP
*
         LTR   R6,R6              See if end of input data set
         BNZ   READEOD            Is end, go return to caller
* If RECFM=FB or U, store BUFFADDR in BUFFCURR
* If RECFM=V, VB, VBS, etc. store BUFFADDR+4 in BUFFCURR
         LR    R5,R4              Copy buffer address to init BUFFCURR
         CLI   RECFMIX,4          See if RECFM=V
         BNE   NOTV               Is RECFM=U or F, so not RECFM=V
         LH    R0,0(,R4)          Get presumed block length
         C     R0,BLKSIZE         Valid?
         BH    BADBLOCK           No
         ICM   R0,3,2(R4)         Garbage in BDW?
         BNZ   BADBLOCK           Yes; fail
         LA    R5,4(,R4)          Bump buffer address past BDW
NOTV     DS    0H
* Subtract residual from BLKSIZE, add BUFFADDR, store as BUFFEND
         ST    R5,BUFFCURR        Indicate data available
         L     R8,DECIOBPT        Load address of Input/Output Block
         USING IOBSTDRD,R8        Give assembler IOB base
         SLR   R7,R7              Clear residual amount work register
         ICM   R7,B'0011',IOBCSW+5  Load residual count
         DROP  R8                 Don't need IOB address base anymore
         L     R8,BLKSIZE         Load maximum block size
         SLR   R8,R7              Find block size read in
         LR    R7,R8              Save size of block read in
         AL    R8,BUFFADDR        Find address after block read in
         ST    R8,BUFFEND         Save address after end of input block
*
DEBLOCK  DS    0H
*        R4 has address of block buffer
*        R5 has address of current record
*        If RECFM=U, then R7 has size of block read in
         CLI   RECFMIX,4          Is data set RECFM=U
         BH    DEBLOCKU           Is RECFM=U, go deblock it
         BL    DEBLOCKF           Is RECFM=Fx, go deblock it
*
* Must be RECFM=V, VB, VBS, VS, VA, VM, VBA, VBM, VSA, VSM, VBSA, VBSM
*  VBS SDW ( Segment Descriptor Word ):
*  REC+0 length 2 is segment length
*  REC+2 0 is record not segmented
*  REC+2 1 is first segment of record
*  REC+2 2 is last seqment of record
*  REC+2 3 is one of the middle segments of a record
*        R5 has address of current record
DEBLOCKV CLI   0(R5),X'80'   LOGICAL END OF BLOCK ?
         BE    REREAD        YES; DONE WITH THIS BLOCK
         LH    R8,0(,R5)     GET LENGTH FROM RDW
         CH    R8,H4         AT LEAST MINIMUM ?
         BL    BADBLOCK      NO; BAD RECORD OR BAD BLOCK
         C     R8,LRECL      VALID LENGTH ?
         BH    BADBLOCK      NO
         LA    R7,0(R8,R5)   SET ADDRESS OF LAST BYTE +1
         C     R7,BUFFEND    WILL IT FIT INTO BUFFER ?
         BL    DEBVCURR      LOW - LEAVE IT
         BH    BADBLOCK      NO; FAIL
         SR    R7,R7         PRESET FOR BLOCK DONE
DEBVCURR ST    R7,BUFFCURR        for recursion
         TM    3(R5),X'FF'   CLEAN RDW ?
         BNZ   BADBLOCK
         TM    IOFLAGS,IOFLSDW    WAS PREVIOUS RECORD DONE ?
         BO    DEBVAPND           NO
         SPACE 1
         CLI   2(R5),1            What is this?
         BL    SETCURR            Simple record
         BH    BADBLOCK           Not=1; have a sequence error
         OI    IOFLAGS,IOFLSDW    Starting a new segment
         L     R10,VBSADDR        Get start of buffer
         MVC   0(4,R10),=X'00040000'   Preset null record
         B     DEBVMOVE           And move this
         SPACE 1
DEBVAPND CLI   2(R5),3            IS THIS A MIDDLE SEGMENT ?
         BE    DEBVMOVE           YES, PUT IT OUT
         CLI   2(R5),2            IS THIS THE LAST SEGMENT ?
         BNE   BADBLOCK           No; bad segment sequence
         NI    IOFLAGS,255-IOFLSDW  INDICATE RECORD COMPLETE
DEBVMOVE L     R15,VBSADDR        Get segment assembly area
         SR    R11,R11
         ICM   R11,3,0(R15)       Get amount used so far
         LA    R10,0(R11,R15)     Address for next segment
         LH    R1,0(,R5)          Length of addition
         SH    R1,H4              Data length
         LA    R0,4(,R5)          Skip SDW
         LA    R14,0(R1,R11)      New length
         STH   R14,0(,R15)        Update RDW
         A     R14,VBSADDR        New end address
         C     R14,VBSEND         Will it fit ?
         BH    BADBLOCK
         LR    R11,R1             Move all
         MVCL  R10,R0             Append segment
         TM    IOFLAGS,IOFLSDW    Did last segment?
         BNZ   REREAD             No; get next one
         L     R5,VBSADDR         Give user the assembled record
         B     SETCURR            Done
         SPACE 2
* If BUFFCURR equal BUFFEND, zero BUFFCURR
*
DEBLOCKU DS    0H
* If RECFM=U, a block is treated a variable record
*        R4 has address of block buffer
*        R7 has size of block read in
         LA    R7,4(,R7)          Add four to block size for fake RDW
         S     R4,=F'4'           Go back to BDW
         STH   R7,0(,R4)          Store variable RDW for RECFM=U
         LR    R5,R4              Indicate start of buffer is record
         XC    BUFFCURR,BUFFCURR  Indicate no next record in block
         B     RECBACK            Go store the record addr. for return
*
DEBLOCKF DS    0H
* If RECFM=FB, bump address by lrecl
*        R5 has address of current record
         L     R7,LRECL           Load RECFM=F DCB LRECL
         AR    R7,R5              Find the next record address
* If address=BUFFEND, zero BUFFCURR
SETCURR  CL    R7,BUFFEND         Is it off end of block?
         BL    SETCURS            Is not off, go store it
         LA    R7,0               Clear the next record address
SETCURS  ST    R7,BUFFCURR        Store the next record address
*        BNH   RECBACK            Go store the record addr. for return
RECBACK  DS    0H
         ST    R5,0(,R3)          Store record address for caller
         B     READEXIT
READEOD  OI    IOFLAGS,IOFLEOF    Remeber that we hit EOF
READEXIT DS    0H
*        LR    R1,R13             Save temp.save area addr.for FREEMAIN
*        L     R13,SAVEAREA+4     Restore Caller's save area address
         L     R13,SAVEADCB+4
*        FREEMAIN RU,LV=WORKLEN,A=(1),SP=SUBPOOL  Free temp.save area
         LR    R15,R6             Set return code 1=EOF or 0=not-eof
         RETURN (14,12),RC=(15)   Return to caller
*
BADBLOCK WTO   'MVSSUPA - @@AREAD - problem processing RECFM=V(bs) file*
               ',ROUTCDE=11       Send to programmer and listing
         ABEND 1234,DUMP          Abend U1234 and allow a dump
*
         LTORG ,                  In case someone adds literals
*
H4       DC    H'4'               Constant for BDW/SDW/RDW handling
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  AWRITE - Write to an open data set
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@AWRITE
@@AWRITE SAVE  (14,12),,@@AWRITE
         LR    R12,R15
         USING @@AWRITE,R12
         L     R2,0(,R1)          R2 contains GETMAINed address
         L     R3,4(,R1)          R3 points to the record address
         L     R4,8(,R1)          R4 points to length of data to write
         USING ZDCBAREA,R2
*        GETMAIN RU,LV=WORKLEN,SP=SUBPOOL
         LA    R1,SAVEADCB
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
*        USING WORKAREA,R13
*
         AIF   ('&SYS' NE 'S380').N380WR1
         CALL  @@SETM24
.N380WR1 ANOP
*
         STCM  R4,B'0011',DCBLRECL
*
         AIF   ('&OUTM' NE 'L').NLM2
         PUT   (R2)
.NLM2    ANOP
         AIF   ('&OUTM' NE 'M').NMM2
* In move mode, always use our internal buffer. Ignore passed parm.
         L     R3,ASMBUF
         PUT   (R2),(R3)
.NMM2    ANOP
*
         AIF   ('&SYS' NE 'S380').N380WR2
         CALL  @@SETM31
.N380WR2 ANOP
*
         AIF   ('&OUTM' NE 'L').NLM3
         ST    R1,0(,R3)
.NLM3    ANOP
*        LR    R1,R13
*        L     R13,SAVEAREA+4
         L     R13,SAVEADCB+4
*        FREEMAIN RU,LV=WORKLEN,A=(1),SP=SUBPOOL
         RETURN (14,12),RC=0
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ACLOSE - Close a data set
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@ACLOSE
@@ACLOSE SAVE  (14,12),,@@ACLOSE
         LR    R12,R15
         USING @@ACLOSE,R12
         L     R2,0(,R1)          R2 contains GETMAINed address/handle
         USING ZDCBAREA,R2
         GETMAIN RU,LV=WORKLEN,SP=SUBPOOL
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
         USING WORKAREA,R13
*
* If we are doing move mode, free internal assembler buffer
         AIF   ('&OUTM' NE 'M').NMM6
         L     R5,ASMBUF
         LTR   R5,R5
         BZ    NFRCL
         L     R6,=F'32768'
         FREEMAIN RU,LV=(R6),A=(R5),SP=SUBPOOL
NFRCL    DS    0H
.NMM6    ANOP
         ICM   R1,B'1111',VBSADDR  Load VBS record area
         BZ    FREEBUFF           No area, skip free of it
         L     R0,VBSEND          Load address past end of VBS area
         SLR   R0,R1              Calculate size of VBS record area
         FREEMAIN RU,LV=(0),A=(1),SP=SUBPOOL  Free VBS record area
FREEBUFF DS    0H
         ICM   R1,B'1111',BUFFADDR  Load input buffer address
         BZ    CLOSE              No area, skip free of it
         L     R3,BLKSIZE         Load the BLKSIZE for buffer size
         LA    R0,4(,R3)          Add 4 bytes for RECFM=U
         FREEMAIN RU,LV=(0),A=(1),SP=SUBPOOL  Free input buffer
CLOSE    CLOSE MF=(E,OPENCLOS)
         TM    DCBMACR1,DCBMRRD   See if using MACRF=R, no dynamic buff
         BO    NOPOOL             Is MACRF=R, skip FREEPOOL
         FREEPOOL ((R2))
NOPOOL   DS    0H
         FREEMAIN RU,LV=ZDCBLEN,A=(R2),SP=SUBPOOL
*
         LR    R1,R13
         L     R13,SAVEAREA+4
         FREEMAIN RU,LV=WORKLEN,A=(1),SP=SUBPOOL
         RETURN (14,12),RC=0
         LTORG ,
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  GETM - GET MEMORY
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@GETM
@@GETM   SAVE  (14,12),,@@GETM
         LR    R12,R15
         USING @@GETM,R12
*
         LDINT R3,0(,R1)          LOAD REQUESTED STORAGE SIZE
         LR    R4,R3
         LA    R3,8(,R3)
*
* To avoid fragmentation, round up size to 64 byte multiple
*
         A     R3,=A(64-1)
         N     R3,=X'FFFFFFC0'
*
         AIF   ('&SYS' NE 'S380').N380GM1
         GETMAIN RU,LV=(R3),SP=SUBPOOL,LOC=ANY
         AGO   .N380GM2
.N380GM1 ANOP
         GETMAIN RU,LV=(R3),SP=SUBPOOL
.N380GM2 ANOP
*
* WE STORE THE AMOUNT WE REQUESTED FROM MVS INTO THIS ADDRESS
         ST    R3,0(R1)
* AND JUST BELOW THE VALUE WE RETURN TO THE CALLER, WE SAVE
* THE AMOUNT THEY REQUESTED
         ST    R4,4(R1)
         A     R1,=F'8'
         LR    R15,R1
*
RETURNGM DS    0H
         RETURN (14,12),RC=(15)
         LTORG ,
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  FREEM - FREE MEMORY
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@FREEM
@@FREEM  SAVE  (14,12),,@@FREEM
         LR    R12,R15
         USING @@FREEM,R12
*
         L     R2,0(,R1)
         S     R2,=F'8'
         L     R3,0(,R2)
*
         FREEMAIN RU,LV=(R3),A=(R2),SP=SUBPOOL
*
RETURNFM DS    0H
         RETURN (14,12),RC=(15)
         LTORG ,
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  GETCLCK - GET THE VALUE OF THE MVS CLOCK TIMER AND MOVE IT TO AN
*  8-BYTE FIELD.  THIS 8-BYTE FIELD DOES NOT NEED TO BE ALIGNED IN
*  ANY PARTICULAR WAY.
*
*  E.G. CALL 'GETCLCK' USING WS-CLOCK1
*
*  THIS FUNCTION ALSO RETURNS THE NUMBER OF SECONDS SINCE 1970-01-01
*  BY USING SOME EMPERICALLY-DERIVED MAGIC NUMBERS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@GETCLK
@@GETCLK SAVE  (14,12),,@@GETCLK
         LR    R12,R15
         USING @@GETCLK,R12
*
         L     R2,0(,R1)
         STCK  0(R2)
         L     R4,0(,R2)
         L     R5,4(,R2)
         SRDL  R4,12
         SL    R4,=X'0007D910'
         D     R4,=F'1000000'
         SL    R5,=F'1220'
         LR    R15,R5
*
RETURNGC DS    0H
         RETURN (14,12),RC=(15)
         LTORG ,
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  SAVER - SAVE REGISTERS AND PSW INTO ENV_BUF
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@SAVER
@@SAVER  SAVE  (14,12),,@@SAVER    * SAVE REGS AS NORMAL
         LR    R12,R15
         USING @@SAVER,12
         L     R1,0(,R1)           * ADDRESS OF ENV TO R1
         L     R2,=A(@@MANSTK)
         L     R2,0(R2)            * R2 POINTS TO START OF STACK
         L     R3,=A(@@MANSTL)
         L     R3,0(R3)            * R3 HAS LENGTH OF STACK
         LR    R5,R3               * AND R5
         LR    R9,R1               * R9 NOW CONTAINS ADDRESS OF ENV
* GET A SAVE AREA
         GETMAIN RU,LV=(R3),SP=SUBPOOL
         ST    R1,0(R9)            * SAVE IT IN FIRST WORK OF ENV
         ST    R5,4(R9)            * SAVE LENGTH IN SECOND WORD OF ENV
         ST    R2,8(R9)            * NOTE WHERE WE GOT IT FROM
         ST    R13,12(R9)          * AND R13
         LR    R4,R1               * AND R4
         MVCL  R4,R2               * COPY SETJMP'S SAVE AREA TO ENV
*        STM   R0,R15,0(R1)               SAVE REGISTERS
*        BALR  R15,0                     GET PSW INTO R15
*        ST    R15,64(,R1)                SAVE PSW
*
RETURNSR DS    0H
         SR    R15,R15              * CLEAR RETURN CODE
         RETURN (14,12),RC=(15)
         LTORG ,
*
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  LOADR - LOAD REGISTERS AND PSW FROM ENV_BUF
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@LOADR
@@LOADR  BALR  R12,0
         USING *,R12
         L     R1,0(,R1)          * R1 POINTS TO ENV
         L     R2,8(,R1)          * R2 POINTS TO STACK
         L     R3,4(,R1)          * R3 HAS HOW LONG
         LR    R5,R3              * AS DOES R5
         L     R6,24(,R1)         * R6 HAS RETURN CODE
         L     R4,0(,R1)          * OUR SAVE AREA
         L     R13,12(,R1)        * GET OLD STACK POINTER
         LR    R8,R4              * Save before clobbered by MVCL
         LR    R7,R5              * Save before clobbered by MVCL
         MVCL  R2,R4              * AND RESTORE STACK
         ST    R6,24(,R1)         * SAVE VAL IN ENV
         L     R6,=F'1'
         ST    R6,20(R1)          * AND SET LONGJ TO 1.
         FREEMAIN RU,LV=(R7),A=(R8),SP=SUBPOOL
*        L     R14,16(R1)          * AND RETURN ADDRESS
*        B     RETURNSR            * AND BACK INTO SETJMP
*        L     R15,64(,R1)                RESTORE PSW
*        LM    R0,R15,0(R1)               RESTORE REGISTERS
*        BR    R15                        JUMP TO SAVED PSW
*
RETURNLR DS    0H
         SR    R15,R15            * CLEAR RETURN CODE
         RETURN (14,12),RC=(15)
         LTORG ,
         SPACE 3
***********************************************************************
**                                                                   **
**   CALL @@SYSTEM,(req-type,pgm-len,pgm-name,parm-len,parm),VL      **
**                                                                   **
**   "-len" fields are self-defining values in the calling list,     **
**       or else pointers to 32-bit signed integer values            **
**                                                                   **
**   "pgm-name" is the address of the name of the program to be      **
**       executed (one to eight characters)                          **
**                                                                   **
**   "parm" is the address of a text string of length "parm-len",    **
**       and may be zero to one hundred bytes (OS JCL limit)         **
**                                                                   **
**   "req-type" is or points to 1 for a program ATTACH               **
**                              2 for TSO CP invocation              **
**                                                                   **
*---------------------------------------------------------------------*
**                                                                   **
**    Author:  Gerhard Postpischil                                   **
**                                                                   **
**    This program is placed in the public domain.                   **
**                                                                   **
*---------------------------------------------------------------------*
**                                                                   **
**    Assembly: Any MVS or later assembler may be used.              **
**       Requires SYS1.MACLIB. TSO CP support requires additional    **
**       macros from SYS1.MODGEN (SYS1.AMODGEN in MVS).              **
**       Intended to work in any 24 and 31-bit environment.          **
**                                                                   **
**    Linker/Binder: RENT,REFR,REUS                                  **
**                                                                   **
*---------------------------------------------------------------------*
**    Return codes:  when R15:0 R15 has return from program.         **
**    Else R15 is 0480400n   GETMAIN failed                          **
**      R15 is 04806nnn  ATTACH failed                               **
**      R15 is 1400000n  PARM list error: n= 1,2, or 3 (req/pgm/parm)**
***********************************************************************
         SPACE 2
         ENTRY @@SYSTEM
@@SYSTEM B     SYSATBEG-*(,R15)      SKIP ID
         DC    AL1(9),C'@@SYSTEM &SYSDATE'
SYSATBEG STM   R14,R12,12(R13)    SAVE CALLER'S REGISTERS
         LR    R12,R15            ESTABLISH MY BASE
         USING @@SYSTEM,R12       AND DECLARE IT
         LA    R11,16(,R13)       REMEMBER THE RETURN CODE ADDRESS
         MVC   0(4,R11),=X'04804000'  PRESET FOR GETMAIN FAILURE
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
         LA    R0,SYSATDLN        GET LENGTH OF SAVE AND WORK AREA
         GETMAIN RC,LV=(0)        GET STORAGE
         LTR   R15,R15            SUCCESSFUL ?
         BZ    SYSATHAV           YES
         STC   R15,3(,R11)        SET RETURN VALUES
         B     SYSATRET           RELOAD AND RETURN
         SPACE 1
*    CLEAR GOTTEN STORAGE AND ESTABLISH SAVE AREA
*
SYSATHAV ST    R1,8(,R13)         LINK OURS TO CALLER'S SAVE AREA
         ST    R13,4(,R1)         LINK CALLER'S TO OUR AREA
         LR    R13,R1
         USING SYSATWRK,R13
         XC    SYSATZER,SYSATZER  CLEAR DYNAMIC STUFF
         MVC   0(4,R11),=X'14000002'  PRESET FOR PARM ERROR
         LDINT R4,0(,R9)          REQUEST TYPE
         LDINT R5,4(,R9)          LENGTH OF PROGRAM NAME
         L     R6,8(,R9)          -> PROGRAM NAME
         LDINT R7,12(,R9)         LENGTH OF PARM
         L     R8,16(,R9)         -> PARM TEXT
*   NOTE THAT THE CALLER IS EITHER COMPILER CODE, OR A COMPILER
*   LIBRARY ROUTINE, SO WE DO MINIMAL VALIDITY CHECKING
*
         SPACE 1
*   EXAMINE PROGRAM NAME LENGTH AND STRING
*
         LTR   R5,R5              ANY LENGTH ?
         BNP   SYSATEXT           NO; OOPS
         CH    R5,=H'8'           NOT TOO LONG ?
         BH    SYSATEXT           TOO LONG; TOO BAD
         BCTR  R5,0
         MVC   SYSATPGM(L'SYSATPGM+L'SYSATOTL),=CL10' '  PRE-BLANK
         EX    R5,SYSAXPGM        MOVE PROGRAM NAME
         CLC   SYSATPGM,=CL10' '  STILL BLANK ?
         BE    SYSATEXT           YES; TOO BAD
         SPACE 1
*   BRANCH AND PROCESS ACCORDING TO REQUEST TYPE
*
         MVI   3(R11),1           SET BAD REQUEST TYPE
         CH    R4,=H'2'           CP PROGRAM ATTACH ?
         BE    SYSATCP            YES
         CH    R4,=H'1'           OS PROGRAM ATTACH ?
         BNE   SYSATEXT           NO; HAVE ERROR CODE
         SPACE 2
*   OS PROGRAM ATTACH - PREPARE PARM, ETC.
*
*   NOW LOOK AT PARM STRING
         LTR   R7,R7              ANY LENGTH ?
         BM    SYSATEXT           NO; OOPS
         STH   R7,SYSATOTL        PASS LENGTH OF TEXT
         BZ    SYSATNTX
         CH    R7,=AL2(L'SYSATOTX)  NOT TOO LONG ?
         BH    SYSATEXT           TOO LONG; TOO BAD
         BCTR  R7,0
         EX    R7,SYSAXTXT        MOVE PARM STRING
SYSATNTX LA    R1,SYSATOTL        GET PARAMETER ADDRESS
         ST    R1,SYSATPRM        SET IT
         OI    SYSATPRM,X'80'     SET END OF LIST BIT
         B     SYSATCOM           GO TO COMMON ATTACH ROUTINE
         SPACE 2
*   TSO CP REQUEST - PREPARE PARM, CPPL, ETC.
*
SYSATCP  LTR   R7,R7              ANY LENGTH ?
         BM    SYSATEXT           NO; OOPS
         LA    R1,SYSATOTX-SYSATOPL(,R7)  LENGTH WITH HEADER
         STH   R1,SYSATOPL        PASS LENGTH OF COMMAND TEXT
         LA    R1,1(,R7)
         STH   R1,SYSATOPL+2      LENGTH PROCESSED BY PARSER
         BZ    SYSATXNO
         CH    R7,=AL2(L'SYSATOTX)  NOT TOO LONG ?
         BH    SYSATEXT           TOO LONG; TOO BAD
         BCTR  R7,0
         EX    R7,SYSAXTXT        MOVE PARM STRING
SYSATXNO LA    R1,SYSATOPL        GET PARAMETER ADDRESS
         ST    R1,SYSATPRM        SET IT
*   TO MAKE THIS WORK, WE NEED THE UPD, PSCB, AND ECT ADDRESS.
*   THE FOLLOWING CODE WORKS PROVIDED THE CALLER WAS INVOKED AS A
*   TSO CP, USED NORMAL SAVE AREA CONVENTIONS, AND HASN'T MESSED WITH
*   THE TOP SAVE ARE.
         MVI   3(R11),4           SET ERROR FOR BAD CP REQUEST
         LA    R2,SYSATPRM+8      CPPLPSCB
         EXTRACT (R2),FIELDS=PSB  GET THE PSCB
         PUSH  USING
         L     R1,PSATOLD-PSA     GET THE CURRENT TCB
         USING TCB,R1
         L     R1,TCBFSA          GET THE TOP LEVEL SAVE AREA
         N     R1,=X'00FFFFFF'    KILL TCBIDF BYTE
         POP   USING
         L     R1,24(,R1)         ORIGINAL R1
         LA    R1,0(,R1)            CLEAN IT
         LTR   R1,R1              ANY?
         BZ    SYSATEXT           NO; TOO BAD
         TM    0(R1),X'80'        END OF LIST?
         BNZ   SYSATEXT           YES; NOT CPPL
         TM    4(R1),X'80'        END OF LIST?
         BNZ   SYSATEXT           YES; NOT CPPL
         TM    8(R1),X'80'        END OF LIST?
         BNZ   SYSATEXT           YES; NOT CPPL
         CLC   8(4,R1),SYSATPRM+8   MATCHES PSCB FROM EXTRACT?
         BNE   SYSATEXT           NO; TOO BAD
         MVC   SYSATPRM+4(3*4),4(R1)  COPY UPT, PSCB, ECT
SYSATCOM LA    R1,SYSATPRM        PASS ADDRESS OF PARM ADDRESS
         LA    R2,SYSATPGM        POINT TO NAME
         LA    R3,SYSATECB        AND ECB
         ATTACH EPLOC=(R2),       INVOKE THE REQUESTED PROGRAM         *
               ECB=(R3),SF=(E,SYSATLST)
         LTR   R0,R15             CHECK RETURN CODE
         BZ    SYSATWET           GOOD
         MVC   0(4,R11),=X'04806000'  ATTACH FAILED
         STC   R15,3(,R11)        SET ERROR CODE
         B     SYSATEXT           FAIL
         SPACE 1
SYSATWET ST    R1,SYSATTCB        SAVE FOR DETACH
         WAIT  ECB=SYSATECB       WAIT FOR IT TO FINISH
         MVC   1(3,R11),SYSATECB+1
         MVI   0(R11),0           SHOW CODE FROM PROGRAM
         DETACH SYSATTCB          GET RID OF SUBTASK
         B     SYSATEXT           AND RETURN
SYSAXPGM OC    SYSATPGM(0),0(R6)  MOVE NAME AND UPPER CASE
SYSAXTXT MVC   SYSATOTX(0),0(R8)    MOVE PARM TEXT
         SPACE 1
*    PROGRAM EXIT, WITH APPROPRIATE RETURN CODES
*
SYSATEXT LR    R1,R13        COPY STORAGE ADDRESS
         L     R9,4(,R13)    GET CALLER'S SAVE AREA
         LA    R0,SYSATDLN   GET ORIGINAL LENGTH
         FREEMAIN R,A=(1),LV=(0)  AND RELEASE THE STORAGE
         L     R13,4(,R13)   RESTORE CALLER'S SAVE AREA
SYSATRET LM    R14,R12,12(R13) RESTORE REGISTERS; SET RETURN CODES
         BR    R14           RETURN TO CALLER
         SPACE 2
*    DYNAMICALLY ACQUIRED STORAGE
*
SYSATWRK DSECT ,             MAP STORAGE
         DS    18A           OUR OS SAVE AREA
         SPACE 1
SYSATCLR DS    0F            START OF CLEARED AREA
SYSATECB DS    F             EVENT CONTROL FOR SUBTASK
SYSATTCB DS    A             ATTACH TOKEN FOR CLEAN-UP
SYSATPRM DS    4A            PREFIX FOR CP
SYSATOPL DS    2Y     1/4    PARM LENGTH / LENGTH SCANNED
SYSATPGM DS    CL8    2/4    PROGRAM NAME (SEPARATOR)
SYSATOTL DS    Y      3/4    OS PARM LENGTH / BLANKS FOR CP CALL
SYSATOTX DS    CL100  4/4    NORMAL PARM TEXT STRING
         SPACE 1
SYSATLST ATTACH EPLOC=SYSATPGM,ECB=SYSATECB,SF=L
SYSATZER EQU   SYSATCLR,*-SYSATCLR,C'X'   ADDRESS & SIZE TO CLEAR
SYSATDLN EQU   *-SYSATWRK     LENGTH OF DYNAMIC STORAGE
         CSECT ,             RESTORE
         SPACE 2
***********************************************************************
**                                                                   **
**   INVOKE IDCAMS: CALL @@IDCAMS,(@LEN,@TEXT)                       **
**                                                                   **
***********************************************************************
         PUSH  USING
         DROP  ,
         ENTRY @@IDCAMS
         USING @@IDCAMS,R15
@@IDCAMS B     IDCBEG
         DC    AL1(17),CL17'@@IDCAMS &SYSDATE'
IDCBEG   STM   R14,R12,12(R13)    SAVE CALLER'S REGISTERS
         LR    R12,R15            SET LOCAL BASE
         DROP  R15
         USING @@IDCAMS,R12       DECLARE PROGRAM BASE
         LR    R10,R13            SAVE CALLER'S SAVE AREA
         LA    R13,IDCSAVE        LOAD OUR SAVE AREA
         ST    R10,4(,R13)        BACK LINK
         ST    R13,8(,R10)        DOWN LINK
         LA    R1,0(,R1)          ADDRESS OF IDCAMS REQUEST (V-CON)
         ST    R1,IDC@REQ         SAVE REQUEST ADDRESS
         MVI   EXFLAGS,0          INITIALIZE FLAGS
         LA    R1,AMSPARM         PASS PARAMETER LIST
         LINK  EP=IDCAMS          INVOKE UTILITY
         ST    R15,16(,R10)       SAVE RETURN CODE
         LR    R13,R10
         LM    R14,R12,12(R13)    RESTORE CALLER'S REGS
         BR    R14                RETURN
         POP   USING
         SPACE 2
***************************************************************
* IDCAMS ASYNCHRONOUS EXIT ROUTINE
***************************************************************
         SPACE 1
         PUSH  USING
         DROP  ,
XIDCAMS  STM   R14,R12,12(R13)
         LR    R12,R15
         USING XIDCAMS,R12
         LA    R9,XIDSAVE         SET MY SAVE AREA
         ST    R13,4(,R9)         MAKE BACK LINK
         ST    R9,8(,R13)         MAKE DOWN LINK
         LR    R13,R9             MAKE ACTIVE SAVE AREA
         SR    R15,R15            PRESET FOR GOOD RETURN
         LM    R3,R5,0(R1)        LOAD PARM LIST ADDRESSES
         SLR   R14,R14
         IC    R14,0(,R4)         LOAD FUNCTION
         B     *+4(R14)
         B     XIDCEXIT   OPEN           CODE IN R14 = X'00'
         B     XIDCEXIT   CLOSE          CODE IN R14 = X'04'
         B     XIDCGET    GET SYSIN      CODE IN R14 = X'08'
         B     XIDCPUT    PUT SYSPRINT   CODE IN R14 = X'0C'
         SPACE 1
XIDCGET  TM    EXFLAGS,EXFGET            X'FF' = PRIOR GET ISSUED ?
         BNZ   XIDCGET4                  YES, SET RET CODE = 04
         L     R1,IDC@REQ         GET REQUEST ADDRESS
         LDINT R3,0(,R1)          LOAD LENGTH
         L     R2,4(,R1)          LOAD TEXT POINTER
         LA    R2,0(,R2)          CLEAR HIGH
         STM   R2,R3,0(R5)        PLACE INTO IDCAMS LIST
         OI    EXFLAGS,EXFGET            X'FF' = A GET HAS BEEN ISSUED
         B     XIDCEXIT
         SPACE 1
XIDCGET4 LA    R15,4                     SET REG 15 = X'00000004'
         B     XIDCEXIT
         SPACE 1
XIDCPUT  TM    EXFLAGS,EXFSUPP+EXFSKIP  ANY FORM OF SUPPRESSION?
         BNZ   XIDCPUTZ           YES; DON'T BOTHER WITH REST
         LM    R4,R5,0(R5)
         LA    R4,1(,R4)          SKIP CARRIAGE CONTROL CHARACTER
         BCTR  R5,0               FIX LENGTH
         ICM   R5,8,=C' '         BLANK FILL
         LA    R14,XIDCTEXT
         LA    R15,L'XIDCTEXT
         MVCL  R14,R4
         TM    EXFLAGS,EXFMALL    PRINT ALL MESSAGES?
         BNZ   XIDCSHOW           YES; PUT THEM ALL OUT
         CLC   =C'IDCAMS ',XIDCTEXT    IDCAMS TITLE ?
         BE    XIDCEXIT           YES; SKIP
         CLC   XIDCTEXT+1(L'XIDCTEXT-1),XIDCTEXT   ALL BLANK OR SOME?
         BE    XIDCEXIT           YES; SKIP
*        CLC   DELETCMD(8),XIDCTEXT    OUR REQUEST?
*        BE    XIDCEXIT           YES; SKIP
         CLC   =C'IDC0002I',XIDCTEXT   AMS PGM END
         BE    XIDCEXIT           YES; SKIP
*PRINT   CLC   =C'IDC3012I',XIDCTEXT   DATASET NOT FOUND?
*ALL     BE    XIDCSKIP           YES; THAT'S JUST FINE AND DANDY
*RESULT  CLC   =C'IDC0550I',XIDCTEXT   DATASET DELETED?
*MSGS    BE    XIDCSKIP           YES; THAT'S JUST FINE AND DANDY
*XIDCSHOW WTO   MF=(E,AMSPRINT)    SHOW MESSAGE
XIDCSHOW DS    0H
XIDCPUTZ SR    R15,R15
         B     XIDCEXIT
XIDCSKIP OI    EXFLAGS,EXFSKIP    SKIP THIS AND REMAINING MESSAGES
         SR    R15,R15
         SPACE 2
***************************************************************
* IDCAMS ASYNC EXIT ROUTINE - EXIT, CONSTANTS & WORKAREAS
***************************************************************
         SPACE 1
XIDCEXIT L     R13,4(,R13)        GET CALLER'S SAVE AREA
         L     R14,12(,R13)
         RETURN (0,12)            RESTORE AND RETURN TO IDCAMS
         SPACE 1
IDCSAVE  DC    18F'0'             MAIN ROUTINE'S REG SAVEAREA
XIDSAVE  DC    18F'0'             ASYNC ROUTINE'S REG SAVEAREA
         SPACE 1
AMSPRINT DC    0A(0),AL2(4+L'XIDCTEXT,0)
XIDCTEXT DC    CL132' '
         SPACE 1
AMSPARM  DC    A(HALF00,HALF00,HALF00,X'80000000'+ADDRLIST)
         SPACE 1
ADDRLIST DC    F'2'
         DC    A(DDNAME01)
         DC    A(XIDCAMS)
IDC@REQ  DC    A(0)               ADDRESS OF REQUEST POINTER
         DC    A(DDNAME02)
         DC    A(XIDCAMS)
         DC    A(0)
         SPACE 1
HALF00   DC    H'0'
DDNAME01 DC    CL10'DDSYSIN   '
DDNAME02 DC    CL10'DDSYSPRINT'
         SPACE 1
EXFLAGS  DC    X'08'              EXIT PROCESSING FLAGS
EXFGET   EQU   X'01'                PRIOR GET WAS ISSUED
EXFNOM   EQU   X'04'                SUPPRESS ERROR WTOS
EXFRET   EQU   X'08'                NO ABEND; RETURN WITH COND.CODE
EXFMALL  EQU   X'10'                ALWAYS PRINT MESSAGES
EXFSUPP  EQU   X'20'                ALWAYS SUPPRESS MESSAGES
EXFSKIP  EQU   X'40'                SKIP SUBSEQUENT MESSAGES
EXFGLOB  EQU   EXFMALL+EXFSUPP+EXFRET  GLOBAL FLAGS
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**   CALL @@DYNAL,(ddn-len,ddn-adr,dsn-len,dsn-adr),VL               **
**                                                                   **
**   "-len" fields are self-defining values in the calling list,     **
**       or else pointers to 32-bit signed integer values            **
**                                                                   **
**   "ddn-adr"  is the address of the DD name to be used. When the   **
**       contents is hex zero or blank, and len=8, gets assigned.    **
**                                                                   **
**   "dsn-adr" is the address of a 1 to 44 byte data set name of an  **
**       existing file (sequential or partitioned).                  **
**                                                                   **
**   Calling @@DYNAL with a DDNAME and a zero length for the DSN     **
**   results in unallocation of that DD (and a PARM error).          **
**                                                                   **
*---------------------------------------------------------------------*
**                                                                   **
**    Author:  Gerhard Postpischil                                   **
**                                                                   **
**    This program is placed in the public domain.                   **
**                                                                   **
*---------------------------------------------------------------------*
**                                                                   **
**    Assembly: Any MVS or later assembler may be used.              **
**       Requires SYS1.MACLIB                                        **
**       Intended to work in any 24 and 31-bit environment.          **
**                                                                   **
**    Linker/Binder: RENT,REFR,REUS                                  **
**                                                                   **
*---------------------------------------------------------------------*
**    Return codes:  R15:04sssnnn   it's a program error code:       **
**    04804 - GETMAIN failed;  1400000n   PARM list error            **
**                                                                   **
**    Otherwise R15:0-1  the primary allocation return code, and     **
**      R15:2-3 the reason codes.                                    **
***********************************************************************
*  Maintenance:                                     new on 2008-06-07 *
*                                                                     *
***********************************************************************
         ENTRY @@DYNAL
@@DYNAL  B     DYNALBEG-*(,R15)      SKIP ID
         DC    AL1(9),C'@@SYSTEM &SYSDATE'
DYNALBEG STM   R14,R12,12(R13)    SAVE CALLER'S REGISTERS
         LR    R12,R15            ESTABLISH MY BASE
         USING @@DYNAL,R12        AND DECLARE IT
         LA    R11,16(,R13)       REMEMBER THE RETURN CODE ADDRESS
         MVC   0(4,R11),=X'04804000'  PRESET
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
         LA    R0,DYNALDLN        GET LENGTH OF SAVE AND WORK AREA
         GETMAIN RC,LV=(0)        GET STORAGE
         LTR   R15,R15            SUCCESSFUL ?
         BZ    DYNALHAV           YES
         STC   R15,3(,R11)        SET RETURN VALUES
         B     DYNALRET           RELOAD AND RETURN
         SPACE 1
*    CLEAR GOTTEN STORAGE AND ESTABLISH SAVE AREA
*
DYNALHAV ST    R1,8(,R13)         LINK OURS TO CALLER'S SAVE AREA
         ST    R13,4(,R1)         LINK CALLER'S TO OUR AREA
         LR    R13,R1
         USING DYNALWRK,R13
         MVC   0(4,R11),=X'14000001'  PRESET FOR PARM LIST ERROR
         MVC   DYNLIST(ALLDYNLN),PATLIST  INITIALIZE EVERYTHING
         LDINT R4,0(,R9)          DD NAME LENGTH
         L     R5,4(,R9)          -> DD NAME
         LDINT R6,8(,R9)          DSN LENGTH
         L     R7,12(,R9)         -> DATA SET NAME
         SPACE 1
*   NOTE THAT THE CALLER IS EITHER COMPILER CODE, OR A COMPILER
*   LIBRARY ROUTINE, SO WE DO MINIMAL VALIDITY CHECKING
*
         SPACE 1
*   PREPARE DYNAMIC ALLOCATION REQUEST LISTS
*
         LA    R0,ALLARB
         STCM  R0,7,ALLARBP+1     REQUEST POINTER
         LA    R0,ALLATXTP
         ST    R0,ALLARB+8        TEXT UNIT POINTER
         LA    R0,ALLAXDSN
         LA    R1,ALLAXDSP
         LA    R2,ALLAXDDN
         O     R2,=X'80000000'
         STM   R0,R2,ALLATXTP     TEXT UNIT ADDRESSES
         SPACE 1
*   COMPLETE REQUEST WITH CALLER'S DATA
*
         LTR   R4,R4              CHECK DDN LENGTH
         BNP   DYNALEXT           OOPS
         CH    R4,=AL2(L'ALLADDN)   REASONABLE SIZE ?
         BH    DYNALEXT           NO
         BCTR  R4,0
         EX    R4,DYNAXDDN        MOVE DD NAME
         OC    ALLADDN,=CL10' '   CONVERT HEX ZEROES TO BLANKS
         CLC   ALLADDN,=CL10' '   NAME SUPPLIED ?
         BNE   DYNALDDN           YES
         MVI   ALLAXDDN+1,DALRTDDN  REQUEST RETURN OF DD NAME
         CH    R4,=AL2(L'ALLADDN-1)   CORRECT SIZE FOR RETURN ?
         BE    DYNALNDD           AND LEAVE R5 NON-ZERO
         B     DYNALEXT           NO
         SPACE 1
DYNALDDN SR    R5,R5              SIGNAL NO FEEDBACK
*  WHEN USER SUPPLIES A DD NAME, DO AN UNCONDITIONAL UNALLOCATE ON IT
         LA    R0,ALLURB
         STCM  R0,7,ALLURBP+1     REQUEST POINTER
         LA    R0,ALLUTXTP
         ST    R0,ALLURB+8        TEXT UNIT POINTER
         LA    R2,ALLUXDDN
         O     R2,=X'80000000'
         ST    R2,ALLUTXTP        TEXT UNIT ADDRESS
         MVC   ALLUDDN,ALLADDN    SET DD NAME
         LA    R1,ALLURBP         POINT TO REQUEST BLOCK POINTER
         DYNALLOC ,               REQUEST ALLOCATION
DYNALNDD LTR   R6,R6              CHECK DSN LENGTH
         BNP   DYNALEXT           OOPS
         CH    R6,=AL2(L'ALLADSN)   REASONABLE SIZE ?
         BH    DYNALEXT           NO
         STH   R6,ALLADSN-2       SET LENGTH INTO TEXT UNIT
         BCTR  R6,0
         EX    R6,DYNAXDSN        MOVE DS NAME
         SPACE 1
*    ALLOCATE
         LA    R1,ALLARBP         POINT TO REQUEST BLOCK POINTER
         DYNALLOC ,               REQUEST ALLOCATION
         STH   R15,0(,R11)        PRIMARY RETURN CODE
         STH   R0,2(,R11)         REASON CODES
         LTR   R5,R5              NEED TO RETURN DDN ?
         BZ    DYNALEXT           NO
         MVC   0(8,R5),ALLADDN    RETURN NEW DDN, IF ANY
         B     DYNALEXT           AND RETURN
DYNAXDDN MVC   ALLADDN(0),0(R5)   COPY DD NAME
DYNAXDSN MVC   ALLADSN(0),0(R7)   COPY DATA SET NAME
         SPACE 1
*    PROGRAM EXIT, WITH APPROPRIATE RETURN CODES
*
DYNALEXT LR    R1,R13        COPY STORAGE ADDRESS
         L     R9,4(,R13)    GET CALLER'S SAVE AREA
         LA    R0,DYNALDLN   GET ORIGINAL LENGTH
         FREEMAIN R,A=(1),LV=(0)  AND RELEASE THE STORAGE
         L     R13,4(,R13)   RESTORE CALLER'S SAVE AREA
DYNALRET LM    R14,R12,12(R13) RESTORE REGISTERS; SET RETURN CODES
         BR    R14           RETURN TO CALLER
         SPACE 2
         LTORG ,
         PUSH  PRINT
         PRINT NOGEN         DON'T NEED TWO COPIES
PATLIST  DYNPAT P=PAT        EXPAND ALLOCATION DATA
         POP   PRINT
         SPACE 2
*    DYNAMICALLY ACQUIRED STORAGE
*
DYNALWRK DSECT ,             MAP STORAGE
         DS    18A           OUR OS SAVE AREA
         SPACE 1
DYNLIST  DYNPAT P=ALL        EXPAND ALLOCATION DATA
DYNALDLN EQU   *-DYNALWRK     LENGTH OF DYNAMIC STORAGE
         CSECT ,             RESTORE
*
*
*
* Keep this code last because it uses different base register
*
* S/370 doesn't support switching modes so this code is useless,
* and won't compile anyway because "BSM" is not known.
*
         AIF   ('&SYS' EQ 'S370').NOMODE  If S/370 we can't switch mode
         DROP  R12
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  SETM24 - Set AMODE to 24
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@SETM24
         USING @@SETM24,R15
@@SETM24 ICM   R14,8,=X'00'       Sure hope caller is below the line
         BSM   0,R14              Return in amode 24
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  SETM31 - Set AMODE to 31
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ENTRY @@SETM31
         USING @@SETM31,R15
@@SETM31 ICM   R14,8,=X'80'       Set to switch mode
         BSM   0,R14              Return in amode 31
         LTORG ,
*
.NOMODE  ANOP  ,                  S/370 doesn't support MODE switching
*
*
*
         IEZIOB                   Input/Output Block
*
WORKAREA DSECT
SAVEAREA DS    18F
WORKLEN  EQU   *-WORKAREA
*
         DCBD  DSORG=PS,DEVD=DA   Map Data Control Block
         ORG   IHADCB             Overlay the DCB DSECT
ZDCBAREA DS    0H
         DS    CL(INDCBLN)
         ORG   IHADCB             Only using one DCB
         DS    CL(OUTDCBLN)         so overlay this one
         ORG   ,
OPENCLOS DS    A                  OPEN/CLOSE parameter list
DCBXLST  DS    A
EOFR24   DS    CL(EOFRLEN)
* This is for when we are using move mode, and need to
* pass back extra information
BEGINDCB DS    A                  The beginning of this entire block
ASMBUF   DS    A                  Pointer to an area for PUTting data
*
         IHADECB DSECT=NO         Data Event Control Block
BLKSIZE  DS    F                  Save area for input DCB BLKSIZE
LRECL    DS    F                  Save area for input DCB LRECL
BUFFADDR DS    A                  Location of the BLOCK Buffer
BUFFEND  DS    A                  Address after end of current block
BUFFCURR DS    A                  Current record in the buffer
VBSADDR  DS    A                  Location of the VBS record build area
VBSEND   DS    A                  Addr. after end VBS record build area
JFCB     DS    0F
         IEFJFCBN LIST=YES        Job File Control Block
CAMLST   DS    XL(CAMLEN)         CAMLST for OBTAIN to get VTOC entry
* Format 1 Data Set Control Block
*   N.B. Current program logic does not use DS1DSNAM, leaving 44 bytes
*     of available space
         IECSDSL1 1               Map the Format 1 DSCB
DSCBCCHH DS    CL5                CCHHR of DSCB returned by OBTAIN
         DS    CL47               Rest of OBTAIN's 148 byte work area
         ORG   DS1DSNAM           Reuse unused field
DEVINFO  DS    2F                 UCB Type
MEMBER24 DS    CL8
RECFMIX  DS    X             Record format index: 0-F 4-V 8-U
IOFLAGS  DS    X             Remember prior events
IOFLEOF  EQU   1               Encountered an End-of-File
IOFLSDW  EQU   2               Spanned record incomplete
         ORG   ,
SAVEADCB DS    18F                Register save area for PUT
ZDCBLEN  EQU   *-ZDCBAREA
*
         SPACE 2
         PRINT NOGEN
         IHAPSA ,            MAP LOW STORAGE
         IKJTCB ,            MAP TASK CONTROL BLOCK
         IEFZB4D0 ,          MAP SVC 99 PARAMETER LIST
         IEFZB4D2 ,          MAP SVC 99 PARAMETERS
         END
