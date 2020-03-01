; osstart.asm - startup code for C programs
; 
; This program written by Paul Edwards
; Released to the public domain
; w/mods by Matthew Parker

; This program is meant to be used as a stub for your C programs
; It is designed for OS/2

.386p

.model flat

;.stack 4000h

extrn __start:near, DosExit:near

_DATA   segment dword public use32 'DATA'
ifdef LINK386_FIX
        org 20000h
endif        
banner  db  "PDPCLIB"
_DATA   ends
_BSS    segment dword public use32 'BSS'
_BSS    ends
_STACK  segment dword stack use32 'STACK'
        db 4000h dup(?)
_STACK  ends

DGROUP  group   _DATA,_BSS
        assume cs:_TEXT,ds:DGROUP

_TEXT segment 'CODE'

top:

public __main
__intstart proc 
mov eax, [esp+16]
push eax
call __start
sub sp,4
push eax
call __exita
__intstart endp

public __exita
__exita proc
pop eax
pop eax
push eax
push 1
call DosExit
sub sp,8
__exita endp

__main proc
ret
__main endp

_TEXT ends
          
end top
