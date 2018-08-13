;=============================================================================
; 32-bit Assembler Random Number Generation.
;
; For use under DMPI 0.9 protected mode.
;
; The pseudo random generator is basically a Linear congruential generator, as
; explained on https://en.wikipedia.org/wiki/Linear_congruential_generator.
;
; This implementation assumes a modulo of 2^32.
;
; The constants are use from C99 and C11.
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
; Copyright (c) 2015, Tim Bruylants <tim.bruylants@gmail.com>
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from this
;    software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
;=============================================================================

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

INCLUDE "rand.inc"

RAND_A = 1103515245
RAND_C = 12345
;RAND_M = 2^32

;=============================================================================
; Uninitialized DATA
;=============================================================================
UDATASEG
    rand_seed   dd ?

;=============================================================================
; DATA
;=============================================================================
DATASEG

;=============================================================================
; CODE
;=============================================================================
CODESEG

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Initialize the random number generator.
;
; ARGUMENTS:
;   none
; RETURNS:
;   nothing
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PROC rand_init
    USES    eax, ecx, edx

    mov     ah, 02ch        ; Get system time
    int     21h

    mov     ax, dx          ; Use time to generate seed in EAX
    shl     eax, 16
    mov     ax, cx

    mov     ah, 02ah        ; Get system date
    int     21h

    shl     ecx, 16         ; Mangle date into the seed in EAX
    mov     cx, dx
    xor     eax, ecx
    
    mov     [rand_seed], eax

    ret
ENDP rand_init

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Returns the next (pseudo) random number in EAX.
;
; ARGUMENTS:
;   none
; RETURNS:
;   EAX     Random 32-bit value
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PROC rand
    USES    ecx, edx

    mov     eax, [rand_seed]
    mov     ecx, RAND_A
    mul     ecx
    add     eax, RAND_C
    mov     [rand_seed], eax

    ret
ENDP rand

END

