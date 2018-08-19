; -- colors.asm -----------------------------------------------------
; This file handles everything color-related
; -------------------------------------------------------------------

;;;; total number of colors used
NCOLORS   equ 9
;;;; colors in default palette
BLACK     equ 0
RED       equ 1
GREEN     equ 2
BLUE      equ 3
YELLOW    equ 4
ORANGE    equ 5
CYAN      equ 6
PINK      equ 7
WHITE     equ 8


; -------------------------------------------------------------------
codeseg

;;;; returns a random color for a tile in eax
proc randomTileColor
    uses ebx, edx

    xor     edx, edx
    mov     ebx, NTILES
    call    rand
    div     ebx
    inc     edx
    mov     eax, edx
    ret
endp randomTileColor


;;;; installs the _currentPalette
proc updateColorPalette
    uses    eax, ecx, edx, esi

    mov     esi, [dword ptr _currentPalette]    ; pointer to source palette
    mov     ecx, NCOLORS                        ; amount of colours to read

    ; multiply ecx by 3 (3 color components per color)
    ; do it efficiently (2*ecx + ecx)
    mov     eax, ecx
    sal     eax, 1
    add     ecx, eax

    mov     dx, 03c8h
    xor     al, al
    out     dx, al

    inc     dx
    rep     outsb

    ret
endp updateColorPalette


;;;; switches to the next palette
;;;; more palettes can be added in the data segment
proc changeColorPalette
    uses    eax, ebx, edx, edx

    mov     eax, [dword ptr _currentPalette]
    sub     eax, offset _palette_1
    add     eax, 3 * NCOLORS
    mov     ebx, [dword ptr _endPalette]
    xor     edx, edx
    div     ebx
    add     edx, offset _palette_1
    mov     [dword ptr _currentPalette], edx
    call    updateColorPalette

    ret
endp changeColorPalette


; -------------------------------------------------------------------
dataseg

    _palette_1 \
        db  000h, 000h, 000h    ; 0 black
        db  0FFh, 000h, 000h    ; 1 red
        db  000h, 0FFh, 000h    ; 2 green
        db  000h, 000h, 0FFh    ; 3 blue
        db  0FFh, 0FFh, 000h    ; 4 yellow
        db  0FFh, 0A5h, 000h    ; 5 orange
        db  000h, 0FFh, 0FFh    ; 6 cyan
        db  0FFh, 0D9h, 0BFh    ; 7 pink
        db  0FFh, 0FFh, 0FFh    ; 8 white

    _palette_2 \
        db  010h, 009h, 00Dh    ; 0
        db  0FFh, 0C3h, 0E2h    ; 1
        db  08Bh, 0FEh, 0A8h    ; 2
        db  0C7h, 09Bh, 0F2h    ; 3
        db  0FFh, 0FFh, 0E3h    ; 4
        db  0FFh, 0A8h, 0A8h    ; 5
        db  0A4h, 0F0h, 0B7h    ; 6
        db  0EEh, 0DDh, 0BDh    ; 7
        db  0FFh, 0FFh, 0FFh    ; 8

    _palette_3 \
        db  000h, 000h, 055h    ; 0
        db  0FFh, 0C3h, 0FFh    ; 1
        db  08Bh, 0FEh, 0FFh    ; 2
        db  0C7h, 09Bh, 0FFh    ; 3
        db  0FFh, 0FFh, 0EEh    ; 4
        db  0FFh, 0A8h, 0FFh    ; 5
        db  0A4h, 0F0h, 0FFh    ; 6
        db  0EEh, 0DDh, 0FFh    ; 7
        db  0FFh, 0FFh, 0FFh    ; 8

    ; more 9-color palettes can be inserted here
    ; the above code can handle an arbitrary number of palettes
    ; as long as they're located between _palette_1 and _endPalette

    _endPalette \
        dd  ($ - offset _palette_1)

    _currentPalette \
        dd  offset _palette_1

; -------------------------------------------------------------------

