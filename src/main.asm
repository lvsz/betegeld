ideal
P386
model FLAT, C
assume cs:_TEXT, ds:FLAT, es:FLAT, fs:FLAT, gs:FLAT

include "keyb.inc"
include "mouse.inc"

; compile-time constants
VMEMADR   equ offset _screenBuffer  ; change to 0A0000h to skip buffer
SCRWIDTH  equ 320       ; screen width
SCRHEIGHT equ 200       ; screen height
TILESIZE  equ 20        ; tile size
BRDWIDTH  equ 8         ; number of tiles that fit in a board's row
BRDHEIGHT equ 8         ; number of tiles that fit in a board's column
BRDX0     equ ((SCRWIDTH - BRDWIDTH * TILESIZE) / 2)
BRDY0     equ (SCRHEIGHT - BRDHEIGHT * TILESIZE)

NCOLORS   equ 9         ; number of colors used
; colors in palette used
RED       equ 0
GREEN     equ 1
BLUE      equ 2
YELLOW    equ 3
ORANGE    equ 4
PURPLE    equ 5
PINK      equ 6
BLACK     equ 7
WHITE     equ 8

; -------------------------------------------------------------------
codeseg

; set the video mode
proc setVideoMode
    arg     @@VM: byte
    uses    eax

    mov     ah, 00h
    mov     al, [@@VM]
    int     10h

    ret
endp setVideoMode


proc updateColourPalette
    uses    eax, ecx, edx, esi

    mov     esi, offset _palette ; pointer to source palette
    mov     ecx, NCOLORS   ; amount of colours to read

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
endp updateColourPalette


proc fillBackground
    arg     @@color: byte
    uses    eax, ecx, edi

    mov     edi, VMEMADR
    mov     ecx, SCRWIDTH * SCRHEIGHT

    mov     al, [@@color]
    mov     [edi], al

    rep     stosb

    ret
endp fillBackground


proc drawTile
    arg     @@x: word, @@y: word, @@color: byte
    uses    eax, ecx, edx, edi

    movzx   eax, [@@y]
    mov     edx, SCRWIDTH
    mul     edx
    add     ax, [@@x]
    mov     edx, TILESIZE
    mul     edx
    add     eax, (BRDY0 + 1) * SCRWIDTH + BRDX0 + 1

    mov     edi, VMEMADR
    add     edi, eax

    mov     edx, TILESIZE - 2
    mov     al, [@@color]

    @@tile_loop:
        ; plot horizontal line
        mov     ecx, TILESIZE - 2
        rep     stosb
        ; move down one line
        add     edi, SCRWIDTH - TILESIZE + 2
        ; keep looping to draw tile line by line
        dec     edx
        cmp     edx, 0
        jne     @@tile_loop
    ret
endp drawTile


proc updateBoard
    uses    eax, ebx, ecx, edx

    call    fillBackground, BLACK

    xor     ebx, ebx
    mov     edx, offset _board + BRDWIDTH + 3 ; skip top 0-border
    @@outer_loop:
        xor     ecx, ecx
        @@inner_loop:
            bsf     eax, [edx]  ; finds first set bit to determine tile color
            call    drawTile, ecx, ebx, ax
            inc     ecx
            inc     edx
            cmp     ecx, BRDWIDTH
            jl      @@inner_loop
        inc     ebx
        add     edx, 2
        cmp     ebx, BRDHEIGHT
        jl      @@outer_loop
    ret
endp updateBoard


proc drawCursor
    arg     @@point: word               ; point = x:y
    uses    eax, ecx, edx, edi

    movzx   eax, [byte ptr @@point + 1] ; gets cursor y position
    mov     edx, SCRWIDTH
    mul     edx
    add     al, [byte ptr @@point]      ; adds cursor x position
    mov     edx, TILESIZE
    mul     edx
    add     eax, BRDY0 * SCRWIDTH + BRDX0

    mov     edi, VMEMADR
    add     edi, eax

    mov     edx, TILESIZE
    mov     al, WHITE

    ; plot the top edge
    mov     ecx, TILESIZE
    rep     stosb
    sub     edi, TILESIZE               ; reset edi to top left corner

    ; plot both vertical edges
    mov ecx, TILESIZE
    @@vert_loop:
        mov     [edi], al
        mov     [edi + TILESIZE - 1], al
        add     edi, SCRWIDTH
        loop    @@vert_loop

    ; edi should point at bottom left corner now
    sub     edi, SCRWIDTH

    ; plot bottom edge
    mov     ecx, TILESIZE
    rep     stosb
    ret
endp drawCursor


proc updateGame
    uses    eax, ebx
    call    updateBoard
    movzx   eax, [word ptr _cursorPos]
    call    drawCursor, eax
    ret
endp updateGame


proc drawGame
    uses    ecx, edx, edi, esi

    mov     dx, 03DAh               ; VGA status port

    @@waitVBlank_wait1:
        in      al, dx              ; read status
        and     al, 01000b          ; test bit 3
        jnz     @@waitVBlank_wait1

    @@waitVBlank_wait2:
        in  al, dx                  ; read status
        and al, 01000b              ; test bit 3
        jz  @@waitVBlank_wait2

    mov     esi, VMEMADR
    mov     edi, 0A0000h            ; video memory address
    mov     ecx, SCRWIDTH * SCRHEIGHT / 4
    rep     movsd

    ret
endp drawGame


proc processUserInput
    uses    ebx, edx

    xor     edx, edx    ; emptying for later
    xor     ebx, ebx    ; emptying for later

    xor     eax, eax    ; == mov ah, 0
    int     16h         ; keyboard interupt

    cmp     ah, 01h     ; ESC scan code
    jnz     @@continue_game_1
    ret

    @@continue_game_1:
        cmp     ah, 039h    ; SPACE scan code
        jnz     @@continue_game_2
        mov     [byte ptr _moveMode], 1

    @@continue_game_2:
        ; check for cursor movements
        movzx   edx, ah
        mov     ax, [word ptr offset _cursorPos]
        add     ax, [word ptr _moves + edx + edx]
        or      ax, 1111100011111000b
        xor     ax, 1111100011111000b
        mov     [word ptr offset _cursorPos], ax

    xor al, al
    ret
endp processUserInput


; Terminate the program.
proc terminateProcess
    uses    eax
    call    setVideoMode, 03h
    mov     ax, 04C00h
    int     21h
    ret
endp terminateProcess


proc main
    sti
    cld

    push    ds
    pop     es

    call    setVideoMode, 13h
    call    updateColourPalette

    @@main_loop:
        call    updateGame
        call    drawGame
        call    processUserInput    ; returns al > 0 for exit
        cmp     al, 0
        jz      @@main_loop

    call    terminateProcess
endp main

; -------------------------------------------------------------------
dataseg
    _screenBuffer \
        db  (SCRWIDTH * SCRHEIGHT) dup (?)

    _cursorPos \
        db  BRDWIDTH / 2 - 1
        db  BRDHEIGHT / 2 - 1

    ; active tiles are all powers of two
    ; this allows efficient match checking with bitwise-and
    ; 0-borders unnecessitates bounds checking
    _board \
        db  0,  0,  0,  0,  0,  0,  0,  0,  0, 0    ; row 0
        db  0,  2,  2,  4,  4, 16,  4,  1,  4, 0    ; row 1
        db  0, 64, 32, 16,  1, 16, 32,  8, 32, 0    ; row 2
        db  0,  2,  4,  1,  2, 32, 16,  1, 64, 0    ; row 3
        db  0,  4,  2, 32,  4,  2,  1,  8,  2, 0    ; row 4
        db  0, 64,  8,  4, 16, 16,  4, 32,  4, 0    ; row 5
        db  0,  8,  4, 64, 32, 64, 64, 16,  1, 0    ; row 6
        db  0,  4,  4,  1,  8, 32, 16,  8,  1, 0    ; row 7
        db  0, 64, 64,  4, 16, 16, 64,  1, 64, 0    ; row 8
        db  0,  0,  0,  0,  0,  0,  0,  0,  0, 0    ; row 9

    _palette \
        db  0FFh, 000h, 000h    ; 00 red
        db  000h, 0FFh, 000h    ; 01 green
        db  000h, 000h, 0FFh    ; 02 blue
        db  0FFh, 0FFh, 000h    ; 03 yellow
        db  0FFh, 0A5h, 000h    ; 04 orange
        db  000h, 0FFh, 0FFh    ; 05 purple
        db  0FFh, 0DFh, 0EEh    ; 06 pink
        db  000h, 000h, 000h    ; 07 black
        db  0FFh, 0FFh, 0FFh    ; 08 white

    ; indices based on keyboard scan codes
    _moves \
        dw  72 dup (?)
        dw  0ff00h      ; move up
        dw  2 dup (?)
        dw  0ffffh      ; move left
        dw  (?)
        dw  00001h      ; move right
        dw  2 dup (?)
        dw  00100h      ; move down

    _moveMode \
        db 0

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
stack 100h

end main
