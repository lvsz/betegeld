ideal
P386
model FLAT, C
assume cs:_TEXT, ds:FLAT, es:FLAT, fs:FLAT, gs:FLAT

include "keyb.inc"
include "mouse.inc"
include "rand.inc"

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
TCOLORS   equ 7         ; number of colors used for tile
; colors in palette used
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
    uses    ebx, ecx, edx

    call    fillBackground, BLACK

    xor     ebx, ebx
    mov     edx, offset _board
    @@outer_loop:
        xor     ecx, ecx
        @@inner_loop:
            call    drawTile, ecx, ebx, [word ptr edx]
            inc     ecx
            inc     edx
            cmp     ecx, BRDWIDTH
            jl      @@inner_loop
        inc     ebx
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
    cmp     [byte ptr _moveMode], 1         ; switching mode, a tile is selected
    jne     @@nothing_selected
    movzx   eax, [word ptr _selectedTile]
    call    drawCursor, eax

    @@nothing_selected:
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
        in      al, dx              ; read status
        and     al, 01000b          ; test bit 3
        jz      @@waitVBlank_wait2

    mov     esi, VMEMADR
    mov     edi, 0A0000h            ; video memory address
    mov     ecx, SCRWIDTH * SCRHEIGHT / 4
    rep     movsd

    ret
endp drawGame


;Call with a point (x:y),
;returns absolute address of its position on the board in eax.
proc getBoardPosition
    arg     @@point: word
    uses    edx

    movzx   eax, [byte ptr @@point + 1]     ; gets point's y position
    mov     edx, BRDWIDTH                   ; switch to one-dimensional view
    mul     edx
    add     al, [byte ptr @@point]          ; adds point's x position
    add     eax, offset _board              ; adds board's offset
    ret
endp getBoardPosition


;Call with the point (x:y) of the selected tile
proc swapTiles
    arg     @@selectedTile: word
    uses    eax, ebx, edx

    call    getBoardPosition, [@@selectedTile]
    mov     ebx, eax                                ; remember its position
    mov     dl, [eax]                               ; and its value
    call    getBoardPosition, [word ptr _cursorPos]
    mov     dh, [eax]                               ; and the other value
    mov     [eax], dl                               ; swap
    mov     [ebx], dh
    ;call    animateMoves

    push    eax
    call    checkForMatches ; return 0 in eax if no match made
    cmp     eax, 0
    je      @@undo_swap     ; in case of no match, move is invalid
    pop     eax
    ret

    @@undo_swap:
        pop     eax
        mov     [eax], dh   ; restore old value
        mov     [ebx], dl   ; restore old value
        ;call    animateMoves
        ret
endp swapTiles

proc animateMoves
    uses    eax, ecx, edx

    xor     eax, eax
    xor     ecx, ecx
    xor     edx, edx

    call    updateGame
    call    drawGame

    ; delay for a second
    mov     cx, 0Fh
    mov     dx, 4240h
    mov     ah, 86h
    int     15h

    ret

endp animateMoves

proc printInt
    arg     @@int:dword
    uses    eax, ebx, edx

    xor     edx, edx
    mov     ebx, 10
    mov     eax, [@@int]
    push    eax

    @@loop:

        div     ebx
        push    edx
        cmp     eax, 0
        jne     @@loop

    @@loop2:
        pop     edx

        mov     ah, 2h
        add     edx, 48
        int     21h

        sub     edx, 48
        cmp     edx, [@@int]
        jne     @@loop2

    ret
endp printInt


proc mouseHandler
    uses    eax, ebx, ecx, edx

    movzx   eax, dx                 ; copy absolute Y position
    cmp     eax, BRDY0
    jl      @@notInField            ; skip if above field
    cmp     eax, BRDY0 + BRDHEIGHT * TILESIZE
    jge     short @@notInField    ; skip if below field

    sar     cx, 1           ; need to halve absolute X position
    cmp     cx, BRDX0
    jl      short @@notInField    ; skip if left of field
    cmp     cx, BRDX0 + BRDWIDTH * TILESIZE
    jge     short @@notInField    ; skip if right of field

    push    bx              ; save button state until after cursor move
                            ; can't save it before ^^ checks,
                            ; otherwise stack messes up when mouse goes out of bounds

    push    bx                      ; save button state until after cursor move
                                    ; can't save it before checks,
                                    ; otherwise stack messes up when mouse goes out of bounds
    sub     eax, BRDY0
    xor     edx, edx
    mov     ebx, TILESIZE
    div     ebx
    mov     [byte ptr _cursorPos + 1], al   ; saves relative Y position
    mov     ax, cx
    sub     ax, BRDX0
    xor     edx, edx
    div     ebx
    mov     [byte ptr _cursorPos], al       ; saves relative X position

    pop     bx
    cmp     bl, 1                   ; left-click?
    jl      @@noClick               ; 0
    je      @@switchOrSelectTile    ; 1, so left-click
    mov     [byte ptr _moveMode], 0 ; else, right/scrollclick => deselect
    jmp     @@noClick

    @@switchOrSelectTile:
        cmp     [byte ptr _moveMode], 1
        jnz     @@select
        call    swapTiles, [word ptr _selectedTile]
        mov     [byte ptr _moveMode], 0
        jmp     @@noClick

    @@select:
        call    selectTile
        mov     [byte ptr _moveMode], 1

    @@noClick:
        call    updateGame
        call    drawGame


    @@notInField:
        ret
ENDP mouseHandler


proc selectTile
; select the current cursor position
    uses    eax
    mov     ax, [word ptr offset _cursorPos]
    mov     [word ptr _selectedTile], ax
    ret
endp selectTile

proc processUserInput
    uses    ebx, edx

    xor     edx, edx    ; emptying for later
    xor     ebx, ebx    ; emptying for later

    xor     eax, eax    ; == mov ah, 0
    int     16h         ; keyboard interrupt

    cmp     ah, 01h     ; ESC scan code
    je      @@done

    @@continue_game:
        cmp     ah, 039h                ; SPACE scan code
        jnz     @@move_cursor
        cmp     [byte ptr _moveMode], 1 ; if _moveMode = 1 (switching) then space swaps
        jne     @@selecting_tile        ; if _moveMode = 0 (selecting) then space selects
        call    swapTiles, [word ptr _selectedTile]
        mov     [byte ptr _moveMode], 0 ; set _moveMode to selecting mode
        jmp     @@move_cursor

    @@selecting_tile:               ; select the current cursor position
        call    selectTile
        mov     [byte ptr _moveMode], 1 ; set _moveMode to swapping mode

    @@move_cursor:
        ; check for cursor movements
        movzx   eax, ah                             ; only interested in ah part of eax
        mov     bx, [word ptr _moves + eax + eax]   ; stored as words so eax added twice
        cmp     [byte ptr _moveMode], 1             ; go to limited options in move mode
        je      @@limited_move
        mov     dx, [word ptr offset _cursorPos]    ; get current position
        add     dl, bl                              ; use as an efficient modulo
        add     dh, bh                              ; use as an efficient modulo
        and     dx, 0707h
        mov     [word ptr offset _cursorPos], dx
        xor     al, al                              ; clean up eax before returning
        jmp     @@done

    @@limited_move:
        mov     dx, [word ptr offset _selectedTile]
        add     dl, bl
        add     dh, bh

        cmp     dl, -1          ; check if move would be left of board
        jne     $+6             ; if not, jump 6 bytes (next check)
        inc     dl
        jmp     @@finish_move

        cmp     dl, 8           ; check if move would be right of board
        jne     $+6             ; if not, jump 6 bytes (next check)
        dec     dl
        jmp     @@finish_move

        cmp     dh, -1          ; check if move would be above board
        jne     $+6             ; if not, jump 6 bytes (next check)
        inc     dh
        jmp     @@finish_move

        cmp     dh, 8           ; check if move would be below board
        jne     @@finish_move   ; if not, jump to finish_move
        dec     dh

        @@finish_move:
            mov     [word ptr offset _cursorPos], dx
            xor     al, al      ; clean up eax before returning

    @@done:
        ret
endp processUserInput


proc matchRows
    uses    ecx, edx, edi, esi

    mov     esi, offset _board
    mov     edi, offset _board + 1
    mov     edx, BRDWIDTH   ; edx is #tiles remaining in row before matching
    mov     ecx, edx        ; ecx is #tiles remaining in row after matching

    @@loop:
        repe    cmpsb       ; repeat while tile is equal to previous tile
                            ; changes values of ecx, esi & edi
        sub     edx, ecx    ; result is length of match found
        cmp     edx, 3      ; match needs to be length 3 or more
        jge     @@match

        cmp     ecx, 3      ; ecx is number of remaining tiles in row
        jl      @@next_row  ; if ecx < 3, no match possible, so go to next row
        mov     edx, ecx    ; else update edx to ecx to continue search
        jmp     @@loop

        @@match:
            push    esi         ; save esi
            push    edi         ; save edi
            dec     esi         ; move esi to last matching tile
            xchg    ecx, edx    ; save ecx in edx & put #tiles remaining in ecx

            ; set edi to equivalent position in _matches array
            lea     edi, [offset _matches + esi - offset _board]

            std             ; set direction flag to move over string backwards
            rep     movsb   ; copies the matching tiles from _board to _matches
            cld             ; clear direction flag

            ; restore values to continue loop
            mov     ecx, edx
            pop     edi
            pop     esi
            jmp     @@loop

        @@next_row:
            add     esi, ecx        ; add #tiles remaining to current
            add     edi, ecx        ; position to move to next row
            cmp     esi, offset _board + (BRDWIDTH * BRDHEIGHT)
            jge     @@done          ; done if current position is out of bounds
            mov     edx, BRDWIDTH   ; else reset ecx & edx and continue loop
            mov     ecx, edx
            jmp     @@loop

        @@done:
            ret
endp matchRows


proc matchOneColumn
    arg     @@n: dword
    uses    ebx, ecx, edi, esi

    xor     ebx, ebx
    mov     esi, offset _board
    add     esi, [@@n]              ; set esi to correct column
    lea     edi, [esi + BRDWIDTH]   ; set edi to tile below esi
    mov     ecx, BRDHEIGHT          ; use ecx as counter

    @@loop:
        cmpsb   ; derefs & compares esi & ebi, then adds 1 to both pointers
        lea     esi, [esi + BRDWIDTH - 1]   ; -1 because cmpsb already did +1
        lea     edi, [edi + BRDWIDTH - 1]
        jne     @@no_match
        inc     ebx
        dec     ecx
        jnz     @@loop
        @@no_match:
            cmp     ebx, 2
            jge     @@process_match ; enough tiles matched
            @@process_match_return:
                xor     ebx, ebx
                cmp     ecx, 3      ; check how many left to compare
                jl      @@done      ; if fewer than 3, no more matches possible
                dec     ecx
                jmp     @@loop

    @@process_match:
        push    esi
        push    edi
        inc     ebx
        @@copy:
            lea     esi, [esi - BRDWIDTH]
            lea     edi, [offset _matches + esi - offset _board]
            mov     dl, [esi]
            mov     [edi], dl   ; copy matched tile to _matches
            dec     ebx
            jnz     @@copy
        pop     edi
        pop     esi
        jmp     @@process_match_return

    @@done:
        ret
endp matchOneColumn


proc matchColumns
    uses ebx

    xor     ebx, ebx

    @@loop:
        call    matchOneColumn, ebx
        inc     ebx
        cmp     ebx, BRDWIDTH
        jl      @@loop
    ret
endp matchColumns


; Removes found matches from the main board refils empty spots
proc checkForMatches
    uses ebx, ecx, esi, edi

    xor eax, eax

    ; cascade for matching newly dropped tiles
    @@cascade:
        call    matchRows
        call    matchColumns

        xor     ebx, ebx
        mov     esi, offset _matches
        mov     edi, offset _board
        mov     ecx, BRDWIDTH * BRDHEIGHT

        @@loop:
            cmpsb
            jne     @@skip
            inc     eax
            inc     ebx
            mov     [byte ptr edi - 1], 0
            @@skip:
                mov     [byte ptr esi - 1], 0   ; reset tile on _matches
                dec     ecx
                jnz     @@loop

        cmp     ebx, 0
        je      @@done          ; done if no matches found
        call    collapseTiles   ; else collapse & refill tiles
        jmp     @@cascade       ; and repeat

    @@done:
        ret
endp checkForMatches

; collapse tiles so there's no more empty space between them
proc collapseTiles
    uses    eax, ebx, ecx

    ;call    animateMoves

    mov     ecx, offset _board + BRDWIDTH * BRDHEIGHT

    @@loop:
        dec     ecx
        cmp     ecx, offset _board
        jl      @@done
        cmp     [byte ptr ecx], 0
        jne     @@loop
        lea     ebx, [ecx - BRDWIDTH]
        @@find_tile_above:
            cmp     [byte ptr ebx], 0
            jne     @@collapse
            lea     ebx, [ebx - BRDWIDTH]
            jmp     @@find_tile_above
        @@collapse:
            mov     ah, [byte ptr ebx]
            mov     [byte ptr ecx], ah
            mov     [byte ptr ebx], 0
            jmp     @@loop

    @@done:
        ;call   animateMoves
        call    refillDrops
        ;call   animateMoves
        ret
endp collapseTiles


proc refillDrops
    uses eax, ebx, ecx, edx

    mov     ebx, TCOLORS
    mov     ecx, offset _drops + BRDWIDTH * BRDHEIGHT

    @@loop:
        dec     ecx
        cmp     ecx, offset _drops
        jl      @@done
        cmp     [byte ptr ecx], 0
        jne     @@loop
        call    rand
        xor     edx, edx
        div     ebx                 ; use div to get remainder of eax / 7
        inc     dl                  ; tile colors are between 1 & 7
        mov     [byte ptr ecx], dl  ; move value to empty tile
        jmp     @@loop

    @@done:
        ret
endp refillDrops


; fills both visible _board & invisible _drops
proc fillBoard
    uses eax, ebx, ecx, edx, edi

    mov     edi, offset _drops
    mov     ecx, 2 * BRDWIDTH * BRDHEIGHT
    mov     ebx, TCOLORS

    @@loop:
        xor     edx, edx
        call    rand
        div     ebx     ; divide to get remainder
        inc     dl      ; tile colors are between 1 & 7
        mov     al, dl  ; move remainder to al
        stosb           ; store value of al into edi
        dec     ecx
        jnz     @@loop

    ret
endp fillBoard


; Terminate the program.
proc terminateProcess
    uses    eax

    call    setVideoMode, 03h
    mov     ax, 04C00h
    int     21h

    ret
endp terminateProcess


proc waitForSpecificKeystroke
    arg     @@key:byte
    uses    eax

    @@waitForKeystroke:
        mov     ah, 00h
        int     16h
        cmp     al, [@@key]
        jne     @@waitForKeystroke

    ret
endp waitForSpecificKeystroke


proc main
    sti
    cld

    push    ds
    pop     es

    call rand_init

    call mouse_present
    cmp eax, 1
    je @@mouse_present

    mov ah, 9
    mov edx, offset msg_no_mouse
    int 21h

    @@mouse_present:

    call    setVideoMode, 13h
    call    updateColourPalette
    call    mouse_install, offset mouseHandler

    call    fillBoard
    call    checkForMatches

    @@main_loop:
        call    updateGame
        call    drawGame
        call    processUserInput    ; returns al > 0 for exit
        cmp     al, 0
        jz      @@main_loop


    call    mouse_uninstall
    call    terminateProcess
endp main


; -------------------------------------------------------------------
dataseg
    _screenBuffer \
        db  (SCRWIDTH * SCRHEIGHT) dup (?)

    _test \
        db 1
        db 1
    msg_no_mouse \
        db 'Hij komt hier wel é', 0dh, 0ah, '$'

    _cursorPos \
        db  BRDWIDTH / 2 - 1
        db  BRDHEIGHT / 2 - 1

    _selectedTile \
        db  0
        db  0

    _drops \
        db  (BRDWIDTH * BRDHEIGHT) dup (?)

    _board \
        db  (BRDWIDTH * BRDHEIGHT) dup (?)
        ;db   1,  1,  2,  2,  1,  1,  2,  2  ; row 0
        ;db   1,  1,  2,  2,  1,  1,  2,  2  ; row 1
        ;db   4,  3,  1,  1,  3,  3,  1,  1  ; row 2
        ;db   4,  4,  1,  1,  4,  4,  1,  1  ; row 3
        ;db   1,  6,  5,  5,  6,  6,  5,  5  ; row 4
        ;db   6,  6,  5,  5,  6,  6,  5,  5  ; row 5
        ;db   7,  7,  6,  6,  7,  7,  6,  6  ; row 6
        ;db   1,  2,  3,  4,  5,  6,  7,  1  ; row 7

    _matches \
        db  (BRDWIDTH * BRDHEIGHT) dup (0)

    _score \
        dd  0

    _palette \
        db  000h, 000h, 000h    ; 0 black
        db  0FFh, 000h, 000h    ; 1 red
        db  000h, 0FFh, 000h    ; 2 green
        db  000h, 000h, 0FFh    ; 3 blue
        db  0FFh, 0FFh, 000h    ; 4 yellow
        db  0FFh, 0A5h, 000h    ; 5 orange
        db  000h, 0FFh, 0FFh    ; 6 cyan
        db  0FFh, 0D9h, 0BFh    ; 7 pink
        db  0FFh, 0FFh, 0FFh    ; 8 white

    ; indices based on keyboard scan codes
    _moves \
        dw  72 dup (?)
        dw  0ff00h      ; move up
        dw  2 dup (?)
        dw  000ffh      ; move left
        dw  (?)
        dw  00001h      ; move right
        dw  2 dup (?)
        dw  00100h      ; move down

    _moveMode \
        db 0

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
stack 2000h

end main
