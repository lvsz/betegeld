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
STDDELAY  equ 3         ; standard delay in 1/10th seconds

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


; returns a random color for a tile in eax
proc randomColor
    uses ebx, edx

    xor     edx, edx
    mov     ebx, TCOLORS
    call    rand
    div     ebx
    inc     edx
    mov     eax, edx
    ret
endp randomColor


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
    uses    eax

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
    uses    eax, ecx, edx, edi, esi

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

    call    displayScore

    cmp     [byte ptr _gameOver], 1
    je      @@game_over

    ret

    @@game_over:
        call    displayString, offset _gameOverString, 13, 15
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

    call    animateMoves

    push    eax
    call    checkForMatches     ; return 0 in eax if no match made
    cmp     eax, 0
    je      @@undo_swap         ; in case of no match, move is invalid
    pop     eax
    call    potentialMatches    ; check if there are still valid moves left
    ret

    @@undo_swap:
        pop     eax
        mov     [eax], dh   ; restore old value
        mov     [ebx], dl   ; restore old value
        mov     [byte ptr _delay], 0
        call    animateMoves
        mov     [byte ptr _delay], STDDELAY

    ret
endp swapTiles


proc animateMoves

    call    updateGame
    call    drawGame
    call    delay

    ret
endp animateMoves


proc delay
    uses    eax, ecx, edx

    cmp     [byte ptr _delayActivate], 0
    je      @@done

    mov     ah, 86h
    movzx   cx, [byte ptr _delay]
    xor     dx, dx
    int     15h

    @@done:
        ret
endp delay


proc mouseHandler
    uses    eax, ebx, ecx, edx

    ; check if mouse is in playing field
    movzx   eax, dx                 ; copy absolute Y position
    cmp     eax, BRDY0
    jl      @@notInField            ; skip if above field
    cmp     eax, BRDY0 + BRDHEIGHT * TILESIZE
    jge     @@notInField        ; skip if below field

    sar     cx, 1                   ; need to halve absolute X position
    cmp     cx, BRDX0
    jl      @@notInField        ; skip if left of field, short override because @@notInField is close enough.
    cmp     cx, BRDX0 + BRDWIDTH * TILESIZE
    jge     @@notInField        ; skip if right of field

    push    bx              ; save button state until after cursor move
                            ; can't save it before ^^ checks,
                            ; otherwise stack messes up when mouse goes out of field

    ; update _cursorPos with mouse coordinates
    sub     eax, BRDY0
    xor     edx, edx
    mov     ebx, TILESIZE
    div     ebx
    mov     [byte ptr _cursorPos + 1], al   ; saves boardCoordinate Y position
    mov     ax, cx
    sub     ax, BRDX0
    xor     edx, edx
    div     ebx
    mov     [byte ptr _cursorPos], al       ; saves boardCoordinate X position

    ; swapping or selecting
    cmp     [byte ptr _moveMode], 0
    jz      short @@handleClick ; if _moveMode = 0 (selecting) then click will select

    ; swappingTile:         ; a tile has been selected, _moveMode = 1 (swapping)
    mov     ax, [word ptr _cursorPos]
    mov     bx, [word ptr _selectedTile]
    sub     al, bl          ; relative x coordinate, with bx being the centre now
    sub     ah, bh          ; relative y coordinate

    push    ax              ; save relative coordinates

    ; signX
    cmp     al, 0           ; is x positive
    jge     @@signY
    neg     al              ; absolute value when x is negative

    @@signY:
        cmp     ah, 0
        jge     @@findMax
        neg     ah              ; absolute value

    @@findMax:
        pop     cx              ; get back original values
        cmp     al, ah          ; find biggest coordinate
        jge     @@largestX

        ; largestY
        mov     bh, 1           ;
        shr     ch, 7           ; find sign of y
        sub     bh, ch
        sub     bh, ch          ; subtract sign from 1 twice, if positive, 1-0-0=1, negative, 1-1-1=-1
        mov     cx, [word ptr _selectedTile]    ; get base tile
        add     ch, bh                          ; add up or down to it
		call	cursorBoundsCheck, cx
		cmp		al, 0
		jz		@@handleClick
        mov     [word ptr _cursorPos], cx       ; put it in cursorPos
        jmp     @@handleClick

    @@largestX:
        mov     bl, 1           ;
        shr     cl, 7           ; find sign of x
        sub     bl, cl
        sub     bl, cl          ; subtract sign from 1 twice, if positive, 1-0-0=1, negative, 1-1-1=-1
        mov     cx, [word ptr _selectedTile]    ; get base tile
        add     cl, bl                          ; add right or left to it
		call	cursorBoundsCheck, cx
		cmp		al, 0
		jz		@@handleClick
        mov     [word ptr _cursorPos], cx       ; put it in cursorPos
        jmp     @@handleClick

    @@handleClick:
        pop     bx
        cmp     bl, 1                   ; left-click?
        jl      @@mouseHandled          ; 0, no click
        jg      @@deselect              ; 1+, so deselect
        cmp     [byte ptr _moveMode], 0 ; swap or select?
        je      @@selectTile
        call    swapTiles, [word ptr _selectedTile] ; 1, so swap

    @@deselect:
        mov     [byte ptr _moveMode], 0 ; deselect tile
        jmp     @@mouseHandled

    @@selectTile:
        call    selectTile
        mov     [byte ptr _moveMode], 1 ; set _moveMode to swapping mode
        jmp     @@mouseHandled

    @@mouseHandled:                     ; since mouseHandler is via an interrupt
        call    updateGame              ; we need to update the game here too
        call    drawGame

    @@notInField:
        ret
endp mouseHandler


; select the current cursor position
proc selectTile
    uses    eax
    mov     ax, [word ptr _cursorPos]
    mov     [word ptr _selectedTile], ax
    ret
endp selectTile


proc cursorBoundsCheck
	arg     @@newCursorPos: word
    uses    edx
	
	xor		al, al			; return value
	mov		dx, [word ptr @@newCursorPos]
	
	cmp     dl, -1          ; check if move would be left of board
    jne     $+6             ; if not, jump 6 bytes (next check)
    inc     dl
    jmp     @@outOfBounds
    
    cmp     dl, BRDWIDTH    ; check if move would be right of board
    jne     $+6             ; if not, jump 6 bytes (next check)
    dec     dl
    jmp     @@outOfBounds
    
    cmp     dh, -1          ; check if move would be above board
    jne     $+6             ; if not, jump 6 bytes (next check)
    inc     dh
    jmp     @@outOfBounds
    
    cmp     dh, BRDHEIGHT	; check if move would be below board
    jne     @@finish_move   ; if not, jump to finish_move
    dec     dh
	
	@@outOfBounds:
	ret
	
	@@finish_move:
	mov		al, 1
	ret
endp cursorBoundsCheck


proc processUserInput
    uses    ebx, edx

    xor     edx, edx    ; emptying for later
    xor     ebx, ebx    ; emptying for later

    xor     eax, eax    ; == mov ah, 0
    int     16h         ; keyboard interrupt

    cmp     ah, 01h     ; ESC scan code
    je      @@done + 2  ; jump to @@done, skip "xor al, al" part

    ; cmp     ah, 036h  ; RSHIFT scan code
    ; je        @@deselect

    @@continue_game:
        cmp     ah, 02Eh
        je      @@change_color
        cmp     ah, 039h                ; SPACE scan code
        jne     @@maybe_move_cursor
        cmp     [byte ptr _moveMode], 1 ; if _moveMode = 1 (switching) then space swaps
        jne     @@selecting_tile        ; if _moveMode = 0 (selecting) then space selects
        call    swapTiles, [word ptr _selectedTile]
        mov     [byte ptr _moveMode], 0 ; set _moveMode to selecting mode
        jmp     @@done

    @@change_color:
        call    changeColorPalette
        jmp     short @@done

    @@deselect:
        mov     [byte ptr _moveMode], 0 ; deselect
        jmp     short @@done

    @@selecting_tile:                   ; select the current cursor position
        call    selectTile
        mov     [byte ptr _moveMode], 1 ; set _moveMode to swapping mode

    @@maybe_move_cursor:
        cmp     ah, 4bh         ; check if key is LEFT
        je      @@move_cursor
        cmp     ah, 4dh         ; check if key is RIGHT
        je      @@move_cursor
        cmp     ah, 48h         ; check if key is UP
        je      @@move_cursor
        cmp     ah, 50h         ; check if key is DOWN
        jne     @@done

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
        jmp     @@done

    @@limited_move:
        mov     dx, [word ptr offset _selectedTile]
        add     dl, bl
        add     dh, bh
		
		call	cursorBoundsCheck, dx
		cmp		al, 0
		jz		@@done
		mov     [word ptr offset _cursorPos], dx

    @@done:
        xor     al, al  ; clean up eax before returning
        ret
endp processUserInput


; adds score for a match based on length and consecutiveness
; score = (length - 1) * 50 * K, with K the Kth match in a single turn
proc addScore
    arg @@length:dword  ; length of match
    uses eax

    mov     eax, [dword ptr @@length]
    dec     eax
    mul     [dword ptr _scoreCoefficient]
    add     [dword ptr _score], eax
    add     [dword ptr _scoreCoefficient], 50

    ret
endp addScore


; searches for rows with 3+ consecutive tiles of the same color
; updates score dependent of length of match found
proc matchRows
    uses    ebx, ecx, edi, esi

    mov     esi, offset _board
    mov     edi, offset _board + 1
    mov     ebx, BRDWIDTH   ; ebx is #tiles remaining in row before matching
    mov     ecx, BRDWIDTH   ; ecx is #tiles remaining in row after matching

    @@loop:
        repe    cmpsb       ; repeat while tile is equal to previous tile
                            ; changes values of ecx, esi & edi
        sub     ebx, ecx    ; result is length of match found
        cmp     ebx, 3      ; match needs to be length 3 or more
        jge     @@match

        cmp     ecx, 3      ; ecx is number of remaining tiles in row
        jl      @@next_row  ; if ecx < 3, no match possible, so go to next row
        mov     ebx, ecx    ; else update ebx to ecx to continue search
        jmp     @@loop

        @@match:
            call    addScore, ebx
            push    esi         ; save esi
            push    edi         ; save edi
            dec     esi         ; move esi to last matching tile
            xchg    ecx, ebx    ; save ecx in ebx & put #tiles remaining in ecx

            ; set edi to equivalent position in _matches array
            lea     edi, [offset _matches + esi - offset _board]

            std             ; set direction flag to move over string backwards
            rep     movsb   ; copies the matching tiles from _board to _matches
            cld             ; clear direction flag

            ; restore values to continue loop
            mov     ecx, ebx
            pop     edi
            pop     esi
            jmp     @@loop

        @@next_row:
            add     esi, ecx        ; add #tiles remaining to current
            add     edi, ecx        ; position to move to next row
            cmp     esi, offset _board + (BRDWIDTH * BRDHEIGHT)
            jge     @@done          ; done if current position is out of bounds
            mov     ebx, BRDWIDTH   ; else reset ecx & ebx and continue loop
            mov     ecx, ebx
            jmp     @@loop

        @@done:
            ret
endp matchRows


; searches a column with 3+ consecutive tiles of the same color
; updates score dependent of length of match found
proc matchOneColumn
    arg     @@col: dword
    uses    ecx, ebx, edi, esi

    xor     ebx, ebx
    mov     esi, offset _board
    add     esi, [@@col]            ; set esi to correct column
    lea     edi, [esi + BRDWIDTH]   ; set edi to tile below esi
    mov     ecx, BRDHEIGHT          ; use ecx as counter

    @@loop:
        cmpsb   ; derefs & compares esi & ebi, then adds 1 to both pointers
        lea     esi, [esi + BRDWIDTH - 1]   ; -1 because cmpsb already did +1
        lea     edi, [edi + BRDWIDTH - 1]
        jne     @@check_for_match
        inc     ebx
        dec     ecx
        jnz     @@loop

    @@check_for_match:
        cmp     ebx, 2
        jl      @@no_match
        ; process match
        push    esi
        push    edi
        inc     ebx
        call    addScore, ebx
        @@copy:
            lea     esi, [esi - BRDWIDTH]
            lea     edi, [offset _matches + esi - offset _board]
            mov     dl, [esi]
            mov     [edi], dl   ; copy matched tile to _matches
            dec     ebx
            jnz     @@copy
        pop     edi
        pop     esi

    @@no_match:
        xor     ebx, ebx
        cmp     ecx, 3      ; check how many left to compare
        jl      @@done      ; if fewer than 3, no more matches possible
        dec     ecx
        jmp     @@loop

    @@done:
        ret
endp matchOneColumn


; calls matchOneColumn on all columns
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


; Removes found matches from the main board, refills empty spots
; Stores total number of matches made in eax register
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
            cmpsb           ; compare tile from _board & _matches
            jne     @@skip  ; skip when not equal
            inc     eax     ; increase total matches made counter
            inc     ebx     ; increase current matches made counter
            mov     [byte ptr edi - 1], 0       ; empty _board tile
            @@skip:
                mov     [byte ptr esi - 1], 0   ; reset tile on _matches
                dec     ecx
                jnz     @@loop

        cmp     ebx, 0
        je      @@done          ; done if no matches found
        call    collapseTiles   ; else collapse & refill tiles
        jmp     @@cascade       ; and repeat

    @@done:
        mov     [dword ptr _scoreCoefficient], 50   ; reset coefficient
        ret
endp checkForMatches


; transposes the board, used for checking potential matches
proc transposeBoard
    uses eax, ebx, ecx, esi, edi

    mov     esi, offset _board
    mov     edi, offset _transposedBoard
    mov     ecx, BRDWIDTH / 4

    @@loop:
        push    edi ; save edi to undo changes while filling a column
        @@fill_column:
            lodsd   ; load 4 tiles at once in eax
            mov     [byte ptr edi + 0 * BRDHEIGHT], al
            mov     [byte ptr edi + 1 * BRDHEIGHT], ah
            shr     eax, 16 ; bitshift 16 bits to access the other half of eax
            mov     [byte ptr edi + 2 * BRDHEIGHT], al
            mov     [byte ptr edi + 3 * BRDHEIGHT], ah
            add     edi, BRDHEIGHT * 4
            dec     ecx
            jnz     @@fill_column
        mov     ecx, BRDWIDTH / 4
        pop     edi
        inc     edi ; increase edi to fill out the next column
        cmp     edi, offset _transposedBoard + BRDHEIGHT
        jl      @@loop

    ret
endp transposeBoard


; checks for patterns that allow a match with a single move
; call with _board to check each row
; call with _transposedBoard to check each column of _board
proc potentialMatchesHelper
    arg @@board:dword
    uses ebx, ecx, esi, edi

    mov     esi, [@@board]
    lea     edi, [esi + 1]
    mov     ebx, BRDHEIGHT
    mov     ecx, BRDWIDTH

    ; searches for 2 matching adjacent tiles
    ; then see if there's a 3rd tile that can be moved to create a match
    ; only need one, so exit once found
    @@row_check_1:
        repne   cmpsb   ; repeat while [esi] & [edi] are not equal
        cmp     ecx, 0  ; 0 means end of row, go to next
        je      @@row_next_1

        mov     al, [byte ptr esi]

        ; each test checks if there's a 3rd tile in the data that could
        ; be used to form a match
        ; when true, do a bounds check to make sure the move doesn't have
        ; to cross any borders to form the match

        @@test1:
        cmp     al, [byte ptr esi + 1 + BRDWIDTH]   ; MM_
        jne     @@test2                             ; __M
        cmp     ecx, 1  ; ecx = 1 means we're in last column
        jne     short @@true

        @@test2:
        cmp     al, [byte ptr esi + 1 - BRDWIDTH]   ; __M
        jne     @@test3                             ; MM_
        cmp     ecx, 1  ; ecx = 1 means we're in last column
        jne     short @@true

        @@test3:
        cmp     al, [byte ptr esi + 2]              ; MM_M
        jne     @@test4
        cmp     ecx, 2  ; ecx = 2 means we're in penultimate column
        jg      @@true

        @@test4:
        cmp     ecx, BRDWIDTH - 1   ; check if we're in 1st column
        je      @@row_check_1       ; if true, possibilities are exhausted

        cmp     al, [byte ptr esi - 2 + BRDWIDTH]   ; _MM
        je      @@true                              ; M__

        @@test5:
        cmp     al, [byte ptr esi - 2 - BRDWIDTH]   ; M__
        je      @@true                              ; _MM

        @@test6:
        cmp     al, [byte ptr esi - 3]               ; M_MM
        jne     @@row_check_1
        cmp     ecx, BRDWIDTH - 2   ; check if we're in 2nd column
        jne     @@true


    @@row_next_1:
        mov     ecx, BRDWIDTH
        dec     ebx
        jnz     @@row_check_1


    ; the next part checks for matches that can be made by
    ; swapping a tile in between 2 others
    mov     esi, [@@board]
    lea     edi, [esi + 2]
    mov     ebx, BRDHEIGHT
    mov     ecx, BRDWIDTH

    @@row_check_2:
        repne   cmpsb
        cmp     ecx, 0
        je      @@row_next_2

        mov     al, [byte ptr esi - 1]

        cmp     al, [byte ptr esi + BRDWIDTH]   ; M_M
        je      @@true                          ; _M_

        cmp     al, [byte ptr esi - BRDWIDTH]   ; _M_
        je      @@true                          ; M_M
        jmp     @@row_check_2

    @@row_next_2:
        dec     ebx
        mov     ecx, BRDWIDTH
        jnz     @@row_check_2

    ; getting here means no match was found
    ; return 0 in al
    xor     al, al
    ret

    @@true:
        mov     al, 1
        ret

endp potentialMatchesHelper


; through the helper, it returns result in al
; 1 if there's at least one potential match, 0 otherwise
; 0 potential matches means game over
proc potentialMatches
    call    potentialMatchesHelper, offset _board
    cmp     al, 1
    je      @@done  ; if first call was successful, we can exit
    call    transposeBoard
    call    potentialMatchesHelper, offset _transposedBoard
    cmp     al, 1
    je      @@done
    mov     [byte ptr _gameOver], 1

    @@done:
        ret
endp potentialMatches


; collapse tiles so there's no more empty space between them
; also generates new tiles for places that remain empty
proc collapseTiles
    uses    eax, ebx, esi

    call    animateMoves

    mov     esi, offset _board + BRDWIDTH * BRDHEIGHT - 1
    mov     edi, esi

    @@loop:
        cmp     [byte ptr esi], 0
        je      @@find_tile
        @@found_tile:
            dec     esi
            cmp     esi, offset _board
            jge     @@loop
            jmp     @@done

    @@find_tile:
        mov     ebx, esi
        sub     ebx, BRDWIDTH
        cmp     ebx, offset _board
        jl      @@insert_random_tiles   ; no more tiles, insert new
        cmp     [byte ptr ebx], 0
        je      @@find_tile + 2         ; jump back, but skip 1st instruction
        mov     al, [byte ptr ebx]
        mov     [byte ptr esi], al
        mov     [byte ptr ebx], 0
        jmp     @@found_tile

    ; random tiles are only needed when the top of the column is empty
    ; this section immediately fills the entire column
    @@insert_random_tiles:
        add     ebx, BRDWIDTH
        call    randomColor
        mov     [byte ptr ebx], al
        cmp     ebx, esi
        jge     @@found_tile            ; entire column has been filled
        jmp     @@insert_random_tiles

    @@done:
        call    animateMoves
        mov     [byte ptr _delay], 0
        call    animateMoves
        mov     [byte ptr _delay], STDDELAY
        ret
endp collapseTiles


; fills board with random tiles
proc fillBoard
    uses eax, ebx, ecx, edi

    mov     edi, offset _board
    mov     ecx, BRDWIDTH * BRDHEIGHT
    mov     ebx, TCOLORS

    @@loop:
        call    randomColor ; put a random color in al
        stosb               ; store value of al into edi
        dec     ecx
        jnz     @@loop

    ret
endp fillBoard


proc displayString
    arg @@string:dword, @@row:byte, @@col:byte
    uses eax, ebx, ecx, edx, edi

    ;mov     dx, 0201h   ; set cursor position to row 2 & column 1
    mov     dh, [@@row]
    mov     dl, [@@col]
    xor     bx, bx
    mov     ah, 2h      ; function to set cursor position
    int     10h         ; video mode interrupt

    mov     edx, [@@string] ; gets the string string
    mov     ah, 9h          ; function to display string
    int     21h             ; displays string

    ret
endp displayString


; displays current score on screen
proc displayScore
    uses eax, ebx, ecx, edx, edi

    mov     dx, 0101h   ; set cursor position to row 1 & column 1
    xor     bx, bx
    mov     ah, 2h      ; function to set cursor position
    int     10h         ; video mode interrupt

    mov     edx, offset _scoreText  ; gets the "SCORE: " string
    mov     ah, 9h                  ; function to display string
    int     21h                     ; displays string

    mov     eax, [dword ptr _score]
    mov     ebx, 10
    xor     ecx, ecx

    ; integers gets processed from lowest to highest significant figure
    ; so put on stack to display eventual string correctly later
    @@put_digits_on_stack:
        xor     edx, edx
        div     ebx         ; divide eax by ebx, with remainder stored in edx
        add     edx, '0'    ; turn digit into char by adding char code of '0'
        push    edx         ; put on stack for later
        inc     ecx
        cmp     eax, 0
        jne     @@put_digits_on_stack

    mov     edi, offset _scoreBuffer    ; set destination to buffer

    @@pop_digits_from_stack:
        pop     eax
        stosb                           ; store value of eax in buffer
        dec     ecx
        jnz     @@pop_digits_from_stack

    mov     [dword ptr edi], '$'        ; mark end of string
    mov     edx, offset _scoreBuffer
    mov     ah, 9h                      ; function to display string
    int     21h

    ret
endp displayScore


; Terminate the program.
proc terminateProcess
    uses eax

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
    call    updateColorPalette
    call    mouse_install, offset mouseHandler

    @@random_game_over:

    call    fillBoard
    call    checkForMatches

    mov     [byte ptr _gameOver], 0
    call    potentialMatches
    cmp     [byte ptr _gameOver], 1
    je      @@random_game_over              ; no moves possible, redo

    mov     [dword ptr _score], 0           ; reset score from filling board
    mov     [byte ptr _delayActivate], 1    ; activate delay

    @@main_loop:
        call    updateGame
        call    drawGame
        call    processUserInput    ; returns al > 0 for exit
        cmp     al, 0
        je      @@main_loop

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

    _delay \
        db STDDELAY

    _delayActivate \
        db  0

    _seconds \
        db ?

    _rowBuffer1 \
        db  BRDWIDTH dup (0)

    _board \
        db  (BRDWIDTH * BRDHEIGHT) dup (?)
        ;db  1,1,2,2,1,1,2,2
        ;db  1,1,2,2,1,1,2,2
        ;db  3,3,4,4,3,3,4,4
        ;db  3,3,4,4,3,3,4,4
        ;db  1,1,2,2,5,1,2,2
        ;db  1,1,2,2,5,1,2,2
        ;db  3,3,4,5,3,3,4,4
        ;db  3,3,4,4,3,3,4,4

    _rowBuffer2 \
        db  BRDWIDTH dup (0)

    _transposedBoard \
        db  (BRDWIDTH * BRDHEIGHT) dup (8)

    _rowBuffer3 \
        db  BRDWIDTH dup (0)

    _matches \
        db  (BRDWIDTH * BRDHEIGHT) dup (0)

    _score \
        dd  0

    _scoreCoefficient \
        dd  50

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
        db  0

    _scoreText \
        db  "SCORE: ", '$'

    _scoreBuffer \
        db  20 dup (?)

    _gameOver \
        db  0

    _gameOverString \
        db  "GAME OVER", '$'

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
        db  010h, 009h, 00Dh    ; 0 black
        db  0FFh, 0C3h, 0E2h    ; 1
        db  08Bh, 0FEh, 0A8h    ; 2
        db  0C7h, 09Bh, 0F2h    ; 3
        db  0FFh, 0FFh, 0E3h    ; 4
        db  0FFh, 0A8h, 0A8h    ; 5
        db  0A4h, 0F0h, 0B7h    ; 6
        db  0EEh, 0DDh, 0BDh    ; 7
        db  0FFh, 0FFh, 0FFh    ; 8

    _palette_3 \
        db  000h, 000h, 055h    ; 0 black
        db  0FFh, 0C3h, 0FFh    ; 1
        db  08Bh, 0FEh, 0FFh    ; 2
        db  0C7h, 09Bh, 0FFh    ; 3
        db  0FFh, 0FFh, 0EEh    ; 4
        db  0FFh, 0A8h, 0FFh    ; 5
        db  0A4h, 0F0h, 0FFh    ; 6
        db  0EEh, 0DDh, 0FFh    ; 7
        db  0FFh, 0FFh, 0FFh    ; 8

    _endPalette \
        dd  ($ - offset _palette_1)

    _currentPalette \
        dd  offset _palette_1

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
stack 2000h

end main
