; -- main.asm -------------------------------------------------------
; Main file, deals with initialization & the bulk of game logic
; -------------------------------------------------------------------

ideal
P386
model FLAT, C
assume cs:_TEXT, ds:FLAT, es:FLAT, fs:FLAT, gs:FLAT

include "keyb.inc"
include "mouse.inc"
include "rand.inc"

include "input.asm"
include "gfx.asm"
include "colors.asm"

;;;; compile-time constants
NTILES      equ 7   ; number of types of tiles used
BRDWIDTH    equ 8   ; number of tiles that fit in a board's row
BRDHEIGHT   equ 8   ; number of tiles that fit in a board's column
BRDX0       equ ((SCRWIDTH - BRDWIDTH * TILESIZE) / 2)
BRDY0       equ (SCRHEIGHT - BRDHEIGHT * TILESIZE)


; -------------------------------------------------------------------
codeseg

;;;; call with a point (x:y),
;;;; returns absolute address of its position on the board in eax.
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


;;;; call with the point (x:y) of the selected tile
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


;;;; adds score for a match based on length and consecutiveness
;;;; score = (length - 1) * 50 * K, with K the Kth match in a single turn
proc addScore
    arg     @@length:dword  ; length of match
    uses    eax

    mov     eax, [dword ptr @@length]
    dec     eax
    mul     [dword ptr _scoreCoefficient]
    add     [dword ptr _score], eax
    add     [dword ptr _scoreCoefficient], 50

    ret
endp addScore


;;;; searches for rows with 3+ consecutive tiles of the same color
;;;; updates score dependent of length of match found
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


;;;; searches a column with 3+ consecutive tiles of the same color
;;;; updates score dependent of length of match found
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


;;;; calls matchOneColumn on all columns
proc matchColumns
    uses    ebx

    xor     ebx, ebx

    @@loop:
        call    matchOneColumn, ebx
        inc     ebx
        cmp     ebx, BRDWIDTH
        jl      @@loop

    ret
endp matchColumns


;;;; Removes found matches from the main board, refills empty spots
;;;; Stores total number of matches made in eax register
proc checkForMatches
    uses    ebx, ecx, esi, edi

    xor     eax, eax

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


;;;; transposes the board, used for checking potential matches
proc transposeBoard
    uses    eax, ebx, ecx, esi, edi

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


;;;; checks for patterns that allow a match with a single move
;;;; call with _board to check each row
;;;; call with _transposedBoard to check each column of _board
proc potentialMatchesHelper
    arg     @@board: dword
    uses    ebx, ecx, esi, edi

    mov     esi, [@@board]
    lea     edi, [esi + 1]
    mov     ebx, BRDHEIGHT
    mov     ecx, BRDWIDTH

    push    esi

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
        ; due to data layout, only horizontal bounds checking is required

        @@test1:
            cmp     al, [byte ptr esi + 1 + BRDWIDTH]   ; MM_
            jne     @@test2                             ; __M
            cmp     ecx, 1  ; ecx = 1 means we're in last column
            jne     @@true

        @@test2:
            cmp     al, [byte ptr esi + 1 - BRDWIDTH]   ; __M
            jne     @@test3                             ; MM_
            cmp     ecx, 1  ; ecx = 1 means we're in last column
            jne     short @@true

        @@test3:
            cmp     al, [byte ptr esi + 2]              ; MM_M
            jne     @@test4
            cmp     ecx, 2  ; ecx = 2 means we're in penultimate column
            jg      short @@true

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
        pop     esi
        add     esi, BRDWIDTH
        lea     edi, [esi + 1]
        push    esi
        mov     ecx, BRDWIDTH
        dec     ebx
        jnz     @@row_check_1


    ; empty stack
    pop     esi

    ; the next part checks for matches that can be made by
    ; swapping a tile in between 2 others
    mov     esi, [@@board]
    lea     edi, [esi + 2]
    mov     ebx, BRDHEIGHT
    mov     ecx, BRDWIDTH

    push    esi

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
        pop     esi
        add     esi, BRDWIDTH
        lea     edi, [esi + 2]
        push    esi
        dec     ebx
        mov     ecx, BRDWIDTH
        jnz     @@row_check_2

    ; empty stack
    pop     esi

    ; getting here means no match was found
    ; return 0 in al
    xor     al, al
    ret

    @@true:
        pop     esi ; empty stack
        mov     al, 1
        ret
endp potentialMatchesHelper


;;;; through the helper, it returns result in al
;;;; 1 if there's at least one potential match, 0 otherwise
;;;; 0 potential matches means game over
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


;;;; collapse tiles so there's no more empty space between them
;;;; also generates new tiles for places that remain empty
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
        call    randomTileColor
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


;;;; fills board with random tiles
;;;; should only be used for starting a new game
proc fillBoard
    uses    eax, ecx, edi

    call    disableAnimation

    mov     edi, offset _board
    mov     ecx, BRDWIDTH * BRDHEIGHT

    @@loop:
        call    randomTileColor     ; put a random color in al
        stosb                       ; store value of al into edi
        dec     ecx
        jnz     @@loop

    call    checkForMatches
    mov     [dword ptr _score], 0   ; reset any score from filling board

    call    enableAnimation

    ret
endp fillBoard


;;;; terminate the program.
proc terminateProcess
    uses    eax

    call    setVideoMode, 03h
    mov     ax, 04C00h
    int     21h

    ret
endp terminateProcess


;;;; the main function that gets everything going
proc main
    sti
    cld

    push    ds
    pop     es

    call    setVideoMode, 13h
    call    updateColorPalette
    call    mouse_install, offset mouseHandler
    call    rand_init

    @@random_game_over:

    call    fillBoard

    mov     [byte ptr _gameOver], 0
    call    potentialMatches
    cmp     [byte ptr _gameOver], 1
    je      @@random_game_over      ; no moves possible, redo

    @@main_loop:
        call    drawGame
        call    processUserInput    ; returns al > 0 for exit
        cmp     al, 0
        je      @@main_loop

    call    mouse_uninstall
    call    terminateProcess
endp main


; -------------------------------------------------------------------
dataseg

    ; set default cursor position in the middle
    _cursorPos \
        db  BRDWIDTH / 2 - 1
        db  BRDHEIGHT / 2 - 1

    _selectedTile \
        db  ?   ; x coordinate
        db  ?   ; y coordinate

    ; boardBuffers are used during potential match checking
    ; removing the need to do vertical bounds checking
    _boardBuffer1 \
        db  BRDWIDTH dup (0)

    _board \
        db  (BRDWIDTH * BRDHEIGHT) dup (?)

    _boardBuffer2 \
        db  BRDWIDTH dup (0)

    _transposedBoard \
        db  (BRDWIDTH * BRDHEIGHT) dup (8)

    _boardBuffer3 \
        db  BRDWIDTH dup (0)

    _matches \
        db  (BRDWIDTH * BRDHEIGHT) dup (0)

    _score \
        dd  0

    _scoreCoefficient \
        dd  50

    _scoreBuffer \
        db  20 dup (?)

    _gameOver \
        db  0

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
stack 1024h

end main
