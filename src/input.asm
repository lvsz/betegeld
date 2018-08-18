; -- input.asm ------------------------------------------------------
; Handles all of the player's input
; -------------------------------------------------------------------

;;;; keyboard scan codes uses
ESCAPE  equ 001h
SPACE   equ 039h
C_KEY   equ 02Eh
LEFT    equ 04Bh
RIGHT   equ 04Dh
UP      equ 048h
DOWN    equ 050h


; -------------------------------------------------------------------
codeseg

;;;; process input from the keyboard
proc processUserInput
    uses    ebx, edx

    xor     edx, edx    ; emptying for later
    xor     ebx, ebx    ; emptying for later

    xor     eax, eax    ; == mov ah, 0
    int     16h         ; keyboard interrupt

    cmp     ah, ESCAPE  ; ESC scan code
    je      @@done + 2  ; jump to @@done, skip "xor al, al" part

    @@continue_game:
        ; check keyboard scan codes & jump to correct block
        cmp     ah, C_KEY       ; C scan code
        je      @@change_color
        cmp     ah, SPACE       ; SPACE scan code
        je      short @@select  ; if none of above, assume arrow key
        cmp     ah, LEFT        ; check if key is LEFT
        je      @@move_cursor
        cmp     ah, RIGHT       ; check if key is RIGHT
        je      @@move_cursor
        cmp     ah, UP          ; check if key is UP
        je      @@move_cursor
        cmp     ah, DOWN        ; check if key is DOWN
        jne     @@done          ; no valid key, so done
        ; getting here means DOWN was pressed, so continue to move cursor
    @@move_cursor:
        ; check for cursor movements
        movzx   eax, ah                             ; only interested in ah part of eax
        mov     bx, [word ptr _moves + eax + eax]   ; stored as words so eax added twice
        cmp     [byte ptr _moveMode], 1             ; go to limited options in swap mode
        je      @@limited_move
        mov     dx, [word ptr offset _cursorPos]    ; get current position
        add     dl, bl
        add     dh, bh
        and     dx, 0707h                           ; used as "modulo" to teleport cursor
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
        ; limited_move means select mode, so continue
    @@select:
        cmp     [byte ptr _moveMode], 1 ; if _moveMode = 1 (swapping) then space swaps
        jne     @@selecting_tile        ; if _moveMode = 0 (selecting) then space selects
        call    swapTiles, [word ptr _selectedTile]
        mov     [byte ptr _moveMode], 0 ; set _moveMode to selecting mode
        jmp     @@done

    @@selecting_tile:                   ; select the current cursor position
        call    selectTile
        mov     [byte ptr _moveMode], 1 ; set _moveMode to swapping mode
        jmp     @@done

    @@change_color:
        call    changeColorPalette
        jmp     @@done

    @@done:
        xor     al, al  ; clean up eax before returning
        ret
endp processUserInput


;;;; handles everything mouse-related
proc mouseHandler
    uses    eax, ebx, ecx, edx

    ; check if mouse is in playing field
    movzx   eax, dx         ; copy absolute Y position
    cmp     eax, BRDY0
    jl      @@notInField    ; skip if above field
    cmp     eax, BRDY0 + BRDHEIGHT * TILESIZE
    jge     @@notInField    ; skip if below field

    sar     cx, 1                   ; need to halve absolute X position
    cmp     cx, BRDX0
    jl      @@notInField    ; skip if left of field, short override because @@notInField is close enough.
    cmp     cx, BRDX0 + BRDWIDTH * TILESIZE
    jge     @@notInField    ; skip if right of field

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
        neg     ah          ; absolute value

    @@findMax:
        pop     cx          ; get back original values
        cmp     al, ah      ; find biggest coordinate
        jge     @@largestX

        ; largestY
        mov     bh, 1
        shr     ch, 7       ; find sign of y
        sub     bh, ch
        sub     bh, ch      ; subtract sign from 1 twice, if positive, 1-0-0=1, negative, 1-1-1=-1
        mov     cx, [word ptr _selectedTile]    ; get base tile
        add     ch, bh                          ; add up or down to it
		call	cursorBoundsCheck, cx
		cmp		al, 0
		jz		@@handleClick
        mov     [word ptr _cursorPos], cx       ; put it in cursorPos
        jmp     @@handleClick

    @@largestX:
        mov     bl, 1
        shr     cl, 7       ; find sign of x
        sub     bl, cl
        sub     bl, cl      ; subtract sign from 1 twice, if positive, 1-0-0=1, negative, 1-1-1=-1
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
        call    drawGame

    @@notInField:
        ret
endp mouseHandler


;;;; select the current cursor position
proc selectTile
    uses    eax
    mov     ax, [word ptr _cursorPos]
    mov     [word ptr _selectedTile], ax
    ret
endp selectTile


;;;; check if a potential cursor move would place it out of bounds
;;;; sets al to 1 if move is valid, 0 otherwise
proc cursorBoundsCheck
	arg     @@newCursorPos: word
    uses    edx

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
        xor		al, al  ; return 0 (invalid)
        ret

	@@finish_move:
        mov		al, 1   ; return 1 (valid)
        ret
endp cursorBoundsCheck


; -------------------------------------------------------------------
dataseg
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

; -------------------------------------------------------------------

