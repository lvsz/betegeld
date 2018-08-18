; -- gfx.asm --------------------------------------------------------
; Handles the graphical output to screen
; -------------------------------------------------------------------

;;;; compile-time constants
VMEMADR     equ offset _screenBuffer    ; change to 0A0000h to skip buffer
SCRWIDTH    equ 320                     ; screen width in pixels
SCRHEIGHT   equ 200                     ; screen height in pixels
TILESIZE    equ 20                      ; tile size in pixels
STDDELAY    equ 3                       ; standard delay in 1/10th seconds


; -------------------------------------------------------------------
codeseg

;;; set the video mode
proc setVideoMode
    arg     @@VM: byte
    uses    eax

    mov     ah, 00h
    mov     al, [@@VM]
    int     10h

    ret
endp setVideoMode


;;;; displays a given string on a given row & column
proc displayString
    arg @@string:dword, @@row:byte, @@col:byte
    uses eax, ebx, ecx, edx, edi

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


;;;; displays current score on screen
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


;;;; delay used for animations
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


;;;; uses a delay to animate changes
proc animateMoves

    call    drawGame
    call    delay

    ret
endp animateMoves


;;;; fill background with given color
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


;;;; draw a tile using given board coordinates & color
proc drawTile
    arg     @@x: word, @@y: word, @@color: byte
    uses    eax, ecx, edx, edi

    ; convert board coordinates to screen coordinates
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


;;;; draw cursor in given board coordinates
proc drawCursor
    arg     @@point: word               ; point = x:y
    uses    eax, ecx, edx, edi

    ; convert board coordinates to screen coordinates
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


;;;; updates board state before drawing
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


;;;; updates rest of game state before drawing
proc updateGame
    uses    eax

    movzx   eax, [word ptr _cursorPos]
    call    drawCursor, eax
    cmp     [byte ptr _moveMode], 1         ; switching mode, a tile is selected
    jne     @@nothing_selected
    movzx   eax, [word ptr _selectedTile]
    call    drawCursor, eax

    @@nothing_selected:
        ret
endp updateGame


;;;; draws everything on screen
proc drawGame
    uses    eax, ecx, edx, edi, esi

    call    updateBoard
    call    updateGame

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


; -------------------------------------------------------------------
dataseg

    _screenBuffer \
        db  (SCRWIDTH * SCRHEIGHT) dup (?)

    _delay \
        db STDDELAY

    _delayActivate \
        db  0

    _scoreText \
        db  "SCORE: ", '$'

    _gameOverString \
        db  "GAME OVER", '$'

; -------------------------------------------------------------------

