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
    mov     edx, offset _board
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

;Call with a point (x:y), 
;returns absolute address of its position on the board in eax.
proc getBoardPosition
	arg		@@point: word
	uses	edx
	
	movzx   eax, [byte ptr @@point + 1]		; gets point's y position
    mov     edx, BRDWIDTH					; switch to one-dimensional view
    mul     edx
    add     al, [byte ptr @@point]			; adds point's x position
	add		eax, offset _board				; adds board's offset
	ret
endp getBoardPosition

;Call with the point (x:y) of the selected tile
proc switchTiles
	arg		@@selectedTile: word
	uses	eax, ebx, edx
	
	call	getBoardPosition, [@@selectedTile]
	mov		ebx, eax								; remember its position
	mov		dl, [eax]								; and its value
	call	getBoardPosition, [word ptr _cursorPos]
	mov 	dh, [eax]								; and the other value
	mov		[eax], dl								; swap
	mov		[ebx], dh
	
	
	ret
endp switchTiles

proc printInt
	arg		@@int:dword
	uses	eax, edx
	
	xor 	edx, edx
	mov		eax, [@@int]
	push 	eax
	
@@loop:
			
	div		[dword 10]
	push	edx
	cmp		eax, 0
	jne		@@loop

@@loop2:
	pop		edx
	
	mov     ah, 2h
    add     edx, 48
    int     21h
	
	sub 	edx, 48
	cmp 	edx, [@@int]
	jne		@@loop2

	ret
endp printInt


PROC mouseHandler
    uses	eax, ebx, ecx, edx
	
	;Ik wil hier de absolute muiscoördinaten omzetten naar cursorPosities,
	;en zo de cursor updaten wanneer de muis beweegt.
	;Ik gebruik de BRDX0 en BRDY0 als offsets om het veld te vinden in het scherm.
	;Alles buiten het veld is voorlopig niet nuttig.
	
	;Dit komt van hun code, dient om te checken welke knop(pen) ingedrukt zijn.
	;and	bl, 3							; check for two mouse buttons (2 low end bits)
	;jz		@@skipit						; only execute if a mousebutton is pressed
		
@@inField:			
	
	;Hier zou ik willen die offset van de muiscoördinaat halen, en tergelijk
	;checken of hij dan in het veld ligt, door te kijken of sub onder nul gaat of overflowt.
	
	; sub		cx, BRDX0					; overflows if too small
	; jo		@@notInField				; is signed if not in field
	; cmp		cx, BRDWIDTH * TILESIZE			
	; jg		@@notInField
	; call 	printInt, edx
	
	;Momenteel gebruik ik een simpele compare.
	cmp 	dx, BRDY0						; Is mouseY lower than BRDY0
	jl		@@notInField					; If so, don't bother
		
	sub		dx, BRDY0						; Switch absolute mouseY to relative y
	
	cmp		cx, BRDX0						; Same for mouseX, with the addition
	jl		@@notInField					; of checking for the right side margin
	cmp		cx, SCRWIDTH - BRDX0			
	jg		@@notInField
	sub		cx, BRDX0
	
	movzx	eax, cx							; Change absolute x position to tilesized positions
	div		[byte TILESIZE]					; By dividing the width (amount of pixels) by the width of a tile
	movzx	ecx, al							; put it in ecx for safekeeping
			
    movzx	eax, dx							; Do the same for y
	div		[byte TILESIZE]					; 
	mov		ch, al							; Put it in the other part of cx
	mov		[word ptr _cursorPos], cx		; Update _cursorPos with cx
	
	;Probleem lijkt mij de mapping van pixelcoördinaten naar tegelcoördinaten,
	;Maar ik zie al de hele namiddag niet wat ik verkeerd doe.
	
	;Hier call ik deze dingen zodat de cursor live geüpdated wordt.
	call 	updateGame
	call 	drawGame
	ret
	
@@notInField:
    ret
ENDP mouseHandler


proc processUserInput
    uses    ebx, edx

    xor     edx, edx    ; emptying for later
    xor     ebx, ebx    ; emptying for later

    xor     eax, eax    ; == mov ah, 0
    int     16h         ; keyboard interrupt

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
        and     ax, 0707h
        mov     [word ptr offset _cursorPos], ax

    xor al, al
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


proc findMatch
    call matchRows
    ret
endp findMatch


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
	call	mouse_install, offset mouseHandler

    @@main_loop:
        call    findMatch
        call    updateGame
        call    drawGame
        call    processUserInput    ; returns al > 0 for exit
        cmp     al, 0
        jz      @@main_loop
		
	

	call	mouse_uninstall
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

    _board \
        db   2,  2,  2,  4, 32, 64, 64, 64  ; row 0
        db  64, 64, 32, 16, 16, 32,  8, 32  ; row 1
        db   4,  4,  4,  2, 32,  2,  2,  2  ; row 2
        db   2,  2,  8,  4,  4,  1,  8,  2  ; row 3
        db  64,  8,  4, 16, 16, 16, 16, 16  ; row 4
        db   8,  4, 64, 64, 64, 64,  4,  4  ; row 5
        db   4,  4,  4,  4, 32, 16,  8,  1  ; row 6
        db  64, 64, 64, 64, 64, 64,  1,  8  ; row 7

    _matches \
        db  (BRDWIDTH * BRDHEIGHT) dup (0)

    _score \
        dd  0

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
