;------------------------------------------------------------------------------
; File:         bio.inc
; Description:  Subroutines for BIOS I/O
; Author:       Christofer Odén
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
sprint:
; Prints the zero terminated string at ds:si
; Destroys: AL, SI

  lodsb                 ; Load the char at ds:si into al and increase si
  cmp   al, 0           ; Check if the char is null
  je    sprintdone      ; If so, return
  call  cprint          ; Print a single char
  jmp   sprint          ; Loop
sprintdone:
  add   byte [ypos], 1  ; Put the cursor on next line to the left
  mov   byte [xpos], 0

  ret                   ; Return
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
cprint:
; Prints the char in AL to [ES:DI + 160 * y + x * 2]
; Destroys: AX, BX, CX, DX, DI

  mov   ah, 0x0f        ; Attrib = white on black
  mov   cx, ax          ; Save char/attribute

  ; Calculate x and y offset
  movzx ax, byte [ypos] ; Get cursor y position
  mov   dx, 160         ; 80 columns of 2 bytes (char/attrib)
  mul   dx              ; Calculate linear y position
  movzx bx, byte [xpos] ; Get cursor x position
  shl   bx, 1           ; Multiply x pos by 2 (2 bytes, char/attrib)

  ; Implement x and y offset
  mov   di, 0
  add   di, ax          ; Add y offset
  add   di, bx          ; Add x offset

  mov   ax, cx          ; Restore char/attribute
  stosw                 ; Store AX (char/attrib) to ES:DI
  add   byte [xpos], 1  ; Move the cursor to the right

  ret
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
printreg16:
; Prints the word in reg16 as a hexadecimal string to the screen
; Destroys: AX, BX, CX, DI, SI

  mov   di, outstr16
  mov   ax, [reg16]
  mov   si, hexstr
  mov   cx, 4
hexloop:
  rol   ax, 4
  mov   bx, ax
  and   bx, 0x0f
  mov   bl, [si + bx] ; Index into hexstr
  mov   [di], bl
  inc   di
  dec   cx
  jnz   hexloop

  mov   si, outstr16
  call  sprint

  ret
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Data area
xpos      db 0
ypos      db 0
hexstr    db '0123456789abcdef'
outstr16  db '0000', 0
reg16     dw 0
;------------------------------------------------------------------------------
