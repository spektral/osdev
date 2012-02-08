;------------------------------------------------------------------------------
; File:         bootload.asm
; Description:  Bootloader and experimental functionality 
; Author:       Christofer Odén
;------------------------------------------------------------------------------
[bits 16]

start:
    mov     ax, 0x07c0    ; Set data segment to where we are after boot (07c0)
    mov     ds, ax

    mov     ax, 0x09c0    ; Set stack segment to 09c0
    mov     ss, ax
    xor     sp, sp

    call    init_video

    ; Read sectors to video memory + palette
    mov     ax, 0xa000      ; Video memory segment
    mov     es, ax          ;   -> ES
    mov     bx, 0           ; Destination offset
    mov     ax, 1           ; Sector offset
    mov     cx, 256         ; Sector count (320*200 + 768) rounded to sector
    call    read_sectors

    ; Set palette
    mov     ax, 0xa000
    mov     es, ax
    mov     si, 0xfa00      ; [ES:SI] = Just after video mem
    mov     cx, 256         ; Color count
    call    set_palette
    ; End read and set palette

    mov     ax, 0xa000
    mov     es, ax
    mov     si, 0xfa00
    mov     cx, 512
    ;call    dump_mem

    hlt
    jmp     $

; Procedures ------------------------------------------------------------------

;-----------------
; Set VGA palette
; Input:    es:si - pointer to palette
;           cx    - color count
; Output:   none
set_palette:
    push    ax              ; Save registers
    push    bx

    mov     ax, 0           ; Start at color number 0
.l1:
    mov     ah, byte [es:si]; Get red component
    inc     si              ; Step
    mov     bl, byte [es:si]; Get green component
    inc     si              ; Step
    mov     bh, byte [es:si]; Get blue component
    inc     si              ; Step

    ; Set color in palette
    call    set_color

    inc     al              ; Step to next color
    loop    .l1             ; Loop

    pop     bx              ; Restore registers
    pop     ax
    ret

;-------------------------------
; Set specific color in palette
; Input:    al - color number
;           ah - Red component
;           bl - Green component
;           bh - Blue component
; Output:   Palette data is updated
set_color:
    push    ax
    push    bx
    push    dx

    mov     dx, 0x03c8      ; Choose color
    out     dx, al          ;   number AL

    mov     dx, 0x03c9      ; Set palette
    mov     al, ah
    out     dx, al          ; Set red

    mov     al, bl
    out     dx, al          ; Set green

    mov     al, bh
    out     dx, al          ; Set blue

    pop     dx
    pop     bx
    pop     ax
    ret

;-------------
; Dump memory
; Input     ES:SI - Start of memory
;           CX - Byte count
dump_mem:
    cld                 ; Direction = Increasing
.L1:
    mov     al, byte [es:si] ; AL = [ES:SI]
    inc     si
    call    put_byte    ; Print AL as hex to screen
    loop    .L1         ; CX--; JNZ .L1
    ret 

;----------------------
; Print byte to screen
; Input:    AL - Byte to print
put_byte:
    call    byte_to_hex ; AL:BL = High:low nybble as ASCII hex
    call    put_char    ; Print ASCII in AL to screen
    mov     al, bl      ; Move low nybble to al
    call    put_char    ; Print ASCII in AL to screen
    ret

;----------------------
; Write char to screen
; Input:    AL - ASCII value to print
put_char:
    mov     ah, 0x0e
    int     0x10
    ret

;--------------------------------------
; Transform a byte to ascii hex values
; Input:    AL - Byte to transform
; Output:   AX - High nybble as ASCII hex
;           BL - Low nybble as ASCII hex
byte_to_hex:
    push    si              ; Save SI

    xor     ah, ah
    push    ax              ; Save byte
    and     al, 0x0f        ; Get low nybble of AL
    mov     si, hexdata     ; Get ASCII hex string
    add     si, ax          ; Move to offset in string
    mov     bl, byte [ds:si]; Store low nybble ASCII hex in BL
    pop     ax              ; Restore byte
    shr     al, 4           ; Shift high nybble to low nybble
    and     al, 0x0f        ; Repeat previous steps
    mov     si, hexdata     ;
    add     si, ax          ;
    mov     al, byte [ds:si]; Store high nybble ASCII hex in AL

    pop     si              ; Restore SI
    ret

;------------------
; Initialize video
init_video:
    xor     ax, ax          ; ah = 0x00, Set Video Mode Function
    mov     al, 0x13        ; VGA 320x200 256 color
    int     0x10
    ret

;-------------------
; Output read error
; Not implemented
read_error:
     hlt

;----------------------------------
; Read sectors from disk to memory
; Input:    ax - Start sector
;           cx - Sector count
; Output:   [es:bx] - Read data
read_sectors:
.main:
    mov     di, 5
.sectorloop:
    push    ax
    push    bx
    push    cx
    call    lba2chs                 ; Fetch the CHS from LBA
    mov     ah, 02                  ; Read function
    mov     al, 01                  ; Read one sector
    mov     ch, byte [chs_cylinder] ; Get track from chs
    mov     cl, byte [chs_sector]   ; Get sector from chs
    mov     dh, byte [chs_head]     ; Get head from chs
    mov     dl, byte [drive]        ; Get drive number
    int     0x13                    ; Read a sector
    jnc     .success
    dec     di                      ; Try to read the sector again
    pop     cx                      ;  if it failed.  Try di times.
    pop     bx
    pop     ax
    jnz     .sectorloop
    call    read_error
.success:
    pop     cx                      ; Restore the registers
    pop     bx
    pop     ax
    add     bx, word [bytes_per_sector] ; Adjust output destination
    inc     ax                      ; Set to read next sector
    dec     cx                      ; Decrease the sector counter
    jz      .endread
    loop    .main
.endread:
    ret

;--------------------
; Convert LBA to CHS
; Input:    ax - LBA value
; Output:   chs_cylinder - cylinder
;           chs_head - head
;           chs_sector - sector
lba2chs:
    push    bx                      ; Save registers
    push    dx

    xor     dx, dx                  ; Zero dx
    movzx   bx, byte [sectors_per_track] ; Fetch SPT constant
    div     bx                      ; dx:ax / bx -> ax, dx
                                    ;             (LBA / SPT = tracks, sector)
    inc     dx                      ; Add one to sector (1-indexed)
    mov     [chs_sector], dl        ; Save sector

    xor     dx, dx                  ; Zero dx
    movzx   bx, byte [head_count]   ; Fetch number of heads
    div     bx                      ; dx:ax / bx -> ax, dx
                                    ;  (tracks / heads = cylinder, head)
    mov     [chs_cylinder], al      ; Save cylinder
    mov     [chs_head], dl          ; Save head

    pop     dx
    pop     bx                      ; Restore registers
    ret

; Data, padding and boot signature --------------------------------------------
    chs_cylinder        db 0
    chs_head            db 0
    chs_sector          db 0
    drive               db 0

    bytes_per_sector    dw 512
    sectors_per_track   db 18
    tracks_per_head     db 80
    head_count          db 2

    hexdata             db "0123456789abcdef"

    times 510-($-$$)    nop
                        dw 0xaa55
