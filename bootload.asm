;------------------------------------------------------------------------------
; File:         bootload.asm
; Description:  Bootloader and experimental functionality 
; Author:       Christofer Odén
;------------------------------------------------------------------------------
[bits 16]

    jmp main              ; Jump past the included code

main:
    mov     ax, 0x07c0    ; We will be at DS:00 = 07c0:0000 after boot
    mov     ds, ax

    mov     ax, 0x09c0    ; Set stack to SS:SP = 09c0:0000
    mov     ss, ax
    xor     sp, sp

    call    init_video

	; Fill screen with something
	mov		ax, 0xa000	; Video mem
	mov		es, ax		; Segment = 0x0a00
	xor		ax, ax
	mov		di, ax		; Offset = 0
	mov		al, 0x1 	; Color = blue
	mov		cx, 64000 	; Count = 320x200
	rep		stosb

    ; Read boot sector to video memory
    mov     ax, 0xa000  ; Video memory is at a000:0000
    mov     es, ax      ; Destination segment = 0xa000
    xor     bx, bx      ; Destination offset = 0
    mov     ax, 1       ; Source LBA value
    mov     cx, 60      ; Sectors to read
    call    read_lba

    mov     ax, 0xa780  ; Video memory is at a000:0000
    mov     es, ax      ; Destination segment = 0xa000
    xor     bx, bx      ; Destination offset = 0
    mov     ax, 61      ; Source LBA value
    mov     cx, 65      ; Sectors to read
    call    read_lba

    jmp     $

; Procedures ------------------------------------------------------------------

; Initialize video
init_video:
    xor     ax, ax          ; ah = 0x00, Set Video Mode Function
    mov     al, 0x13        ; VGA 320x200 256 color
    ;mov     al, 0x12
    int     0x10
    ret

; Read data from floppy drive 0 with LBA
; Input:    ax - LBA value
;           cx - Bytes to read
;           es - Destination segment
;           bx - Destination offset
read_lba:
    push    dx          ; Save whatever is in dx

    push    bx          ; Push destination offset
    push    cx          ; Push byte read count

    call    lba2chs     ; ax, bx, cx -> C, H, S
    mov     ch, al      ; Cylinder
    mov     dx, bx      ; Head
    ;mov     cl, cl     ; Sector (already in place)
    mov     dl, 0       ; Drive, first = 0

    pop     ax          ; Pop byte count to read to al
    pop     bx          ; Pop destination offset

    mov     ah, 0x02    ; Function number
read_sectors:
    int     0x13
    jc      read_sectors

    pop     dx          ; Restore what was in dx

; Convert LBA to CHS
; Input:    ax - LBA value
; Output:   ax - Cylinder
;           bx - Head
;           cx - Sector
lba2chs:
    push    dx                      ; Save dx
    xor     dx, dx                  ; Zero dx
    mov     bx, [sector_per_track]   ; Fetch SPT constant
    div     bx                      ; dx:ax / bx -> ax, dx
                                    ;             (LBA / SPT = tracks, sector)
    inc     dx                      ; Add one to sector (1-indexed)
    push    dx                      ; Save sector to stack

    xor     dx, dx                  ; Zero dx
    mov     bx, [head_count]        ; Fetch number of heads
    div     bx                      ; dx:ax / bx -> ax, dx
                                    ;  (tracks / heads = cylinder, head)
    mov     bx, dx                  ; Move head to bx (cylinder is already in ax)
    pop     cx                      ; Pop sector to cx
    pop     dx                      ; Restore dx
    ret

; Data, padding and boot signature --------------------------------------------
    sector_per_track    dw 18
    head_count          dw 2

    times 510-($-$$)    nop
                        dw 0xaa55
    times 80            dd 0x12345678
    times 80            dd 0xabcddcba
    times 80            dd 0xefefefef
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
    times 80            dd 0x12345678
