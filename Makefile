floppy.img: bootload.bin
	dd if=/dev/zero of=floppy.img bs=1474560 count=1
	dd if=bootload.bin of=floppy.img conv=notrunc

bootload.bin: bootload.asm bio.inc
	nasm -f bin -l $(subst .bin,.lst,$@) -o $@ $<

.PHONY=clean
clean:
	@rm *.o *.lst *.bin

run: floppy.img
