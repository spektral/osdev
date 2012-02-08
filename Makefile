floppy.img: bootload.bin pic.raw pal.dat
	dd if=/dev/zero of=floppy.img bs=1474560 count=1
	cat bootload.bin pic.raw pal.dat > concat.tmp
	dd if=concat.tmp of=floppy.img conv=notrunc
	rm concat.tmp

bootload.bin: bootload.asm bio.inc
	nasm -f bin -l $(subst .bin,.lst,$@) -o $@ $<

pal.dat: monkey.pal
	./paltobytes.py $< $@

.PHONY=clean
clean:
	@rm *.o *.lst *.bin *.log *.dat

run: floppy.img
	bochs -q
