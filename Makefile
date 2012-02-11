AUX=auxiliary
ASM=nasm
ASM_FLAGS=-f bin
PYTHON=python2
TMPFILE=/tmp/tmp.jmIV49k1sB
PICTURE=loom.rli

floppy.img: bootload.bin $(PICTURE)
	dd if=/dev/zero of=$@ bs=1474560 count=1
	cat $^ > $(TMPFILE)
	dd if=$(TMPFILE) of=$@ conv=notrunc
	rm $(TMPFILE)

bootload.bin: bootload.asm
	$(ASM) $(ASM_FLAGS) -l $(subst .bin,.lst,$@) -o $@ $<

%.rli: %.bmp
	$(PYTHON) $(AUX)/bmp_to_pic.py $< $@

.PHONY=clean
clean:
	@rm *.o *.lst *.bin *.log *.dat *.rli

run: floppy.img
	bochs -q
