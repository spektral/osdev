#!/usr/bin/python2
# -*- coding: utf-8 -*-

"""
Converts a BMP image to my RLI (Run Length encoded Image) format.


RLI Format Specification:

Offset  Size    Description
0       2       Image width
2       2       Image height
4       768     Palette (256 RGB tuples)
772     -       Run length encoded image data
Trailer 2       Two zero bytes, indicating end of image data


Run Length Encoding Specification:

Runs are coded as two bytes.  The first byte indicates how many of the
following byte is in the run.  The second byte indicates the value to
fill the run with.


NOTICE:

This implementation is simple to the extreme.

The run length encoding implementation is far from a fix-all.  A
noisy picture will in the worst case be twice its original size if
compressed with this algorithm!  The better cases are achieved by
having long horizontal lines of the same color in the image.

"""

import sys
import os
from struct import pack, unpack
from sys import argv, exit

__author__ = "Christofer Oden"
__copyright__ = "Copyright (C) 2012 Christofer Odén"
__email__ = "bei.oden@gmail.com"

class Auxiliary:

    """Contains some helper methods"""

    @staticmethod
    def read_dword(infile, offset):

        """Reads 4 bytes at the offset and unpacks them"""

        infile.seek(offset, os.SEEK_SET)
        return unpack('<L', infile.read(4))[0]

    @staticmethod
    def read_word(infile, offset):

        """Reads 2 bytes at the offset and unpacks them"""

        infile.seek(offset, os.SEEK_SET)
        return unpack('<H', infile.read(2))[0]


class BmpData:

    """Reads a BMP and parses the data"""

    def __init__(self, infile):

        """Reads data from a BMP file and parses it"""

        aux = Auxiliary

        infile.seek(0, os.SEEK_SET)

        magic_number = infile.read(2)
        if magic_number != "BM":
            raise "File is not a BMP"

        self.file_size = aux.read_dword(infile, 2)
        self.image_offset = aux.read_dword(infile, 10)
        self.header_size = aux.read_dword(infile, 14)
        self.width = aux.read_dword(infile, 18)
        self.height = aux.read_dword(infile, 22)
        self.bpp = aux.read_word(infile, 28)
        self.image_size = aux.read_dword(infile, 34)
        self.palette = self.read_palette(infile)
        self.image_data = self.read_image(infile)

    def read_palette(self, infile):

        """Reads the palette data"""

        infile.seek(54, os.SEEK_SET)
        # BMP palette is stored as 256 BGRa values, which means 256 * 4 bytes
        # VGA only uses the low 6 bits of a color, right shift by two
        data = infile.read(4 * 256)
        colors = []
        for i in range(0, 4 * 256, 4):
            colors.append(chr(ord(data[i+2]) / 4))
            colors.append(chr(ord(data[i+1]) / 4))
            colors.append(chr(ord(data[i]) / 4))
        return ''.join(colors)

    def read_image(self, infile):

        """Reads the image data"""

        infile.seek(self.image_offset, os.SEEK_SET)

        # BMP store rows in reverse FOR SOME REASON.  Bring order.
        rows = []
        for row in range(self.height):
            rows.insert(0,  infile.read(self.width))

        return ''.join(rows)


class RliData:

    """Converts BmpData to RliData and writes it to file"""

    def __init__(self, bmp_data):

        """Takes a BmpData and turns it into a RliData"""

        self.magic_number = "?P"
        self.width = bmp_data.width
        self.height = bmp_data.height
        self.palette = bmp_data.palette
        self.image_data = self.rle_compress(bmp_data.image_data)

    def rle_compress(self, data):

        """RLE compresses the data"""

        max_len = 255
        rle_data = []
        rcount = 1

        rvalA = data[0]
        index = 1

        while True:
            # Read the next value
            rvalB = data[index]
            index += 1

            if index == len(data):
                # Write the last object and finish
                rle_data.append("%c%c" % (rcount, rvalA))
                break
            
            # Check if still in a run and the max run length is not reached
            if rvalA == rvalB and rcount < max_len:
                # Just step to the next byte
                rcount += 1
                continue
            else:
                # Write the run length object and start with the next
                rle_data.append("%c%c" % (rcount, rvalA))
                rvalA = rvalB
                rcount = 1

        rle_data.append("\x00\x00")

        return ''.join(rle_data)

    def write(self, outfile):

        """Writes the data to file"""

        outfile.seek(0, os.SEEK_SET)
        outfile.write(pack('<H', self.width))
        outfile.write(pack('<H', self.height))
        outfile.write(self.palette)
        outfile.write(self.image_data)


if __name__ == "__main__":

    """Read the BMP file, transform it to a RLI file, and write it"""

    if (len(argv) < 3):
        print "Usage: %s BMPFILE OUTFILE" % argv[0]
        exit(1)

    with open(argv[1], "rb") as infile:
        bmp_data = BmpData(infile)

    rli_data = RliData(bmp_data)

    with open(argv[2], 'wb') as outfile:
        rli_data.write(outfile)
