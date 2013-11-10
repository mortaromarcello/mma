# midiM.py

"""
This module is an integeral part of the program
MMA - Musical Midi Accompaniment.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Bob van der Poel <bob@mellowood.ca>

This module contains the MIDI number (un)packing routines.

These are necessary to create the MSB/LSB stuff that
MIDI expects.

"""


def intToWord(x):
    """ Convert INT to a 2 byte MSB LSB value. Used in MIDI headers. """

    return    chr(x>>8 & 0xff) + chr(x & 0xff)

def intTo3Byte(x):
    """ Convert INT to a 3 byte MSB...LSB value. """

    return intToLong(x)[1:]

def intToLong(x):
    """ Convert INT to a 4 byte MSB...LSB value. """

    return intToWord(x>>16) + intToWord(x)


def intToVarNumber(x):
    """ Convert INT to a variable length MIDI value. """

    lst = chr(x & 0x7f)
    while  1:
        x = x >> 7
        if x:
            lst = chr((x & 0x7f) | 0x80) + lst
        else:
            return lst


def intTo14(x):
    """ Convert INT to a 2 byte 14 bit number

        Value to be converted must be 0...16383. A
        ValueError is raised if out of range.
    """

    if x<0 or x>0x3fff:
        raise ValueError

    return  chr(0x007f & x), chr(x>>7)
