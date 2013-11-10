#!/usr/bin/env python

# create tex files of the mma midi constants

import sys, os, commands

sys.path = ["/usr/local/share/mma/MMA", "/usr/share/mma/MMA", 
      "/home/bob/src/bv/mma/MMA/"] + sys.path

from miditables import *
from chordtable import chordlist

err, version = commands.getstatusoutput( "mma -v")
if err:
    print "Can't get MMA version ... strange error!"
    sys.ex

def dodrums(order):
    """ Print LaTex table of drum names. """

    notenames = ['E\\flat', 'E', 'F', 'G\\flat', 'G', 'A\\flat',
                 'A', 'B\\flat', 'B', 'C', 'D\\flat', 'D'] * 5


    if order == "m":
        for a in sorted(drumNames.keys()):
            n = drumNames[a].replace('&', '\&')
            outfile.write("\\insline{%s} {%s$^{%s}$}\n" % (a, n,notenames[a-27]))

    else:
        for a in sorted(drumInx.keys()):
            v=drumInx[a]
            n=drumNames[v].replace('&', '\&')
            outfile.write( "\\insline{%s} {%s$^{%s}$}\n" % (n, v, notenames[v-27]))


def docrtls(order):
    """ Print LaTex table of MIDI controller names. """
   
    if order == "m":
        for a in sorted(ctrlNames.keys()):
            n = ctrlNames[a].replace('&', '\&')
            outfile.write("\\insline{%s} {%s}\n" % (a, n))

    else:
        for a in sorted(ctrlInx.keys()):
            v=ctrlInx[a]
            n=ctrlNames[v].replace('&', '\&')
            outfile.write( "\\insline{%s} {%s}\n" % (n, v))


def doinsts(order):
    """ Print LaTex table of instrument names. """

    if order == "m":
        for a in sorted(voiceNames.keys()):
            if a>127: continue  # we don't want "none" to appear
            n = voiceNames[a].replace('&', '\&')
            outfile.write("\\insline{%s} {%s}\n" % (a, n))

    else:
        for a in sorted(voiceInx.keys()):
            v=voiceInx[a]
            if v>127: continue  # we don't want "none" to appear
            n=voiceNames[v].replace('&', '\&')
            outfile.write( "\\insline{%s} {%s}\n" % (n, v))

def dochords():
    """ Print out a list of chord names and docs in LaTex. """

    for n in sorted(chordlist.keys()):
        nm=n.replace("#", '$\\sharp$')
        nm=nm.replace('b', '$\\flat$')
        nm=nm.replace(chr(176), '\\diminished')
        nm=nm.replace(chr(248), '\\halfdim')

        outfile.write( "\\insline{%s}{%s}\n" % (nm, chordlist[n][2]) )


for a,f,o in (
    ('m', docrtls, 'ctrlmidi.AUTO'),
    ('a', docrtls, 'ctrlalpha.AUTO'),
    ('m', dodrums, 'drumsmidi.AUTO'),
    ('a', dodrums, 'drumsalpha.AUTO'),
    ('m', doinsts, 'instmidi.AUTO'),
    ('a', doinsts, 'instalpha.AUTO') ):
        outfile = file(o, 'w')
        f(a)
        outfile.close()

outfile = file("chordnames.AUTO", 'w')
dochords()
outfile.close()

