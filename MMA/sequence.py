
# sequence.py

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


"""

import gbl
from   MMA.common import *

from MMA.notelen import noteLenTable

from   MMA.alloc import trackAlloc
from   MMA.macro import macros
from   MMA.pat import pats

def trackSequence(name, ln):
    """ Define a sequence for a track.

    The format for a sequence:
    TrackName Seq1 [Seq2 ... ]

    Note, that SeqX can be a predefined seq or { seqdef }
    The {} is dynamically interpreted into a def.
    """

    if not ln:
        error ("Use: %s Sequence NAME [...]" % name)

    ln = ' '.join(ln)
    track = gbl.tnames[name]  # this is the pattern class

    if track.vtype == "SOLO":
            warning ("Sequences for SOLO tracks are not saved in Grooves.")

    """ Before we do extraction of {} stuff make sure we have matching {}s.
        Count the number of { and } and if they don't match read more lines and 
        append. If we get to the EOF then we're screwed and we error out. Only trick
        is to make sure we do macro expansion! This code lets one have long
        sequence lines without bothering with '\' continuations.
    """
    
    oLine=gbl.lineno   # in case we error out, report start line
    while ln.count('{') != ln.count('}'):
        l = gbl.inpath.read()
        if l == None:   # reached eof, error
            gbl.lineno = oLine
            error("%s Sequence {}s do not match" % name)

        l=' '.join(macros.expand(l))

        if l[-1] != '}' and l[-1] != ';':
            error("%s: Expecting multiple sequence lines to end in ';'" % name)

        ln += ' ' + l


    """ Extract out any {} definitions and assign them to new
        define variables (__1, __99, etc) and melt them
        back into the string.
    """

    ids=1

    while 1:
        sp = ln.find("{")

        if sp<0:
            break

        ln, s = pextract(ln, "{", "}", 1)
        if not s:
            error("Did not find matching '}' for '{'")

        pn = "_%s" % ids
        ids+=1

        trk=name.split('-')[0]
        trackAlloc(trk, 1)

        """ We need to mung the plectrum classes. Problem is that we define all
            patterns in the base class (plectrum-banjo is created in PLECTRUM)
            which is fine, but the def depends on the number of strings in the
            instrument (set by the tuning option). So, we save the tuning for
            the base class, copy the real tuning, and restore it.

            NOTE: at this point the base and current tracks have been initialized.
        """

        if trk == 'PLECTRUM' and name != trk:
            z=gbl.tnames[trk]._tuning[:]
            gbl.tnames[trk]._tuning = gbl.tnames[name]._tuning
        else:
            z = None

        gbl.tnames[trk].definePattern(pn, s[0])  # 'trk' is a base class!
        if z:
            gbl.tnames[trk]._tuning = z

        ln = ln[:sp] + ' ' + pn + ' ' + ln[sp:]

    ln=ln.split()
    
    """ We now have a sequence we can save for the track. All the {} defs have
        been converted to special defines (_1, _2, etc.).

        First we expand ln to the proper length. lnExpand() also
        duplicates '/' to the previous pattern.

        Then we step though ln:

          - convert 'z', 'Z' and '-' to empty patterns.

          - duplicate the existing pattern for '*'

          - copy the defined pattern for everything else.
            There's a bit of Python reference trickery here.
            Eg, if we have the line:

              Bass Sequence B1 B2

            the sequence is set with pointers to the existing
            patterns defined for B1 and B2. Now, if we later change
            the definitions for B1 or B2, the stored pointer DOESN'T
            change. So, changing pattern definitions has NO EFFECT.

    """

    ln = lnExpand(ln, '%s Sequence' % track.name)
    tmp = [None] * len(ln)

    for i, n in enumerate(ln):
        n=n.upper()

        if n in     ('Z', '-'):
            tmp[i] = None

        elif n == '*':
            tmp[i] = track.sequence[i]

        else:
            p= (track.vtype, n)
            if not p in pats:
                error("Track %s does not have pattern '%s'" % p )
            tmp[i] = pats[p]

    track.sequence = seqBump(tmp)

    if gbl.seqshow:
        print "%s sequence set:" % track.name,
        for a in ln:
            if  a in "Zz-":
                print "-",
            else:
                print a,
        print


def setAutoPat(name, ln):
    """ Set the auto create flag for a track. """

    track = gbl.tnames[name]  # this is the pattern class

    if not ln:
        error("%s AutoPattern: Needs at least one option." % track.name)

    autos = []

    for l in ln:
        if l.upper() in ('ON', '1'):
            autos.append(1)

        elif l.upper() in ('OFF', 'NONE', '0'):
            autos.append(None)

        else:
            error("%s AutoPattern: Unknown option." % track.name)

    track.autoPat = seqBump(autos)

