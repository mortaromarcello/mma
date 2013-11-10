
# tempo.py

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

#######################################
# Tempo/timing


def setTime(ln):
    """ Set the 'time sig'.

        We do restrict the time setting to the range of 1..12.
        No particular reason, but we do need some limit? Certainly
        it has to be greater than 0.
    """

    if len(ln) != 1:
        error("Time: Expecting single argument (integer)")

    n = stoi(ln[0], "Time: Argument must be integer.")

    if n < 1 or n > 12:
        error("Time: Value must be 1..12")

    # If no change, just ignore this.

    origTime = gbl.QperBar
    if origTime != n:
        gbl.QperBar = int(n)

        # Time changes zap all predfined sequences

        for a in gbl.tnames.values():
            if a.riff:
                warning("Time: Change from %s to %s deleting %s riffs." % (origTime, n, a.name))
                a.riff = []
            a.clearSequence()


def tempo(ln):
    """ Set tempo.

        Note: All tempo stuff is inserted into the meta track.
    """

    if not ln or len(ln) >2:
        error("Tempo: Use [*,+,-]BperM [BARS]")

    # Get new value.

    a = ln[0][0]
    if a in "+-*":
        v = stof(ln[0][1:], "Tempo: Expecting value for rate adjustment, not '%s'" % ln[0])
        if a == '-':
            v = gbl.tempo - v
        elif a == '+':
            v += gbl.tempo
        elif a == '*':
            v *= gbl.tempo

    else:
        v  = stof(ln[0], "Tempo: Expecting rate, not '%s'" % ln[0])

    if v <= 1:
        error("Tempo: Value must be greater than 1.")

    # is this immediate or over time?

    if len(ln) == 1:
        gbl.tempo = int(v)

        gbl.mtrks[0].addTempo(gbl.tickOffset, gbl.tempo)
        if gbl.debug:
            print "Tempo: Set to %s" % gbl.tempo


    else:              # Do a tempo change over bar count
        bars = ln[1]

        bars = stof(bars, "Tempo: Beat count expecting value, not %s" % bars )
        numbeats = int(bars * gbl.QperBar)

        if numbeats < 1:
            error("Tempo: Beat count must be greater than 1")

        # Vary the rate in the meta track

        tincr = (v - gbl.tempo) / float(numbeats)    # incr per beat
        bstart = gbl.tickOffset            # start
        boff = 0
        tempo = gbl.tempo

        for n in range(numbeats):
            tempo += tincr
            if tempo:
                gbl.mtrks[0].addTempo(bstart + boff, int(tempo))
            boff += gbl.BperQ

        if tempo != v:
            gbl.mtrks[0].addTempo(bstart + boff, int(v) )

        gbl.tempo = int(v)

        if gbl.debug:
            print "Tempo: Set future value to %s over %s beats" % \
                ( int(tempo), numbeats)

    if gbl.tempo <=0:
        error("Tempo: Setting must be greater than 0.")


def beatAdjust(ln):
    """ Delete or insert some beats into the sequence.

        This just adjusts the current song position. Nothing is
        lost or added to the actual file.
    """

    if len(ln) != 1:
        error("BeatAdjust: Expecting single value.")

    adj = stof(ln[0], "BeatAdjust: Expecting a value (not %s)." % ln[0])

    gbl.tickOffset += int(adj * gbl.BperQ)

    gbl.totTime += adj / gbl.tempo   # adjust total time

    if gbl.debug:
        print "BeatAdjust: inserted %s at bar %s." % (adj, gbl.barNum + 1)


def cut(ln):
    """ Insert a all-note-off into ALL tracks. """

    if not len(ln):
        ln=['0']

    if len(ln) != 1:
        error("Cut: Expecting single offset or empty argument.")

    """ Loop though all the tracks. Note that trackCut() checks
        to make sure that there is a need to insert in specified track.
        In this loop we create a list of channels as we loop though
        all the tracks, skipping over any duplicate channels or
        tracks with no channel assigned.
    """

    l = []
    for t in sorted(gbl.tnames.keys()):
        c = gbl.tnames[t].channel
        if not c or c in l:
            continue
        l.append(c)
        trackCut(t, ln)


def trackCut(name, ln):
    """ Insert a ALL NOTES OFF at the given offset. """


    if not len(ln):
        ln=['0']

    if len(ln) != 1:
        error("Cut %s: Offset missing." % name)

    offset = stof(ln[0], "Cut %s: Expecting value, (not '%s') for offest." % (name, ln[0]))

    if offset < -gbl.QperBar or offset > gbl.QperBar:
        warning("Cut %s: %s is a large beat offset" % (name, offset))

    moff = int(gbl.tickOffset + (gbl.BperQ * offset))

    if moff < 0:
        error("Cut %s: Offset comes before start of track." % name)

    # Insert allnoteoff directly in track. This skips the normal
    # queueing in pats because it would never take if at the end
    # of a track.

    m = gbl.tnames[name].channel
    if m and len(gbl.mtrks[m].miditrk) > 1:
        gbl.mtrks[m].addNoteOff(moff)

        if gbl.debug:
            print "Cut %s: Beat %s, Bar %s" % (name, offset, gbl.barNum + 1)


def fermata(ln):
    """ Apply a fermata timing to the specified beat. """

    if len(ln) != 3:
        error("Fermata: use 'offset' 'duration' 'adjustment'")

    offset = stof(ln[0], "Fermata: Expecting a value (not '%s') for offset" % ln[0] )

    if offset < -gbl.QperBar or offset > gbl.QperBar:
        warning("Fermata: %s is a large beat offset" % offset)

    dur = stof(ln[1], "Fermata: Expecting a value (not '%s') for duration" % ln[1])

    if dur <= 0:
        error("Fermata: duration must be greater than 0")

    if dur > gbl.QperBar:
        warning("Fermata: %s is a large duration." % dur)

    adj = stof(ln[2], "Fermata: expecting a value (not '%s') for adjustment." % ln[2])

    if adj< 100:
        warning("Fermata: Adjustment less than 100 is shortening beat value.")

    if adj == 100:
        error("Fermata: using value of 100 makes no difference, must be an error.")

    moff=int(gbl.tickOffset + (gbl.BperQ * offset))

    if moff < 0:
        error("Fermata: Offset comes before track start.")

    if offset >= 0:
        warning("Fermata: Better results when placed after event (negative offset).")

    fermataDuration = int(gbl.BperQ * dur)  # Duration in ticks
    mend = moff + fermataDuration

    # This next section is needed to figure out the start tempo (which
    # is not always gbl.tempo!), the needed tempo at the end of the fermata
    # section (again, not always gbl.tempo!) and to delete any tempo changes
    # in the section. All this due to the effects of tempo changes over a
    # a set of bars.

    # Extract ALL tempo changes from the meta track and save in a list.
    # Also, delete any tempo changes found in the miditrack in our range.

    tempos=[]
    tcmd = chr(0xff) + chr(0x51) + chr(0x03)   # midi TEMPO 
    mt = gbl.mtrks[0].miditrk  # The meta track

    # note 2 loops are needed. 1st we copy existing tempos into a new list
    for t in mt: 
        for ev in mt[t]:
            if ev[0:3] == tcmd:
                tempos.append((t, 60000000/ (ord(ev[3])*65535 + ord(ev[4])*255 + ord(ev[5]))))

    # now we delete any tempos in the fermata range. This avoids broken loops
    for t, ev in tempos:
        if t >= moff and t <= mend:
            gbl.mtrks[0].delDup(t, tcmd)
        
    # find last tempo before fermata bock and last tempo in block
    tempos.sort()
    oldTempo = gbl.tempo
    newTempo = gbl.tempo
    for f, t in tempos:
        if f<= moff: oldTempo = t
        if f>= moff and f <= mend: newTempo = t

    gbl.mtrks[0].addTempo(moff, int(oldTempo / (adj/100)) )
    gbl.mtrks[0].addTempo(mend, newTempo)

    # Move selected events in the effected area to it's start/end.
    #   To start -> note on, program change
    #   to end   -> note off
    # Done only if the fermata comes after desired location ... 
    # otherwise there aren't any events to zap!

    if offset < 0:
        for n, tr in gbl.mtrks.items():  # do each track
            if n <= 0: continue        # skip meta track

            trk = gbl.mtrks[n].miditrk
            startEvents = []
            endEvents = []

            for f in sorted(trk):          # all in this track (sort keeps orig order)
                if f > moff and f < mend:  # well, only in the fermata range 
                    remain = []
                    for ev in trk[f]:      # all events in the offset
                        if not ev:         # skip empty events 
                            continue
                        evtype = ord(ev[0]) >> 4  # what we have here
                        if evtype == 0x9:         # note event 
                            if ord(ev[2])== 0:    # off to end
                                endEvents.append(ev)
                            else:
                                startEvents.append(ev)  # on to start
                            continue
                        if evtype == 0xb or evtype == 0xc:  # program/controller change
                            startEvents.append(ev)          # all to start (??)
                            continue
                        remain.append(ev)
                    trk[f] = remain        # remaining events for this offset

            if startEvents:
                if moff in trk:
                    trk[moff].extend(startEvents)
                else:
                    trk[moff] = startEvents

            if endEvents:
                if mend in trk:
                    trk[mend] = endEvents + trk[mend]
                else:
                    trk[mend] = endEvents

    if gbl.debug:
        print "Fermata: Beat %s, Duration %s, Change %s, Bar %s" % \
              (offset, dur, adj, gbl.barNum + 1)
        if offset < 0:
            print "\tNoteOn Events moved in tick range %s to %s" \
                  % (moff+1, mend-1)

