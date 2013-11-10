# midi.py

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


from   MMA.midiM  import intToWord, intTo3Byte, intToLong, intToVarNumber, intTo14
import MMA.midiC

import gbl
from   MMA.common import *
from   MMA.miditables import NONETONE

splitChannels = []

syncTone = [80,90]  # tone/velocity for -0 option. Changable from setSyncTone

# some constants we use to catorgize event types
MIDI_NOTE = 1
MIDI_PRG  = 2

def setSyncTone(ln):
    """ Parser routine, sets tone/velocity for the -0 sync tone. """

    global syncTone
    emsg = "SetSyncTone: Expecting option pairs: Tone=xx Velocity=xx | Volume=xx."
    
    notopts, ln = opt2pair(ln)

    if notopts or not ln:
        error(emsg)
    
    for cmd, opt in ln:
        cmd=cmd.upper()

        if cmd=="TONE":
            t = stoi(opt)
            if t<0 or t>127:
                error("SetSyncTone: Tone must be 0..127, not %s." % t)
            syncTone[0]=t

        elif cmd == "VELOCITY" or cmd == "VOLUME":
            t = stoi(opt)
            if t<1 or t>127:
                error("SetSyncTone: Velocity must be 1..127, not %s." % v)
            syncTone[1]=t

        else:
            error(emsg)

    if gbl.debug:
        print "SetSyncTone: Tone=%s, Velocity=%s" % tuple(syncTone)

def setSplitChannels(ln):
    """ Parser routine, sets up list of track to split. Overwrites existing. """

    global splitChannels

    splitChannels = []

    for a in ln:
        a = stoi(a)
        if a < 1 or a >16:
            error("SplitChannels: Expecting value 1 to 16, not %s" % a)
        splitChannels.append(a)

    if gbl.debug:
        print "SplitChannels: ",
        printList(splitChannels)


####################

def writeTracks(out):
    """ Write the accumulated MIDI tracks to file. """

    keys=gbl.mtrks.keys()
    keys.sort()

    """ For type 0 MIDI files all data is contained in 1 track.
        We take all our tracks and copy them to track 0, then
        set up keys[] so that only track 0 remains.
    """

    if gbl.midiFileType == 0:
        trk0=gbl.mtrks[0].miditrk
        for n in keys[1:]:
            trk=gbl.mtrks[n].miditrk
            for k,v in trk.items():
                if k in trk0:
                    trk0[k].extend(v)
                else:
                    trk0[k]=v
        keys=[0]

    # Write header

    tcount = len(keys)
    out.write( mkHeader(tcount, gbl.BperQ, gbl.midiFileType) )

    if gbl.barRange:   # compensate for -B/-b options
        stripRange()

    # Write data chunks for each track

    for n in keys:
        if gbl.debug:
            print "Writing <%s> ch=%s;" % \
                (gbl.mtrks[n].trackname, n),

        if n in splitChannels and gbl.midiFileType:
            tcount += writeSplitTrack(n, out)
        else:
            gbl.mtrks[n].writeMidiTrack(out)

    """ We may have changed the track count! So, we need to
        fix the file header. This is offset 10/11 which contains
        the number of tracks. The counter tcount has been
        tracking this, so just seek, replace and seek back.
    """

    if tcount != len(keys):
        out.seek(0)
        out.write( mkHeader(tcount, gbl.BperQ, gbl.midiFileType) )
        out.seek(0, 2)  # return to eof


def writeSplitTrack(channel, out):
    """ Split a track. In drum tracks this puts different instruments
        into individual tracks (useful!); for instrument tracks it puts
        each pitch into a track (probably not useful).
    """

    tr = gbl.mtrks[channel].miditrk   # track to split

    """ A dict to store the split midi tracks. We'll end out with
        a track for each pitch which appears in the track and
        a track (labeled -1) to store every other-than-note-on data.
    """

    notes={}

    onEvent = 0x90 + (channel-1)
    offEvent = 0x80 + (channel-1)

    for offset in tr.keys():
        for x in range(len(tr[offset])-1, -1, -1):
            ev = tr[offset][x]
            if len(ev) == 3 and ( ord(ev[0]) in (onEvent, offEvent)):
                n = ord(ev[1])
            else:
                n = -1      # special value for non-note on events

            if not n in notes:   # create a new mtrk if needed
                notes[n]=Mtrk(10)

            if offset in notes[n].miditrk:  # copy event to new track
                notes[n].miditrk[offset].append(ev)
            else:
                notes[n].miditrk[offset]=[ev]

    if gbl.debug:
        print " Data has been split into %s tracks." % len(notes)

    # Insert a channel name in all the new tracks.

    for a in notes.keys():
        if a == -1:
            continue
        if channel == 10:
            m = "%s" % MMA.midiC.valueToDrum(a)
        else:
            m = "%s-%s" % (gbl.mtrks[channel].trackname, a)

        notes[a].addTrkName(0, m)

    for a in sorted(notes.keys()):
        notes[a].writeMidiTrack(out)

    """ The split tracks have been written. Return the number of additional tracks
        so that the caller can properly update the midi file header. Note that
        len(notes)-1 IS CORRECT ... we've already figured on writing 1 track.
    """

    return len(notes)-1


def mkHeader(count, tempo, Mtype):

    return "MThd" + intToLong(6) + intToWord(Mtype) + intToWord(count) + intToWord(tempo)


""" Midi track class. All the midi creation is done here.
    We create a class instance for each track. mtrks{}.
"""

class Mtrk:

    def __init__(self, channel):
        self.miditrk={}
        self.channel = channel-1
        self.trackname = ''
        self.lastEvent = [None] * 129
        self.lastPrg = 0

    def delDup(self, offset, cmd):
        """ Delete a duplicate event. Used by timesig, etc.    """

        tr = self.miditrk
        lg = len(cmd)
        if offset in tr:
            tmp = []
            for ev in tr[offset]:
                if ev[0:lg] != cmd:  # not target, copy it
                    tmp.append(ev)
            tr[offset] = tmp           # new set of events (may be empty)

    def addTimeSig(self, offset,  nn, dd, cc, bb):
        """ Create a midi time signature.

            delta - midi delta offset
            nn = sig numerator, beats per measure
            dd - sig denominator, 2=quarter note, 3=eighth,
            cc - midi clocks/tick
            bb - # of 32nd notes in quarter (normally 8)

            This is only called by timeSig.set(). Don't
            call this directly since the timeSig.set() checks for
            duplicate settings.
        """

        cmd = chr(0xff) + chr(0x58)
        self.delDup(offset, cmd)   # NEEDED???
        self.addToTrack(offset, cmd + chr(0x04) + \
            chr(nn) + chr(dd) + chr(cc) + chr(bb) )


    def addKeySig(self, offset, n, mi):
        """ Set the midi key signature. """

        cmd = chr(0xff) + chr(0x59)
        self.delDup(offset, cmd)
        self.addToTrack(offset, cmd + chr(0x02) + chr(n) + chr(mi) )


    def addMarker(self, offset, msg):
        """ Create a midi MARKER event."""

        self.addToTrack(offset, chr(0xff) + chr(0x06) + intToVarNumber(len(msg)) + msg )


    def addCopyright(self, offset, msg):
        """ Insert copyright. """

        # should never happen since the caller sets offset=0
        if offset != 0:
            error("Copyright message must be at offset 0, not %s." % offset)

        # we need to bypass addToTrack to force copyright to the start of the track.

        ev=chr(0xff) + chr(0x02) + intToVarNumber(len(msg)) + msg
        tr=self.miditrk

        if hasattr(self, 'ipoint'):
            self.ipoint += 1
        else:
            self.ipoint=0
        
        if offset in tr:
            tr[offset].insert(self.ipoint, ev)
        else:
            tr[offset]=[ev]

        
    def addText(self, offset, msg):
        """ Create a midi TextEvent."""

        self.addToTrack( offset, chr(0xff) + chr(0x01) + intToVarNumber(len(msg)) + msg )

    def addLyric(self, offset, msg):
        """ Create a midi lyric event. """

        self.addToTrack( offset,
            chr(0xff) + chr(0x05) + intToVarNumber(len(msg)) + msg )

    def addCuePoint(self, offset, msg):
        """ Create a MIDI cue pointr event. """

        self.addToTrack( offset, chr(0xff) + chr(0x07) + intToVarNumber(len(msg)) + msg )


    def addTrkName(self, offset, msg):
        """ Creates a midi track name event. """

        offset = 0 # ignore user offset, always put this at 0

        self.trackname    = msg

        cmd = chr(0xff) + chr(0x03)
        self.delDup(offset, cmd)
        self.addToTrack(offset, cmd + intToVarNumber(len(msg)) + msg )

    def addProgChange( self, offset, program, oldprg):
        """ Create a midi program change (handles extended voicing).

            program - The MIDI program (voice) value
            oldprg  - existing MIDI program
        """

        # We truck around a special pseudo voice 'NONE' or 127.127.127 which
        # is a signal that we don't want mma to set the voicing. Might be
        # useful when know the track that mma is using and we have preset
        # an external synth.

        if program == NONETONE:
            return

        v1, lsb1, msb1 = MMA.midiC.voice2tup(oldprg)
        v2, lsb2, msb2 = MMA.midiC.voice2tup(program)

        if msb1 != msb2:   # only if CTRL32 has changed
            self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x20) + chr(msb2) )

        if lsb1 != lsb2:   # only if CTRL0 has changed
            self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x00) + chr(lsb2) )

        # Always do voice change. Maybe not necessary, but let's be safe.

        self.addToTrack(offset, chr(0xc0 | self.channel) + chr(v2), MIDI_PRG )

    def addGlis(self, offset, v):
        """ Set the portamento. LowLevel MIDI.

            This does 2 things:
                1. turns portamento on/off,
                2. sets the LSN rate.
        """

        if v == 0:
            self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x41) + chr(0x00) )

        else:
            self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x41) + chr(0x7f) )
            self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x05) + chr(v) )


    def addWheel(self, offset, v):
        """ Set lsb/msb for the modulation wheel. """

        lsb, msb = intTo14(v)
        self.addToTrack(offset, chr(0xe0 | self.channel) + lsb + msb )

    def addPan(self, offset, v):
        """ Set the lsb of the pan setting."""

        self.addToTrack(offset, chr(0xb0 | self.channel) + chr(0x0a) + chr(v) )


    def addCtl(self, offset, l):
        """ Add arbitary control sequence to track."""

        self.addToTrack(offset, chr(0xb0 | self.channel) + l)


    def addNoteOff(self, offset):
        """ Insert a "All Note Off" into the midi stream.

            Called from the cutTrack() function.
        """

        self.addToTrack(offset,
            chr(0xb0 | self.channel) + chr(0x7b) + chr(0), 1 )

    def addMasterVolume(self, offset, v):
        """ System Exclusive master volume message. Meta track. """

        lsb, msb = intTo14(v)
        self.addToTrack(offset,
                chr(0xf0) +    # Start sysex
                chr(0x07) +    # message size (needed for SMF)
                chr(0x7f) +    # realtime
                chr(0x7f) +    # disregard channel
                chr(0x04) +    # device control
                chr(0x01) +    # master volume
                lsb + msb  +   # params (14 bit)
                chr(0xf7) )    # EOX


    def addChannelVol(self, offset, v):
        """ Set the midi channel volume."""
        
        tr = self.miditrk
        cvol = chr(0xb0 | self.channel) + chr(0x07)  # 2 byte channel vol

        # Before adding a new channel volume event we check to see if there
        # are any future channel volume envents and delete them.
        
        for off in tr:  
            if off >= offset:
                tr[off] = [e for e in tr[off] if e[0:2] != cvol] 
        self.addToTrack(offset, cvol + chr(v) )


    def addTempo(self, offset, beats):
        """ Create a midi tempo meta event.

             beats - beats per second

             Return - packed midi string
        """

        cmd = chr(0xff) + chr(0x51) + chr(0x03)
        self.delDup(offset, cmd)
        self.addToTrack( offset, cmd + intTo3Byte(60000000/beats) )


    def writeMidiTrack(self, out):
        """ Create/write the MIDI track.

            We convert timing offsets to midi-deltas.
        """

        tr = self.miditrk

        """ If the -1 flag is set we need to add a terminate
            to the end of each track. This is done to make looping
            software like seq24 happy. We do this by truncating all
            data in the file past the current tick pointer and inserting
            an all-notes-off at that position.
        """

        if gbl.endsync and self.channel>=0:
            eof = gbl.tickOffset
            for offset in tr.keys():
                if offset > eof:
                    del tr[offset]
            self.addToTrack(eof, chr(0xb0 | self.channel) + chr(0x7b) + chr(0))


        """ To every MIDI track we generate we add (if the -0 flag
            was set) an on/off beep at offset 0. This makes for
            easier sync in multi-tracks.
        """

        if gbl.synctick and self.channel >= 0:
            t,v = syncTone
            self.addToTrack(0, chr(0x90 | self.channel) + chr(t) + chr(v) )
            self.addToTrack(1, chr(0x90 | self.channel) + chr(t) + chr(0) )


        if gbl.debug:
            ttl = 0
            lg=1
            for t in tr:
                a=len(tr[t])
                if a > lg:
                    lg = a
                ttl += a
            print "Unique ts: %s; Ttl events %s; Average ev/ts %.2f" % \
                (len(tr), ttl,    float(ttl)/len(tr) )

        last = 0

        # Convert all events to MIDI deltas and store in
        # the track array/list

        tdata = []        # empty track container
        lastSts = None    # Running status tracker

        for a in sorted(tr.keys()):
            delta = a-last
            
            if not tr[a]:
                continue  # this skips the delta offset update!

            for d in tr[a]:
                """ Running status check. For each packet compare
                    the first byte with the first byte of the previous
                    packet. If it is can be converted to running status
                    we strip out byte 0. Note that valid running status
                    byte are 0x80..0xef. 0xfx are system messages
                    and are note suitable for running status.
                """

                if len(d) > 1:
                    if d[0] == lastSts:
                        d=d[1:]
                    else:
                        lastSts = d[0]
                        s=ord(lastSts)
                        if s < 0x80 or s > 0xef or not gbl.runningStatus:
                            lastSts = None

                tdata.extend( [ intToVarNumber(delta) , d ] )
                delta = 0
            last = a

        # Add an EOF to the track (included in total track size)

        tdata.append( intToVarNumber(0))
        tdata.append( chr(0xff) + chr(0x2f) + chr(0x00) )

        tdata = ''.join(tdata)
        totsize = len(tdata)

        out.write("MTrk")
        out.write(intToLong(totsize))
        out.write( tdata )


    def addPairToTrack(self, boffset, startRnd, endRnd, duration, note, v, unify):
        """ Add a note on/off pair to a track.

            boffset      - offset into current bar
            startRnd, endRnd  - rand val start adjustment
            duration  - note len
            note      - midi value of note
            v      - midi velocity
            unify      - if set attempt to unify/compress on/offs

            This function tries its best to handle overlapping events.
            Easy to show effect with a table of note ON/OFF pairs. Both
            events are for the same note pitch.

            Offsets     |     200  |      300  |  320  |  420
            ---------|--------|--------|-------|--------
            Pair1     |     on      |       |  off  |
            Pair2     |      |      on   |       |  off

            The logic here will delete the OFF event at 320 and
            insert a new OFF at 300. Result is that when playing
            Pair1 will turn off at 300 followed by the same note
            in Pair2 beginning sounded right after. Why the on/off?
            Remember: Velocities may be different!

            However, if the unify flag is set we should end up with:

            Offsets     |     200  |      300  |  320  |  420
            ---------|--------|--------|-------|--------
            Pair1     |     on      |       |       |
            Pair2     |      |       |       |  off

        """

        # Start/end offsets

        onOffset  = getOffset( boffset, startRnd, endRnd)
        offOffset = onOffset + duration

        # ON/OFF events

        onEvent  = chr(0x90 | self.channel) + chr(note) + chr(v)
        offEvent = onEvent[:-1] + chr(0)

        """ Check for overlap on last event set for this track and
            do some ugly trickry.

            - The noOnFlag is set if we don't want to have the main
              routine add in the ON event. This is set when UNIFY is
              set and we have an overlap.

            - We set F to the stored event time for this note and,
              if it's in the same event range as the current event
              we loop though the saved events for this track. We are
              looking for a NOTE OFF event.

            - If we get a matching event we then delete it from the
              track. This requires 2 statements: one for an event
              list with only 1 event, a 2nd for multiple events.

             - If UNIFY is NOT set we insert a NOTE OFF at the current
               on time. This replaces the OFF we just deleted.

             - If UNIFY is SET we skip the above step, and we set the
               noOnFlag so that the ON event isn't set.

        """

        noOnFlag = None

        f=self.lastEvent[note]
        if f >= onOffset and f <= offOffset:
            tr=self.miditrk
            for i in range(len(tr[f])):
                if tr[f][i] == offEvent:
                    if len(tr[f]) == 1:
                        del(tr[f])
                    else:
                        del(tr[f][i])
                    if not unify:
                        self.addToTrack(onOffset, offEvent, MIDI_NOTE)
                    else:
                        noOnFlag=1
                    break

        if not noOnFlag:
            self.addToTrack(onOffset, onEvent, MIDI_NOTE)
        self.addToTrack(offOffset, offEvent, MIDI_NOTE )

        # Save the NOTE OFF time for the next loop.

        self.lastEvent[note] = offOffset


    def addNoteOnToTrack(self, boffset, note, v, startRnd=None, endRnd=None):
        """ Add a single note on or note off when v=0 to a track.
            boffset      - offset into current bar
            duration  - note len
            note      - midi value of note
            v      - midi velocity, set to 0 for note off
            startRnd/endRnd  - rand val start adjustment

            Added by louisjb for plectrum.
        """

        # Start offsets

        onOffset  = getOffset( boffset, startRnd, endRnd)

        # ON/OFF events (off is on with v = 0)

        onEvent  = chr(0x90 | self.channel) + chr(note) + chr(v)

        self.addToTrack(onOffset, onEvent, MIDI_NOTE )



    def zapRangeTrack(self, start, end):
        """ Clear NoteOn events from track in range: start ... end.

            This is called from the fermata function.

            We delete the entire event list (3 bytes) from the buffer. This
            can result in empty directory enteries, but that isn't a problem.
        """

        trk=self.miditrk
        startEvents = []
        endEvents = []
        for f in sorted(trk):
            if f > start and f < end:
                print 'zap',f, len(trk[f])
                for ev in trk[f]:
                    if len(ev)==3 and (ord(ev[0]) & 0xF0 == 0x90) and ord(ev[2])== 0:
                            endEvents.append(ev)
                    else:
                        startEvents.append(ev)
                del trk[f]
        print start,end, "start",len(startEvents),"end", len(endEvents)

        if startEvents:
            if start in trk:
                trk[start].extend(startEvents)
            else:
                trk[start] = startEvents

        if endEvents:
            if end in trk:
                trk[end] = endEvents + trk[end]
            else:
                trk[end] = endEvents
        
    def addToTrack(self, offset, event, evType=None):
        """ Add an event to a track.

            MIDI data is saved as created in track structures.
            Each track has a miditrk dictionary entry which used
            the time offsets and keys and has the various events
            as data. Each event is a packed string of bytes and
            the events are stored as a list in the order they are
            created. Our storage looks like:

                 miditrk[OFFSET_VALUE] = [event1, event2, ...]

            evType is an optional arg. Two values are used:
                 MIDI_PR - a program (voice) change. Save the timestamp.
                 MIDI_NOTE - note on/off ... check to see it doesn't happen
                             before the last program change.

        """

        if evType == MIDI_NOTE and offset < self.lastPrg:
            offset = self.lastPrg
        elif evType == MIDI_PRG:
            self.lastPrg=offset

        # Important. It's possible that a negative delay could generate
        # a negative offset.

        if offset < 0:
            offset = 0

        tr=self.miditrk

        if offset in tr:
            tr[offset].append(event)
        else:
            tr[offset]=[event]


class TimeSig:
    """ Track and set the current time signature.

        Timesigs are completely optional and are inserted into
        the MIDI file by addTimeSig(). MMA routines ignore timesig
        settings.
    """

    def __init__(self):
        """ Initialze to null value, user will never set to this."""

        self.lastsig = (None,None)

    def set(self, nn, dd):
        """ Set timesig. If no change from last value, ignore. """

        if self.lastsig != (nn, dd):
            gbl.mtrks[0].addTimeSig(gbl.tickOffset, nn, dd, 48, 8)
            self.lastsig = (nn, dd)

    def get(self):
        """ Return existing timesig. """

        return self.lastsig


timeSig = TimeSig()



def stripRange():
    """ Strip out range limited data. """

    bp = gbl.barPtrs   # list generated at compile time

    if gbl.barRange[-1] == 'ABS':
        gbl.barRange.pop()  # delete abs marker
        for a in bp:        # convert comment numbers to abs numbers
            bp[a][0] = str(a)
    validRange = []

    for a in gbl.barRange:   # list of bars we want to produce
        for b in bp:
            if a == bp[b][0]:
                validRange.append([bp[b][1], bp[b][2]])

    if not validRange:
        print "  Range directive -b/B would result in empty file."
        print "  Entire file is being created. Check the range."
        return

    # Collaspe/merge the valid range pointers
    validRange.sort()   # barptrs was dict, so this list is not in order
    tmp = []
    a,b = validRange[0]
    for i in range(1, len(validRange)):
        if b+1 == validRange[i][0]:
            b = validRange[i][1]
        else:
            tmp.append( [a,b] )
            a,b = validRange[i]
    tmp.append( [a,b] )
    validRange = tmp  # list of event times to keep

    """ Create list of event times to discard. Each item in the list:
        is [ start-time, end-time]
     """

    disList = []
    lowestEv = bp[1][1]
    for a in validRange:
        disList.append( [lowestEv, a[0]-1] )
        lowestEv = a[1]

    """ Determine the last event time in the buffer. This is not nesc. the
        same as the last barptr ... tempos and midinote can write past the
        last bar. We need to test each track and find the highest event time.
    """

    lastev = -1
    for a in gbl.mtrks:
        z = sorted(gbl.mtrks[a].miditrk)
        if z[-1]>lastev:
            lastev = z[-1]

    disList.append( [validRange[-1][1]+1, lastev] )

    """ 1st pass. For each track take all the cut parts and adjust their
        offsets to the end offset of the previous keep section. Strip out
        all note on events, lyric and text events in the effected bits.
    """

    for start, end in disList:
        for n in gbl.mtrks:
            tr=gbl.mtrks[n].miditrk
            newEvents = []
            for ev in sorted(tr.keys()):   # sort is important!
                if ev >= start and ev <=end:
                    for e in tr[ev]:
                        e0 = ord(e[0]) & 0xf0
                        # skip note on events
                        if e0 == 0x90 and ord(e[2]):
                            continue
                        # skip lyric and text events
                        if ord(e[0]) == 0xff:
                            if ord(e[1]) == 0x05 or ord(e[1]) == 0x01:
                                continue
                        # skip if event is note off and it already is present
                        # note: mma never generates a 0x80 (note-off) event, but
                        # the test is included here since midi-inc may have them.
                        if (e0 == 0x90 and ord(e[2]) == 0) or (e0 == 0x80):
                            for x in newEvents:
                                if x == e:
                                    e=None
                                    break
                        if e:
                            newEvents.append(e)
                    del (tr[ev])

            """ We now have a new list of events for the 'start' offset (it might
                just be an empty list) AND we have deleted the events for 'start'
                to 'end'. Just a matter of inserted a new event list.
            """

            if newEvents:
                tr[start] = newEvents

    """ 2nd pass. Adjust offsets of stuff to keep. """

    offset = 0
    for vals in range(len(validRange)):   # each valid range
        start = validRange[vals][0]
        end   = validRange[vals][1]+1
        offset += (start - disList[vals][0])
        for a in gbl.mtrks:               # each track
            tr = gbl.mtrks[a].miditrk
            for ev in sorted(tr.keys()):  # each event list
                if ev >= start and ev <= end:
                    newoffset = ev-offset
                    if ev != newoffset:  # don't append/copy creating duplicates
                        if newoffset in tr:
                            tr[newoffset].extend(tr[ev])
                        else:
                            tr[newoffset] = tr[ev]
                        del tr[ev]

    """ After all this compressing the end of file marker is past the
        end of the real data. This could bugger up the -1 option, so fix
        it. Go though all the tracks again and find the end.
    """

    lastev = -1
    for a in gbl.mtrks:
        z = sorted(gbl.mtrks[a].miditrk)
        if z and z[-1]>lastev:
            lastev = z[-1]
    gbl.tickOffset = lastev

    print "  File has been truncated for -b/-B option. Bar count/time incorrect."
