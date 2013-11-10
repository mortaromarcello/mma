
# main.py

"""
The program "MMA - Musical Midi Accompaniment" and the associated
modules distributed with it are protected by copyright.

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

import os

import MMA.midi
import MMA.midifuncs
import MMA.parse
import MMA.file 
import MMA.options
import MMA.auto
import MMA.docs

import gbl
from   MMA.common import *
from   MMA.lyric import lyric
import MMA.paths

########################################
########################################

# This is the program mainline. It is called/executed
# exactly once from a call in the stub program mma.py.

MMA.paths.init()   # initialize the lib/include paths

# Get our command line stuff

MMA.options.opts()

#  LibPath and IncPath are set before option parsing ... Debug setting
#  wasn't set so check it now. 

if gbl.debug:
    print "Initialization has set LibPath set to", MMA.paths.libPath
    print "Initialization has set IncPath set to", MMA.paths.incPath

#######################################
# Set up initial meta track stuff. Track 0 == meta

m = gbl.mtrks[0] = MMA.midi.Mtrk(0)

if gbl.infile:
    if gbl.infile != 1:
        fileName=MMA.file.locFile(gbl.infile, None)
        if fileName:
            m.addTrkName(0, "%s" % fileName.rstrip(".mma") )
            m.addText(0, "Created by MMA. Input filename: %s" % fileName)


m.addTempo(0, gbl.tempo)              # most user files will override this
MMA.midifuncs.setTimeSig(['4','4'])   # most stdlib (and/or user) files will override this

#####################################
# Read an RC file. All found files are processed.

docOption = gbl.createDocs   # Disable doc printing for RC file
gbl.createDocs = 0

rcfiles = ('mmarc', 'c:\\mma\\mmarc', '~/.mmarc', '/usr/local/etc/mmarc', '/etc/mmarc'    )
if gbl.mmaRC:
    rcfiles = [ gbl.mmaRC ]

for i in rcfiles:
    f = MMA.file.locFile(i, None)
    if f:
        if gbl.showrun:
            print "Reading RC file '%s'" % f
        MMA.parse.parseFile(f)
        break
    else:
        if gbl.mmaRC:
            error("Specified init file '%s' not found" % gbl.mmaRC)

else: #if not rcread:
    if gbl.debug:
        gbl.lineno = -1
        warning("No RC file was found or processed")

gbl.createDocs = docOption   # Restore doc options


################################################
# Update the library database file(s) (-g option)
# Note: This needs to be here, after reading of RC files

if gbl.makeGrvDefs:
    if gbl.infile:
        error("No filename is permitted with the -g option")
    MMA.auto.libUpdate()                # update and EXIT


################################
# We need an input file for anything after this point.

if not gbl.infile:
    MMA.options.usage("No input filename specified.")

################################
# Just extract docs (-Dxh, etc) to stdout.

if docOption:
    if docOption == 4:
        MMA.docs.htmlGraph(gbl.infile)
    else:
        f=MMA.file.locFile(gbl.infile, None)
        if not f:
            error("File '%s' not found" % gbl.infile)
        MMA.parse.parseFile(f)
        MMA.docs.docDump()
    sys.exit(0)


#########################################################
# These cmdline options override settings in RC files

if gbl.cmdSMF:
    gbl.lineno = -1
    MMA.midifuncs.setMidiFileType(['SMF=%s' % gbl.cmdSMF])

######################################
# Create the output filename

MMA.paths.createOutfileName(".mid")


################################################
# Read/process files....

# First the mmastart files

for f in gbl.mmaStart:
    fn = MMA.file.locFile(f, MMA.paths.incPath)
    if not fn:
        warning("MmaStart file '%s' not found/processed" % fn)
    MMA.parse.parseFile(fn)
    gbl.lineno = -1

# The song file specified on the command line

if gbl.infile == 1:  # use stdin, set filename to 1
    f = 1
else:
    f = MMA.file.locFile(gbl.infile, None)

    if not f:
        gbl.lineno = -1
        error("Input file '%s' not found" % gbl.infile)


MMA.parse.parseFile(f)

# Finally, the mmaend files

for f in gbl.mmaEnd:
    fn = MMA.file.locFile(f, None)
    if not fn:
        warning("MmaEnd file '%s' not found/processed" % f)
    MMA.parse.parseFile(fn)

#################################################
# Just display the channel assignments (-c) and exit...

if gbl.chshow:
    print "\nFile '%s' parsed, but no MIDI file produced!" % gbl.infile
    print
    print "Tracks allocated:"
    k=gbl.tnames.keys()
    k.sort()
    max=0
    for a in k + gbl.deletedTracks:
        if len(a)>max:
            max = len(a)
    max+=1
    wrap=0
    for a in k:
        wrap += max
        if wrap>60:
            wrap = max
            print
        print " %-*s" %( max, a),
    print
    print
    if gbl.deletedTracks:
        print "Deleted Tracks:"
        wrap=0
        for a in gbl.deletedTracks:
            wrap += max
            if wrap>60:
                wrap=max
                print
            print " %-*s" %( max,a),
        print
        print
    print "Channel assignments:"
    for c, n in sorted(gbl.midiAssigns.items()):
        if n:
            wrap = 3
            print " %2s" % c,
            for nn in n:
                wrap += max
                if wrap>63:
                    print "\n   ",
                    wrap=max+3
                print "%-*s" % (max,nn),

            print
    print
    sys.exit(0)


####################################
# Dry run, no output

if gbl.noOutput:
    warning( "Input file parsed successfully. No midi file generated")
    sys.exit(0)


##############################
# Create the output (MIDI) file

gbl.lineno=-1    # disable line nums for error/warning


""" We fix the outPath now. This lets you set outpath in the song file.

    The filename "outfile" was created in paths, get a copy.

    It is either the input filename with '.mma' changed to '.mid' (or kar)
    OR if -f<FILE> was used then it's just <FILE>.

    If any of the following is true we skip inserting the outputpath into the
    filename:

        - if outfile starts with a '/'
        - if outPath was not set
        - if -f was used

    Next, the outPath is inserted into the filename. If outPath starts with
    a ".", "/" or "\ " then it is inserted at the start of the path;
    otherwise it is inserted before the filename portion.
"""

outfile = MMA.paths.outfile

if (not outfile.startswith('/')) and gbl.outPath and not gbl.outfile and not gbl.playFile:
    if gbl.outPath[0] in '.\\/':
        outfile = "%s/%s" % (gbl.outPath, outfile)
    else:
        head, tail = os.path.split(outfile)
        outfile = "%s/%s/%s" % (head, gbl.outPath, tail)

fileExist = os.path.exists(outfile)

""" Check if any pending midi events are still around. Mostly
    this will be a DRUM event which was assigned to the 'DRUM'
    track, but no DRUM track was used, just DRUM-xx tracks used.
"""

for n in gbl.tnames.values():
    if n.channel:
        n.clearPending()
        n.doMidiClear()
        n.doChannelReset()
        if n.riff:
            warning("%s has pending Riff(s)" % n.name)

""" Check all the tracks and find total number used. When
    initializing each track (class) we made an initial entry
    in the track at offset 0 for the track name, etc. So, if the
    track only has one entry we can safely skip the entire track.
"""

trackCount=1    # account for meta track

for n in sorted(gbl.mtrks.keys())[1:]:     # check all but 0 (meta)
    if len(gbl.mtrks[n].miditrk) > 1:
        trackCount += 1

if trackCount == 1: # only meta track
    if fileExist:
        print
    print "No data created. Did you remember to set a groove/sequence?"
    if fileExist:
        print "Existing file '%s' has not been modified." % outfile
    sys.exit(1)

lyric.leftovers()

if fileExist:
    print "Overwriting existing",
else:
    print "Creating new",
print "midi file (%s bars, %.2f min): '%s'" %  (gbl.barNum, gbl.totTime, outfile)

try:
    out = file(outfile, 'wb')
except IOError:
    error("Can't open file '%s' for writing" % outfile)

MMA.midi.writeTracks(out)
out.close()

if gbl.playFile:
    import MMA.player
    MMA.player.playMidi(outfile)

if gbl.debug:
    print "Completed processing file '%s'." % outfile



