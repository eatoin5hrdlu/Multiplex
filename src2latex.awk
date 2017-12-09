#   File    : src2latex
#   Author  : Al
#   SCCS    : @(#)91/06/06 src2tex.awk  1.2
#   Purpose : awk script to convert pl file to tex file.

# Convert /** into \endtexcode and **/ into \texcode{}.  If people
# might have different begin and end terminators for code sections of
# tex files, perhaps /** and **/ should simple be taken out but not
# converted to anything, which would also simplify the script.
#
# Note that only lines with nothing but /** or **/ (not followed or preceeded
# by spaces) are consumed by the script.
#
# All lines preceeding the first /** are ignored (eg. sccs header, initial
# comments, etc).
#
# The script does error checking to make sure the delimiters match.
#
# Use the following to generate tex from a pl file:
#
#   awk -f src2tex.awk tex.pl


BEGIN {
    header=1
    codestart=0
    code=0
    linecnt=0
    error=0

    begtext="/**"
    endtext="**/"
}

{
    linecnt++
}

{
    if ($0 == begtext) {
        if (header) {
            header=0
            next
        } else if (! code) {
            print "** Line " linecnt ": Closing '" endtext "' missing for previous '" begtext "'."
            error=1
            exit 1
        } else if (! codestart) {
            print "\\end{verbatim}"
        }
        codestart=0
        code=0
        next
    }
}

{
    if ($0 == endtext) {
        if (header || code || codestart) {
            print "** Line " linecnt ": Opening '" begtext "' missing for '" endtext "'."
            error=1
            exit 1
        } else {
            # don't print \texcode{} yet; it might be last endtext in file
            codestart=1
            code=1
            next
        }
    }
}

{
    if (header) {
        next
    } else if (codestart) {
        print "\\begin{verbatim}"
        codestart=0
    }
    print
}

END {
    if (! error) {
        if (code && ! codestart) {
            print "\\end{verbatim}"
        } else if (! codestart) {
            print "** Line " linecnt ": Final closing '" endtext "' is missing."
            exit 1
        }
    }
}
