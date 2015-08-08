package require Tcl 8.5
package require Tk

proc honeycomb {w letterpattern} {
    set basey 10
    foreach row $letterpattern {
	set basex 10
	set majoroffsety 0
	foreach letter $row {
	    set x [expr {$basex + 60}]
	    set y [expr {$basey + 50 + $majoroffsety}]
	    drawhex $w $x $y $letter 30 50
	    set majoroffsety [expr {50 - $majoroffsety}]
	    incr basex 90
	}
	incr basey 100
    }

    bind $w <space> { global t; .c delete "all"; incr t; honeycomb .c $letterpattern }

    return $w
}

namespace import tcl::mathop::?   ;# For convenience
proc drawhex {w x y ch dx dy} {
    global locked
    global tile
    global t

    set fillcolor white

    if {$ch in $locked} { set fillcolor lightyellow }
    if {$ch in [lindex $tile $t] }  { set fillcolor skyblue }

    $w create polygon \
	[- $x $dx] [- $y $dy] [+ $x $dx] [- $y $dy] [+ $x $dx $dx] $y \
	[+ $x $dx] [+ $y $dy] [- $x $dx] [+ $y $dy] [- $x $dx $dx] $y \
	-fill $fillcolor -outline black -tags [list hex$ch hull$ch] -width 3
#    $w create text $x $y -text $ch -fill black -tags [list hex$ch txt$ch] -font {Arial 16 bold}
    $w bind hex$ch <Enter> [list enterhex $w $ch]
    $w bind hex$ch <Leave> [list leavehex $w $ch]
}
 
# Callbacks for various bindings
proc enterhex {w ch} {
    global locked
    global tile

    if {$ch ni $locked && $ch ni $tile} {
	$w itemconfigure hull$ch -fill salmon -outline black
	$w itemconfigure txt$ch -fill black
    }
}
proc leavehex {w ch} {
    global locked
    global tile

    if {$ch ni $locked && $ch ni $tile} {
	$w itemconfigure hull$ch -fill white -outline black
    }
}

# Initial declarations of state variables
set locked { 7.0 7.3 7.4 7.8 6.8 6.7 }
set tile { {0.3 0.5 1.4} {1.3 1.5 2.4} {2.3 2.5 3.4} {3.3 3.5 3.4} {3.4 3.6 3.5} {4.4 4.6 5.5} }
set t 0

set gh 8
set gw 10
set letterpattern {}

proc init {width height} {
    global letterpattern

    for {set i 0} {$i < $height} {incr i} {
	set row {}
	for {set j 0} {$j < $width} {incr j} {
	    lappend row $i.$j
	}
    lappend letterpattern $row
    }
}
 
# Build the GUI
init $gw $gh
canvas .c
pack [honeycomb .c $letterpattern] -fill both -expand true
focus .c
# Usually don't use this, but it's ideal for this interaction pattern
tkwait window .c
exit
