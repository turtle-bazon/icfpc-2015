package require Tcl 8.5
package require Tk
 
# How to make a honeycomb
proc honeycomb {w letterpattern} {
    canvas $w -width 500 -height 900
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
    return $w
}
 
namespace import tcl::mathop::?   ;# For convenience
# How to draw a single hexagon, centered at a particular point.
proc drawhex {w x y ch dx dy} {
    if {$ch eq ""} return          ;# Allow elision of cells (not used here)
    $w create polygon \
	[- $x $dx] [- $y $dy] [+ $x $dx] [- $y $dy] [+ $x $dx $dx] $y \
	[+ $x $dx] [+ $y $dy] [- $x $dx] [+ $y $dy] [- $x $dx $dx] $y \
	-fill yellow -outline black -tags [list hex$ch hull$ch] -width 3
    $w create text $x $y -text $ch -fill red -tags [list hex$ch txt$ch] \
	-font {Arial 16 bold}
    # Install bindings on items
    $w bind hex$ch <Enter> [list enterhex $w $ch]
    $w bind hex$ch <Leave> [list leavehex $w $ch]
    $w bind hex$ch <Button-1> [list dohex $w $ch]
    # Handle keyboard activity through canvas-level bindings
    bind $w [string toupper $ch] [list dokey $w $ch]
    bind $w [string tolower $ch] [list dokey $w $ch]
}
 
# Callbacks for various bindings
proc enterhex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	$w itemconfigure hull$ch -fill magenta
	$w itemconfigure txt$ch -fill black
    }
}
proc leavehex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	$w itemconfigure hull$ch -fill yellow
	$w itemconfigure txt$ch -fill red
    }
}
proc dohex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	lappend chosen $ch
	puts "chosen $ch"
    }
    if {[llength $chosen] >= 5} {
	destroy $w
    }
}
proc dokey {w ch} {
    enterhex $w $ch
    dohex $w $ch
}
 
# Initial declarations of state variables
set chosen {}
set letterpattern {
    {0.0 0.1 0.2 0.3 0.4}
    {1.0 1.1 1.2 1.3 1.4}
    {2.0 2.1 2.2 2.3 2.4}
    {3.0 3.1 3.2 3.3 3.4}
    {4.0 4.1 4.2 4.3 4.4}
    {5.0 5.1 5.2 5.3 5.4}
    {6.0 6.1 6.2 6.3 6.4}
    {7.0 7.1 7.2 7.3 7.4}
}
 
# Build the GUI
pack [honeycomb .c $letterpattern]
focus .c
# Usually don't use this, but it's ideal for this interaction pattern
tkwait window .c
puts "overall list of characters: $chosen"
exit
