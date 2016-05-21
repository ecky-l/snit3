## snit3test.tcl (created by Tloona here)
set libDir [file dirname [info script]]
if {[lsearch $auto_path $libDir] < 0} {
    lappend auto_path $libDir
}

package require snit 3.0.0

::snit::type a {
    
    variable prop x
    
    constructor {args} {
        puts [self],$self,$args,$prop
    }
    
    method jaja {selfi args} {
        puts hahaha
    }
}

snit::type c {
    variable bulla 45
    
}

#oo::class create b {
#    constructor {args} {
#        puts yay
#    }
#}
