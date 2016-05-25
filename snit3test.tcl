## snit3test.tcl (created by Tloona here)
set libDir [file dirname [info script]]
if {[lsearch $auto_path $libDir] < 0} {
    lappend auto_path $libDir
}

package require snit 3.0.0


::snit::type a {
    variable prop x
    variable bah c
    
    option -opt -default ha -validatemethod moo
    option -blubb moo
    
    delegate method fuck to what
    
    constructor {args} {
        puts [self],$self,$args,$prop
        install x y z
    }
    
    method jaja {selfi args} {
        puts hahaha,$options(-opt)
        install a b c
    }
    
    proc ttt {args} {
        puts ttt
    }
}


snit::type c {
    variable bulla 45
    
}

oo::class create b {
    constructor {args} {
        puts yay
    }
}
