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
    
    delegate method this to that as bla
    delegate method * to bluna
    
    delegate option -bolla to that
    delegate option -fck to that as -muh
    
    constructor {args} {
        puts holla,$self,$args
        install that using ::c %AUTO%
        install bluna using ::c %AUTO%
    }
    
    method jaja {selfi args} {
        puts hahaha,$options(-opt)
    }
    
    proc ttt {args} {
        puts ttt
    }
}


snit::type c {
    variable bulla 45
    option -bolla murps
    option -muh 42
    
    method bla {args} {
        puts yay,$self,$args
    }
    method blo {args} {
        puts blo_in,$self,$args
    }
}

snit::type papers {
    option -akcflag 1
}

snit::type dog {
    option -color black
    delegate option -akc to papers as -akcflag
    
    constructor {args} {
        install papers using papers %AUTO%
        #set papers [papers create $self.papers]
    }

    destructor {
        catch {$self.papers destroy}
    }
}