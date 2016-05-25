## \brief Common methods and procs for snit
package require TclOO


namespace eval ::snit {

## \brief A mixin object that defines "public variable" behaviour of Itcl.
# 
# Defines methods [configure] and [cget], which are used to set and get the 
# values of public variables. This is done by calls like
# [obj configure -varname1 value -varname2 value] and [obj cget -varname] 
# respectively (with a dash in front of the variable name). The configuration
# or cget occurs on an object variable name without the dash.
# 
# Constructors can also be called with the variables, and usually there is a 
# traditional call to [eval configure $args] in every constructor of an Itcl
# object. Since this has been repetitive all the time, we can now let
# the mixin object do it automatically.
::oo::class create snitmethods {
    
    ## \brief setting -var value pairs
    method configure {args} {
        set class [info obj class [self]]
        set ns [info obj namespace [self]]
        
        if {[llength $args] == 0} {
            set opts [namespace eval $ns [list array names options]]
            return [$class optionDefaults [self] {*}$opts]
        } elseif {[llength $args] == 1} {
            set opt $args
            if {[string index $opt 0] ne "-" || [string is upper [string index $opt 1]]} {
                throw SNIT_OPTION_WRONG_NAMESPEC "Error in \"option $opt...\", badly named option \"$opt\""
            }
            return {*}[$class optionDefaults [self] $opt]
        }
        
        if {[llength $args] % 2 != 0
              || [lsearch [lmap _x [dict keys $args] {string comp -l 1 $_x -}] 1] >= 0 } {
            error "not var/value pairs: $args"
        }
        
        foreach {opt val} $args {
            if {[string index $opt 0] ne "-" \
                    || [string is upper [string index $opt 1]]} {
                throw SNIT_OPTION_WRONG_NAMESPEC "Error in \"option $opt...\", badly named option \"$opt\""
            }
            
            set var [string range $opt 1 end]
            if {$var in [info class variables $class] } {
                my variable $var
                set $var $val
            } elseif {[namespace eval $ns [list info exists options($opt)]]} {
                namespace eval $ns [list set options($opt) $val]
            } else {
                throw SNIT_OPTION_UNDEFINED "Error in \"[self] configure $opt...\": No such option $opt"
            }
        }
    }
    
    ## \brief get the value of -variable
    method cget {opt} {
        if {[string index $opt 0] ne "-" \
                || [string is upper [string index $opt 1]]} {
            throw SNIT_OPTION_WRONG_NAMESPEC "Error in \"option $opt...\", badly named option \"$opt\""
        }
        
        set var [string range $opt 1 end]
        set class [info obj class [self]]
        set ns [info obj namespace [self]]
        if {$var in [info class variables $class] } {
            my variable $var
            return [set $var]
        } elseif {[namespace eval $ns [list info exists options($opt)]]} {
            return {*}[namespace eval $ns [list set options($opt)]]
        }
        throw SNIT_OPTION_UNDEFINED "Error in \"[self] cget $opt...\": No such option $opt"
    }
    
    method info {args} {
        my variable self
        switch -- [lindex $args 0] {
        type {
            return [info obj class [self]]
        }
        }
    }
}

} ;# namespace snit

package provide snit::common 3.0.0
