## \brief the snit::type class
package require TclOO

namespace eval ::snit {
namespace export type

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
    
    ## \brief inspect snit objects
    method info {args} {
        my variable self
        switch -- [lindex $args 0] {
        type {
            return [info obj class [self]]
        }
        }
    }
    
    ## \brief The install method for components
    method install {args} {
        my variable self
        puts muahaha,$args
    }
    
    ## \brief default constructor
    method Construct {args} {
        if {[llength $args] > 0} {
            my configure {*}$args
        }
        
        if {[self next] != {}} {
            next Construct {*}$args
        }
    }
}


## \brief A meta class for default variable assignment and inheritance
#
# When defining classes, it is almost always very convenient to assigns 
# default values, like 
# 
# 'variable varname value'
# 'variabel varname {}'
#
# These default values should be assigned during object construction, so
# that they are available in every method if not defined to be something 
# else and it is not always necessary to check for existence before using
# the variables.
# Additionally the variables and defaults shall be installed automatically
# in derived classes (but not in mixins)
::oo::class create type {
    superclass ::oo::class
    
    variable _VarDefaults
    variable _Options
    variable _SetGet
    
    ## \brief Installs handlers for oo::define before creating the class
    constructor {args} {
        set _VarDefaults {}
        set _Options {}
        set _SetGet {}
        
        ::oo::define [self] variable self
        ::oo::define [self] variable options
        ::oo::define [self] mixin ::snit::snitmethods
        
        set ns [self namespace]::define
        foreach {cmd} [lmap x [info commands ::oo::define::*] {namespace tail $x}] {
            interp alias {} ${ns}::$cmd {} [self] $cmd
        }
        
        set cmdList { option delegate }
        foreach {cmd} $cmdList {
            interp alias {} ${ns}::$cmd {} [self] $cmd
        }
        
        namespace eval $ns {*}$args 
    }
    
    ## \brief install variable defaults in case there is no
    method new {args} {
        set obj [next {*}$args]
        try {
            my InstallVars $obj [self] {*}[info class variables [self]]
            my InstallOptions $obj [self] {*}[dict keys $_Options]
            my InstallProcs $obj
            ::oo::objdefine $obj export Construct
            $obj Construct {*}$args
            ::oo::objdefine $obj unexport Construct
        } trap {} {err errOpts} {
            $obj destroy
            throw [dict get $errOpts -errorcode] $err
        }
        return $obj
    }
    
    ## \brief create named or local objects 
    method create {name args} {
        set obj [next $name {*}$args]
        try {
            my InstallVars $obj [self] {*}[info class variables [self]]
            my InstallOptions $obj [self] {*}[dict keys $_Options]
            my InstallProcs $obj
            ::oo::objdefine $obj export Construct
            $obj Construct {*}$args
            ::oo::objdefine $obj unexport Construct
        } trap {} {err errOpts} {
            $obj destroy
            throw [dict get $errOpts -errorcode] $err
        }
        return $obj
    }
    
    ## \brief unknown dispatches to create
    method unknown {args} {
        uplevel [self] create $args
    }
    
    ## \brief Checks whether there is a default value.
    #
    # If there is one, returns true and sets the value in valPtr
    # Otherwise leaves valPtr as it is and returns false.
    method varDefault {var valPtr} {
        upvar $valPtr val
        if {[dict exists $_VarDefaults $var]} {
            set val [dict get $_VarDefaults $var]
            return 1
        }
        return 0
    }
    
    ## \brief return the default, resource and class for options
    method optionDefaults {obj args} {
        lmap x $args {
            concat $x [dict get $_Options $x resource] \
                [dict get $_Options $x class] \
                    [dict get $_Options $x default] \
                        [$obj cget $x]
        }
    }
    
    ## \brief Installs variables from the args list in an object obj.
    method InstallVars {obj cls args} {
        set ov [info obj vars $obj]
        set ns [info obj namespace $obj]
        lmap v [lmap x $args {expr {($x in $ov) ? [continue] : $x}}] {
            if {[$cls varDefault $v val]} {
                namespace eval $ns [list variable $v [lindex $val 0]]
            } else {
                namespace eval $ns [list variable $v]
            }
        }
        namespace eval $ns [list variable self $obj]
    }
    
    ## \brief Installs the options for 
    method InstallOptions {obj cls args} {
        set ns [info obj namespace $obj]
        namespace eval $ns {
            if {![array exists options]} {
                array set options {}
            }
        }
        foreach {k} $args {
            if {![namespace eval $ns [list info exists options($k)]]} {
                namespace eval $ns [list set options($k) [dict get $_Options $k default]]
            }
        }
    }
    
    ## \brief install default procs, such as "install"
    method InstallProcs {obj} {
        set ns [info object namespace $obj]
        interp alias {} ${ns}::install {} $obj install
    }
    
    ## \brief Construct an object.
    # 
    # This is redefined as the real constructor via the corresponding constructor
    # keyword. The trick is that before running the constructor, the options and
    # variables must be installed. Therefore this one is called after installing
    # options and variables in the create and new method.
    method Construct {obj args} {
        if {[llength $args] > 0} {
            $obj configure {*}$args
        }
    }
    
} ;# class snit::type

foreach {cmd} [lmap x [info commands ::oo::define::*] {namespace tail $x}] {
    ::oo::define ::snit::type method $cmd {args} [concat {::oo::define [self]} $cmd {{*}$args}]
}

## \brief Defines the constructor and installs variables
#
# Prepend some code in front of the constructor body, which takes the vars from the class 
# definition and installs the corresponding defaults into the newly created object. If a 
# constructor is defined, it needs access to the variables and defaults. Prepend code to 
# install the variables in front of the constructor body, so that the variable defaults 
# are installed first, before anything else. 
::oo::define ::snit::type method constructor {argsList body} {
    ::oo::define [self] method Construct [concat obj $argsList] $body
}

## \brief The Variable with default command.
#
# Is executed with a definition script after [create] (from the constructor) or with calls 
# to oo::define <cls> (variable). Arranges for the default to be installed in all existing 
# or new instances of this class. For private and protected variables there is additional 
# support for automatic getter and setter generation. If one or both of the switches {-set, -get} 
# are in the arguments after the value, methods {"setVarname", "getVarname"} are created. The 
# name is constructed from the varname (uppercase first letter for protected, underscore _ 
# for private). This happens only if there are no methods of the same name already defined.
::oo::define ::snit::type method variable {args} {
    ::oo::define [self] variable [lindex $args 0]
    if {[llength $args] >= 2} {
        dict set _VarDefaults [lindex $args 0] [lrange $args 1 end]
    }
    
    # install getters and setters for private/protected variables
    set vn [string index [lindex $args 0] 0]
    if {[string match $vn _] || [string is upper $vn] 
            && [llength $args] >=3} {
        set rem [lrange $args 2 end]
        set varName [lindex $args 0]
        if {[lsearch $rem -get] >= 0 && 
                [lsearch [info cl methods [self]] get[set varName]] < 0} {
            ::oo::define [self] method \
                get[set varName] {} " return \$$varName "
        }
        if {[lsearch $rem -set] >= 0 &&
                [lsearch [info cl methods [self]] set[set varName]] < 0} {
            ::oo::define [self] method \
                set[set varName] {value} " set $varName \$value "
        }
    }
    
    lmap o [info class inst [self]] {
        my InstallVars $o [self] [lindex $args 0]
    }
    return
}

## \brief The method command that is used while the type is constructed
::oo::define ::snit::type method method {mName argsList mBody} {
    if {[lsearch $argsList self] >= 0} {
        throw SNIT_METHOD_WRONG_ARG "method $mName's arglist may not contain \"self\" explicitly"
    }
    if {[lsearch $argsList type] >= 0} {
        throw SNIT_METHOD_WRONG_ARG "method $mName's arglist may not contain \"type\" explicitly"
    }
    ::oo::define [self] method $mName $argsList $mBody
}

## \brief The options
::oo::define ::snit::type method option {namespec args} {
    set name $namespec
    set resource [string range $name 1 end]
    set class [string tou $resource 0]
    if {[llength $namespec] == 3} {
        lassign $namespec name resource class
    } elseif {[llength $namespec] == 2} {
        lassign $namespec name resource
        set class [string tou $resource 0]
    } elseif {[llength $namespec] != 1} {
        throw SNIT_OPTION_WRONG_NAMESPEC "option namespec must have one or three components"
    }
    
    if {[string index $name 0] ne "-" \
            || [string is upper [string index $name 1]] \
                || [regexp {[ \t\n]} $name]} {
        throw SNIT_OPTION_WRONG_NAMESPEC "Error in \"option $name...\", badly named option \"$name\""
    }
    
    # check trivial option defaults
    if {[llength $args] == 0} {
        dict set _Options $name [list resource $resource class $class default {}]
        return
    } elseif {[llength $args] == 1 && [string index $args 0] ne "-"} {
        dict set _Options $name [list resource $resource class $class default $args]
        return
    }
    
    # parse remaining arguments
    dict set _Options $name [list resource $resource class $class]
    set validArgs { -default -readonly -type -cgetmethod -configuremethod -validatemethod }
    foreach {a v} $args {
        if {$a ni $validArgs} {
            throw SNIT_OPTION_WRONG_ARGS "Error in \"option $name...\", wrong arg \"$a\""
        }
        dict set _Options $name [string range $a 1 end] $v
    }
    
    lmap o [info class inst [self]] {my InstallOptions $o [self] $name}
    return
}

## \brief delegate methods or options to components
::oo::define ::snit::type method delegate {what namespec args} {
    switch -- $what {
    method {
        
    }
    option {
    }
    typemethod {
    }
    default {
        throw SNIT_DELEGATE_WRONG_TYPE "Error in \"delegate $what $namespec...\", \"${what}\"?"
    }
    }
}

## \brief The unknown method dispatches to create
::oo::objdefine ::snit::type method unknown {clName args} {
    if {[llength $args] > 1} {
        error "usage ::snit::type <name> <script>"
    }
    uplevel ::snit::type create $clName $args
}

## \brief snit::method to redefine methods, constructors, destructors etc.
#
# A wrapper around the specialized methods in snit::type definition (see above),
# but must be applied a little different than just by method call.
proc method {class name args} {
    set ns [info object namespace $class]::define
    switch -- $name {
    constructor - destructor {
        namespace eval $ns $name $args
    }
    default {
        namespace eval $ns method $name $args
    }
    }
    return
}

} ;# namespace snit

package provide snit::type 3.0.0
