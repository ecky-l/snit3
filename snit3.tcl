## snit3.tcl (created by Tloona here)
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
        if {[llength $args] % 2 != 0
              || [lsearch [lmap _x [dict keys $args] {string comp -l 1 $_x -}] 1] >= 0 } {
            error "not var/value pairs: $args"
        }
        foreach {var val} $args {
            set var [string range $var 1 end]
            if {[string is upper [string index $var 0]]} {
                error "$var seems to be a private variable"
            }
            my variable $var
            set $var $val
        }
    }
    
    ## \brief get the value of -variable
    method cget {var} {
        if {[string compare -length 1 $var -] != 0} {
            error "Usage: obj cget -var"
        }
        set var [string range $var 1 end]
        if {[string is upper [string index $var 0]]} {
            error "$var seems to be a private variable"
        }
        my variable $var
        return [set $var]
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
    variable _OptionDefaults
    variable _SetGet
    
    ## \brief Installs handlers for oo::define before creating the class
    constructor {args} {
        set _VarDefaults {}
        set _OptionDefaults {}
        set _SetGet {}
        
        ::oo::define [self] variable self
        ::oo::define [self] variable options
        ::oo::define [self] mixin ::snit::snitmethods
        
        set ns [self namespace]::define
        foreach {cmd} [lmap x [info commands ::oo::define::*] {namespace tail $x}] {
            interp alias {} ${ns}::$cmd {} [self] $cmd
        }
        namespace eval $ns {*}$args 
    }
    
    ## \brief install variable defaults in case there is no
    method new {args} {
        set obj [next {*}$args]
        my InstallVars $obj [self] {*}[info class variables [self]]
        return $obj
    }
    
    ## \brief create named or local objects 
    method create {args} {
        set obj [next {*}$args]
        my InstallVars $obj [self] {*}[info class variables [self]]
        return $obj
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
    
    method InstallOptions {obj cls args} {
        set ns [info obj namespace $obj]
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
::oo::define ::snit::type method constructor {args} {
    append cbody apply " \{ " 
    append cbody [info cl definition [self class] InstallVars] \n " \}"
    append cbody " " {[self] [self class] {*}[info class variables [self class]]} 
    append cbody [lindex $args 1]
    ::oo::define [self] constructor [lindex $args 0] $cbody
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
::oo::define ::snit::type method option {name args} {
    
}

::oo::objdefine ::snit::type method unknown {clName args} {
    if {[llength $args] > 1} {
        error "usage ::snit::type <name> <script>"
    }
    uplevel ::snit::type create $clName $args
}

} ;# namespace ::snit


package provide snit 3.0.0
