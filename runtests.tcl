## runtests.tcl (created by Tloona here)
set libDir [file dirname [info script]]
if {[lsearch $auto_path $libDir] < 0} {
    lappend auto_path $libDir
}

package require tcltest
package require snit 3.0.0

tcltest::configure -verbose {pass error line} \
    -testdir [file join [file dirname [file normalize [info script]]] test]
tcltest::configure -file {
    methods.test
}

tcltest::runAllTests
