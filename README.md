# snit3
snit successor in TclOO

This is a rather unfinished work in progress to implement the Tcl object system 
[snit](https://core.tcl.tk/tcllib/doc/trunk/embedded/www/tcllib/files/modules/snit/snit.html) in TclOO. 
Currently its more a POC and far away from a release...

The objective is to have snit combined with the speed and capabilities of TclOO, i.e. a snit::type that can
inherit from or mixin ordinary TclOO classes and objects, but still works with all the snit code that is already
written out there.

## Differences from snit2

### install vs. variable assignment

In snit2 and snit 1 it is possible to create components by simply assigning variables in the type/widget method 
definitions, and then delegate options or methods to these components. In snit3 this is not possible. Components 
*must* be installed using [install] or [installhull] respectively, otherwise they are not recognized for method
or option delegation
