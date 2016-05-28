ApplicationName : snit3

==

Description :

Differences from snit2

*) install vs. variable assignment
In snit2 and snit 1 it is possible to create components by simply assigning variables in the type/widget method 
definitions, and then delegate options or methods to these components. In snit3 this is not possible. Components 
*must* be installed using [install] or [installhull] respectively, otherwise they are not recognized for method
or option delegation

==

Release 3.0.0 : Date 2016/05/23 :
* initial release