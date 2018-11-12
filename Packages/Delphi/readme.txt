IDE Debugger Viewer for the matrix classes in mrMath
-----------------------------------------------------

This package provides an IDE package that can be used with
Delphi2010 and later versions. It provides a debug viewer
for the TDoubleMatrix, IMatrix and TThreadedMatrix classes.

Features are:
-> Evaluating matrices up to 65536x65536
-> Changing values
-> Colored visualization of selected submatrices.
-> Configurable appearance.


Per default the grid shows up to 1000x1000 matrices. If that is not
enough one can double click on the appearing '...' cells which then
doubles the appearance. Another double click will then show the maximum
up to 65536.

A special thanks to Boian Mitov's Bitmap debug visualizator on which this 
matrix debugger plugin is based on.

Installation:
---------------

* Setup the search paths for the mrMath library.
* Open the mrMath.dpk file - compile and install.
* Open the mrMathIDE.dpk file - compile and install. Thats it.


Unistallation:
---------------

At least for Delphi2010:
* Open Delphi
* Start -> Install Packages
* Locate mrMath IDE plugin
* Remove that package