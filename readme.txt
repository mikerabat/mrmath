// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ###################################################################
// ###################################################################
// #### certain MACOS contributions and testing William Cantrall
// ###################################################################

Installation:

open the mrMath.dpk file and compile it.
You may find quite a few examples in "Test\MathUtilsTests.dpr" - it's a unit test
program which shows the usage of the library nicely.

Also check out
https://github.com/mikerabat/mrmath
for a deeper description.

Platforms:

This library was built mainly for the windows platform and is 
compatible with Delphi2007 and later versions. 

The main problem for older Delphi version is that the compiler doesn't 
recognize some of the assembler instructions - the generic Delphi code
is compatible to at least Delphi 7. For older Delphi versions one would
have to exchange the assembler codes with "db" instructions which represent
the compiled code parts.

The library has been extendend to be compatible with MacOS. There is a new
unit test project - MathUtilsTestsMac.dpr - which can be used on this
operating system. However has a Mac can extend the project so the latest
things work as well.

FreePascal support has been added for both x86 and x64 windows platforms.
You find a unit test project in the "Test" folder called MathUtilsTestsFPC.lpr


Developer:

Rabatscher Michael - main windows and core development - contact via www.mrsoft.com
William Cantrall - special thanks for making the library MacOs compatible. 

// ###################################################################
changelog:

Date: 17.11.2015
* New random number engine which supports the creation of Mersenne Twister PRNG.
* Encapsulated the Delphi random generator into an object (thread safety)
* Access to the windows crypto random number generator.
* Various random distribution functions (gauss, exponential, poission, erlang)
* Untested but there: interface to the Intel RDRAND assembler instruction
* Added utility functions: greatest common divisor and least common multiple

Date: 8.8.2015
* Fixes for FPC 2.7.1 - Typhon IDE is now able to compile the code.

Date: 7.7.2015
* Optimizations for QR - decomposition: Assembler optimized multiplication used only there.
* New global subspace method: Independent component analysis. 

Date: 16.6.2015
* Reduced the number of threads for simple operations -> there the thread handling overhead is higher than the
  benefit.

Date: 15.6.2015
* New Median functions (row and column wise)
* New property to have vector like access to the matrix.

Date: 24.04.2015
* Project was not compilable under Delphi 2007 -> fixed exit statements
* The linear dependent vector testcase did not properly work since the eigenvector array was not initialized

Date: 17.03.2015
* Moved to github

Date: 26.01.2014
* Fixed SymEig initialization -> the wrong initial matrix dimension have been used.
* Revised unsymmetric eigenvalue calculation and fixed the implementaion.
* Added new testcases
* Fixed wrong destination matrix size in the normalization function.

Date: 29.10.2014
* Blocked and threaded QR decomposition
* Introduced full QR decomposition (from economysize QR to full economysize Q and R matrices)
* Threaded version of the full QR decomposition
* The resulting matrices are now always in the form of the calling type
* changed nnmf test case so no floating point exception occurs.

Date: 16.10.2014
New global subspace method: Non-Negative Matrix Transformation including 3 different
calculations schemes: divergence update, eukledian update and alternating least squares.
Removed an unnecessary exit in the threaded matrix multiplication routin and optimized
a bit the blocked matrix x vector multiplication.
Fix: On platforms with many cores (> 10) the threaded matrix operations may fail because the
break critera was met too early.

Date: 30.9.2014
Implemented a blocked QR decomposition -> outcome is the economysize QR decomposition of the input matrix
Fixed a few problems when loading constants. 
New transposed multiplication methods (used in the QR decomposition!)
A simple threaded version of QR decomposition (actually only the big multiplications are multithreaded)


Date: 11.7.2014
Minor: The Assign function does now also set the subwidth fields.
Added the utilities unit to the package.
Fixed some unused references.

Date: 10.7.2014
Fixed some x64 variance calculation issues (tested under Lazarus).
Made some data access functions public in the matrix class.
Tidy up work - removed some unreferenced units.

Date 6.7.2014
Implemented x64 compatibility for Freepascal. Removed a few unused units. 
-> The stack handling had to be completely rewritten for x64.
Sidenotes: Sometimes stackalignment is not garanted to be properly aligned so I had to introduce some alignment dummy variables.
FPC reserves extra space for parameters thus accessing the parameters instead of the registers leads to different 
results.

Date: 12.06.2014
Made a Lazarus compatible project - mrMath is now compatible with Freepascal for windows 32 bit!

Date: 12.06.2014
Removed some unnecessary block size setup variables and unit tests. Made the SetWidthHeight proceure public in the
matrix class.

Date: Donnerstag 6.2.2014
The prefetch instructions raised an external exception in Pentium 4 processors. I removed (commented them out) 
them since the performance gain was marginal anyway.

Date: Donnerstag 09. Jänner 2014 17:06
Fix: The implementation of the SVD in the matrix class didn't work for the case width > height.

Date: Samstag, 19. Oktober 2013	11:45:37
Many thanks to W. Cantrall who implemented the MAC OS support (Thread pooling and Unit tests).
Rewrote all the Thread pooling for windows as well.
Some minor changes on the asm interfaces: some integer params have been ported to nativeint for better large matrix support.

Date: Mittwoch, 14. August 2013 11:27:35
Added a new incremental PCA class.
Added testing routines for that.

Date: Mittwoch, 14. August 2013 11:12:03
Fixed problems with many core cpu's (e.g. 12 cores didn't work)
Moved some generic functions away from the asm routines to the SimpleMatrixOperations
Removed ASMConsts -> these constants are now in MatrixConst

Date: Donnerstag, 20. Dezember 2012 19:33:36
Fix: the element wise multiplication could fail with an AV since the mulpd asm does not support unaligned memory addresses.

Date: Donnerstag, 20. Dezember 2012 19:13:23
FixFix: The testing for zero was wrong for the previous fix.

Date: Donnerstag, 20. Dezember 2012 11:38:58
Fixed a bug reported from "sutetemp" -> the transposed matrix multiplication failed on resulting matrices of height=1

Date: Sonntag, 25. November 2012 18:03:44
Changed thread pool a bit: the number of threads is now set to the number of cpu's
-> the initialization now creates the threads (instead of the first attempt to use them)
Added new tests for very small matrices for the element wise norm functions
Removed unused variable in the block size setup.

Date: Freitag, 23. November 2012 09:36:47
Message:
Fixed a problem with the odd matrix element wise multiplication (wrong opcode for the last column)
Added test cases for that.

Date: Samstag, 29. September 2012 12:20:55
Fix: The calculation of a matrix determinant was wrong for uneven matrix widths. 
Used now the more optimal matrix copy function in the svd calculation.

Date: Freitag, 28. September 2012 12:30:14
Fixed a memory leak. Special thanks to Joshua Whiting for pointing me to that problem!

Revision: 17
Fixed Delphi2007 incompatibilities in LinearAlgebraicEquations
Fixed a few buffer overflows in the testing routines which had side effects to the threaded functions
Added a new new PolynomFit function to the NonLinearFit class.
Extended the matrix class test procedures.

Date: Sonntag, 05. August 2012 19:34:41
Maded a few functions in TDoubleMatrix virtual so they can be overridden in TThreadedMatrix (better support of a threaded IMatrix).
Fixed a problem in the thread pool. The manual sync event wasn't cleared which could lead to race conditions in threaded matrix operations.
TThreadedMatrix now supports threaded lin equation functions.

Date: Samstag, 04. August 2012 20:06:28
Fixed AV if the number of CPU's is bigger than 4 and the matrix multiplication is executed.

Date: Montag, 30. Juli 2012 09:37:21
Fixed a problem in the thread pool when using the thread pool more than one time. Sync could check against a wrong finished value.

Date: Montag, 30. Juli 2012 00:16:51
Introduced a  recursive LU decomposition algorithm.
Multithreaded LU decomposition.
Added row exchange ASM function.
Fixed a base functionality problem in the scale operations.
Extended persistence functionality.

Date: Freitag, 09. März 2012 13:54:07
Added the nonlinear fitting class to the project

Date: Mittwoch, 29. Februar 2012 09:16:00
Fix: IsSSE3Present on x64 platforms always returned false.

Date: Dienstag, 28. Februar 2012 16:52:02
Fixed a wrong assert in the ElementWiseMult asm functions.
Fixed problems on scaling/norming large matrices.
The matrix class now got a new method which allows cloning.
Added a new feature: Non Linear Fitting.

Date: Donnerstag, 15. Dezember 2011 09:24:21
Some minor adjustments.

Added a test for singularity in a matrix inversion.
Removed files which are not yet there.

Date: Donnerstag, 10. November 2011 21:51:28
Implementation of a fast Strassen Multiplication algorithm.
Added a PCA unit including test cases and images

Date: Samstag, 15. Oktober 2011 19:59:32
Message:
Init
