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

open the mrMath.dpk or the approriate FPC pendant in the packages directories and compile it.
You may find quite a few examples in "Test\MathUtilsTests.dpr" - it's a unit test
program which shows the usage of the library nicely.

Add the basepath to the delphi library paths.
Basically you can use the "Matrix.pas" unit in your project it encapsulates
all the matrix functions in a class.
Alternatively one can use the ThreadedMatrix.pas file which adds multithreaded
functionality to the base class.

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
Gustav Kaiser - special thanks for implementing the PLS algorithm.
Andrea Mauri - pointing out a few issues with FPC and conditional compilation and helped fixing them.

// ###################################################################
changelog:

Date 27.04.2020
* Implemented a new thread pool based on IO Completion ports (windows only)
* Moved the setup of the thread pool type away from the initialization section

Date 11.10.2019
* Kernel PCA with polynomial and Gauss Kernel
* First version of a more flexible Thread pool.

Date 22.8.2019
* Extended distance class with pairwise distance functions
* These distance functions are now available in tsne

Date 02.07.2019
* Created a Lazarus FPC package
* Renamed RowMajor to ColMajor since that was actually the intention

Date: 03.06.2019
* Added TSNE algorithm - still alpha state do not yet use.

Date: 15.05.2019
* Thanks To Andrea Mauri who pointed out a problem in the Determinant (LU Decomposition) -> in some cases there was an Access violation.

Date: 06.05.2019
* New Statistical methods: KS Test, FTest, Student T Test, ChiSquareTest from numerical recipies
* Histograms
* PDF Functions
* Beta, Gamma functions
* spearman Correlation
* extended Pearsons correlation (+ probability)

Date: 25.3.2019
* New normalization routine to normalize to zero mean/unit variance.
* New Sorting routines which are sligtly faster
* New special bet/gamma functions
* A few new statistical tests (starting) - StudentT 
* Some minor bugfixes to PCA - the eigenvalues where wrongly scaled.

Date: 07.02.2019
* Fixed 64 bit FMA routines on Unix based systems
* Fixed s problem in case the QR decomposition fails within a certain path of the SVD routine.

Date: 19.12.2018
* Renamed movntdq to movapd
* New logic for matrix mult -> better performance on some cpu's
* A few more memory optmizations.

Date: 16.11.2018
* Anonymous methods are now possible for the elementwise functions.

Date: 11.11.2018
* Revisited all 32bit asm routines and changed the calling convention to "register"/"assembler".
* AVX and FMA support needed a second check beside the CPU features -> the OS needed to be queried for that support too.
* Fixed a small push pop problem in avx min max calculations (edi, esi switched)

Date: 21.09.2018
* Started using register (Delphi) and assembler (FPC) calling convention for
  some functions.
* Possible problem fixed in asm row swap.

Date: 11.09.2018
* Convolution
* Changed 64 bit assembler api prologs for Unix based machines.

Date: 15.05.2018
* thanks to Andrea Mauri for pointing out the problems on FPC (lazarus) -> fixing some ifdefs and uppercase issues.

Date: 07.05.2018
* A few new SSE, AVX optimized Matrix-Vector sub/add routines

Date: 26.04.2018
* Optimzed windows thread pool -> Now I try to avoid that a thread runs on the same CPU that is calling it.

Date: 23.04.2018
* Fixed some stack issues to be more conform with the x64 ABI

Date: 19.04.2018
* Added distance class which implements L1, L2 (Euclid) and Mahalonobis distance
* Extended matrix class with "Append", "Resize" and updated QR factorization interface.
* Fixed a few things introduced with QR and some missing stack hickups (didn't surfuace though).

Date: 09.03.2018
* Added the IDE plugin to the svn repository.
* Extended the classes with class functions for easier access.

Date: 01.03.2018
* Added Fused Multiply and Add (FMA3) opcodes where appropriate.
* Removed a few warnings in FPC

Date: 27.02.2018
* Added Robust B-Spline class.

Date: 26.02.2018
* Added Expectation Maximation algorithm.
* Fixed a few things in the threaded LU decomp (parity was not var)

Date: 14.02.2018
* Added AVX support for FPC and Delphi (delphi64 not tested yet)
* Added AVX asm code to DB code project.

Date: 10.11.2017
* Spared a few cycles and memory accesses in the matrix multiplications

Date: 06.11.2017
* Fixed issues with Delphi 2007 (thanks to Zoran for pointing that out)

Date: 25.10.2017
* Extended NonLinear polynomfit by a few faster (but less robust) routines:
  -> linear regression is done in a fast loop.
  -> qr based least squares function that outperforms the svd routines in speed (but is less robust in 
     non nice behaving data)
* Polynom regression is now memory optimized for usage with the same x vector matrix more than once.
* matrix class with more inline functions.

Date: 06.10.2017
* Added a new neat function for linear regression that is faster in a loop.
  -> note this function does not depend on SVD so it may be not as robust as the svd version
* Moved the sqrt from the elementary vector norm from the asm routines.

Date: 18.07.2017
* First version of Linux support (FPC Typhoon64 6.1)
* Some minor optimizations

Date: 21.06.2017
* Minor improvements - the Fast DTW algorithm now implements a SSE version of the reduce by half
  routine.

Date: 09.06.2017
* Introduced a new base class that easily allows to change the matrix implementation used for
  PCA, CCA, PLS, ICA, NMF. (e.g. from single core to multithreaded)
* Fixed an inconvenience in the threaded LU decomposition on small matrices and updated the 
  invocation procedure.

Date: 30.05.2017
* ASM Version of the combined mean and variance implementation

Date: 29.05.2017
* Added base correlation functions: base correlation and covariance matrix
* Added Fast and standard Dynamic Time Warp algorithm to the library.
* Added mean and variance calculation to a matrix in one step.
* Added a new constructor type.

Date: 10.05.2017
* Added persistence tests for CCA and PLS

Date: 09.05.2017
* New global subspace method used for regression: Partial Least Squares
* Extended CCA to have a "project" function that allows to map inputs to predicted outputs
  (e.g. images to degree rotation)

Date: 21.04.2017
* Faster transposed matrix multiplication by reducing the steps in the inner loop
* one less register used

Date: 15.3.2017
* New least squares solver for overdetermined linear equation systems. (multithreaded and single threaded)
* Fixed a bug in the matrix diff function for row wise differentiation.

Date: 09.03.2017
* Added a simple "differentiate" function (diff between two neighbouring mtx elements)
* Fixed a few (non fatal) warnings in fpc

Date: 01.03.2017:
* Added cumulative matrix sum helper function (+assembler optimization)
* Added repeat matrix helper function

Date: 24.02.2017:
* A new matrix constructor to generate a linearily spaced vector.

Date: 20.02.2017:
* No "genericxyz" functions any more in the LinAlg functions.

Date: 18.02.2017:
* Added a progress functionality to SVD
* Changed threading in SVD: Throwing more threads at the plane rotations seems to help :)

Date: 17.02.2017:
* New assembler optimized multiplication methods - these are small upper lower triangulation multiplications used in QR and SVD decomposition.
 -> just a few ms faster now.
* the initial scaling in SVD does now also a unscaling at the end.
* removed unnecessary stack operations

Date: 09.02.2017
* Fixed som FPC issues - some constants weren't loaded via the [RIP + cMyConst] directive (delphi is
  "smarter" here)
* Moved constants to MatrixConst.pas

Date: 08.02.2017:
* Intruduction of assembler optimized vector Rotation routines -> amazing speedup in SVD
* Fixed some initialization routines - in rare cases it may happen that old initialized memory could
  be involved and produces Invalid Floating point exceptions (e.g. vector matrix mult where 0 is multiplied
  with NAN -> exception later on)
* New assembler optimized allocation (init to zero) function.
* Fixed a rare range check error in the UInt64 random number generator.


Date: 26.02.2017:
* Rank1Updates are now assembler optimized results in a slightly faster QR and SVD decomposition.

Date: 20.01.2017:
* Implemented lapacks SVD routine. 
* Blockwise and Multithreaded SVD
* Updated thread pool -> no thread suspend but rather signals are used (which seems to be faster)
* Splitted the linear algebra stuff in different units.
* New routines for matrix vector multiplication (standard pascal, x64 x86 asm).
* Fixed a problem in the aligned odd assembler scale and add routine.
* changed the base as how the threaded functions are using the pool - the current thread is now always 
  involved in calculations (spares one "thread weakup")
* Some enhancements on the random engine -> 64 bit (int64) values are now available
* Quite a few new threaded functions have been added

Date: 08.05.2016:
* Updated persistence model so the reader knows which type to read (needed for JSON reader).

Date: 19.04.2016:
* New persistence reader class TJSONReaderWriter. The library is now able to read and write JSON encoded objects.

Date: 29.3.2016:
* Some speed optimizations for the generic (Pascal) implementations: Whera applicable the pointer
  arithmetic was changed to array access (PConstDoubleArr)

Date: 28.3.2016
* Updated sym rank update in cholesky decomposition for row major matrix memory access.
* -> around 10% faster cholesky decomp.

Date: 23.2.2016
* New auxilary function to sort a matrix (column and row wise)
* Threaded version of sort
* New TakeOver routine that grabs the memory of another matrix (instead of copying it)

Date: 19.2.2016
* New blocked based implementation of the cholesky decomposition.
* Threaded cholesky version which makes use of threaded multiplication routines.
* Fix: a scale function checked for the wrong params (assertion was raised)
* Updated RDRAND calls for 32 and 64 bit - rdrand is now working.
* Fix: the hardware random number generator routine returned negative values as well.

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
