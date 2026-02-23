# mrMath — High-performance Delphi/Free Pascal matrix & numerical library

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE.txt) [![GitHub Repo](https://img.shields.io/badge/repo-mikerabat%2Fmrmath-lightgrey.svg)](https://github.com/mikerabat/mrmath)

mrMath is a high-performance matrix and numerical computing library written in Object Pascal. It targets Delphi and Free Pascal (FPC) users and provides a wide range of linear algebra routines, statistical methods, dimensionality-reduction algorithms and optimized low-level kernels (ASM / AVX / FMA) for maximum performance.

Key highlights
- Full-featured linear algebra: LU, QR, Cholesky, SVD and solvers.
- Dimensionality reduction & ML primitives: PCA, Incremental PCA, Kernel PCA, t-SNE (Barnes-Hut), ICA, NNMF, PLS.
- Statistics and signal-processing tools: correlation, wavelets, SSA, rolling medians, fitting.
- Highly optimized kernels: assembly/AVX/FMA/x64 variants where applicable.
- Multi-threading support: thread pools and threaded matrix operations.
- Designed to be usable with Delphi (various versions) and Free Pascal (FPC) on Windows, Linux and macOS.
- Licensed under Apache License 2.0 — friendly for reuse.

Repository layout (selected)
- Matrix.pas, SimpleMatrixOperations.pas, ThreadedMatrixOperations.pas — core matrix API and operations.
- DblMatrix.pas, CplxMatrix.pas - core specialized matrices. 
- LinAlgLU.pas, LinAlgQR.pas, LinAlgSVD.pas, LinAlgCholesky.pas — linear algebra modules.
- PCA.pas, KernelPCA.pas, IncrementalPCA.pas, tSNE.pas — dimensionality reduction.
- ASM*/, AVX*/, FMA* files — optimized implementations for various instruction sets.
- src/ - contains all the library files - add this directory to the library path.
- Examples/ — example projects demonstrating usage.
- Test/ — tests and small test projects.
- Packages/ — IDE/packaging scaffolding (Delphi packages may be present).
- LICENSE.txt — Apache-2.0 license.

Quickstart

Prerequisites
- Free Pascal (FPC) — recommended recent stable release (e.g. 3.x).
- or Delphi (supported versions vary; see notes below).
- For best performance on modern CPUs, use a compiler/target that allows linking to the AVX/FMA optimized units (x64 builds usually).
- Optionally: Lazarus IDE for easier project management with FPC.

Clone the repo
```
git clone https://github.com/mikerabat/mrmath.git
cd mrmath
```

Using with Free Pascal (FPC)
- Add the mrMath\src source directory to your project's search path (or place units in your project folder).
- You can compile the example projects found in Examples/ with fpc or open them with Lazarus:
  - Using fpc (example):
    ```
    fpc -Fu/path/to/mrmath -Mdelphi Examples/YourExample.pas
    ```
    Adjust -Fu path and compiler mode as needed.

Using with Delphi
- Add the source path to the general library/search path. 
- Open and compile the _mrMath_ package
- If you want to have IDE support for the IMatrix and TDoubleMatrix types compile and install the _mrMathIDE_ package too.
- Add required units to the uses clause (for instance "Matrix, LinAlgSVD, PCA" etc.).

Simple usage examples

Create and multiply matrices (conceptual)

```pascal
uses Matrix, PCA;

var mtx1, mtx2 : TDoubleMatrix;
	mtxRes : TDoubleMatrix;
begin
     RandSeed := 301;
     mtx1 := TDoubleMatrix.CreateRand(100, 100);
     mtx2 := TDoubleMatrix.CreateRand(100, 100, raSystem, 301);
     try
		mtxRes := mtx1.Mult(mtx2);
		
		// write the first column
		for i := 0 to mtxRes.Height do
			Writeln(mtxRes[0, i]);
			
		mtxRes.Free;
     finally
            mtx1.Free;
            mtx2.Free;
     end;
end;

```
The _examples_ folder contains a few in depth examples - check out the readme there.

Building & testing notes
- There are unit tests and example projects stored under Test/ and Examples/. They may require small adjustments for compiler modes or search paths depending on your environment.
- There is no mandatory build system enforced; projects are typically compiled with the native compiler/IDE.
- Recommended: run examples with a small dataset first to verify runtime environment, then scale up.

Supported compilers/platforms
- Free Pascal Compiler (FPC) — Linux, macOS, Windows (x86/x64) (Tested basically with the CodeTyphon IDE).
- Embarcadero Delphi 2010 to Delphi 13.0
- The codebase contains platform-specific files (winRandomGen.pas, MacOsRandomGen.pas, linuxthrpool.pas, etc.) and CPU detection utilities (CPUFeatures.pas, mrMath_CPU.inc).

Performance & tuning
- mrMath ships with multiple optimized implementations for critical kernels. The library chooses the appropriate implementation based on platform and CPU features at runtime/compile-time.
- For maximum speed:
  - Use x64 builds where AVX/FMA implementations are available.
  - Ensure CPU feature detection works (CPUFeatures.pas) in your environment.
  - Consider enabling any project-specific compiler optimizations.
  - Optimal blocksizes are dependent on the actual CPU - a good preset is used here though.

Documentation
- API is documented inline in many units. Example projects are a practical source of usage patterns.
- If you need specific API documentation, open the unit files (e.g., Matrix.pas, PCA.pas) and inspect the exported types and documented methods.
- The project wiki provides some insights too.

Contributing
Contributions are welcome. Suggested ways to contribute:
- Open an issue describing the bug/feature request.
- Fork the repo, make changes in a topic branch and submit a pull request.
- Provide tests or examples for new functionality.
- Follow these guidelines:
  - Keep changes modular and well-documented.
  - Include compiler/version notes if you introduce new conditional compilation.
  - Run existing examples/tests and note any platform-specific caveats.

Suggested improvements you can help with
- Add a modern README.md (this file) and richer documentation.
- Add CI to build and run tests on FPC (and Delphi if possible).
- Provide packaged releases and a CHANGELOG.
- Add unit test automation for core algorithms.

Issue tracker & support
- Please use GitHub Issues for bug reports and feature requests: https://github.com/mikerabat/mrmath/issues
- For questions or discussion, open an issue and tag it appropriately.

License
- mrMath is distributed under the Apache License 2.0. See LICENSE.txt for details.

Acknowledgements & references
- This project has evolved with contributions and testing on multiple platforms and compilers. Many optimizations are low-level (assembly and SIMD) and targeted to performance-sensitive workloads.

Contact
- Repo: https://github.com/mikerabat/mrmath

Notes and troubleshooting
- The project contains many platform- and compiler-specific conditionals; if you hit compile errors, check the top of the unit that failed for defines and the mrMath_CPU.inc. When reporting issues, include:
  - Compiler name and version (FPC/Delphi).
  - Target platform/architecture (Windows/Linux/macOS x86/x64).
  - Exact error text and the unit/line numbers.
