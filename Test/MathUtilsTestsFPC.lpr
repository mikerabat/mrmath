program MathUtilsTestsFPC;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  TestSimpleMatrixOperations in 'TestSimpleMatrixOperations.pas',
  TestLineEquations in 'TestLineEquations.pas',
  TestEigensystems in 'TestEigensystems.pas',
  BaseMatrixTestCase in 'BaseMatrixTestCase.pas',
  TestMatrixClass in 'TestMatrixClass.pas',
  ASMMatrixAddSubOperations in '..\ASMMatrixAddSubOperations.pas',
  ASMMatrixAddSubOperationsx64 in '..\ASMMatrixAddSubOperationsx64.pas',
  ASMMatrixElementwiseMultOperations in '..\ASMMatrixElementwiseMultOperations.pas',
  ASMMatrixElementwiseMultOperationsx64 in '..\ASMMatrixElementwiseMultOperationsx64.pas',
  ASMMatrixMeanOperations in '..\ASMMatrixMeanOperations.pas',
  ASMMatrixMeanOperationsx64 in '..\ASMMatrixMeanOperationsx64.pas',
  ASMMatrixMinMaxOperations in '..\ASMMatrixMinMaxOperations.pas',
  ASMMatrixMinMaxOperationsx64 in '..\ASMMatrixMinMaxOperationsx64.pas',
  ASMMatrixMultOperations in '..\ASMMatrixMultOperations.pas',
  ASMMatrixMultOperationsx64 in '..\ASMMatrixMultOperationsx64.pas',
  ASMMatrixMultTransposedOperations in '..\ASMMatrixMultTransposedOperations.pas',
  ASMMatrixMultTransposedOperationsx64 in '..\ASMMatrixMultTransposedOperationsx64.pas',
  ASMMatrixNormOperations in '..\ASMMatrixNormOperations.pas',
  ASMMatrixNormOperationsx64 in '..\ASMMatrixNormOperationsx64.pas',
  ASMMatrixOperations in '..\ASMMatrixOperations.pas',
  ASMMatrixScaleOperations in '..\ASMMatrixScaleOperations.pas',
  ASMMatrixScaleOperationsx64 in '..\ASMMatrixScaleOperationsx64.pas',
  ASMMatrixSqrtOperations in '..\ASMMatrixSqrtOperations.pas',
  ASMMatrixSqrtOperationsx64 in '..\ASMMatrixSqrtOperationsx64.pas',
  ASMMatrixSumOperations in '..\ASMMatrixSumOperations.pas',
  ASMMatrixSumOperationsx64 in '..\ASMMatrixSumOperationsx64.pas',
  ASMMatrixTransposeOperations in '..\ASMMatrixTransposeOperations.pas',
  ASMMatrixTransposeOperationsx64 in '..\ASMMatrixTransposeOperationsx64.pas',
  ASMMatrixVectorMultOperations in '..\ASMMatrixVectorMultOperations.pas',
  ASMMatrixVectorMultOperationsx64 in '..\ASMMatrixVectorMultOperationsx64.pas',
  ASMMoveOperations in '..\ASMMoveOperations.pas',
  ASMMoveOperationsx64 in '..\ASMMoveOperationsx64.pas',
  ASMMatrixAbsOperations in '..\ASMMatrixAbsOperations.pas',
  ASMMatrixAbsOperationsx64 in '..\ASMMatrixAbsOperationsx64.pas',
  BaseMathPersistence in '..\BaseMathPersistence.pas',
  BinaryReaderWriter in '..\BinaryReaderWriter.pas',
  BlockSizeSetup in '..\BlockSizeSetup.pas',
  BufferedStream in '..\BufferedStream.pas',
  CPUFeatures in '..\CPUFeatures.pas',
  Eigensystems in '..\Eigensystems.pas',
  IncrementalPCA in '..\IncrementalPCA.pas',
  LinearAlgebraicEquations in '..\LinearAlgebraicEquations.pas',
  MathUtilFunc in '..\MathUtilFunc.pas',
  Matrix in '..\Matrix.pas',
  MatrixConst in '..\MatrixConst.pas',
  MtxThreadPool in '..\MtxThreadPool.pas',
  MtxTimer in '..\MtxTimer.pas',
  NonLinearFit in '..\NonLinearFit.pas',
  OptimizedFuncs in '..\OptimizedFuncs.pas',
  PCA in '..\PCA.pas',
  SimpleMatrixOperations in '..\SimpleMatrixOperations.pas',
  ThreadedLinAlg in '..\ThreadedLinAlg.pas',
  ThreadedMatrix in '..\ThreadedMatrix.pas',
  ThreadedMatrixOperations in '..\ThreadedMatrixOperations.pas',
  GCDDispatch in '..\mac\GCDDispatch.pas',
  MacOsThreadPool in '..\mac\MacOsThreadPool.pas',
  winCPUInfo in '..\win\winCPUInfo.pas',
  SimpleWinThreadPool in '..\win\SimpleWinThreadPool.pas',

  WinThreadPool in '..\win\WinThreadPool.pas',
  CCA in '..\CCA.pas',
  TestCCA in 'TestCCA.pas',
  Utilities in '..\Utilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

