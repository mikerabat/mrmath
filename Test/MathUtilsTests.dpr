// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


program MathUtilsTests;

{$IFNDEF MSWINDOWS}
{$MESSAGE ERROR 'This unit test program is only compilable for Windows.'}
{$ENDIF}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4 in 'D:\Daten\Delphi2010\3rdParty\FastMM4\FastMM4.pas',
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSimpleMatrixOperations in 'TestSimpleMatrixOperations.pas',
  TestAVX in 'TestAVX.pas',
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
  ASMMatrixRotations in '..\ASMMatrixRotations.pas',
  ASMMatrixRotationsx64 in '..\ASMMatrixRotationsx64.pas',
  ASMMatrixAbsOperations in '..\ASMMatrixAbsOperations.pas',
  ASMMatrixAbsOperationsx64 in '..\ASMMatrixAbsOperationsx64.pas',
  ASMMatrixCumSumDiffOperations in '..\ASMMatrixCumSumDiffOperations.pas',
  ASMMatrixCumSumDiffOperationsx64 in '..\ASMMatrixCumSumDiffOperationsx64.pas',
  BlockSizeSetup in '..\BlockSizeSetup.pas',
  BlockedMult in '..\BlockedMult.pas',
  CPUFeatures in '..\CPUFeatures.pas',
  Eigensystems in '..\Eigensystems.pas',
  LinearAlgebraicEquations in '..\LinearAlgebraicEquations.pas',
  MathUtilFunc in '..\MathUtilFunc.pas',
  Matrix in '..\Matrix.pas',
  MatrixConst in '..\MatrixConst.pas',
  MtxThreadPool in '..\MtxThreadPool.pas',
  OptimizedFuncs in '..\OptimizedFuncs.pas',
  SimpleMatrixOperations in '..\SimpleMatrixOperations.pas',
  ThreadedMatrix in '..\ThreadedMatrix.pas',
  ThreadedMatrixOperations in '..\ThreadedMatrixOperations.pas',
  BaseMathPersistence in '..\BaseMathPersistence.pas',
  BinaryReaderWriter in '..\BinaryReaderWriter.pas',
  BufferedStream in '..\BufferedStream.pas',
  TestPCA in 'TestPCA.pas',
  CCA in '..\CCA.pas',
  PCA in '..\PCA.pas',
  TestNonLinFit in 'TestNonLinFit.pas',
  NonLinearFit in '..\NonLinearFit.pas',
  IncrementalPCA in '..\IncrementalPCA.pas',
  MacOsThreadPool in '..\mac\MacOsThreadPool.pas',
  SimpleWinThreadPool in '..\win\SimpleWinThreadPool.pas',
  WinThreadPool in '..\win\WinThreadPool.pas',
  GCDDispatch in '..\mac\GCDDispatch.pas',
  MtxTimer in '..\MtxTimer.pas',
  TestCCA in 'TestCCA.pas',
  NNMF in '..\NNMF.pas',
  TestNMF in 'TestNMF.pas',
  winCPUInfo in '..\win\winCPUInfo.pas',
  ICA in '..\ICA.pas',
  TestICA in 'TestICA.pas',
  RandomEng in '..\RandomEng.pas',
  TestRandom in 'TestRandom.pas',
  winRandomGen in '..\win\winRandomGen.pas',
  MacOsRandomGen in '..\mac\MacOsRandomGen.pas',
  JSONReaderWriter in '..\JSONReaderWriter.pas',
  LinAlgSVD in '..\LinAlgSVD.pas',
  HouseholderReflectors in '..\HouseholderReflectors.pas',
  MatrixRotations in '..\MatrixRotations.pas',
  LinAlgCholesky in '..\LinAlgCholesky.pas',
  LinAlgLU in '..\LinAlgLU.pas',
  LinAlgQR in '..\LinAlgQR.pas',
  PLS in '..\PLS.pas',
  Corr in '..\Corr.pas',
  TestCorr in 'TestCorr.pas',
  linuxthrpool in '..\mac\linuxthrpool.pas',
  AVXMatrixAbsOperations in '..\AVXMatrixAbsOperations.pas',
  AVXMatrixAbsOperationsx64 in '..\AVXMatrixAbsOperationsx64.pas',
  AVXMatrixAddSubOperations in '..\AVXMatrixAddSubOperations.pas',
  AVXMatrixAddSubOperationsx64 in '..\AVXMatrixAddSubOperationsx64.pas',
  AVXMatrixCumSumDiffOperations in '..\AVXMatrixCumSumDiffOperations.pas',
  AVXMatrixCumSumDiffOperationsx64 in '..\AVXMatrixCumSumDiffOperationsx64.pas',
  AVXMatrixElementwiseMultOperations in '..\AVXMatrixElementwiseMultOperations.pas',
  AVXMatrixElementwiseMultOperationsx64 in '..\AVXMatrixElementwiseMultOperationsx64.pas',
  AVXMatrixMeanOperations in '..\AVXMatrixMeanOperations.pas',
  AVXMatrixMeanOperationsx64 in '..\AVXMatrixMeanOperationsx64.pas',
  AVXMatrixMinMaxOperations in '..\AVXMatrixMinMaxOperations.pas',
  AVXMatrixMinMaxOperationsx64 in '..\AVXMatrixMinMaxOperationsx64.pas',
  AVXMatrixMultOperations in '..\AVXMatrixMultOperations.pas',
  AVXMatrixMultOperationsx64 in '..\AVXMatrixMultOperationsx64.pas',
  AVXMatrixMultTransposedOperations in '..\AVXMatrixMultTransposedOperations.pas',
  AVXMatrixMultTransposedOperationsx64 in '..\AVXMatrixMultTransposedOperationsx64.pas',
  AVXMatrixNormOperations in '..\AVXMatrixNormOperations.pas',
  AVXMatrixNormOperationsx64 in '..\AVXMatrixNormOperationsx64.pas',
  AVXMatrixOperations in '..\AVXMatrixOperations.pas',
  AVXMatrixRotations in '..\AVXMatrixRotations.pas',
  AVXMatrixRotationsx64 in '..\AVXMatrixRotationsx64.pas',
  AVXMatrixScaleOperations in '..\AVXMatrixScaleOperations.pas',
  AVXMatrixScaleOperationsx64 in '..\AVXMatrixScaleOperationsx64.pas',
  AVXMatrixSqrtOperations in '..\AVXMatrixSqrtOperations.pas',
  AVXMatrixSqrtOperationsx64 in '..\AVXMatrixSqrtOperationsx64.pas',
  AVXMatrixSumOperations in '..\AVXMatrixSumOperations.pas',
  AVXMatrixSumOperationsx64 in '..\AVXMatrixSumOperationsx64.pas',
  AVXMatrixTransposeOperations in '..\AVXMatrixTransposeOperations.pas',
  AVXMatrixTransposeOperationsx64 in '..\AVXMatrixTransposeOperationsx64.pas',
  AVXMatrixVectorMultOperations in '..\AVXMatrixVectorMultOperations.pas',
  AVXMatrixVectorMultOperationsx64 in '..\AVXMatrixVectorMultOperationsx64.pas',
  AVXMoveOperations in '..\AVXMoveOperations.pas',
  AVXMoveOperationsx64 in '..\AVXMoveOperationsx64.pas',
  FastMM4Messages in 'D:\Daten\Delphi2010\3rdParty\FastMM4\FastMM4Messages.pas';

{$R *.RES}

begin
     ReportMemoryLeaksOnShutdown := True;
     Application.Initialize;
     if IsConsole
     then
         TextTestRunner.RunRegisteredTests
     else
         GUITestRunner.RunRegisteredTests;
end.

