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
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
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
  BlockSizeSetup in '..\BlockSizeSetup.pas',
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
  ASMMatrixAbsOperations in '..\ASMMatrixAbsOperations.pas',
  ASMMatrixAbsOperationsx64 in '..\ASMMatrixAbsOperationsx64.pas',
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
  LinAlgQR in '..\LinAlgQR.pas';

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

