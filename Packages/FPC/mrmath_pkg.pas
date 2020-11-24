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

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mrMath_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  ASMMatrixAbsOperations, ASMMatrixAbsOperationsx64, 
  ASMMatrixAddSubOperations, ASMMatrixAddSubOperationsx64, 
  ASMMatrixCumSumDiffOperations, ASMMatrixCumSumDiffOperationsx64, 
  ASMMatrixElementwiseMultOperations, ASMMatrixElementwiseMultOperationsx64, 
  ASMMatrixMeanOperations, ASMMatrixMeanOperationsx64, 
  ASMMatrixMinMaxOperations, ASMMatrixMinMaxOperationsx64, 
  ASMMatrixMultOperations, ASMMatrixMultOperationsx64, 
  ASMMatrixMultTransposedOperations, ASMMatrixMultTransposedOperationsx64, 
  ASMMatrixNormOperations, ASMMatrixNormOperationsx64, ASMMatrixOperations, 
  ASMMatrixRotations, ASMMatrixRotationsx64, ASMMatrixScaleOperations, 
  ASMMatrixScaleOperationsx64, ASMMatrixSqrtOperations, 
  ASMMatrixSqrtOperationsx64, ASMMatrixSumOperations, 
  ASMMatrixSumOperationsx64, ASMMatrixTransposeOperations, 
  ASMMatrixTransposeOperationsx64, ASMMatrixVectorMultOperations, 
  ASMMatrixVectorMultOperationsx64, ASMMoveOperations, ASMMoveOperationsx64, 
  ASMVecConvolve, ASMVecConvolvex64, AVXMatrixAbsOperations, 
  AVXMatrixAbsOperationsx64, AVXMatrixAddSubOperations, 
  AVXMatrixAddSubOperationsx64, AVXMatrixCumSumDiffOperations, 
  AVXMatrixCumSumDiffOperationsx64, AVXMatrixElementwiseMultOperations, 
  AVXMatrixElementwiseMultOperationsx64, AVXMatrixMeanOperations, 
  AVXMatrixMeanOperationsx64, AVXMatrixMinMaxOperations, 
  AVXMatrixMinMaxOperationsx64, AVXMatrixMultOperations, 
  AVXMatrixMultOperationsx64, AVXMatrixMultTransposedOperations, 
  AVXMatrixMultTransposedOperationsx64, AVXMatrixNormOperations, 
  AVXMatrixNormOperationsx64, AVXMatrixOperations, AVXMatrixRotations, 
  AVXMatrixRotationsx64, AVXMatrixScaleOperations, 
  AVXMatrixScaleOperationsx64, AVXMatrixSqrtOperations, 
  AVXMatrixSqrtOperationsx64, AVXMatrixSumOperations, 
  AVXMatrixSumOperationsx64, AVXMatrixTransposeOperations, 
  AVXMatrixTransposeOperationsx64, AVXMatrixVectorMultOperations, 
  AVXMatrixVectorMultOperationsx64, AVXMoveOperations, AVXMoveOperationsx64, 
  AVXVecConvolve, AVXVecConvolvex64, BaseMathPersistence, BinaryReaderWriter, 
  BlockedMult, BlockSizeSetup, BufferedStream, CCA, Corr, CPUFeatures, Dist, 
  Eigensystems, EM, FMAMatrixMultOperations, FMAMatrixMultOperationsx64, 
  FMAMatrixMultTransposedOperations, FMAMatrixMultTransposedOperationsx64, 
  FMAMatrixOperations, FMAMatrixVectorMultOperations, 
  FMAMatrixVectorMultOperationsx64, FMAVecConvolve, FMAVecConvolvex64, 
  GCDDispatch, HouseholderReflectors, ICA, IncrementalPCA, JSONReaderWriter, 
  LinAlgCholesky, LinAlgLU, LinAlgQR, LinAlgSVD, LinearAlgebraicEquations, 
  linuxthrpool, MacOsRandomGen, MacOsThreadPool, MathUtilFunc, Matrix, 
  MatrixASMStubSwitch, MatrixConst, MatrixRotations, MtxThreadPool, MtxTimer, 
  NNMF, NonLinearFit, PCA, PLS, RandomEng, RBSplines, SimpleMatrixOperations, 
  SimpleWinThreadPool, Statistics, ThreadedMatrix, ThreadedMatrixOperations, 
  tSNE, winCPUInfo, winRandomGen, WinThreadPool, KernelPCA,
  SSA, Roots, IOCompletionPortsThreadPool, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('mrMath_pkg', @Register);
end.
