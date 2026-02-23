// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

library mrMath;

{$mode objfpc}{$H+}
{$ALIGN 16}
{$DESCRIPTION 'mrMath Matrix library'}

{$I '..\..\src\mrMath_CPU.inc'}

uses
  {$IFDEF LINUX} cthreads, {$ENDIF}
  MatrixConst in '..\..\src\MatrixConst.pas',
  SimpleMatrixOperations in '..\..\src\SimpleMatrixOperations.pas',
  CplxSimpleMatrixOperations in '..\..\src\CplxSimpleMatrixOperations.pas',

// ###########################################
// #### Assembler only routines

{$IFNDEF MRMATH_NOASM}
  ASMMatrixAddSubOperations in '..\..\src\ASMMatrixAddSubOperations.pas',
  ASMMatrixAddSubOperationsx64 in '..\..\src\ASMMatrixAddSubOperationsx64.pas',
  ASMMatrixElementwiseMultOperations in '..\..\src\ASMMatrixElementwiseMultOperations.pas',
  ASMMatrixElementwiseMultOperationsx64 in '..\..\src\ASMMatrixElementwiseMultOperationsx64.pas',
  ASMMatrixMeanOperations in '..\..\src\ASMMatrixMeanOperations.pas',
  ASMMatrixMeanOperationsx64 in '..\..\src\ASMMatrixMeanOperationsx64.pas',
  ASMMatrixMinMaxOperations in '..\..\src\ASMMatrixMinMaxOperations.pas',
  ASMMatrixMinMaxOperationsx64 in '..\..\src\ASMMatrixMinMaxOperationsx64.pas',
  ASMMatrixMultOperations in '..\..\src\ASMMatrixMultOperations.pas',
  ASMMatrixMultOperationsx64 in '..\..\src\ASMMatrixMultOperationsx64.pas',
  ASMMatrixMultTransposedOperations in '..\..\src\ASMMatrixMultTransposedOperations.pas',
  ASMMatrixMultTransposedOperationsx64 in '..\..\src\ASMMatrixMultTransposedOperationsx64.pas',
  ASMMatrixNormOperations in '..\..\src\ASMMatrixNormOperations.pas',
  ASMMatrixNormOperationsx64 in '..\..\src\ASMMatrixNormOperationsx64.pas',
  ASMMatrixOperations in '..\..\src\ASMMatrixOperations.pas',
  ASMMatrixScaleOperations in '..\..\src\ASMMatrixScaleOperations.pas',
  ASMMatrixScaleOperationsx64 in '..\..\src\ASMMatrixScaleOperationsx64.pas',
  ASMMatrixSqrtOperations in '..\..\src\ASMMatrixSqrtOperations.pas',
  ASMMatrixSqrtOperationsx64 in '..\..\src\ASMMatrixSqrtOperationsx64.pas',
  ASMMatrixSumOperations in '..\..\src\ASMMatrixSumOperations.pas',
  ASMMatrixSumOperationsx64 in '..\..\src\ASMMatrixSumOperationsx64.pas',
  ASMMatrixTransposeOperations in '..\..\src\ASMMatrixTransposeOperations.pas',
  ASMMatrixTransposeOperationsx64 in '..\..\src\ASMMatrixTransposeOperationsx64.pas',
  ASMMatrixVectorMultOperations in '..\..\src\ASMMatrixVectorMultOperations.pas',
  ASMMatrixVectorMultOperationsx64 in '..\..\src\ASMMatrixVectorMultOperationsx64.pas',
  ASMMoveOperations in '..\..\src\ASMMoveOperations.pas',
  ASMMoveOperationsx64 in '..\..\src\ASMMoveOperationsx64.pas',
  ASMMatrixRotations in '..\..\src\ASMMatrixRotations.pas',
  ASMMatrixRotationsx64 in '..\..\src\ASMMatrixRotationsx64.pas',
  ASMMatrixCumSumDiffOperations in '..\..\src\ASMMatrixCumSumDiffOperations.pas',
  ASMMatrixCumSumDiffOperationsx64 in '..\..\src\ASMMatrixCumSumDiffOperationsx64.pas',
  ASMMatrixAbsOperations in '..\..\src\ASMMatrixAbsOperations.pas',
  ASMMatrixAbsOperationsx64 in '..\..\src\ASMMatrixAbsOperationsx64.pas',
  AVXMatrixAbsOperations in '..\..\src\AVXMatrixAbsOperations.pas',
  AVXMatrixAbsOperationsx64 in '..\..\src\AVXMatrixAbsOperationsx64.pas',
  AVXMatrixAddSubOperations in '..\..\src\AVXMatrixAddSubOperations.pas',
  AVXMatrixAddSubOperationsx64 in '..\..\src\AVXMatrixAddSubOperationsx64.pas',
  AVXMatrixCumSumDiffOperations in '..\..\src\AVXMatrixCumSumDiffOperations.pas',
  AVXMatrixCumSumDiffOperationsx64 in '..\..\src\AVXMatrixCumSumDiffOperationsx64.pas',
  AVXMatrixElementwiseMultOperations in '..\..\src\AVXMatrixElementwiseMultOperations.pas',
  AVXMatrixElementwiseMultOperationsx64 in '..\..\src\AVXMatrixElementwiseMultOperationsx64.pas',
  AVXMatrixMeanOperations in '..\..\src\AVXMatrixMeanOperations.pas',
  AVXMatrixMeanOperationsx64 in '..\..\src\AVXMatrixMeanOperationsx64.pas',
  AVXMatrixMinMaxOperations in '..\..\src\AVXMatrixMinMaxOperations.pas',
  AVXMatrixMinMaxOperationsx64 in '..\..\src\AVXMatrixMinMaxOperationsx64.pas',
  AVXMatrixMultOperations in '..\..\src\AVXMatrixMultOperations.pas',
  AVXMatrixMultOperationsx64 in '..\..\src\AVXMatrixMultOperationsx64.pas',
  AVXMatrixMultTransposedOperations in '..\..\src\AVXMatrixMultTransposedOperations.pas',
  AVXMatrixMultTransposedOperationsx64 in '..\..\src\AVXMatrixMultTransposedOperationsx64.pas',
  AVXMatrixNormOperations in '..\..\src\AVXMatrixNormOperations.pas',
  AVXMatrixNormOperationsx64 in '..\..\src\AVXMatrixNormOperationsx64.pas',
  AVXMatrixOperations in '..\..\src\AVXMatrixOperations.pas',
  AVXMatrixRotations in '..\..\src\AVXMatrixRotations.pas',
  AVXMatrixRotationsx64 in '..\..\src\AVXMatrixRotationsx64.pas',
  AVXMatrixScaleOperations in '..\..\src\AVXMatrixScaleOperations.pas',
  AVXMatrixScaleOperationsx64 in '..\..\src\AVXMatrixScaleOperationsx64.pas',
  AVXMatrixSqrtOperations in '..\..\src\AVXMatrixSqrtOperations.pas',
  AVXMatrixSqrtOperationsx64 in '..\..\src\AVXMatrixSqrtOperationsx64.pas',
  AVXMatrixSumOperations in '..\..\src\AVXMatrixSumOperations.pas',
  AVXMatrixSumOperationsx64 in '..\..\src\AVXMatrixSumOperationsx64.pas',
  AVXMatrixTransposeOperations in '..\..\src\AVXMatrixTransposeOperations.pas',
  AVXMatrixTransposeOperationsx64 in '..\..\src\AVXMatrixTransposeOperationsx64.pas',
  AVXMatrixVectorMultOperations in '..\..\src\AVXMatrixVectorMultOperations.pas',
  AVXMatrixVectorMultOperationsx64 in '..\..\src\AVXMatrixVectorMultOperationsx64.pas',
  AVXMoveOperations in '..\..\src\AVXMoveOperations.pas',
  AVXMoveOperationsx64 in '..\..\src\AVXMoveOperationsx64.pas',
  FMAMatrixMultOperations in '..\..\src\FMAMatrixMultOperations.pas',
  FMAMatrixMultOperationsx64 in '..\..\src\FMAMatrixMultOperationsx64.pas',
  FMAMatrixMultTransposedOperations in '..\..\src\FMAMatrixMultTransposedOperations.pas',
  FMAMatrixMultTransposedOperationsx64 in '..\..\src\FMAMatrixMultTransposedOperationsx64.pas',
  FMAMatrixOperations in '..\..\src\FMAMatrixOperations.pas',
  FMAMatrixVectorMultOperations in '..\..\src\FMAMatrixVectorMultOperations.pas',
  FMAMatrixVectorMultOperationsx64 in '..\..\src\FMAMatrixVectorMultOperationsx64.pas',
  ASMVecDist in '..\..\src\ASMVecDist.pas',
  ASMVecConvolve in '..\..\src\ASMVecConvolve.pas',
  ASMVecConvolvex64 in '..\..\src\ASMVecConvolvex64.pas',
  AVXVecConvolve in '..\..\src\AVXVecConvolve.pas',
  AVXVecConvolvex64 in '..\..\src\AVXVecConvolvex64.pas',
  FMAVecConvolve in '..\..\src\FMAVecConvolve.pas',
  FMAVecConvolvex64 in '..\..\src\FMAVecConvolvex64.pas',
  AVXChaCha in '..\..\src\AVXChaCha.pas',
  AVXChaChax64 in '..\..\src\AVXChaChax64.pas',

{$ENDIF}

  BlockSizeSetup in '..\..\src\BlockSizeSetup.pas',
  CPUFeatures in '..\..\src\CPUFeatures.pas',
  Eigensystems in '..\..\src\Eigensystems.pas',
  MathUtilFunc in '..\..\src\MathUtilFunc.pas',
  Matrix in '..\..\src\Matrix.pas',
  DblMatrix in '..\..\src\DblMatrix.pas',
  CplxMatrix in '..\..\src\CplxMatrix.pas',
  MtxThreadPool in '..\..\src\MtxThreadPool.pas',
  ThreadedMatrix in '..\..\src\ThreadedMatrix.pas',
  ThreadedMatrixOperations in '..\..\src\ThreadedMatrixOperations.pas',
  BaseMathPersistence in '..\..\src\BaseMathPersistence.pas',
  BinaryReaderWriter in '..\..\src\BinaryReaderWriter.pas',
  JSONReaderWriter in '..\..\src\JSONReaderWriter.pas',
  PCA in '..\..\src\PCA.pas',
  NonLinearFit in '..\..\src\NonLinearFit.pas',
  IncrementalPCA in '..\..\src\IncrementalPCA.pas',
  MtxTimer in '..\..\src\MtxTimer.pas',
  CCA in '..\..\src\CCA.pas',
  NNMF in '..\..\src\NNMF.pas',
  ICA in '..\..\src\ICA.pas',
  RandomEng in '..\..\src\RandomEng.pas',
  LinAlgCholesky in '..\..\src\LinAlgCholesky.pas',
  LinAlgLU in '..\..\src\LinAlgLU.pas',
  LinAlgQR in '..\..\src\LinAlgQR.pas',
  LinAlgSVD in '..\..\src\LinAlgSVD.pas',
  HouseholderReflectors in '..\..\src\HouseholderReflectors.pas',
  MatrixRotations in '..\..\src\MatrixRotations.pas',
  LinearAlgebraicEquations in '..\..\src\LinearAlgebraicEquations.pas',
  PLS in '..\..\src\PLS.pas',
  Corr in '..\..\src\Corr.pas',
  BlockedMult in '..\..\src\BlockedMult.pas',
  EM in '..\..\src\EM.pas',
  RBSplines in '..\..\src\RBSplines.pas',
  Dist in '..\..\src\Dist.pas',
  MatrixASMStubSwitch in '..\..\src\MatrixASMStubSwitch.pas',
  GCDDispatch in '..\..\src\GCDDispatch.pas',
  linuxthrpool in '..\..\src\linuxthrpool.pas',
  MacOsRandomGen in '..\..\src\MacOsRandomGen.pas',
  MacOsThreadPool in '..\..\src\MacOsThreadPool.pas',
  winCPUInfo in '..\..\src\winCPUInfo.pas',
  winRandomGen in '..\..\src\winRandomGen.pas',
  WinThreadPool in '..\..\src\WinThreadPool.pas',
  Statistics in '..\..\src\Statistics.pas',
  tSNE in '..\..\src\tSNE.pas',
  KernelPCA in '..\..\src\KernelPCA.pas',
  IOCompletionPortsThreadPool in '..\..\src\IOCompletionPortsThreadPool.pas',
  Roots in '..\..\src\Roots.pas',
  SSA in '..\..\src\SSA.pas',
  MtxUtilFunc in '..\..\src\MtxUtilFunc.pas',
  RollingMedMean in '..\..\src\RollingMedMean.pas',
  SpatialTrees in '..\..\src\SpatialTrees.pas',
  Wavelets in '..\..\src\Wavelets.pas';

begin
end.

