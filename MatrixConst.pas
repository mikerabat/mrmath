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


unit MatrixConst;

// ##############################################
// #### Constants for the mathematic utilities
// ##############################################

interface

uses SysUtils, Types;

type
  TLinEquResult = (leOk, leSingular);
  TSVDResult = (srOk, srNoConvergence);
  TCholeskyResult = (crOk, crNoPositiveDefinite);
  TQRResult = (qrOK, qrSingular);

const cDefEpsilon : double = 1e-20;

type
  TEigenvalueConvergence = (qlOk, qlNoConverge, qlMatrixError);

type
  TMathOperationRes = (moSuccess, moFailure);

  EBaseMatrixException = class(Exception);
  ELinEQSingularException = class(EBaseMatrixException);

type
  TDynArrayofDoubleArray = Array of TDoubleDynArray;

type
  TConstDoubleArr = Array[0..MaxInt div sizeof(double) - 1] of double;
  PConstDoubleArr = ^TConstDoubleArr;

type
  TMatrixFunc = procedure(var Value : double);
  TMatrixObjFunc = procedure(var Value : double) of Object;
  TMatrixMtxRefFunc = procedure(var Value : double; const data : PDouble; LineWidth : integer; x, y : integer);
  TMatrixMtxRefObjFunc = procedure(var Value : double; const data : PDouble; LineWidth : integer; x, y : integer) of Object;

implementation

end.
