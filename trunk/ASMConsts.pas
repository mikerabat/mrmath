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

unit ASMConsts;

// #####################################################
// #### Compiler dependent definitions
// #####################################################

interface

{$IFDEF CPUX64}
type
  TASMNativeInt = NativeInt;
  TASMNativeUInt = NativeUInt;
{$ELSE}
type
  TASMNativeInt = integer;
  TASMNativeUInt = Cardinal;
{$ENDIF}

type
  TMatrixMultDestOperation = (doNone, doAdd, doSub);
implementation

end.
