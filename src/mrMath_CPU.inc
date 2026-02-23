// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2023, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################
// ###################################################################
// #### contains the used cpu definitions in the project
// ###################################################################

// can be used in FPC and Delphi
// x64 is created if the CPU is a 64 bit AMD/Intel target
// x86 is created if the CPU is a 32 bit AMD/Intel target
// MRMATH_NOASM is created for all others e.g. ARM where no assembler files have been created

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

{$IFDEF CPU86}
{$DEFINE x86}
{$ENDIF}
{$IFDEF CPUX86}
{$DEFINE x86}
{$ENDIF}

{$IFDEF CPU386}
{$DEFINE x86}
{$ENDIF}

{$IF not defined(x64) and not defined(x86)}
  {$DEFINE MRMATH_NOASM}
{$ifend}

{$DEFINE AVXSUP}  // assembler support for AVX/FMA built in
{$IFNDEF FPC}
{$IF CompilerVersion<35}
{$UNDEF AVXSUP}
{$IFEND}
{$ENDIF}

// ###########################################
// #### Free Pascal specific
{$IFNDEF MRMATH_NOASM}
{$IFDEF FPC} {$ASMMODE intel} {$S-} {$ENDIF}
{$ENDIF}

