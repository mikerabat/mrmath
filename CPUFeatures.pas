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


unit CPUFeatures;

// unit to determine some cpu features

interface

function IsSSE3Present : boolean;

implementation

// ##############################################################
// #### feature detection code
// ##############################################################

type
  TRegisters = record
    EAX,
    EBX,
    ECX,
    EDX: Cardinal;
  end;

{$IFDEF CPUX64}

function IsCPUID_Available : boolean;
begin
 	   Result := true;
end;

procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
asm
   .pushnv rbx;                        {save affected registers}
   .pushnv rdi;
   MOV     RDI, Registers
   XOR     RBX, RBX                    {clear EBX register}
   XOR     RCX, RCX                    {clear ECX register}
   XOR     RDX, RDX                    {clear EDX register}
   DB $0F, $A2                         {CPUID opcode}
   MOV     TRegisters(RDI).&EAX, EAX   {save EAX register}
   MOV     TRegisters(RDI).&EBX, EBX   {save EBX register}
   MOV     TRegisters(RDI).&ECX, ECX   {save ECX register}
   MOV     TRegisters(RDI).&EDX, EDX   {save EDX register}
end;


{$ELSE}


function IsCPUID_Available: Boolean; register;
asm
   PUSHFD                 {save EFLAGS to stack}
   POP     EAX            {store EFLAGS in EAX}
   MOV     EDX, EAX       {save in EDX for later testing}
   XOR     EAX, $200000;  {flip ID bit in EFLAGS}
   PUSH    EAX            {save new EFLAGS value on stack}
   POPFD                  {replace current EFLAGS value}
   PUSHFD                 {get new EFLAGS}
   POP     EAX            {store new EFLAGS in EAX}
   XOR     EAX, EDX       {check if ID bit changed}
   JZ      @exit          {no, CPUID not available}
   MOV     EAX, True      {yes, CPUID is available}
@exit:
end;


procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
asm
   PUSH    EBX                         {save affected registers}
   PUSH    EDI
   MOV     EDI, Registers
   XOR     EBX, EBX                    {clear EBX register}
   XOR     ECX, ECX                    {clear ECX register}
   XOR     EDX, EDX                    {clear EDX register}
   DB $0F, $A2                         {CPUID opcode}
   MOV     TRegisters(EDI).&EAX, EAX   {save EAX register}
   MOV     TRegisters(EDI).&EBX, EBX   {save EBX register}
   MOV     TRegisters(EDI).&ECX, ECX   {save ECX register}
   MOV     TRegisters(EDI).&EDX, EDX   {save EDX register}
   POP     EDI                         {restore registers}
   POP     EBX
end;
{$ENDIF}

function IsSSE3Present : boolean;
var reg : TRegisters;
begin
     Result := False;

     if IsCPUID_Available then
     begin
          GetCPUID($00000001, reg);

          Result := (reg.ECX and $00000001) <> 0;
     end;
end;

end.
