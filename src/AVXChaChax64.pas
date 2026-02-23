// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2021, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit AVXChaChax64;

interface

uses RandomEng;

{$I 'mrMath_CPU.inc'}

{$IFDEF x64}

procedure AVXChaChaDoubleQuarterRound( chachaMtx : PChaChaAVXMtx );
procedure AVXRealingAndAddMtx( chaChaMtx : PChaChaAVXMtx; inpChaCha : PChaChaMtx );

{$ENDIF}

implementation


{$IFDEF x64}

uses MatrixConst;

const cShuf16 : Array[0..31] of byte = (3, 0, 1, 2,
                                        7, 4, 5, 6,
                                        11, 8, 9, 10,
                                        15, 12, 13, 14,
                                        3, 0, 1, 2,
                                        7, 4, 5, 6,
                                        11, 8, 9, 10,
                                        15, 12, 13, 14);

procedure AVXChaChaDoubleQuarterRound( chachaMtx : PChaChaAVXMtx );
var dYMM4, dYMM5 : TYMMArr;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // The parameters are passed in the following order:
   // RDI, RSI, RDX, RCX, r8, r9 -> mov to RCX, RDX, R8, R9, width and height
   // in our case only rdi to rcx
   mov rcx, rdi;
   {$ENDIF}
   {$IFDEF x64}
   {$IFDEF AVXSUP}vmovupd dYMM4, ymm4;                                {$ELSE}db $C5,$FD,$11,$65,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd dYMM5, ymm5;                                {$ELSE}db $C5,$FD,$11,$6D,$B8;{$ENDIF} 

   // 64bit version
   lea rdx, [rip + cShuf16];
   {$IFDEF AVXSUP}vmovdqu ymm5, [rdx];                                {$ELSE}db $C5,$FE,$6F,$2A;{$ENDIF} 

   // move the matrix to xmm0 to xmm3
   {$IFDEF AVXSUP}vmovdqa ymm0, [rcx];                                {$ELSE}db $C5,$FD,$6F,$01;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm1, [rcx + 32];                           {$ELSE}db $C5,$FD,$6F,$49,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm2, [rcx + 64];                           {$ELSE}db $C5,$FD,$6F,$51,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm3, [rcx + 96];                           {$ELSE}db $C5,$FD,$6F,$59,$60;{$ENDIF} 

   // v0 += v1; v3 ^= v0; v3 <<<= 16
   {$IFDEF AVXSUP}vpaddd ymm0, ymm0, ymm1;                            {$ELSE}db $C5,$FD,$FE,$C1;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm3, ymm3, ymm0;                             {$ELSE}db $C5,$E5,$EF,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vpshufhw ymm3, ymm3, $B1;                           {$ELSE}db $C5,$FE,$70,$DB,$B1;{$ENDIF} // 10 11 00 01
   {$IFDEF AVXSUP}vpshuflw ymm3, ymm3, $B1;                           {$ELSE}db $C5,$FF,$70,$DB,$B1;{$ENDIF} 

   // v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
   {$IFDEF AVXSUP}vpaddd ymm2, ymm2, ymm3;                            {$ELSE}db $C5,$ED,$FE,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm1, ymm1, ymm2;                             {$ELSE}db $C5,$F5,$EF,$CA;{$ENDIF} 
   // rotate is x << n | x >> 32 - n
   {$IFDEF AVXSUP}vpslld ymm4, ymm1, 12;                              {$ELSE}db $C5,$DD,$72,$F1,$0C;{$ENDIF} 
   {$IFDEF AVXSUP}vpsrld ymm1, ymm1, 20;                              {$ELSE}db $C5,$F5,$72,$D1,$14;{$ENDIF} // 32 - 12
   {$IFDEF AVXSUP}vpor ymm1, ymm1, ymm4;                              {$ELSE}db $C5,$F5,$EB,$CC;{$ENDIF} 

   // v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
   {$IFDEF AVXSUP}vpaddd ymm0, ymm0, ymm1;                            {$ELSE}db $C5,$FD,$FE,$C1;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm3, ymm3, ymm0;                             {$ELSE}db $C5,$E5,$EF,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vpshufb ymm3, ymm3, ymm5;                           {$ELSE}db $C4,$E2,$65,$00,$DD;{$ENDIF} 

   // v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
   {$IFDEF AVXSUP}vpaddd ymm2, ymm2, ymm3;                            {$ELSE}db $C5,$ED,$FE,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm1, ymm1, ymm2;                             {$ELSE}db $C5,$F5,$EF,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vpslld ymm4, ymm1, 7;                               {$ELSE}db $C5,$DD,$72,$F1,$07;{$ENDIF} 
   {$IFDEF AVXSUP}vpsrld ymm1, ymm1, 25;                              {$ELSE}db $C5,$F5,$72,$D1,$19;{$ENDIF} // 32 - 7
   {$IFDEF AVXSUP}vpor ymm1, ymm1, ymm4;                              {$ELSE}db $C5,$F5,$EB,$CC;{$ENDIF} 

   // v1 >>>= 32; v2 >>>= 64; v3 >>>= 96;

   // palignr is actually a sse3 opcode but ok...
   {$IFDEF AVXSUP}vmovapd ymm4, ymm1;                                 {$ELSE}db $C5,$FD,$29,$CC;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm1, ymm1, ymm4, 4;                       {$ELSE}db $C4,$E3,$75,$0F,$CC,$04;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm2;                                 {$ELSE}db $C5,$FD,$29,$D4;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm2, ymm2, ymm4, 8;                       {$ELSE}db $C4,$E3,$6D,$0F,$D4,$08;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm3;                                 {$ELSE}db $C5,$FD,$29,$DC;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm3, ymm3, ymm4, 12;                      {$ELSE}db $C4,$E3,$65,$0F,$DC,$0C;{$ENDIF} 

   // v0 += v1; v3 ^= v0; v3 <<<= 16
   {$IFDEF AVXSUP}vpaddd ymm0, ymm0, ymm1;                            {$ELSE}db $C5,$FD,$FE,$C1;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm3, ymm3, ymm0;                             {$ELSE}db $C5,$E5,$EF,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vpshufhw ymm3, ymm3, $B1;                           {$ELSE}db $C5,$FE,$70,$DB,$B1;{$ENDIF} // 10 11 00 01
   {$IFDEF AVXSUP}vpshuflw ymm3, ymm3, $B1;                           {$ELSE}db $C5,$FF,$70,$DB,$B1;{$ENDIF} 

   // v2 += v3; v1 ^= v2; v1 <<<= (12, 12, 12, 12);
   {$IFDEF AVXSUP}vpaddd ymm2, ymm2, ymm3;                            {$ELSE}db $C5,$ED,$FE,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm1, ymm1, ymm2;                             {$ELSE}db $C5,$F5,$EF,$CA;{$ENDIF} 
   // rotate is x << n | x >> 32 - n
   {$IFDEF AVXSUP}vpslld ymm4, ymm1, 12;                              {$ELSE}db $C5,$DD,$72,$F1,$0C;{$ENDIF} 
   {$IFDEF AVXSUP}vpsrld ymm1, ymm1, 20;                              {$ELSE}db $C5,$F5,$72,$D1,$14;{$ENDIF} // 32 - 12
   {$IFDEF AVXSUP}vpor ymm1, ymm1, ymm4;                              {$ELSE}db $C5,$F5,$EB,$CC;{$ENDIF} 

   // v0 += v1; v3 ^= v0; v3 <<<= ( 8,  8,  8,  8);
   {$IFDEF AVXSUP}vpaddd ymm0, ymm0, ymm1;                            {$ELSE}db $C5,$FD,$FE,$C1;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm3, ymm3, ymm0;                             {$ELSE}db $C5,$E5,$EF,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vpshufb ymm3, ymm3, ymm5;                           {$ELSE}db $C4,$E2,$65,$00,$DD;{$ENDIF} 

   // v2 += v3; v1 ^= v2; v1 <<<= ( 7,  7,  7,  7);
   {$IFDEF AVXSUP}vpaddd ymm2, ymm2, ymm3;                            {$ELSE}db $C5,$ED,$FE,$D3;{$ENDIF} 
   {$IFDEF AVXSUP}vpxor ymm1, ymm1, ymm2;                             {$ELSE}db $C5,$F5,$EF,$CA;{$ENDIF} 
   {$IFDEF AVXSUP}vpslld ymm4, ymm1, 7;                               {$ELSE}db $C5,$DD,$72,$F1,$07;{$ENDIF} 
   {$IFDEF AVXSUP}vpsrld ymm1, ymm1, 25;                              {$ELSE}db $C5,$F5,$72,$D1,$19;{$ENDIF} // 32 - 7
   {$IFDEF AVXSUP}vpor ymm1, ymm1, ymm4;                              {$ELSE}db $C5,$F5,$EB,$CC;{$ENDIF} 

   // v1 <<<= 32; v2 <<<= 64; v3 <<<= 96; Return
   {$IFDEF AVXSUP}vmovapd ymm4, ymm1;                                 {$ELSE}db $C5,$FD,$29,$CC;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm1, ymm1, ymm4, 12;                      {$ELSE}db $C4,$E3,$75,$0F,$CC,$0C;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm2;                                 {$ELSE}db $C5,$FD,$29,$D4;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm2, ymm2, ymm4, 8;                       {$ELSE}db $C4,$E3,$6D,$0F,$D4,$08;{$ENDIF} 
   {$IFDEF AVXSUP}vmovapd ymm4, ymm3;                                 {$ELSE}db $C5,$FD,$29,$DC;{$ENDIF} 
   {$IFDEF AVXSUP}vpalignr ymm3, ymm3, ymm4, 4;                       {$ELSE}db $C4,$E3,$65,$0F,$DC,$04;{$ENDIF} 

   // move back
   {$IFDEF AVXSUP}vmovdqa [rcx], ymm0;                                {$ELSE}db $C5,$FD,$7F,$01;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 32], ymm1;                           {$ELSE}db $C5,$FD,$7F,$49,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 64], ymm2;                           {$ELSE}db $C5,$FD,$7F,$51,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 96], ymm3;                           {$ELSE}db $C5,$FD,$7F,$59,$60;{$ENDIF} 

   // cleanup registers
   {$IFDEF AVXSUP}vmovupd ymm4, dYMM4;                                {$ELSE}db $C5,$FD,$10,$65,$D8;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd ymm5, dYMM5;                                {$ELSE}db $C5,$FD,$10,$6D,$B8;{$ENDIF} 
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
{$ENDIF}
end;
{$IFDEF FPC}
end;
{$ENDIF}

// realign the chacha matrix such that further access to it is linear
procedure AVXRealingAndAddMtx( chaChaMtx : PChaChaAVXMtx; inpChaCha : PChaChaMtx );
var dXMM4, dXMM5 : TXMMArr;
{$IFDEF FPC}
begin
{$ENDIF}
asm
   {$IFDEF UNIX}
   // Linux uses a diffrent ABI -> copy over the registers so they meet with winABI
   // The parameters are passed in the following order:
   // RDI, RSI -> mov to RCX, RDX
   mov rcx, rdi;
   mov rdx, rsi;
   {$ENDIF}
   {$IFDEF AVXSUP}vmovupd dXMM4, xmm4;                                {$ELSE}db $C5,$F9,$11,$65,$E0;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd dXMM5, xmm5;                                {$ELSE}db $C5,$F9,$11,$6D,$D0;{$ENDIF} 

   // store second matrix
   {$IFDEF AVXSUP}vmovdqa xmm0, [rcx + 16];                           {$ELSE}db $C5,$F9,$6F,$41,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm1, [rcx + 48];                           {$ELSE}db $C5,$F9,$6F,$49,$30;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm2, [rcx + 80];                           {$ELSE}db $C5,$F9,$6F,$51,$50;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm3, [rcx + 112];                          {$ELSE}db $C5,$F9,$6F,$59,$70;{$ENDIF} 

   {$IFDEF AVXSUP}vpaddd xmm0, xmm0, [rdx];                           {$ELSE}db $C5,$F9,$FE,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm1, xmm1, [rdx + 16];                      {$ELSE}db $C5,$F1,$FE,$4A,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm2, xmm2, [rdx + 32];                      {$ELSE}db $C5,$E9,$FE,$52,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm3, xmm3, [rdx + 48];                      {$ELSE}db $C5,$E1,$FE,$5A,$30;{$ENDIF} 


   // move positions
   {$IFDEF AVXSUP}vmovapd xmm5, [rcx];                                {$ELSE}db $C5,$F9,$28,$29;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [rdx];                           {$ELSE}db $C5,$D1,$FE,$2A;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx], xmm5;                                {$ELSE}db $C5,$F9,$7F,$29;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [rcx + 32];                           {$ELSE}db $C5,$F9,$6F,$69,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [rdx + 16];                      {$ELSE}db $C5,$D1,$FE,$6A,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 16], xmm5;                           {$ELSE}db $C5,$F9,$7F,$69,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [rcx + 64];                           {$ELSE}db $C5,$F9,$6F,$69,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [rdx + 32];                      {$ELSE}db $C5,$D1,$FE,$6A,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 32], xmm5;                           {$ELSE}db $C5,$F9,$7F,$69,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [rcx + 96];                           {$ELSE}db $C5,$F9,$6F,$69,$60;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [rdx + 48];                      {$ELSE}db $C5,$D1,$FE,$6A,$30;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 48], xmm5;                           {$ELSE}db $C5,$F9,$7F,$69,$30;{$ENDIF} 

   // append second matrix
   {$IFDEF AVXSUP}vmovdqa [rcx + 64], xmm0;                           {$ELSE}db $C5,$F9,$7F,$41,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 80], xmm1;                           {$ELSE}db $C5,$F9,$7F,$49,$50;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 96], xmm2;                           {$ELSE}db $C5,$F9,$7F,$51,$60;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [rcx + 112], xmm3;                          {$ELSE}db $C5,$F9,$7F,$59,$70;{$ENDIF} 

   // cleanup registers
   {$IFDEF AVXSUP}vmovupd xmm4, dXMM4;                                {$ELSE}db $C5,$F9,$10,$65,$E0;{$ENDIF} 
   {$IFDEF AVXSUP}vmovupd xmm5, dXMM5;                                {$ELSE}db $C5,$F9,$10,$6D,$D0;{$ENDIF} 

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;
{$IFDEF FPC}
end;
{$ENDIF}

{$ENDIF}

end.

