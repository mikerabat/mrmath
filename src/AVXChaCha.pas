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

unit AVXChaCha;

interface

uses RandomEng;

{$I 'mrMath_CPU.inc'}

{$IFNDEF x64}

procedure AVXChaChaDoubleQuarterRound( chachaMtx : PChaChaAVXMtx ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
procedure AVXRealingAndAddMtx( chaChaMtx : PChaChaAVXMtx; inpChaCha : PChaChaMtx ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}

{$ENDIF}

implementation

{$IFNDEF x64}

const cShuf16 : Array[0..31] of byte = (3, 0, 1, 2,
                                        7, 4, 5, 6,
                                        11, 8, 9, 10,
                                        15, 12, 13, 14,
                                        3, 0, 1, 2,
                                        7, 4, 5, 6,
                                        11, 8, 9, 10,
                                        15, 12, 13, 14);

procedure AVXChaChaDoubleQuarterRound( chachaMtx : PChaChaAVXMtx ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   lea ecx, cShuf16;
   {$IFDEF AVXSUP}vmovdqu ymm5, [ecx];                                {$ELSE}db $C5,$FE,$6F,$29;{$ENDIF} 

   // move the matrix to xmm0 to xmm3
   {$IFDEF AVXSUP}vmovdqa ymm0, [eax];                                {$ELSE}db $C5,$FD,$6F,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm1, [eax + 32];                           {$ELSE}db $C5,$FD,$6F,$48,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm2, [eax + 64];                           {$ELSE}db $C5,$FD,$6F,$50,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa ymm3, [eax + 96];                           {$ELSE}db $C5,$FD,$6F,$58,$60;{$ENDIF} 

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
   {$IFDEF AVXSUP}vmovdqa [eax], ymm0;                                {$ELSE}db $C5,$FD,$7F,$00;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 32], ymm1;                           {$ELSE}db $C5,$FD,$7F,$48,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 64], ymm2;                           {$ELSE}db $C5,$FD,$7F,$50,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 96], ymm3;                           {$ELSE}db $C5,$FD,$7F,$58,$60;{$ENDIF} 
   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

// realign the chacha matrix such that further access to it is linear
procedure AVXRealingAndAddMtx( chaChaMtx : PChaChaAVXMtx; inpChaCha : PChaChaMtx ); {$IFDEF FPC} assembler; {$ELSE} register; {$ENDIF}
asm
   // store second matrix
   {$IFDEF AVXSUP}vmovdqa xmm0, [eax + 16];                           {$ELSE}db $C5,$F9,$6F,$40,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm1, [eax + 48];                           {$ELSE}db $C5,$F9,$6F,$48,$30;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm2, [eax + 80];                           {$ELSE}db $C5,$F9,$6F,$50,$50;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm3, [eax + 112];                          {$ELSE}db $C5,$F9,$6F,$58,$70;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm0, xmm0, [edx];                           {$ELSE}db $C5,$F9,$FE,$02;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm1, xmm1, [edx + 16];                      {$ELSE}db $C5,$F1,$FE,$4A,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm2, xmm2, [edx + 32];                      {$ELSE}db $C5,$E9,$FE,$52,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm3, xmm3, [edx + 48];                      {$ELSE}db $C5,$E1,$FE,$5A,$30;{$ENDIF} 

   // move positions
   {$IFDEF AVXSUP}vmovapd xmm5, [eax];                                {$ELSE}db $C5,$F9,$28,$28;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [edx];                           {$ELSE}db $C5,$D1,$FE,$2A;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax], xmm5;                                {$ELSE}db $C5,$F9,$7F,$28;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [eax + 32];                           {$ELSE}db $C5,$F9,$6F,$68,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [edx + 16];                      {$ELSE}db $C5,$D1,$FE,$6A,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 16], xmm5;                           {$ELSE}db $C5,$F9,$7F,$68,$10;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [eax + 64];                           {$ELSE}db $C5,$F9,$6F,$68,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [edx + 32];                      {$ELSE}db $C5,$D1,$FE,$6A,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 32], xmm5;                           {$ELSE}db $C5,$F9,$7F,$68,$20;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa xmm5, [eax + 96];                           {$ELSE}db $C5,$F9,$6F,$68,$60;{$ENDIF} 
   {$IFDEF AVXSUP}vpaddd xmm5, xmm5, [edx + 48];                      {$ELSE}db $C5,$D1,$FE,$6A,$30;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 48], xmm5;                           {$ELSE}db $C5,$F9,$7F,$68,$30;{$ENDIF} 

   // append second matrix
   {$IFDEF AVXSUP}vmovdqa [eax + 64], xmm0;                           {$ELSE}db $C5,$F9,$7F,$40,$40;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 80], xmm1;                           {$ELSE}db $C5,$F9,$7F,$48,$50;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 96], xmm2;                           {$ELSE}db $C5,$F9,$7F,$50,$60;{$ENDIF} 
   {$IFDEF AVXSUP}vmovdqa [eax + 112], xmm3;                          {$ELSE}db $C5,$F9,$7F,$58,$70;{$ENDIF} 

   {$IFDEF AVXSUP}vzeroupper;                                         {$ELSE}db $C5,$F8,$77;{$ENDIF} 
end;

{$ENDIF}

end.
