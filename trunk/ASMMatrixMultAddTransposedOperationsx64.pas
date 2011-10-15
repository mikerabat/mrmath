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


unit ASMMatrixMultAddTransposedOperationsx64;

// ##############################################################
// #### Assembler optimized matrix multiplication assuming a transposed second
// #### matrix
// ##############################################################

interface

{$IFDEF CPUX64}

procedure ASMMatrixMultAddAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

procedure ASMMatrixMultAddAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
procedure ASMMatrixMultAddUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);

{$ENDIF}

implementation

{$IFDEF CPUX64}

procedure ASMMatrixMultAddAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;

procedure ASMMatrixMultAddUnAlignedEvenW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;


procedure ASMMatrixMultAddAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;

procedure ASMMatrixMultAddUnAlignedEvenW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;


procedure ASMMatrixMultAddAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;

procedure ASMMatrixMultAddUnAlignedOddW1EvenH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;


procedure ASMMatrixMultAddAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;

procedure ASMMatrixMultAddUnAlignedOddW1OddH2Transposed(dest : PDouble; const destLineWidth : integer; mt1, mt2 : PDouble; width1 : integer; height1 : integer; width2 : integer; height2 : integer; const LineWidth1, LineWidth2 : integer);
begin
end;


{$ENDIF}

end.
