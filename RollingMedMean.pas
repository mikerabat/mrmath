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

unit RollingMedMean;

interface

uses SysUtils, Types, MatrixConst;

// ###########################################
// #### Rolling Mean/Median algorithms
// ###########################################

// ###########################################
// #### Simple Median filter.
// #### -> Sample by sample median calculation
// ####    output is the median of the last N samples.
// #### -> The median filter implements an indirect heap structure
// ####    (operates only on the "sorted heap indices").
// #### -> the sliding median is an delphi conversion of the min / max heap
// ####    C implementation found on: http://ideone.com/XPbl6
// ###########################################

{.$DEFINE DEBUG_MEDIAN}

type
  TRollingMedian = class(TObject)
  private
    type
      TMedianIntArr = Array[-High(SmallInt)..High(SmallInt)] of Integer;
      PMedianIntArr = ^TMedianIntArr;

  private
    fOrder : integer;
    fDelay : integer;
    fSwingIn : integer;
    fOrderD2 : integer;
    fOrderD2M1 : integer;

    fBuf : TDoubleDynArray;       // circular queue of values
    fPos : TIntegerDynArray;     // index into `heap` for each value
    fHeapArrBuf : TIntegerDynArray;
    fHeapArr : PMedianIntArr;      // used as a pointer trick to avoid the constant addition with fOrderD2
    fBufIdx : integer;            //position in circular queue
    fMinCount,
    fMaxCount : integer;          // count of items in the heaps

    function mless(i, j : integer) : boolean; {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
    procedure mexchange(i, j : integer); {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
    procedure minSortDown(i : integer); {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
    procedure maxSortDown(i : integer); {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
    function minSortUp(i : integer) : boolean; {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
    function maxSortUp(i : integer) : boolean;  {$IFNDEF DEBUG_MEDIAN} inline; {$ENDIF}
  public
    property Delay : integer read fDelay;
    property Order : integer read fOrder;
    property SwingIn : integer read fSwingIn;

    procedure Clear;

    function FilterVal(const samp : double) : double;
    procedure FilterVals( buf : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt );

    constructor Create(order : SmallInt);
  end;

// ###########################################
// #### Moving mean and variance class
// #### http://www.dsprelated.com/showthread/comp.dsp/97276-1.php
// #### and http://calcandstuff.blogspot.se/2014/02/rolling-variance-calculation.html
// ###########################################

type
  TMovingMeanVar = class(TObject)
  private
    fX : TDoubleDynArray;
    fX2 : TDoubleDynArray;
    fIdx : integer;
    fIdx2 : integer;
    fCnt : cardinal;
    fWinSize : integer;

    fSx1 : double;
    fSx2 : double;
  public
    constructor Create(aWinSize : integer);

    procedure Clear;

    // for a one time use with an array of arbitrary size which we do not want to store
    // this algorithm actually implements a (numericall not soooo stable) one pass algorithm
    // but it's ok for standard things.
    procedure AddVal( const Value : double );
    procedure ReadMeanVar( var aMean, aVar : double );

    // numerically more stable variance/mean estimation based on the given winsize
    procedure FeedValWelford(const value : double; var aMean, aVar : double);  // preferred method (num stable and one pass)
    // Direct but numerically more unstable implementation. Use Welford instead
    procedure FeedValOnePass(const value : double; var aMean, aVar : double);
  end;

implementation

uses Math;

{$IFNDEF DEBUG_MEDIAN}
{$R-}{$Q-}
{$ENDIF}

{ TRollingMedian }

procedure TRollingMedian.mexchange(i, j: integer);
var t : integer;
    r : integer;
begin
     t := fHeapArr^[i];
     r := fHeapArr^[j];
     fHeapArr^[i] := r;
     fHeapArr^[j] := t;

     fPos[r] := i;
     fPos[t] := j;
end;

function TRollingMedian.mless(i, j: integer): boolean;
begin
     Result := fBuf[fHeapArr^[i]] < fBuf[fHeapArr^[j]];
end;


procedure TRollingMedian.Clear;
var counter : integer;
    sign : integer;
begin
     fMinCount := 0;
     fMaxCount := 0;
     fBufIdx := 0;
     if (fOrder - 1) and $01 = 1
     then
         sign := -1
     else
         sign := 1;

     //set up initial heap fill pattern: median,max,min,max,...
     for counter := fOrder - 1 downto 0 do
     begin
          fPos[counter] := ((counter + 1) div 2)*sign;
          sign := sign*-1;
          fHeapArr^[ fPos[counter] ] := counter;
     end;
end;

constructor TRollingMedian.Create(order: SmallInt);
begin
     inherited Create;

     fOrder := order;
     fOrderD2 := order div 2;
     fOrderD2M1 := (order - 1) div 2;
     SetLength(fBuf, fOrder);
     SetLength(fPos, fOrder);
     SetLength(fHeapArrBuf, fOrder);

     // pointer trick to avoid the constant addition with fOrderD2 (gains 5-10% speed)
     fHeapArr := PMedianIntArr(Integer(@fHeapArrBuf[fOrderD2]) + Low(TMedianIntArr)*sizeof(fHeapArrBuf[0]) );

     fSwingIn := fOrder + 1;
     fDelay := fOrder div 2;

     Clear;
end;

procedure TRollingMedian.FilterVals( buf : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt );
var i: Integer;
begin
     for i := 0 to N - 1 do
     begin
          buf^ := FilterVal( buf^ );
          inc(PByte(buf), LineWidth);
     end;
end;

//Inserts item, maintains median in O(lg nItems)
function TRollingMedian.FilterVal(const samp: double): double;
var p : integer;
    old : double;
begin
     p := fPos[fBufIdx];
     old := fBuf[fBufIdx];
     fBuf[fBufIdx] := samp;
     inc(fBufIdx);
     if fBufIdx >= fOrder then
        fBufIdx := 0;

     if p > 0 then
     begin
          //new item is in minHeap
          if fMinCount < fOrderD2M1
          then
              inc(fMinCount)
          else if samp > old then
          begin
               minSortDown(p);

               //returns median item (or average of 2 when item count is even)
               Result := fBuf[fHeapArr^[0]];
               if fMinCount < fMaxCount then
                  Result := (Result + fBuf[fHeapArr^[-1]])/2;
               exit;
          end;

          if minSortUp(p) then
          begin
               if mless(0, -1) then
               begin
                    mexchange(0, -1);
                    maxSortDown(-1);
               end;
          end;
     end
     else if p < 0 then
     begin
          //new item is in maxheap
          if fMaxCount < fOrderD2
          then
              inc(fMaxCount)
          else
          begin
               if samp < old then
               begin
                    maxSortDown(p);

                    //returns median item (or average of 2 when item count is even)
                    Result := fBuf[fHeapArr^[0]];
                    if fMinCount < fMaxCount then
                       Result := (Result + fBuf[fHeapArr^[-1]])/2;
                    exit;
               end;
          end;

          if maxSortUp(p) and (fMinCount > 0) then
          begin
               if mless(1, 0) then
               begin
                    mexchange(1, 0);
                    minSortDown(1);
               end;
          end;
     end
     else
     begin
          // new item is median
          if (fMaxCount > 0) and maxSortUp(-1) then
             maxSortDown(-1);
          if (fMinCount > 0) and minSortUp(1) then
             minSortDown(1);
     end;

     //returns median item (or average of 2 when item count is even)
     Result := fBuf[fHeapArr^[0]];
     if fMinCount < fMaxCount then
        Result := (Result + fBuf[fHeapArr^[-1]])/2;
end;

procedure TRollingMedian.maxSortDown(i: integer);
var i2 : integer;
begin
     i2 := i;
     i := i*2;
     while i >= -fMaxCount do
     begin
          if (i > -fMaxCount) and mless(i, i - 1)  then
             dec(i);

          if not mless(i2, i) then
             break;

          mexchange(i2, i);

          i2 := i;
          i := i * 2;
     end;
end;

function TRollingMedian.maxSortUp(i: integer): boolean;
var i2 : integer;
begin
     while (i < 0) do
     begin
          i2 := i div 2;

          if not mless(i2, i) then
             break;

          mexchange(i2, i);
          i := i2;
     end;

     Result := i = 0;
end;


procedure TRollingMedian.minSortDown(i: integer);
var i2 : integer;
begin
     i2 := i;
     i := i*2;
     while i <= fMinCount do
     begin
          if (i < fMinCount) and (mless(i + 1, i)) then
             inc(i);

          if not mless(i, i2) then
             break;

          mexchange(i, i2);

          i2 := i;
          i := i * 2;
     end;
end;

function TRollingMedian.minSortUp(i: integer): boolean;
var i2 : integer;
begin
     while (i > 0) do
     begin
          i2 := i div 2;
          if not mless(i, i2) then
             break;

          mexchange(i, i2);

          i := i2;
     end;

     Result := i = 0;
end;


// ###########################################
// #### Moving/rolling mean variance
// ###########################################

constructor TMovingMeanVar.Create(aWinSize: integer);
begin
     fWinSize := aWinSize;
     if aWinSize > 0 then
     begin
          SetLength(fX, aWinSize + 1);
          SetLength(fX2, aWinSize + 1);
     end;

     Clear;

     inherited Create;
end;

procedure TMovingMeanVar.Clear;
begin
     fIdx := 0;
     fIdx2 := 1;
     if Length(fX) > 0 then
     begin
          FillChar(fX[0], Length(fX)*sizeof(double), 0);
          FillChar(fX2[0], Length(fX2)*sizeof(double), 0);
     end;
     fCnt := 0;
     fSx1 := 0;
     fSx2 := 0;
end;

procedure TMovingMeanVar.FeedValOnePass(const value: double; var aMean, aVar: double);
begin
     // numerically unstable but direct implementation of the base mean variance calculation
     fX[fIdx] := Value;
     fX2[fIdx] := sqr(Value);

     fsx1 := fsx1 + fX[fIdx] - fX[fIdx2];
     fsx2 := fsx2 + fX2[fIdx] - fX2[fIdx2];

     inc(fIdx);
     if fIdx > fWinSize then
        fIdx := 0;
     inc(fIdx2);
     if fIdx2 > fWinSize then
        fIdx2 := 0;

     inc(fCnt);
     if fCnt >= Cardinal(fWinSize) then
     begin
          aMean := fsx1/fWinSize;
          aVar := (fWinSize*fSx2 - sqr(fsx1))/(fWinSize*(fWinSize - 1));
     end
     else
     begin
          aMean := 0;
          aVar := 0;
     end;
end;

procedure TMovingMeanVar.FeedValWelford(const value: double; var aMean,
  aVar: double);
var newMean : double;
begin
     // numerically stable one pass variance algorithm for sliding windows
     fX[fIdx] := value;

     newMean := fSx1 + (value - fX[fIdx2])/fWinSize;
     fSx2 := fSx2 + (value - fSx1)*(value - newMean) - (fX[fIdx2] - fSx1)*(fX[fIdx2] - newMean);
     fSx1 := newMean;

     inc(fIdx);
     if fIdx > fWinSize then
        fIdx := 0;
     inc(fIdx2);
     if fIdx2 > fWinSize then
        fIdx2 := 0;

     inc(fCnt);
     if fCnt >= Cardinal(fWinSize) then
     begin
          aMean := fsx1;
          aVar := fSx2/(fWinSize - 1);
     end
     else
     begin
          aMean := 0;
          aVar := 0;
     end;
end;

procedure TMovingMeanVar.AddVal(const Value: double);
begin
     fSx1 := fSx1 + Value;
     fSx2 := fSx2 + sqr(Value);
     inc(fCnt);
end;

procedure TMovingMeanVar.ReadMeanVar(var aMean, aVar: double);
var denom : double;
begin
     assert(fCnt >= 2, 'At least 2 values need to be feeded');
     aMean := fsx1/fCnt;
     denom := fCnt*1.0*(fCnt - 1);
     aVar := Max(0, (fCnt*fSx2 - sqr(fsx1)))/denom;
end;


end.
