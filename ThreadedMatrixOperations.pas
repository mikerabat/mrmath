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

unit ThreadedMatrixOperations;

interface

{$IFDEF FPC} {$MODESWITCH ADVANCEDRECORDS} {$ENDIF}

uses MatrixConst;

procedure ThrMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ThrMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone);
procedure ThrMatrixMultEx(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
                          width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
                          const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt;
                          op : TMatrixMultDestOperation; mem : PDouble); overload;

procedure ThrMatrixMultT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
                          width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone);
procedure ThrMatrixMultT1Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
  width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);

procedure ThrMatrixMultT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone);
procedure ThrMatrixMultT2Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
                            width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt;
                            height2 : TASMNativeInt; const LineWidth1,
                            LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt;
                            op : TMatrixMultDestOperation; mem : PDouble);

procedure ThrMatrixVecMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, v : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidthMt, LineWidthV : TASMNativeInt; const Alpha, Beta : double);
procedure ThrMatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure ThrMatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);

procedure ThrMatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ThrMatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ThrMatrixMedian(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
procedure ThrMatrixSort(dest : PDouble; destLineWidth : TASMNativeInt; width, height : integer; RowWise : boolean);

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;

{$IFDEF FPC}
   {.$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

{$IFDEF ANONMETHODS}
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFuncRef); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFuncRef); overload;
{$ENDIF}

// helper functions
function NumCoresToUseForMult(width1, height1, width2, height2, blockSize : TASMNativeInt) : integer;

implementation

uses  MtxThreadPool, MatrixASMStubSwitch, BlockSizeSetup, BlockedMult,
      Windows, SysUtils, MtxTimer;

type
  TMatrixAddSubFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

type
  TAsyncMultRec = record //class(TObject)
    thrIdx : TASMNativeInt;

    op : TMatrixMultDestOperation;
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble;
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    width2 : TASMNativeInt;
    height2 : TASMNativeInt;
    LineWidth1, LineWidth2 : TASMNativeInt;
    mem : PDouble;
    BlockSize : integer;

    procedure Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, amt2 : PDouble; awidth1 : TASMNativeInt; aheight1 : TASMNativeInt;
                       awidth2, aheight2 : TASMNativeInt; const aLineWidth1, aLineWidth2 : TASMNativeInt; aOp : TMatrixMultDestOperation);
  end;
  PAsyncMultRec = ^TAsyncMultRec;

type
  TAsyncMatrixVectorMultRec = record
  public
    thrIdx : TASMNativeInt;

    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, V : PDouble;
    width : TASMNativeInt;
    height : TASMNativeInt;
    LineWidthMt : TASMNativeInt;
    LineWidthV : TASMNativeInt;
    Alpha, Beta : double;

    procedure Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, aV : PDouble; awidth : TASMNativeInt; aheight : TASMNativeInt;
                       const aLineWidthMT, aLineWidthV : TASMNativeInt; const aAlpha, aBeta : double);
  end;
  PAsyncMatrixVectorMultRec = ^TAsyncMatrixVectorMultRec;

type
  TAsyncMatrixAddAndScaleRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    Width, Height : TASMNativeInt;
    Offset, Scale : double;

    procedure Create(aDest : PDouble; const aDestLineWidth : TASMNativeInt; aWidth, aHeight : TASMNativeInt; const aOffset, aScale : double);
  end;
  PAsyncMatrixAddAndScaleRec = ^TAsyncMatrixAddAndScaleRec;

type
  TAsyncMatrixAddSubRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble;
    width : TASMNativeInt;
    height : TASMNativeInt;
    LineWidth1, LineWidth2 : TASMNativeInt;
    func : TMatrixAddSubFunc;

    procedure Create(aDest : PDouble; aDestLineWidth : TASMNativeInt; aMt1, aMt2 : PDouble; aWidth, aHeight : TASMNativeInt; aLineWidth1, aLineWidth2 : TASMNativeInt);
  end;
  PAsyncMatrixAddSubRec = ^TAsyncMatrixAddSubRec;
type
  TAsyncMatrixMedianRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    Src : PDouble;
    srcLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    rowWise : boolean;
    hlpMem : PDouble;
  end;
  PAsyncMatrixMedianRec = ^TAsyncMatrixMedianRec;
type
  TAsyncMatrixSortRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    rowWise : boolean;
    hlpMem : PDouble;
  end;
  PAsyncMatrixSortRec = ^TAsyncMatrixSortRec;

type
  TAsyncMatrixFuncRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixFunc;
  end;
  PAsyncMatrixFuncRec = ^TAsyncMatrixFuncRec;
type
  TAsyncMatrixFuncObjRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixObjFunc;
  end;
  PAsyncMatrixFuncObjRec = ^TAsyncMatrixFuncObjRec;
type
  TAsyncMatrixFuncRefRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixMtxRefFunc;
  end;
  PAsyncMatrixFuncRefRec = ^TAsyncMatrixFuncRefRec;
type
  TAsyncMatrixFuncRefObjRec = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixMtxRefObjFunc;
  end;
  PAsyncMatrixFuncRefObjRec = ^TAsyncMatrixFuncRefObjRec;

{$IFDEF ANONMETHODS}
type
  TAsyncMatrixFuncRefRecAnon = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixMtxRefFuncRef;
  end;
  PAsyncMatrixFuncRefRecAnon = ^TAsyncMatrixFuncRefRecAnon;

type
  TAsyncMatrixFuncRecAnon = record
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixFuncRef;
  end;
  PAsyncMatrixFuncRecAnon = ^TAsyncMatrixFuncRecAnon;
{$ENDIF}


procedure MatrixFuncFunc(obj : Pointer);
begin
     MatrixFunc(PAsyncMatrixFuncRec(obj)^.dest,
                PAsyncMatrixFuncRec(obj)^.destLineWidth,
                PAsyncMatrixFuncRec(obj)^.width,
                PAsyncMatrixFuncRec(obj)^.height,
                PAsyncMatrixFuncRec(obj)^.func);
end;

procedure MatrixFuncObjFunc(obj : Pointer);
begin
     MatrixFunc(PAsyncMatrixFuncObjRec(obj)^.dest,
                PAsyncMatrixFuncObjRec(obj)^.destLineWidth,
                PAsyncMatrixFuncObjRec(obj)^.width,
                PAsyncMatrixFuncObjRec(obj)^.height,
                PAsyncMatrixFuncObjRec(obj)^.func);
end;

procedure MatrixFuncRefFunc(obj : Pointer);
begin
     MatrixFunc(PAsyncMatrixFuncRefRec(obj)^.dest,
                PAsyncMatrixFuncRefRec(obj)^.destLineWidth,
                PAsyncMatrixFuncRefRec(obj)^.width,
                PAsyncMatrixFuncRefRec(obj)^.height,
                PAsyncMatrixFuncRefRec(obj)^.func);
end;

procedure MatrixFuncRefObjFunc(obj : Pointer);
begin
     MatrixFunc(PAsyncMatrixFuncRefObjRec(obj)^.dest,
                PAsyncMatrixFuncRefObjRec(obj)^.destLineWidth,
                PAsyncMatrixFuncRefObjRec(obj)^.width,
                PAsyncMatrixFuncRefObjRec(obj)^.height,
                PAsyncMatrixFuncRefObjRec(obj)^.func);
end;

procedure MatrixMultFunc(obj : Pointer);
begin
     if (PAsyncMultRec(obj)^.op = doNone) and not Assigned(PAsyncMultRec(obj)^.mem)
     then
         MatrixMultEx(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, BlockMatrixCacheSize, doNone, nil)
     else
         BlockMatrixMultiplication(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, PAsyncMultRec(obj)^.BlockSize, PAsyncMultRec(obj)^.op,
                    PAsyncMultRec(obj)^.mem);
end;

procedure MatrixMultT1Func(obj : Pointer);
begin
     if (PAsyncMultRec(obj)^.op = doNone) and not Assigned(PAsyncMultRec(obj)^.mem)
     then
         MatrixMultT1Ex(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, BlockMatrixCacheSize, doNone, nil)
     else
         BlockMatrixMultiplicationT1(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, PAsyncMultRec(obj)^.BlockSize, PAsyncMultRec(obj)^.op,
                    PAsyncMultRec(obj)^.mem);
end;

procedure MatrixMultT2Func(obj : Pointer);
begin
     if (PAsyncMultRec(obj)^.op = doNone) and not Assigned(PAsyncMultRec(obj)^.mem)
     then
         MatrixMultT2Ex(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, BlockMatrixCacheSize, PAsyncMultRec(obj)^.op, nil )
     else
         BlockMatrixMultiplicationT2(PAsyncMultRec(obj)^.dest,
                    PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                    PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                    PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                    PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                    PAsyncMultRec(obj)^.LineWidth2, PAsyncMultRec(obj)^.BlockSize, PAsyncMultRec(obj)^.op,
                    PAsyncMultRec(obj)^.mem);
end;

procedure MatrixMultDirectFunc(obj : Pointer);
begin
     BlockMatrixMultiplicationDirect(PAsyncMultRec(obj)^.dest,
                                 PAsyncMultRec(obj)^.destLineWidth, PAsyncMultRec(obj)^.mt1,
                                 PAsyncMultRec(obj)^.mt2, PAsyncMultRec(obj)^.width1,
                                 PAsyncMultRec(obj)^.height1, PAsyncMultRec(obj)^.width2,
                                 PAsyncMultRec(obj)^.height2, PAsyncMultRec(obj)^.LineWidth1,
                                 PAsyncMultRec(obj)^.LineWidth2);
end;

procedure MatrixVectorMultFunc(obj : Pointer);
begin
     MatrixMtxVecMult(PAsyncMatrixVectorMultRec(obj)^.dest,
                      PAsyncMatrixVectorMultRec(obj)^.destLineWidth,
                      PAsyncMatrixVectorMultRec(obj)^.mt1,
                      PAsyncMatrixVectorMultRec(obj)^.v,
                      PAsyncMatrixVectorMultRec(obj)^.LineWidthMt,
                      PAsyncMatrixVectorMultRec(obj)^.LineWidthV,
                      PAsyncMatrixVectorMultRec(obj)^.width,
                      PAsyncMatrixVectorMultRec(obj)^.height,
                      PAsyncMatrixVectorMultRec(obj)^.Alpha,
                      PAsyncMatrixVectorMultRec(obj)^.Beta);
end;

procedure MatrixAddAndScaleFunc(obj : Pointer);
begin
     MatrixAddAndScale(PAsyncMatrixAddAndScaleRec(obj)^.dest,
                       PAsyncMatrixAddAndScaleRec(obj)^.destLineWidth,
                       PAsyncMatrixAddAndScaleRec(obj)^.Width,
                       PAsyncMatrixAddAndScaleRec(obj)^.Height,
                       PAsyncMatrixAddAndScaleRec(obj)^.Offset,
                       PAsyncMatrixAddAndScaleRec(obj)^.Scale);
end;

procedure MatrixScaleAndAddFunc(obj : Pointer);
begin
     MatrixScaleAndAdd(PAsyncMatrixAddAndScaleRec(obj)^.dest,
                       PAsyncMatrixAddAndScaleRec(obj)^.destLineWidth,
                       PAsyncMatrixAddAndScaleRec(obj)^.Width,
                       PAsyncMatrixAddAndScaleRec(obj)^.Height,
                       PAsyncMatrixAddAndScaleRec(obj)^.Offset,
                       PAsyncMatrixAddAndScaleRec(obj)^.Scale);
end;


procedure MatrixMedianFunc(obj : Pointer);
begin
     MatrixMedian(PAsyncMatrixMedianRec(obj)^.dest,
                  PAsyncMatrixMedianRec(obj)^.destLineWidth,
                  PAsyncMatrixMedianRec(obj)^.Src,
                  PAsyncMatrixMedianRec(obj)^.srcLineWidth,
                  PAsyncMatrixMedianRec(obj)^.width,
                  PAsyncMatrixMedianRec(obj)^.height,
                  PAsyncMatrixMedianRec(obj)^.rowWise,
                  PAsyncMatrixMedianRec(obj)^.hlpMem);
end;

procedure MatrixSortFunc(obj : Pointer);
begin
     MatrixSort(PAsyncMatrixSortRec(obj)^.dest,
                PAsyncMatrixSortRec(obj)^.destLineWidth,
                PAsyncMatrixSortRec(obj)^.width,
                PAsyncMatrixSortRec(obj)^.height,
                PAsyncMatrixSortRec(obj)^.rowWise,
                PAsyncMatrixSortRec(obj)^.hlpMem);
end;


procedure MatrixAddSubFunc(obj : Pointer);
begin
     PAsyncMatrixAddSubRec(obj)^.func(
               PAsyncMatrixAddSubRec(obj)^.dest,
               PAsyncMatrixAddSubRec(obj)^.destLineWidth,
               PAsyncMatrixAddSubRec(obj)^.mt1,
               PAsyncMatrixAddSubRec(obj)^.mt2,
               PAsyncMatrixAddSubRec(obj)^.width,
               PAsyncMatrixAddSubRec(obj)^.height,
               PAsyncMatrixAddSubRec(obj)^.LineWidth1,
               PAsyncMatrixAddSubRec(obj)^.LineWidth2);
end;

function NumCoresToUseForMult(width1, height1, width2, height2, blockSize : TASMNativeInt) : integer;
var numBlocks : integer;
begin
     if (width1 <= blockSize) and (width2 <= blockSize) then
     begin
          Result := 1;

          if height1 > blockSize then
             Result := Min(numCPUCores, 1 + height1 div blockSize);
     end
     else
     begin
          numBlocks := height1 div Blocksize + width2 div BlockSize;

          // at least 2 block per core:
          Result := Max(1, Min(numCPUCores, numBlocks ) );
     end;
end;

procedure ThrMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    doBreak : boolean;
    numUsedCores : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMultRec;
    numUsed : integer;

begin
     numUsedCores := NumCoresToUseForMult(width1, height1, width2, height2, BlockMatrixCacheSize);

     if numUsedCores = 1 then
     begin
          BlockMatrixMultiplicationDirect(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
          exit;
     end;
     cpud2 := numUsedCores div 2;
     thrWidth := width2;
     if cpud2 > 1 then
     begin
          heightCores := cpud2;
          cpud2 := numUsedCores div heightCores;

          widthFits := (width2 mod cpud2) = 0;
          thrWidth := (width2 div cpud2) + TASMNativeInt(not widthFits);

          heightFits := (height1 mod cpud2) = 0;
          thrHeight := height1 div cpud2 + TASMNativeInt(not heightFits);
          heightCores := cpud2;
     end
     else
     begin
          cpud2 := 1;
          heightFits := (height1 mod numUsedCores) = 0;
          thrHeight := height1 div numUsedCores + TASMNativeInt(not heightFits);
          heightCores := numUsedCores;
     end;

     // #################################################
     // ##### Prepare thread objects
     calls := MtxInitTaskGroup;
     doBreak := False;
     numUsed := 0;

     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to heightCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, doNone);
               objs[numUsed].thrIdx := (j*heightCores) + i;
               inc(PByte(objs[numUsed].dest), i*thrHeight*destLineWidth);
               inc(PByte(objs[numUsed].mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   objs[numUsed].height1 := thrHeight
               else
                   objs[numUsed].height1 := objs[numUsed].height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   objs[numUsed].width2 := thrWidth
               else
                   objs[numUsed].width2 := objs[numUsed].width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (objs[numUsed].height1 <= 0) or (objs[numUsed].width2 <= 0) then
               begin
                    doBreak := True;
                    break;
               end;

               inc(objs[numUsed].dest, j*thrWidth);
               inc(objs[numUsed].mt2, j*thrWidth);

               inc(numUsed);
          end;

          if doBreak then
             break;
     end;

     // ################################################
     // #### Execute threads
     if numUsed = 0 then
        exit;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixMultDirectFunc, @objs[i]);

     MatrixMultDirectFunc(@objs[numUsed - 1]);
     calls.SyncAll;
end;

procedure ThrMatrixMultEx(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
                          const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
var i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    usedCores : integer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMultRec;
    numUsed : integer;
begin
     if UseInnerBlockMult(width1, height1) then
     begin
          ThrBlockMatrixMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
          exit;
     end;
     // #############################################
     // #### determine a good number of cores
     // -> do not use to many for smaller matrices
     usedCores := NumCoresToUseForMult(width1, height1, width2, height2, blockSize);

     if usedCores = 1 then
     begin
          MatrixMultEx(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
          exit;
     end;

     numUsed := 0;
     cpud2 := usedCores div 2;
     thrWidth := width2;
     if cpud2 > 1 then
     begin
          heightCores := cpud2;
          cpud2 := usedCores div heightCores;

          widthFits := (width2 mod cpud2) = 0;
          // ensure that we can do aligned operations on matrices
          thrWidth := (width2 div cpud2) + TASMNativeInt(not widthFits);
          thrWidth := thrWidth + thrWidth and $1;

          heightFits := (height1 mod heightCores) = 0;
          thrHeight := height1 div heightCores + TASMNativeInt(not heightFits);
     end
     else
     begin
          cpud2 := 1;
          heightFits := (height1 mod usedCores) = 0;
          thrHeight := height1 div usedCores + TASMNativeInt(not heightFits);
          thrHeight := thrHeight + thrHeight and $1;
          heightCores := usedCores;
     end;

     // ####################################################
     // #### Prepare thread objects
     calls := MtxInitTaskGroup;

     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to heightCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                                     LineWidth1, LineWidth2, op);
               objs[numUsed].thrIdx := (j*heightCores) + i;
               inc(PByte(objs[numUsed].dest), i*thrHeight*destLineWidth);
               inc(PByte(objs[numUsed].mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   objs[numUsed].height1 := thrHeight
               else
                   objs[numUsed].height1 := objs[numUsed].height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   objs[numUsed].width2 := thrWidth
               else
                   objs[numUsed].width2 := objs[numUsed].width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (objs[numUsed].height1 <= 0) or (objs[numUsed].width2 <= 0) then
                   break;

               inc(objs[numUsed].dest, j*thrWidth);
               inc(objs[numUsed].mt2, j*thrWidth);

               if Assigned(mem) then
               begin
                    objs[numUsed].mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               objs[numUsed].BlockSize := blockSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ####################################################
     // #### Execute threads

     // start threads
     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixMultFunc, @objs[i]);

     // use the current thread for the last element
     MatrixMultFunc(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;
end;

procedure ThrMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt;
  height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt;
  op : TMatrixMultDestOperation);
begin
     ThrMatrixMultEx(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

procedure ThrMatrixMultT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone);
begin
     ThrMatrixMultT1Ex(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

procedure ThrMatrixMultT1Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
  width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
var i, j : TASMNativeInt;
    thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, widthFits1 : boolean;
    widthCores : TASMNativeInt;
    thrWidth1 : integer;
    usedCores : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMultRec;
    numUsed : integer;
begin
     // #############################################
     // #### determine a good number of cores
     // -> do not use to many for smaller matrices
     usedCores := NumCoresToUseForMult(height1, width1, width2, height2, blockSize);

     if usedCores = 1 then
     begin
          MatrixMultT1Ex(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2,
                         blockSize, op, mem);
          exit;
     end;

     // calculates mt1'*mt2
     cpud2 := usedCores div 2;
     thrWidth := width2;
     if cpud2 > 1 then
     begin
          widthCores := cpud2;
          cpud2 := usedCores div widthCores;

          widthFits := (width2 mod cpud2) = 0;
          // ensure that we can do aligned operations on matrices
          thrWidth := (width2 div cpud2) + TASMNativeInt(not widthFits);
          thrWidth := thrWidth + thrWidth and $1;

          widthFits1 := (width1 mod widthCores) = 0;
          thrWidth1 := width1 div widthCores + TASMNativeInt(not widthFits1);
          thrWidth1 := thrWidth1 + thrWidth1 and $1;
     end
     else
     begin
          cpud2 := 1;
          widthFits1 := (width1 mod usedCores) = 0;
          thrWidth1 := width1 div usedCores + TASMNativeInt(not widthFits1);
          thrWidth1 := thrWidth1 + thrWidth1 and $1;
          widthCores := usedCores;
     end;

     // #################################################
     // #### Prepare thread objects
     numUsed := 0;
     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to widthCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                           LineWidth1, LineWidth2, op);
               objs[numUsed].thrIdx := (j*widthCores) + i;
               inc(PByte(objs[numUsed].dest), i*thrWidth1*destLineWidth);
               inc(objs[numUsed].mt1, i*thrWidth1);
               if width1 > (i + 1)*thrWidth1
               then
                   objs[numUsed].width1 := thrWidth1
               else
                   objs[numUsed].width1 := objs[numUsed].width1 - i*thrWidth1;

               if width2 > (j + 1)*thrWidth
               then
                   objs[numUsed].width2 := thrWidth
               else
                   objs[numUsed].width2 := objs[numUsed].width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (objs[numUsed].width1 <= 0) or (objs[numUsed].width2 <= 0) then
                  break;

               inc(objs[numUsed].dest, j*thrWidth);
               inc(objs[numUsed].mt2, j*thrWidth);

               if Assigned(mem) then
               begin
                    objs[numUsed].mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               objs[numUsed].BlockSize := blockSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ####################################################
     // #### Execute threads

     calls := MtxInitTaskGroup;

     // start threads
     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixMultT1Func, @objs[i]);

     // use the current thread for the last element
     MatrixMultT1Func(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;
end;

procedure ThrMatrixMultT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone);
begin
     ThrMatrixMultT2Ex(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

// calculates mt1*mt2'
procedure ThrMatrixMultT2Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1,
  LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
var i, j : TASMNativeInt;
    thrHeight, thrHeight1 : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    heightFits1, heightFits : boolean;
    heightCores : TASMNativeInt;
    usedCores : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMultRec;
    numUsed : integer;
begin
     if UseInnerBlockMult(width1, height1) then
     begin
          ThrBlockMatrixMultiplicationT2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
          exit;
     end;

     usedCores := NumCoresToUseForMult(width1, height1, height2, width2, blockSize);

     if usedCores = 1 then
     begin
          MatrixMultT2Ex(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2,
                         blockSize, op, mem);
          exit;
     end;


     cpud2 := usedCores div 2;
     thrHeight1 := height2;
     if cpud2 > 1 then
     begin
          heightCores := cpud2;
          cpud2 := usedCores div heightCores;

          heightFits1 := (height2 mod cpud2) = 0;
          // ensure that we can do aligned operations on matrices
          thrHeight1 := (height2 div cpud2) + TASMNativeInt(not heightFits1);
          thrHeight1 := thrHeight1 + thrHeight1 and $1;

          heightFits := (height1 mod heightCores) = 0;
          thrHeight := height1 div heightCores + TASMNativeInt(not heightFits);
          thrHeight := thrHeight + thrHeight and $1;
     end
     else
     begin
          cpud2 := 1;
          heightFits := (height1 mod usedCores) = 0;
          thrHeight := height1 div usedCores + TASMNativeInt(not heightFits);
          thrHeight := thrHeight + thrHeight and $1;
          heightCores := usedCores;
     end;

     // #################################################
     // #### Prepare thread objects
     numUsed := 0;

     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to heightCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                                     LineWidth1, LineWidth2, op);
               objs[numUsed].thrIdx := (j*heightCores) + i;
               inc(PByte(objs[numUsed].dest), i*thrHeight*destLineWidth);
               inc(PByte(objs[numUsed].mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   objs[numUsed].height1 := thrHeight
               else
                   objs[numUsed].height1 := objs[numUsed].height1 - i*thrHeight;

               if height2 > (j + 1)*thrHeight1
               then
                   objs[numUsed].height2 := thrHeight1
               else
                   objs[numUsed].height2 := objs[numUsed].height2 - j*thrHeight1;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (objs[numUsed].height1 <= 0) or (objs[numUsed].height2 <= 0) then
                  break;

               inc(objs[numUsed].dest, j*thrHeight1);
               inc(PByte(objs[numUsed].mt2), j*thrHeight1*LineWidth2);

               if Assigned(mem) then
               begin
                    objs[numUsed].mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               objs[numUsed].BlockSize := blockSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixMultT2Func, @objs[i]);

     MatrixMultT2Func(@objs[numUsed - 1]);

     calls.SyncAll;
end;


procedure ThrMatrixVecMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, v : PDouble; width : TASMNativeInt; height : TASMNativeInt;
  const LineWidthMt, LineWidthV : TASMNativeInt; const Alpha, Beta : double);
var i : TASMNativeInt;
    thrHeight : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    heightFits : boolean;
    numTask : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixVectorMultRec;
    numUsed : integer;
begin
     heightFits := (height mod numCPUCores) = 0;
     thrHeight := Max(8, height div numCPUCores + TASMNativeInt(not heightFits) );

     // ###############################################
     // #### Prepare thread objects
     numTask := Min(numCPUCores, height div thrHeight + 1);
     numUsed := 0;

     for i := 0 to numTask - 1 do
     begin
          objs[numUsed].Create(dest, destLineWidth, mt1, v, width, height,
                               LineWidthMt, LineWidthV, alpha, beta);
          objs[numUsed].thrIdx := i;
          inc(PByte(objs[numUsed].dest), i*thrHeight*destLineWidth);
          inc(PByte(objs[numUsed].mt1), i*thrHeight*LineWidthMt);
          if height > (i + 1)*thrHeight
          then
              objs[numUsed].height := thrHeight
          else
              objs[numUsed].height := objs[numUsed].height - i*thrHeight;

          inc(numUsed);

          if height <= (i + 1)*thrHeight then
             break;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixVectorMultFunc, @objs[i]);

     MatrixVectorMultFunc(@objs[numUsed - 1]);

     calls.SyncAll;
end;

procedure ThrMatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddAndScaleRec;
    numUsed : integer;
begin
     // check if it is really necessary to split the call:
     if (width < BlockMatrixCacheSize) and (height < BlockMatrixCacheSize) then
     begin
          MatrixScaleAndAdd(Dest, LineWidth, Width, height, offset, scale);
          exit;
     end;

     // ###############################################
     // #### Prepare thread objects
     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCoresForSimpleFuncs) = 0;
          thrSize := width div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               objs[numUsed].Create(dest, LineWidth, thrSize, height, Offset, Scale);

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCoresForSimpleFuncs) = 0;
          thrSize := height div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               objs[numUsed].Create(dest, LineWidth, width, thrSize, Offset, Scale);

               inc(PByte(objs[numUsed].dest), i*thrSize*LineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixScaleAndAddFunc, @objs[i]);

     MatrixScaleAndAddFunc(@objs[numUsed - 1]);

     calls.SyncAll;
end;


procedure ThrMatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddAndScaleRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < BlockMatrixCacheSize) and (height < BlockMatrixCacheSize) then
     begin
          MatrixAddAndScale(Dest, LineWidth, Width, height, offset, scale);
          exit;
     end;

     // ###############################################
     // #### Prepare thread objects
     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCoresForSimpleFuncs) = 0;
          thrSize := width div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               objs[numUsed].Create(dest, LineWidth, thrSize, height, Offset, Scale);

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCoresForSimpleFuncs) = 0;
          thrSize := height div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               objs[numUsed].Create(dest, LineWidth, width, thrSize, Offset, Scale);

               inc(PByte(objs[numUsed].dest), i*thrSize*LineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixAddAndScaleFunc, @objs[i]);

     MatrixAddAndScaleFunc(@objs[numUsed - 1]);

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncFunc, Pointer(@objs[i]));

     MatrixFuncFunc(Pointer(@objs[numUsed  - 1]));

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncObjRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;
     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncObjFunc, @objs[i]);

     MatrixFuncObjFunc(@objs[numUsed  - 1]);

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRefRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncRefFunc, @objs[i]);

     MatrixFuncRefFunc(@objs[numUsed  - 1]);

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRefObjRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncRefObjFunc, @objs[i]);

     MatrixFuncRefObjFunc(@objs[numUsed  - 1]);

     calls.SyncAll;
end;

{$IFDEF ANONMETHODS}

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFuncRef);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRecAnon;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncFunc, Pointer(@objs[i]));

     MatrixFuncFunc(Pointer(@objs[numUsed  - 1]));

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFuncRef);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRefRecAnon;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     numUsed := 0;

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height := thrSize;
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixFuncRefObjFunc, @objs[i]);

     MatrixFuncRefObjFunc(@objs[numUsed  - 1]);

     calls.SyncAll;
end;

{$ENDIF}

procedure ThrMatrixAddSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; func : TMatrixAddSubFunc);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddSubRec;
    numUsed : integer;
begin
     // check if it is really necessary to thread the call:
     if (width < BlockMatrixCacheSize) and (height < BlockMatrixCacheSize) then
     begin
          func(Dest, destLineWidth, mt1, mt2, Width, height, LineWidth1, LineWidth2);
          exit;
     end;

     // ################################################
     // #### Prepare thread objects
     numUsed := 0;
     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, Mt1, Mt2, thrSize, height, LineWidth1, LineWidth2);
               objs[numUsed].func := func;

               inc(objs[numUsed].dest, i*thrSize);
               inc(objs[numUsed].mt1, i*thrSize);
               inc(objs[numUsed].mt2, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               objs[numUsed].Create(dest, destLineWidth, Mt1, Mt2, width, thrSize, LineWidth1, LineWidth2);
               objs[numUsed].func := func;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               inc(PByte(objs[numUsed].mt1), i*thrSize*LineWidth1);
               inc(PByte(objs[numUsed].mt2), i*thrSize*LineWidth2);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixAddSubFunc, @objs[i]);

     MatrixAddSubFunc(@objs[numUsed - 1]);

     calls.SyncAll;
end;

procedure ThrMatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     ThrMatrixAddSub(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2, {$IFDEF FPC}@{$ENDIF}MatrixSub);
end;

procedure ThrMatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     ThrMatrixAddSub(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2, {$IFDEF FPC}@{$ENDIF}MatrixAdd);
end;

procedure ThrMatrixSort(dest : PDouble; destLineWidth : TASMNativeInt; width, height : integer; RowWise : boolean);
var i: TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
    maxNumCores : integer;
    hlpMem : PDouble;
    mem : Pointer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixSortRec;
    numUsed : integer;

begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixSort(Dest, destLineWidth, Width, height, RowWise, nil);
          exit;
     end;

     mem := nil;
     numUsed := 0;
     if not RowWise then
     begin
          maxNumCores := Min(width, numCPUCores);

          sizeFits := (width mod maxNumCores) = 0;
          thrSize := width div maxNumCores + TASMNativeInt(not sizeFits);
          hlpMem := MtxMallocAlign( maxNumCores*height*sizeof(double), mem);

          for i := 0 to maxNumCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].height := height;
               objs[numUsed].RowWise := rowWise;
               objs[numUsed].hlpMem := hlpMem;


               inc(objs[numUsed].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
               //calls.AddTaskRec(@MatrixSortFunc, rec);
               inc(hlpMem, height);
          end;
     end
     else
     begin
          maxNumCores := Min(height, numCPUCores);

          sizeFits := (height mod maxNumCores) = 0;
          thrSize := height div maxNumCores + TASMNativeInt(not sizeFits);

          for i := 0 to maxNumCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].height :=  thrSize;
               objs[numUsed].RowWise := rowWise;
               objs[numUsed].hlpMem := nil;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
          end;
     end;

     calls := nil;
     if numUsed > 1 then
        calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixSortFunc, @objs[i]);

     MatrixSortFunc(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;

     if Assigned(mem) then
        FreeMem(mem);
end;

procedure ThrMatrixMedian(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
var i: TASMNativeInt;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixMedianRec;
    numUsed : integer;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
    maxNumCores : integer;
    hlpMem : PDouble;
    mem : Pointer;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixMedian(Dest, destLineWidth, Src, srcLineWidth, Width, height, RowWise, nil);
          exit;
     end;

     numUsed := 0;
     if not RowWise then
     begin
          maxNumCores := Min(width, numCoresForSimpleFuncs);

          sizeFits := (width mod maxNumCores) = 0;
          thrSize := width div maxNumCores + TASMNativeInt(not sizeFits);
          hlpMem := MtxMallocAlign(maxNumCores*height*sizeof(double), mem);

          for i := 0 to maxNumCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].Src := src;
               objs[numUsed].srcLineWidth := srcLineWidth;
               objs[numUsed].width := thrSize;
               objs[numUsed].Height := height;
               objs[numUsed].rowWise := RowWise;
               objs[numUsed].hlpMem := hlpMem;

               inc(objs[numUsed].dest, i*thrSize);
               inc(objs[numUsed].src, i*thrSize);
               if width < (i + 1)*thrSize then
                  objs[numUsed].Width := width - i*thrSize;

               inc(numUsed);
               inc(hlpMem, height);
          end;
     end
     else
     begin
          maxNumCores := Min(height, numCoresForSimpleFuncs);

          hlpMem := MtxMallocAlign(maxNumCores*width*sizeof(double), mem);

          sizeFits := (height mod maxNumCores) = 0;
          thrSize := height div maxNumCores + TASMNativeInt(not sizeFits);

          for i := 0 to maxNumCores - 1 do
          begin
               objs[numUsed].dest := dest;
               objs[numUsed].destLineWidth := destLineWidth;
               objs[numUsed].Src := src;
               objs[numUsed].srcLineWidth := srcLineWidth;
               objs[numUsed].width := width;
               objs[numUsed].Height := thrSize;
               objs[numUsed].rowWise := RowWise;
               objs[numUsed].hlpMem := hlpMem;

               inc(PByte(objs[numUsed].dest), i*thrSize*destLineWidth);
               inc(PByte(objs[numUsed].src), i*thrSize*srcLineWidth);
               if height < (i + 1)*thrSize then
                  objs[numUsed].Height := height - i*thrSize;

               inc(numUsed);
               inc(hlpMem, width);
          end;
     end;

     calls := nil;
     if numUsed > 1 then
        calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixMedianFunc, @objs[i]);

     MatrixMedianFunc(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;

     FreeMem(mem);
end;

{ TAsyncMultRec }

procedure TAsyncMultRec.Create(adest: PDouble; const adestLineWidth: TASMNativeInt;
  amt1, amt2: PDouble; awidth1, aheight1, awidth2, aheight2: TASMNativeInt;
  const aLineWidth1, aLineWidth2: TASMNativeInt; aOp : TMatrixMultDestOperation);
begin
     dest := adest;
     destLineWidth := adestLineWidth;
     mt1 := amt1;
     mt2 := amt2;
     width1 := awidth1;
     height1 := aheight1;
     width2 := awidth2;
     height2 := aheight2;
     LineWidth1 := aLineWidth1;
     LineWidth2 := aLineWidth2;
     op := aop;
     mem := nil;
     BlockSize := BlockMatrixCacheSize;
end;

{ TAsyncMatrixVectorMultRec }

procedure TAsyncMatrixVectorMultRec.Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, aV : PDouble; awidth : TASMNativeInt; aheight : TASMNativeInt;
                       const aLineWidthMT, aLineWidthV : TASMNativeInt; const aAlpha, aBeta : double);
begin
     dest := adest;
     destLineWidth := adestLineWidth;
     mt1 := amt1;
     v := aV;
     width := awidth;
     height := aheight;
     LineWidthMt := aLineWidthMt;
     LineWidthV := aLineWidthV;
     alpha := aAlpha;
     beta := aBeta;
end;

{ TAsyncMatrixAddAndSclaObj }

procedure TAsyncMatrixAddAndScaleRec.Create(aDest: PDouble;
  const aDestLineWidth: TASMNativeInt; aWidth, aHeight: TASMNativeInt; const aOffset,
  aScale: double);
begin
     dest := aDest;
     destLineWidth := aDestLineWidth;
     width := aWidth;
     Height := aHeight;
     offset := aOffset;
     scale := aScale;
end;

{ TAsyncMatrixAddSubRec }

procedure TAsyncMatrixAddSubRec.Create(aDest: PDouble;
  aDestLineWidth: TASMNativeInt; aMt1, aMt2: PDouble; aWidth, aHeight, aLineWidth1,
  aLineWidth2: TASMNativeInt);
begin
     dest := adest;
     destLineWidth := adestLineWidth;
     mt1 := amt1;
     mt2 := amt2;
     width := awidth;
     height := aheight;
     LineWidth1 := aLineWidth1;
     LineWidth2 := aLineWidth2;
end;

end.
