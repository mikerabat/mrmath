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


implementation

uses  MtxThreadPool, ASMMatrixOperations, OptimizedFuncs, BlockSizeSetup, Math;

type
  TMatrixAddSubFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

type
  TAsyncMultObj = class(TObject)
  public
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

    constructor Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, amt2 : PDouble; awidth1 : TASMNativeInt; aheight1 : TASMNativeInt;
                       awidth2, aheight2 : TASMNativeInt; const aLineWidth1, aLineWidth2 : TASMNativeInt; aOp : TMatrixMultDestOperation);
  end;

type
  TAsyncMatrixVectorMultObj = class(TObject)
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

    constructor Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, aV : PDouble; awidth : TASMNativeInt; aheight : TASMNativeInt;
                       const aLineWidthMT, aLineWidthV : TASMNativeInt; const aAlpha, aBeta : double);
  end;

type
  TAsyncMatrixAddAndScaleObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    Width, Height : TASMNativeInt;
    Offset, Scale : double;

    constructor Create(aDest : PDouble; const aDestLineWidth : TASMNativeInt; aWidth, aHeight : TASMNativeInt; const aOffset, aScale : double);
  end;

type
  TAsyncMatrixAddSubObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble;
    width : TASMNativeInt;
    height : TASMNativeInt;
    LineWidth1, LineWidth2 : TASMNativeInt;
    func : TMatrixAddSubFunc;

    constructor Create(aDest : PDouble; aDestLineWidth : TASMNativeInt; aMt1, aMt2 : PDouble; aWidth, aHeight : TASMNativeInt; aLineWidth1, aLineWidth2 : TASMNativeInt);
  end;
type
  TAsyncMatrixMedianObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    Src : PDouble;
    srcLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    rowWise : boolean;
    hlpMem : PDouble;

    constructor Create(aDest : PDouble; aDestLineWidth : TASMNativeInt; aSrc : PDouble; aSrcLineWidth : TASMNativeInt; aWidth, aHeight : TASMNativeInt; aRowWise : boolean; aHelpMem : PDouble);
  end;
type
  TAsyncMatrixSortObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    rowWise : boolean;
    hlpMem : PDouble;

    constructor Create(aDest : PDouble; aDestLineWidth : TASMNativeInt; aWidth, aHeight : TASMNativeInt; aRowWise : boolean; aHelpMem : PDouble);
  end;


type
  TAsyncMatrixFuncObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixFunc;
  end;
type
  TAsyncMatrixFuncObjObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixObjFunc;
  end;
type
  TAsyncMatrixFuncRefObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixMtxRefFunc;
  end;
type
  TAsyncMatrixFuncRefObjObj = class(TObject)
    dest : PDouble;
    destLineWidth : TASMNativeInt;
    width : TASMNativeInt;
    height : TASMNativeInt;
    func : TMatrixMtxRefObjFunc;
  end;

function MatrixFuncFunc(obj : TObject) : integer;
begin
     MatrixFunc(TAsyncMatrixFuncObj(obj).dest,
                TAsyncMatrixFuncObj(obj).destLineWidth,
                TAsyncMatrixFuncObj(obj).width,
                TAsyncMatrixFuncObj(obj).height,
                TAsyncMatrixFuncObj(obj).func);
     Result := 0;
end;

function MatrixFuncObjFunc(obj : TObject) : integer;
begin
     MatrixFunc(TAsyncMatrixFuncObjObj(obj).dest,
                TAsyncMatrixFuncObjObj(obj).destLineWidth,
                TAsyncMatrixFuncObjObj(obj).width,
                TAsyncMatrixFuncObjObj(obj).height,
                TAsyncMatrixFuncObjObj(obj).func);
     Result := 0;
end;

function MatrixFuncRefFunc(obj : TObject) : integer;
begin
     MatrixFunc(TAsyncMatrixFuncRefObj(obj).dest,
                TAsyncMatrixFuncRefObj(obj).destLineWidth,
                TAsyncMatrixFuncRefObj(obj).width,
                TAsyncMatrixFuncRefObj(obj).height,
                TAsyncMatrixFuncRefObj(obj).func);
     Result := 0;
end;

function MatrixFuncRefObjFunc(obj : TObject) : integer;
begin
     MatrixFunc(TAsyncMatrixFuncRefObjObj(obj).dest,
                TAsyncMatrixFuncRefObjObj(obj).destLineWidth,
                TAsyncMatrixFuncRefObjObj(obj).width,
                TAsyncMatrixFuncRefObjObj(obj).height,
                TAsyncMatrixFuncRefObjObj(obj).func);
     Result := 0;
end;

function MatrixMultFunc(obj : TObject) : integer;
begin
     if (TAsyncMultObj(obj).op = doNone) and not Assigned(TAsyncMultObj(obj).mem)
     then
         MatrixMult(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2)
     else
         BlockedMatrixMultiplication(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2, TAsyncMultObj(obj).BlockSize, TAsyncMultObj(obj).op,
                    TAsyncMultObj(obj).mem);

     Result := 0;
end;

function MatrixMultT1Func(obj : TObject) : integer;
begin
     if (TAsyncMultObj(obj).op = doNone) and not Assigned(TAsyncMultObj(obj).mem)
     then
         MatrixMultT1(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2)
     else
         BlockedMatrixMultiplicationT1(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2, TAsyncMultObj(obj).BlockSize, TAsyncMultObj(obj).op,
                    TAsyncMultObj(obj).mem);

     Result := 0;
end;

function MatrixMultT2Func(obj : TObject) : integer;
begin
     if (TAsyncMultObj(obj).op = doNone) and not Assigned(TAsyncMultObj(obj).mem)
     then
         MatrixMultT2(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2)
     else
         BlockedMatrixMultiplicationT2(TAsyncMultObj(obj).dest,
                    TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                    TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                    TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                    TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                    TAsyncMultObj(obj).LineWidth2, TAsyncMultObj(obj).BlockSize, TAsyncMultObj(obj).op,
                    TAsyncMultObj(obj).mem);

     Result := 0;
end;

function MatrixMultDirectFunc(obj : TObject) : integer;
begin
     BlockedMatrixMultiplicationDirect(TAsyncMultObj(obj).dest,
                                 TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                                 TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                                 TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                                 TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                                 TAsyncMultObj(obj).LineWidth2);

     Result := 0;
end;

function MatrixVectorMultFunc(obj : TObject) : integer;
begin
     MatrixMtxVecMult(TAsyncMatrixVectorMultObj(obj).dest,
                      TAsyncMatrixVectorMultObj(obj).destLineWidth,
                      TAsyncMatrixVectorMultObj(obj).mt1,
                      TAsyncMatrixVectorMultObj(obj).v,
                      TAsyncMatrixVectorMultObj(obj).LineWidthMt,
                      TAsyncMatrixVectorMultObj(obj).LineWidthV,
                      TAsyncMatrixVectorMultObj(obj).width,
                      TAsyncMatrixVectorMultObj(obj).height,
                      TAsyncMatrixVectorMultObj(obj).Alpha,
                      TAsyncMatrixVectorMultObj(obj).Beta);

     Result := 0;
end;

function MatrixAddAndScaleFunc(obj : TObject) : integer;
begin
     MatrixAddAndScale(TAsyncMatrixAddAndScaleObj(obj).dest,
                       TAsyncMatrixAddAndScaleObj(obj).destLineWidth,
                       TAsyncMatrixAddAndScaleObj(obj).Width,
                       TAsyncMatrixAddAndScaleObj(obj).Height,
                       TAsyncMatrixAddAndScaleObj(obj).Offset,
                       TAsyncMatrixAddAndScaleObj(obj).Scale);

     Result := 0;
end;

function MatrixScaleAndAddFunc(obj : TObject) : integer;
begin
     MatrixScaleAndAdd(TAsyncMatrixAddAndScaleObj(obj).dest,
                       TAsyncMatrixAddAndScaleObj(obj).destLineWidth,
                       TAsyncMatrixAddAndScaleObj(obj).Width,
                       TAsyncMatrixAddAndScaleObj(obj).Height,
                       TAsyncMatrixAddAndScaleObj(obj).Offset,
                       TAsyncMatrixAddAndScaleObj(obj).Scale);

     Result := 0;
end;


function MatrixMedianFunc(obj : TObject) : integer;
begin
     MatrixMedian(TAsyncMatrixMedianObj(obj).dest,
                  TAsyncMatrixMedianObj(obj).destLineWidth,
                  TAsyncMatrixMedianObj(obj).Src,
                  TAsyncMatrixMedianObj(obj).srcLineWidth,
                  TAsyncMatrixMedianObj(obj).width,
                  TAsyncMatrixMedianObj(obj).height,
                  TAsyncMatrixMedianObj(obj).rowWise,
                  TAsyncMatrixMedianObj(obj).hlpMem);

     Result := 0;
end;

function MatrixSortFunc(obj : TObject) : integer;
begin
     MatrixSort(TAsyncMatrixSortObj(obj).dest,
                TAsyncMatrixSortObj(obj).destLineWidth,
                TAsyncMatrixSortObj(obj).width,
                TAsyncMatrixSortObj(obj).height,
                TAsyncMatrixSortObj(obj).rowWise,
                TAsyncMatrixSortObj(obj).hlpMem);

     Result := 0;
end;


function MatrixAddSubFunc(obj : TObject) : integer;
begin
     TAsyncMatrixAddSubObj(obj).func(
               TAsyncMatrixAddSubObj(obj).dest,
               TAsyncMatrixAddSubObj(obj).destLineWidth,
               TAsyncMatrixAddSubObj(obj).mt1,
               TAsyncMatrixAddSubObj(obj).mt2,
               TAsyncMatrixAddSubObj(obj).width,
               TAsyncMatrixAddSubObj(obj).height,
               TAsyncMatrixAddSubObj(obj).LineWidth1,
               TAsyncMatrixAddSubObj(obj).LineWidth2);

     Result := 0;
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
var obj : TAsyncMultObj;
    i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    doBreak : boolean;
    numUsedCores : integer;

    objs : Array[0..63] of TAsyncMultObj;
    numUsed : integer;

begin
     numUsedCores := NumCoresToUseForMult(width1, height1, width2, height2, BlockMatrixCacheSize);

     if numUsedCores = 1 then
     begin
          BlockedMatrixMultiplicationDirect(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
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
               obj := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, doNone);
               obj.thrIdx := (j*heightCores) + i;
               inc(PByte(obj.dest), i*thrHeight*destLineWidth);
               inc(PByte(obj.mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   obj.height1 := thrHeight
               else
                   obj.height1 := obj.height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   obj.width2 := thrWidth
               else
                   obj.width2 := obj.width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (obj.height1 <= 0) or (obj.width2 <= 0) then
               begin
                    doBreak := True;
                    break;
               end;

               inc(obj.dest, j*thrWidth);
               inc(obj.mt2, j*thrWidth);

               objs[numUsed] := obj;
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
         calls.AddTask(@MatrixMultDirectFunc, objs[i]);

     MatrixMultDirectFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;
     calls.SyncAll;
end;

procedure ThrMatrixMultEx(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
                          const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
var obj : TAsyncMultObj;
    i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    usedCores : integer;
    objs : Array[0..63] of TAsyncMultObj;
    numUsed : integer;
begin
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
               obj := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                                     LineWidth1, LineWidth2, op);
               obj.thrIdx := (j*heightCores) + i;
               inc(PByte(obj.dest), i*thrHeight*destLineWidth);
               inc(PByte(obj.mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   obj.height1 := thrHeight
               else
                   obj.height1 := obj.height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   obj.width2 := thrWidth
               else
                   obj.width2 := obj.width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (obj.height1 <= 0) or (obj.width2 <= 0) then
               begin
                    obj.Free;
                    break;
               end;
               inc(obj.dest, j*thrWidth);
               inc(obj.mt2, j*thrWidth);

               if Assigned(mem) then
               begin
                    obj.mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               obj.BlockSize := blockSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ####################################################
     // #### Execute threads

     // start threads
     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixMultFunc, objs[i]);

     // use the current thread for the last element
     MatrixMultFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

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
var obj : TAsyncMultObj;
    i, j : TASMNativeInt;
    thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    widthFits, widthFits1 : boolean;
    widthCores : TASMNativeInt;
    thrWidth1 : integer;
    usedCores : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMultObj;
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
               obj := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                           LineWidth1, LineWidth2, op);
               obj.thrIdx := (j*widthCores) + i;
               inc(PByte(obj.dest), i*thrWidth1*destLineWidth);
               inc(obj.mt1, i*thrWidth1);
               if width1 > (i + 1)*thrWidth1
               then
                   obj.width1 := thrWidth1
               else
                   obj.width1 := obj.width1 - i*thrWidth1;

               if width2 > (j + 1)*thrWidth
               then
                   obj.width2 := thrWidth
               else
                   obj.width2 := obj.width2 - j*thrWidth;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (obj.width1 <= 0) or (obj.width2 <= 0) then
               begin
                    obj.Free;
                    break;
               end;
               inc(obj.dest, j*thrWidth);
               inc(obj.mt2, j*thrWidth);

               if Assigned(mem) then
               begin
                    obj.mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               obj.BlockSize := blockSize;

               objs[numUsed] := obj;
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
         calls.AddTask(@MatrixMultT1Func, objs[i]);

     // use the current thread for the last element
     MatrixMultT1Func(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

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
var obj : TAsyncMultObj;
    i, j : TASMNativeInt;
    thrHeight, thrHeight1 : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    heightFits1, heightFits : boolean;
    heightCores : TASMNativeInt;
    usedCores : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMultObj;
    numUsed : integer;
begin
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
               obj := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                                     LineWidth1, LineWidth2, op);
               obj.thrIdx := (j*heightCores) + i;
               inc(PByte(obj.dest), i*thrHeight*destLineWidth);
               inc(PByte(obj.mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   obj.height1 := thrHeight
               else
                   obj.height1 := obj.height1 - i*thrHeight;

               if height2 > (j + 1)*thrHeight1
               then
                   obj.height2 := thrHeight1
               else
                   obj.height2 := obj.height2 - j*thrHeight1;

               // check if the number of cores exceeds the matrix dimension -> simply use not so many cores
               if (obj.height1 <= 0) or (obj.height2 <= 0) then
               begin
                    obj.Free;
                    break;
               end;
               inc(obj.dest, j*thrHeight1);
               inc(PByte(obj.mt2), j*thrHeight1*LineWidth2);

               if Assigned(mem) then
               begin
                    obj.mem := mem;
                    inc(mem, 4*blockSize*blockSize + 2);
               end;
               obj.BlockSize := blockSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixMultT2Func, objs[i]);

     MatrixMultT2Func(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

     calls.SyncAll;
end;


procedure ThrMatrixVecMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, v : PDouble; width : TASMNativeInt; height : TASMNativeInt;
  const LineWidthMt, LineWidthV : TASMNativeInt; const Alpha, Beta : double);
var obj : TAsyncMatrixVectorMultObj;
    i : TASMNativeInt;
    thrHeight : TASMNativeInt;
    calls : IMtxAsyncCallGroup;
    heightFits : boolean;
    numTask : integer;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixVectorMultObj;
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
          obj := TAsyncMatrixVectorMultObj.Create(dest, destLineWidth, mt1, v, width, height,
                                                  LineWidthMt, LineWidthV, alpha, beta);
          obj.thrIdx := i;
          inc(PByte(obj.dest), i*thrHeight*destLineWidth);
          inc(PByte(obj.mt1), i*thrHeight*LineWidthMt);
          if height > (i + 1)*thrHeight
          then
              obj.height := thrHeight
          else
              obj.height := obj.height - i*thrHeight;

          objs[numUsed] := obj;
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
         calls.AddTask(@MatrixVectorMultFunc, objs[i]);

     MatrixVectorMultFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

     calls.SyncAll;
end;

procedure ThrMatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var i: TASMNativeInt;
    obj : TAsyncMatrixAddAndScaleObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddAndScaleObj;
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
               obj := TAsyncMatrixAddAndScaleObj.Create(dest, LineWidth, thrSize, height, Offset, Scale);

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCoresForSimpleFuncs) = 0;
          thrSize := height div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               obj := TAsyncMatrixAddAndScaleObj.Create(dest, LineWidth, width, thrSize, Offset, Scale);

               inc(PByte(obj.dest), i*thrSize*LineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixScaleAndAddFunc, objs[i]);

     MatrixScaleAndAddFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

     calls.SyncAll;
end;


procedure ThrMatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var i: TASMNativeInt;
    obj : TAsyncMatrixAddAndScaleObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddAndScaleObj;
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
               obj := TAsyncMatrixAddAndScaleObj.Create(dest, LineWidth, thrSize, height, Offset, Scale);

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCoresForSimpleFuncs) = 0;
          thrSize := height div numCoresForSimpleFuncs + TASMNativeInt(not sizeFits);

          for i := 0 to numCoresForSimpleFuncs - 1 do
          begin
               obj := TAsyncMatrixAddAndScaleObj.Create(dest, LineWidth, width, thrSize, Offset, Scale);

               inc(PByte(obj.dest), i*thrSize*LineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixAddAndScaleFunc, objs[i]);

     MatrixAddAndScaleFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc);
var i: TASMNativeInt;
    obj : TAsyncMatrixFuncObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
    
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncObj;
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
               obj := TAsyncMatrixFuncObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := thrSize;
               obj.height := height;
               obj.func := func;

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;
                   
               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj := TAsyncMatrixFuncObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := width;
               obj.height := thrSize;
               obj.func := func;

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixFuncFunc, objs[i]);

     MatrixFuncFunc(objs[numUsed  - 1]);
     objs[numUsed - 1].Free;
     
     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc);
var i: TASMNativeInt;
    obj : TAsyncMatrixFuncObjObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncObjObj;
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
               obj := TAsyncMatrixFuncObjObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := thrSize;
               obj.height := height;
               obj.func := func;

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj := TAsyncMatrixFuncObjObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := width;
               obj.height := thrSize;
               obj.func := func;

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixFuncObjFunc, objs[i]);

     MatrixFuncObjFunc(objs[numUsed  - 1]);
     objs[numUsed - 1].Free;
     
     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc);
var i: TASMNativeInt;
    obj : TAsyncMatrixFuncRefObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRefObj;
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
               obj := TAsyncMatrixFuncRefObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := thrSize;
               obj.height := height;
               obj.func := func;

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj := TAsyncMatrixFuncRefObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := width;
               obj.height := thrSize;
               obj.func := func;

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixFuncRefFunc, objs[i]);

     MatrixFuncRefFunc(objs[numUsed  - 1]);
     objs[numUsed - 1].Free;
     
     calls.SyncAll;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc);
var i: TASMNativeInt;
    obj : TAsyncMatrixFuncRefObjObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixFuncRefObjObj;
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
               obj := TAsyncMatrixFuncRefObjObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := thrSize;
               obj.height := height;
               obj.func := func;

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj := TAsyncMatrixFuncRefObjObj.Create;
               obj.dest := dest;
               obj.destLineWidth := destLineWidth;
               obj.width := width;
               obj.height := thrSize;
               obj.func := func;

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     // ###########################################
     // #### Run threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixFuncRefObjFunc, objs[i]);

     MatrixFuncRefObjFunc(objs[numUsed  - 1]);
     objs[numUsed - 1].Free;
     
     calls.SyncAll;
end;

procedure ThrMatrixAddSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; func : TMatrixAddSubFunc);
var i: TASMNativeInt;
    obj : TAsyncMatrixAddSubObj;
    calls : IMtxAsyncCallGroup;
    sizeFits : boolean;
    thrSize : TASMNativeInt;

    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixAddSubObj;
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
               obj := TAsyncMatrixAddSubObj.Create(dest, destLineWidth, Mt1, Mt2, thrSize, height, LineWidth1, LineWidth2);
               obj.func := func;

               inc(obj.dest, i*thrSize);
               inc(obj.mt1, i*thrSize);
               inc(obj.mt2, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj := TAsyncMatrixAddSubObj.Create(dest, destLineWidth, Mt1, Mt2, width, thrSize, LineWidth1, LineWidth2);
               obj.func := func;

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               inc(PByte(obj.mt1), i*thrSize*LineWidth1);
               inc(PByte(obj.mt2), i*thrSize*LineWidth2);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

               objs[numUsed] := obj;
               inc(numUsed);
          end;
     end;

     if numUsed = 0 then
        exit;

     // ##############################################
     // #### Execute threads
     calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTask(@MatrixAddSubFunc, objs[i]);

     MatrixAddSubFunc(objs[numUsed - 1]);
     objs[numUsed - 1].Free;

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
    obj : TAsyncMatrixSortObj;
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
          MatrixSort(Dest, destLineWidth, Width, height, RowWise, nil);
          exit;
     end;

     calls := MtxInitTaskGroup;
     mem := nil;
     if not RowWise then
     begin
          maxNumCores := Min(width, numCPUCores);

          sizeFits := (width mod maxNumCores) = 0;
          thrSize := width div maxNumCores + TASMNativeInt(not sizeFits);
          GetMem(mem, maxNumCores*height*sizeof(double));
          hlpMem := mem;

          for i := 0 to maxNumCores - 1 do
          begin
               obj := TAsyncMatrixSortObj.Create(dest, destLineWidth, thrSize, height, RowWise, hlpMem);

               inc(obj.dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               calls.AddTask(@MatrixSortFunc, obj);
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
               obj := TAsyncMatrixSortObj.Create(dest, destLineWidth, width, thrSize, RowWise, nil);

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

              calls.AddTask(@MatrixSortFunc, obj);
          end;
     end;

     calls.SyncAll;

     if Assigned(mem) then
        FreeMem(mem);
end;

procedure ThrMatrixMedian(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
var i: TASMNativeInt;
    obj : TAsyncMatrixMedianObj;
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

     calls := MtxInitTaskGroup;

     if not RowWise then
     begin
          maxNumCores := Min(width, numCoresForSimpleFuncs);

          sizeFits := (width mod maxNumCores) = 0;
          thrSize := width div maxNumCores + TASMNativeInt(not sizeFits);
          GetMem(mem, maxNumCores*height*sizeof(double));
          hlpMem := mem;

          for i := 0 to maxNumCores - 1 do
          begin
               obj := TAsyncMatrixMedianObj.Create(dest, destLineWidth, src, srcLineWidth, thrSize, height, RowWise, hlpMem);

               inc(obj.dest, i*thrSize);
               inc(obj.src, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj.Width := width - i*thrSize;

               calls.AddTask(@MatrixMedianFunc, obj);
               inc(hlpMem, height);
          end;
     end
     else
     begin
          maxNumCores := Min(height, numCoresForSimpleFuncs);

          GetMem(mem, maxNumCores*width*sizeof(double));
          hlpMem := mem;

          sizeFits := (height mod maxNumCores) = 0;
          thrSize := height div maxNumCores + TASMNativeInt(not sizeFits);

          for i := 0 to maxNumCores - 1 do
          begin
               obj := TAsyncMatrixMedianObj.Create(dest, destLineWidth, src, srcLineWidth, width, thrSize, RowWise, hlpMem);

               inc(PByte(obj.dest), i*thrSize*destLineWidth);
               inc(PByte(obj.src), i*thrSize*srcLineWidth);
               if height < (i + 1)*thrSize then
                  obj.Height := height - i*thrSize;

              calls.AddTask(@MatrixMedianFunc, obj);
              inc(hlpMem, width);
          end;
     end;

     calls.SyncAll;

     FreeMem(mem);
end;

{ TAsyncMultObj }

constructor TAsyncMultObj.Create(adest: PDouble; const adestLineWidth: TASMNativeInt;
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

{ TAsyncMatrixVectorMultObj }

constructor TAsyncMatrixVectorMultObj.Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, aV : PDouble; awidth : TASMNativeInt; aheight : TASMNativeInt;
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

constructor TAsyncMatrixAddAndScaleObj.Create(aDest: PDouble;
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

{ TAsyncMatrixAddSubObj }

constructor TAsyncMatrixAddSubObj.Create(aDest: PDouble;
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

{ TAsyncMatrixMedianObj }

constructor TAsyncMatrixMedianObj.Create(aDest: PDouble;
  aDestLineWidth: TASMNativeInt; aSrc: PDouble; aSrcLineWidth, aWidth,
  aHeight: TASMNativeInt; aRowWise : boolean; aHelpMem : PDouble);
begin
     dest := aDest;
     destLineWidth := aDestLineWidth;
     src := aSrc;
     srcLineWidth := aSrcLineWidth;
     width := aWidth;
     height := aHeight;
     rowWise := aRowWise;
     hlpMem := aHelpMem;
end;

{ TAsyncMatrixSortObj }

constructor TAsyncMatrixSortObj.Create(aDest: PDouble; aDestLineWidth, aWidth,
  aHeight: TASMNativeInt; aRowWise: boolean; aHelpMem: PDouble);
begin
     dest := aDest;
     destLineWidth := aDestLineWidth;
     width := aWidth;
     height := aHeight;
     rowWise := aRowWise;
     hlpMem := aHelpMem;
end;

end.
