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

uses ASMConsts, MatrixConst;

procedure ThrMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ThrMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ThrMatrixVecMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
procedure ThrMatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure ThrMatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure ThrMatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;


implementation

uses Windows, MtxThreadPool, Math, ASMMatrixOperations, ASMMatrixVectorMultOperations,
     OptimizedFuncs, BlockSizeSetup;

var numCPUCores : TASMNativeInt = 0;
    sysInfo : TSystemInfo;

type
  TMatrixAddSubFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

type
  TAsyncMultObj = class(TObject)
  public
    thrIdx : TASMNativeInt;

    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble;
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    width2 : TASMNativeInt;
    height2 : TASMNativeInt;
    LineWidth1, LineWidth2 : TASMNativeInt;

    constructor Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, amt2 : PDouble; awidth1 : TASMNativeInt; aheight1 : TASMNativeInt;
                       awidth2, aheight2 : TASMNativeInt; const aLineWidth1, aLineWidth2 : TASMNativeInt);
  end;

type
  TAsyncMatrixVectorMultObj = class(TObject)
  public
    thrIdx : TASMNativeInt;

    dest : PDouble;
    destLineWidth : TASMNativeInt;
    mt1, mt2 : PDouble;
    width1 : TASMNativeInt;
    height1 : TASMNativeInt;
    height2 : TASMNativeInt;
    LineWidth1 : TASMNativeInt;

    constructor Create(adest : PDouble; const adestLineWidth : TASMNativeInt; amt1, amt2 : PDouble; awidth1 : TASMNativeInt; aheight1 : TASMNativeInt;
                       aheight2 : TASMNativeInt; const aLineWidth1 : TASMNativeInt);
  end;

type
  TAsyncMatrixAddAndSclaObj = class(TObject)
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
     MatrixMult(TAsyncMultObj(obj).dest,
                TAsyncMultObj(obj).destLineWidth, TAsyncMultObj(obj).mt1,
                TAsyncMultObj(obj).mt2, TAsyncMultObj(obj).width1,
                TAsyncMultObj(obj).height1, TAsyncMultObj(obj).width2,
                TAsyncMultObj(obj).height2, TAsyncMultObj(obj).LineWidth1,
                TAsyncMultObj(obj).LineWidth2);

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
     if TAsyncMatrixVectorMultObj(obj).width1 > 10*cCacheMtxSize*cCacheMtxSize
     then
         BlockedMatrixVectorMultiplication(TAsyncMatrixVectorMultObj(obj).dest,
                                           TAsyncMatrixVectorMultObj(obj).destLineWidth,
                                           TAsyncMatrixVectorMultObj(obj).mt1,
                                           TAsyncMatrixVectorMultObj(obj).mt2,
                                           TAsyncMatrixVectorMultObj(obj).width1,
                                           TAsyncMatrixVectorMultObj(obj).height1,
                                           TAsyncMatrixVectorMultObj(obj).height2,
                                           TAsyncMatrixVectorMultObj(obj).LineWidth1,
                                           BlockedVectorMatrixMultSize)
     else
         MatrixMult(TAsyncMatrixVectorMultObj(obj).dest,
                    TAsyncMatrixVectorMultObj(obj).destLineWidth,
                    TAsyncMatrixVectorMultObj(obj).mt1,
                    TAsyncMatrixVectorMultObj(obj).mt2,
                    TAsyncMatrixVectorMultObj(obj).width1,
                    TAsyncMatrixVectorMultObj(obj).height1,
                    1,
                    TAsyncMatrixVectorMultObj(obj).height2,
                    TAsyncMatrixVectorMultObj(obj).LineWidth1,
                    sizeof(double));

     Result := 0;
end;

function MatrixAddAndScaleFunc(obj : TObject) : integer;
begin
     MatrixAddAndScale(TAsyncMatrixAddAndSclaObj(obj).dest,
                       TAsyncMatrixAddAndSclaObj(obj).destLineWidth,
                       TAsyncMatrixAddAndSclaObj(obj).Width,
                       TAsyncMatrixAddAndSclaObj(obj).Height,
                       TAsyncMatrixAddAndSclaObj(obj).Offset,
                       TAsyncMatrixAddAndSclaObj(obj).Scale);

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

procedure ThrMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var obj : Array of TAsyncMultObj;
    i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : array of IMtxAsyncCall;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    numCalls : TASMNativeInt;
begin
     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     cpud2 := numCPUCores div 2;
     thrWidth := width2;
     if cpud2 > 1 then
     begin
          widthFits := (width2 mod cpud2) = 0;
          thrWidth := (width2 div cpud2) + TASMNativeInt(not widthFits);

          heightFits := (height1 mod cpud2) = 0;
          thrHeight := height1 div cpud2 + TASMNativeInt(not heightFits);
          heightCores := cpud2;
     end
     else
     begin
          cpud2 := 1;
          heightFits := (height1 mod numCPUCores) = 0;
          thrHeight := height1 div numCPUCores + TASMNativeInt(not heightFits);
          heightCores := numCPUCores;
     end;

     numCalls := 0;
     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to heightCores - 1 do
          begin
               obj[numCalls] := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
               obj[numCalls].thrIdx := (j*heightCores) + i;
               inc(PByte(obj[numCalls].dest), i*thrHeight*destLineWidth);
               inc(PByte(obj[numCalls].mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   obj[numCalls].height1 := thrHeight
               else
                   obj[numCalls].height1 := obj[numCalls].height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   obj[numCalls].width2 := thrWidth
               else
                   obj[numCalls].width2 := obj[numCalls].width2 - j*thrWidth;

               inc(obj[numCalls].dest, j*thrWidth);
               inc(obj[numCalls].mt2, j*thrWidth);

               calls[numCalls] := MtxAsyncCall(MatrixMultDirectFunc, obj[numCalls]);
               inc(numCalls);
          end;
     end;

     for i := 0 to numCalls - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var obj : Array of TAsyncMultObj;
    i, j : TASMNativeInt;
    thrHeight, thrWidth : TASMNativeInt;
    cpud2 : TASMNativeInt;
    calls : array of IMtxAsyncCall;
    widthFits, heightFits : boolean;
    heightCores : TASMNativeInt;
    numCalls : TASMNativeInt;
begin
     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     cpud2 := numCPUCores div 2;
     thrWidth := width2;
     if cpud2 > 1 then
     begin
          widthFits := (width2 mod cpud2) = 0;
          // ensure that we can do aligned operations on matrices
          thrWidth := (width2 div cpud2) + 2*TASMNativeInt(not widthFits);

          heightFits := (height1 mod cpud2) = 0;
          thrHeight := height1 div cpud2 + TASMNativeInt(not heightFits);
          heightCores := cpud2;
     end
     else
     begin
          cpud2 := 1;
          heightFits := (height1 mod numCPUCores) = 0;
          thrHeight := height1 div numCPUCores + TASMNativeInt(not heightFits);
          heightCores := numCPUCores;
     end;

     numCalls := 0;
     for j := 0 to cpud2 - 1 do
     begin
          for i := 0 to heightCores - 1 do
          begin
               obj[numCalls] := TAsyncMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
               obj[numCalls].thrIdx := (j*heightCores) + i;
               inc(PByte(obj[numCalls].dest), i*thrHeight*destLineWidth);
               inc(PByte(obj[numCalls].mt1), i*thrHeight*LineWidth1);
               if height1 > (i + 1)*thrHeight
               then
                   obj[numCalls].height1 := thrHeight
               else
                   obj[numCalls].height1 := obj[numCalls].height1 - i*thrHeight;

               if width2 > (j + 1)*thrWidth
               then
                   obj[numCalls].width2 := thrWidth
               else
                   obj[numCalls].width2 := obj[numCalls].width2 - j*thrWidth;

               inc(obj[numCalls].dest, j*thrWidth);
               inc(obj[numCalls].mt2, j*thrWidth);

               calls[numCalls] := MtxAsyncCall(MatrixMultFunc, obj[numCalls]);
               inc(numCalls);
          end;
     end;

     for i := 0 to numCalls - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixVecMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt);
var obj : Array of TAsyncMatrixVectorMultObj;
    i : TASMNativeInt;
    thrHeight : TASMNativeInt;
    calls : array of IMtxAsyncCall;
    heightFits : boolean;
begin
     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     heightFits := (height1 mod numCPUCores) = 0;                                  
     thrHeight := height1 div numCPUCores + TASMNativeInt(not heightFits);
     for i := 0 to numCPUCores - 1 do
     begin
          obj[i] := TAsyncMatrixVectorMultObj.Create(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1);
          obj[i].thrIdx := i;
          inc(PByte(obj[i].dest), i*thrHeight*destLineWidth);
          inc(PByte(obj[i].mt1), i*thrHeight*LineWidth1);
          if height1 > (i + 1)*thrHeight
          then
              obj[i].height1 := thrHeight
          else
              obj[i].height1 := obj[i].height1 - i*thrHeight;

          calls[i] := MtxAsyncCall(MatrixVectorMultFunc, obj[i]);
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixAddAndSclaObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixAddAndScale(Dest, LineWidth, Width, height, offset, scale);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixAddAndSclaObj.Create(dest, LineWidth, thrSize, height, Offset, Scale);

               inc(obj[i].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixAddAndScaleFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixAddAndSclaObj.Create(dest, LineWidth, width, thrSize, Offset, Scale);

               inc(PByte(obj[i].dest), i*thrSize*LineWidth);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixAddAndScaleFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixFuncObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := thrSize;
               obj[i].height := height;
               obj[i].func := func;

               inc(obj[i].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := width;
               obj[i].height := thrSize;
               obj[i].func := func;

               inc(PByte(obj[i].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixAddAndScaleFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixFuncObjObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncObjObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := thrSize;
               obj[i].height := height;
               obj[i].func := func;

               inc(obj[i].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncObjFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncObjObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := width;
               obj[i].height := thrSize;
               obj[i].func := func;

               inc(PByte(obj[i].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncObjFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixFuncRefObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncRefObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := thrSize;
               obj[i].height := height;
               obj[i].func := func;

               inc(obj[i].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncRefFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncRefObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := width;
               obj[i].height := thrSize;
               obj[i].func := func;

               inc(PByte(obj[i].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncRefFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixFuncRefObjObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          MatrixFunc(dest, destLineWidth, width, height, func);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncRefObjObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := thrSize;
               obj[i].height := height;
               obj[i].func := func;

               inc(obj[i].dest, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncRefObjFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixFuncRefObjObj.Create;
               obj[i].dest := dest;
               obj[i].destLineWidth := destLineWidth;
               obj[i].width := width;
               obj[i].height := thrSize;
               obj[i].func := func;

               inc(PByte(obj[i].dest), i*thrSize*destLineWidth);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixFuncRefObjFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixAddSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; func : TMatrixAddSubFunc);
var i: TASMNativeInt;
    obj : Array of TAsyncMatrixAddSubObj;
    calls : Array of IMtxAsyncCall;
    sizeFits : boolean;
    thrSize : TASMNativeInt;
begin
     // check if it is really necessary to thread the call:
     if (width < numCPUCores) and (height < numCPUCores) then
     begin
          func(Dest, destLineWidth, mt1, mt2, Width, height, LineWidth1, LineWidth2);
          exit;
     end;

     SetLength(obj, numCPUCores);
     SetLength(calls, numCPUCores);

     if width > height then
     begin
          sizeFits := (width mod numCPUCores) = 0;
          thrSize := width div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixAddSubObj.Create(dest, destLineWidth, Mt1, Mt2, thrSize, height, LineWidth1, LineWidth2);
               obj[i].func := func;

               inc(obj[i].dest, i*thrSize);
               inc(obj[i].mt1, i*thrSize);
               inc(obj[i].mt2, i*thrSize);
               if width < (i + 1)*thrSize then
                  obj[i].Width := width - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixAddSubFunc, obj[i]);
          end;
     end
     else
     begin
          sizeFits := (height mod numCPUCores) = 0;
          thrSize := height div numCPUCores + TASMNativeInt(not sizeFits);

          for i := 0 to numCPUCores - 1 do
          begin
               obj[i] := TAsyncMatrixAddSubObj.Create(dest, destLineWidth, Mt1, Mt2, width, thrSize, LineWidth1, LineWidth2);
               obj[i].func := func;

               inc(PByte(obj[i].dest), i*thrSize*destLineWidth);
               inc(PByte(obj[i].mt1), i*thrSize*LineWidth1);
               inc(PByte(obj[i].mt2), i*thrSize*LineWidth2);
               if height < (i + 1)*thrSize then
                  obj[i].Height := height - i*thrSize;

               calls[i] := MtxAsyncCall(MatrixAddSubFunc, obj[i]);
          end;
     end;

     for i := 0 to numCpuCores - 1 do
     begin
          calls[i].sync;
          obj[i].Free;
     end;
end;

procedure ThrMatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     ThrMatrixAddSub(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2, MatrixSub);
end;

procedure ThrMatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     ThrMatrixAddSub(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2, MatrixAdd);
end;


{ TAsyncMultObj }

constructor TAsyncMultObj.Create(adest: PDouble; const adestLineWidth: TASMNativeInt;
  amt1, amt2: PDouble; awidth1, aheight1, awidth2, aheight2: TASMNativeInt;
  const aLineWidth1, aLineWidth2: TASMNativeInt);
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
end;

{ TAsyncMatrixVectorMultObj }

constructor TAsyncMatrixVectorMultObj.Create(adest: PDouble;
  const adestLineWidth: TASMNativeInt; amt1, amt2: PDouble; awidth1, aheight1,
  aheight2: TASMNativeInt; const aLineWidth1: TASMNativeInt);
begin
     dest := adest;
     destLineWidth := adestLineWidth;
     mt1 := amt1;
     mt2 := amt2;
     width1 := awidth1;
     height1 := aheight1;
     height2 := aheight2;
     LineWidth1 := aLineWidth1;
end;

{ TAsyncMatrixAddAndSclaObj }

constructor TAsyncMatrixAddAndSclaObj.Create(aDest: PDouble;
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

initialization
  GetSystemInfo(SysInfo);
  numCPUCores := SysInfo.dwNumberOfProcessors;
finalization

end.
