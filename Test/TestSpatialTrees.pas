// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestSpatialTrees;

interface

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

// ###########################################
// #### Simple random generator tests (only functional tests no randomness tests!)
// ###########################################

uses {$IFDEF FPC} testregistry, {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF}, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestSpatialTrees = class(TBaseMatrixTestCase)
  private
  published
    procedure TestSimpleVPTree;
    procedure TestSimpleKDTree;

    procedure TestBigKDTree;
  end;

implementation

uses MatrixConst, SpatialTrees, MtxTimer, Types, Math;

{ TTestSpatialTrees }

procedure TTestSpatialTrees.TestSimpleKDTree;
var tree : TKDTree;
    pts : Array[0..17] of double; // 6 points in 3 dimensions
    i: Integer;
    coords : TDataPointArr;
    res : TSearchPtEntryArr;
begin
     Randseed := 127;
     for i := 0 to Length(pts) - 1 do
         pts[i] := random(10);

     SetLength(coords, 6);
     for i := 0 to Length(coords) - 1 do
         coords[i].Init(i, @pts[i*3]);

     tree := TKDTree.Create(3, dfEuclid);
     try
        tree.BuildTree(coords);

        // check if we find the nearest 3 neigbhours -> check the first one that must be the input point
        for i := 0 to Length(coords) - 1 do
        begin
             tree.Search(i, 3, res);
             Check( res[0].dist = 0, 'Points distance is not zero');
             Check( res[0].pt^.X[0] = pts[coords[i].Ind*3], 'Wrong point in coord x');
             Check( res[0].pt^.X[1] = pts[coords[i].Ind*3 + 1], 'Wrong point in coord y');
             Check( res[0].pt^.X[2] = pts[coords[i].Ind*3 + 2], 'Wrong point in coord z');
        end;
     finally
            tree.Free;
     end;
end;

procedure TTestSpatialTrees.TestSimpleVPTree;
var tree : TVPTree;
    pts : Array[0..17] of double; // 6 points in 3 dimensions
    i: Integer;
    coords : TDataPointArr;
    res : TSearchPtEntryArr;
begin
     Randseed := 127;
     for i := 0 to Length(pts) - 1 do
         pts[i] := random(10);

     SetLength(coords, 6);
     for i := 0 to Length(coords) - 1 do
         coords[i].Init(i, @pts[i*3]);

     tree := TVPTree.Create(3, dfEuclid);
     try
        tree.BuildTree(coords);

        // check if we find the nearest 3 neigbhours -> check the first one that must be the input point
        for i := 0 to Length(coords) - 1 do
        begin
             tree.Search(i, 3, res);
             Check( res[0].dist = 0, 'Points distance is not zero');
             Check( res[0].pt^.X[0] = pts[coords[i].Ind*3], 'Wrong point in coord x');
             Check( res[0].pt^.X[1] = pts[coords[i].Ind*3 + 1], 'Wrong point in coord y');
             Check( res[0].pt^.X[2] = pts[coords[i].Ind*3 + 2], 'Wrong point in coord z');
        end;

     finally
            tree.Free;
     end;
end;

procedure TTestSpatialTrees.TestBigKDTree;
const cNumPts : Array[0..4] of integer = (10, 100, 1000, 10000, 100000);
      cNumDim : Array[0..3] of integer = (3, 10, 25, 50);
var start, stop : Int64;
    numPts : Integer;
    dim : integer;
    pts : TDoubleDynArray;
    coords : TDataPointArr;
    res : TSearchPtEntryArr;
    j : integer;
    tree : TKDTree;
    nearestIdx : Array[0..99] of integer;
    testPt : Array[0..49] of double;
    i: Integer;

  function NearestNeighbor( pt : PConstDoubleArr ) : integer;
  var i, j, idx : integer;
      shortDist : double;
      dist : double;
  begin
       shortDist := MaxDouble;
       Result := 0;

       idx := 0;
       for i := 0 to Length(coords) - 1 do
       begin
            dist := 0;
            for j := 0 to dim - 1 do
                dist := dist + sqr( pt^[j] - pts[idx + j]);
            dist := sqrt(dist);
            inc(idx, dim);

            if dist < shortDist then
            begin
                 Result := i;
                 shortDist := dist;
            end;
       end;
  end;

  function SamePt( pt : PDouble; const resPt : TDataPoint ): boolean;
  var i : integer;
  begin
       Result := True;
       for I := 0 to dim - 1 do
       begin
            Result := Result and SameValue(pt^, resPt.X[i]) ;
            inc(pt);
       end;
  end;
begin
     RandSeed := 127;
     for numPts in cNumPts do
     begin
          SetLength(coords, numPts);

          for dim in cNumDim do
          begin
               SetLength(pts, dim*numPts);
               for j := 0 to Length(pts) - 1 do
                   pts[j] := Random;

               for j := 0 to Length(coords) - 1 do
                   coords[j].Init( j, @pts[j*dim]);

               // ###########################################
               // #### Reference -> nearest neighbor linear search
               RandSeed := 127;
               start := MtxGetTime;
               for j := 0 to 100 - 1 do
               begin
                    for i := 0 to dim - 1 do
                        testPt[i] := random;

                    nearestIdx[j] := NearestNeighbor(@testPt[0]);
               end;
               stop := MtxGetTime;
               Status( Format('Simple search - %d pts, %d dim: %.3f ms', [numPts, dim, (stop - start)/mtxFreq*1000]));

               // ###########################################
               // #### Tree based search
               tree := TKDTree.Create(dim, dfEuclid);
               try
                  tree.BuildTree(coords);

                  RandSeed := 127;
                  start := MtxGetTime;
                  for j := 0 to 100 - 1 do
                  begin
                       for i := 0 to dim - 1 do
                           testPt[i] := random;

                       tree.Search(@testPt[0], 1, res);

                       Check( SamePt( @pts[dim*nearestIdx[j]], res[0].pt^ ), 'Failed nearest neighbor search for coord: ' + IntToStr(j));
                  end;
                  stop := MtxGetTime;

                  Status( Format('Tree search - %d pts, %d dim: %.3f ms', [numPts, dim, (stop - start)/mtxFreq*1000]));
               finally
                      tree.Free;
               end;
          end;
     end;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestSpatialTrees{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
