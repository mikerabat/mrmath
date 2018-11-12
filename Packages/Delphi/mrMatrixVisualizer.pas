// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

// Delphi IDE debugger visualisation of TDoubleMatrix and IMatrix (and decendants)

unit mrMatrixVisualizer;

interface

procedure Register;

implementation

uses ToolsAPI, DesignIntf, SysUtils, mrMathVisualizerForm;

var MtxVis: IOTADebuggerVisualizer;

procedure Register;
begin
     MtxVis := TmrMathMtxViewerFrame.Create;
     (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(MtxVis);
end;

procedure RemoveVisualizer;
var DebuggerServices: IOTADebuggerServices;
begin
     if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
     begin
          DebuggerServices.UnregisterDebugVisualizer(MtxVis);
          MtxVis := nil;
     end;
end;

initialization
finalization
  RemoveVisualizer;
end.
