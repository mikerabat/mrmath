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


unit BaseMathPersistence;

// ###################################################
// #### Base abstract I/O functionality
// ###################################################

interface

uses SysUtils, Classes, Types;

// #############################################################
// #### Loading/saving data
type
  TCustomMathPersistenceIO = class;
  TCustomMathPersistenceIOClass = class of TCustomMathPersistenceIO;
  IMathPersistence = interface
    ['{16A5F1D0-CA39-4BD0-BABE-E1E70B0045C3}']
    procedure SaveToFile(const FileName : string; Writer : TCustomMathPersistenceIOClass);
    procedure SaveToStream(stream : TStream; Writer : TCustomMathPersistenceIOClass);
  end;

  TBaseMathPersistence = class(TInterfacedObject, IMathPersistence)
  private
    fWriter : TCustomMathPersistenceIO;
    fInList : boolean;
  protected
    // functions to add and load properties
    procedure BeginList(const Name : String; count : integer);
    procedure EndList;
    procedure AddObjectDescription(const Name : String; const Value : String);
    procedure AddDoubleProperty(const Name : String; const Value : double);
    procedure AddStringProperty(const Name : String; const Value : String);
    procedure AddIntProperty(const Name : String; const Value : integer);
    procedure AddBinaryProperty(const Name : String; const Value; size : integer);
    procedure AddDoubleArr(const Name : String; const Value : TDoubleDynArray);
    procedure AddListDoubleArr(const Value : TDoubleDynArray);
    procedure AddIntArr(const Name : String; const Value : TIntegerDynArray);
    procedure AddListIntArr(const Value : TIntegerDynArray);
    procedure AddObject(Obj : TBaseMathPersistence); overload;
    procedure AddObject(const Name : String; obj : TBaseMathPersistence); overload;
  protected
    procedure OnLoadDoubleProperty(const Name : String; const Value : double); virtual;
    procedure OnLoadStringProperty(const Name : String; const Value : String); virtual;
    procedure OnLoadIntProperty(const Name : String; Value : integer); virtual;
    procedure OnLoadBinaryProperty(const Name : String; const Value; size : integer); virtual;
    function OnLoadObject(Obj : TBaseMathPersistence) : boolean; overload; virtual;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; overload; virtual;
    procedure OnLoadBeginList(const Name : String; count : integer); virtual;
    procedure OnLoadEndList; virtual;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); virtual;
    procedure OnLoadIntArr(const Name : String; const Value : TIntegerDynArray); virtual;
    procedure OnLoadListDoubleArr(const Value : TDoubleDynArray); virtual;
    procedure OnLoadListIntArr(const Value : TIntegerDynArray); virtual;

    procedure InitWriter(Writer : TCustomMathPersistenceIO);
    class function ClassIdentifier : String; virtual;
    procedure DefineProps; virtual; abstract;
    procedure CleanupWriter;
  public
    procedure SaveToFile(const FileName : string; WriterClass : TCustomMathPersistenceIOClass);
    procedure SaveToStream(stream : TStream; WriterClass : TCustomMathPersistenceIOClass);
  end;
  TBaseMathPersistenceClass = class of TBaseMathPersistence;

// ###################################################
// #### Base IO functionality
  TCustomMathPersistenceIO = class(TObject)
  private
    fStream : TStream;
  protected
    property Stream : TStream read fStream;

    class function CanReadStream(Stream : TStream) : boolean; virtual; abstract;

    procedure WriteStreamHeader; virtual; abstract;
    procedure FinalizeStream; virtual; abstract;

    procedure WriteObjDescriptor(const Name : String; const Value : String); virtual; abstract;
    procedure WriteListBegin(const Name : String; count : integer); virtual; abstract;
    procedure WriteListEnd; virtual; abstract;
    procedure WriteProperty(const Name : String; const Value : double); overload; virtual; abstract;
    procedure WriteProperty(const Name : String; const Value : String); overload; virtual; abstract;
    procedure WriteProperty(const Name : String; const Value : integer); overload; virtual; abstract;
    procedure WriteDoubleArr(const Name : String; const Value : Array of double); virtual; abstract;
    procedure WriteListDoubleArr(const Value : Array of double); virtual; abstract;
    procedure WriteIntArr(const Name : String; const Value : Array of Integer); virtual; abstract;
    procedure WriteListIntArr(const Value : Array of Integer); virtual; abstract;
    procedure WriteBinaryProperty(const Name : String; const Value; size : integer); virtual; abstract;
    procedure WriteObject(Obj : TBaseMathPersistence); overload; virtual; abstract;
    procedure WriteObject(const Name : String; Obj : TBaseMathPersistence); overload; virtual; abstract;
  public
    function LoadFromFile(const FileName : string) : TBaseMathPersistence;
    function LoadFromStream(Stream : TStream) : TBaseMathPersistence; virtual; abstract;

    procedure SaveToFile(aMathObj : TBaseMathPersistence; const FileName : string);
    procedure SaveToStream(aMathObj : TBaseMathPersistence; aStream : TStream); virtual;

    class procedure StaticSaveToFile(aMathObj : TBaseMathPersistence; const FileName : string);
    class procedure StaticSaveToStream(aMathObj : TBaseMathPersistence; Stream : TStream);
  end;

function CreateMathIOReader(const FileName : TFileName) : TCustomMathPersistenceIOClass; overload;
function CreateMathIOReader(Stream : TStream) : TCustomMathPersistenceIOClass; overload;

function ReadObjFromFile(const FileName : TFileName) : TBaseMathPersistence;
function ReadObjFromStream(stream : TStream) : TBaseMathPersistence;

// every math IO file type must be registered here to be recognized
procedure RegisterMathIOReader(reader : TCustomMathPersistenceIOClass);

// every classifier must be registered here in order to get recognized in the IO routines
procedure RegisterMathIO(AMathIOClass : TBaseMathPersistenceClass);
function MathIOClassFromName(const CLName : String) : TBaseMathPersistenceClass;

// for dll deinitalizations:
procedure FinalizeMathIO;

implementation

uses Contnrs;

// ###################################################
// #### Global functions / reader writer handling
// ###################################################

var IOObjects : TClassList;
    MathIOClasses : TClassList;

function GetIOObjects : TClassList;
begin
     if not Assigned(IOObjects) then
        IOObjects := TClassList.Create;

     Result := IOObjects;
end;

function GetMathIOClasses : TClassList;
begin
     if not Assigned(MathIOClasses) then
        MathIOClasses := TClassList.Create;

     Result := MathIOClasses;
end;

procedure RegisterMathIO(AMathIOClass : TBaseMathPersistenceClass);
begin
     GetMathIOClasses.Add(AMathIOClass);
end;

function MathIOClassFromName(const CLName : String) : TBaseMathPersistenceClass;
var cl : TBaseMathPersistenceClass;
    i : integer;
begin
     Result := nil;

     for i := 0 to GetMathIOClasses.Count - 1 do
     begin
          cl := TBaseMathPersistenceClass(GetMathIOClasses[i]);

          if CompareText(String(cl.ClassIdentifier), String(CLName)) = 0 then
          begin
               Result := cl;
               exit;
          end;
     end;
end;

function CreateMathIOReader(const FileName : TFileName) : TCustomMathPersistenceIOClass;
var fs : TFileStream;
begin
     fs := TFileStream.Create(FileName, fmOpenRead);
     try
        Result := CreateMathIOReader(fs);
     finally
            fs.Free;
     end;
end;

function CreateMathIOReader(Stream : TStream) : TCustomMathPersistenceIOClass;
var i : integer;
    startPos : Int64;
begin
     startPos := stream.Position;
     Result := nil;
     for i := 0 to GetIOObjects.Count - 1 do
     begin
          try
             if TCustomMathPersistenceIOClass(GetIOObjects[i]).CanReadStream(stream) then
             begin
                  Result := TCustomMathPersistenceIOClass(GetIOObjects[i]);
                  break;
             end;
          except
          end;

          stream.Position := startPos;
     end;
end;

procedure RegisterMathIOReader(reader : TCustomMathPersistenceIOClass);
begin
     GetIOObjects.Add(reader);
end;

function ReadObjFromFile(const FileName : TFileName) : TBaseMathPersistence;
var readerClass : TCustomMathPersistenceIOClass;
begin
     Result := nil;

     readerClass := CreateMathIOReader(FileName);
     if not Assigned(readerClass) then
        exit;

     with readerClass.Create do
     try
        Result := LoadFromFile(FileName);
     finally
            Free;
     end;
end;

function ReadObjFromStream(stream : TStream) : TBaseMathPersistence;
var readerClass : TCustomMathPersistenceIOClass;
begin
     Result := nil;

     readerClass := CreateMathIOReader(stream);
     if not Assigned(readerClass) then
        exit;

     with readerClass.Create do
     try
        Result := LoadFromStream(stream);
     finally
            Free;
     end;
end;

{ TCustomMathPersistenceIO }

function TCustomMathPersistenceIO.LoadFromFile(
  const FileName: string): TBaseMathPersistence;
var fs : TFileStream;
begin
     fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
     try
        Result := LoadFromStream(fs);
     finally
            fs.Free;
     end;
end;

procedure TCustomMathPersistenceIO.SaveToFile(aMathObj : TBaseMathPersistence; const FileName: string);
var fs : TFileStream;
begin
     fs := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
     try
        SaveToStream(aMathObj, fs);
     finally
            fs.Free;
     end;
end;

procedure TCustomMathPersistenceIO.SaveToStream(aMathObj: TBaseMathPersistence;
  aStream: TStream);
begin
     fStream := aStream;
     try
        WriteStreamHeader;
        aMathObj.InitWriter(Self);
        aMathObj.DefineProps;
        FinalizeStream;
     finally
            aMathObj.CleanupWriter;
            fStream := nil;
     end;
end;

class procedure TCustomMathPersistenceIO.StaticSaveToFile(
  aMathObj: TBaseMathPersistence; const FileName: string);
begin
     with self.Create do
     try
        SaveToFile(aMathObj, FileName);
     finally
            Free;
     end;
end;

class procedure TCustomMathPersistenceIO.StaticSaveToStream(
  aMathObj: TBaseMathPersistence; Stream: TStream);
begin
     with self.Create do
     try
        SaveToStream(aMathObj, Stream);
     finally
            Free;
     end;
end;

{ TBaseMathPersistence }

procedure TBaseMathPersistence.AddBinaryProperty(const Name: String;
  const Value; size: integer);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteBinaryProperty(Name, Value, size);
end;

procedure TBaseMathPersistence.AddDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteDoubleArr(Name, Value);
end;

procedure TBaseMathPersistence.AddObject(const Name: String;
  obj: TBaseMathPersistence);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteObject(Name, obj);
end;

procedure TBaseMathPersistence.AddObjectDescription(const Name,
  Value: String);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteObjDescriptor('obj', Value);
end;

procedure TBaseMathPersistence.AddObject(Obj: TBaseMathPersistence);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteObject(Obj);
end;

procedure TBaseMathPersistence.AddStringProperty(const Name, Value: String);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteProperty(Name, Value);
end;

procedure TBaseMathPersistence.AddDoubleProperty(const Name: String;
  const Value: double);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteProperty(Name, Value);
end;

procedure TBaseMathPersistence.AddIntProperty(const Name: String;
  const Value: integer);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteProperty(Name, Value);
end;

procedure TBaseMathPersistence.BeginList(const Name: String; count: integer);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteListBegin(Name, count);
     fInList := True;
end;

class function TBaseMathPersistence.ClassIdentifier: String;
begin
     Result := String(ClassName);
end;

procedure TBaseMathPersistence.CleanupWriter;
begin
     fWriter := nil;
end;

procedure TBaseMathPersistence.EndList;
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteListEnd;
     fInList := False;
end;

procedure TBaseMathPersistence.InitWriter(Writer: TCustomMathPersistenceIO);
begin
     fInList := False;
     fWriter := Writer;

     // init the stream with a header
     AddObjectDescription('hea', ClassIdentifier);
end;

procedure TBaseMathPersistence.OnLoadBeginList(const Name: String;
  count: integer);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadBinaryProperty(const Name: String;
  const Value; size: integer);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadEndList;
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadIntArr(const Name: String;
  const Value: TIntegerDynArray);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadListDoubleArr(
  const Value: TDoubleDynArray);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadListIntArr(const Value: TIntegerDynArray);
begin
     // do nothing in the base class
end;

function TBaseMathPersistence.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence) : boolean;
begin
     Result := False;
end;

function TBaseMathPersistence.OnLoadObject(Obj: TBaseMathPersistence) : boolean;
begin
     // do nothing in the base class
     Result := False;
end;

procedure TBaseMathPersistence.OnLoadStringProperty(const Name, Value: String);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.SaveToFile(const FileName: string;
  WriterClass: TCustomMathPersistenceIOClass);
var fs : TFileStream;
begin
     fs := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
     try
        SaveToStream(fs, WriterClass);
     finally
            fs.Free;
     end;
end;

procedure TBaseMathPersistence.SaveToStream(stream: TStream;
  WriterClass : TCustomMathPersistenceIOClass);
var obj : TCustomMathPersistenceIO;
begin
     obj := WriterClass.Create;
     try
        obj.SaveToStream(self, stream);
     finally
            obj.Free;
     end;
end;

procedure TBaseMathPersistence.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     // do nothing in the base class
end;

procedure TBaseMathPersistence.OnLoadDoubleProperty(const Name: String;
  const Value: double);
begin
     // do nothing in the base class
end;

procedure FinalizeMathIO;
begin
     FreeAndNil(IOObjects);
     FreeAndNil(MathIOClasses);
end;

procedure TBaseMathPersistence.AddIntArr(const Name: String;
  const Value: TIntegerDynArray);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     fWriter.WriteIntArr(Name, Value);
end;

procedure TBaseMathPersistence.AddListDoubleArr(const Value: TDoubleDynArray);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     if not fInList then
        raise EWriteError.Create('Error call "addlist x" function without call of BeginList');
        
     fWriter.WriteListDoubleArr(Value);
end;

procedure TBaseMathPersistence.AddListIntArr(const Value: TIntegerDynArray);
begin
     assert(Assigned(fWriter), 'No writer assigned');
     if not fInList then
        raise EWriteError.Create('Error call "addlist x" function without call of BeginList');
        
     fWriter.WriteListIntArr(Value);
end;

initialization
  IOObjects := nil;
  MathIOClasses := nil;

finalization
  FinalizeMathIO;

end.
