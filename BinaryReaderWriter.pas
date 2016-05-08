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


unit BinaryReaderWriter;

// ###################################################
// #### Binary reader and writer classes
// ###################################################

interface

uses SysUtils, Classes, BaseMathPersistence, types;

// #############################################################
// #### Loading/saving data
type
  TBinaryReaderWriter = class(TCustomMathPersistenceIO)
  private
    procedure ReadBinaryData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadStringData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadDoubleData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadDoubleArrData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadIntArrData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadListDoubleArrData(Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadListIntArrData(Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadIntegerData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);
    procedure ReadListBeginData(sectName : String; Len : LongInt; obj : TBaseMathPersistence; aStream : TStream);

    function ReadObjFromStream(aStream : TStream) : TBaseMathPersistence;
  protected
    class function CanReadStream(aStream : TStream) : boolean; override;

    procedure WriteStreamHeader; override;
    procedure FinalizeStream; override;

    procedure WriteObjDescriptor(const Name : String; const Value : String); override;
    procedure WriteDoubleArr(const Name : String; const Value : Array of double); override;
    procedure WriteListBegin(const Name : String; count : integer); override;
    procedure WriteListEnd; override;
    procedure WriteBinaryProperty(const Name : String; const Value; size : integer); override;
    procedure WriteProperty(const Name : String; const Value : double); overload; override;
    procedure WriteProperty(const Name : String; const Value : String); overload; override;
    procedure WriteProperty(const Name : String; const Value : integer); overload; override;
    procedure WriteObject(Obj : TBaseMathPersistence); override;
    procedure WriteObject(const Name : String; Obj : TBaseMathPersistence); overload; override;
    procedure WriteListDoubleArr(const Value : Array of double); override;
    procedure WriteIntArr(const Name : String; const Value : Array of Integer); override;
    procedure WriteListIntArr(const Value : Array of Integer); override;
  public
    function LoadFromStream(aStream : TStream) : TBaseMathPersistence; override;
  end;

implementation

uses Contnrs;

const cBinaryReaderHeader : Array[0..4] of AnsiChar = 'RMCLS';
      cBinaryReaderVersion : LongWord = 1;

{$MINENUMSIZE 1}
type
  TBinarySectionType = (bsBinary, bsString, bsDouble, bsDoubleArr, bsIntArr, bsInteger, bsBeginList,
                        bsListIntArr, bsListDoubleArr, bsEndList, bsObjectDescription, bsObjectBegin, bsObjectEnd, bsEndaStream);

type
  TBinarySectionRec = packed record
    Len : LongInt;
    SectTpye : TBinarySectionType;
    NameLen : Word;
  end;
  // followed by NameLen UTF8 characters -> then followed by the data

{ TBinaryReaderWriter }

class function TBinaryReaderWriter.CanReadStream(aStream: TStream): boolean;
var hea : Array[0..4] of AnsiChar;
begin
     Result := aStream.Read(hea, sizeof(hea)) = sizeof(hea);

     if not Result or not CompareMem(@hea[0], @cBinaryReaderHeader[0], sizeof(cBinaryReaderHeader)) then
        Result := False;
end;

function TBinaryReaderWriter.ReadObjFromStream(
  aStream : TStream): TBaseMathPersistence;
var MathIOObjReader : TCustomMathPersistenceIO;
begin
     MathIOObjReader := TBinaryReaderWriter.Create;
     try
        Result := MathIOObjReader.LoadFromStream(aStream);
     finally
            MathIOObjReader.Free;
     end;

     Assert(Assigned(Result), 'Error could not load object from aStream');
end;

type
  TSectObj = class(TObject)
    ObjName : String;
    MathIOObj : TBaseMathPersistence;

    constructor Create(const Name : String; aObj : TBaseMathPersistence);
  end;

procedure TBinaryReaderWriter.FinalizeStream;
var sect : TBinarySectionRec;
begin
     // #####################################################
     // #### Write Closing section
     sect.Len := sizeof(sect);
     sect.SectTpye := bsEndaStream;
     sect.NameLen := 0;

     Stream.Write(sect, sizeof(sect));
end;

function TBinaryReaderWriter.LoadFromStream(aStream: TStream): TBaseMathPersistence;
var version : LongWord;
    sect : TBinarySectionRec;
    objs : TObjectStack;
    obj : TSectObj;
    sectName : String;
    utfSectName : UTF8String;
    res : boolean;
    mathIOObj : TBaseMathPersistence;
    mathIOClass : TBaseMathPersistenceClass;
    mathIOClassDescr : String;
    utf8MathIOClassDescr : UTF8String;
begin
     if not CanReadStream(aStream) then
        raise EReadError.Create('Wrong file format');

     // ####################################################
     // #### Read version -> take actions according to the new version
     aStream.ReadBuffer(version, sizeof(version));
     WriteVersion(version);

     // ####################################################
     // #### Read data
     objs := TObjectStack.Create;
     try
        obj := nil;
        repeat
              // read section type
              aStream.ReadBuffer(sect, sizeof(sect));
              SetLength(utfSectName, sect.NameLen);
              if sect.NameLen > 0 then
                 aStream.ReadBuffer(utfSectName[1], sect.NameLen);

              sectName := String(utfsectName);
              if objs.Count > 0 then
                 obj := TSectObj(objs.Peek);

              assert((sect.SectTpye <> bsObjectBegin) or assigned(obj), 'Error no object in aStream');

              case sect.SectTpye of
                bsObjectDescription: begin
                                          assert(sect.Len > Length(utfSectName) + sizeof(TBinarySectionRec), 'Error no data');
                                          SetLength(utf8MathIOClassDescr, sect.len - Length(utfSectName) - sizeof(TBinarySectionRec));
                                          aStream.ReadBuffer(utf8MathIOClassDescr[1], Length(utf8MathIOClassDescr));
                                          mathIOClassDescr := String(utf8MathIOClassDescr);
                                          mathIOClass := MathIOClassFromName(mathIOClassDescr);
                                          assert(Assigned(mathIOClass), 'Error could not find class from descriptor (' + mathIOClassDescr + ')');
                                          mathIOObj := mathIOClass.Create;
                                          objs.Push(TSectObj.Create(mathIOClassDescr, mathIOObj));
                                     end;
                bsBinary: ReadBinaryData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsString: ReadStringData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsDouble: ReadDoubleData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsDoubleArr: ReadDoubleArrData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsIntArr: ReadIntArrData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsListIntArr: ReadListIntArrData(sect.Len, obj.MathIOObj, aStream);
                bsListDoubleArr: ReadListDoubleArrData(sect.Len, obj.MathIOObj, aStream);
                bsInteger: ReadIntegerData(sectName, sect.Len, obj.MathIOObj, aStream);
                bsBeginList: ReadListBeginData(sectName, sect.len, obj.MathIOObj, aStream);
                bsEndList: ReadEndList(obj.MathIOObj);
                bsObjectBegin: objs.Push(TSectObj.Create(sectName, ReadObjFromStream(aStream)));
                bsObjectEnd: begin
                                  // append loaded object to the previous object
                                  assert(objs.Count > 0, 'Error: ending section found before beginnig section');
                                  if objs.Count > 1 then
                                  begin
                                       obj := TSectObj(objs.Pop);
                                       if obj.ObjName = ''
                                       then
                                           res := ReadObject(TSectObj(objs.Peek).MathIOObj, obj.MathIOObj)
                                       else
                                           res := ReadObject(TSectObj(objs.Peek).MathIOObj, obj.ObjName, obj.MathIOObj);

                                       if not res then
                                          obj.MathIOObj.Free;
                                          
                                       FreeAndNil(obj);
                                  end;
                             end;
              end;
        until sect.SectTpye = bsEndaStream;

        assert(objs.Count > 0, 'Error no objects read');
        obj := TSectObj(objs.Pop);
        Result := obj.MathIOObj;

        FreeAndNil(obj);
     finally
            objs.Free;
     end;

     // inform the object that it has been finally initalized
     ReadFinalize(Result);
end;

procedure TBinaryReaderWriter.ReadBinaryData(sectName : String; Len : LongInt; obj: TBaseMathPersistence;
  aStream: TStream);
var value : Array of byte;
    sSect : UTF8String;
begin
     if Len = Length(sectName) + sizeof(TBinarySectionRec) then
        exit;

     sSect := UTF8String(sectName);
     assert(Len > Length(sSect) + sizeof(TBinarySectionRec), 'Error no data');
     SetLength(value, len - Length(sSect) - sizeof(TBinarySectionRec));
     aStream.ReadBuffer(value[0], Length(value));
     ReadBinaryProperty(obj, sectName, Value[0], Length(value));
end;

procedure TBinaryReaderWriter.ReadDoubleArrData(sectName: String;
  Len: Integer; obj: TBaseMathPersistence; aStream: TStream);
var value : TDoubleDynArray;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     assert(((len - Length(sSect) - sizeof(TBinarySectionRec)) mod sizeof(double)) = 0, 'Error wrong initialized section');
     SetLength(value, (len - length(sSect) - sizeof(TBinarySectionRec)) div sizeof(double));
     if Length(value) > 0 then
        aStream.ReadBuffer(value[0], sizeof(double)*Length(value));
     ReadDoubleArr(obj, sectName, Value);
end;

procedure TBinaryReaderWriter.ReadDoubleData(sectName : String; Len : LongInt; obj: TBaseMathPersistence;
  aStream: TStream);
var value : double;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     assert(len - Length(sSect) - sizeof(TBinarySectionRec) = sizeof(double), 'Error wrong initialized section');
     aStream.ReadBuffer(value, sizeof(double));
     ReadDoubleProperty(obj, sectName, Value);
end;

procedure TBinaryReaderWriter.ReadIntArrData(sectName: String; Len: Integer;
  obj: TBaseMathPersistence; aStream: TStream);
var value : TIntegerDynArray;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     assert((len - Length(sSect) - sizeof(TBinarySectionRec)) mod sizeof(integer) = 0, 'Error wrong initialized section');
     SetLength(value, (len - length(sSect) - sizeof(TBinarySectionRec)) div sizeof(integer));
     if Length(value) > 0 then
        aStream.ReadBuffer(value[0], sizeof(integer)*Length(value));
     ReadIntArr(obj, sectName, Value);
end;

procedure TBinaryReaderWriter.ReadIntegerData(sectName : String; Len : LongInt; obj: TBaseMathPersistence;
  aStream: TStream);
var value : longint;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     assert((len - Length(sSect) - sizeof(TBinarySectionRec)) = sizeof(longint), 'Error wrong initialized section');
     aStream.ReadBuffer(value, sizeof(longint));
     ReadIntProperty(obj, sectName, value);
end;

procedure TBinaryReaderWriter.ReadListBeginData(sectName: String;
  Len: Integer; obj: TBaseMathPersistence; aStream: TStream);
var value : LongInt;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     assert((len - Length(sSect) - sizeof(TBinarySectionRec)) = sizeof(longint), 'Error wrong initialized section');
     aStream.ReadBuffer(value, sizeof(longint));
     ReadBeginList(obj, sectName, value);
end;

procedure TBinaryReaderWriter.ReadListDoubleArrData(Len: Integer; obj: TBaseMathPersistence; aStream: TStream);
var value : TDoubleDynArray;
begin
     assert((len - sizeof(TBinarySectionRec)) mod sizeof(double) = 0, 'Error wrong initialized section');
     SetLength(value, (len - sizeof(TBinarySectionRec)) div sizeof(double));
     if Length(value) > 0 then
        aStream.ReadBuffer(value[0], sizeof(double)*Length(value));
     ReadListDoubleArr(obj, Value);
end;

procedure TBinaryReaderWriter.ReadListIntArrData(Len: Integer; obj: TBaseMathPersistence; aStream: TStream);
var value : TIntegerDynArray;
begin
     assert((len - sizeof(TBinarySectionRec)) mod sizeof(integer) = 0, 'Error wrong initialized section');
     SetLength(value, (len - sizeof(TBinarySectionRec)) div sizeof(integer));
     if Length(value) > 0 then
        aStream.ReadBuffer(value[0], sizeof(integer)*Length(value));
     ReadListIntArr(obj, Value);
end;

procedure TBinaryReaderWriter.ReadStringData(sectName : String; Len : LongInt; obj: TBaseMathPersistence;
  aStream: TStream);
var value : String;
    sSect : UTF8String;
begin
     sSect := UTF8String(sectName);

     if Len = Length(sSect) + sizeof(TBinarySectionRec) then
        exit;

     assert(Len > Length(sSect) + sizeof(TBinarySectionRec), 'Error no data');
     SetLength(value, len - Length(sectName) - sizeof(TBinarySectionRec));
     aStream.ReadBuffer(value, Length(value));
     ReadStringProperty(obj, sectName, Value);
end;

procedure TBinaryReaderWriter.WriteBinaryProperty(const Name: String;
  const Value; size: integer);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + size;
     sect.SectTpye := bsBinary;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.Write(sName[1], Length(sName));

     if Size > 0 then
        Stream.Write(Value, size);
end;

procedure TBinaryReaderWriter.WriteDoubleArr(const Name: String;
  const Value: Array of double);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + Length(Value)*Sizeof(double);
     sect.SectTpye := bsDoubleArr;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.Write(sName[1], Length(sName));

     if Length(Value) > 0 then
        Stream.Write(Value[0], Length(Value)*sizeof(double));
end;

procedure TBinaryReaderWriter.WriteIntArr(const Name: String;
  const Value: array of Integer);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + Length(Value)*Sizeof(Integer);
     sect.SectTpye := bsIntArr;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.Write(sName[1], Length(sName));

     if Length(Value) > 0 then
        Stream.Write(Value[0], Length(Value)*sizeof(integer));
end;

procedure TBinaryReaderWriter.WriteListBegin(const Name: String;
  count: integer);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + sizeof(LongInt);
     sect.SectTpye := bsBeginList;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.Write(sName[1], Length(sName));

     Stream.Write(count, sizeof(count));
end;

procedure TBinaryReaderWriter.WriteListDoubleArr(const Value: array of double);
var sect : TBinarySectionRec;
begin
     sect.Len := sizeof(sect) + Length(Value)*Sizeof(double);
     sect.SectTpye := bsListDoubleArr;
     sect.NameLen := 0;

     Stream.Write(sect, sizeof(sect));

     if Length(Value) > 0 then
        Stream.Write(Value[0], Length(Value)*sizeof(double));
end;

procedure TBinaryReaderWriter.WriteListEnd;
var sect : TBinarySectionRec;
begin
     sect.Len := sizeof(sect);
     sect.SectTpye := bsEndList;
     sect.NameLen := 0;

     Stream.WriteBuffer(sect, sizeof(sect));
end;

procedure TBinaryReaderWriter.WriteListIntArr(const Value: array of Integer);
var sect : TBinarySectionRec;
begin
     sect.Len := sizeof(sect) + Length(Value)*Sizeof(Integer);
     sect.SectTpye := bsListIntArr;
     sect.NameLen := 0;

     Stream.Write(sect, sizeof(sect));
     if Length(Value) > 0 then
        Stream.Write(Value[0], Length(Value)*sizeof(integer));
end;

procedure TBinaryReaderWriter.WriteObjDescriptor(const Name, Value: String);
var sect : TBinarySectionRec;
    sName : UTF8String;
    sValue : UTF8String;
begin
     sName := UTF8String(Name);
     sValue := UTF8String(Value);

     sect.Len := sizeof(sect) + Length(sName) + Length(sValue);
     sect.SectTpye := bsObjectDescription;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.WriteBuffer(sName[1], Length(sName));

     if Length(sValue) > 0 then
        Stream.WriteBuffer(sValue[1], Length(sValue));
end;

procedure TBinaryReaderWriter.WriteObject(const Name: String;
  Obj: TBaseMathPersistence);
var sect : TBinarySectionRec;
    aWriter : TBinaryReaderWriter;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName);
     sect.SectTpye := bsObjectBegin;
     sect.NameLen := Length(sName);

     Stream.WriteBuffer(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.WriteBuffer(sName[1], Length(sName));

     // Now write the object
     aWriter := TBinaryReaderWriter.Create;
     try
        aWriter.SaveToStream(Obj, Stream);
     finally
            aWriter.Free;
     end;

     // write closing section
     sect.Len := sizeof(sect);
     sect.SectTpye := bsObjectEnd;
     sect.NameLen := 0;

     Stream.WriteBuffer(sect, sizeof(sect));
end;

procedure TBinaryReaderWriter.WriteObject(Obj: TBaseMathPersistence);
begin
     WriteObject('', obj);
end;

procedure TBinaryReaderWriter.WriteProperty(const Name: String;
  const Value: integer);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + sizeof(LongInt);
     sect.SectTpye := bsInteger;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.WriteBuffer(sName[1], Length(sName));

     Stream.WriteBuffer(Value, sizeof(Value));
end;

procedure TBinaryReaderWriter.WriteStreamHeader;
begin
     Stream.WriteBuffer(cBinaryReaderHeader, sizeof(cBinaryReaderHeader));
     Stream.WriteBuffer(cBinaryReaderVersion, sizeof(cBinaryReaderVersion));
end;

procedure TBinaryReaderWriter.WriteProperty(const Name, Value: String);
var sect : TBinarySectionRec;
    sName : UTF8String;
    sValue : UTF8String;
begin
     sName := UTF8String(Name);
     sValue := UTF8String(Value);

     sect.Len := sizeof(sect) + Length(sName) + Length(sValue);
     sect.SectTpye := bsString;
     sect.NameLen := Length(sName);

     Stream.Write(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.WriteBuffer(sName[1], Length(sName));

     if Length(sValue) > 0 then
        Stream.WriteBuffer(sValue[1], Length(sValue));
end;

procedure TBinaryReaderWriter.WriteProperty(const Name: String;
  const Value: double);
var sect : TBinarySectionRec;
    sName : UTF8String;
begin
     sName := UTF8String(Name);

     sect.Len := sizeof(sect) + Length(sName) + sizeof(double);
     sect.SectTpye := bsDouble;
     sect.NameLen := Length(sName);

     Stream.WriteBuffer(sect, sizeof(sect));
     if Length(sName) > 0 then
        Stream.WriteBuffer(sName[1], Length(sName));

     Stream.WriteBuffer(Value, sizeof(Value));
end;

{ TSectObj }

constructor TSectObj.Create(const Name: String; aObj: TBaseMathPersistence);
begin
     ObjName := Name;
     MathIOObj := aObj;
end;

initialization
  RegisterMathIOReader(TBinaryReaderWriter);

end.
