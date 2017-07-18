// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2016, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit JSONReaderWriter;

interface

// ###################################################
// #### Simple JSON reader and writer classes
// ###################################################

// note: there are a few restrictions that are not
// defined in the JSON format as is.
// 1.) The objects write some kind of header, aka the writer itself identifies
//     itself with the property "Lib":"mrMath". This tag is optional but only if
//     TJSONReaderWriter is invoked directly. Reading from the generic reader function does not work
//     without this header.
// 2.) each Object and thus also the library have a version property. If it is missing
//     Version 1 is assumed.
// 3.) Reading a file is done in one pass and character by character thus the object description needs to be
//     the first property of an object. Before that property only the lib header
//     and the version are allowed to be placed. Ensure that your JSON writer
//     can handle this.
// 4.) Binary properties are simply strings with the prefix 0x .
//     Each byte in the binary stream is simply encoded as hex value of that byte (00-FF)
//     meaing that every byte is encoded into 2 characters.
//     e.g. the byte streams $54$FA$11 is encoded into the string: 0x54FA11

// todo: perhaps Base64 encoding?

uses SysUtils, Classes, BaseMathPersistence, MathUtilFunc, types;

// #############################################################
// #### Loading/saving data
type
  TJsonReaderWriter = class(TCustomMathPersistenceIO)
  private
    fFmt : TFormatSettings;
    fIsFirst : boolean;

    function EncodeJSONString(const s : string) : UTF8String;
    function InternalLoadFromStream(aStream : TStream) : TBaseMathPersistence;
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

    constructor Create;
  end;


implementation

uses Math;

const cJSONHeader : UTF8String = '{"Lib":"mrMath"';
      cJSONVersion : UTF8String = ',"Version":1';
      cJSONFinalize : UTF8String = '}';
{ TJsonReaderWriter }

class function TJsonReaderWriter.CanReadStream(aStream: TStream): boolean;
var buf : UTF8String;
begin
     SetLength(buf, Length(cJSONHeader));
     Result := aStream.Read(buf[1], Length(cJSONHeader)) = Length(cJSONHeader);
     if Result then
        Result := (CompareStr(String(buf), String(cJSONHeader)) = 0);
end;

procedure TJsonReaderWriter.WriteStreamHeader;
begin
     Stream.WriteBuffer(cJSONHeader[1], Length(cJSONHeader));
     Stream.WriteBuffer(cJSONVersion[1], Length(cJSONVersion));
end;

procedure TJsonReaderWriter.FinalizeStream;
begin
     Stream.WriteBuffer(cJSONFinalize[1], Length(cJSONFinalize));
end;

procedure TJsonReaderWriter.WriteObjDescriptor(const Name, Value: String);
var s : UTF8String;
begin
     s := ',"' + EncodeJSONString(Name) + '":"' + EncodeJSONString(Value) + '"';
     Stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteDoubleArr(const Name: String;
  const Value: array of double);
var s : UTF8String;
    counter: Integer;
begin
     s := ',"' + EncodeJSONString(Name) + '":[';
     Stream.WriteBuffer(s[1], Length(s));

     for counter := 0 to Length(Value) - 1 do
     begin
          s := UTF8String( Format('%g', [Value[counter]], fFmt) );
          if counter <> Length(Value) - 1 then
             s := s + ',';
          stream.WriteBuffer(s[1], Length(s));
     end;

     s := ']';
     stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteListBegin(const Name: String; count: integer);
var s : UTF8String;
begin
     s := ',"' + EncodeJSONString(Name) + '":[';
     Stream.WriteBuffer(s[1], Length(s));
     fIsFirst := True;
end;

procedure TJsonReaderWriter.WriteListEnd;
var s : UTF8String;
begin
     s := ']';
     stream.WriteBuffer(s[1], Length(s));
     fIsFirst := False;
end;

procedure TJsonReaderWriter.WriteBinaryProperty(const Name: String; const Value;
  size: integer);
var s : UTF8String;
    counter: Integer;
    pVal : PByte;
begin
     s := ',"' + EncodeJSONString(Name) + '":"0x'; // 0x indicates a binary data string
     // simple encoding (todo: use base64?!)
     pVal := @Value;
     for counter := 0 to size - 1 do
     begin
          s := s + UTF8String( IntToHex(pVal^, 2) );
          inc(pVal);
     end;

     s := s + '"';
     stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteProperty(const Name: String;
  const Value: integer);
var s : utf8String;
begin
     s := UTF8String(Format(',"%s":%d', [EncodeJSONString(Name), Value]));

     Stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteProperty(const Name, Value: String);
var s : UTF8String;
begin
     s := ',"' + EncodeJSONString(Name) + '":"' + EncodeJSONString(Value) + '"';
     Stream.WriteBuffer(s[1], Length(s));
end;


procedure TJsonReaderWriter.WriteProperty(const Name: String;
  const Value: double);
var s : UTF8String;
begin
     s := ',"' + EncodeJSONString(Name) + '":' + UTF8String( Format('%g', [Value], fFmt) );
     Stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteObject(const Name: String;
  Obj: TBaseMathPersistence);
var s : UTF8String;
    aWriter : TJsonReaderWriter;
begin
     s := ',"' + EncodeJSONString(Name) + '":';
     Stream.WriteBuffer(s[1], Length(s));
     // Now write the object
     aWriter := TJsonReaderWriter.Create;
     try
        aWriter.SaveToStream(Obj, Stream);
     finally
            aWriter.Free;
     end;
end;

procedure TJsonReaderWriter.WriteObject(Obj: TBaseMathPersistence);
var aWriter : TJsonReaderWriter;
    c : AnsiChar;
begin
     if not fIsFirst then
     begin
          c := ',';
          Stream.WriteBuffer(c, sizeof(c));
     end;

     fIsFirst := False;

     aWriter := TJsonReaderWriter.Create;
     try
        aWriter.SaveToStream(Obj, Stream);
     finally
            aWriter.Free;
     end;
end;

procedure TJsonReaderWriter.WriteListDoubleArr(const Value: array of double);
var s : UTF8String;
    counter: Integer;
begin
     s := '';
     if not fIsFirst then
        s := ',';
     s := s + '[';
     for counter := 0 to Length(Value) - 1 do
     begin
          s := s + UTF8String( Format('%g', [Value[counter]], fFmt) );
          if counter <> Length(Value) - 1 then
             s := s + ',';
     end;

     s := s + ']';

     fIsFirst := False;
     if Length(s) > 0 then
        Stream.WriteBuffer(s[1], Length(s));
end;

procedure TJsonReaderWriter.WriteIntArr(const Name: String;
  const Value: array of Integer);
var s : UTF8String;
    counter: Integer;
begin
     s := ',"' + EncodeJSONString(Name) + '":[';
     for counter := 0 to Length(Value) - 1 do
     begin
          s := s + UTF8String( Format('%d', [Value[counter]], fFmt) );
          if counter <> Length(Value) - 1 then
             s := s + ',';
     end;

     s := s + ']';

     Stream.WriteBuffer(s[1], Length(s));
end;


procedure TJsonReaderWriter.WriteListIntArr(const Value: array of Integer);
var s : UTF8String;
    counter: Integer;
begin
     s := '';
     if not fIsFirst then
        s := ',';
     s := s + '[';
     for counter := 0 to Length(Value) - 1 do
     begin
          s := s + UTF8String( Format('%d', [Value[counter]], fFmt) );
          if counter <> Length(Value) - 1 then
             s := s + ',';
     end;

     fIsFirst := False;
     s := s + ']';

     if Length(s) > 0 then
        Stream.WriteBuffer(s[1], Length(s));
end;

function TJsonReaderWriter.EncodeJSONString(const s: string): UTF8String;
var counter: Integer;
    resIdx : integer;
    res : UTF8String;
    hexStr : UTF8String;
procedure AddChar(const c : AnsiChar);
begin
     res[resIdx] := c;
     inc(resIdx);
end;
begin
     resIdx := 1;
     // reserve the largest possible encoded string as memory once
     SetLength(res, 6*Length(s));

     for counter := 1 to Length(s) do
     begin
          // check if the char needs to be encoded
          case s[counter] of
            '"',
            '\',
            '/': begin
                      AddChar('\');
                      AddChar( AnsiChar( s[counter]) );
                 end;
            #08: begin // backspace
                      AddChar('\');
                      AddChar('b');
                 end;
            #09: begin // tab
                      AddChar('\');
                      AddChar('t');
                 end;
            #10: begin // carrige return
                      AddChar('\');
                      AddChar('r');
                 end;
            #12: begin // form feed
                      AddChar('\');
                      AddChar('f');
                 end;
            #13: begin // line feed
                      AddChar('\');
                      AddChar('n');
                 end;
          else
              // check for: numeric, letter (a..z) and white space
              // all others encode!
              if ((Ord(s[counter]) >= Ord('a')) and (ord(s[counter]) <= Ord('z'))) or
                 ((Ord(s[counter]) >= Ord('A')) and (ord(s[counter]) <= Ord('Z'))) or
                 (s[counter] = ' ') or ((Ord(s[counter]) >= Ord('0')) and (Ord(s[counter]) <= Ord('9')))
              then
                  AddChar(AnsiChar(s[counter]))
              else
              begin
                   AddChar('\');
                   AddChar('u');

                   hexStr := Utf8String( IntToHex(ord(s[counter]), 4) );
                   assert(Length(hexStr) = 4, 'IntToHex failed');
                   AddChar(hexStr[1]);
                   AddChar(hexStr[2]);
                   AddChar(hexStr[3]);
                   AddChar(hexStr[4]);
              end;
          end;
     end;

     Result := Copy(res, 1, resIdx - 1);
end;

constructor TJsonReaderWriter.Create;
begin
     inherited Create;

     ffmt := GetLocalFMTSet;
     fFmt.DecimalSeparator := '.';
end;

function TJsonReaderWriter.LoadFromStream(
  aStream: TStream): TBaseMathPersistence;
var c : AnsiChar;
begin
     // state machine to decode json:
     aStream.ReadBuffer(c, sizeof(c));

     if c <> '{' then
        raise EReadError.Create('No valid JSON format - stream must start with "{"');

     Result := InternalLoadFromStream(aStream);
end;


type
  TJSONState = (sObj, sString, sStringEscape, sStringEscHex, sPropertyEnd,
                sValueBegin, sValueEnd, sListBegin, slistValue, slistNextValue, sListEnd,
                sNumericBegin, sNumericNum, sNumericFrac, sNumericExp,
                sNumericExpSign, sNumericExpVal);
  TJSONListType = (jlNone, jlDouble, jlString, jlInt, jlObject, jlDoubleArr, jlIntArr);


function TJsonReaderWriter.InternalLoadFromStream(
  aStream: TStream): TBaseMathPersistence;

var c : Ansichar;
    sProp : string;
    sVal : string;
    states : Array of TJSONState;
    stateIdx : integer;
    listType : TJSONListType;
    dVal : double;
    iVal : integer;
    intIdx : integer;
    numType : TPropType;

    binDat : TByteDynArray;
    listD : TDoubleDynArray;
    listDD : Array of TDoubleDynArray;
    listI : TIntegerDynArray;
    listII : Array of TIntegerDynArray;
    listS : TStringDynArray;
    listO : Array of TBaseMathPersistence;
    listCnt : integer;

    aObj : TBaseMathPersistence;
    mathIOClass : TBaseMathPersistenceClass;
    version: Integer;
    counter: Integer;

function DecodeStringToBinary(const s : string) : TByteDynArray;
var binIdx : integer;
    counter : integer;
begin
     SetLength(Result, (Length(s) - 2) div 2);
     binIdx := 0;
     counter := 3;
     while counter < Length(s) do
     begin
          Result[binIdx] := StrToInt( '$' + s[counter] + s[counter + 1] );
          inc(counter, 2);
          inc(binIdx);
     end;

end;

procedure PushState(aState : TJSONState);
begin
     if stateIdx >= Length(states) then
        SetLength(states, 100 + Length(states));

     states[stateIdx] := aState;
     inc(stateIdx);
end;

function PeekState : TJSONState;
begin
     Result := states[stateIdx - 1];
end;

procedure PopState;
begin
     dec(stateIdx);

     if stateIdx < 0 then
        raise EReadError.Create('Error - json stack went negative');
end;

function IsWhiteSpace(c : AnsiChar) : boolean;
begin
     Result := c in [#9, ' ', #13, #10];
end;

function ReadValue(ch : AnsiChar; readInt : boolean) : double;
const cBase : double = 10;
var sign : double;
    c : AnsiChar;
    fracFact : double;
    expSign : integer;
    expVal : integer;
begin
     Result := 0;
     PushState(sNumericBegin);
     sign := 1;
     expSign := 1;
     expVal := 0;
     c := ch;
     fracFact := 0.1;
     repeat
           case PeekState of
             sNumericBegin: begin
                                 if c = '+'
                                 then
                                     sign := 1
                                 else if c = '-'
                                 then
                                     sign := -1
                                 else if c in ['0'..'9']
                                 then
                                     Result := Ord(c) - Ord('0')
                                 else
                                     raise EReadError.Create('Error: unsupported character on numerics');

                                 PopState;
                                 PushState(sNumericNum);
                            end;
             sNumericNum: begin
                               if c = '.' then
                               begin
                                    if readInt then
                                       raise EReadError.Create('Error integer value expected');

                                    PopState;
                                    PushState(sNumericFrac);
                               end
                               else if (c = 'e') or (c = 'E') then
                               begin
                                    PopState;
                                    PushState(sNumericExpSign);
                               end
                               else if (c in ['0'..'9']) then
                               begin
                                    Result := Result*10 + Ord(c) - Ord('0');
                               end
                               else
                                   break; // any other character stops the decoding
                          end;
             sNumericFrac: begin
                                if (c = 'e') or (c = 'E') then
                                begin
                                    if readInt then
                                       raise EReadError.Create('Error no exponential allowed on integer values');
                                    PopState;
                                    PushState(sNumericExpSign);
                               end
                               else if (c in ['0'..'9']) then
                               begin
                                    Result := Result + fracFact*(Ord(c) - Ord('0'));
                                    fracFact := fracFact*0.1;
                               end
                               else
                                   break; // any other character stops the decoding
                           end;
             sNumericExpSign: begin
                                   if (c = '-')
                                   then
                                       expSign := -1
                                   else if (c = '+')
                                   then
                                       expSign := 1
                                   else if c in ['0'..'9']
                                   then
                                       expVal := Ord(c) - Ord('0')
                                   else
                                       raise EReadError.Create('Error in decoding the exponent');

                                   PopState;
                                   PushState(sNumericExp);
                              end;
             sNumericExp: begin
                               if c in ['0'..'9']
                               then
                                   expVal := expVal*10 + (Ord(c) - Ord('0'))
                               else
                                   break; // any other character stops the decoding
                          end;
           else
               raise EReadError.Create('Error: unrecognized state on numerics');
           end;
     until aStream.Read(c, sizeof(c)) <> sizeof(c);

     PopState;

     // calculate the result
     Result := sign*Result;
     if expVal <> 0 then
        Result := Result*IntPower(cBase, expSign*expVal);

     // undo the last read -> it is not part of the value
     aStream.Seek(-sizeof(c), soFromCurrent);
end;

function ReadInt(ch : AnsiChar) : integer;
begin
     Result := Round(ReadValue(Ch, True));
end;
function ReadNumeric(ch : AnsiChar) : double;
begin
     Result := ReadValue(ch, False);
end;


function ReadString : string;
var c : AnsiChar;
    sHlp : string;
begin
     sHlp := '';
     PushState(sString);
     Result := '';
     while aStream.Read(c, sizeof(c)) = 1 do
     begin
          case PeekState of
            sString: begin
                          // deode a string
                          if c = '\' then
                          begin
                               PushState(sStringEscape);
                               continue;
                          end;

                          if c = '"' then
                          begin
                               // string end -> return
                               PopState;
                               break;
                          end
                          else
                              Result := Result + String(c);
                     end;
             sStringEscape: begin
                                 // last character was escaped
                                 // -> check current
                                 if c = 'u' then
                                 begin
                                      sHlp := '$';
                                      PushState(sStringEscHex); // next four chars are hex chars
                                      continue;
                                 end;
                                 if (c = '"') or (c = '\') or (c = '/')
                                 then
                                     Result := Result + String(c)
                                 else if c = 'b'
                                 then
                                     Result := Result + #8
                                 else if c = 't'
                                 then
                                     Result := Result + #9
                                 else if c = 'r'
                                 then
                                     Result := Result + #10
                                 else if c = 'f'
                                 then
                                     Result := Result + #12
                                 else if c = 'n'
                                 then
                                     Result := Result + #13
                                 else
                                     raise EReadError.Create('JSON string not encoded correctly ');

                                 PopState;
                            end;
             sStringEscHex: begin
                                 if c in ['0'..'9', 'a'..'f', 'A'..'F']
                                 then
                                     sHlp := sHlp + String(c)
                                 else
                                     raise EReadError.Create('Not a correct hex value');

                                 if Length(sHlp) = 5 then
                                 begin
                                      Result := Result + Char( StrToInt(sHlp) );
                                      PopState;
                                      PopState;
                                 end;
                            end;
          else
              raise EReadError.Create('Incorrect state when reading strings');
          end;
     end;
end;

function ReadList(readIntVal : boolean) : TDoubleDynArray;
var c : AnsiChar;
    retCnt : integer;
begin
     PushState(slistNextValue);
     Result := nil;
     retCnt := 0;

     while stateIdx > 0 do
     begin
          aStream.ReadBuffer(c, sizeof(c));

          case PeekState of
            slistValue: begin
                             if IsWhiteSpace(c) then
                                continue;

                             if c = ',' then
                             begin
                                  PopState;
                                  PushState(slistNextValue);
                             end
                             else if c = ']' then
                             begin
                                  // ###################################
                                  // #### Break the loop here!
                                  break;
                             end
                             else
                                 raise EReadError.Create('Error unrecognized character when reading the list');
                        end;
            slistNextValue: begin
                                 if IsWhiteSpace(c) then
                                    continue;

                                 if c in ['+', '-', '0'..'9'] then
                                 begin
                                      if retCnt >= Length(Result) then
                                         SetLength(Result, Min(Length(Result) + 100, 1 + 2*Length(Result)));

                                      if readIntVal
                                      then
                                          Result[retCnt] := ReadInt(c)
                                      else
                                          Result[retCnt] := ReadNumeric(c);

                                      inc(retCnt);

                                      PopState;
                                      PushState(slistValue);
                                 end;
                            end;

          else
              raise EReadError.Create('Error state not recognized in reading a list');
          end;
     end;

     PopState;

     SetLength(Result, retCnt);
end;

function ReadListDouble : TDoubleDynArray;
begin
     Result := ReadList(False);
end;

function ReadListInt : TIntegerDynArray;
var dblArr : TDoubleDynArray;
    counter : integer;
begin
     dblArr := ReadList(True);

     SetLength(Result, Length(dblArr));
     for counter := 0 to Length(Result) - 1 do
         Result[counter] := round(dblArr[counter]);
end;

begin
     states := nil;
     stateIdx := 0;
     Result := nil;
     listType := jlNone;
     listCnt := 0;
     listD := nil;
     listO := nil;
     listI := nil;
     listS := nil;
     listdd := nil;
     listii := nil;
     sProp := '';

     PushState(sObj);

     while stateIdx > 0 do
     begin
          aStream.ReadBuffer(c, sizeof(c));

          case PeekState of
            sObj: begin
                       // white spaces are allowed (and ignored)
                       if IsWhiteSpace(c) then
                          continue;

                       if c = '"' then
                       begin
                            sProp := ReadString;
                            PopState;
                            PushState(sPropertyEnd);
                       end
                       else
                           raise EReadError.Create('Decode Error - no property found');
                   end;
            sPropertyEnd: begin
                               if IsWhiteSpace(c) then
                                  continue;

                               if c = ':' then
                               begin
                                    PopState;
                                    PushState(sValueBegin);
                               end;
                          end;
            sValueBegin: begin
                              if IsWhiteSpace(c) then
                                 continue;

                              if c = '"' then
                              begin
                                   PushState(sString);
                                   sVal := ReadString;

                                   if sProp = '' then
                                      raise EReadError.Create('Error - no object name given');


                                   if sameText(sProp, 'lib') then
                                   begin
                                        if not SameText(sVal, 'mrMath') then
                                           raise EReadError.Create('Error wrong lib qualifier');
                                   end
                                   else if SameText(sProp, 'obj') then
                                   begin
                                        mathIOClass := MathIOClassFromName(sVal);
                                        if not Assigned(mathIOClass) then
                                           raise EReadError.Create('Unknown class ' + sVal);

                                        Result := mathIOClass.Create;
                                   end
                                   else
                                   begin
                                        if not Assigned(Result) then
                                           raise EReadError.Create('Error no object type given before a value has been read.');

                                        if (Length(sVal) > 2) and (sVal[1] = '0') and (sVal[2] = 'x') then
                                        begin
                                             // decode binary property...
                                             binDat := DecodeStringToBinary(sVal);
                                             if Length(binDat) = 0 then
                                                raise EReadError.Create('Error: No binary data decoded');

                                             ReadBinaryProperty(Result, sProp, binDat[0], Length(binDat));
                                        end
                                        else
                                            ReadStringProperty(Result, sProp, sVal);
                                   end;
                                   sProp := '';
                                   sVal := '';
                                   PopState;
                                   PushState(sValueEnd);
                              end
                              else if c in ['+', '-', '0'..'9'] then
                              begin
                                   iVal := 0;
                                   dVal := 0;

                                   if SameText(sProp, 'Version') then
                                   begin
                                        version := ReadInt(c);

                                        WriteVersion(version);

                                        // todo: do something with the version
                                        PopState;
                                        PushState(sValueEnd);
                                        continue;
                                   end;

                                   if not Assigned(Result) then
                                      raise EReadError.Create('Error no object type given before a value has been read.');

                                   numType := PropTypeOfName(Result, sProp);
                                   intIdx := Pos('_i', sProp);
                                   if (IntIdx > 0) and (intIdx = Length(sProp) - 1) then
                                   begin
                                        SetLength(sProp, Length(sProp) - 2);
                                        numType := ptInteger;
                                   end;

                                   if numType = ptInteger
                                   then
                                       iVal := ReadInt(c)
                                   else
                                       dVal := ReadNumeric(c);

                                   if sProp = '' then
                                      raise EReadError.Create('Error - no object name given');

                                   if numType = ptInteger
                                   then
                                       ReadIntProperty(Result, sProp, iVal)
                                   else
                                       ReadDoubleProperty(Result, sProp, dVal);
                                   sProp := '';
                                   sVal := '';
                                   PopState;
                                   PushState(sValueEnd);
                              end
                              else if c = '[' then
                              begin
                                   PopState;
                                   PushState(sListBegin);
                                   listCnt := 0;
                                   listType := jlNone;
                              end
                              else if c = '{' then
                              begin
                                   aObj := InternalLoadFromStream(aStream);

                                   if not Assigned(Result) then
                                      raise EReadError.Create('Error no object type given before a value has been read.');
                                   if sProp = '' then
                                      raise EReadError.Create('Error - no object name given');

                                   if not ReadObject( Result, sProp, aObj ) then
                                      aObj.Free;

                                   sProp := '';
                                   PopState;
                                   PushState(sValueEnd);
                              end
                              else if c = '[' then
                              begin
                                   listCnt := 0;
                                   listType := jlNone;
                                   PopState;
                                   PushState(sListBegin);
                              end
                              else
                                  raise EReadError.Create('Error illegal character');
                         end;
            sListBegin: begin
                             if IsWhiteSpace(c) then
                                continue;

                             if c = ']' then
                             begin
                                  // empty list...
                                  PopState;
                                  PushState(sValueEnd);
                             end
                             else if c = '"' then
                             begin
                                  if listType = jlNone
                                  then
                                      listType := jlString
                                  else
                                      raise EReadError.Create('Error list already initalized');

                                  if listCnt >= Length(listS) then
                                     SetLength(listS, Min(length(listS) + 100, 1 + 2*Length(listS)));

                                  listS[listCnt] := ReadString;
                                  inc(listCnt);
                                  PopState;
                                  PushState(slistValue);
                             end
                             else if c = '{' then
                             begin
                                  if listType = jlNone
                                  then
                                      listType := jlObject
                                  else
                                      raise EReadError.Create('Error list already initalized');

                                  if listCnt >= Length(listO) then
                                     SetLength(listO, Min(length(listO) + 100, 1 + 2*Length(listO)));

                                  listO[listCnt] := InternalLoadFromStream(aStream);
                                  inc(listCnt);
                                  PopState;
                                  PushState(slistValue);
                             end
                             else if c = '[' then
                             begin
                                  if listType <> jlNone then
                                     raise EReadError.Create('Error list already initalized');

                                  intIdx := Pos('_i', sProp);
                                  numType := PropTypeOfName(Result, sProp);
                                  if (numType = ptInteger) or ((intIdx > 0) and (intIdx = Length(sProp) - 1)) then
                                  begin
                                       if numType <> ptInteger then
                                          SetLength(sProp, Length(sProp) - 2);
                                       listType := jlIntArr;
                                       if listCnt >= Length(listII) then
                                          SetLength(listII, Min(length(listII) + 100, 1 + 2*Length(listII)));
                                  end
                                  else
                                  begin
                                       listType := jlDoubleArr;
                                       if listCnt >= Length(listDD) then
                                          SetLength(listDD, Min(length(listDD) + 100, 1 + 2*Length(listDD)));
                                  end;

                                  if listType = jlDoubleArr
                                  then
                                      listDD[listCnt] := ReadListDouble
                                  else
                                      listII[listCnt] := ReadListInt;

                                  inc(listCnt);
                                  PopState;
                                  PushState(slistValue);

                             end
                             else if c in ['0'..'9', '+', '-'] then
                             begin
                                  if listType <> jlNone then
                                     raise EReadError.Create('Error list already initalized');

                                  intIdx := Pos('_i', sProp);
                                  numType := PropTypeOfName(Result, sProp);
                                  if (numType = ptInteger) or ((intIdx > 0) and (intIdx = Length(sProp) - 1)) then
                                  begin
                                       if numType <> ptInteger then
                                          SetLength(sProp, Length(sProp) - 2);
                                       listType := jlInt;
                                       if listCnt >= Length(listI) then
                                          SetLength(listI, Min(length(listI) + 100, 1 + 2*Length(listI)));
                                  end
                                  else
                                  begin
                                       listType := jlDouble;
                                       if listCnt >= Length(listD) then
                                          SetLength(listD, Min(length(listD) + 100, 1 + 2*Length(listD)));
                                  end;

                                  if listType = jlInt
                                  then
                                      listI[listCnt] := ReadInt(c)
                                  else
                                      listD[listCnt] := ReadNumeric(c);

                                  inc(listCnt);
                                  PopState;
                                  PushState(slistValue);
                             end;
                        end;
            slistValue: begin
                             if IsWhiteSpace(c) then
                                continue;

                             if c = ',' then
                             begin
                                  PopState;
                                  PushState(slistNextValue);
                                  continue;
                             end
                             else if c = ']' then
                             begin
                                  if not Assigned(Result) then
                                     raise EReadError.Create('No object type given before list is read');
                                  case listType of
                                   jlDouble: begin
                                                  SetLength(listD, listCnt);
                                                  ReadDoubleArr(Result, sProp, listD);
                                             end;
                                   jlString: begin
                                                  ReadBeginList(Result, sProp, listCnt);
                                                  for counter := 0 to listCnt - 1 do
                                                      ReadStringProperty(Result, '', listS[counter]);
                                                  ReadEndList(Result);
                                             end;
                                   jlInt: begin
                                               SetLength(listI, listCnt);
                                               ReadIntArr(Result, sProp, listI);
                                          end;
                                   jlObject: begin
                                                  ReadBeginList(Result, sProp, listCnt);
                                                  for counter := 0 to listCnt - 1 do
                                                      if not ReadObject(Result, listO[counter])  then
                                                         listO[counter].Free;
                                                  ReadEndList(Result);
                                             end;
                                   jlDoubleArr: begin
                                                     ReadBeginList(Result, sProp, listCnt);
                                                     for counter := 0 to listCnt - 1 do
                                                         ReadListDoubleArr(Result, listDD[counter]);
                                                     ReadEndList(Result);
                                                end;
                                   jlIntArr: begin
                                                  ReadBeginList(Result, sProp, listCnt);
                                                  for counter := 0 to listCnt - 1 do
                                                      ReadListIntArr(Result, listII[counter]);
                                                  ReadEndList(Result);
                                             end;

                                  else
                                      raise EReadError.Create('Error non recognized array type');
                                  end;
                             end;

                             listO := nil;
                             listI := nil;
                             listD := nil;
                             listS := nil;
                             listDD := nil;
                             listII := nil;
                             listCnt := 0;
                             PopState;
                             PushState(sValueEnd);
                        end;
            sListNextValue: begin
                                 if IsWhiteSpace(c) then
                                    continue;

                                 if c = '"' then
                                 begin
                                      if listType <> jlString then
                                         raise EReadError.Create('Unrecognized string qualifier in list');
                                      PushState(sString);
                                      sVal := ReadString;

                                      if listCnt >= Length(listS) then
                                         SetLength(listS, Min(length(listS) + 100, 1 + 2*Length(listS)));

                                      listS[listCnt] := ReadString;
                                      inc(listCnt);
                                      PopState;
                                      PushState(slistValue);
                                 end
                                 else if c in ['+', '-', '0'..'9'] then
                                 begin
                                      if listType = jlInt then
                                      begin
                                           if listCnt >= Length( listI ) then
                                              SetLength(listI, Min(length(listi) + 100, 1 + 2*Length(listI)));

                                           listI[listCnt] := ReadInt(c);
                                           inc(listCnt);
                                      end
                                      else if listType = jlDouble then
                                      begin
                                           if listCnt >= Length( listD ) then
                                              SetLength(listD, Min(length(listD) + 100, 1 + 2*Length(listD)));

                                           listD[listCnt] := ReadNumeric(c);
                                           inc(listCnt);
                                      end
                                      else
                                          raise EReadError.Create('Error unrecognized value found in list');

                                      PopState;
                                      PushState(slistValue);
                                 end
                                 else if c = '{' then
                                 begin
                                      if listType <> jlObject then
                                         raise EReadError.Create('Error unrecognized object found in list');

                                      if listCnt >= Length(listO) then
                                         SetLength(listO, Min(length(listO) + 100, 1 + 2*Length(listO)));

                                      listO[listCnt] := InternalLoadFromStream(aStream);
                                      inc(listCnt);
                                      PopState;
                                      PushState(slistValue);
                                 end
                                 else if c = '[' then
                                 begin
                                      if (listType <> jlDoubleArr) and (listType <> jlIntArr) then
                                         raise EReadError.Create('Error unrecognized list found in a list');

                                      if listType = jlDoubleArr then
                                      begin
                                           if listCnt >= Length(listD) then
                                              SetLength(listDD, Min(length(listDD) + 100, 1 + 2*Length(listDD)));

                                           listDD[listCnt] := ReadListDouble;
                                      end
                                      else if listType = jlIntArr then
                                      begin
                                           if listCnt >= Length(listII) then
                                              SetLength(listII, Min(length(listII) + 100, 1 + 2*Length(listII)));

                                           listII[listCnt] := ReadListInt;
                                      end;

                                      inc(listCnt);
                                      PopState;
                                      PushState(slistValue);
                                 end
                                 else
                                     raise EReadError.Create('Error illegal character');
                         end;
            sValueEnd: begin
                            if IsWhiteSpace(c) then
                               continue;

                            listCnt := 0;
                            sProp := '';
                            sVal := '';

                            if c = '}' then
                            begin
                                 // end of object!! -> return
                                 if not Assigned(Result) then
                                    raise EReadError.Create('Error no object to return');

                                 // exit the loop!
                                 break;
                            end
                            else if c = ',' then
                            begin
                                 PopState;
                                 PushState(sObj);
                            end
                            else
                                raise EReadError.Create('Error charcter not allowed after value');
                       end;

          end;
     end;

     if Assigned(Result) then
        ReadFinalize(Result);
end;


initialization
  RegisterMathIOReader(TJsonReaderWriter);


end.
