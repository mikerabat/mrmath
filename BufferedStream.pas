// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ###################################################################

unit BufferedStream;

// ###############################################
// #### Buffered stream reading
// ###############################################

interface

uses SysUtils, Classes;

// ###############################################
// #### Reads blockwise from stream
// This class speedups the access to files
// by reading blocksize bytes from it and handle the bytewise reading
// from a buffer 
type
  TBufferedStream = class(TStream)
  private
    fStream : TStream;
    fBuffer : array of Byte;
    fActPos : PByte;
    fBytesLeft : integer;
    fBlockSize: integer;
    fStartPos : int64;
    fBytesRead : int64;
    fReadBlockSize : integer;
    function InternalRead(var Buf; BufSize : integer) : integer;
  public
    function Read(var Buf; BufSize : integer) : integer; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property BlockSize : integer read fBlockSize;
    constructor Create(Stream : TStream; aBlockSize : integer);
    destructor Destroy; override;
  end;

implementation

uses Math;

{ TBufferedStream }

constructor TBufferedStream.Create(Stream: TStream; aBlockSize: integer);
begin
     inherited Create;

     assert(Assigned(stream), 'Stream may not be nil');
     assert(BlockSize > 0, 'Error Blocksize must be larger than 0');

     fBlockSize := aBlockSize;
     SetLength(fBuffer, aBlockSize);
     fStream := Stream;

     fActPos  := @fBuffer[0];
     fBytesLeft := 0;

     fStartPos := Stream.Position;
     fBytesRead := 0;
     fReadBlockSize := 0;
end;

destructor TBufferedStream.Destroy;
begin
     {$IF DEFINED(FPC) or (CompilerVersion <= 21)}
     fStream.Seek(fStartPos + fBytesRead, soFromBeginning);
     {$ELSE}
     fStream.Seek(fStartPos + fBytesRead, soBeginning);
     {$IFEND}

     inherited;
end;

function TBufferedStream.InternalRead(var Buf; BufSize: integer) : integer;
var toBuf : PByte;
    bytesToRead : integer;
begin
     Result := 0;
     toBuf := @Buf;

     // #################################################
     // #### Read from stream
     while BufSize > fBytesLeft do
     begin
          // read a block of data
          if fBytesLeft > 0 then
          begin
               Move(fActPos^, toBuf^, fBytesLeft);
               inc(toBuf, fBytesLeft);
               inc(fBytesRead, fBytesLeft);
               inc(Result, fBytesLeft);
               dec(BufSize, fBytesLeft);

               fBytesLeft := 0;
          end;

          fActPos := @fBuffer[0];
          fBytesLeft := fStream.Read(fBuffer[0], fBlockSize);
          fReadBlockSize := fBytesLeft;
          
          if fBytesLeft < fBlockSize then
             break;
     end;

     // #################################################
     // #### Read from the buffer
     bytesToRead := Min(fBytesLeft, BufSize);
     Move(fActPos^, toBuf^, bytesToRead);
     inc(fActPos, bytesToRead);
     inc(Result, bytesToRead);
     inc(fBytesRead, bytesToRead);
     dec(fBytesLeft, bytesToRead);
end;

function TBufferedStream.Read(var Buf; BufSize: integer): integer;
begin
     Result := InternalRead(Buf, BufSize);
end;

function TBufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var NewPosition : Int64;
    actBlockStart : Int64;
begin
     case origin of
      soBeginning: NewPosition := Offset;
      soCurrent: NewPosition := fBytesRead + Offset;
      soEnd: NewPosition := fStream.Size + Offset;
     else
       NewPosition := 0;
     end;

     // check if the new position is within the actual buffer:
     actBlockStart := fstream.Position - fReadBlockSize;
     if (NewPosition >= actBlockStart) and (NewPosition < actBlockStart + fReadBlockSize) then
     begin
          Result := NewPosition;

          fBytesRead := NewPosition - actBlockStart;
          fBytesLeft := fReadBlockSize - (NewPosition - actBlockStart);
          fActPos := @fBuffer[0];
          inc(fActPos, fReadBlockSize - fBytesLeft);
     end
     else
     begin
          // clear and wait until the next block is read
          fBytesRead := NewPosition;
          fBytesLeft := 0;
          {$IF DEFINED(FPC) or (CompilerVersion <= 21)}
          Result := fStream.Seek(NewPosition, soFromBeginning);
          {$ELSE}
          Result := fStream.Seek(NewPosition, soBeginning);
          {$IFEND}

          fReadBlockSize := 0;
          fActPos := @fBuffer[0];
     end;
end;

function TBufferedStream.Write(const Buffer; Count: Integer): Longint;
begin
     Result := -1;
     raise Exception.Create('Writes not implemented');
end;

end.
