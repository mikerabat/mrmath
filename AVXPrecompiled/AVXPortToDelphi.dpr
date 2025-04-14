program AVXPortToDelphi;

{$APPTYPE CONSOLE}

uses Windows, SysUtils, Classes, StrUtils, Math;


var inpPath : string;
    pasPath : string;
    sr : TSearchRec;
    verbose : boolean;
    s : string;
    definestr : string;  // define that is inserted for the opcodes. Def is: {$IFDEF AVXSUP}
    searchDefineStr : string;   // to exchange already placed ifdefs use this. Def is definestr
    tabStopElse : integer;  // defines the number of spaces from left where the {$ELSE} string shall start
                            // default = 55;

function LineToDBStr( aLine : string ) : string;
var s1, s2, s3, s4, s5, s6 : string;
    i1, i2, i3 : integer;
begin
     aLine := Copy(aLine, Pos(' _ ', aLine) + 3, Length(aLine));

     // format: C5 FD: 28. 4C 01, 80 stands for vmovapd ymm1, ymmword ptr [ecx+eax-80H]
     // or: vmovupd ymm0, ymmword ptr [TC_$MATRIXCONST_$$_CSIGNBITS4]; 0021 _ C5 FD: 10. 05, 00000000(d)

     Result := 'db ';
     i1 := Pos( ': ', aLine );

     s5 := Copy(aLine, 1, i1 - 1);
     s5 := ReplaceStr(s5, ' ', ',$');
     Result := Result + '$' + s5;

     i1 := i1 + 2;
     
     //Result := Result + ',$' + Copy(aLine, i1 - 4, 2);
     i2 := Pos( '. ', aLine);
     if i2 = 0 then
     begin
          // not much more to process -> build result and exit
          i2 := Length(aLine);
          Result := Result + ',$' + Copy(aLine, i1, i2 - i1 + 1) + ';';
          exit;
     end;
     s1 := Copy(aLine, i1, i2 - i1);
     i1 := Pos(', ', aLine);
     if i1 = 0 then
        i1 := Length(aLine) + 1;
     i2 := i2 + 2;
     s2 := Copy(aLine, i2, i1 - i2);
     s2 := ReplaceStr(s2, ' ', ',$');
     i1 := i1 + 2;
     s3 := Copy(aLine, i1, Length(aLine));

     if (Pos('(rel)', aLine) > 0) or (Pos('(d)', aLine) > 0) then
        raise Exception.Create('A reference to a relative or absolute address for an AVX/FMA instruction was found - this cannot be processed');

     // invert the hex codes instead of $00000080 we need $80000000
     SetLength(s6, Length(s3));
     i3 := 0;
     while i3 < Length(s6) do
     begin
          Move( s3[i3 + 1], s6[Length(s6) - i3 - 1], sizeof(char)*2 );
          inc(i3, 2);
     end;
      
     
     s4 := '';
     while Length(s6) > 0 do
     begin
          s4 := s4 + ',$' + Copy(s6, 1, 2);
          delete(s6, 1, 2);
     end;
    
     Result := Result + ',$' + s1 + ',$' + s2 + s4 + ';';
end;

procedure EvalOneFile( asmfile, pasFile : string );
var slIn : TStringList;
    slOut : TStringList;
    i, j : Integer;
    curSect : string;
    pasIdx : integer;
    s : string;
    opCode : string;
    dbStr : string;
    outLine : string;
    pre : string;
    preIdx : integer;
    idx : integer;
    cmt : string;
begin
     slIn := TStringList.Create;
     slOut := TStringList.Create;

     slIn.LoadFromFile(asmFile, TEncoding.ASCII);
     slOut.LoadFromFile(pasFile, TEncoding.ASCII);

     curSect := '';
     pasIdx := -1;
     for i := 0 to slIn.Count - 1 do
     begin
          // AVX in the first few characters hints for a procedure/function name
          if (Pos(' PROC', slIn[i] ) > 1) then // or (Pos('SINGLE PROC', slIn[i] ) > 1) then
          begin
               // search for the function name in the output file
               s := slIn[i];

               s := Copy(s, Pos('_$$_', s) + 4, 1000);
               s := Copy(s, 1, Pos('$', s) - 1);

               if curSect <> s then
               begin
                    curSect := s;
                    pasIdx := -1;
                    for j := slOut.IndexOf('implementation') to slOut.Count - 1 do
                    begin
                         if (Pos(UpperCase('procedure ' + s + '('), UpperCase(slOut[j])) > 0) or
                            (Pos(UpperCase('function ' + s + '('), UpperCase(slOut[j])) > 0)
                         then
                         begin
                              pasIdx := j;
                              break;
                         end;
                    end;
               end;
          end
          else if pasIdx >= 0 then
          begin
               // evaluate the section aka procedure

               // strip the first 9 chars -> (either ?001 or simply spaces
               s := Copy( slIn[i], 9, Length(slIn[i]));

               if s <> '' then
               begin
                    // vex encoded -> simply check if the first element is a "v"....
                    if s[1] = 'v' then
                    begin
                         opCode := Copy(s, 1, Pos(' ', s) - 1);

                         dbStr := LineToDBStr( Copy(s, Pos(';', s) + 1, Length(s)) );

                         // now search and replace for the next occurence of this opcode
                         while pasIdx < slOut.Count do
                         begin
                              opCode := LowerCase(opcode);
                              preIdx := Pos(opCode, LowerCase(slOut[pasIdx]));
                              if preIdx > 0 then
                              begin
                                   pre := StringOfChar( ' ', preIdx - 1);

                                   // found -> replace by ifdef
                                   outLine := slOut[pasIdx];

                                   // first cut the cmt
                                   idx := Pos('//', outLine);
                                   cmt := '';
                                   if idx > 0 then
                                   begin
                                        cmt := Copy(outLine, idx, Length(outLine));
                                        outLine := Copy(outLine, 1, idx - 1);
                                   end;


                                   idx := Pos(searchDefineStr, outLine);
                                   if idx > 0 then
                                   begin
                                        outLine := Copy(outLine, idx + length(searchDefineStr), Length(outLine));
                                        pre := StringOfChar(' ', idx - 1);
                                   end
                                   else
                                       outLine := Copy(outLine, preIdx, Length(outLine));
                                   idx := Pos('{$ELSE}', UpperCase(outLine));
                                   if idx > 0 then
                                      outLine := TrimRight( Copy(outLine, 1, idx - 1) );

                                   outLine := pre + definestr + outLine;
                                   outline := outline + StringOfChar(' ', Max(0, tabStopElse - Length(outline) )) +
                                              '{$ELSE}' + dbStr + '{$ENDIF}' + ' ' + cmt;

                                   if verbose then
                                      Writeln('Replace ', outLine);

                                   slout[pasIdx] := outLine;

                                   inc(pasIdx);
                                   break;
                              end;

                              inc(pasIdx);
                         end;
                    end;
               end;
          end;
     end;

     // ###########################################
     // #### Write back result
     slOut.SaveToFile(pasFile, TEncoding.ASCII);
end;

var i : integer;
begin
  try
     inpPath := '.\oFiles\';
     pasPath := '..\';
     verbose := True;
     definestr := '{$IFDEF AVXSUP}';
     searchDefineStr := definestr;
     tabStopElse := 70;

     i := 1;
     while i < ParamCount do
     begin
          if SameText( ParamStr(i), '-inpath') then
             inpPath := ParamStr(i + 1);
          if SameText( ParamStr(i), '-paspath') then
             pasPath := ParamStr(i + 1);
          if SameText( ParamStr(i), '-define') then
             defineStr := '{$IFDEF ' + ParamStr(i + 1) + '}';
          if SameText( ParamStr(i), '-searchdefine') then
             searchDefineStr := '{$IFDEF ' + ParamStr(i + 1) + '}';
          if SameText( ParamStr(i), '-tabstop') then
             tabStopElse := StrToInt(ParamStr(i + 1));

          inc(i);
     end;


     Writeln('Checking input dir: ' + inpPath);
     Writeln('Checking output dir: ' + paspath);

     if FindFirst( inpPath + '*.asm', faAnyFile, sr) = 0 then
     begin
          repeat
                Writeln('');
                Writeln('');
                Writeln('Processing...' + sr.Name);

                s := pasPath + sr.Name;
                s := ChangeFileExt(s, '.pas');

                if FileExists(s) then
                begin
                     //CopyFile( PChar( s ), PChar(s + '.orig'), False );
                     EvalOneFile( inpPath + sr.Name, s);
                     Writeln('Done');
                end
                else
                    Writeln('Pas file not found');

          until FindNext(sr) <> 0;
     end;

     FindClose(sr);

     Writeln('Done');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
