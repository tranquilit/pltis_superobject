unit soutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,SuperObject,DB;

function StringList2SuperObject(St:TStringList):ISuperObject;
function SplitLines(const St:String):ISuperObject;
function Split(const St: String; Sep: Char): ISuperObject;
function StrIn(const St: String; List:ISuperObject): Boolean;

function CharIsWhiteSpace(const C: Char): Boolean;
function CharIsWildcard(const C: Char): Boolean;
function StrCompare(const S1, S2: string; CaseSensitive: Boolean): SizeInt;

function StrToken(var S: string; Separator: Char): string;
procedure StrTokens(const S: string; const List: TStrings);

function StrWord(const S: string; var Index: SizeInt; out Word: string): Boolean; overload;
function StrWord(var S: PChar; out Word: string): Boolean; overload;

function StrIndex(const S: string; const List: array of string; CaseSensitive: Boolean = False): SizeInt;
function StrIsOneOf(const S: string; const List: array of string): Boolean;

function Dataset2SO(DS:TDataset;AllRecords:Boolean=True):ISuperObject;
procedure SO2Dataset(SO:ISuperObject;DS:TDataset;ExcludedFields:Array of String);

const
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);
  NativeCrLf           = string(#13#10);
  // default line break for a version of Delphi on a platform
  {$IFDEF MSWINDOWS}
  NativeLineBreak      = NativeCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  NativeLineBreak      = NativeLineFeed;
  {$ENDIF UNIX}
  // Misc. often used character definitions
  NativeNull = Char(#0);
  NativeSoh = Char(#1);
  NativeStx = Char(#2);
  NativeEtx = Char(#3);
  NativeEot = Char(#4);
  NativeEnq = Char(#5);
  NativeAck = Char(#6);
  NativeBell = Char(#7);
  NativeBackspace = Char(#8);
  NativeTab = Char(#9);
  NativeVerticalTab = Char(#11);
  NativeFormFeed = Char(#12);
  NativeSo = Char(#14);
  NativeSi = Char(#15);
  NativeDle = Char(#16);
  NativeDc1 = Char(#17);
  NativeDc2 = Char(#18);
  NativeDc3 = Char(#19);
  NativeDc4 = Char(#20);
  NativeNak = Char(#21);
  NativeSyn = Char(#22);
  NativeEtb = Char(#23);
  NativeCan = Char(#24);
  NativeEm = Char(#25);
  NativeEndOfFile = Char(#26);
  NativeEscape = Char(#27);
  NativeFs = Char(#28);
  NativeGs = Char(#29);
  NativeRs = Char(#30);
  NativeUs = Char(#31);
  NativeSpace = Char(' ');
  NativeComma = Char(',');
  NativeBackslash = Char('\');
  NativeForwardSlash = Char('/');

  NativeDoubleQuote = Char('"');
  NativeSingleQuote = Char('''');



implementation
uses StrUtils,character;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  case C of
    NativeTab,
    NativeLineFeed,
    NativeVerticalTab,
    NativeFormFeed,
    NativeCarriageReturn,
    NativeSpace:
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsWildcard(const C: Char): Boolean;
begin
  case C of
    '*', '?':
      Result := True;
  else
    Result := False;
  end;
end;

function StrCompareRangeEx(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
var
  Len1, Len2: SizeInt;
  I: SizeInt;
  C1, C2: Char;
begin
  if Pointer(S1) = Pointer(S2) then
  begin
    if (Count <= 0) and (S1 <> '') then
      Result := -2 // no work
    else
      Result := 0;
  end
  else
  if (S1 = '') or (S2 = '') then
    Result := -1 // null string
  else
  if Count <= 0 then
    Result := -2 // no work
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);

    if (Index - 1) + Count > Len1 then
      Result := -2
    else
    begin
      if (Index - 1) + Count > Len2 then // strange behaviour, but the assembler code does it
        Count := Len2 - (Index - 1);

      if CaseSensitive then
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            Result := Ord(C1) - Ord(C2);
            Exit;
          end;
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          C1 := S1[Index + I];
          C2 := S2[Index + I];
          if C1 <> C2 then
          begin
            C1 := TCharacter.ToLower(C1);
            C2 := TCharacter.ToLower(C2);
            if C1 <> C2 then
            begin
              Result := Ord(C1) - Ord(C2);
              Exit;
            end;
          end;
        end;
      end;
      Result := 0;
    end;
  end;
end;

function StrCompare(const S1, S2: string; CaseSensitive: Boolean): SizeInt;
var
  Len1, Len2: SizeInt;
begin
  if Pointer(S1) = Pointer(S2) then
    Result := 0
  else
  begin
    Len1 := Length(S1);
    Len2 := Length(S2);
    Result := Len1 - Len2;
    if Result = 0 then
      Result := StrCompareRangeEx(S1, S2, 1, Len1, CaseSensitive);
  end;
end;

function StrCompareRange(const S1, S2: string; Index, Count: SizeInt; CaseSensitive: Boolean): SizeInt;
begin
  Result := StrCompareRangeEx(S1, S2, Index, Count, CaseSensitive);
end;


function StrIndex(const S: string; const List: array of string; CaseSensitive: Boolean): SizeInt;
var
  I: SizeInt;
begin
  Result := -1;
  for I := Low(List) to High(List) do
  begin
    if StrCompare(S, List[I], CaseSensitive) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrWord(const S: string; var Index: SizeInt; out Word: string): Boolean;
var
  Start: SizeInt;
  C: Char;
begin
  Word := '';
  if (S = '') then
  begin
    Result := True;
    Exit;
  end;
  Start := Index;
  Result := False;
  while True do
  begin
    C := S[Index];
    case C of
      #0:
        begin
          if Start <> 0 then
            Word := Copy(S, Start, Index - Start);
          Result := True;
          Exit;
        end;
      ' ', NativeLineFeed, NativeCarriageReturn:
        begin
          if Start <> 0 then
          begin
            Word := Copy(S, Start, Index - Start);
            Exit;
          end
          else
          begin
            while CharIsWhiteSpace(C) do
            begin
              Inc(Index);
              C := S[Index];
            end;
          end;
        end;
    else
      if Start = 0 then
        Start := Index;
      Inc(Index);
    end;
  end;
end;

function StrWord(var S: PChar; out Word: string): Boolean;
var
  Start: PChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    case S^ of
      #0:
      begin
        if Start <> nil then
          SetString(Word, Start, S - Start);
        Result := True;
        Exit;
      end;
      NativeSpace, NativeLineFeed, NativeCarriageReturn:
      begin
        if Start <> nil then
        begin
          SetString(Word, Start, S - Start);
          Exit;
        end
        else
          while CharIsWhiteSpace(S^) do
            Inc(S);
      end;
    else
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;


function StrToken(var S: string; Separator: Char): string;
var
  I: SizeInt;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokens(const S: string; const List: TStrings);
var
  Start: PChar;
  Token: string;
  Done:  Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := Pointer(S);
    repeat
      Done := StrWord(Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

function StrIsOneOf(const S: string; const List: array of string): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;


function StringList2SuperObject(St: TStringList): ISuperObject;
var
  i:integer;
begin
  Result := TSuperObject.Create(stArray);
  for i:=0 to st.Count-1 do
    Result.AsArray.Add(st[i]);
end;


function SplitLines(const St: String): ISuperObject;
var
  tok : String;
  St2:String;
begin
  Result := TSuperObject.Create(stArray);
  St2 := StrUtils.StringsReplace(St,[#13#10,#13,#10],[#13,#13,#13],[rfReplaceAll]);
  while St2<>'' do
  begin
    tok := StrToken(St2,#13);
    Result.AsArray.Add(tok);
  end;
end;

function Split(const St: String; Sep: Char): ISuperObject;
var
  tok : String;
  St2:String;
begin
  Result := TSuperObject.Create(stArray);
  St2 := St;
  while St2<>'' do
  begin
    tok := StrToken(St2,Sep);
    Result.AsArray.Add(tok);
  end;
end;

// return True if St is in the List list of string
function StrIn(const St: String; List: ISuperObject): Boolean;
var
  it:ISuperObject;
begin
  for it in List do
    if (it.DataType=stString) and (it.AsString=St) then
    begin
      result := True;
      Exit;
    end;
  result := False;
end;

function Dataset2SO(DS: TDataset;AllRecords:Boolean=True): ISuperObject;
var
  rec: ISuperObject;

  procedure Fillrec(rec:ISuperObject);
  var
    i:integer;
  begin
    for i:=0 to DS.Fields.Count-1 do
    begin
      if DS.Fields[i].IsNull then
        rec.N[DS.Fields[i].fieldname] := Nil
      else
      case DS.Fields[i].DataType of
        ftString : rec.S[DS.Fields[i].fieldname] := UTF8Decode(DS.Fields[i].AsString);
        ftInteger : rec.I[DS.Fields[i].fieldname] := DS.Fields[i].AsInteger;
        ftFloat : rec.D[DS.Fields[i].fieldname] := DS.Fields[i].AsFloat;
        ftBoolean : rec.B[DS.Fields[i].fieldname] := DS.Fields[i].AsBoolean;
        ftDateTime : rec.S[DS.Fields[i].fieldname] :=  DelphiDateTimeToISO8601Date(DS.Fields[i].AsDateTime);
      else
        rec.S[DS.Fields[i].fieldname] := UTF8Decode(DS.Fields[i].AsString);
      end;
    end;
  end;

begin
  if AllRecords then
  begin
    DS.First;
    Result := TSuperObject.Create(stArray);
    While not DS.EOF do
    begin
      rec := TSuperObject.Create(stObject);
      Result.AsArray.Add(rec);
      Fillrec(Rec);
      DS.Next;
    end;
  end
  else
  begin
    Result := TSuperObject.Create;
    Fillrec(Result);
  end;
end;

procedure SO2Dataset(SO: ISuperObject; DS: TDataset;ExcludedFields:Array of String);
var
  arec : ISuperObject;
  procedure Fillrec(rec:ISuperObject);
  var
    i:integer;
    dt : TDateTime;
  begin
    for i:=0 to DS.Fields.Count-1 do
    begin
      if StrIsOneOf(DS.Fields[i].fieldname,ExcludedFields) then
        Continue;
      if rec.AsObject.Exists(DS.Fields[i].fieldname) then
      begin
        if ObjectIsNull(rec.N[DS.Fields[i].fieldname]) then
          DS.Fields[i].Clear
        else
        case DS.Fields[i].DataType of
          ftString : DS.Fields[i].AsString := UTF8Encode(rec.S[DS.Fields[i].fieldname]);
          ftInteger : DS.Fields[i].AsInteger := rec.I[DS.Fields[i].fieldname];
          ftFloat : DS.Fields[i].AsFloat := rec.D[DS.Fields[i].fieldname];
          ftBoolean : DS.Fields[i].AsBoolean := rec.B[DS.Fields[i].fieldname];

          ftDateTime : if ISO8601DateToDelphiDateTime(rec.S[DS.Fields[i].fieldname],dt) then
            DS.Fields[i].AsDateTime := dt;
        else
          DS.Fields[i].AsString := UTF8Encode(rec.S[DS.Fields[i].fieldname]);
        end
      end
    end;
  end;

begin
  // If SO is an array, we fill the dataset with all records
  if SO.DataType = stArray then
  begin
    for arec in SO do
    begin
      DS.Append;
      Fillrec(ARec);
      DS.Post;
    end;
  end
  else
  begin
    // If SO is a single object, we fill the dataset with one record
    if not (DS.State in dsEditModes) then
      DS.Append;
    Fillrec(SO);
    DS.Post;
  end;
end;



end.

