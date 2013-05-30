unit soutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,SuperObject,DB;

function StringList2SuperObject(St:TStringList):ISuperObject;
function SplitLines(const St:String):ISuperObject;
function Split(const St: String; Sep: Char): ISuperObject;
function Join(const Sep: String; Arr:ISuperObject):String;
function StrIn(const St: String; List:ISuperObject): Boolean;
function StrIsOneOf(const S: string; const List: array of string): Boolean;
function DynArr2SuperObject(const items: Array of String):ISuperObject;

function StrToken(var S: string; Separator: Char): string;

function Dataset2SO(DS:TDataset;AllRecords:Boolean=True):ISuperObject;
procedure SO2Dataset(SO:ISuperObject;DS:TDataset;ExcludedFields:Array of String);


implementation
uses StrUtils,character;

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

function StrIsOneOf(const S: string; const List: array of string): Boolean;
var
  i:integer;
begin
  Result := False;
  for i:=low(List) to High(List) do
    if List[i] = S then
    begin
      Result:=True;
      Exit;
    end;
end;

function DynArr2SuperObject(const items: Array of String):ISuperObject;
var
  i:integer;
begin
  Result := TSuperObject.Create(stArray);
  for i:=low(items) to High(items) do
    Result.AsArray.Add(items[i]);

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

function Join(const Sep: String; Arr: ISuperObject): String;
var
  item:ISuperObject;
begin
  result := '';
  for item in Arr do
  begin
    if Result<>'' then
      Result:=Result+Sep;
    Result:=Result+item.AsString;
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
    if not DS.Active then DS.Open;
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
    if not DS.Active then DS.Open;
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

