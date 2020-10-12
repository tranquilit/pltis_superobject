unit soutils;
{ -----------------------------------------------------------------------
#    This file is part of WAPT
#    Copyright (C) 2013  Tranquil IT Systems http://www.tranquil.it
#    WAPT aims to help Windows systems administrators to deploy
#    setup and update applications on users PC.
#
#    Part of this file is based on JEDI JCL library
#
#    WAPT is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    WAPT is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with WAPT.  If not, see <http://www.gnu.org/licenses/>.
#
# -----------------------------------------------------------------------
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,SuperObject;

function StringList2SOArray(St:TStringList):ISuperObject;
function StringArray2SOArray(A:TStringArray):ISuperObject;
function DynArr2SOArray(const items: Array of String):ISuperObject;

function SOArray2StringArray(items: ISuperObject):TStringArray;

function MergeSOArrays(A,B:ISuperObject):ISuperObject;

function SplitLines(const St:String):ISuperObject;
function Split(const St: String; Sep: String): ISuperObject;
function Join(const Sep: String; Arr:ISuperObject):String;
function StrIn(const St: String; List:ISuperObject): Boolean;
function StrToken(var S: string; Separator: String): string;
function ExtractField(SOList:ISuperObject;const fieldname:String;NilIfNil:Boolean=True):ISuperObject;
function ExtractFields(SOList:ISuperObject;const keys: Array of String):ISuperObject;

// expand an array of array to an array of dict. All items must have same count of cell
// fieldnames of cell is in ColumnNames
function SOArrayArrayExpand(SOArray:ISuperobject;ColumnNames: TStringArray): ISuperObject;
function SOArrayArrayExpand(SOArray: ISuperobject; ColumnName: String ): ISuperObject;

function csv2SO(csv:String;Sep:Char=#0):ISuperObject;

// Compare 2 values given their PropertyName
type TSOCompareByPropertyName=function (const PropertyName:String;const d1,d2:ISuperObject):TSuperCompareResult;

type TSOCompare=function (SOArray:ISuperObject;idx1,idx2:integer):integer;

// Default function to QuickSort an arrays of SuperObject.
function DefaultSOCompareFunc(SOArray:ISuperObject;idx1,idx2:integer):integer;

procedure Sort(SOArray: ISuperObject;CompareFunc: TSOCompare);

//
procedure SOArrayExtend(const TargetArray,NewElements:ISuperObject);

// Sort the Array SOArray using the composite key described by keys array
procedure SortByFields(SOArray: ISuperObject;Fields:array of string;reversed:Boolean=False);

// return an object with only keys attributes. If keys is empty, return SO itself.
function SOExtractFields(SO:ISuperObject; const keys: Array of String):ISuperObject;

// return an object without the  ExcludedKeys attributes. If ExcludedKeys is empty, return SO itself.
function SOFilterFields(SO:ISuperObject; const ExcludedKeys: Array of String):ISuperObject;

// extract one string property as an array of string
function SOExtractStringField(SOList:ISuperObject;const fieldname:String):TStringArray;

//Compare 2 SO objects given a list of keys
function SOCompareByKeys(SO1, SO2: ISuperObject; const keys: array of String;const CompareFunc:TSOCompareByPropertyName=Nil): TSuperCompareResult;

// Return the first occurence of AnObject in the List of objects, using the composite key described by keys array
function SOArrayFindFirst(AnObject, List: ISuperObject; const keys: array of String): ISuperobject;

function SOArrayIndexOf(AnObject, List: ISuperObject): Integer;

function CompareInt(i1,i2: LongInt):Integer;

function GetIntCompResult(const i: int64): TSuperCompareResult;

implementation

uses StrUtils;

operator in(const a:string;b:Array Of String):Boolean;inline;
var i:integer;
begin
  Result := False;
  for i :=Low(b) to High(b) do
    if a = b[i] then
    begin
      Result := True;
      Break;
    end;
end;

function CompareInt(i1,i2: LongInt):Integer;
begin
  if i1<i2 then Result := -1 else
  if i1>i2 then Result := 1 else
  Result := 0;
end;

function GetIntCompResult(const i: int64): TSuperCompareResult;
begin
  if i < 0 then result := cpLess else
  if i = 0 then result := cpEqu else
    Result := cpGreat;
end;


function SOArray2StringArray(items: ISuperObject): TStringArray;
var
  s:ISuperObject;
  i:integer;
begin
  if (items<>Nil) and (items.AsArray<>Nil) then
  begin
    SetLength(result,items.AsArray.Length);
    i:= 0;
    for s in items do
    begin
      result[i] := Utf8Encode(s.AsString);
      inc(i);
    end;
  end
  else
    SetLength(result,0);
end;

function StrToken(var S: string; Separator: String): string;
var
  I: SizeInt;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I+Length(Separator)-1);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function DynArr2SOArray(const items: array of String): ISuperObject;
var
  i:integer;
begin
  Result := TSuperObject.Create(stArray);
  for i:=low(items) to High(items) do
    Result.AsArray.Add(items[i]);

end;


function StringList2SOArray(St: TStringList): ISuperObject;
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
    Result.AsArray.Add(UTF8Decode(tok));
  end;
end;

function Split(const St: String; Sep: String): ISuperObject;
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

function ExtractField(SOList:ISuperObject;const fieldname:String;NilIfNil:Boolean=True):ISuperObject;
var
  item:ISuperObject;
begin
  if (SOList<>Nil) and (SOList.AsArray<>Nil) then
  begin
    Result := TSuperObject.Create(stArray);
    for item in SOList do
    begin
      if Item.DataType = stObject then
        Result.AsArray.Add(item[fieldname])
      else if Item.DataType = stArray then
        Result.AsArray.Add(item.AsArray[StrToInt(fieldname)]);
    end;
  end
  else
    if NilIfNil then
      Result := Nil
    else
      Result := SA([]);
end;

function SOFilterFields(SO: ISuperObject; const ExcludedKeys: array of String
  ): ISuperObject;
var
  key: ISuperObject;
begin
  if length(ExcludedKeys) = 0 then
    Result := SO
  else
  begin
    result := TSuperObject.Create(stObject);
    for key in SO.AsObject.GetNames do
      if not (key.AsString in ExcludedKeys) then
        Result[key.AsString] := SO[key.AsString];
  end;
end;

function SOExtractStringField(SOList:ISuperObject;const fieldname:String):TStringArray;
var
  item:ISuperObject;
  i: integer;
begin
  if (SOList<>Nil) and (SOList.AsArray<>Nil) then
  begin
    SetLength(Result,SOList.AsArray.Length);
    i := 0;
    for item in SOList do
    begin
      if Item.DataType = stObject then
        Result[i] := item.S[fieldname]
      else if Item.DataType = stArray then
        Result[i] := item.AsArray.S[StrToInt(fieldname)];
      inc(i);
    end;
  end
  else
    Result := Nil;
end;

function ExtractFields(SOList: ISuperObject; const keys: array of String
  ): ISuperObject;
var
  item:ISuperObject;
begin
  if(SOList<>Nil) then
  begin
    if (SOList.DataType <> stArray) then
      Raise Exception.Create('Need a TSuperObject Array');

    Result := TSuperObject.Create(stArray);
    for item in SOList do
      Result.AsArray.Add(SOExtractFields(item,keys))
  end
  else
    Result := Nil;
end;


function Join(const Sep: String; Arr: ISuperObject): String;
var
  item:ISuperObject;
begin
  result := '';
  if Arr<>Nil then
  begin
    if Arr.DataType=stArray then
      for item in Arr do
      begin
        if Result<>'' then
          Result:=Result+Sep;
        Result:=Result+UTF8Encode(item.AsString);
      end
    else
      Result := UTF8Encode(Arr.AsString);
  end;
end;

// return True if St is in the List list of string
function StrIn(const St: String; List: ISuperObject): Boolean;
var
  it:ISuperObject;
begin
  if List <>Nil then
    for it in List do
    begin
      if (it.DataType=stString) and (it.AsString=St) then
      begin
        result := True;
        Exit;
      end;
    end;
  result := False;
end;

function SOArrayArrayExpand(SOArray: ISuperobject; ColumnNames: TStringArray
  ): ISuperObject;

var
  item,cell,row: ISUperObject;
  i: integer;
begin
  Result := TSuperObject.Create(stArray);
  for item in SOArray do
  begin
    row := SO();
    for i:=0 to length(ColumnNames)-1 do
      row[ColumnNames[i]] := item.AsArray[i];
    Result.AsArray.Add(row);
  end;
end;

function SOArrayArrayExpand(SOArray: ISuperobject; ColumnName: String
  ): ISuperObject;
var
  item: ISUperObject;
begin
  Result := TSuperObject.Create(stArray);
  if Assigned(SOArray) then
    for item in SOArray do
      // single item
      Result.AsArray.Add(SO([ColumnName, item]));
end;

function csv2SO(csv: String;Sep:Char=#0): ISuperObject;
var
  r,col,maxcol:integer;
  row : String;
  Lines,header,values,newrec:ISuperObject;
begin
  lines := SplitLines(csv);
  row := UTF8Encode(lines.AsArray.S[0]);
  if Sep=#0 then
  begin
    if pos(#9,row)>0 then
      Sep := #9
    else
    if pos(';',row)>0 then
      Sep := ';'
    else
    if pos(',',row)>0 then
      Sep := ',';
  end;

  header := Split(row,Sep);
  result := TSuperObject.Create(stArray);
  if Lines.AsArray.Length>1 then
    for r:=1 to lines.AsArray.Length-1 do
    begin
      row := Utf8Encode(lines.AsArray.S[r]);
      values :=Split(row,sep);
      Newrec := TSuperObject.Create;
      result.AsArray.Add(newrec);
      maxcol := values.AsArray.Length;
      if maxcol > header.AsArray.Length then
        maxcol := header.AsArray.Length;

      for col := 0 to maxcol-1 do
        newrec.S[header.AsArray.S[col]] := values.AsArray.S[col];
    end
  else
  begin
    Newrec := TSuperObject.Create;
    result.AsArray.Add(newrec);
    for col := 0 to header.AsArray.Length-1 do
      newrec.S[header.AsArray.S[col]] := '';
  end;
end;

function DefaultSOCompareFunc(SOArray:ISuperObject;idx1,idx2:integer):integer;
var
  compresult : TSuperCompareResult;
  SO1,SO2:ISuperObject;

begin
  SO1 := SOArray.AsArray[idx1];
  SO2 := SOArray.AsArray[idx2];
  compresult := SO1.Compare(SO2);
  case compresult of
    cpLess : Result := -1;
    cpEqu  : Result := 0;
    cpGreat : Result := 1;
    cpError :  Result := CompareStr(Utf8Encode(SO1.AsString),Utf8Encode(SO2.AsString));
  end;
end;

procedure Sort(SOArray: ISuperObject;CompareFunc: TSOCompare);
  procedure QuickSort(L, R: integer;CompareFunc: TSOCompare);
  var
    I, J, P: Integer;
    item1,item2:ISuperObject;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while CompareFunc(SOArray, I, P) < 0 do Inc(I);
        while CompareFunc(SOArray,J, P) > 0 do Dec(J);
        if I <= J then
        begin
          //exchange items
          item1 := SOArray.AsArray[I];
          item2 := SOArray.AsArray[J];
          SOArray.AsArray[I] := item2;
          SOArray.AsArray[J] := item1;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J, CompareFunc);
      L := I;
    until I >= R;
  end;

begin
  If CompareFunc=Nil then
     CompareFunc :=  @DefaultSOCompareFunc;
  if (SOArray.AsArray<>Nil) and (SOArray.AsArray.Length>1) then
    QuickSort(0,SOArray.AsArray.Length-1,CompareFunc);
end;

procedure SOArrayExtend(const TargetArray, NewElements: ISuperObject);
var
  Elem: ISuperObject;
begin
  For Elem in NewElements do
    TargetArray.AsArray.Add(Elem);
end;

procedure SortByFields(SOArray: ISuperObject;Fields:array of string;reversed:Boolean=False);
  function SOCompareFields(SOArray:ISuperObject;idx1,idx2:integer):integer;
  var
    compresult : TSuperCompareResult;
    SO1,SO2,F1,F2:ISuperObject;
    i:integer;
  begin
    SO1 := SOArray.AsArray[idx1];
    SO2 := SOArray.AsArray[idx2];
    for i:=low(Fields) to high(fields) do
    begin
      F1 := SO1[Fields[i]];
      F2 := SO2[Fields[i]];
      compresult := SO1.Compare(SO2);
      case compresult of
        cpLess : Result := -1;
        cpEqu  : Result := 0;
        cpGreat : Result := 1;
        cpError :  Result := CompareStr(Utf8Encode(F1.AsString),Utf8Encode(F2.AsString));
      end;
      if Reversed then Result := -Result;
      if Result<>0 then
        Break;
    end;
  end;

  procedure QuickSort(L, R: integer);
  var
    I, J, P: Integer;
    item1,item2:ISuperObject;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while SOCompareFields(SOArray, I, P) < 0 do Inc(I);
        while SOCompareFields(SOArray,J, P) > 0 do Dec(J);
        if I <= J then
        begin
          //exchange items
          item1 := SOArray.AsArray[I];
          item2 := SOArray.AsArray[J];
          SOArray.AsArray[I] := item2;
          SOArray.AsArray[J] := item1;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if (SOArray.AsArray<>Nil) and (SOArray.AsArray.Length>1) then
    QuickSort(0,SOArray.AsArray.Length-1);
end;

function SOExtractFields(SO: ISuperObject; const keys: array of String): ISuperObject;
var
  key: String;
begin
  if length(keys) = 0 then
    Result := SO
  else
  begin
    result := TSuperObject.Create(stObject);
    for key in keys do
      Result[key] := SO[key];
  end;
end;

function StringArrayIntersect(const a1,a2:TStringArray):TStringArray;
var
  i,j:integer;
begin
  SetLength(Result,0);
  for i:=0 to Length(a1)-1 do
  begin
    for j := 0 to Length(a2)-1 do
    begin
      if a1[i] = a2[j] then
      begin
        SetLength(result,Length(Result)+1);
        result[Length(result)-1] := a1[i];
      end;
    end;
  end;
end;

// Compare 2 objects SO1 and SO2 by comparing each key value in turn. If CompareFunc is supplied, comparison function can be based on the key name
function SOCompareByKeys(SO1, SO2: ISuperObject; const keys: array of String;const CompareFunc:TSOCompareByPropertyName=Nil): TSuperCompareResult;
var
  i: integer;
  key: String;
  reckeys: Array of String;
begin
  // Nil is Less than something..
  if (SO1=Nil) and (SO2<>Nil) then
  begin
    Result := cpLess;
    exit;
  end
  else
  if (SO1<>Nil) and (SO2=Nil) then
  begin
    Result := cpGreat;
    exit;
  end
  else
  // can not compare nothing
  if (SO1=Nil) and (SO2=Nil) then
  begin
    Result := cpError;
    exit;
  end
  else
  // can not compare objects which have no keys...
  if (SO1.AsObject=Nil) or (SO2.AsObject=Nil) then
  begin
    Result := cpError;
    exit;
  end;

  // If no key property names, take all common attributes names to make comparison.
  if length(keys) = 0 then
    reckeys := StringArrayIntersect(SOArray2StringArray(SO1.AsObject.GetNames()),SOArray2StringArray(SO2.AsObject.GetNames()))
  else
  begin
    // copy list of keys passed as parameter
    SetLength(reckeys,length(keys));
    for i := 0 to length(keys)-1 do
      reckeys[i] := keys[i];
  end;

  // If no key -> error
  if (length(reckeys) = 0) then
    result := cpError
  else
    for key in reckeys do
    begin
      if (key<>'') then
      begin
        if (SO1[key] = Nil) and (SO2[key] = Nil) or (ObjectIsNull(SO1[key]) and ObjectIsNull(SO2[key])) then
          // both objects have no value with this key.
          Result := cpEqu
        else if ((SO1[key] = Nil) or ObjectIsNull(SO1[key])) and ((SO2[key] <> Nil) and not ObjectIsNull(SO1[key])) then
          // Nil first
          Result :=  cpLess
        else if ((SO1[key] <> Nil) and not ObjectIsNull(SO1[key])) and ((SO2[key] = Nil) or ObjectIsNull(SO2[key])) then
          // Nil first
          Result :=  cpGreat
        else
          // TODO: problem when comparison returns cpError
          if not Assigned(CompareFunc) then
          begin
            if SO1[key] <> Nil then
              Result := SO1[key].Compare(SO2[key])
            else
              Result := cpEqu;
          end
          else
            Result := CompareFunc(key,SO1[key],SO2[key]);
      end
      else
        Result := cpEqu;

      if (Result <> cpEqu) then
        break;
    end;
end;

function SOArrayFindFirst(AnObject, List: ISuperObject; const keys: array of String
  ): ISuperobject;
var
  item: ISuperObject;
  key:String;
begin
  Result := Nil;
  for item in List do
  begin
    if length(keys) = 0 then
    begin
      if (item = AnObject) or (item.Compare(AnObject) = cpEqu) then
      begin
        Result := item;
        exit;
      end;
    end
    else
    begin
      for key in keys do
      begin
        if (item[key]<>Nil) and (item[key].Compare(AnObject[key]) <> cpEqu) then
          break;
      end;
      if (item[key]<>Nil) and (item[key].Compare(AnObject[key]) = cpEqu) then
      begin
        Result := item;
        exit;
      end;
    end;
  end;
end;

function SOArrayIndexOf(AnObject, List: ISuperObject): Integer;
var
  item: ISuperObject;
begin
  for result := 0 to List.AsArray.Length-1 do
    if (List.AsArray[Result] = AnObject) or (List.AsArray[Result].Compare(AnObject) = cpEqu) then
      exit;
  Result := -1;
end;

function StringArray2SOArray(A:TStringArray):ISuperObject;
var
  s:String;
begin
  Result := TSuperObject.Create(stArray);
  for s in A do
    Result.AsArray.Add(s);
end;

function MergeSOArrays(A,B:ISuperObject):ISuperObject;
var
  item,itemA:ISuperObject;
  DoAppend: Boolean;
begin
  result := A.Clone;
  for item in B do
  begin
    DoAppend := True;
    for itemA in result do
      if itemA.Compare(item) = cpEqu then
      begin
        DoAppend := False;
        break;
      end;
    if DoAppend then
      result.AsArray.Add(item);
  end;
end;

end.

