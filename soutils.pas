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
// aggregate Rows by identical KeyName value.
// If AggFieldName is not empty, returns a list of single extracted value from rows
// else return a list of rows objects
function SOAggregate(Rows: ISuperObject; KeyName: String; AggFieldName: String=''): ISuperObject;

function MergeSOArrays(A,B:ISuperObject):ISuperObject;

function SplitLines(const St:String):ISuperObject;
function Split(const St: String; Sep: String): ISuperObject;
function Join(const Sep: String; Arr:ISuperObject):String;
function StrIn(const St: String; List:ISuperObject): Boolean;
function StrToken(var S: string; Separator: String): string;
// create a new array with a field
function SOReduce(SOList:ISuperObject;const fieldname:String; NilIfNil:Boolean=True):ISuperObject;
// create a new array with a subset of the fields
function SOReduce(SOList:ISuperObject; const keys: Array of String):ISuperObject;

function SOObjectMatch(SOObject: ISuperObject;  keys: array of String; Values: array of const): Boolean;
function SOEquals(SOObject1, SOObject2: ISuperObject): Boolean;
function SOArrayDeleteMatching(SOList:ISuperObject; keys: Array of String; Values: Array of const):Boolean;

// obsolete
function ExtractField(SOList:ISuperObject;const fieldname:String;NilIfNil:Boolean=True):ISuperObject; deprecated 'Use SOReduce';
function ExtractFields(SOList:ISuperObject;const keys: Array of String):ISuperObject; deprecated 'Use SOReduce';



function RemoveDuplicates(SOArray: ISuperObject): ISuperObject;

// expand an array of array to an array of dict. All items must have same count of cell
// fieldnames of cell is in ColumnNames
function SOArrayArrayExpand(SOArray:ISuperobject;ColumnNames: TStringArray): ISuperObject;
function SOArrayArrayExpand(SOArray: ISuperobject; ColumnName: String ): ISuperObject;

function csv2SO(csv:String;Sep:Char=#0):ISuperObject;

// Compare 2 values given their PropertyName
type TSOCompareByPropertyName=function (const PropertyName:String;const d1,d2:ISuperObject):TSuperCompareResult;

type TSOCompare=function (const SO1,SO2:ISuperObject):integer;

// Default function to QuickSort an arrays of SuperObject.
function DefaultSOCompareFunc(const SO1,SO2:ISuperObject):integer;

procedure Sort(SOArray: ISuperObject;CompareFunc: TSOCompare;Reversed:Boolean=False);

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
function SOCompareByKeys(SO1, SO2: ISuperObject; const keys: array of String;const CompareFunc:TSOCompareByPropertyName=Nil;DirectProperties:Boolean=False): TSuperCompareResult;

// Return the first occurence of AnObject in the List of objects, using the composite key described by keys array
function SOArrayFindFirst(AnObject, List: ISuperObject; const keys: array of String): ISuperobject;

function SOArrayIndexOf(AnObject, List: ISuperObject): Integer;
function SOArrayIndexOf(const S:String; List: ISuperObject): Integer;

// string splitted by comma.
function SOEnsureList(StringOrArray: ISuperObject): ISuperObject;
function SOEnsureStringArray(StringOrArray: ISuperObject): TStringArray;

function CompareInt(i1,i2: Int64):Integer;

function GetIntCompResult(const i: int64): TSuperCompareResult;

function SOArrayIntersect(const a1,a2:ISuperObject):ISuperObject;

function StringArrayIntersect(const a1,a2:TStringArray):TStringArray;


implementation

uses
  StrUtils,
  supertypes,
  Variants;

operator in(const a:string;const b:Array Of String):Boolean;inline;
var i:integer;
begin
  //result := pos(a,b)>=0;
  Result := False;
  for i :=Low(b) to High(b) do
    if a = b[i] then
    begin
      Result := True;
      Break;
    end;
end;

function CompareInt(i1,i2: Int64):Integer;
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

function SOReduce(SOList:ISuperObject;const fieldname:String;NilIfNil:Boolean=True):ISuperObject;
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

function SOReduce(SOList: ISuperObject; const keys: array of String
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

function SOObjectMatch(SOObject: ISuperObject;  keys: array of String; Values: array of const): Boolean;
var
  i: Integer;
begin
  if not SOObject.IsType(stObject) then
    Exit(False);

  for i := low(Keys) to high(keys) do
  begin
    if not SOObject.AsObject.Exists(Keys[i]) then
      Exit(False);
    case TVarRec(Values[i]).VType of
      vtInteger : if SOObject.I[Keys[i]] <> TVarRec(Values[i]).VInteger then Exit(False);
      vtInt64   : if SOObject.I[Keys[i]] <> TVarRec(Values[i]).VInt64^ then Exit(False);
      vtBoolean : if SOObject.B[Keys[i]] <> TVarRec(Values[i]).VBoolean then Exit(False);
      vtChar    : if SOObject.S[Keys[i]] <> String(TVarRec(Values[i]).VChar) then Exit(False);
      vtWideChar: if SOObject.S[Keys[i]] <> WideString(TVarRec(Values[i]).VWideChar) then Exit(False);
      vtExtended: if SOObject.D [Keys[i]] <> TVarRec(Values[i]).VExtended^ then Exit(False);
      vtCurrency: if SOObject.C[Keys[i]] <> TVarRec(Values[i]).VCurrency^ then Exit(False);
      vtString  : if SOObject.S[Keys[i]] <> Utf8Decode(TVarRec(Values[i]).VString^) then Exit(False);
      vtAnsiString: if SOObject.S[Keys[i]] <> UTF8Decode(PChar(TVarRec(Values[i]).VAnsiString^)) then Exit(False);
      vtWideString: if SOObject.S[Keys[i]] <> PWideString(TVarRec(Values[i]).VWideString)^ then Exit(False);
      vtInterface: if SOEquals(SOObject[Keys[i]], ISuperObject(TVarRec(Values[i]).VInterface)) then Exit(False);
      vtVariant: if SOEquals(SOObject[Keys[i]], SO(TVarRec(Values[i]).VVariant^)) then Exit(False);
      vtUnicodeString: if SOObject.S[Keys[i]] <> SOString(TVarRec(Values[i]).VUnicodeString) then Exit(False);
    else
      Exit(False);
    end;
  end;

  Exit(True);
end;

function SOEquals(SOObject1, SOObject2: ISuperObject): Boolean;
var
  item: TSuperAvlEntry;
  i: Integer;
begin
  if SOObject1=SOObject2 then
    Exit(True);

  if (Assigned(SOObject1) and not Assigned(SOObject2)) or (not Assigned(SOObject1) and Assigned(SOObject2)) then
    Exit(False);

  if SOObject1.DataType <> SOObject2.DataType then
    Exit(False);

  case SOObject1.DataType of
    stBoolean:
      if SOObject1.AsBoolean <> SOObject2.AsBoolean then
        Exit(False);

    stDouble:
      if SOObject1.AsDouble <> SOObject2.AsDouble then
        Exit(False);

    stCurrency:
      if SOObject1.AsCurrency <> SOObject2.AsCurrency then
        Exit(False);

    stInt:
        if SOObject1.AsInteger <> SOObject2.AsInteger then
          Exit(False);
    stObject:
        begin
          if SOObject1.AsObject.count <> SOObject2.AsObject.count then
            Exit(False);

          for item in SOObject1.AsObject do
            if not SOEquals(item.Value, SOObject2.AsObject[item.Name]) then
              Exit(False);
        end;
    stArray:
        begin
          if SOObject1.AsArray.length <> SOObject2.AsArray.length then
            Exit(False);

          for i := 0 to SOObject1.AsArray.Length-1 do
            if not SOEquals(SOObject1.AsArray[i], SOObject2.AsArray[i]) then
              Exit(False);
        end;
    else
      if SOObject1.AsString<> SOObject2.AsString then
        Exit(False);
    end;
  exit(True);
end;

function SOArrayDeleteMatching(SOList: ISuperObject; keys: array of String; Values: array of const): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := SOList.AsArray.Length-1 downto 0 do
  begin
    if SOObjectMatch(SOList.AsArray[i],keys,values) then
    begin
      SOList.AsArray.Delete(i);
      Result := True;
    end;
  end;
end;

function ExtractField(SOList: ISuperObject; const fieldname: String; NilIfNil: Boolean): ISuperObject;
begin
  result := SOReduce(SOList,fieldname,NilIfNil);
end;

function ExtractFields(SOList: ISuperObject; const keys: array of String): ISuperObject;
begin
  result := SOReduce(SOList,keys);
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

function RemoveDuplicates(SOArray: ISuperObject): ISuperObject;
var
  Done: Array of UnicodeString;
  item: ISuperObject;
  s: UnicodeString;
begin
  Result := TSuperObject.Create(stArray);
  for item in SOArray do
  begin
    s := item.AsString;
    if not (s in Done) then
    begin
      SetLength(Done,Length(Done)+1);
      Done[Length(Done)-1] := s;
      Result.AsArray.Add(item);
    end;
  end;
end;

function SOArrayArrayExpand(SOArray: ISuperobject; ColumnNames: TStringArray
  ): ISuperObject;

var
  item,row: ISUperObject;
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

function DefaultSOCompareFunc(const SO1,SO2:ISuperObject):integer;
var
  compresult : TSuperCompareResult;

begin
  compresult := SO1.Compare(SO2);
  case compresult of
    cpLess : Result := -1;
    cpEqu  : Result := 0;
    cpGreat : Result := 1;
    cpError :  Result := CompareStr(Utf8Encode(SO1.AsString),Utf8Encode(SO2.AsString));
  end;
end;

procedure Sort(SOArray: ISuperObject;CompareFunc: TSOCompare;Reversed:Boolean=False);
var
  IReversed:Integer;

  procedure QuickSort(L, R: integer;CompareFunc: TSOCompare;IReversed:Integer=1);
  var
    I, J : Integer;
    pivot, temp:ISuperObject;
  begin
    I := L;
    J := R;
    pivot := SOArray.AsArray[(L + R) div 2];
    repeat
      while (IReversed*CompareFunc(SOArray.AsArray[I], pivot) < 0) do Inc(I);
      while (IReversed*CompareFunc(SOArray.AsArray[J], pivot) > 0) do Dec(J);
      if I <= J then
      begin
        //exchange items
        temp := SOArray.AsArray[I];
        SOArray.AsArray[I] := SOArray.AsArray[J];
        SOArray.AsArray[J] := temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if J > L then QuickSort(L, J, CompareFunc,IReversed);
    if I < R then QuickSort(I, R, CompareFunc,IReversed);
  end;

begin
  if Reversed then IReversed:=-1 else IReversed:=1;
  If CompareFunc=Nil then
     CompareFunc :=  @DefaultSOCompareFunc;
  if (SOArray.AsArray<>Nil) and (SOArray.AsArray.Length>1) then
    QuickSort(0,SOArray.AsArray.Length-1,CompareFunc,IReversed);
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

function SOArrayIntersect(const a1,a2:ISuperObject):ISuperObject;
var
  i,j:integer;
begin
  Result := TSuperObject.Create(stArray);
  if assigned(a1) and assigned(a2) then
    for i:=0 to a1.AsArray.Length -1 do
    begin
      for j := 0 to a2.AsArray.Length-1 do
      begin
        if SOEquals(a1.AsArray[i], a2.AsArray[j]) then
          Result.AsArray.Add(a1.AsArray[i]);
      end;
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
function SOCompareByKeys(SO1, SO2: ISuperObject; const keys: array of String;const CompareFunc:TSOCompareByPropertyName=Nil;DirectProperties:Boolean=False): TSuperCompareResult;
var
  i: integer;
  key: String;
  reckeys: Array of String;
  PropValue1,PropValue2: ISuperObject;
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
        if DirectProperties then
        begin
          PropValue1 := SO1.AsObject[key];
          PropValue2 := SO2.AsObject[key];
        end
        else
        begin
          PropValue1 := SO1[key];
          PropValue2 := SO2[key];
        end;

        if (PropValue1 = Nil) and (PropValue2 = Nil) or (ObjectIsNull(PropValue1) and ObjectIsNull(PropValue2)) then
          // both objects have no value with this key.
          Result := cpEqu
        else if ((PropValue1 = Nil) or ObjectIsNull(PropValue1)) and ((PropValue2 <> Nil) and not ObjectIsNull(PropValue2)) then
          // Nil first
          Result :=  cpLess
        else if ((PropValue1 <> Nil) and not ObjectIsNull(PropValue1)) and ((PropValue2 = Nil) or ObjectIsNull(PropValue2)) then
          // Nil first
          Result :=  cpGreat
        else
          // TODO: problem when comparison returns cpError
          if not Assigned(CompareFunc) then
          begin
            if PropValue1 <> Nil then
              Result := PropValue1.Compare(PropValue2)
            else
              Result := cpEqu;
          end
          else
            Result := CompareFunc(key,PropValue1,PropValue2);
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
  key:String;
  Item: ISuperObject;
  LastMatch: Boolean;
begin
  Result := Nil;
  if not Assigned(List) then
    Exit;
  if not Assigned(AnObject) then
    Exit;

  for Item in List do
  begin
    if length(keys) = 0 then
    begin
      if SOEquals(item,AnObject) then
      begin
        Result := Item;
        break;
      end;
    end
    else
    begin
      LastMatch := False;
      for key in keys do
      begin
        LastMatch := SOEquals(item[key], AnObject[key]);
        if not LastMatch then
          break;
      end;

      if LastMatch then
      begin
        Result := Item;
        break;
      end;
    end;

    if Result <> Nil then
      break;
  end;
end;

function SOArrayIndexOf(AnObject, List: ISuperObject): Integer;
var
  item: ISuperObject;
begin
  if Assigned(List) and (List.DataType=stArray) then
    for result := 0 to List.AsArray.Length-1 do
      if (List.AsArray[Result] = AnObject) or (List.AsArray[Result].Compare(AnObject) = cpEqu) then
        exit;
  Result := -1;
end;

function SOArrayIndexOf(const S: String; List: ISuperObject): Integer;
begin
  if Assigned(List) and (List.DataType=stArray) then
    for result := 0 to List.AsArray.Length-1 do
      if List.AsArray.S[Result] = S then
        exit;
  Result := -1;
end;

// string splitted by comma.
function SOEnsureList(StringOrArray: ISuperObject): ISuperObject;
begin
  if not Assigned(StringOrArray) then
    Result := TSuperObject.Create(stArray)
  else if StringOrArray.DataType=stArray then
    Result := StringOrArray
  else
    Result := SOUtils.Split(StringOrArray.AsString,',');
end;

function SOEnsureStringArray(StringOrArray: ISuperObject): TStringArray;
begin
  if not Assigned(StringOrArray) then
    Exit(Nil)

  else if StringOrArray.DataType=stArray then
    Exit(SOArray2StringArray(StringOrArray))
  else
    Exit(String(StringOrArray.AsString).Split(','));
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

function SOAggregate(Rows: ISuperObject; KeyName: String; AggFieldName: String=''): ISuperObject;
var
  Key: UnicodeString;
  Row,Data: ISuperObject;
begin
  Result := SO();
  for Row in Rows do
  begin
    Key := Row.AsObject.S[KeyName];
    if (AggFieldName<>'') then
      Data := Row.AsObject[AggFieldName]
    else
      Data := Row;
    if Result.AsObject.Exists(Key) then
      Result.AsObject[Key].AsArray.Add(Data)
    else
      Result.AsObject[Key] := SA([Data]);
  end;
end;



end.

