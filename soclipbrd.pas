unit soclipbrd;

{$mode delphi}{$H+}

interface
uses SuperObject,ClipBrd,lcltype;

function ClipboardSOData: ISuperObject;

var
  ClipbrdJson: TClipboardFormat;



implementation

uses
  Classes, SysUtils,soutils;

function ClipboardSOData: ISuperObject;
var
  NewData, row: ISuperObject;
  St: TStringStream;
begin
  Result := TSuperObject.Create(stArray);
  if Clipboard.HasFormat(ClipbrdJson) then
  try
    st := TStringStream.Create('');
    if Clipboard.GetFormat(ClipbrdJson, St) then
    begin
      St.Seek(0, 0);
      NewData := SO(St.DataString);
      if NewData.DataType = stArray then
        for row in NewData do
          Result.AsArray.Add(row);
    end;
  finally
    St.Free;
  end;
end;


begin
  ClipbrdJson := RegisterClipboardFormat('application/json');


end.

