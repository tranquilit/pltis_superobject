unit soclipbrd;

{$mode delphi}{$H+}

interface
uses SuperObject,ClipBrd,lcltype;

function ClipboardSOData: ISuperObject;

var
  ClipbrdJson: TClipboardFormat;



implementation

uses
  Classes, SysUtils,soutils,Dialogs;

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
      begin
        for row in NewData do
          Result.AsArray.Add(row);
      end
      else
        if NewData.DataType = stObject then
          Result.AsArray.Add(NewData);
    end;
  finally
    St.Free;
  end
  else if Clipboard.HasFormat(CF_Text) then
  try
    st := TStringStream.Create('');
    if Clipboard.GetFormat(CF_Text, St) then
    try
      St.Seek(0, 0);
      NewData := SO(St.DataString);
      if NewData.DataType in [stArray,stObject] then
      begin
        if NewData.DataType = stArray then
        begin
          for row in NewData do
            Result.AsArray.Add(row);
        end
        else if NewData.DataType = stObject then
          Result.AsArray.Add(NewData);
      end;
    except
      on E:Exception do ShowMessage('Invalid clipboard format. Expect JSON text');
    end
  finally
    St.Free;
  end

end;


begin
  ClipbrdJson := RegisterClipboardFormat('application/json');


end.

