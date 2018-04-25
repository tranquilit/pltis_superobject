{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_superobject;

interface

uses
  superobject, superxmlparser, soutils, soclipbrd, sodbutils, supertypes, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pltis_superobject', @Register);
end.
