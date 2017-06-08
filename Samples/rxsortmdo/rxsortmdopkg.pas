{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxsortmdopkg;

interface

uses
  exsortmdo, RxSortMDO, rxsortmdoreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('rxsortmdoreg', @rxsortmdoreg.Register);
end;

initialization
  RegisterPackage('rxsortmdopkg', @Register);
end.
