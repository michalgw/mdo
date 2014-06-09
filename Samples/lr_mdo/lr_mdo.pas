{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_mdo;

interface

uses
  lr_mdodataset, lr_mdoeditparams, lr_mdoconst, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lr_mdodataset', @lr_mdodataset.Register);
end;

initialization
  RegisterPackage('lr_mdo', @Register);
end.
