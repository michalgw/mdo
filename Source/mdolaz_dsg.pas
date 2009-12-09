{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit mdolaz_dsg; 

interface

uses
    MDODatabaseEdit, MDOEventsEditor, MDOGeneratorLinkEditor, 
  MDOServiceEditor, MDOTransactionEdit, MDOUpdateSQLEditor, MDOSQLEdit, 
  MDODBRegLaz, MDODBLogDlg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MDODBRegLaz', @MDODBRegLaz.Register); 
end; 

initialization
  RegisterPackage('mdolaz_dsg', @Register); 
end.
