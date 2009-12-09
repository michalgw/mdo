program BackupService;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fm_main in 'fm_main.pas', mdolaz_dsg {frmMain};

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
