unit fm_main;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, EditBtn, MDOServices{, MDOServices};

type

  { TfrmMain }

  TfrmMain = class(TForm)
    edtDbFile: TFileNameEdit;
    Label12: TLabel;
    MDOBackupService1: TMDOBackupService;
    Button1: TButton;
    mmoLog: TMemo;
    edtBkFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation


procedure TfrmMain.Button1Click(Sender: TObject);
begin
  with MDOBackupService1 do
  begin
    mmoLog.Lines.Clear;
    mmoLog.Lines.Add('Start');
    DatabaseName := edtDbFile.Text;
    ServerName := '';
    Protocol := Local;
    Active := True;
    BackupFile.Add(edtBkFile.Text);
    try
      ServiceStart;
      while not Eof do
        mmoLog.Lines.Add(GetNextLine);
    finally
      Active := False;
      mmoLog.Lines.Add('Finish');
    end;
  end;
end;

initialization
  {$i fm_main.lrs}

end.
