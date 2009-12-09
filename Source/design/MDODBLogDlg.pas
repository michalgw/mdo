unit MDODBLogDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TLoginDialog }

  TLoginDialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditUser: TEdit;
    EditPassword: TEdit;
    LabelDatabase: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

function LoginDialog(const DBName: String; var UserName, Passwd: String): Boolean;
function LoginDialogEx(const DBName: String; var UserName, Passwd: String; UserNameReadOnly: Boolean): Boolean;

implementation

uses
  MDO;

function LoginDialog(const DBName: String; var UserName, Passwd: String): Boolean;
begin
  LoginDialogEx(DBName, UserName, Passwd, False);
end;

function LoginDialogEx(const DBName: String; var UserName, Passwd: String; UserNameReadOnly: Boolean): Boolean;
var
  LD: TLoginDialog;
begin
  with TLoginDialog.Create(nil) do
  begin
    EditUser.Text := UserName;
    EditPassword.Text := Passwd;
    LabelDatabase.Caption := DBName;
    EditUser.Enabled := not UserNameReadOnly;
    Result := (ShowModal = mrOK);
    if Result then
    begin
      UserName := EditUser.Text;
      Passwd := EditPassword.Text;
    end;
    Free;
  end;
end;

initialization
  {$I MDODBLogDlg.lrs}
  LoginDialogProc := @LoginDialog;

end.

