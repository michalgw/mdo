{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                    Mercury Database Objects [info@mdolib.com]          }
{                                                                        }
{************************************************************************}
unit MDOServiceEditor;

interface

{$I ..\MDO.inc}

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MDOServices,
  MDOConst, Variants;

type
  TMDOServiceEditorForm = class (TForm)
    bBrowse: TButton;
    bCancel: TButton;
    bOk: TButton;
    cbActive: TCheckBox;
    cbLoginPrompt: TCheckBox;
    cbProtocol: TComboBox;
    cbServer: TComboBox;
    eDataBase: TEdit;
    ePwd: TEdit;
    eServer: TEdit;
    eUsr: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lProtocol: TLabel;
    lServer: TLabel;
    mSettings: TMemo;
    procedure bBrowseClick(Sender: TObject);
    procedure cbServerChange(Sender: TObject);
    procedure ePwdChange(Sender: TObject);
    procedure eUsrChange(Sender: TObject);
  private
    Service: TMDOCustomService;
    procedure AddParam(AName, Value: string);
    procedure DeleteParam(AName: string);
    function Edit: Boolean;
    function GetParam(AName: string): string;
  end;
  
var
  MDOServiceEditorForm: TMDOServiceEditorForm;

function EditMDOService(AService: TMDOCustomService): Boolean;

implementation

uses TypInfo;

{$R *.lfm}

function EditMDOService(AService: TMDOCustomService): Boolean;
begin
  with TMDOServiceEditorForm.Create(Application) do
    try
      Service := AService;
      Result := Edit;
    finally
      Free;
    end;
end;

{
**************************** TMDOServiceEditorForm *****************************
}
procedure TMDOServiceEditorForm.AddParam(AName, Value: string);
var
  i: Integer;
  found: Boolean;
begin
  found := False;
  if Trim(Value) <> '' then
    begin
      for i := 0 to mSettings.Lines.Count - 1 do
        begin
          if (Pos(AName, LowerCase(mSettings.Lines.Names[i])) = 1) then
            begin
              mSettings.Lines.Values[mSettings.Lines.Names[i]] := Value;
              found := True;
              break;
            end;
        end;
      if not found then mSettings.Lines.Add(AName + '=' + Value);
    end
  else DeleteParam(AName);
end;

procedure TMDOServiceEditorForm.bBrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    begin
      Filter := SDatabaseFilter;
      if Execute then
        eDataBase.Text := FileName;
    end;
end;

procedure TMDOServiceEditorForm.cbServerChange(Sender: TObject);
begin
  lServer.Enabled := cbServer.ItemIndex = 1;
  eServer.Enabled := cbServer.ItemIndex = 1;
  
  lProtocol.Enabled := cbServer.ItemIndex = 1;
  cbProtocol.Enabled := cbServer.ItemIndex = 1;
end;

procedure TMDOServiceEditorForm.DeleteParam(AName: string);
var
  i: Integer;
begin
  for i := 0 to mSettings.Lines.Count - 1 do
    begin
      if (Pos(AName, LowerCase(mSettings.Lines.Names[i])) = 1) then
        begin
          mSettings.Lines.Delete(i);
          break;
        end;
    end;
end;

function TMDOServiceEditorForm.Edit: Boolean;
var
  PropInfo: PPropInfo;
begin
  mSettings.Lines := Service.Params;
  cbLoginPrompt.Checked := Service.LoginPrompt;
  eUsr.Text := GetParam('user_name');
  ePwd.Text := GetParam('password');
  
  PropInfo := GetPropInfo(Service, 'DatabaseName', [tkString]);
  
  eDataBase.Enabled := (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString);
  bBrowse.Enabled := (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString);
  
  
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString) then
    eDataBase.Text := VarToStr(GetPropValue(Service, 'DatabaseName'));
  
  mSettings.Lines := Service.Params;
  
  case Service.Protocol of
    Local: cbServer.ItemIndex := 0;
    TCP: cbProtocol.ItemIndex := 0;
    SPX: cbProtocol.ItemIndex := 1;
    NamedPipe: cbProtocol.ItemIndex := 2;
  end;
  
  eServer.Text := Service.ServerName;
  cbActive.Checked := Service.Active;
  
  Result := False;
  if ShowModal = mrOk then
    begin
      if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkString) then
        SetPropValue(Service, 'DatabaseName', eDataBase.Text);
  
      if cbServer.ItemIndex = 0 then Service.Protocol := Local
      else
        begin
          case cbProtocol.ItemIndex of
            0: Service.Protocol := TCP;
            1: Service.Protocol := SPX;
            2: Service.Protocol := NamedPipe;
          end;
        end;
  
  
      if cbProtocol.ItemIndex = 0 then Service.ServerName := 'localhost'
      else Service.ServerName := eServer.Text;
  
      Service.Params := mSettings.Lines;
      Service.LoginPrompt := cbLoginPrompt.Checked;
      Service.Active := cbActive.Checked;
  
      Result := True;
    end;
end;

procedure TMDOServiceEditorForm.ePwdChange(Sender: TObject);
begin
  AddParam('password', ePwd.Text);
end;

procedure TMDOServiceEditorForm.eUsrChange(Sender: TObject);
begin
  AddParam('user_name', eUsr.Text);
end;

function TMDOServiceEditorForm.GetParam(AName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to mSettings.Lines.Count - 1 do
    begin
      if (Pos(AName, LowerCase(mSettings.Lines.Names[i])) = 1) then
        begin
          Result := mSettings.Lines.Values[mSettings.Lines.Names[i]];
          break;
        end;
    end;
end;

end.

