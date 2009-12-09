{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2006, The Mercury Team          }
{                  Contact: info@mdolib.org                  }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}

unit MDOGeneratorLinkEditor;

{$I ..\mdo.inc}

interface

uses
  {$IFDEF MDO_FPC}
  LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  MDOCustomDataset;

type
  TMDOGeneratorLinkEditForm = class (TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbxFields: TComboBox;
    cbxGenerators: TComboBox;
    edtIncrement: TEdit;
    grpWhereApply: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    MDOGeneratorLink: TMDOGeneratorLink;
    function Edit: Boolean;
  end;
  
var
  MDOGeneratorLinkEditForm: TMDOGeneratorLinkEditForm;

function EditMDOGeneratorLink(AGeneratorLink: TMDOGeneratorLink): Boolean;

implementation

{$IFNDEF MDO_FPC}
{$R *.DFM}
{$ENDIF}

function EditMDOGeneratorLink(AGeneratorLink: TMDOGeneratorLink): Boolean;
begin
  with TMDOGeneratorLinkEditForm.Create(Application) do
  try
    MDOGeneratorLink := AGeneratorLink;
    Result := Edit;
  finally
    Free;
  end;
end;

{ TMDOGeneratorLinkEditForm }

{
************************** TMDOGeneratorLinkEditForm ***************************
}
function TMDOGeneratorLinkEditForm.Edit: Boolean;
begin
  cbxGenerators.ItemIndex := cbxGenerators.Items.IndexOf(MDOGeneratorLink.Generator);
  cbxFields.ItemIndex := cbxFields.Items.IndexOf(MDOGeneratorLink.Field);
  case MDOGeneratorLink.WhereApply of
    waNewRecord: grpWhereApply.ItemIndex := 0;
    waPost: grpWhereApply.ItemIndex := 1;
    waServer: grpWhereApply.ItemIndex := 2;
  end;
  Result := False;
  if ShowModal = mrOk then
  begin
    MDOGeneratorLink.Generator := cbxGenerators.Items.Strings[cbxGenerators.ItemIndex];
    MDOGeneratorLink.Field := cbxFields.Items.Strings[cbxFields.ItemIndex];
    case grpWhereApply.ItemIndex of
      0: MDOGeneratorLink.WhereApply := waNewRecord;
      1: MDOGeneratorLink.WhereApply := waPost;
      2: MDOGeneratorLink.WhereApply := waServer;
    end;
    Result := True;
  end;
end;

{$IFDEF MDO_FPC}
initialization
  {$I MDOGeneratorLinkEditor.lrs}
{$ENDIF}

end.
