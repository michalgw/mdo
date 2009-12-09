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
  MDOCustomDataset, MDOSQL;

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

function EditMDOGeneratorLink(AGeneratorLink: TMDOGeneratorLink): Boolean; overload;
function EditMDOGeneratorLink(ADataSet: TMDODataSet): Boolean; overload;

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

function EditMDOGeneratorLink(ADataSet: TMDODataSet): Boolean; overload;
const
  SQLS = 'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE (RDB$SYSTEM_FLAG = 0) ' +
         'OR (RDB$SYSTEM_FLAG IS NULL)';
var
  fSQL: TMDOSQL;
  i: Integer;
  bDBC, bTRC: Boolean;
begin
  with TMDOGeneratorLinkEditForm.Create(Application) do
  begin
    cbxFields.Items.Clear;
    cbxGenerators.Items.Clear;
    fSQL := TMDOSQL.Create(nil);
    fSQL.Database := ADataSet.Database;
    fSQL.Transaction := ADataSet.Transaction;
    fSQL.SQL.Text := SQLS;
    bDBC := ADataSet.Database.Connected;
    bTRC := ADataSet.Transaction.Active;
    if not bDBC then
      ADataSet.Database.Open;
    if not bTRC then
      ADataSet.Transaction.StartTransaction;
    fSQL.ExecQuery;
    while not fSQL.Eof do
    begin
      cbxGenerators.Items.Add(fSQL.Fields[0].AsString);
      fSQL.Next;
    end;
    ADataSet.Transaction.Rollback;
    if bTRC then
      ADataSet.Transaction.Active := True;
    ADataSet.Database.Connected := bDBC;
    fSQL.Free;
    if ADataSet.FieldDefs.Count > 0 then
      for i := 0 to ADataSet.FieldDefs.Count - 1 do
        cbxFields.Items.Add(ADataSet.FieldDefs[i].Name)
    else
      if ADataSet.Fields.Count > 0 then;
        for i := 0 to ADataSet.Fields.Count - 1 do
          cbxFields.Items.Add(ADataSet.Fields[i].FieldName);
    MDOGeneratorLink := ADataSet.GeneratorLink;
    Result := Edit;
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
  if cbxGenerators.ItemIndex = -1 then
    cbxGenerators.Text := MDOGeneratorLink.Generator;
  if cbxFields.ItemIndex = -1 then
    cbxFields.Text := MDOGeneratorLink.Field;
  case MDOGeneratorLink.WhereApply of
    waNewRecord: grpWhereApply.ItemIndex := 0;
    waPost: grpWhereApply.ItemIndex := 1;
    waServer: grpWhereApply.ItemIndex := 2;
  end;
  Result := False;
  if ShowModal = mrOk then
  begin
    MDOGeneratorLink.Generator := cbxGenerators.Items.Strings[cbxGenerators.ItemIndex];
    MDOGeneratorLink.Field := cbxFields.Text;
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
