unit MDOSQLEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SynEdit, SynHighlighterSQL, MDOUpdateSQLEditor;

type

{  TGetTableNamesProc = procedure(List: TStrings; SystemTables: Boolean) of object;
  TGetFieldNamesProc = procedure(Table: String; List: TStrings) of object;}

  { TTMDOSQLEditForm }

  { TMDOSQLEditForm }

  TMDOSQLEditForm = class(TForm)
    ImageListMain: TImageList;
    LabelTables: TLabel;
    LabelFields: TLabel;
    ListBoxTables: TListBox;
    ListBoxFields: TListBox;
    OpenDialogSQL: TOpenDialog;
    PanelLeft: TPanel;
    SaveDialogSQL: TSaveDialog;
    SplitterCenter: TSplitter;
    SplitterLeft: TSplitter;
    StatusBarMain: TStatusBar;
    SynEditSQL: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
    ToolBarMain: TToolBar;
    ToolButtonOk: TToolButton;
    ToolButtonCancel: TToolButton;
    ToolButtonS1: TToolButton;
    ToolButtonLoad: TToolButton;
    ToolButtonSave: TToolButton;
    procedure ListBoxFieldsDblClick(Sender: TObject);
    procedure ListBoxTablesClick(Sender: TObject);
    procedure ListBoxTablesDblClick(Sender: TObject);
    procedure SynEditSQLDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynEditSQLDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ToolButtonCancelClick(Sender: TObject);
    procedure ToolButtonLoadClick(Sender: TObject);
    procedure ToolButtonOkClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
  private
    { private declarations }
    procedure LoadTables;
    procedure LoadFields;
  public
    { public declarations }
    GetTableNamesProc: TGetTableNamesProc;
    GetFieldNamesProc: TGetFieldNamesProc;
    SQL: String;
    function Execute: Boolean;
  end; 

function EditSQL(var SQLText: String; GetTableNamesPr: TGetTableNamesProc;
  GetFieldNamesPr: TGetFieldNamesProc): Boolean;

implementation

{$R *.lfm}

function EditSQL(var SQLText: String; GetTableNamesPr: TGetTableNamesProc;
  GetFieldNamesPr: TGetFieldNamesProc): Boolean;
begin
  with TMDOSQLEditForm.Create(nil) do
  begin
    SQL := SQLText;
    GetTableNamesProc := GetTableNamesPr;
    GetFieldNamesProc := GetFieldNamesPr;
    Result := Execute;
    if Result then
      SQLText := SynEditSQL.Text;
    Free;
  end;
end;

{ TMDOSQLEditForm }

procedure TMDOSQLEditForm.ToolButtonLoadClick(Sender: TObject);
begin
  if OpenDialogSQL.Execute then
    SynEditSQL.Lines.LoadFromFile(OpenDialogSQL.FileName);
end;

procedure TMDOSQLEditForm.ToolButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMDOSQLEditForm.ListBoxTablesClick(Sender: TObject);
begin
  LoadFields;
end;

procedure TMDOSQLEditForm.ListBoxFieldsDblClick(Sender: TObject);
begin
  if ListBoxFields.ItemIndex > -1 then
    SynEditSQL.InsertTextAtCaret(ListBoxFields.Items[ListBoxFields.ItemIndex]);
end;

procedure TMDOSQLEditForm.ListBoxTablesDblClick(Sender: TObject);
begin
  if ListBoxTables.ItemIndex > -1 then
    SynEditSQL.InsertTextAtCaret(ListBoxTables.Items[ListBoxTables.ItemIndex]);
end;

procedure TMDOSQLEditForm.SynEditSQLDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if (Source is TListBox) then
    SynEditSQL.InsertTextAtCaret(TListBox(Source).Items[TListBox(Source).ItemIndex]);
end;

procedure TMDOSQLEditForm.SynEditSQLDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TMDOSQLEditForm.ToolButtonOkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMDOSQLEditForm.ToolButtonSaveClick(Sender: TObject);
begin
  if SaveDialogSQL.Execute then
    SynEditSQL.Lines.SaveToFile(SaveDialogSQL.FileName);
end;

procedure TMDOSQLEditForm.LoadTables;
begin
  ListBoxTables.Clear;
  if (GetTableNamesProc <> nil) then
  begin
    GetTableNamesProc(ListBoxTables.Items, false);
    SynSQLSyn.TableNames.Clear;
    GetTableNamesProc(SynSQLSyn.TableNames, false);
  end;
end;

procedure TMDOSQLEditForm.LoadFields;
begin
  ListBoxFields.Clear;
  if (GetFieldNamesProc <> nil) then
    GetFieldNamesProc(ListBoxTables.Items[ListBoxTables.ItemIndex], ListBoxFields.Items);
end;

function TMDOSQLEditForm.Execute: Boolean;
begin
  LoadTables;
  SynEditSQL.ClearAll;
  SynEditSQL.Lines.Text := SQL;
  Result := (ShowModal = mrOk);
  SQL := SynEditSQL.Text;
end;

end.

