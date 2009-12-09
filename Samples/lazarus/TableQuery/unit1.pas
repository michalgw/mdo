unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, DbCtrls, DBGrids, SynEdit, MDODatabase,
  MDOTable, MDOQuery, MDOCustomDataSet;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    ListBox1: TListBox;
    MDODatabase1: TMDODatabase;
    MDOQuery1: TMDOQuery;
    MDOTable1: TMDOTable;
    MDOTransaction1: TMDOTransaction;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheetQuery: TTabSheet;
    TabSheetTable: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    bConnect: TToolButton;
    bQClose: TToolButton;
    bQNew: TToolButton;
    bDisconnect: TToolButton;
    bTOpen: TToolButton;
    bTClose: TToolButton;
    bQOpen: TToolButton;
    bQExec: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure bConnectClick(Sender: TObject);
    procedure bDisconnectClick(Sender: TObject);
    procedure bQCloseClick(Sender: TObject);
    procedure bQExecClick(Sender: TObject);
    procedure bQNewClick(Sender: TObject);
    procedure bQOpenClick(Sender: TObject);
    procedure bTCloseClick(Sender: TObject);
    procedure bTOpenClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MDOQuery1AfterClose(DataSet: TDataSet);
    procedure MDOQuery1AfterOpen(DataSet: TDataSet);
    procedure MDOTable1AfterClose(DataSet: TDataSet);
    procedure MDOTable1AfterOpen(DataSet: TDataSet);
    procedure ToolButton2Click(Sender: TObject);
  private
    procedure AdjustDBButtons;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  fMDODatabaseEdit;

{ TForm1 }

procedure TForm1.bConnectClick(Sender: TObject);
begin
  if EdiTMDODatabase(MDODatabase1) then
    MDODatabase1.Open;
  ListBox1.Clear;
  MDODatabase1.GetTableNames(ListBox1.Items);
  AdjustDBButtons;
end;

procedure TForm1.bDisconnectClick(Sender: TObject);
begin
  MDODatabase1.Close;
  AdjustDBButtons;
end;

procedure TForm1.bQCloseClick(Sender: TObject);
begin
  MDOQuery1.Close;
end;

procedure TForm1.bQExecClick(Sender: TObject);
begin
  MDOQuery1.SQL.Clear;
  MDOQuery1.SQL.AddStrings(Memo1.Lines);
  MDOQuery1.ExecSQL;
end;

procedure TForm1.bQNewClick(Sender: TObject);
begin
  MDOQuery1.Close;
  Memo1.Clear;
end;

procedure TForm1.bQOpenClick(Sender: TObject);
begin
  MDOQuery1.SQL.Clear;
  MDOQuery1.SQL.AddStrings(Memo1.Lines);
  MDOQuery1.Open;
end;

procedure TForm1.bTCloseClick(Sender: TObject);
begin
  MDOTable1.Close;
end;

procedure TForm1.bTOpenClick(Sender: TObject);
begin
  MDOTable1.TableName:=ListBox1.Items[ListBox1.ItemIndex];
  MDOTable1.Open;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if not MDODatabase1.Connected then
    Exit;
  MDOTable1.Close;
  MDOTable1.TableName := ListBox1.Items[ListBox1.ItemIndex];
  MDOTable1.Open;
  AdjustDBButtons;
end;

procedure TForm1.MDOQuery1AfterClose(DataSet: TDataSet);
begin
  bQOpen.Enabled:=True;
  bQExec.Enabled:=True;
  bQNew.Enabled:=True;
  bQClose.Enabled:=False;
end;

procedure TForm1.MDOQuery1AfterOpen(DataSet: TDataSet);
begin
  bQOpen.Enabled:=False;
  bQExec.Enabled:=False;
  bQNew.Enabled:=True;
  bQClose.Enabled:=True;
end;

procedure TForm1.MDOTable1AfterClose(DataSet: TDataSet);
begin
  bTClose.Enabled:=False;
  bTOpen.Enabled:=True;
end;

procedure TForm1.MDOTable1AfterOpen(DataSet: TDataSet);
begin
  bTClose.Enabled:=True;
  bTOpen.Enabled:=False;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  MDODatabase1.Close;
  Close;
end;

procedure TForm1.AdjustDBButtons;
begin
  if MDODatabase1.Connected then
  begin
    bConnect.Enabled:=False;
    bDisconnect.Enabled:=True;
    bTOpen.Enabled:=True;
    bQOpen.Enabled:=True;
    bQExec.Enabled:=True;
  end
  else
  begin
    bConnect.Enabled:=True;
    bDisconnect.Enabled:=False;
    bTOpen.Enabled:=False;
    bTClose.Enabled:=False;
    bQOpen.Enabled:=False;
    bQExec.Enabled:=False;
    bQNew.Enabled:=False;
  end;
end;

initialization
  {$I unit1.lrs}

end.

