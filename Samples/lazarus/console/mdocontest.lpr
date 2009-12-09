program mdocontest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  MDODatabase, MDOQuery, DB;

type

  { TMDOConsoleApp }

  TMDOConsoleApp = class(TCustomApplication)
  private
    db: TMDODataBase;
    tr: TMDOTransaction;
    qr: TMDOQuery;
    procedure DumpDataset(DS: TDataSet);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMDOConsoleApp }

procedure TMDOConsoleApp.DumpDataset(DS: TDataSet);
var
  i: Integer;
begin
  if (Assigned(DS) and DS.Active) then
  begin
    WriteLn('Records: ' + IntToStr(DS.RecordCount));
    if DS.RecordCount > 0 then
    begin
      DS.First;
      while not DS.EOF do
      begin
        for i := 0 to DS.Fields.Count - 1 do
          WriteLn(DS.Fields[i].FieldName + ': ' + DS.Fields[i].AsString);
        WriteLn('------------------------------');
        DS.Next;
      end;
    end;
  end;
end;

procedure TMDOConsoleApp.DoRun;
var
  sql, dbname, username, passw: String;
begin
  db := TMDODataBase.Create(nil);
  tr := TMDOTransaction.Create(nil);
  db.DefaultTransaction := tr;
  qr := TMDOQuery.Create(nil);
  qr.Database := db;
  WriteLn('Enter database name:');
  ReadLn(dbname);
  WriteLn('User name:');
  ReadLn(username);
  WriteLn('Password:');
  ReadLn(passw);
  db.DatabaseName := dbname;
  db.Params.Values['user_name'] := username;
  db.Params.Values['password'] := passw;
  db.LoginPrompt:=false;
  try
    Write('Open database...');
    db.Open;

  except
    WriteLn('Exception');
    Terminate;
  end;
  WriteLn('OK');
  WriteLn('Enter SQL:');
  ReadLn(sql);
  qr.SQL.Text := sql;
  qr.Open;
  DumpDataset(qr);
  qr.Close;
  db.Close;
  qr.Free;
  db.Free;
  tr.Free;
  Terminate;
end;

constructor TMDOConsoleApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMDOConsoleApp.Destroy;
begin
  inherited Destroy;
end;

procedure TMDOConsoleApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMDOConsoleApp;

{$IFDEF WINDOWS}{$R mdocontest.rc}{$ENDIF}

begin
  Application:=TMDOConsoleApp.Create(nil);
  Application.Title:='MDO Console app demo';
  Application.Run;
  Application.Free;
end.

