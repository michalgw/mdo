unit MDOLCLWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMDOLCLWait = class(TComponent)
  end;

implementation

uses
  MDOWait, Forms, Controls;

type

  { TMDOLCLWaitHandler }

  TMDOLCLWaitHandler = class(TMDOWaitHandler)
  private
    FActive: Boolean;
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  published
    property Active: Boolean read FActive write FActive default True;
  end;

{ TMDOLCLWaitHandler }

constructor TMDOLCLWaitHandler.Create;
begin
  inherited;
  FActive := True;
  FList := TList.Create;
end;

destructor TMDOLCLWaitHandler.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TMDOLCLWaitHandler.Start;
begin
  if not FActive then
    Exit;
  FList.Add(Pointer(Screen.Cursor));
  if (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault) then
    Screen.Cursor := crSQLWait;
end;

procedure TMDOLCLWaitHandler.Stop;
begin
  if not FActive then
    Exit;
  if FList.Count > 0 then
  begin
    Screen.Cursor := TCursor(PtrInt(FList.Items[FList.Count - 1]));
    FList.Delete(FList.Count - 1);
  end;
end;

initialization
  RegisterWaitHandler(TMDOLCLWaitHandler.Create);

end.
