unit MDOWait;

{$I ..\MDO.inc}

interface

uses
  Classes, SysUtils;

type

  { TMDOWaitHandler }

  TMDOWaitHandler = class
  public
    constructor Create;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
  end;

function GetWaitHandler: TMDOWaitHandler;
procedure RegisterWaitHandler(Handler: TMDOWaitHandler);

implementation

var
  WaitHandler: TMDOWaitHandler = nil;

type

  { TMDOEmptyWaitHandler }

  TMDOEmptyWaitHandler = class(TMDOWaitHandler)
  public
    procedure Start; override;
    procedure Stop; override;
  end;

function GetWaitHandler: TMDOWaitHandler;
begin
  if WaitHandler = nil then
    WaitHandler := TMDOEmptyWaitHandler.Create;
  Result := WaitHandler;
end;

procedure RegisterWaitHandler(Handler: TMDOWaitHandler);
begin
  if Assigned(WaitHandler) then
    FreeAndNil(WaitHandler);
  WaitHandler := Handler;
end;

{ TMDOWaitHandler }

constructor TMDOWaitHandler.Create;
begin

end;

{ TMDOEmptyWaitHandler }

procedure TMDOEmptyWaitHandler.Start;
begin

end;

procedure TMDOEmptyWaitHandler.Stop;
begin

end;

finalization
  if Assigned(WaitHandler) then
    FreeAndNil(WaitHandler);

end.

