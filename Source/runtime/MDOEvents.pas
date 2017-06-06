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

{$I ..\MDO.inc}

unit MDOEvents;

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, DB, MDOHeader, MDOExternals, MDO, MDODatabase;

const
  MaxEvents = 15;
  EventLength = 64;

type

  TEventAlert = procedure (Sender: TObject; EventName: string; EventCount: 
          longint; var CancelAlerts: Boolean) of object;
  TEventBuffer = array[ 0..MaxEvents-1, 0..EventLength-1] of char;

  TMDOEvents = class (TComponent)
  private
    Buffer: TEventBuffer;
    Changing: Boolean;
    CS: TRTLCriticalSection;
    EventBuffer: PChar;
    EventBufferLen: Integer;
    EventID: ISC_LONG;
    FDatabase: TMDODatabase;
    FEvents: TStrings;
    FIBLoaded: Boolean;
    FOnEventAlert: TEventAlert;
    FQueued: Boolean;
    FRegistered: Boolean;
    ProcessingEvents: Boolean;
    RegisteredState: Boolean;
    ResultBuffer: PChar;
    procedure DoQueueEvents;
    procedure EventChange(sender: TObject);
    procedure SetDatabase(Value: TMDODatabase);
    procedure UpdateResultBuffer(length: short; AUpdated: PChar);
    procedure ValidateDatabase(Database: TMDODatabase);
  protected
    function GetNativeHandle: TISC_DB_HANDLE;
    procedure HandleEvent;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
            override;
    procedure SetEvents(Value: TStrings);
    procedure SetRegistered(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelEvents;
    procedure QueueEvents;
    procedure RegisterEvents;
    procedure UnRegisterEvents;
    property Queued: Boolean read FQueued;
  published
    property Database: TMDODatabase read FDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property Registered: Boolean read FRegistered write SetRegistered;
  end;

implementation

uses
  MDOIntf;

{ TMDOEvents }

function HandleEvent( param: Pointer): PtrInt;
begin
  { don't let exceptions propogate out of thread }
  try
    TMDOEvents( param).HandleEvent;
  except
    Classes.ApplicationHandleException(nil);
  end;
end;

procedure FBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
var
  ThreadID: PtrUInt;
begin
  { Handle events asynchronously in second thread }
  EnterCriticalSection( TMDOEvents( ptr).CS);
  TMDOEvents( ptr).UpdateResultBuffer( length, updated);
  if TMDOEvents( ptr).Queued then
    BeginThread( nil, 8192, @HandleEvent, ptr, 0, ThreadID);
  LeaveCriticalSection( TMDOEvents( ptr).CS);
end;


{
********************************** TMDOEvents **********************************
}
constructor TMDOEvents.Create(AOwner: TComponent);
begin
  inherited Create( AOwner);
  FIBLoaded := False;
  CheckFBLoaded;
  FIBLoaded := True;
  InitCriticalSection(CS);
  FEvents := TStringList.Create;
  with TStringList( FEvents) do
  begin
    OnChange := @EventChange;
    Duplicates := dupIgnore;
  end;
end;

destructor TMDOEvents.Destroy;
begin
  if FIBLoaded then
  begin
    UnregisterEvents;
    SetDatabase( nil);
    TStringList(FEvents).OnChange := nil;
    FEvents.Free;
    DoneCriticalsection(CS);
  end;
  inherited Destroy;
end;

procedure TMDOEvents.CancelEvents;
begin
  if ProcessingEvents then
    MDOError(mdoeInvalidCancellation, [nil]);
  if FQueued then
  begin
    try
      { wait for event handler to finish before cancelling events }
      EnterCriticalSection( CS);
      ValidateDatabase( Database);
      FQueued := false;
      Changing := true;
      if (isc_Cancel_events( StatusVector, @FDatabase.Handle, @EventID) > 0) then
        MDODatabaseError;
    finally
      LeaveCriticalSection( CS);
    end;
  end;
end;

procedure TMDOEvents.DoQueueEvents;
var
  callback: Pointer;
begin
  ValidateDatabase( DataBase);
  callback := @FBEventCallback;
  if (isc_que_events( StatusVector, @FDatabase.Handle, @EventID, EventBufferLen,
                     EventBuffer, TISC_CALLBACK(callback), PVoid(Self)) > 0) then
    MDODatabaseError;
  FQueued := true;
end;

procedure TMDOEvents.EventChange(sender: TObject);
begin
  { check for blank event }
  if TStringList(Events).IndexOf( '') <> -1 then
    MDOError(mdoeInvalidEvent, [nil]);
  { check for too many events }
  if Events.Count > MaxEvents then
  begin
    TStringList(Events).OnChange := nil;
    Events.Delete( MaxEvents);
    TStringList(Events).OnChange := @EventChange;
    MDOError(mdoeMaximumEvents, [nil]);
  end;
  if Registered then RegisterEvents;
end;

function TMDOEvents.GetNativeHandle: TISC_DB_HANDLE;
begin
  if assigned( FDatabase) and FDatabase.Connected then
    Result := FDatabase.Handle
  else result := nil;
end;

procedure TMDOEvents.HandleEvent;
var
  Status: PStatusVector;
  CancelAlerts: Boolean;
  i: Integer;
begin
  try
    { prevent modification of vital data structures while handling events }
    EnterCriticalSection( CS);
    ProcessingEvents := true;
    isc_event_counts( StatusVector, EventBufferLen, EventBuffer, ResultBuffer);
    CancelAlerts := false;
    if assigned(FOnEventAlert) and not Changing then
    begin
      for i := 0 to Events.Count-1 do
      begin
        try
        Status := StatusVectorArray;
        if (Status^[i] <> 0) and not CancelAlerts then
            FOnEventAlert( self, Events[Events.Count-i-1], Status^[i], CancelAlerts);
        except
          Classes.ApplicationHandleException(nil);
        end;
      end;
    end;
    Changing := false;
    if not CancelAlerts and FQueued then DoQueueEvents;
  finally
    ProcessingEvents := false;
    LeaveCriticalSection( CS);
  end;
end;

procedure TMDOEvents.Loaded;
begin
  inherited Loaded;
  try
    if RegisteredState then RegisterEvents;
  except
    if csDesigning in ComponentState then
      Classes.ApplicationHandleException(nil)
    else raise;
  end;
end;

procedure TMDOEvents.Notification(AComponent: TComponent; Operation: 
        TOperation);
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    UnregisterEvents;
    FDatabase := nil;
  end;
end;

procedure TMDOEvents.QueueEvents;
begin
  if not FRegistered then
    MDOError(mdoeNoEventsRegistered, [nil]);
  if ProcessingEvents then
    MDOError(mdoeInvalidQueueing, [nil]);
  if not FQueued then
  begin
    try
      { wait until current event handler is finished before queuing events }
      EnterCriticalSection( CS);
      DoQueueEvents;
      Changing := true;
    finally
      LeaveCriticalSection( CS);
    end;
  end;
end;

procedure TMDOEvents.RegisterEvents;
var
  i: Integer;
  bufptr: Pointer;
  eventbufptr: Pointer;
  resultbufptr: Pointer;
  buflen: Integer;

function ev(no: Integer): PChar;
begin
  if no < Events.Count then
    Result := @Buffer[no][0]
  else
    Result := nil;
end;

begin
  ValidateDatabase( Database);
  if csDesigning in ComponentState then FRegistered := true
  else begin
    UnregisterEvents;
    if Events.Count = 0 then exit;
    for i := 0 to Events.Count-1 do
      StrPCopy( @Buffer[i][0], Events[i]);
    i := Events.Count;
    bufptr := @buffer[0];
    eventbufptr :=  @EventBuffer;
    resultBufPtr := @ResultBuffer;
    //TODO: Remove asm
    (*
    {$IFNDEF MDO_64BIT}
    asm
      mov ecx, dword ptr [i]
      mov eax, dword ptr [bufptr]
      @@1:
      push eax
      add  eax, EventLength
      loop @@1
      push dword ptr [i]
      push dword ptr [resultBufPtr]
      push dword ptr [eventBufPtr]
      call [isc_event_block]
      mov  dword ptr [bufLen], eax
      mov eax, dword ptr [i]
      shl eax, 2
      add eax, 12
      add esp, eax
    end;
    {$ENDIF}
    *)
    buflen := isc_event_block(eventbufptr, resultbufptr, i, [ev(0),ev(1),ev(2),ev(3),ev(4),ev(5),
      ev(6),ev(7),ev(8),ev(9),ev(10),ev(11),ev(12),ev(13),ev(14)]);
    EventBufferlen := Buflen;
    FRegistered := true;
    QueueEvents;
  end;
end;

procedure TMDOEvents.SetDatabase(Value: TMDODatabase);
begin
  if value <> FDatabase then
  begin
    UnregisterEvents;
    if assigned( value) and value.Connected then ValidateDatabase( value);
    FDatabase := value;
  end;
end;

procedure TMDOEvents.SetEvents(Value: TStrings);
begin
  FEvents.Assign( value);
end;

procedure TMDOEvents.SetRegistered(Value: Boolean);
begin
  if (csReading in ComponentState) then
    RegisteredState := value
  else if FRegistered <> value then
    if value then RegisterEvents else UnregisterEvents;
end;

procedure TMDOEvents.UnRegisterEvents;
begin
  if ProcessingEvents then
    MDOError(mdoeInvalidRegistration, [nil]);
  if csDesigning in ComponentState then
    FRegistered := false
  else if not (csLoading in ComponentState) then
  begin
    CancelEvents;
    if FRegistered then
    begin
      isc_free( EventBuffer);
      EventBuffer := nil;
      isc_free( ResultBuffer);
      ResultBuffer := nil;
    end;
    FRegistered := false;
  end;
end;

procedure TMDOEvents.UpdateResultBuffer(length: short; AUpdated: PChar);
var
  i: Integer;
begin
  for i := 0 to length-1 do
    ResultBuffer[i] := AUpdated[i];
end;

procedure TMDOEvents.ValidateDatabase(Database: TMDODatabase);
begin
  if not assigned( Database) then
    MDOError(mdoeDatabaseNameMissing, [nil]);
  if not Database.Connected then
    MDOError(mdoeDatabaseClosed, [nil]);
end;


end.
