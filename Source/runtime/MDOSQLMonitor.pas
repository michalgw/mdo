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
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

{
  This unit has been significantly revised for the Lazarus port. Specially,
  there was a need to re-organise the code to isolate the Windows specific
  IPC and to introduce SV5 IPC as an alternative for Linux and other platforms.
}

unit MDOSQLMonitor;

{$I ..\MDO.inc}

interface

uses
  MDO, MDOUtils, MDOSQL, MDOCustomDataSet, MDODatabase, MDOServices, MDOConst,
  SysUtils, Classes,
{$IFDEF WINDOWS }
  Windows
{$ELSE}
  unix
{$ENDIF}
;

{Note that the original inter-thread communication between the Reader Thread and
 the ISQL Monitor used the Windows PostMessage interface. This is currently not
 useable under the FPC RTL as AllocateHWnd is not functional. It has been replaced
 by the use of the Synchronize method.}

{$IFDEF WINDOWS}
{$DEFINE USE_WINDOWS_IPC}
{$ENDIF}

{$IFDEF UNIX}
{$DEFINE USE_SV5_IPC}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE HAS_SEMTIMEDOP}
{$ENDIF}

type
  TSQLEvent = procedure(EventText: String; EventTime : TDateTime) of object;

  { TMDOCustomSQLMonitor }

  TMDOCustomSQLMonitor = class(TComponent)
  private
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure ReleaseObject;  {Called from Writer Thread}
    procedure ReceiveMessage(Msg: TObject);    {Called from Reader Thread}
    property OnSQL: TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property Enabled : Boolean read FEnabled write SetEnabled default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release;
  end;

  { TMDOSQLMonitor }

  TMDOSQLMonitor = class(TMDOCustomSQLMonitor)
  published
    property OnSQL;
    property TraceFlags;
    property Enabled;
  end;

  IIBSQLMonitorHook = interface
    ['{CF65434C-9B75-4298-BA7E-E6B85B3C769D}']
    procedure RegisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
    procedure SQLPrepare(qry: TMDOSQL);
    procedure SQLExecute(qry: TMDOSQL);
    procedure SQLFetch(qry: TMDOSQL);
    procedure DBConnect(db: TMDODatabase);
    procedure DBDisconnect(db: TMDODatabase);
    procedure TRStart(tr: TMDOTransaction);
    procedure TRCommit(tr: TMDOTransaction);
    procedure TRCommitRetaining(tr: TMDOTransaction);
    procedure TRRollback(tr: TMDOTransaction);
    procedure TRRollbackRetaining(tr: TMDOTransaction);
    procedure ServiceAttach(service: TMDOCustomService);
    procedure ServiceDetach(service: TMDOCustomService);
    procedure ServiceQuery(service: TMDOCustomService);
    procedure ServiceStart(service: TMDOCustomService);
    procedure SendMisc(Msg : String);
    function GetTraceFlags : TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetTraceFlags(const Value : TTraceFlags);
    function GetEnabled : boolean;
    procedure SetEnabled(const Value : Boolean);
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;


function MonitorHook: IIBSQLMonitorHook;
procedure EnableMonitoring;
procedure DisableMonitoring;
function MonitoringEnabled: Boolean;

implementation

uses
   contnrs, syncobjs, CustApp
   {$IFDEF USE_SV5_IPC}
   ,ipc, Errors, baseunix
   {$IF FPC_FULLVERSION <= 20402 } , initc {$ENDIF}
   {$ENDIF};

   {$IF FPC_FULLVERSION < 20600 }{$STATIC ON} {$ENDIF}

const
  cMonitorHookSize = 1024;
  cMsgWaitTime = 1000;
  cWriteMessageAvailable = 'WriterMsgQueue';

type
  { There are two possible objects.  One is a trace message object.
    This object holds the flag of the trace type plus the message.
    The second object is a Release object.  It holds the handle that
    the CM_RELEASE message is to be queued to. }

  { TTraceObject }

  TTraceObject = Class(TObject)
    FDataType : TTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
  public
    constructor Create(Msg : String; DataType : TTraceFlag); overload;
    constructor Create(obj : TTraceObject); overload;
    constructor Create(obj : TTraceObject; MsgOffset, MsgLen: integer); overload;
  end;

  { TReleaseObject }

  TReleaseObject = Class(TObject)
    FMonitor : TMDOCustomSQLMonitor;
  public
    constructor Create(Monitor : TMDOCustomSQLMonitor);
  end;

  {$IFDEF USE_SV5_IPC}
  {$I sv5ipc.inc}
  {$ENDIF}
  {$IFDEF USE_WINDOWS_IPC}
  {$I winipc.inc}
  {$ENDIF}

type

  { TMDOSQLMonitorHook }

  TMDOSQLMonitorHook = class(TInterfacedObject, IIBSQLMonitorHook)
  private
    FGlobalInterface: TGlobalInterface;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TMDOCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
    procedure SQLPrepare(qry: TMDOSQL); virtual;
    procedure SQLExecute(qry: TMDOSQL); virtual;
    procedure SQLFetch(qry: TMDOSQL); virtual;
    procedure DBConnect(db: TMDODatabase); virtual;
    procedure DBDisconnect(db: TMDODatabase); virtual;
    procedure TRStart(tr: TMDOTransaction); virtual;
    procedure TRCommit(tr: TMDOTransaction); virtual;
    procedure TRCommitRetaining(tr: TMDOTransaction); virtual;
    procedure TRRollback(tr: TMDOTransaction); virtual;
    procedure TRRollbackRetaining(tr: TMDOTransaction); virtual;
    procedure ServiceAttach(service: TMDOCustomService); virtual;
    procedure ServiceDetach(service: TMDOCustomService); virtual;
    procedure ServiceQuery(service: TMDOCustomService); virtual;
    procedure ServiceStart(service: TMDOCustomService); virtual;
    procedure SendMisc(Msg : String);
    function GetEnabled: Boolean;
    function GetTraceFlags: TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
    procedure ForceRelease;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
  end;

  { TWriterThread }

  TWriterThread = class(TThread)
  private
    { Private declarations }
    FGlobalInterface: TGlobalInterface;
    FMsgs : TObjectList;
    FCriticalSection: TCriticalSection;
    FMsgAvailable: TEventObject;
    procedure RemoveFromList;
    procedure PostRelease;
  public
    procedure ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
  protected
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure WriteToBuffer;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure WriteSQLData(Msg : String; DataType : TTraceFlag);
  end;

  { TReaderThread }

  TReaderThread = class(TThread)
  private
    { Private declarations }
    st : TTraceObject;
    FMonitors : TObjectList;
    FGlobalInterface: TGlobalInterface;
    FCriticalSection: TCriticalSection;
    procedure AlertMonitors;
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure ReadSQLData;
    procedure Execute; override;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure AddMonitor(Arg : TMDOCustomSQLMonitor);
    procedure RemoveMonitor(Arg : TMDOCustomSQLMonitor);
  end;


var
  FWriterThread : TWriterThread;
  FReaderThread : TReaderThread;
  _MonitorHook: TMDOSQLMonitorHook;
  bDone: Boolean;
  CS : TCriticalSection;

const
  ApplicationTitle: string = 'Unknown';

{ TMDOCustomSQLMonitor }

constructor TMDOCustomSQLMonitor.Create(AOwner: TComponent);
var aParent: TComponent;
begin
  inherited Create(AOwner);
  FTraceFlags := [tfqPrepare .. tfMisc];
  if not (csDesigning in ComponentState)  then
  begin
    aParent := AOwner;
    while aParent <> nil do
    begin
      if aParent is TCustomApplication then
      begin
        ApplicationTitle := TCustomApplication(aParent).Title;
        break;
      end;
      aParent := aParent.Owner;
    end;
    MonitorHook.RegisterMonitor(self);
  end;
  FEnabled := true;
end;

destructor TMDOCustomSQLMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled and assigned(_MonitorHook) then
      MonitorHook.UnregisterMonitor(self);
  end;
  inherited Destroy;
end;

procedure TMDOCustomSQLMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TMDOCustomSQLMonitor.ReleaseObject;
begin
  Free
end;

procedure TMDOCustomSQLMonitor.ReceiveMessage(Msg: TObject);
var
  st: TTraceObject;
begin
  st := (Msg as TTraceObject);
  if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
  st.Free;
end;

procedure TMDOCustomSQLMonitor.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if not (csDesigning in ComponentState) then
      if FEnabled then
        Monitorhook.RegisterMonitor(self)
      else
        MonitorHook.UnregisterMonitor(self);
  end;
end;

{ TIBSQLMonitorHook }

constructor TMDOSQLMonitorHook.Create;
begin
  inherited Create;
  FTraceFlags := [tfQPrepare..tfMisc];
  FEnabled := false;
end;

destructor TMDOSQLMonitorHook.Destroy;
begin
  if assigned(FGlobalInterface) then FGlobalInterface.Free;
  inherited Destroy;
end;

procedure TMDOSQLMonitorHook.DBConnect(db: TMDODatabase);
var
  st : String;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Connect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

procedure TMDOSQLMonitorHook.DBDisconnect(db: TMDODatabase);
var
  st: String;
begin
  if (Self = nil) then exit;
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Disconnect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

function TMDOSQLMonitorHook.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMDOSQLMonitorHook.GetMonitorCount: Integer;
begin
  Result := FGlobalInterface.MonitorCount
end;

function TMDOSQLMonitorHook.GetTraceFlags: TTraceFlags;
begin
  Result := FTraceFlags;
end;

procedure TMDOSQLMonitorHook.RegisterMonitor(SQLMonitor: TMDOCustomSQLMonitor);
begin
   {$IFDEF DEBUG}writeln('Register Monitor');{$ENDIF}
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
 if not Assigned(FReaderThread) then
    FReaderThread := TReaderThread.Create(FGlobalInterface);
  FReaderThread.AddMonitor(SQLMonitor);
end;

procedure TMDOSQLMonitorHook.ReleaseMonitor(Arg: TMDOCustomSQLMonitor);
begin
  FWriterThread.ReleaseMonitor(Arg);
end;

procedure TMDOSQLMonitorHook.SendMisc(Msg: String);
begin
  if FEnabled then
  begin
    WriteSQLData(Msg, tfMisc);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceAttach(service: TMDOCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Attach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceDetach(service: TMDOCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Detach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceQuery(service: TMDOCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Query]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.ServiceStart(service: TMDOCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Start]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TMDOSQLMonitorHook.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
  if (not FEnabled) and (Assigned(FWriterThread)) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

procedure TMDOSQLMonitorHook.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value
end;

procedure TMDOSQLMonitorHook.ForceRelease;
begin
    if Assigned(FReaderThread) then
    begin
      FReaderThread.Terminate;
      if not Assigned(FWriterThread) then
        FWriterThread := TWriterThread.Create(FGlobalInterface);
      FWriterThread.WriteSQLData(' ', tfMisc);
    end;
end;

procedure TMDOSQLMonitorHook.SQLExecute(qry: TMDOSQL);
var
  st: String;
  i: Integer;
begin
  if FEnabled then
  begin
    if not ((tfQExecute in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags)) ) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Execute] ' + qry.SQL.Text; {do not localize}
    if qry.Params.Count > 0 then begin
      for i := 0 to qry.Params.Count - 1 do begin
        st := st + CRLF + '  ' + qry.Params[i].Name + ' = ';
        try
          if qry.Params[i].IsNull then
            st := st + '<NULL>'; {do not localize}
          st := st + qry.Params[i].AsString;
        except
          st := st + '<' + SCantPrintValue + '>';
        end;
      end;
    end;
    WriteSQLData(st, tfQExecute);
  end;
end;

procedure TMDOSQLMonitorHook.SQLFetch(qry: TMDOSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQFetch in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Fetch] ' + qry.SQL.Text; {do not localize}
    if (qry.EOF) then
      st := st + CRLF + '  ' + SEOFReached;
    WriteSQLData(st, tfQFetch);
  end;
end;

procedure TMDOSQLMonitorHook.SQLPrepare(qry: TMDOSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQPrepare in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TMDOCustomDataSet then
      st := TMDOCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Prepare] ' + qry.SQL.Text + CRLF; {do not localize}
    st := st + '  Plan: ' + qry.Plan; {do not localize}
    WriteSQLData(st, tfQPrepare);
  end;
end;

procedure TMDOSQLMonitorHook.TRCommit(tr: TMDOTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit (Hard commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRCommitRetaining(tr: TMDOTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit retaining (Soft commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRRollback(tr: TMDOTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRRollbackRetaining(tr: TMDOTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback retaining (Soft rollback)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.TRStart(tr: TMDOTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Start transaction]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TMDOSQLMonitorHook.UnregisterMonitor(SQLMonitor: TMDOCustomSQLMonitor);
var
  Created : Boolean;
begin
{$IFDEF DEBUG}writeln('Unregister Monitor');{$ENDIF}
  if assigned(FReaderThread) then
  begin
    FReaderThread.RemoveMonitor(SQLMonitor);
    if FReaderThread.FMonitors.Count = 0 then
    begin
      FReaderThread.Terminate;

      { There is a possibility of a reader thread, but no writer one.
        When in that situation, the reader needs to be released after
        the terminate is set.  To do that, create a Writer thread, send
        the release code (a string of ' ' and type tfMisc) and then free
        it up. }

      Created := false;
      if not Assigned(FWriterThread) then
      begin
        FWriterThread := TWriterThread.Create(FGlobalInterface);
        Created := true;
      end;
      FWriterThread.WriteSQLData(' ', tfMisc);
      {$IFDEF DEBUG}writeln('Wait for read Terminate');{$ENDIF}
      FReaderThread.WaitFor;
      if assigned(FReaderThread.FatalException) then
        MDOError(mdoeThreadFailed,['Reader',Exception(FReaderThread.FatalException).Message]);
      {$IFDEF DEBUG}writeln('Freeing Reader Thread');{$ENDIF}
      FreeAndNil(FReaderThread);
      {$IFDEF DEBUG}writeln('Reader Thread Freed');{$ENDIF}
      if Created then
      begin
        FWriterThread.Terminate;
        {$IFDEF DEBUG}writeln('Wait for write Terminate');{$ENDIF}
        FWriterThread.WaitFor;
        if assigned(FWriterThread.FatalException) then
          MDOError(mdoeThreadFailed,['Writer',Exception(FWriterThread.FatalException).Message]);
        FreeAndNil(FWriterThread);
      end;
    end;
  end;
  {$IFDEF DEBUG}writeln('Unregister done'){$ENDIF}
end;

procedure TMDOSQLMonitorHook.WriteSQLData(Text: String;
  DataType: TTraceFlag);
begin
 {$IFDEF DEBUG}writeln('Write SQL Data: '+Text);{$ENDIF}
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
  Text := CRLF + '[Application: ' + ApplicationTitle + ']' + CRLF + Text; {do not localize}
  if not Assigned(FWriterThread) then
    FWriterThread := TWriterThread.Create(FGLobalInterface);
  FWriterThread.WriteSQLData(Text, DataType);
end;

{ TWriterThread }

constructor TWriterThread.Create(GlobalInterface: TGlobalInterface);

begin
  inherited Create(true);
  {$IFDEF DEBUG}writeln('Write Object Created');{$ENDIF}
  FGlobalInterface := GlobalInterface;
  FMsgs := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
  FMsgAvailable := TEventObject.Create(FGlobalInterface.Sa,true,false,cWriteMessageAvailable);
  Resume;
end;

destructor TWriterThread.Destroy;
begin
  if assigned(FMsgs) then FMsgs.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FMsgAvailable) then FMsgAvailable.Free;
  inherited Destroy;
end;

procedure TWriterThread.Execute;
begin
{$IFDEF DEBUG}writeln('Write Thread starts');{$ENDIF}
 try
  { Place thread code here }
  while ((not Terminated) and (not bDone)) or
        (FMsgs.Count <> 0) do
  begin
    FMsgAvailable.WaitFor(cMsgWaitTime);
    { Any one listening? }
    if FGlobalInterface.MonitorCount = 0 then
    begin
      if FMsgs.Count <> 0 then
      begin
        {$IFDEF DEBUG}writeln('Write Thread Drop Message');{$ENDIF}
        RemoveFromList;
      end;
    end
    else
      { Anything to process? }
      if FMsgs.Count <> 0 then
      begin
       { If the current queued message is a release release the object }
        if FMsgs.Items[0] is TReleaseObject then
        begin
          {$IFDEF DEBUG}writeln('Post Release');{$ENDIF}
          Synchronize(@PostRelease);
        end
        else
        { Otherwise write the TraceObject to the buffer }
        begin
          WriteToBuffer;
        end;
      end
      else
      begin
        FCriticalSection.Enter;
        try
          if FMsgs.Count = 0 then
          FMsgAvailable.ResetEvent
        finally
          FCriticalSection.Leave
        end;
      end;
  end;
 except on E: Exception do
   begin
     {$IFDEF DEBUG}writeln('Write Thread raised Exception: ' + E.Message);{$ENDIF}
     raise
   end
  end;
  {$IFDEF DEBUG}writeln('Write Thread Ends');{$ENDIF}
end;

procedure TWriterThread.WriteSQLData(Msg : String; DataType: TTraceFlag);
begin
  FCriticalSection.Enter;
  try
    FMsgs.Add(TTraceObject.Create(Msg, DataType));
  finally
    FCriticalSection.Leave;
  end;
  FMsgAvailable.SetEvent
end;

procedure TWriterThread.BeginWrite;
begin
{$IFDEF DEBUG}writeln('Begin Write');{$ENDIF}
  with FGlobalInterface do
  begin
    ReadReadyEvent.PassThroughGate;    {Wait for readers to become ready }
    WriterBusyEvent.Lock;     {Set Busy State}
  end;
{$IFDEF DEBUG}writeln('Begin Write Complete');{$ENDIF}
end;

procedure TWriterThread.EndWrite;
begin
  {$IFDEF DEBUG}writeln('End Write');{$ENDIF}
  with FGlobalInterface do
  begin
    DataAvailableEvent.Unlock;   { Signal Data Available. }
    ReadFinishedEvent.PassThroughGate; {Wait for readers to finish }
    DataAvailableEvent.Lock;  {reset Data Available }
    WriterBusyEvent.Unlock;      {Signal not Busy }
  end;
  {$IFDEF DEBUG}writeln('End Write Complete');{$ENDIF}
  end;

procedure TWriterThread.WriteToBuffer;
var I, len: integer;
    Temp: TTraceObject;
begin
  {$IFDEF DEBUG}writeln('Write to Buffer');{$ENDIF}
  FGlobalInterface.WriteLock.Lock;
  try
    { If there are no monitors throw out the message
      The alternative is to have messages queue up until a
      monitor is ready.}

    if FGlobalInterface.MonitorCount = 0 then
      RemoveFromList
    else
    begin
      i := 1;
      len := Length(TTraceObject(FMsgs[0]).FMsg);
      if len <= FGlobalInterface.MaxBufferSize then
      begin
        BeginWrite;
        try
          FGlobalInterface.SendTrace(TTraceObject(FMsgs[0]))
        finally
          RemoveFromList;
          EndWrite
        end;
      end
      else
      try
        while len > 0 do
        begin
          {$IFDEF DEBUG}writeln('Sending Partial Message, len = ',len);{$ENDIF}
          Temp := TTraceObject.Create(TTraceObject(FMsgs[0]),i,Min(len,FGlobalInterface.MaxBufferSize));
          try
            BeginWrite;
            FGlobalInterface.SendTrace(Temp);
            Inc(i,FGlobalInterface.MaxBufferSize);
            Dec(len,FGlobalInterface.MaxBufferSize);
          finally
            Temp.Free;
            EndWrite
          end
        end;
      finally
        RemoveFromList;
      end
    end;
  finally
    FGlobalInterface.WriteLock.Unlock;
  end;
  {$IFDEF DEBUG}writeln('Done Write');{$ENDIF}
end;

procedure TWriterThread.RemoveFromList;
begin
  {$IFDEF DEBUG}writeln('Write Thread: Remove object From List');{$ENDIF}
  FCriticalSection.Enter;
  try
    FMsgs.Remove(FMsgs[0]); { Pop the written item }
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWriterThread.PostRelease;
var Monitor: TMDOCustomSQLMonitor;
begin
  Monitor := TReleaseObject(FMsgs.Items[0]).FMonitor;
  Monitor.ReleaseObject
end;

procedure TWriterThread.ReleaseMonitor(Arg : TMDOCustomSQLMonitor);
begin
  FMsgs.Add(TReleaseObject.Create(Arg));
end;

{ TTraceObject }

constructor TTraceObject.Create(Msg : String; DataType: TTraceFlag);
begin
  FMsg := Msg;
  FDataType := DataType;
  FTimeStamp := Now;
end;

constructor TTraceObject.Create(obj: TTraceObject);
begin
  FMsg := obj.FMsg;
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
end;

constructor TTraceObject.Create(obj: TTraceObject; MsgOffset, MsgLen: integer);
begin
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
  FMsg := copy(obj.FMsg,MsgOffset,MsgLen)
end;

{ TReleaseObject }

constructor TReleaseObject.Create(Monitor : TMDOCustomSQLMonitor);
begin
  FMonitor := Monitor;
end;

{ ReaderThread }

procedure TReaderThread.AddMonitor(Arg: TMDOCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    if FMonitors.IndexOf(Arg) < 0 then
      FMonitors.Add(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

procedure TReaderThread.AlertMonitors;
var i : Integer;
    FTemp : TTraceObject;
    Monitor: TMDOCustomSQLMonitor;
begin
  for i := 0 to FMonitors.Count - 1 do
  begin
     {$IFDEF DEBUG}writeln('Sending Message to Monitor ' +IntToStr(i));{$ENDIF}
     FTemp := TTraceObject.Create(st);
     Monitor := TMDOCustomSQLMonitor(FMonitors[i]);
     Monitor.ReceiveMessage(FTemp);
  end;
end;

procedure TReaderThread.BeginRead;
begin
{$IFDEF DEBUG}writeln('Begin Read');{$ENDIF}
  with FGlobalInterface do
  begin
    WriterBusyEvent.PassthroughGate;     { Wait for Writer not busy}
    ReadFinishedEvent.Lock;          { Prepare Read Finished Gate}
    ReadReadyEvent.Unlock;                { Signal read ready  }
    {$IFDEF DEBUG}writeln('Read Ready Unlocked');{$ENDIF}
    DataAvailableEvent.PassthroughGate;  { Wait for a Data Available }
  end;
{$IFDEF DEBUG}writeln('Begin Read Complete');{$ENDIF}
end;

constructor TReaderThread.Create(GlobalInterface: TGlobalInterface);
begin
  inherited Create(true);
  FGlobalInterface := GlobalInterface;
  st := TTraceObject.Create('', tfMisc);
  FGlobalInterface.IncMonitorCount;
  FMonitors := TObjectList.Create(false);
  FCriticalSection := TCriticalSection.Create;
  {$IFDEF DEBUG}writeln('Reader Thread Created');{$ENDIF}
  FGlobalInterface.ReadReadyEvent.Lock;           { Initialise Read Ready}
  Resume;
end;

destructor TReaderThread.Destroy;
begin
{$IFDEF DEBUG}writeln('Reader Thread Destory');{$ENDIF}
  FGlobalInterface.ReadReadyEvent.UnLock;
  if assigned(FGlobalInterface) and (FGlobalInterface.MonitorCount > 0) then
     FGlobalInterface.DecMonitorCount;
  FMonitors.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  st.Free;
  inherited Destroy;
end;

procedure TReaderThread.EndRead;
begin
{$IFDEF DEBUG}writeln('End Read');{$ENDIF}
  FGlobalInterface.ReadReadyEvent.Lock;           { reset Read Ready}
  FGlobalInterface.ReadFinishedEvent.Unlock; {Signal Read completed }
  {$IFDEF DEBUG}writeln('End Read Complete');{$ENDIF}
end;

procedure TReaderThread.Execute;
begin
{$IFDEF DEBUG}writeln('Read Thread Starts');{$ENDIF}
  { Place thread code here }
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData;
    if (st.FMsg <> '') and
       not ((st.FMsg = ' ') and (st.FDataType = tfMisc)) then
    begin
      {$IFDEF DEBUG}writeln('Sending Message to Monitors');{$ENDIF}
      Synchronize(@AlertMonitors);
    end;
  end;
  {$IFDEF DEBUG}writeln('Read Thread Ends');{$ENDIF}
end;

procedure TReaderThread.ReadSQLData;
begin
  st.FMsg := '';
  BeginRead;
  if not bDone then
  try
    FGlobalInterface.ReceiveTrace(st)
  finally
    EndRead;
  end;
end;

procedure TReaderThread.RemoveMonitor(Arg: TMDOCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    FMonitors.Remove(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

{ Misc methods }

function MonitorHook: IIBSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
  begin
    CS.Enter;
    if (_MonitorHook = nil) and (not bDone) then
    begin
      _MonitorHook := TMDOSQLMonitorHook.Create;
      _MonitorHook._AddRef;
    end;
    CS.Leave;
  end;
  result := _MonitorHook;
end;

procedure EnableMonitoring;
begin
  MonitorHook.Enabled := True;
end;

procedure DisableMonitoring;
begin
  MonitorHook.Enabled := False;
end;

function MonitoringEnabled: Boolean;
begin
  result := MonitorHook.Enabled;
end;

procedure CloseThreads;
begin
{$IFDEF DEBUG}writeln('Closed Threads Called');{$ENDIF}
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Terminate;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end;
  if Assigned(FWriterThread) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

initialization
  CS := TCriticalSection.Create;
  _MonitorHook := nil;
  FWriterThread := nil;
  FReaderThread := nil;
  bDone := False;
{$IFDEF USE_SV5_IPC}
  if FpGetEnv('FBSQL_IPCFILENAME') <> nil then
    IPCFileName := strpas(FpGetEnv('FBSQL_IPCFILENAME'))
  else
    IPCFileName := GetTempDir(true) + IPCFileName + '.' + strpas(FpGetEnv('USER'));
{$ENDIF}

finalization
  {$IFDEF DEBUG}writeln('Entered Finalisation');{$ENDIF}
  try
    { Write an empty string to force the reader to unlock during termination }
    bDone := True;
    if Assigned(_MonitorHook) then
      _MonitorHook.ForceRelease;
    CloseThreads;
    if Assigned(_MonitorHook) then
      _MonitorHook._Release;

  finally
    _MonitorHook := nil;
    if assigned(CS) then CS.Free;
  end;
end.

