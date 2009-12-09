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

{$I ..\mdo.inc}

unit MDOBatchMove;

interface

uses
  {$IFNDEF MDO_FPC}
  Windows, StdVCL, Graphics, Controls,
  {$ENDIF}
  SysUtils, Classes, Db, MDO, MDOCustomDataSet, MDODatabase,
  MDOTable;

type
  TBatchMode = (batAppend, batUpdate, batAppendUpdate, batDelete, batCopy);

{ TBatchMove }

  TMDOBatchMove = class (TComponent)
  private
    FAbortOnKeyViol: Boolean;
    FAbortOnProblem: Boolean;
    FChangedCount: LongInt;
    FChangedTableName: TFileName;
    FCommitCount: Integer;
    FDestination: TMDOTable;
    FIBLoaded: Boolean;
    FKeyViolCount: LongInt;
    FKeyViolTableName: TFileName;
    FMappings: TStrings;
    FMode: TBatchMode;
    FMovedCount: LongInt;
    FProblemCount: LongInt;
    FProblemTableName: TFileName;
    FRecordCount: LongInt;
    FSource: TMDOCustomDataSet;
    FTransliterate: Boolean;
    procedure InternalExecute(BatchMode: TBatchMode; FieldCount: Word);
    procedure SetMappings(Value: TStrings);
    procedure SetSource(Value: TMDOCustomDataSet);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
            override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property ChangedCount: LongInt read FChangedCount;
    property KeyViolCount: LongInt read FKeyViolCount;
    property MovedCount: LongInt read FMovedCount;
    property ProblemCount: LongInt read FProblemCount;
  published
    property AbortOnKeyViol: Boolean read FAbortOnKeyViol write FAbortOnKeyViol 
            default True;
    property AbortOnProblem: Boolean read FAbortOnProblem write FAbortOnProblem 
            default True;
    property ChangedTableName: TFileName read FChangedTableName write 
            FChangedTableName;
    property CommitCount: Integer read FCommitCount write FCommitCount default 
            0;
    property Destination: TMDOTable read FDestination write FDestination;
    property KeyViolTableName: TFileName read FKeyViolTableName write 
            FKeyViolTableName;
    property Mappings: TStrings read FMappings write SetMappings;
    property Mode: TBatchMode read FMode write FMode default batAppend;
    property ProblemTableName: TFileName read FProblemTableName write 
            FProblemTableName;
    property RecordCount: LongInt read FRecordCount write FRecordCount default 
            0;
    property Source: TMDOCustomDataSet read FSource write SetSource;
    property Transliterate: Boolean read FTransliterate write FTransliterate 
            default True;
  end;
  
implementation

uses MDOIntf;

{ TMDOBatchMove }

{
******************************** TMDOBatchMove *********************************
}
constructor TMDOBatchMove.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckFBLoaded;
  FIBLoaded := True;
  FAbortOnKeyViol := True;
  FAbortOnProblem := True;
  FTransliterate := True;
  FMappings := TStringList.Create;
end;

destructor TMDOBatchMove.Destroy;
begin
  if FIBLoaded then
  FMappings.Free;
  inherited Destroy;
end;

procedure TMDOBatchMove.Execute;
  
  type
    TFieldMap = array of Word;
  var
    SourceActive, DestinationActive: Boolean;
    BatchMode: TBatchMode;
    I: Integer;
    FieldCount: Word;
    FieldMap: TFieldMap;
    DestName, SourceName: string;
  
    procedure GetMappingNames;
    var
      P: Integer;
      Mapping: string;
    begin
      Mapping := FMappings[I];
      P := Pos('=', Mapping); {MBCS OK}
      if P > 0 then
      begin
        DestName := Copy(Mapping, 1, P - 1);
        SourceName := Copy(Mapping, P + 1, 255);
      end
      else begin
        DestName := Mapping;
        SourceName := Mapping;
      end;
    end;
  
begin
  if (Destination = nil) or (Source = nil) or (Destination = Source) then
    MDOError(mdoeInvalidBatchMove, [nil]);
  SourceActive := Source.Active;
  DestinationActive := Destination.Active;
  FieldCount := 0;
  FieldMap := nil;
  try
    Source.DisableControls;
    Destination.DisableControls;
    Source.Open;
    Source.CheckBrowseMode;
    Source.UpdateCursorPos;
    BatchMode := FMode;
    if BatchMode = batCopy then
    begin
      Destination.Close;
      if FMappings.Count = 0 then
        Destination.FieldDefs := Source.FieldDefs
      else
      begin
        Destination.FieldDefs.Clear;
        for I := 0 to FMappings.Count - 1 do
        begin
          GetMappingNames;
          with Source.FieldDefs.Find(SourceName) do
            Destination.FieldDefs.Add(DestName, DataType, Size, Required);
        end;
      end;
      Destination.IndexDefs.Clear;
      Destination.CreateTable;
      BatchMode := batAppend;
    end;
    Destination.Open;
    Destination.CheckBrowseMode;
    if FMappings.Count <> 0 then
    begin
      FieldCount := Destination.FieldDefs.Count;
      SetLength(FieldMap, FieldCount);
      for I := 0 to FMappings.Count - 1 do
      begin
        GetMappingNames;
        FieldMap[Destination.FieldDefs.Find(DestName).FieldNo-1] :=
          Source.FieldDefs.Find(SourceName).FieldNo;
      end;
    end;
    if FRecordCount > 0 then
    begin
      Source.UpdateCursorPos;
      FMovedCount := FRecordCount;
    end else
    begin
      FMovedCount := MaxLongint;
    end;
    Source.CursorPosChanged;
    try
      InternalExecute (BatchMode, FieldCount);
    finally
      if DestinationActive then Destination.First;
    end;
  finally
    if not DestinationActive then
      Destination.Close;
    if not SourceActive then
      Source.Close;
    Destination.EnableControls;
    Source.EnableControls;
  end;
end;

procedure TMDOBatchMove.InternalExecute(BatchMode: TBatchMode; FieldCount: 
        Word);
begin
  
end;

procedure TMDOBatchMove.Notification(AComponent: TComponent; Operation: 
        TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Destination = AComponent then
      Destination := nil;
    if Source = AComponent then
      Source := nil;
  end;
end;

procedure TMDOBatchMove.SetMappings(Value: TStrings);
begin
  FMappings.Assign(Value);
end;

procedure TMDOBatchMove.SetSource(Value: TMDOCustomDataSet);
begin
  FSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;


end.
