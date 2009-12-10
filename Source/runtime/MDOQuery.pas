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

unit MDOQuery;

{$I ..\MDO.inc}

interface

uses
  {$IFNDEF MDO_FPC}
  Windows, StdVCL, Graphics, Controls,
  {$ENDIF}
  SysUtils, Classes, Db, MDOHeader, MDO, MDOCustomDataSet, MDOSQL;

type

{ TMDOQuery }

  TMDOQuery = class (TMDOCustomDataSet)
  private
    FCheckRowsAffected: Boolean;
    FGenerateParamNames: Boolean;
    FParams: TParams;
    FPrepared: Boolean;
    FRowsAffected: Integer;
    FSQL: TStrings;
    FText: string;
    function GetRowsAffected: Integer;
    function GetStmtHandle: TISC_STMT_HANDLE;
    procedure PrepareSQL(Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure SetParamsList(Value: TParams);
    procedure SetPrepare(Value: Boolean);
    procedure SetPrepared(Value: Boolean);
    procedure SetQuery(Value: TStrings);
    procedure WriteParamData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Disconnect; override;
    function GenerateQueryForLiveUpdate: Boolean;
    function GetParamsCount: Word;
    procedure InitFieldDefs; override;
    procedure InternalOpen; override;
{$IFNDEF MDO_FPC}
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
{$ENDIF}
    procedure SetFiltered(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BatchInput(InputObject: TMDOBatchInput);
    procedure BatchOutput(OutputObject: TMDOBatchOutput);
    procedure ExecSQL;
{$IFNDEF MDO_FPC}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
{$ENDIF}
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    property GenerateParamNames: Boolean read FGenerateParamNames write 
            FGenerateParamNames;
    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property RowsAffected: Integer read GetRowsAffected;
    property StatementType;
    property StmtHandle: TISC_STMT_HANDLE read GetStmtHandle;
    property Text: string read FText;
  published
    property Active;
    property AfterDatabaseDisconnect;
    property AfterTransactionEnd;
    property BeforeDatabaseDisconnect;
    property BeforeTransactionEnd;
    property BufferChunks;
    property CachedUpdates;
    property Constraints stored ConstraintsStored;
    property DatabaseFree;
    property DataSource read GetDataSource write SetDataSource;
    property Filtered;
    property GeneratorLink;
    property LoadDefaults;
    property OnFilterRecord;
    property ParamCheck;
    property Params: TParams read FParams write SetParamsList stored False;
    property SQL: TStrings read FSQL write SetQuery;
    property TransactionFree;
    property UniDirectional default False;
    property UpdateObject;
  end;
  
implementation

{ TMDOQuery }

{
********************************** TMDOQuery ***********************************
}
constructor TMDOQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create(Self);
  ParamCheck := True;
  FGenerateParamNames := False;
  FRowsAffected := -1;
end;

destructor TMDOQuery.Destroy;
begin
  Destroying;
  Disconnect;
  SQL.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TMDOQuery.BatchInput(InputObject: TMDOBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TMDOQuery.BatchOutput(OutputObject: TMDOBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TMDOQuery.DefineProperties(Filer: TFiler);
  
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TMDOQuery(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;
  
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TMDOQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TMDOQuery.ExecSQL;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if SQL.Count <= 0 then
  begin
    FCheckRowsAffected := False;
    MDOError(mdoeEmptySQLStatement, [nil]);
  end;
  ActivateConnection();
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then SetParamsFromCursor;
    if FParams.Count > 0 then SetParams;
    InternalExecQuery;
  finally
    if DidActivate then
      DeactivateTransaction;
    FCheckRowsAffected := True;
  end;
end;

function TMDOQuery.GenerateQueryForLiveUpdate: Boolean;
begin
  Result := False;
end;

{$IFNDEF MDO_FPC}
procedure TMDOQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);
  
    function AddFieldToList(const FieldName: string; DataSet: TDataSet;
      List: TList): Boolean;
    var
      Field: TField;
    begin
      Field := DataSet.FindField(FieldName);
      if (Field <> nil) then
        List.Add(Field);
      Result := Field <> nil;
    end;
  
  var
    i: Integer;
  
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;
{$ENDIF}

function TMDOQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TMDOQuery.GetRowsAffected: Integer;
begin
  Result := -1;
  if Prepared then
   Result := QSelect.RowsAffected
end;

function TMDOQuery.GetStmtHandle: TISC_STMT_HANDLE;
begin
  Result := SelectStmtHandle;
end;

procedure TMDOQuery.InitFieldDefs;
begin
  inherited InitFieldDefs;
end;

procedure TMDOQuery.InternalOpen;
begin
  ActivateConnection();
  ActivateTransaction;
  QSelect.GenerateParamNames := FGenerateParamNames;
  SetPrepared(True);
  if DataSource <> nil then
    SetParamsFromCursor;
  SetParams;
  inherited InternalOpen;
end;

function TMDOQuery.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TMDOQuery.Prepare;
begin
  SetPrepared(True);
end;

procedure TMDOQuery.PrepareSQL(Value: PChar);
begin
  QSelect.GenerateParamNames := FGenerateParamNames;
  InternalPrepare;
end;

{$IFNDEF MDO_FPC}
procedure TMDOQuery.PSExecute;
begin
  ExecSQL;
end;

function TMDOQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

function TMDOQuery.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

procedure TMDOQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

procedure TMDOQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;
{$ENDIF}

procedure TMDOQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
begin
  if not (csReading in ComponentState) then
  begin
    Disconnect;
    if ParamCheck or (csDesigning in ComponentState) then
    begin
      List := TParams.Create(Self);
      try
        FText := List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end else
      FText := SQL.Text;
    DataEvent(dePropertyChange, 0);
  end else
    FText := FParams.ParseSQL(SQL.Text, False);
  SelectSQL.Assign(SQL);
end;

procedure TMDOQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TMDOQuery.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

procedure TMDOQuery.SetParams;
var
  i: Integer;
  Buffer: Pointer;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    if Params[i].IsNull then
      SQLParams[i].IsNull := True
    else begin
      SQLParams[i].IsNull := False;
      case Params[i].DataType of
        ftBytes:
        begin
          GetMem(Buffer,Params[i].GetDataSize);
          try
            Params[i].GetData(Buffer);
            SQLParams[i].AsPointer := Buffer;
          finally
            FreeMem(Buffer);
          end;
        end;
        ftString:
          SQLParams[i].AsString := Params[i].AsString;
        ftBoolean, ftSmallint, ftWord:
          SQLParams[i].AsShort := Params[i].AsSmallInt;
        ftInteger:
          SQLParams[i].AsLong := Params[i].AsInteger;
        ftLargeInt:
        {$IFDEF MDO_DELPHI5}
          SQLParams[i].AsInt64 := StrToInt64(Params[i].Value);
        {$ENDIF}
        {$IFDEF MDO_DELPHI6_UP}
          SQLParams[i].AsInt64 := Params[i].Value;
        {$ENDIF}
        ftFloat:
         SQLParams[i].AsDouble := Params[i].AsFloat;
        ftBCD, ftCurrency:
          SQLParams[i].AsCurrency := Params[i].AsCurrency;
        ftDate:
          SQLParams[i].AsDate := Params[i].AsDateTime;
        ftTime:
          SQLParams[i].AsTime := Params[i].AsDateTime;
        ftDateTime:
          SQLParams[i].AsDateTime := Params[i].AsDateTime;
        ftBlob, ftMemo:
          SQLParams[i].AsString := Params[i].AsString;
        else
          MDOError(mdoeNotSupported, [nil]);
      end;
    end;
  end;
end;

procedure TMDOQuery.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
  
  procedure CheckRequiredParams;
  var
    I: Integer;
  begin
    for I := 0 to FParams.Count - 1 do
    with FParams[I] do
      if not Bound then
        MDOError(mdoeRequiredParamNotSet, [nil]);
  end;
  
begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if not Bound then
          begin
            AssignField(DataSet.FieldByName(Name));
            Bound := False;
          end;
    end
    else
      CheckRequiredParams;
  end
  else
    CheckRequiredParams;
end;

procedure TMDOQuery.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

procedure TMDOQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TMDOQuery.SetPrepared(Value: Boolean);
begin
  CheckDatasetClosed;
  if Value <> Prepared then
  begin
    if Value then
    begin
      FRowsAffected := -1;
      FCheckRowsAffected := True;
      if Length(Text) > 1 then PrepareSQL(PChar(Text))
      else MDOError(mdoeEmptySQLStatement, [nil]);
    end
    else
    begin
      if FCheckRowsAffected then
        FRowsAffected := RowsAffected;
      InternalUnPrepare;
    end;
    FPrepared := Value;
  end;
end;

procedure TMDOQuery.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    Disconnect;
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

procedure TMDOQuery.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TMDOQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;




{ TMDOQuery IProviderSupport }

end.

