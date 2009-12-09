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

unit MDOTable;

interface

uses SysUtils, Classes, DB, MDO, MDODatabase, MDOCustomDataSet, MDOHeader,
  MDOSQL, MDOUtils;

type

{ TMDOTable }

  TMDOTableType = (ttSystem, ttView);
  TMDOTableTypes = set of TMDOTableType;
  TIndexName = String;

  TMDOTable = class;

  TMDOTable = class (TMDOCustomDataSet)
  private
    FDefaultIndex: Boolean;
    FDetailFieldsList: TStringList;
    FFieldsIndex: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexName: TIndexName;
    FMasterFieldsList: TStringList;
    FMasterLink: TMasterDataLink;
    FMultiTableView: Boolean;
    FNameList: TStrings;
    FPrimaryIndexFields: string;
    FReadOnly: Boolean;
    FRegenerateSQL: Boolean;
    FStoreDefs: Boolean;
    FSwitchingIndex: Boolean;
    FSystemTable: Boolean;
    FTableName: string;
    FTableTypes: TMDOTableTypes;
    WhereAllRefreshSQL: TStrings;
    WhereDBKeyRefreshSQL: TStrings;
    WherePrimaryRefreshSQL: TStrings;
    procedure ExtractLinkFields;
    function FieldDefsStored: Boolean;
    function FormatFieldsList(Value: string): string;
    procedure GenerateSQL;
    procedure GenerateUpdateSQL;
    function GetCurrentDBKey: TFBDBKey;
    function GetExists: Boolean;
    function GetIndexField(Index: Integer): TField;
    function GetIndexFieldCount: Integer;
    function GetIndexFieldNames: string;
    function GetIndexName: string;
    function GetMasterFields: string;
    function GetTableNames: TStrings;
    procedure GetTableNamesFromServer;
    function IndexDefsStored: Boolean;
    function InternalGetUpdatable: Boolean;
    function InternalGotoDBKey(DBKey: TFBDBKey): Boolean;
    procedure InternalTableRefresh;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure Reopen;
    procedure ResetSQLStatements;
    procedure SetDataSource(Value: TDataSource);
    procedure SetIndex(const Value: string; FieldsIndex: Boolean);
    procedure SetIndexDefs(Value: TIndexDefs);
    procedure SetIndexField(Index: Integer; Value: TField);
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetMasterFields(const Value: string);
    procedure SetParams;
    procedure SetReadOnly(Value: Boolean);
    procedure SetTableName(Value: string);
    procedure SetTableTypes(const Value: TMDOTableTypes);
    procedure SwitchToIndex;
  protected
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
{$IFNDEF MDO_FPC}
    procedure DefChanged(Sender: TObject); override;
{$ENDIF}
    procedure DoOnNewRecord; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    procedure GetIndexParams(const IndexName: string; FieldsIndex: Boolean; var 
            IndexedName: string);
    procedure InitFieldDefs; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure InternalRefreshRow; override;
{$IFNDEF MDO_FPC}
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    function PSGetKeyFields: string; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
{$ENDIF}
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure UpdateIndexDefs; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; 
            const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteIndex(const Name: string);
    procedure DeleteTable;
    procedure EmptyTable;
{$IFNDEF MDO_FPC}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
{$ENDIF}
    procedure GetIndexNames(List: TStrings);
    procedure GotoCurrent(Table: TMDOTable);
    property CurrentDBKey: TFBDBKey read GetCurrentDBKey;
    property Exists: Boolean read GetExists;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write 
            SetIndexField;
    property TableNames: TStrings read GetTableNames;
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
    property DefaultIndex: Boolean read FDefaultIndex write FDefaultIndex 
            default True;
    property FieldDefs stored FieldDefsStored;
    property Filter;
    property Filtered;
    property IndexDefs: TIndexDefs read FIndexDefs write SetIndexDefs stored 
            IndexDefsStored;
    property IndexFieldNames: string read GetIndexFieldNames write 
            SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property OnFilterRecord;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
    property TableName: string read FTableName write SetTableName;
    property TableTypes: TMDOTableTypes read FTableTypes write SetTableTypes 
            default [];
    property TransactionFree;
    property UniDirectional;
    property UpdateObject;
  end;
  
implementation

{ TMDOTable }

{
********************************** TMDOTable ***********************************
}
constructor TMDOTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameList := TStringList.Create;
  FSwitchingIndex := False;
  FIndexDefs := TIndexDefs.Create(Self);
  WhereAllRefreshSQL := TStringList.Create;
  WhereDBKeyRefreshSQL := TStringList.Create;
  WherePrimaryRefreshSQL := TStringList.Create;
  FDefaultIndex := True;
  FRegenerateSQL := True;
  FMasterFieldsList := TStringList.Create;
  FDetailFieldsList := TStringList.Create;
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  QRefresh.OnSQLChanging := nil;
  QDelete.OnSQLChanging := nil;
  QInsert.OnSQLChanging := nil;
  QModify.OnSQLChanging := nil;
end;

destructor TMDOTable.Destroy;
begin
  FreeAndNil(FNameList);
  FreeAndNil(FIndexDefs);
  FreeAndNil(FMasterFieldsList);
  FreeAndNil(FDetailFieldsList);
  FreeAndNil(FMasterLink);
  FreeAndNil(WhereAllRefreshSQL);
  FreeAndNil(WhereDBKeyRefreshSQL);
  FreeAndNil(WherePrimaryRefreshSQL);
  inherited Destroy;
end;

procedure TMDOTable.AddIndex(const Name, Fields: string; Options: TIndexOptions;
        const DescFields: string = '');
var
  Query: TMDOSQL;
  FieldList: string;
begin
  FieldDefs.Update;
  if Active then begin
    CheckBrowseMode;
    CursorPosChanged;
  end;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Transaction;
    FieldList := FormatFieldsList(Fields);
    if (ixPrimary in Options) then
    begin
     Query.SQL.Text := 'Alter Table ' + {do not localize}
       QuoteIdentifier(Database.SQLDialect, FTableName) +
       ' Add CONSTRAINT ' +   {do not localize}
       QuoteIdentifier(Database.SQLDialect, Name)
       + ' Primary Key (' + {do not localize}
       FormatFieldsList(Fields) +
       ')';
    end
    else if ([ixUnique, ixDescending] * Options = [ixUnique, ixDescending]) then
      Query.SQL.Text := 'Create unique Descending Index ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, Name) +
                        ' on ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, FTableName) +
                        ' (' + FieldList + ')'
    else if (ixUnique in Options) then
      Query.SQL.Text := 'Create unique Index ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, Name) +
                        ' on ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, FTableName) +
                        ' (' + FieldList + ')'
    else if (ixDescending in Options) then
      Query.SQL.Text := 'Create Descending Index ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, Name) +
                        ' on ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, FTableName) +
                        ' (' + FieldList + ')'
    else
      Query.SQL.Text := 'Create Index ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, Name) +
                        ' on ' + {do not localize}
                        QuoteIdentifier(Database.SQLDialect, FTableName) +
                        ' (' + FieldList + ')';
    Query.Prepare;
    Query.ExecQuery;
    IndexDefs.Updated := False;
  finally
    Query.free
  end;
end;

procedure TMDOTable.CreateTable;
var
  FieldList: string;
  
  procedure InitFieldsList;
  var
    I: Integer;
  begin
    InitFieldDefsFromFields;
    for I := 0 to FieldDefs.Count - 1 do begin
      if ( I > 0) then
        FieldList := FieldList + ', ';
      with FieldDefs[I] do
      begin
        case DataType of
          ftString:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' VARCHAR(' + IntToStr(Size) + ')'; {do not localize}
          ftFixedChar:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' CHAR(' + IntToStr(Size) + ')'; {do not localize}
          ftBoolean, ftSmallint, ftWord:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' SMALLINT'; {do not localize}
          ftInteger:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' INTEGER'; {do not localize}
          ftFloat, ftCurrency:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' DOUBLE PRECISION'; {do not localize}
          ftBCD: begin
            if (Database.SQLDialect = 1) then begin
              if (Precision > 9) then
                MDOError(mdoeFieldUnsupportedType,[nil]);
              if (Precision <= 4) then
                Precision := 9;
            end;
            if (Precision <= 4 ) then
              FieldList := FieldList +
                QuoteIdentifier(DataBase.SQLDialect, Name) +
                ' Numeric(18, 4)' {do not localize}
            else
              FieldList := FieldList +
                QuoteIdentifier(DataBase.SQLDialect, Name) +
                ' Numeric(' + IntToStr(Precision) + ', 4)'; {do not localize}
          end;
          ftDate:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' DATE'; {do not localize}
          ftTime:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' TIME'; {do not localize}
          ftDateTime:
            if (Database.SQLDialect = 1) then
              FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' DATE' {do not localize}
            else
              FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' TIMESTAMP'; {do not localize}
          ftLargeInt:
            if (Database.SQLDialect = 1) then
              MDOError(mdoeFieldUnsupportedType,[nil])
            else
              FieldList := FieldList +
                QuoteIdentifier(DataBase.SQLDialect, Name) +
                ' Numeric(18, 0)'; {do not localize}
          ftBlob, ftMemo:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' BLOB SUB_TYPE 1'; {do not localize}
          ftBytes, ftVarBytes, ftGraphic..ftTypedBinary:
            FieldList := FieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) +
              ' BLOB SUB_TYPE 0'; {do not localize}
          ftUnknown, ftADT, ftArray, ftReference, ftDataSet,
          ftCursor, ftWideString, ftAutoInc:
            MDOError(mdoeFieldUnsupportedType,[nil]);
          else
            MDOError(mdoeFieldUnsupportedType,[nil]);
        end;
        if faRequired in Attributes then
          FieldList := FieldList + ' NOT NULL'; {do not localize}
      end;
    end;
  end;
  
  procedure InternalCreateTable;
  var
    I: Integer;
    Query: TMDOSQL;
  begin
    if (FieldList = '') then
      MDOError(mdoeFieldUnsupportedType,[nil]);
    Query := TMDOSQL.Create(self);
    try
      Query.Database := Database;
      Query.transaction := Transaction;
      Query.SQL.Text := 'Create Table ' +
        QuoteIdentifier(DataBase.SQLDialect, FTableName) +
        ' (' + FieldList; {do not localize}
      for I := 0 to IndexDefs.Count - 1 do
      with IndexDefs[I] do
        if ixPrimary in Options then
        begin
          Query.SQL.Text := Query.SQL.Text + ', CONSTRAINT ' +
            QuoteIdentifier(DataBase.SQLDialect, Name) +
            ' Primary Key (' +
            FormatFieldsList(Fields) +
            ')';
        end;
      Query.SQL.Text := Query.SQL.Text + ')';
      Query.Prepare;
      Query.ExecQuery;
    finally
      Query.Free;
    end;
  end;
  
  procedure InternalCreateIndex;
  var
    I: Integer;
  begin
    for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if not (ixPrimary in Options) then
        AddIndex(Name, Fields, Options);
  end;
  
begin
  CheckInactive;
  InitFieldsList;
  InternalCreateTable;
  InternalCreateIndex;
end;

procedure TMDOTable.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if Event = dePropertyChange then begin
    if assigned(IndexDefs) then
      IndexDefs.Updated := False;
    FRegenerateSQL := True;
  end;
  inherited DataEvent(Event, Info);
end;

{$IFNDEF MDO_FPC}
procedure TMDOTable.DefChanged(Sender: TObject);
begin
  StoreDefs := True;
end;
{$ENDIF}

procedure TMDOTable.DeleteIndex(const Name: string);
var
  Query: TMDOSQL;
  
  procedure DeleteByIndex;
  begin
    Query := TMDOSQL.Create(self);
    try
      Query.Database := DataBase;
      Query.Transaction := Transaction;
      Query.SQL.Text := 'Drop index ' +  {do not localize}
                         QuoteIdentifier(Database.SQLDialect, Name);
      Query.Prepare;
      Query.ExecQuery;
      IndexDefs.Updated := False;
    finally
      Query.Free;
    end;
  end;
  
  function DeleteByConstraint: Boolean;
  begin
    Result := False;
    Query := TMDOSQL.Create(self);
    try
      Query.Database := DataBase;
      Query.Transaction := Transaction;
      Query.SQL.Text := 'Select ''foo'' from RDB$RELATION_CONSTRAINTS ' +
        'where RDB$RELATION_NAME = ' +
        '''' +
        FormatIdentifierValue(Database.SQLDialect,
          QuoteIdentifier(DataBase.SQLDialect, FTableName)) +
        ''' ' +
        ' AND RDB$CONSTRAINT_NAME = ' +
        '''' +
        FormatIdentifierValue(Database.SQLDialect,
          QuoteIdentifier(DataBase.SQLDialect, Name)) +
        ''' ' +
        'AND RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''';
      Query.Prepare;
      Query.ExecQuery;
      if not Query.EOF then
      begin
        Query.Close;
        Query.SQL.Text := 'Alter Table ' +  {do not localize}
          QuoteIdentifier(DataBase.SQLDialect, FTableName) +
          ' Drop Constraint ' +
          QuoteIdentifier(DataBase.SQLDialect, Name);
        Query.Prepare;
        Query.ExecQuery;
        IndexDefs.Updated := False;
        Result := True;
      end;
    finally
      Query.Free;
    end;
  end;
  
  procedure DeleteByKey;
  begin
    Query := TMDOSQL.Create(self);
    try
      Query.Database := DataBase;
      Query.Transaction := Transaction;
      Query.SQL.Text := 'Select RDB$CONSTRAINT_NAME from RDB$RELATION_CONSTRAINTS ' +
        'where RDB$RELATION_NAME = ' +
        '''' +
        FormatIdentifierValue(Database.SQLDialect,
          QuoteIdentifier(DataBase.SQLDialect, FTableName)) +
        ''' ' +
        'AND RDB$INDEX_NAME = ' +
        '''' +
        FormatIdentifierValue(Database.SQLDialect,
          QuoteIdentifier(DataBase.SQLDialect, Name)) +
        ''' ' +
        'AND RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''';
      Query.Prepare;
      Query.ExecQuery;
      if not Query.EOF then
      begin
        Query.Close;
        Query.SQL.Text := 'Alter Table ' +  {do not localize}
          QuoteIdentifier(DataBase.SQLDialect, FTableName) +
          ' Drop Constraint ' +
          QuoteIdentifier(DataBase.SQLDialect, Query.Current.ByName('RDB$CONSTRAINT_NAME').AsString);
        Query.Prepare;
        Query.ExecQuery;
        IndexDefs.Updated := False;
      end;
    finally
      Query.Free;
    end;
  end;
  
begin
  if Active then
    CheckBrowseMode;
  IndexDefs.Update;
  if (Pos('RDB$PRIMARY', Name) <> 0 ) then {do not localize} {mbcs ok}
    DeleteByKey
  else if not DeleteByConstraint then
    DeleteByIndex;
end;

procedure TMDOTable.DeleteTable;
var
  Query: TMDOSQL;
begin
  CheckInactive;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'drop table ' +  {do not localize}
      QuoteIdentifier(DataBase.SQLDialect, FTableName);
    Query.Prepare;
    Query.ExecQuery;
  finally
    Query.Free;
  end;
end;

procedure TMDOTable.DoOnNewRecord;
var
  I: Integer;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    for I := 0 to FMasterLink.Fields.Count - 1 do
      IndexFields[I] := TField(FMasterLink.Fields[I]);
  inherited DoOnNewRecord;
end;

procedure TMDOTable.EmptyTable;
var
  Query: TMDOSQL;
begin
  if Active then
    CheckBrowseMode;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'delete from ' + {do not localize}
      QuoteIdentifier(DataBase.SQLDialect, FTableName);
    Query.Prepare;
    Query.ExecQuery;
    if Active then
    begin
      ClearBuffers;
      DataEvent(deDataSetChange, 0);
    end;
  finally
    Query.Free;
  end;
end;

procedure TMDOTable.ExtractLinkFields;
var
  i: Integer;
  DetailFieldNames: string;
begin
  FMasterFieldsList.Clear;
  FDetailFieldsList.Clear;
  i := 1;
  while i <= Length(MasterFields) do
    FMasterFieldsList.Add(ExtractFieldName(MasterFields, i));
  i := 1;
  if IndexFieldNames = '' then
    DetailFieldNames := FPrimaryIndexFields
  else
    DetailFieldNames := IndexFieldNames;
  while i <= Length(DetailFieldNames) do
    FDetailFieldsList.Add(ExtractFieldName(DetailFieldNames, i));
end;

function TMDOTable.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

function TMDOTable.FormatFieldsList(Value: string): string;
var
  FieldName: string;
  i: Integer;
begin
  if Database.SQLDialect = 1 then begin
    Value := QuoteIdentifier(Database.SQLDialect, Value);
    Result := StringReplace (Value, ';', ', ', [rfReplaceAll]);
  end
  else begin
    i := 1;
    Result := '';
    while i <= Length(Value) do
    begin
      FieldName := ExtractFieldName(Value, i);
      if Result = '' then
        Result := QuoteIdentifier(Database.SQLDialect, FieldName)
      else
        Result := Result + ', ' + QuoteIdentifier(Database.SQLDialect, FieldName);
    end;
  end;
end;

procedure TMDOTable.GenerateSQL;
var
  i: Integer;
  SQL: TStrings;
  OrderByStr: string;
  bWhereClausePresent: Boolean;
begin
  bWhereClausePresent := False;
  Database.CheckActive;
  Transaction.CheckInTransaction;
  if IndexDefs.Updated = False then
    IndexDefs.Update;
  if IndexFieldNames <> '' then
    OrderByStr := FormatFieldsList(IndexFieldNames)
  else if IndexName <> '' then
    OrderByStr := FormatFieldsList(IndexDefs[IndexDefs.Indexof (IndexName)].Fields)
  else if FDefaultIndex and (FPrimaryIndexFields <> '') then
    OrderByStr := FormatFieldsList(FPrimaryIndexFields);
  SQL := TStringList.Create;
  SQL.Text := 'select ' + {do not localize}
    QuoteIdentifier(DataBase.SQLDialect, FTableName) + '.*, ' {do not localize}
    + 'RDB$DB_KEY as MDO_INTERNAL_DBKEY from ' {do not localize}
    + QuoteIdentifier(DataBase.SQLDialect, FTableName);
  if Filtered and (Filter <> '') then
  begin
    SQL.Text := SQL.Text + ' where ' + Filter; {do not localize}
    bWhereClausePresent := True;
  end;
  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and (MasterFields <> '') then
  begin
    if bWhereClausePresent then
      SQL.Text := SQL.Text + ' AND ' {do not localize}
    else
      SQL.Text := SQL.Text + ' WHERE '; {do not localize}
    ExtractLinkfields;
    if FDetailFieldsList.Count < FMasterFieldsList.Count then
      MDOError(mdoeUnknownError, [nil]);
    for i := 0 to FMasterFieldsList.Count - 1 do
    begin
      if i > 0 then
        SQL.Text := SQL.Text + 'AND ';
      SQL.Text := SQL.Text +
        QuoteIdentifier(DataBase.SQLDialect, FDetailFieldsList.Strings[i]) +
        ' = :' +
        QuoteIdentifier(DataBase.SQLDialect, FMasterFieldsList.Strings[i]);
    end;
  end;
  if OrderByStr <> '' then
    SQL.Text := SQL.Text + ' order by ' + OrderByStr; {do not localize}
  SelectSQL.Assign(SQL);
  RefreshSQL.Text := 'select ' + {do not localize}
    QuoteIdentifier(DataBase.SQLDialect, FTableName) + '.*, ' {do not localize}
    + 'RDB$DB_KEY as MDO_INTERNAL_DBKEY from ' {do not localize}
    + QuoteIdentifier(DataBase.SQLDialect, FTableName) +
    ' where RDB$DB_KEY = :MDO_INTERNAL_DBKEY'; {do not localize}
  WhereDBKeyRefreshSQL.Assign(RefreshSQL);
  InternalPrepare;
  SQL.Free;
end;

procedure TMDOTable.GenerateUpdateSQL;
var
  InsertFieldList, InsertParamList, UpdateFieldList: string;
  WherePrimaryFieldList, WhereAllFieldList: string;
  
  procedure GenerateFieldLists;
  var
    I: Integer;
  begin
    for I := 0 to FieldDefs.Count - 1 do begin
      with FieldDefs[I] do begin
        if not (InternalCalcField or (faReadOnly in Attributes) or
          (DataType = ftUnknown)) then
        begin
          if ( InsertFieldList <> '' ) then begin
            InsertFieldList := InsertFieldList + ', ';
            InsertParamList := InsertParamList + ', ';
            UpdateFieldList := UpdateFieldList + ', ';
            if (DataType <> ftBlob) and (DataType <>ftMemo) then
              WhereAllFieldList := WhereAllFieldList + ' AND ';
          end;
          InsertFieldList := InsertFieldList +
            QuoteIdentifier(DataBase.SQLDialect, Name);
          InsertParamList := InsertParamList + ':' +
            QuoteIdentifier(DataBase.SQLDialect, Name);
          UpdateFieldList := UpdateFieldList +
            QuoteIdentifier(DataBase.SQLDialect, Name) +
            ' = :' +
            QuoteIdentifier(DataBase.SQLDialect, Name);
          if (DataType <> ftBlob) and (DataType <>ftMemo) then
            WhereAllFieldList := WhereAllFieldList +
              QuoteIdentifier(DataBase.SQLDialect, Name) + ' = :' +
              QuoteIdentifier(DataBase.SQLDialect, Name);{do not localize}
        end;
      end;
    end;
  end;
  
  procedure GenerateWherePrimaryFieldList;
  var
    i: Integer;
    tmp: String;
  begin
    i := 1;
    while i <= Length(FPrimaryIndexFields) do
    begin
      tmp := ExtractFieldName(FPrimaryIndexFields, i);
      tmp :=
        QuoteIdentifier(DataBase.SQLDialect, tmp) +  ' = :' +
        QuoteIdentifier(DataBase.SQLDialect, tmp);{do not localize}
      if WherePrimaryFieldList <> '' then
        WherePrimaryFieldList :=
          WherePrimaryFieldList + ' AND ' + tmp
      else
        WherePrimaryFieldList := tmp;
    end;
  end;
  
begin
  if InternalGetUpdatable = False  then
    FReadOnly := True
  else
  begin
    DeleteSQL.Text := 'delete from ' + {do not localize}
      QuoteIdentifier(DataBase.SQLDialect, FTableName) +
      ' where RDB$DB_KEY = ' + ':MDO_INTERNAL_DBKEY'; {do not localize}
    GenerateFieldLists;
    InsertSQL.Text := 'insert into ' + {do not localize}
      QuoteIdentifier(DataBase.SQLDialect, FTableName) +
    ' (' + InsertFieldList + {do not localize}
      ') values (' + InsertParamList + ')'; {do not localize}
    ModifySQL.Text := 'update ' +
      QuoteIdentifier(DataBase.SQLDialect, FTableName) +
      ' set ' + UpdateFieldList + {do not localize}
      ' where RDB$DB_KEY = :MDO_INTERNAL_DBKEY'; {do not localize}
    WhereAllRefreshSQL.Text := 'select ' +  {do not localize}
      QuoteIdentifier(DataBase.SQLDialect, FTableName) + '.*, '
      + 'RDB$DB_KEY as MDO_INTERNAL_DBKEY from ' {do not localize}
      + QuoteIdentifier(DataBase.SQLDialect, FTableName) +
      ' where ' + WhereAllFieldList; {do not localize}
    if FPrimaryIndexFields <> '' then
    begin
      GenerateWherePrimaryFieldList;
      WherePrimaryRefreshSQL.Text := 'select ' + {do not localize}
        QuoteIdentifier(DataBase.SQLDialect, FTableName) + '.*, ' {do not localize}
        + 'RDB$DB_KEY as MDO_INTERNAL_DBKEY from ' {do not localize}
        + QuoteIdentifier(DataBase.SQLDialect, FTableName) +
        ' where ' + WherePrimaryFieldList; {do not localize}
    end;
    try
      InternalPrepare;
    except
      FReadonly := True;
    end;
  end;
end;

function TMDOTable.GetCanModify: Boolean;
begin
  Result := True;
  if (FTableName = '') or FReadOnly
    or FSystemTable or FMultiTableView then
    Result := False;
end;

function TMDOTable.GetCurrentDBKey: TFBDBKey;
var
  Buf: PChar;
begin
  CheckActive;
  buf := GetActiveBuf;
  if Buf <> nil then
    Result := PRecordData(Buf)^.rdDBKey
  else
    Result.DBKey[0] := 0;
end;

function TMDOTable.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

{$IFNDEF MDO_FPC}
procedure TMDOTable.GetDetailLinkFields(MasterFields, DetailFields: TList);
var
  i: Integer;
  Idx: TIndexDef;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and
     (Self.MasterFields <> '') then
  begin
    Idx := nil;
    MasterSource.DataSet.GetFieldList(MasterFields, Self.MasterFields);
    UpdateIndexDefs;
    if IndexName <> '' then
      Idx := IndexDefs.Find(IndexName)
    else if IndexFieldNames <> '' then
      Idx := IndexDefs.GetIndexForFields(IndexFieldNames, False)
    else
      for i := 0 to IndexDefs.Count - 1 do
        if ixPrimary in IndexDefs[i].Options then
        begin
          Idx := IndexDefs[i];
          break;
        end;
    if Idx <> nil then
      GetFieldList(DetailFields, Idx.Fields);
  end;
end;
{$ENDIF}

function TMDOTable.GetExists: Boolean;
var
  Query: TMDOSQL;
begin
  Result := Active;
  if Result or (TableName = '') then Exit;
  Database.InternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    Query.SQL.Text :=
    'Select USER from RDB$RELATIONS where RDB$RELATION_NAME = ' + {do not localize}
    '''' +
    FormatIdentifierValue(Database.SQLDialect,
      QuoteIdentifier(DataBase.SQLDialect, FTableName)) + '''';
    Query.Prepare;
    Query.ExecQuery;
    Result := not Query.EOF;
  finally
    Query.Free;
    Database.InternalTransaction.Commit;
  end;
end;

function TMDOTable.GetIndexField(Index: Integer): TField;
var
  I, Count: Integer;
  FieldNames, FieldName: string;
begin
  Result := nil;
  FieldName := '';
  FieldNames := IndexFieldNames;
  if FieldNames = '' then
  begin
    for I := 0 to IndexDefs.Count - 1 do
      if (IndexDefs[i].Name = FIndexName) then
      begin
        FieldNames := IndexDefs[i].Fields;
        break;
      end;
  end;
  for I := 0 to Index do
  begin
    Count := Pos(';', FieldNames); {mbcs OK}
    if Count = 0 then
      FieldName := FieldNames
    else begin
      FieldName := Copy(FieldNames, 0, Count - 1);
      System.Delete(FieldNames, 1, Count);
    end;
  end;
  if FieldName <> '' then
    Result := FieldByName(FieldName)
  else
    MDOError(mdoeIndexFieldMissing, [nil]);
end;

function TMDOTable.GetIndexFieldCount: Integer;
var
  I, Index: Integer;
  FieldNames: string;
  done: Boolean;
begin
  FieldNames := IndexFieldNames;
  if FieldNames = '' then
  begin
    for I := 0 to IndexDefs.Count - 1 do
      if (IndexDefs[i].Name = FIndexName) then
      begin
        FieldNames := IndexDefs[i].Fields;
        break;
      end;
  end;
  if FieldNames = '' then
    Result := 0
  else
  begin
    done := False;
    Result := 1;
    while not done do
    begin
      Index := Pos(';', FieldNames); {mbcs ok}
      if Index <> 0 then
      begin
        System.Delete(FieldNames, 1, Index);
        Inc(Result);
      end else
        done := True;
    end;
  end;
end;

function TMDOTable.GetIndexFieldNames: string;
begin
  if FFieldsIndex then Result := FIndexName else Result := '';
end;

function TMDOTable.GetIndexName: string;
begin
  if FFieldsIndex then Result := '' else Result := FIndexName;
end;

procedure TMDOTable.GetIndexNames(List: TStrings);
begin
  IndexDefs.Update;
  IndexDefs.GetItemNames(List);
end;

procedure TMDOTable.GetIndexParams(const IndexName: string; FieldsIndex: 
        Boolean; var IndexedName: string);
var
  IndexStr: TIndexName;
begin
  if IndexName <> '' then
  begin
    IndexDefs.Update;
    IndexStr := IndexName;
    if FieldsIndex then
      IndexStr := IndexDefs.FindIndexForFields(IndexName).Name;
  end;
  IndexedName := IndexStr;
end;

function TMDOTable.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

function TMDOTable.GetTableNames: TStrings;
begin
  FNameList.clear;
  GetTableNamesFromServer;
  Result := FNameList;
end;

procedure TMDOTable.GetTableNamesFromServer;
var
  Query: TMDOSQL;
begin
  if not (csReading in ComponentState) then begin
    ActivateConnection;
    Database.InternalTransaction.StartTransaction;
    Query := TMDOSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := DataBase;
      Query.Transaction := Database.InternalTransaction;
      if (TableTypes * [ttSystem, ttView] = [ttSystem, ttView]) then
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' {do not localize}
      else if ttSystem in TableTypes then
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' + {do not localize}
                          ' where RDB$VIEW_BLR is NULL' {do not localize}
      else if ttView in TableTypes then
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' + {do not localize}
                          ' where RDB$SYSTEM_FLAG = 0' {do not localize}
      else
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' + {do not localize}
                          ' where RDB$VIEW_BLR is NULL and RDB$SYSTEM_FLAG = 0'; {do not localize}
      Query.Prepare;
      Query.ExecQuery;
      while (not Query.EOF) and (Query.Next <> nil) do
        FNameList.Add (TrimRight(Query.Current[0].AsString));
    finally
      Query.Free;
      Database.InternalTransaction.Commit;
    end;
  end;
end;

procedure TMDOTable.GotoCurrent(Table: TMDOTable);
begin
  CheckBrowseMode;
  Table.CheckBrowseMode;
  if (Database <> Table.Database) or
    (CompareText(TableName, Table.TableName) <> 0) then
    MDOError(mdoeTableNameMismatch, [nil]);
  Table.UpdateCursorPos;
  InternalGotoDBKey(Table.CurrentDBKey);
  DoBeforeScroll;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;

function TMDOTable.IndexDefsStored: Boolean;
begin
  Result := StoreDefs and (IndexDefs.Count > 0);
end;

procedure TMDOTable.InitFieldDefs;
var
  sqlscale: Integer;
  Query: TMDOSQL;
begin
  if FTableName = '' then MDOError(mdoeNoTableName, [nil]);
  if (InternalPrepared) then InternalInitFieldDefs else
  begin
    Database.InternalTransaction.StartTransaction;
    Query := TMDOSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := DataBase;
      Query.Transaction := Database.InternalTransaction;
      Query.SQL.Text := 'Select R.RDB$FIELD_NAME, R.RDB$FIELD_POSITION, ' + {do not localize}
                        'F.RDB$COMPUTED_BLR, F.RDB$DEFAULT_VALUE, ' + {do not localize}
                        'F.RDB$NULL_FLAG, ' + {do not localize}
                        'F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE, ' + {do not localize}
                        'F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' + {do not localize}
                        'F.RDB$EXTERNAL_LENGTH, F.RDB$EXTERNAL_SCALE, F.RDB$EXTERNAL_TYPE ' + {do not localize}
                        'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
                        'where R.RDB$RELATION_NAME = ' + {do not localize}
                        '''' +
                        FormatIdentifierValue(Database.SQLDialect,
                          QuoteIdentifier(DataBase.SQLDialect, FTableName)) +
                        ''' ' +
                        'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+ {do not localize}
                        'order by R.RDB$FIELD_POSITION'; {do not localize}
  
      Query.Prepare;
      Query.ExecQuery;
      FieldDefs.BeginUpdate;
      FieldDefs.Clear;
      while (not Query.EOF) and (Query.Next <> nil) do
      begin
          with FieldDefs.AddFieldDef do
          begin
            {$IFNDEF MDO_FPC}
            FieldNo := Query.Current.ByName('RDB$FIELD_POSITION').AsInteger; {do not localize}
            {$ENDIF}
            Name := TrimRight(Query.Current.ByName('RDB$FIELD_NAME').AsString); {do not localize}
            case Query.Current.ByName('RDB$FIELD_TYPE').AsInteger of {do not localize}
              blr_varying, blr_text:
              begin
                DataType := ftString;
                Size := Query.Current.ByName('RDB$FIELD_LENGTH').AsInteger; {do not localize}
              end;
              blr_float, blr_double, blr_d_float: DataType := ftFloat;
              blr_short:
              begin
                sqlscale := Query.Current.ByName('RDB$FIELD_SCALE').AsInteger; {do not localize}
                if (sqlscale = 0) then
                  DataType := ftSmallInt
                else
                begin
                  DataType := ftBCD;
                  Precision := 4;
                end;
              end;
              blr_long:
              begin
                sqlscale := Query.Current.ByName('RDB$FIELD_SCALE').AsInteger; {do not localize}
                if (sqlscale = 0) then
                  DataType := ftInteger
                else if (sqlscale >= (-4)) then
                begin
                  DataType := ftBCD;
                  Precision := 9;
                end
                else
                  DataType := ftFloat;
              end;
              blr_int64:
              begin
                sqlscale := Query.Current.ByName('RDB$FIELD_SCALE').AsInteger; {do not localize}
                if (sqlscale = 0) then
                  DataType := ftLargeInt
                else if (sqlscale >= (-4)) then
                begin
                  DataType := ftBCD;
                  Precision := 18;
                end
                else
                  DataType := ftFloat;
              end;
              blr_timestamp: DataType := ftDateTime;
              blr_sql_time: DataType := ftTime;
              blr_sql_date: DataType := ftDate;
              blr_blob:
                if (Query.Current.ByName('RDB$FIELD_SUB_TYPE').AsInteger = 1) then {do not localize}
                  DataType := ftMemo
                else
                  DataType := ftBlob;
              blr_quad:
              begin
                DataType := ftUnknown;
                Size := sizeof (TISC_QUAD);
              end;
              else
                DataType := ftUnknown;
            end;
            if not (Query.Current.ByName('RDB$COMPUTED_BLR').IsNull) then {do not localize}
            begin
              Attributes := [faReadOnly];
              InternalCalcField := True
            end
            else
              InternalCalcField := False;
            if ((not InternalCalcField) and
                 Query.Current.ByName('RDB$DEFAULT_VALUE').IsNull and {do not localize}
                 (Query.Current.ByName('RDB$NULL_FLAG').AsInteger = 1) )then {do not localize}
            begin
              Attributes := [faRequired];
              Required := True;
            end;
          end;
      end;
      FieldDefs.EndUpdate;
    finally
      Query.free;
      Database.InternalTransaction.Commit;
    end;
  end;
end;

procedure TMDOTable.InternalClose;
begin
  DataEvent(dePropertyChange, 0);
  inherited InternalClose;
end;

function TMDOTable.InternalGetUpdatable: Boolean;
var
  Query: TMDOSQL;
begin
  Database.InternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    Query.SQL.Text := 'Select RDB$SYSTEM_FLAG, RDB$DBKEY_LENGTH ' + {do not localize}
                    'from RDB$RELATIONS where RDB$RELATION_NAME = ' + {do not localize}
                    '''' +
                    FormatIdentifierValue(Database.SQLDialect,
                      QuoteIdentifier(DataBase.SQLDialect, FTableName)) + '''';
    Query.Prepare;
    Query.ExecQuery;
    if (Query.Current[0].AsInteger <> 0) or
       (Query.Current[1].AsInteger <> 8) then
      Result := False
    else
      Result := True;
  finally
    Query.Free;
    Database.InternalTransaction.Commit;
  end;
end;

function TMDOTable.InternalGotoDBKey(DBKey: TFBDBKey): Boolean;
  
  function DBKeyCompare (DBKey1, DBKey2: TFBDBKey): Boolean;
  var
  I: Integer;
  begin
    for I := 0 to 7 do
      if (DBKey1.DBKey[i] <> DBKey2.DBKey[i]) then begin
        result := False;
        exit;
      end;
    result := True;
  end;
  
begin
   CheckActive;
   DisableControls;
  try
     result := False;
     First;
     while ((not result) and (not EOF)) do begin
       if (DBKeyCompare (DBKey, PRecordData(GetActiveBuf)^.rdDBKey)) then
         result := True
       else
         Next;
     end;
     if not result then
       First
     else
       CursorPosChanged;
   finally
     EnableControls;
   end;
end;

procedure TMDOTable.InternalOpen;
begin
  if FTableName = '' then MDOError(mdoeNoTableName, [nil]);
  ActivateConnection;
  ActivateTransaction;
  if FRegenerateSQL then
  begin
    InternalUnprepare;
    GenerateSQL;
    if not FReadOnly then
      GenerateUpdateSQL;
    FRegenerateSQL := False;
  end;
  SetParams;
  inherited InternalOpen;
end;

procedure TMDOTable.InternalRefresh;
var
  DBKey: TFBDBKey;
begin
  DBKey := CurrentDBKey;
  Reopen;
  if DBKey.DBKey[0] <> 0 then
    InternalGotoDBKey(DBKey);
end;

procedure TMDOTable.InternalRefreshRow;
begin
  if CurrentDBKey.DBKey[0] <> 0 then
    QRefresh.SQL.Assign(WhereDBKeyRefreshSQL)
  else if WherePrimaryRefreshSQL.Text <> '' then
    QRefresh.SQL.Assign(WherePrimaryRefreshSQL)
  else
    QRefresh.SQL.Assign(WhereAllRefreshSQL);
  inherited InternalRefreshRow;
end;

procedure TMDOTable.InternalTableRefresh;
var
  DBKey: TFBDBKey;
begin
  CheckActive;
  DBKey := CurrentDBKey;
  FRegenerateSQL := True;
  Reopen;
  if DBKey.DBKey[0] <> 0 then
    InternalGotoDBKey(DBKey);
end;

procedure TMDOTable.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  SetParams;
  ReQuery;
end;

procedure TMDOTable.MasterDisabled(Sender: TObject);
begin
  DataEvent(dePropertyChange, 0);
  ReQuery;
end;

{$IFNDEF MDO_FPC}
function TMDOTable.PSGetDefaultOrder: TIndexDef;
  
    function GetIdx(IdxType: TIndexOption): TIndexDef;
    var
      i: Integer;
    begin
      Result := nil;
      for i := 0 to IndexDefs.Count - 1 do
        if IdxType in IndexDefs[i].Options then
        try
          Result := IndexDefs[i];
          GetFieldList(nil, Result.Fields);
          break;
        except
          Result := nil;
        end;
    end;
  
  var
    DefIdx: TIndexDef;
  
begin
  DefIdx := nil;
  IndexDefs.Update;
  try
    if IndexName <> '' then
      DefIdx := IndexDefs.Find(IndexName)
    else if IndexFieldNames <> '' then
      DefIdx := IndexDefs.FindIndexForFields(IndexFieldNames);
    if Assigned(DefIdx) then
      GetFieldList(nil, DefIdx.Fields);
  except
    DefIdx := nil;
  end;
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixPrimary);
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixUnique);
  if Assigned(DefIdx) then
  begin
    Result := TIndexDef.Create(nil);
    Result.Assign(DefIdx);
  end else
    Result := nil;
end;

function TMDOTable.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := GetIndexDefs(IndexDefs, IndexTypes);
end;

function TMDOTable.PSGetKeyFields: string;
var
  i, Idx: Integer;
  IndexFound: Boolean;
begin
  Result := inherited PSGetKeyFields;
  if Result = '' then
  begin
    if not Exists then Exit;
    IndexFound := False;
    IndexDefs.Update;
    FieldDefs.Update;
    for i := 0 to IndexDefs.Count - 1 do
      if ixUnique in IndexDefs[I].Options then
      begin
        Idx := 1;
        Result := IndexDefs[I].Fields;
        IndexFound := False;
        while Idx <= Length(Result) do
        begin
          IndexFound := FindField(ExtractFieldName(Result, Idx)) <> nil;
          if not IndexFound then Break;
        end;
        if IndexFound then Break;
      end;
    if not IndexFound then
      Result := '';
  end;
end;

function TMDOTable.PSGetTableName: string;
begin
  Result := FTableName;
end;

procedure TMDOTable.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;

procedure TMDOTable.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Open;
  PSReset;
end;
{$ENDIF}

procedure TMDOTable.Reopen;
begin
  DisableControls;
  try
    if Active then
    begin
      SetState(dsInactive);
      CloseCursor;
      OpenCursor{$IFDEF MDO_FPC}(False){$ENDIF};
      SetState(dsBrowse);
    end;
  finally
    EnableControls;
  end;
end;

procedure TMDOTable.ResetSQLStatements;
begin
  SelectSQL.Text := '';
  DeleteSQL.Text := '';
  InsertSQL.Text := '';
  ModifySQL.Text := '';
  RefreshSQL.Text := '';
end;

procedure TMDOTable.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then MDOError(mdoeCircularDataLink, [Self]);
  if FMasterLink.DataSource <> Value then
    DataEvent(dePropertyChange, 0);
  FMasterLink.DataSource := Value;
end;

procedure TMDOTable.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
      InternalTableRefresh;
  end
  else
    inherited SetFiltered(value);
end;

procedure TMDOTable.SetFilterOptions(Value: TFilterOptions);
begin
  if Value <> [] then
    MDOError(mdoeNotSupported, [nil]);
end;

procedure TMDOTable.SetFilterText(const Value: string);
begin
  if Filtered and (Value <> Filter) then
  begin
    inherited SetFilterText(value);
    InternalTableRefresh;
  end
  else
    inherited SetFilterText(value);
end;

procedure TMDOTable.SetIndex(const Value: string; FieldsIndex: Boolean);
begin
  if Active then CheckBrowseMode;
  if (FIndexName <> Value) or (FFieldsIndex <> FieldsIndex) then
  begin
    FIndexName := Value;
    FFieldsIndex := FieldsIndex;
    if Active then
    begin
      SwitchToIndex;
    end;
  end;
end;

procedure TMDOTable.SetIndexDefs(Value: TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;

procedure TMDOTable.SetIndexField(Index: Integer; Value: TField);
begin
  GetIndexField(Index).Assign(Value);
end;

procedure TMDOTable.SetIndexFieldNames(const Value: string);
begin
  SetIndex(Value, Value <> '');
end;

procedure TMDOTable.SetIndexName(const Value: string);
begin
  SetIndex(Value, False);
end;

procedure TMDOTable.SetMasterFields(const Value: string);
begin
  if FMasterLink.FieldNames <> Value then
    DataEvent(dePropertyChange, 0);
  FMasterLink.FieldNames := Value;
end;

procedure TMDOTable.SetParams;
var
  i: Integer;
begin
  if (MasterSource = nil) or (MasterSource.DataSet = nil) or
  (not MasterSource.DataSet.Active) or (FMasterFieldsList.Count = 0) then
    exit;
  for i := 0 to FMasterFieldsList.Count - 1 do
    QSelect.Params.ByName(FMasterFieldsList.Strings[i]).Value :=
    MasterSource.DataSet.FieldByName(FMasterFieldsList.Strings[i]).Value;
end;

procedure TMDOTable.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

procedure TMDOTable.SetTableName(Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if Value <> FTableName then
    begin
      ResetSQLStatements;
      FRegenerateSQL := True;
      FTableName := Value;
      IndexName := '';
      IndexFieldNames := '';
      FPrimaryIndexFields := '';
      DataEvent(dePropertyChange, 0);
    end;
  end
  else if Value <> FTableName then
    FTableName := Value;
end;

procedure TMDOTable.SetTableTypes(const Value: TMDOTableTypes);
begin
  FTableTypes := Value;
end;

procedure TMDOTable.SwitchToIndex;
begin
  FSwitchingIndex := True;
  InternalTableRefresh;
  FSwitchingIndex := False;
end;

procedure TMDOTable.UpdateIndexDefs;
var
  Opts: TIndexOptions;
  Flds: string;
  Query, SubQuery: TMDOSQL;
begin
  if not (csReading in ComponentState) then begin
  if not Active and not FSwitchingIndex  then
    FieldDefs.Update;
  IndexDefs.Clear;
  Database.InternalTransaction.StartTransaction;
  Query := TMDOSQL.Create(self);
  try
    FPrimaryIndexFields := '';
    Query.GoToFirstRecordOnExecute := False;
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    (*
    Query.SQL.Text :=
    'Select I.RDB$INDEX_NAME, I.RDB$UNIQUE_FLAG, I.RDB$INDEX_TYPE, ' + {do not localize}
    'I.RDB$SEGMENT_COUNT, S.RDB$FIELD_NAME from RDB$INDICES I, ' + {do not localize}
    'RDB$INDEX_SEGMENTS S where I.RDB$INDEX_NAME = S.RDB$INDEX_NAME '+ {do not localize}
    'and I.RDB$RELATION_NAME = ' + '''' + {do not localize}
     FormatIdentifierValue(Database.SQLDialect,
       QuoteIdentifier(DataBase.SQLDialect, FTableName)) + '''';
    *)
    Query.SQL.Text :=
    'Select I.RDB$INDEX_NAME, I.RDB$UNIQUE_FLAG, I.RDB$INDEX_TYPE, ' + {do not localize}
    'I.RDB$SEGMENT_COUNT, S.RDB$FIELD_NAME, R.RDB$CONSTRAINT_TYPE ' + {do not localize}
    'from RDB$INDICES I, RDB$INDEX_SEGMENTS S ' + {do not localize}
    '  LEFT JOIN RDB$RELATION_CONSTRAINTS R ON ( R.RDB$INDEX_NAME = S.RDB$INDEX_NAME) ' + {do not localize}
    'where I.RDB$INDEX_NAME = S.RDB$INDEX_NAME '+ {do not localize}
    'and I.RDB$RELATION_NAME = ' + '''' + {do not localize}
     FormatIdentifierValue(Database.SQLDialect,
       QuoteIdentifier(DataBase.SQLDialect, FTableName)) + '''';
    Query.Prepare;
    Query.ExecQuery;
    while (not Query.EOF) and (Query.Next <> nil) do
    begin
      with IndexDefs.AddIndexDef do
      begin
        Name := TrimRight(Query.Current.ByName('RDB$INDEX_NAME').AsString); {do not localize}
        Opts := [];
        (*
        if Pos ('RDB$PRIMARY', Name) = 1 then Include(Opts, ixPrimary); {do not localize} {mbcs ok}
        *)
        // Privary key test, since Firebird 1.5 must be another way...
        if Pos( 'PRIMARY KEY', AnsiUpperCase( Query.Current.ByName( 'RDB$CONSTRAINT_TYPE' ).AsString ) ) > 0 then
          Include( Opts, ixPrimary ); {do not localize} {mbcs ok}
        if Query.Current.ByName('RDB$UNIQUE_FLAG').AsInteger = 1 then Include(Opts, ixUnique); {do not localize}
        if Query.Current.ByName('RDB$INDEX_TYPE').AsInteger = 2  then Include(Opts, ixDescending); {do not localize}
        Options := Opts;
        if (Query.Current.ByName('RDB$SEGMENT_COUNT').AsInteger = 1) then {do not localize}
          Fields := Trim(Query.Current.ByName('RDB$FIELD_NAME').AsString) {do not localize}
        else begin
          SubQuery := TMDOSQL.Create(self);
        try
          SubQuery.GoToFirstRecordOnExecute := False;
          SubQuery.Database := DataBase;
          SubQuery.Transaction := Database.InternalTransaction;
          SubQuery.SQL.Text :=
         'Select RDB$FIELD_NAME from RDB$INDEX_SEGMENTS where RDB$INDEX_NAME = ' + {do not localize}
          '''' +
          FormatIdentifierValue(Database.SQLDialect,
            QuoteIdentifier(DataBase.SQLDialect, Name)) +
          '''' + 'ORDER BY RDB$FIELD_POSITION'; {do not localize}
          SubQuery.Prepare;
          SubQuery.ExecQuery;
          Flds := '';
          while (not SubQuery.EOF) and (SubQuery.Next <> nil) do
          begin
            if (Flds = '') then
              Flds := TrimRight(SubQuery.Current.ByName('RDB$FIELD_NAME').AsString) {do not localize}
            else begin
              Query.Next;
              Flds := Flds + ';' + TrimRight(SubQuery.Current[0].AsString);
            end;
          end;
          Fields := Flds;
        finally
          SubQuery.Free;
        end;
        end;
        if (ixDescending in Opts) then
          DescFields := Fields;
        if ixPrimary in Opts then
          FPrimaryIndexFields := Fields;
      end;
    end;
  finally
    Query.Free;
    Database.InternalTransaction.Commit;
  end;
  end;
end;

{ Index / Ranges / Keys }


{ Informational & Property }


{ TMDOTable IProviderSupport }

end.
