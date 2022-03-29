unit mdotools;

interface

{$I ..\MDO.inc}

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF} Classes, MDODatabase, MDOSQL, MDOServices;

type

  { TMDOStorage }

  TMDOStorage = class
  private
    FDatabase: TMDODataBase;
    FTransaction: TMDOTransaction;
    FTableName: String;
    FKeyField: String;
    FValueField: String;
  public
    function GetValue(AKey: String; ADefValue: String): String; overload;
    function GetValue(AKey: String; ADefValue: Integer): Integer; overload;
    function GetValue(AKey: String; ADefValue: Boolean): Boolean; overload;
    procedure SetValue(AKey: String; AValue: String); overload;
    procedure SetValue(AKey: String; AValue: Integer); overload;
    procedure SetValue(AKey: String; AValue: Boolean); overload;
    function ValueExist(AKey: String): Boolean;
    procedure DeleteValue(AKey: String);
    property Database: TMDODataBase read FDatabase write FDatabase;
    property Transaction: TMDOTransaction read FTransaction write FTransaction;
    property TableName: String read FTableName write FTableName;
    property KeyField: String read FKeyField write FKeyField;
    property ValueField: String read FValueField write FValueField;
  end;

procedure SetParams(Params: TMDOXSQLDA; const ParamName: string; const Values: Variant);

function CreateQuery(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  ParamName: String; ParamValues: Variant): TMDOSQL;

function SQLWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant; Execute: Boolean): Variant;

function QueryWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant): Variant;

function QueryValue(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName: String = ''): Variant;

function ExecWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  ParamName: String; ParamValues: Variant): Integer;

function ExecQuery(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String): Integer;

function QueryRow(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  AParamName: String; AParamValues: Variant): Variant;

const
  INI_DB_SECTION = 'Database';
  INI_DB_NAME = 'database_name';
  INI_DB_USER = 'user_name';
  INI_DB_PASSWORD = 'password';
  INI_DB_LC_TYPE = 'lc_ctype';

procedure LoadDBParamsFromINI(ADB: TMDODatabase; AFileName: String; ASection: String = INI_DB_SECTION; AIdents: array of String); overload;
procedure LoadDBParamsFromINI(AFileName: String; var AServer, ADataBase, AUser, APassword, ALCLT: String; var AProtocol: TProtocol; ASection: String = INI_DB_SECTION; AIdents: array of String); overload;
procedure SaveDBParamsToINI(ADataBase, AUser, APasswd, ALCLT: String; AFileName: String);
procedure LoadSvcParamsFromINI(ASvc: TMDOCustomService; AFileName: String; ASection: String = INI_DB_SECTION);

{$IFDEF WINDOWS}
const
  //REG_DB_SECTION = 'Database';
  REG_DB_NAME = 'database_name';
  REG_DB_USER = 'user_name';
  REG_DB_PASSWORD = 'password';
  REG_DB_LC_TYPE = 'lc_ctype';

procedure LoadDBParamsFromReg(ADB: TMDODatabase; ARootKey: HKEY; AKeyName: String);
procedure SaveDBParamsToReg(ADataBase, AUser, APasswd, ALCLT: String; ARootKey: HKEY; AKeyName: String);
{$ENDIF}

procedure ClearDBParams(ADB: TMDODatabase);

procedure StartTrans(ATransaction: TMDOTransaction);
procedure CommitTrans(ATransaction: TMDOTransaction; Retaining: Boolean = False);
procedure RollBackTrans(ATransaction: TMDOTransaction);

procedure LoadStrings(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String; AFieldName: String; AStrings: TStrings);
procedure LoadStringsWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String; AFieldName: String; AParamNames: String; AParamValues: Variant; AStrings: TStrings);

procedure ClearSvcParams(ASvc: TMDOCustomService);

procedure LoadSvcParamsFromDB(ASvc: TMDOCustomService; ADB: TMDODatabase);

function StringToProtocol(AStrPtoto: String): TProtocol;

function BuildDatabaseName(AProtocol: TProtocol; AServer, ADatabase: String): String;
procedure DecodeDatabaseName(ADatabaseName: String; var AProtocol: TProtocol; var AServer: String; var ADatabase: String);

function DumpDBPatams(ADB: TMDODataBase): String;

const
  PROTOCOL_STR: array[TProtocol] of String = ('TCP', 'SPX', 'NamedPipe', 'Local');

implementation

uses
  sysutils, variants, IniFiles {$IFDEF WINDOWS}, Registry{$ENDIF};

procedure SetParams(Params: TMDOXSQLDA; const ParamName: string; const Values: Variant);
var
  I: Integer;
  S: TStringList;
begin
  if (ParamName <> '') and (not VarIsArray(Values)) then
    Params.ByName(ParamName).Value := Values
  else if (Params.Count = 1) and (not VarIsEmpty(Values)) and (not VarIsArray(Values)) then
    Params[0].Value := Values
  else if (Params.Count > 1) and (ParamName <> '') and VarIsArray(Values) then
  begin
    S := TStringList.Create;
    S.Delimiter := ';';
    S.DelimitedText := ParamName;
    for I := 0 to S.Count do
      if (Params.ByName(S[I]) <> nil) and (I >= VarArrayLowBound(Values, 1)) and (VarArrayHighBound(Values, 1) <= I) then
        Params.ByName(S[I]).Value := Values[I];
    S.Free;
  end
  else if (Params.Count > 1) and VarIsArray(Values) then
    for I := 0 to Params.Count - 1 do
      Params[I].Value := Values[I]
end;

function CreateQuery(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  ParamName: String; ParamValues: Variant): TMDOSQL;
const
  QueryID: Integer = 0;
begin
  Result := TMDOSQL.Create(ADatabase);
  Result.Database := ADatabase;
  Result.Transaction := ATransaction;
  with Result do
  try
    Name := Format('quQuery%d', [QueryID]);
    Inc(QueryID);
    SQL.Text := ASQL;
    if (ParamName <> '') or (not VarIsNull(ParamValues)) then
      SetParams(Params, ParamName, ParamValues);
  except
    Free;
    raise;
  end;
end;

function SQLWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant; Execute: Boolean): Variant;

var
  Query: TMDOSQL;

function ParamExist(AName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Query.Params.Count - 1 do
    if Query.Params.Names[I] = AName then
    begin
      Result := True;
      Break;
    end;
end;

begin
  Query := CreateQuery(ADatabase, ATransaction, ASQL, ParamName, ParamValues);
  Result := Null;
  with Query do
  try
    ExecQuery;
    if Execute then
    begin
      Result := RowsAffected;
    end
    else
      if RecordCount > 0 then
        if (FieldName <> '') then
          Result := FieldByName(FieldName).Value
        else
          Result := Fields[0].Value
      else
        if (FieldName <> '') and ParamExist(FieldName) then
          Result := ParamByName(FieldName).Value;
  finally
    Free;
  end;
end;

function QueryWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant): Variant;
begin
  Result := SQLWithParam(ADatabase, ATransaction, ASQL, FieldName, ParamName, ParamValues, False);
end;

function QueryValue(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  FieldName: String): Variant;
begin
  Result := QueryWithParam(ADatabase, ATransaction, ASQL, FieldName, '', Null);
end;

function ExecWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String;
  ParamName: String; ParamValues: Variant): Integer;
begin
  Result := SQLWithParam(ADatabase, ATransaction, ASQL, '', ParamName, ParamValues, True);
end;

function ExecQuery(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String): Integer;
begin
  Result := ExecWithParam(ADatabase, ATransaction, ASQL, '', Unassigned);
end;

function QueryRow(ADatabase: TMDODataBase; ATransaction: TMDOTransaction;
  ASQL: String; AParamName: String; AParamValues: Variant): Variant;
var
  Query: TMDOSQL;
  I: Integer;
begin
  Result := Null;
  Query := CreateQuery(ADatabase, ATransaction, ASQL, AParamName, AParamValues);
  with Query do
  try
    ExecQuery;
    if RecordCount > 0 then
    begin
      Result := VarArrayCreate([0, Current.Count - 1], varVariant);
      for I := 0 to Current.Count - 1 do
        Result[I] := Fields[I].Value;
    end;
  finally
    Free;
  end;
end;

{ TMDOStorage }

function TMDOStorage.GetValue(AKey: String; ADefValue: String): String;
const
  SELSQL = 'select %s from %s where %s = :KEY';
var
  R: Variant;
begin
  if not FTransaction.InTransaction then
    FTransaction.StartTransaction;
  R := QueryWithParam(FDatabase, FTransaction,
    Format(SELSQL, [FValueField, FTableName, FKeyField]), FValueField, 'KEY', AKey);
  if R = Null then
    Result := ADefValue
  else
    Result := R;
end;

function TMDOStorage.GetValue(AKey: String; ADefValue: Integer): Integer;
begin
  Result := StrToIntDef(GetValue(AKey, ''), ADefValue);
end;

function TMDOStorage.GetValue(AKey: String; ADefValue: Boolean): Boolean;
begin
  Result := StrToBoolDef(GetValue(AKey, ''), ADefValue);
end;

procedure TMDOStorage.SetValue(AKey: String; AValue: String);
const
  SQLI = 'insert into %s (%s, %s) values (:VALUE, :KEY)';
  SQLU = 'update %s set %s = :VALUE where %s = :KEY';
var
  S: String;
  Prms: Variant;
begin
  if ValueExist(AKey) then
    S := Format(SQLU, [FTableName, FValueField, FKeyField])
  else
    S := Format(SQLI, [FTableName, FValueField, FKeyField]);
  Prms := VarArrayCreate([0,1], varvariant);
  Prms[0] := AValue;
  Prms[1] := AKey;
  ExecWithParam(FDatabase, FTransaction, S, '', Prms);
  Prms := Null;
end;

procedure TMDOStorage.SetValue(AKey: String; AValue: Integer);
begin
  SetValue(AKey, IntToStr(AValue));
end;

procedure TMDOStorage.SetValue(AKey: String; AValue: Boolean);
begin
  SetValue(AKey, BoolToStr(AValue, True));
end;

function TMDOStorage.ValueExist(AKey: String): Boolean;
const
  SQLSEL = 'select count(*) from %s where %s = :KEY';
begin
  Result := QueryWithParam(FDatabase, FTransaction, Format(SQLSEL, [FTableName, FKeyField]),
    '', 'KEY', AKey) > 0;
end;

procedure TMDOStorage.DeleteValue(AKey: String);
const
  SQLD = 'delete from %s where %s = :KEY';
begin
  ExecWithParam(FDatabase, FTransaction, Format(SQLD, [FTableName, FKeyField]), 'KEY', AKey);
end;

procedure LoadDBParamsFromINI(ADB: TMDODatabase; AFileName: String;
  ASection: String; AIdents: array of String);
var
  INI: TIniFile;
  AId: array[0..3] of String;
begin
//  if not FileExists(AFileName) then
//    raise Exception.Create('File not found: ' + AFileName);
  if Length(AIdents) <> 4 then
  begin
    AId[0] := INI_DB_NAME;
    AId[1] := INI_DB_USER;
    AId[2] := INI_DB_PASSWORD;
    AId[3] := INI_DB_LC_TYPE;
  end
  else
  begin
    AId[0] := AIdents[0];
    AId[1] := AIdents[1];
    AId[2] := AIdents[2];
    AId[3] := AIdents[3];
  end;

  ClearDBParams(ADB);
  INI := TIniFile.Create(AFileName);
  if INI.ValueExists(ASection, AId[0]) then
    ADB.DatabaseName := INI.ReadString(ASection, AId[0], '');
  if INI.ValueExists(ASection, AId[1]) then
    ADB.Params.Values['user_name'] := INI.ReadString(ASection, AId[1], '');
  if INI.ValueExists(ASection, AId[2]) then
    ADB.Params.Values['password'] := INI.ReadString(ASection, AId[2], '');
  if INI.ValueExists(ASection, AId[3]) then
    ADB.Params.Values['lc_ctype'] := INI.ReadString(ASection, AId[3], '');
  INI.Free;
end;

procedure LoadDBParamsFromINI(AFileName: String; var AServer, ADataBase, AUser,
  APassword, ALCLT: String; var AProtocol: TProtocol; ASection: String;
  AIdents: array of String);

function DecryptPassword(ACrypted: String): String;
var
  D: PChar;
  X: Byte;
  S: String;
  I: Integer;
begin
  D := StrNew('                  ');
  HexToBin(PChar(ACrypted), D, 18);
  X := Ord(D[0]);
  for I := 1 to 17 do
    D[I] := Chr(Ord(D[I]) xor X);
  S := '';
  for I := 1 to Ord(D[1]) do
    S := S + D[I+1];
  Result := S;
//  S := D;
//  X := Ord(S[1]);
//  for I := 2 to 18 do
//    S[I] := Chr(Ord(S[I]) xor X);
//  SetLength(Result, Ord(S[2]));
//  Move(S[3], Result[1], Ord(S[2]));
  StrDispose(D);
end;

var
  INI: TIniFile;
  AId: array[0..3] of String;
begin
//  if not FileExists(AFileName) then
//    raise Exception.Create('File not found: ' + AFileName);
  if Length(AIdents) <> 4 then
  begin
    AId[0] := INI_DB_NAME;
    AId[1] := INI_DB_USER;
    AId[2] := INI_DB_PASSWORD;
    AId[3] := INI_DB_LC_TYPE;
  end
  else
  begin
    AId[0] := AIdents[0];
    AId[1] := AIdents[1];
    AId[2] := AIdents[2];
    AId[3] := AIdents[3];
  end;

  INI := TIniFile.Create(AFileName);
  if INI.ValueExists(ASection, AId[0]) then
  begin
    ADataBase := INI.ReadString(ASection, AId[0], '');
    DecodeDatabaseName(ADataBase, AProtocol, AServer, ADataBase);
  end;
  if INI.ValueExists(ASection, AId[1]) then
    AUser := INI.ReadString(ASection, AId[1], '');
  if INI.ValueExists(ASection, AId[2]) then
    APassword := INI.ReadString(ASection, AId[2], '');
  if INI.ValueExists(ASection, AId[3]) then
    ALCLT := INI.ReadString(ASection, AId[3], '');
  INI.Free;
  if Length(APassword) = 36 then
      APassword := DecryptPassword(APassword);
end;

procedure SaveDBParamsToINI(ADataBase, AUser, APasswd, ALCLT: String; AFileName: String);
var
  INI: TIniFile;
begin
  INI := TIniFile.Create(AFileName);
  INI.WriteString(INI_DB_SECTION, INI_DB_NAME, ADataBase);
  INI.WriteString(INI_DB_SECTION, INI_DB_USER, AUser);
  INI.WriteString(INI_DB_SECTION, INI_DB_PASSWORD, APasswd);
  INI.WriteString(INI_DB_SECTION, INI_DB_LC_TYPE, ALCLT);
  INI.Free;
end;

procedure LoadSvcParamsFromINI(ASvc: TMDOCustomService; AFileName: String; ASection: String);
var
  FSvr, FDb, FUsr, FPas, FLC: String;
  FPr: TProtocol;
begin
  LoadDBParamsFromINI(AFileName, FSvr, FDb, FUsr, FPas, FLC, FPr, ASection, []);
  ClearSvcParams(ASvc);
  ASvc.Protocol := FPr;
  ASvc.ServerName := FSvr;
  if FDb <> '' then
    if ASvc is TMDOBackupRestoreService then
      if ASvc is TMDOBackupService then
        TMDOBackupService(ASvc).DatabaseName := FDb
      else
        TMDORestoreService(ASvc).DatabaseName.Text := FDb;
  if FUsr <> '' then
    ASvc.Params.Values['user_name'] := FUsr;
  if FPas <> '' then
    ASvc.Params.Values['password'] := FPas;
end;

{$IFDEF WINDOWS}
procedure LoadDBParamsFromReg(ADB: TMDODatabase; ARootKey: HKEY; AKeyName: String);
var
  Reg: TRegistry;
begin
  ClearDBParams(ADB);
  Reg := TRegistry.Create;
  Reg.RootKey := ARootKey;
  if Reg.OpenKeyReadOnly(AKeyName) then
  begin
    if Reg.ValueExists(REG_DB_NAME) then
      ADB.DatabaseName := Reg.ReadString(REG_DB_NAME);
    if Reg.ValueExists(REG_DB_USER) then
      ADB.Params.Values['user_name'] := Reg.ReadString(REG_DB_USER);
    if Reg.ValueExists(REG_DB_PASSWORD) then
      ADB.Params.Values['password'] := Reg.ReadString(REG_DB_PASSWORD);
    if Reg.ValueExists(REG_DB_LC_TYPE) then
      ADB.Params.Values['lc_ctype'] := Reg.ReadString(INI_DB_LC_TYPE);
  end;
  Reg.Free;
end;

procedure SaveDBParamsToReg(ADataBase, AUser, APasswd, ALCLT: String; ARootKey: HKEY; AKeyName: String);
var
  Reg: TRegistry;
  B: Boolean;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := ARootKey;
  B := Reg.OpenKey(AKeyName, True);
  if  B then
  begin
    Reg.WriteString(REG_DB_NAME, ADataBase);
    Reg.WriteString(REG_DB_USER, AUser);
    Reg.WriteString(REG_DB_PASSWORD, APasswd);
    Reg.WriteString(REG_DB_LC_TYPE, ALCLT);
    Reg.CloseKey;
  end;
  Reg.Free;
  if not B then
    raise Exception.Create('Nie utworzono klucza rejestru.');
end;
{$ENDIF}

procedure ClearDBParams(ADB: TMDODatabase);
begin
  ADB.Close;
  ADB.DatabaseName := '';
  ADB.Params.Clear;
end;

procedure StartTrans(ATransaction: TMDOTransaction);
begin
  if not ATransaction.InTransaction then
    ATransaction.StartTransaction;
end;

procedure CommitTrans(ATransaction: TMDOTransaction; Retaining: Boolean);
begin
  if ATransaction.InTransaction then
    if Retaining then
      ATransaction.CommitRetaining
    else
      ATransaction.Commit;
end;

procedure RollBackTrans(ATransaction: TMDOTransaction);
begin
  if ATransaction.InTransaction then
    ATransaction.Rollback;
end;

procedure LoadStrings(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String; AFieldName: String; AStrings: TStrings);
begin
  LoadStringsWithParam(ADatabase, ATransaction, ASQL, AFieldName, '', Null, AStrings);
end;

procedure LoadStringsWithParam(ADatabase: TMDODataBase; ATransaction: TMDOTransaction; ASQL: String; AFieldName: String; AParamNames: String; AParamValues: Variant; AStrings: TStrings);
var
  Q: TMDOSQL;
begin
  Q := CreateQuery(ADatabase, ATransaction, ASQL, AParamNames, AParamValues);
  Q.ExecQuery;
  while not Q.Eof do
  begin
    AStrings.Add(Q.FieldByName(AFieldName).AsString);
    Q.Next;
  end;
  Q.Close;
  Q.Free;
end;

function BuildDatabaseName(AProtocol: TProtocol; AServer, ADatabase: String
  ): String;
const
  FMT: array[TProtocol] of String = ('%s:%s', '\\%s\%s', '%s@%s', '%1:s');
begin
  Result := Format(FMT[AProtocol], [AServer, ADatabase]);
end;

procedure DecodeDatabaseName(ADatabaseName: String; var AProtocol: TProtocol; var AServer: String; var ADatabase: String);
var
  I: Integer;
  S: String;
begin
  AProtocol := Local;
  ADatabase := ADatabaseName;
  AServer := '';
  I := Pos(':', ADatabaseName);
  if (I <> 0) and (I <> 2) then
  begin
    AProtocol := TCP;
    AServer := Copy(ADatabaseName, 1, I - 1);
    ADatabase := Copy(ADatabaseName, I + 1, Length(ADatabaseName) - I);
    Exit;
  end;
  I := Pos('\\', ADatabaseName);
  if I = 1 then
  begin
    AProtocol := NamedPipe;
    S := Copy(ADatabaseName, 3, Length(ADatabaseName));
    I := Pos('\', S);
    AServer := Copy(S, 1, I - 1);
    ADatabase := Copy(S, I + 1, Length(S));
    Exit;
  end;
  I := Pos('@', ADatabaseName);
  if I > 0 then
  begin
    AProtocol := SPX;
    AServer := Copy(ADatabaseName, 1, I - 1);
    ADatabase := Copy(ADatabaseName, I + 1, Length(ADatabaseName));
  end;
end;

function DumpDBPatams(ADB: TMDODataBase): String;
const
  FRMDB = 'DatabaseName=%s' + LineEnding + 'ClientLib=%s' + LineEnding + 'Params:' + LineEnding + '%s';
  CLIBSTR: array[TMDOClientLib] of String = ('clAutoDetect', 'clGDS32', 'clFBClient', 'clFBEmbed');
begin
  Result := Format(FRMDB, [ADB.DatabaseName, CLIBSTR[ADB.ClientLib], ADB.Params.Text]);
end;

procedure ClearSvcParams(ASvc: TMDOCustomService);
begin
  if ASvc.Active then
    ASvc.Detach;
  ASvc.Params.Clear;
  ASvc.Protocol := Local;
  if ASvc is TMDOBackupService then
   with ASvc as TMDOBackupService do
   begin
     DatabaseName := '';
     BackupFile.Clear;
   end;
  if ASvc is TMDORestoreService then
   with ASvc as TMDORestoreService do
   begin
     DatabaseName.Clear;
     BackupFile.Clear;
   end;
end;

procedure LoadSvcParamsFromDB(ASvc: TMDOCustomService; ADB: TMDODatabase);
var
  Svr, Dbn: String;
  Pr: TProtocol;
begin
  ClearSvcParams(ASvc);
  DecodeDatabaseName(ADB.DatabaseName, Pr, Svr, Dbn);
  ASvc.Protocol := Pr;
  ASvc.ServerName := Svr;
  ASvc.Params.Values['user_name'] := ADB.Params.Values['user_name'];
  ASvc.Params.Values['password'] := ADB.Params.Values['password'];
  if ASvc is TMDOBackupRestoreService then
    if ASvc is TMDOBackupService then
      TMDOBackupService(ASvc).DatabaseName := Dbn
    else
      TMDORestoreService(ASvc).DatabaseName.Text := Dbn;
end;

function StringToProtocol(AStrPtoto: String): TProtocol;
var
  I: TProtocol;
begin
  Result := Local;
  for I := Low(PROTOCOL_STR) to High(PROTOCOL_STR) do
    if UpperCase(AStrPtoto) = UpperCase(PROTOCOL_STR[I]) then
    begin
      Result := I;
      Break;
    end;
end;

end.

