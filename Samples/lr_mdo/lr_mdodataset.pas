unit lr_mdodataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_DBComponent, MDODatabase, MDOCustomDataSet,
  contnrs, db;

type
  TlrMDOGetTransactionEvent = procedure(Sender: TObject; var ATransaction: TMDOTransaction) of object;

  { TLR_MDO }

  TLR_MDO = class(TComponent)
  private
    FOnGetTransaction: TlrMDOGetTransactionEvent;
    FTransaction: TMDOTransaction;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnGetTransaction: TlrMDOGetTransactionEvent read FOnGetTransaction write FOnGetTransaction;
    property Transaction: TMDOTransaction read FTransaction write FTransaction;
  end;

  { TQueryParam }

  TQueryParam = class
    ParamType:TFieldType;
    ParamName:string;
    ParamValue:string;
  end;

  { TQueryParamList }

  TQueryParamList = class(TFPObjectList)
    function ParamByName(AParamName:string):TQueryParam;
    function Add(AParamType:TFieldType; const AParamName, AParamValue:string):TQueryParam;
    procedure Assign(AList: TQueryParamList);
  end;

  { TlrMDODataSet }

  TlrMDODataSet = class(TLRDataSetControl)
  private
    FDatabase: String;
    FParams: TQueryParamList;
    function GetSQL: String;
    procedure SetDatabase(AValue: String);
    procedure SetSQL(AValue: String);
    procedure DoMakeParams;
    procedure DoEditParams;
    procedure DSBeforeOpen(ADataSet: TDataSet);
  protected
    procedure SetDataSource(AValue: string); override;
    procedure AfterLoad; override;
  public
    constructor Create(AOwnerPage: TfrPage); override;
    destructor Destroy; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Database: String read FDatabase write SetDatabase;
    property SQL: String read GetSQL write SetSQL;
    property Params: TQueryParamList read FParams write FParams;
  end;

  { TlrMDODataBase }

  TlrMDODataBase = class(TfrNonVisualControl)
  private
    function GetCharSet: String;
    function GetConnected: Boolean;
    function GetDatabaseName: String;
    function GetPassword: String;
    function GetRole: String;
    function GetUserName: String;
    procedure SetCharSet(AValue: String);
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabaseName(AValue: String);
    procedure SetPassword(AValue: String);
    procedure SetRole(AValue: String);
    procedure SetUserName(AValue: String);
  protected
    FDatabase: TMDODatabase;
    FTransaction: TMDOTransaction;
    procedure AfterLoad; override;
    procedure SetName(const AValue: string); override;
  public
    constructor Create(AOwnerPage: TfrPage); override;
    destructor Destroy; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property UserName: String read GetUserName write SetUserName;
    property Password: String read GetPassword write SetPassword;
    property Role: String read GetRole write SetRole;
    property CharSet: String read GetCharSet write SetCharSet;
  end;

procedure Register;

implementation

uses
  LR_Utils, DBPropEdits, PropEdits, Controls, Graphics, LResources,
  lr_mdoeditparams, Forms;

var
  lrMDOComponent: TLR_MDO = nil;

procedure Register;
begin
  RegisterComponents('LazReport',[TLR_MDO]);
end;

{ TLR_MDO }

constructor TLR_MDO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lrMDOComponent := Self;
end;

{ TQueryParamList }

function TQueryParamList.ParamByName(AParamName: string): TQueryParam;
var
  i:integer;
begin
  Result:=nil;
  AParamName:=UpperCase(AParamName);
  for i:=0 to Count - 1 do
  begin
    if UpperCase(TQueryParam(Items[i]).ParamName) = AParamName then
    begin
      Result:=TQueryParam(Items[i]);
      exit;
    end;
  end;
end;

function TQueryParamList.Add(AParamType: TFieldType; const AParamName,
  AParamValue: string): TQueryParam;
begin
  Result:=TQueryParam.Create;
  inherited Add(Result);
  Result.ParamType:=AParamType;
  Result.ParamName:=AParamName;
  Result.ParamValue:=AParamValue;
end;

procedure TQueryParamList.Assign(AList: TQueryParamList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to AList.Count - 1 do
    Add(TQueryParam(AList[I]).ParamType, TQueryParam(AList[I]).ParamName,
      TQueryParam(AList[I]).ParamValue);
end;

{ TlrMDODataBase }

procedure SetValue(ASL: TStrings; AValName: String; AValue: String);
begin
  if AValue <> '' then
    ASL.Values[AValName] := AValue
  else
    if ASL.IndexOfName(AValName) >= 0 then
      ASL.Delete(ASL.IndexOfName(AValName));
end;

function TlrMDODataBase.GetCharSet: String;
begin
  Result := FDatabase.Params.Values['lc_ctype'];
end;

function TlrMDODataBase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TlrMDODataBase.GetDatabaseName: String;
begin
  Result := FDatabase.DatabaseName;
end;

function TlrMDODataBase.GetPassword: String;
begin
  Result := FDatabase.Params.Values['password'];
end;

function TlrMDODataBase.GetRole: String;
begin
  Result := FDatabase.Params.Values['sql_role'];
end;

function TlrMDODataBase.GetUserName: String;
begin
  Result := FDatabase.Params.Values['user_name'];
end;

procedure TlrMDODataBase.SetCharSet(AValue: String);
begin
  SetValue(FDatabase.Params, 'lc_ctype', AValue);
end;

procedure TlrMDODataBase.SetConnected(AValue: Boolean);
begin
  FDatabase.Connected := AValue;
end;

procedure TlrMDODataBase.SetDatabaseName(AValue: String);
begin
  FDatabase.DatabaseName := AValue;
end;

procedure TlrMDODataBase.SetPassword(AValue: String);
begin
  SetValue(FDatabase.Params, 'password', AValue);
end;

procedure TlrMDODataBase.SetRole(AValue: String);
begin
  SetValue(FDatabase.Params, 'sql_role', AValue);
end;

procedure TlrMDODataBase.SetUserName(AValue: String);
begin
  SetValue(FDatabase.Params, 'user_name', AValue);
end;

procedure TlrMDODataBase.AfterLoad;
begin
  inherited AfterLoad;
end;

procedure TlrMDODataBase.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FDatabase.Name := AValue;
end;

constructor TlrMDODataBase.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrMDODatabase';
  FDatabase := TMDODataBase.Create(OwnerForm);
  FTransaction := TMDOTransaction.Create(OwnerForm);
  FDatabase.DefaultTransaction := FTransaction;
  FTransaction.DefaultDatabase := FDatabase;
end;

destructor TlrMDODataBase.Destroy;
begin
  if not (Assigned(OwnerPage) and (OwnerPage is TfrPageDialog)) then
  begin
    FDatabase.Connected := False;
    FDatabase.DefaultTransaction := nil;
    FTransaction.DefaultDatabase := nil;
    FTransaction.Free;
    FDatabase.Free;
  end;
  inherited Destroy;
end;

procedure TlrMDODataBase.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FDatabase.DatabaseName := XML.GetValue(Path + 'DatabaseName/Value', '');
  SetValue(FDatabase.Params, 'user_name', XML.GetValue(Path + 'UserName/Value', ''));
  SetValue(FDatabase.Params, 'password', XML.GetValue(Path + 'Password/Value', ''));
  SetValue(FDatabase.Params, 'lc_ctype', XML.GetValue(Path + 'CharSet/Value', ''));
  SetValue(FDatabase.Params, 'sql_role', XML.GetValue(Path + 'SQLRole/Value', ''));
end;

procedure TlrMDODataBase.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'DatabaseName/Value', FDatabase.DatabaseName);
  XML.SetValue(Path + 'UserName/Value', FDatabase.Params.Values['user_name']);
  XML.SetValue(Path + 'Password/Value', FDatabase.Params.Values['password']);
  XML.SetValue(Path + 'SQLRole/Value', FDatabase.Params.Values['sql_role']);
  XML.SetValue(Path + 'CharSet/Value', FDatabase.Params.Values['lc_ctype']);
end;

procedure TlrMDODataBase.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrMDODataBase then
  begin
    DatabaseName := TlrMDODataBase(Source).DatabaseName;
    UserName := TlrMDODataBase(Source).UserName;
    Password := TlrMDODataBase(Source).Password;
    Role := TlrMDODataBase(Source).Role;
    CharSet := TlrMDODataBase(Source).CharSet;
    Connected := TlrMDODataBase(Source).Connected;
  end;
end;

{ TlrMDODataSet }

function TlrMDODataSet.GetSQL: String;
begin
  Result := TMDODataSet(DataSet).SelectSQL.Text;
end;

procedure TlrMDODataSet.SetDatabase(AValue: String);
var
  D: TComponent;
  T: TMDOTransaction;
begin
  if FDatabase = AValue then Exit;
  FDatabase := AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TMDODataSet(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TMDODataBase)then
  begin
    TMDODataSet(DataSet).DataBase:=TMDODataBase(D);
    if Assigned(lrMDOComponent) then
    begin
      T := lrMDOComponent.Transaction;
      if Assigned(lrMDOComponent.OnGetTransaction) then
        lrMDOComponent.OnGetTransaction(Self, T);
      if T <> nil then
        TMDODataSet(DataSet).Transaction := T;
    end;
  end;
end;

procedure TlrMDODataSet.SetSQL(AValue: String);
begin
  TMDODataSet(DataSet).SelectSQL.Text := AValue;
  DoMakeParams;
end;

procedure TlrMDODataSet.DoMakeParams;

function ParamExist(ADS: TMDODataSet; AParamName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ADS.Params.Count - 1 do
    if UpperCase(ADS.Params.Vars[I].Name) = UpperCase(AParamName) then
    begin
      Result := True;
      Break;
    end;
end;

var
  Q:TMDODataSet;
  i:integer;
begin
  Q:=TMDODataSet(DataSet);
  if Q.Params.Count > 0 then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
    begin
      if not Assigned(FParams.ParamByName(Q.Params[i].Name)) then
        FParams.Add(ftUnknown, Q.Params[i].Name, '');
    end;

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
    begin
      if not ParamExist(Q, TQueryParam(FParams[i]).ParamName) then
        FParams.Delete(i);
    end;
  end
  else
    FParams.Clear;
end;

procedure TlrMDODataSet.DoEditParams;
begin
  lrMDOEditParamsForm:=TlrMDOEditParamsForm.Create(Application);
  lrMDOEditParamsForm.LoadParamList(FParams);
  if lrMDOEditParamsForm.ShowModal = mrOk then
  begin
    lrMDOEditParamsForm.SaveParamList(FParams);
    if Assigned(frDesigner) then
      frDesigner.Modified:=true;
  end;
  lrMDOEditParamsForm.Free;
end;

procedure TlrMDODataSet.DSBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TMDODataSet;
  P:TQueryParam;
begin
  Q:=TMDODataSet(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := Self;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    S:=Q.Params[i].Name;
    P:=FParams.ParamByName(S);
    if Assigned(P) and (P.ParamValue <> '') and (DocMode = dmPrinting) then
    begin
      case P.ParamType of
        ftDate,
        ftDateTime:Q.Params[i].AsDateTime := frParser.Calc(P.ParamValue);
        ftInteger:Q.Params[i].AsInteger := frParser.Calc(P.ParamValue);
        ftFloat:Q.Params[i].AsFloat := frParser.Calc(P.ParamValue);
        ftString:Q.Params[i].AsString := frParser.Calc(P.ParamValue);
      else
        Q.Params[i].Value := frParser.Calc(P.ParamValue);
      end;
    end;
  end;

  if Assigned(Q.Database) then
    if not Q.Database.Connected then Q.Database.Open;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

procedure TlrMDODataSet.SetDataSource(AValue: string);
var
  D:TComponent;
begin
  inherited SetDataSource(AValue);
  D:=frFindComponent(OwnerForm, AValue);
  if Assigned(D) and (D is TDataSource)then
    TMDODataSet(DataSet).DataSource:=TDataSource(D);
end;

procedure TlrMDODataSet.AfterLoad;
var
  D:TComponent;
begin
  D:=frFindComponent(OwnerForm, DataSource);
  if Assigned(D) and (D is TDataSource)then
    TMDODataSet(DataSet).DataSource:=TDataSource(D);

  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TMDODataBase)then
  begin
    TMDODataSet(DataSet).DataBase:=TMDODataBase(D);
    DataSet.Active:=FActive;
  end;
end;

constructor TlrMDODataSet.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrMDODataSet';
  DataSet := TMDODataSet.Create(OwnerForm);
  FParams := TQueryParamList.Create;
  DataSet.BeforeOpen := @DSBeforeOpen;
end;

destructor TlrMDODataSet.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function StrToFieldType(AStrTypeName:string):TFieldType;
var
  i:TFieldType;
begin
  Result:=ftUnknown;
  AStrTypeName:=UpperCase(AStrTypeName);
  for i in TFieldType do
  begin
    if UpperCase(Fieldtypenames[i]) = AStrTypeName then
    begin
      Result:=i;
      exit;
    end;
  end;
end;

procedure TlrMDODataSet.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C, I: Integer;
begin
  inherited LoadFromXML(XML, Path);
  TMDODataSet(DataSet).SelectSQL.Text := XML.GetValue(Path + 'SQL/Value', '');
  FDatabase := XML.GetValue(Path + 'Database/Value', '');
  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams.Add(
        StrToFieldType(XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', '')),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', ''),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '')
        );
end;

procedure TlrMDODataSet.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i:integer;
  P:TQueryParam;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'SQL/Value', TMDODataSet(DataSet).SelectSQL.Text);
  XML.SetValue(Path + 'Database/Value', FDatabase);
  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    P:=TQueryParam(FParams[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', P.ParamName);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', P.ParamValue);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', Fieldtypenames[P.ParamType]);
  end;
end;

procedure TlrMDODataSet.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrMDODataSet then
  begin
    Database := TlrMDODataSet(Source).Database;
    SQL := TlrMDODataSet(Source).SQL;
    Params.Assign(TlrMDODataSet(Source).Params);
  end;
end;

type
  TlrMDOQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { TLRZConnectionProtocolProperty }

  TlrMDODataBaselProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TlrMDODataSetSQLProperty }

  TlrMDODataSetSQLProperty = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

{ TLRSQLQuerySQLProperty }

function TlrMDODataSetSQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TlrMDODataSetSQLProperty.GetValue: ansistring;
begin
  Result:='(SQL)';
end;

procedure TlrMDODataSetSQLProperty.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  AString : string;
begin
  AString := GetStrValue;
  TheDialog := TStringsPropEditorDlg.Create(nil);
  try
    TheDialog.Editor := Self;
    TheDialog.Memo.Text := AString;
    TheDialog.MemoChange(nil);
    if (TheDialog.ShowModal = mrOK) then
    begin
      AString := TheDialog.Memo.Text;
      //erase the last lineending if any
      if Copy(AString, length(AString) - length(LineEnding) + 1, length(LineEnding)) = LineEnding then
        Delete(AString, length(AString) - length(LineEnding) + 1, length(LineEnding));
      SetStrValue(AString);
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TLRZConnectionProtocolProperty }

procedure TlrMDODataBaselProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TlrMDODataSet) then
    frGetComponents(nil, TMDODataBase, Values, nil);
end;

{ TLRZQueryParamsProperty }

function TlrMDOQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TlrMDOQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TlrMDOQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TlrMDODataSet) then
    TlrMDODataSet(GetComponent(0)).DoEditParams;
end;

var
  BMP_lrMDODataSet: TBitmap = nil;
  BMP_lrMDODataBase: TBitmap = nil;

initialization
  {$I lr_mdo.lrs}
  if not Assigned(BMP_lrMDODataBase) then
  begin
    BMP_lrMDODataBase := TBitmap.Create;
    BMP_lrMDODataBase.LoadFromLazarusResource('TLRMDODATABASE');
  end;
  frRegisterObject(TlrMDODataBase, BMP_lrMDODataBase, 'TlrMDODataBase', nil, otlUIControl, nil);

  if not Assigned(BMP_lrMDODataSet) then
  begin
    BMP_lrMDODataSet := TBitmap.Create;
    BMP_lrMDODataSet.LoadFromLazarusResource('TLRMDODATASET');
  end;
  frRegisterObject(TlrMDODataSet, BMP_lrMDODataSet, 'TlrMDODataSet', nil, otlUIControl, nil);

  RegisterPropertyEditor(TypeInfo(string), TlrMDODataSet, 'Database', TlrMDODataBaselProperty);
  RegisterPropertyEditor(TypeInfo(string), TlrMDODataSet, 'SQL', TlrMDODataSetSQLProperty);
  RegisterPropertyEditor(TypeInfo(TQueryParamList), TlrMDODataSet, 'Params', TlrMDOQueryParamsProperty);

finalization
  if Assigned(BMP_lrMDODataBase) then
    BMP_lrMDODataBase.Free;
  if Assigned(BMP_lrMDODataSet) then
    BMP_lrMDODataSet.Free;
end.

