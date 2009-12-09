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

unit MDO;

interface
uses
  {$IFDEF MDO_FPC}
    {$IFDEF UNIX}
  cthreads,
    {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Classes, MDOHeader, MDOExternals, MDOUtils, DB, MDOConst;

type
  TTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc);
  TTraceFlags = set of TTraceFlag;

  EMDOError = class (EDatabaseError)
  private
    FMDOErrorCode: Long;
    FSQLCode: Long;
  public
    constructor Create(ASQLCode: Long; Msg: string); overload;
    constructor Create(ASQLCode: Long; AMDOErrorCode: Long; Msg: string); 
            overload;
    property MDOErrorCode: Long read FMDOErrorCode;
    property SQLCode: Long read FSQLCode;
  end;
  
  EMDOFirebirdError = class (EMDOError);
  EMDOClientError = class (EMDOError);
  EMDOFirebirdPermissionError = class(EMDOError);

  
  TMDODataBaseErrorMessage    = (ShowSQLCode,
                                ShowMDOMessage,
                                ShowSQLMessage);
  TMDODataBaseErrorMessages   = set of TMDODataBaseErrorMessage;
  TMDOClientError            = (
    mdoeUnknownError,
    mdoeFirebirdMissing,
    mdoeFirebirdInstallMissing,
    mdoeIB60feature,
    mdoeNotSupported,
    mdoeNotPermitted,
    mdoeFileAccessError,
    mdoeConnectionTimeout,
    mdoeCannotSetDatabase,
    mdoeCannotSetTransaction,
    mdoeOperationCancelled,
    mdoeDPBConstantNotSupported,
    mdoeDPBConstantUnknown,
    mdoeTPBConstantNotSupported,
    mdoeTPBConstantUnknown,
    mdoeDatabaseClosed,
    mdoeDatabaseOpen,
    mdoeDatabaseNameMissing,
    mdoeNotInTransaction,
    mdoeInTransaction,
    mdoeTimeoutNegative,
    mdoeNoDatabasesInTransaction,
    mdoeUpdateWrongDB,
    mdoeUpdateWrongTR,
    mdoeDatabaseNotAssigned,
    mdoeTransactionNotAssigned,
    mdoeXSQLDAIndexOutOfRange,
    mdoeXSQLDANameDoesNotExist,
    mdoeEOF,
    mdoeBOF,
    mdoeInvalidStatementHandle,
    mdoeSQLOpen,
    mdoeSQLClosed,
    mdoeDatasetOpen,
    mdoeDatasetClosed,
    mdoeUnknownSQLDataType,
    mdoeInvalidColumnIndex,
    mdoeInvalidParamColumnIndex,
    mdoeInvalidDataConversion,
    mdoeColumnIsNotNullable,
    mdoeBlobCannotBeRead,
    mdoeBlobCannotBeWritten,
    mdoeEmptyQuery,
    mdoeCannotOpenNonSQLSelect,
    mdoeNoFieldAccess,
    mdoeFieldReadOnly,
    mdoeFieldNotFound,
    mdoeNotEditing,
    mdoeCannotInsert,
    mdoeCannotPost,
    mdoeCannotUpdate,
    mdoeCannotDelete,
    mdoeCannotRefresh,
    mdoeBufferNotSet,
    mdoeCircularReference,
    mdoeSQLParseError,
    mdoeUserAbort,
    mdoeDataSetUniDirectional,
    mdoeCannotCreateSharedResource,
    mdoeWindowsAPIError,
    mdoeColumnListsDontMatch,
    mdoeColumnTypesDontMatch,
    mdoeCantEndSharedTransaction,
    mdoeFieldUnsupportedType,
    mdoeCircularDataLink,
    mdoeEmptySQLStatement,
    mdoeIsASelectStatement,
    mdoeRequiredParamNotSet,
    mdoeNoStoredProcName,
    mdoeIsAExecuteProcedure,
    mdoeUpdateFailed,
    mdoeNotCachedUpdates,
    mdoeNotLiveRequest,
    mdoeNoProvider,
    mdoeNoRecordsAffected,
    mdoeNoTableName,
    mdoeCannotCreatePrimaryIndex,
    mdoeCannotDropSystemIndex,
    mdoeTableNameMismatch,
    mdoeIndexFieldMissing,
    mdoeInvalidCancellation,
    mdoeInvalidEvent,
    mdoeMaximumEvents,
    mdoeNoEventsRegistered,
    mdoeInvalidQueueing,
    mdoeInvalidRegistration,
    mdoeInvalidBatchMove,
    mdoeSQLDialectInvalid,
    mdoeSPBConstantNotSupported,
    mdoeSPBConstantUnknown,
    mdoeServiceActive,
    mdoeServiceInActive,
    mdoeServerNameMissing,
    mdoeQueryParamsError,
    mdoeStartParamsError,
    mdoeOutputParsingError,
    mdoeUseSpecificProcedures,
    mdoeSQLMonitorAlreadyPresent,
    mdoeCantPrintValue,
    mdoeEOFReached,
    mdoeEOFInComment,
    mdoeEOFInString,
    mdoeParamNameExpected,
    mdoeSuccess,
    mdoeDelphiException,
    mdoeNoOptionsSet,
    mdoeNoDestinationDirectory,
    mdoeNosourceDirectory,
    mdoeNoUninstallFile,
    mdoeOptionNeedsClient,
    mdoeOptionNeedsServer,
    mdoeInvalidOption,
    mdoeInvalidOnErrorResult,
    mdoeInvalidOnStatusResult,
    mdoeDPBConstantUnknownEx,
    mdoeTPBConstantUnknownEx,
    mdoeGeneratorNotDefined
    );

  TStatusVector              = array[0..19] of ISC_STATUS;
  PStatusVector              = ^TStatusVector;


const
  MDOPalette1 = 'Mercury'; {do not localize}
  MDOPalette2 = 'Mercury Admin'; {do not localize}
  MDOPalette3 = 'Mercury Tools';
  SMercuryVersion = 'Mercury Database Objects RC3';

  MDOLocalBufferLength = 512;
  MDOBigLocalBufferLength = MDOLocalBufferLength * 2;
  MDOHugeLocalBufferLength = MDOBigLocalBufferLength * 20;

  MDOErrorMessages: array[TMDOClientError] of string = (
    SUnknownError,
    SFirebirdMissing,
    SFirebirdInstallMissing,
    SIB60feature,
    SNotSupported,
    SNotPermitted,
    SFileAccessError,
    SConnectionTimeout,
    SCannotSetDatabase,
    SCannotSetTransaction,
    SOperationCancelled,
    SDPBConstantNotSupported,
    SDPBConstantUnknown,
    STPBConstantNotSupported,
    STPBConstantUnknown,
    SDatabaseClosed,
    SDatabaseOpen,
    SDatabaseNameMissing,
    SNotInTransaction,
    SInTransaction,
    STimeoutNegative,
    SNoDatabasesInTransaction,
    SUpdateWrongDB,
    SUpdateWrongTR,
    SDatabaseNotAssigned,
    STransactionNotAssigned,
    SXSQLDAIndexOutOfRange,
    SXSQLDANameDoesNotExist,
    SEOF,
    SBOF,
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SDatasetClosed,
    SUnknownSQLDataType,
    SInvalidColumnIndex,
    SInvalidParamColumnIndex,
    SInvalidDataConversion,
    SColumnIsNotNullable,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SEmptyQuery,
    SCannotOpenNonSQLSelect,
    SNoFieldAccess,
    SFieldReadOnly,
    SFieldNotFound,
    SNotEditing,
    SCannotInsert,
    SCannotPost,
    SCannotUpdate,
    SCannotDelete,
    SCannotRefresh,
    SBufferNotSet,
    SCircularReference,
    SSQLParseError,
    SUserAbort,
    SDataSetUniDirectional,
    SCannotCreateSharedResource,
    SWindowsAPIError,
    SColumnListsDontMatch,
    SColumnTypesDontMatch,
    SCantEndSharedTransaction,
    SFieldUnsupportedType,
    SCircularDataLink,
    SEmptySQLStatement,
    SIsASelectStatement,
    SRequiredParamNotSet,
    SNoStoredProcName,
    SIsAExecuteProcedure,
    SUpdateFailed,
    SNotCachedUpdates,
    SNotLiveRequest,
    SNoProvider,
    SNoRecordsAffected,
    SNoTableName,
    SCannotCreatePrimaryIndex,
    SCannotDropSystemIndex,
    STableNameMismatch,
    SIndexFieldMissing,
    SInvalidCancellation,
    SInvalidEvent,
    SMaximumEvents,
    SNoEventsRegistered,
    SInvalidQueueing,
    SInvalidRegistration,
    SInvalidBatchMove,
    SSQLDialectInvalid,
    SSPBConstantNotSupported,
    SSPBConstantUnknown,
    SServiceActive,
    SServiceInActive,
    SServerNameMissing,
    SQueryParamsError,
    SStartParamsError,
    SOutputParsingError,
    SUseSpecificProcedures,
    SSQLMonitorAlreadyPresent,
    SCantPrintValue,
    SEOFReached,
    SEOFInComment,
    SEOFInString,
    SParamNameExpected,
    SSuccess,
    SDelphiException,
    SNoOptionsSet,
    SNoDestinationDirectory,
    SNosourceDirectory,
    SNoUninstallFile,
    SOptionNeedsClient,
    SOptionNeedsServer,
    SInvalidOption,
    SInvalidOnErrorResult,
    SInvalidOnStatusResult,
    SDPBConstantUnknownEx,
    STPBConstantUnknownEx,
    SGeneratorNotDefined
  );

// LoginDialog procedure for database
type
  TLoginDialogProc = function(const DBName: String; var DBUserName, DBPassword: String): Boolean;

var
  LoginDialogProc: TLoginDialogProc = nil;

var
  MDOCS: TRTLCriticalSection;

procedure MDOAlloc(var P; OldSize, NewSize: Integer);

procedure MDOError(ErrMess: TMDOClientError; const Args: array of const);
procedure MDODataBaseError;

function StatusVector: PISC_STATUS;
function StatusVectorArray: PStatusVector;
function CheckStatusVector(ErrorCodes: array of ISC_STATUS): Boolean;
function StatusVectorAsText: string;

procedure SeTMDODataBaseErrorMessages(Value: TMDODataBaseErrorMessages);
function GeTMDODataBaseErrorMessages: TMDODataBaseErrorMessages;

implementation

uses
  {$IFNDEF MDO_FPC}
  DBLogDlg,
  {$ENDIF}
  MDOIntf;

var
  MDODataBaseErrorMessages: TMDODataBaseErrorMessages;
threadvar
  FStatusVector : TStatusVector;

procedure MDOAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

procedure MDOError(ErrMess: TMDOClientError; const Args: array of const);
begin
  raise EMDOClientError.Create(Ord(ErrMess),
                              Format(MDOErrorMessages[ErrMess], Args));
end;

procedure MDODataBaseError;
var
  sqlcode: Long;
  MDOErrorCode: Long;
  local_buffer: array[0..MDOHugeLocalBufferLength - 1] of char;
  usr_msg: string;
  status_vector: PISC_STATUS;
  MDODataBaseErrorMessages: TMDODataBaseErrorMessages;
begin
  usr_msg := '';

  { Get a local reference to the status vector.
    Get a local copy of the MDODataBaseErrorMessages options.
    Get the SQL error code }
  status_vector := StatusVector;
  MDOErrorCode := StatusVectorArray[1];
  MDODataBaseErrorMessages := GeTMDODataBaseErrorMessages;
  sqlcode := isc_sqlcode(status_vector);

  if (ShowSQLCode in MDODataBaseErrorMessages) then
    usr_msg := usr_msg + 'SQLCODE: ' + IntToStr(sqlcode); {do not localize}
  Exclude(MDODataBaseErrorMessages, ShowSQLMessage);
  if (ShowSQLMessage in MDODataBaseErrorMessages) then
  begin
    isc_sql_interprete(sqlcode, local_buffer, MDOLocalBufferLength);
    if (ShowSQLCode in MDODataBaseErrorMessages) then
      usr_msg := usr_msg + CRLF;
    usr_msg := usr_msg + string(local_buffer);
  end;

  if (ShowMDOMessage in MDODataBaseErrorMessages) then
  begin
    if (ShowSQLCode in MDODataBaseErrorMessages) or
       (ShowSQLMessage in MDODataBaseErrorMessages) then
      usr_msg := usr_msg + CRLF;
    while (isc_interprete(local_buffer, @status_vector) > 0) do
    begin
      if (usr_msg <> '') and (usr_msg[Length(usr_msg)] <> LF) then
        usr_msg := usr_msg + CRLF;
      usr_msg := usr_msg + string(local_buffer);
    end;
  end;

  if (usr_msg <> '') and (usr_msg[Length(usr_msg)] = '.') then
    Delete(usr_msg, Length(usr_msg), 1);

  if sqlcode = -551 then
    raise EMDOFirebirdPermissionError.Create(sqlcode, MDOErrorCode, usr_msg)
  else
    raise EMDOFirebirdError.Create(sqlcode, MDOErrorCode, usr_msg);
end;

{ Return the status vector for the current thread }
function StatusVector: PISC_STATUS;
begin
  result := @FStatusVector;
end;

function StatusVectorArray: PStatusVector;
begin
  result := @FStatusVector;
end;

function CheckStatusVector(ErrorCodes: array of ISC_STATUS): Boolean;
var
  p: PISC_STATUS;
  i: Integer;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  p := @FStatusVector;
  result := False;
  while (p^ <> 0) and (not result) do
    case p^ of
      3: NextP(3);
      1, 4:
      begin
        NextP(1);
        i := 0;
        while (i <= High(ErrorCodes)) and (not result) do
        begin
          result := p^ = ErrorCodes[i];
          Inc(i);
        end;
        NextP(1);
      end;
      else
        NextP(2);
    end;
end;

function StatusVectorAsText: string;
var
  p: PISC_STATUS;
  function NextP(i: Integer): PISC_STATUS;
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
    result := p;
  end;
begin
  p := @FStatusVector;
  result := '';
  while (p^ <> 0) do
    if (p^ = 3) then
    begin
      result := result + Format('%d %d %d', [p^, NextP(1)^, NextP(1)^]) + CRLF;
      NextP(1);
    end
    else begin
      result := result + Format('%d %d', [p^, NextP(1)^]) + CRLF;
      NextP(1);
    end;
end;


{ EMDOError }
{
********************************** EMDOError ***********************************
}
constructor EMDOError.Create(ASQLCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

constructor EMDOError.Create(ASQLCode: Long; AMDOErrorCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode :=  ASQLCode;
  FMDOErrorCode := AMDOErrorCode;
end;

procedure SeTMDODataBaseErrorMessages(Value: TMDODataBaseErrorMessages);
begin
  EnterCriticalSection(MDOCS);
  try
    MDODataBaseErrorMessages := Value;
  finally
    LeaveCriticalSection(MDOCS);
  end;
end;

function GeTMDODataBaseErrorMessages: TMDODataBaseErrorMessages;
begin
  EnterCriticalSection(MDOCS);
  try
    result := MDODataBaseErrorMessages;
  finally
    LeaveCriticalSection(MDOCS);
  end;
end;

initialization
  IsMultiThread := True;
  {$IFDEF MDO_FPC}
  InitCriticalSection(MDOCS);
  {$ELSE}
  InitializeCriticalSection(MDOCS);
  LoginDialogProc := @DBLogDlg.LoginDialog; // VCL DBLogDlg
  {$ENDIF}
  MDODataBaseErrorMessages := [ShowSQLMessage, ShowMDOMessage];

finalization
  {$IFDEF MDO_FPC}
  DoneCriticalsection(MDOCS);
  {$ELSE}
  DeleteCriticalSection(MDOCS);
  {$ENDIF}

end.
