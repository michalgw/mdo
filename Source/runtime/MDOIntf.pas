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

unit MDOIntf;

{$I ..\MDO.inc}

interface

uses
  MDOHeader, MDOInstallHeader, MDOExternals;

var
  BLOB_get: TBLOB_get;
  BLOB_put: TBLOB_put;
  isc_sqlcode: Tisc_sqlcode;
  isc_sqlcode_s: Tisc_sqlcode_s;
  isc_sql_interprete: Tisc_sql_interprete;
  isc_interprete: Tisc_interprete;
  fb_interpret: Tfb_interpret;
  fb_sqlstate: Tfb_sqlstate;
  isc_vax_integer: Tisc_vax_integer;
  isc_blob_info: Tisc_blob_info;
  isc_open_blob2: Tisc_open_blob2;
  isc_close_blob: Tisc_close_blob;
  isc_get_segment: Tisc_get_segment;
  isc_put_segment: Tisc_put_segment;
  isc_create_blob2: Tisc_create_blob2;
  isc_service_attach: Tisc_service_attach;
  isc_service_detach: Tisc_service_detach;
  isc_service_query: Tisc_service_query;
  isc_service_start: Tisc_service_start;
  isc_decode_date: Tisc_decode_date;
  isc_decode_sql_date: Tisc_decode_sql_date;
  isc_decode_sql_time: Tisc_decode_sql_time;
  isc_decode_timestamp: Tisc_decode_timestamp;
  isc_encode_date: Tisc_encode_date;
  isc_encode_sql_date: Tisc_encode_sql_date;
  isc_encode_sql_time: Tisc_encode_sql_time;
  isc_encode_timestamp: Tisc_encode_timestamp;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_execute2: Tisc_dsql_execute2;
  isc_dsql_execute: Tisc_dsql_execute;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_fetch: Tisc_dsql_fetch;
  isc_dsql_sql_info: Tisc_dsql_sql_info;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_prepare: Tisc_dsql_prepare;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_describe: Tisc_dsql_describe;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_drop_database: Tisc_drop_database;
  isc_detach_database: Tisc_detach_database;
  isc_attach_database: Tisc_attach_database;
  isc_database_info: Tisc_database_info;
  isc_start_multiple: Tisc_start_multiple;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_cancel_events: Tisc_cancel_events;
  isc_que_events: Tisc_que_events;
  isc_event_counts: Tisc_event_counts;
  isc_event_block: Tisc_event_block;
  isc_event_block_a: Tisc_event_block_a;
  isc_event_block_s: Tisc_event_block_s;
  isc_free: Tisc_free;
  isc_add_user   : Tisc_add_user;
  isc_delete_user: Tisc_delete_user;
  isc_modify_user: Tisc_modify_user;

  isc_install_clear_options: Tisc_install_clear_options;
  isc_install_execute: Tisc_install_execute;
  isc_install_get_info: Tisc_install_get_info;
  isc_install_get_message: Tisc_install_get_message;
  isc_install_load_external_text: Tisc_install_load_external_text;
  isc_install_precheck: Tisc_install_precheck;
  isc_install_set_option: Tisc_install_set_option;
  isc_uninstall_execute: Tisc_uninstall_execute;
  isc_uninstall_precheck: Tisc_uninstall_precheck;
  isc_install_unset_option: Tisc_install_unset_option;

{ Library Initialization }
procedure LoadFBLibrary;
procedure FreeFBLibrary;
procedure LoadFBInstallLibrary;
procedure FreeFBInstallLibrary;
function TryFBLoad: Boolean;
procedure CheckFBLoaded;
function GetFBClientVersion: Integer;
procedure CheckFBInstallLoaded;

{ Stubs for 6.0 only functions }
function isc_rollback_retaining_stub(status_vector   : PISC_STATUS;
              tran_handle     : PISC_TR_HANDLE):
                                     ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_service_attach_stub(status_vector      : PISC_STATUS;
                                 isc_arg2           : UShort;
                                 isc_arg3           : PChar;
                                 service_handle     : PISC_SVC_HANDLE;
                                 isc_arg5           : UShort;
                                 isc_arg6           : PChar):
                                 ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_service_detach_stub(status_vector      : PISC_STATUS;
                                 service_handle     : PISC_SVC_HANDLE):
                                 ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_service_query_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar;
                                isc_arg6             : UShort;
                                isc_arg7             : PChar;
                                isc_arg8             : UShort;
                                isc_arg9             : PChar):
                                ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_service_start_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar):
                                ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_encode_sql_date_stub(tm_date           : PCTimeStructure;
                 ib_date           : PISC_DATE);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_encode_sql_time_stub(tm_date           : PCTimeStructure;
                   ib_time           : PISC_TIME);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_encode_timestamp_stub(tm_date          : PCTimeStructure;
                  ib_timestamp     : PISC_TIMESTAMP);
                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_decode_sql_date_stub(ib_date           : PISC_DATE;
                                   tm_date           : PCTimeStructure);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_decode_sql_time_stub(ib_time           : PISC_TIME;
                                   tm_date           : PCTimeStructure);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

procedure isc_decode_timestamp_stub(ib_timestamp     : PISC_TIMESTAMP;
                                    tm_date          : PCTimeStructure);
                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ stubs for install functions }
function isc_install_clear_options_stub(hOption: POPTIONS_HANDLE):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_execute_stub(hOption: OPTIONS_HANDLE;
                             src_dir: TEXT;
                             dest_dir: TEXT;
                             status_func: FP_STATUS;
                             status_data: pointer;
                             error_func: FP_ERROR;
                             error_data: pointer;
                             uninstal_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_get_info_stub(info_type :integer;
                              option :OPT;
                              info_buffer : Pointer;
                              buf_len : Cardinal): MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_get_message_stub(hOption: OPTIONS_HANDLE;
                                 message_no: MSG_NO;
                                 message_txt: Pointer;
                                 message_len: Cardinal):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_load_external_text_stub(msg_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_precheck_stub(hOption: OPTIONS_HANDLE;
                              src_dir: TEXT;
                              dest_dir: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_set_option_stub(hOption: POPTIONS_HANDLE;
                                option: OPT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_uninstall_execute_stub(uninstall_file_name: TEXT;
                               status_func: FP_STATUS;
                               status_data: pointer;
                               error_func: FP_ERROR;
                               error_data: pointer):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_uninstall_precheck_stub(uninstall_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
function isc_install_unset_option_stub(hOption: POPTIONS_HANDLE;
                                  option: OPT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

implementation

uses
  dynlibs,
  Sysutils, MDO;

var
  FBLibrary: THandle;
  FBInstallLibrary: THandle;
  FBClientVersion: Integer;

procedure LoadFBLibrary;

  function GetProcAddr(ProcName: String): Pointer;
  begin
    Result := GetProcAddress(FBLibrary, ProcName);
    if not Assigned(Result) then
      RaiseLastOSError;
  end;

  procedure TryLoadLibrary(aLibrary: string);
  begin
    if FBLibrary = NilHandle then
    begin
      FBLibrary := LoadLibrary(aLibrary);
      if FBLibrary > NilHandle then
         FBASE_CURRENT_DLL := aLibrary;
    end;
  end;

begin
  // If no dll has been specified, try to load the library in the order:
  // Embedded, FBClient, GDS32
  if FBASE_CURRENT_DLL = FBASE_DLL_AUTODETECT then
  begin
    TryLoadLibrary(FBASE_DLL_GDS32);
    TryLoadLibrary(FBASE_DLL_FBClient);
    TryLoadLibrary(FBASE_DLL_FBEMBED);
  end
  else
    TryLoadLibrary(FBASE_CURRENT_DLL);

  if (FBLibrary > NilHandle) then
  begin
    {$IFDEF MDO_DEBUG}
    WriteLn('Library loaded: ' + FBASE_CURRENT_DLL);
    {$ENDIF}
    BLOB_get := TBLOB_get(GetProcAddr('BLOB_get')); {do not localize}
    BLOB_put := TBLOB_put(GetProcAddr('BLOB_put')); {do not localize}
    isc_sqlcode := Tisc_sqlcode(GetProcAddr('isc_sqlcode')); {do not localize}
    isc_sqlcode_s := Tisc_sqlcode_s(GetProcAddr('isc_sqlcode_s')); {do not localize}
    isc_sql_interprete := Tisc_sql_interprete(GetProcAddr('isc_sql_interprete')); {do not localize}
    isc_interprete := Tisc_interprete(GetProcAddr('isc_interprete')); {do not localize}
    fb_interpret := Tfb_interpret(GetProcAddr('fb_interpret')); {do not localize}
    fb_sqlstate := Tfb_sqlstate(GetProcAddr('fb_sqlstate')); {do not localize}
    isc_vax_integer := Tisc_vax_integer(GetProcAddr('isc_vax_integer')); {do not localize}
    isc_blob_info := Tisc_blob_info(GetProcAddr('isc_blob_info')); {do not localize}
    isc_open_blob2 := Tisc_open_blob2(GetProcAddr('isc_open_blob2')); {do not localize}
    isc_close_blob := Tisc_close_blob(GetProcAddr('isc_close_blob')); {do not localize}
    isc_get_segment := Tisc_get_segment(GetProcAddr('isc_get_segment')); {do not localize}
    isc_put_segment := Tisc_put_segment(GetProcAddr('isc_put_segment')); {do not localize}
    isc_create_blob2 := Tisc_create_blob2(GetProcAddr('isc_create_blob2')); {do not localize}
    isc_decode_date := Tisc_decode_date(GetProcAddr('isc_decode_date')); {do not localize}
    isc_encode_date := Tisc_encode_date(GetProcAddr('isc_encode_date')); {do not localize}
    isc_dsql_free_statement := Tisc_dsql_free_statement(GetProcAddr('isc_dsql_free_statement')); {do not localize}
    isc_dsql_execute2 := Tisc_dsql_execute2(GetProcAddr('isc_dsql_execute2')); {do not localize}
    isc_dsql_execute := Tisc_dsql_execute(GetProcAddr('isc_dsql_execute')); {do not localize}
    isc_dsql_set_cursor_name := Tisc_dsql_set_cursor_name(GetProcAddr('isc_dsql_set_cursor_name')); {do not localize}
    isc_dsql_fetch := Tisc_dsql_fetch(GetProcAddr('isc_dsql_fetch')); {do not localize}
    isc_dsql_sql_info := Tisc_dsql_sql_info(GetProcAddr('isc_dsql_sql_info')); {do not localize}
    isc_dsql_alloc_statement2 := Tisc_dsql_alloc_statement2(GetProcAddr('isc_dsql_alloc_statement2')); {do not localize}
    isc_dsql_prepare := Tisc_dsql_prepare(GetProcAddr('isc_dsql_prepare')); {do not localize}
    isc_dsql_describe_bind := Tisc_dsql_describe_bind(GetProcAddr('isc_dsql_describe_bind')); {do not localize}
    isc_dsql_describe := Tisc_dsql_describe(GetProcAddr('isc_dsql_describe')); {do not localize}
    isc_dsql_execute_immediate := Tisc_dsql_execute_immediate(GetProcAddr('isc_dsql_execute_immediate')); {do not localize}
    isc_drop_database := Tisc_drop_database(GetProcAddr('isc_drop_database')); {do not localize}
    isc_detach_database := Tisc_detach_database(GetProcAddr('isc_detach_database')); {do not localize}
    isc_attach_database := Tisc_attach_database(GetProcAddr('isc_attach_database')); {do not localize}
    isc_database_info := Tisc_database_info(GetProcAddr('isc_database_info')); {do not localize}
    isc_start_multiple := Tisc_start_multiple(GetProcAddr('isc_start_multiple')); {do not localize}
    isc_commit_transaction := Tisc_commit_transaction(GetProcAddr('isc_commit_transaction')); {do not localize}
    isc_commit_retaining := Tisc_commit_retaining(GetProcAddr('isc_commit_retaining')); {do not localize}
    isc_rollback_transaction := Tisc_rollback_transaction(GetProcAddr('isc_rollback_transaction')); {do not localize}
    isc_cancel_events := Tisc_cancel_events(GetProcAddr('isc_cancel_events')); {do not localize}
    isc_que_events := Tisc_que_events(GetProcAddr('isc_que_events')); {do not localize}
    isc_event_counts := Tisc_event_counts(GetProcAddr('isc_event_counts')); {do not localize}
    isc_event_block := Tisc_event_block(GetProcAddr('isc_event_block')); {do not localize}
    isc_event_block_a := Tisc_event_block_a(GetProcAddr('isc_event_block_a')); {do not localize}
    isc_event_block_s := Tisc_event_block_s(GetProcAddr('isc_event_block_s')); {do not localize}
    isc_free := Tisc_free(GetProcAddr('isc_free')); {do not localize}
    isc_add_user := Tisc_add_user(GetProcAddr('isc_add_user')); {do not localize}
    isc_delete_user := Tisc_delete_user(GetProcAddr('isc_delete_user')); {do not localize}
    isc_modify_user := Tisc_modify_user(GetProcAddr('isc_modify_user')); {do not localize}

    FBClientVersion := 6;
    isc_rollback_retaining := Tisc_rollback_retaining(GetProcAddress(FBLibrary, 'isc_rollback_retaining')); {do not localize}
    if Assigned(isc_rollback_retaining) then
    begin
      isc_service_attach := Tisc_service_attach(GetProcAddr('isc_service_attach')); {do not localize}
      isc_service_detach := Tisc_service_detach(GetProcAddr('isc_service_detach')); {do not localize}
      isc_service_query := Tisc_service_query(GetProcAddr('isc_service_query')); {do not localize}
      isc_service_start := Tisc_service_start(GetProcAddr('isc_service_start')); {do not localize}
      isc_decode_sql_date := Tisc_decode_sql_date(GetProcAddr('isc_decode_sql_date')); {do not localize}
      isc_decode_sql_time := Tisc_decode_sql_time(GetProcAddr('isc_decode_sql_time')); {do not localize}
      isc_decode_timestamp := Tisc_decode_timestamp(GetProcAddr('isc_decode_timestamp')); {do not localize}
      isc_encode_sql_date := Tisc_encode_sql_date(GetProcAddr('isc_encode_sql_date')); {do not localize}
      isc_encode_sql_time := Tisc_encode_sql_time(GetProcAddr('isc_encode_sql_time')); {do not localize}
      isc_encode_timestamp := Tisc_encode_timestamp(GetProcAddr('isc_encode_timestamp')); {do not localize}
    end else
    begin
      FBClientVersion := 5;
      isc_rollback_retaining := @isc_rollback_retaining_stub;
      isc_service_attach := @isc_service_attach_stub;
      isc_service_detach := @isc_service_detach_stub;
      isc_service_query := @isc_service_query_stub;
      isc_service_start := @isc_service_start_stub;
      isc_decode_sql_date := @isc_decode_sql_date_stub;
      isc_decode_sql_time := @isc_decode_sql_time_stub;
      isc_decode_timestamp := @isc_decode_timestamp_stub;
      isc_encode_sql_date := @isc_encode_sql_date_stub;
      isc_encode_sql_time := @isc_encode_sql_time_stub;
      isc_encode_timestamp := @isc_encode_timestamp_stub;
    end;
  end;
end;

procedure FreeFBLibrary;
begin
  if FBLibrary > NilHandle then
  begin
    FreeLibrary(FBLibrary);
    FBLibrary := 0;
  end;
end;

procedure LoadFBInstallLibrary;

  function GetProcAddr(ProcName: PChar): Pointer;
  begin
    Result := GetProcAddress(FBInstallLibrary, ProcName);
    if not Assigned(Result) then
      RaiseLastOSError;
  end;

begin
  FBInstallLibrary := LoadLibrary(IB_INSTALL_DLL);
  if (FBInstallLibrary > NilHandle) then
  begin
    isc_install_clear_options := Tisc_install_clear_options(GetProcAddr('isc_install_clear_options')); {do not localize}
    isc_install_execute := Tisc_install_execute(GetProcAddr('isc_install_execute')); {do not localize}
    isc_install_get_info := Tisc_install_get_info(GetProcAddr('isc_install_get_info')); {do not localize}
    isc_install_get_message := Tisc_install_get_message(GetProcAddr('isc_install_get_message')); {do not localize}
    isc_install_load_external_text := Tisc_install_load_external_text(GetProcAddr('isc_install_load_external_text')); {do not localize}
    isc_install_precheck := Tisc_install_precheck(GetProcAddr('isc_install_precheck')); {do not localize}
    isc_install_set_option := Tisc_install_set_option(GetProcAddr('isc_install_set_option')); {do not localize}
    isc_uninstall_execute := Tisc_uninstall_execute(GetProcAddr('isc_uninstall_execute')); {do not localize}
    isc_uninstall_precheck := Tisc_uninstall_precheck(GetProcAddr('isc_uninstall_precheck')); {do not localize}
    isc_install_unset_option := Tisc_install_unset_option(GetProcAddr('isc_install_unset_option')); {do not localize}
  end
  else begin
    isc_install_clear_options := @isc_install_clear_options_stub;
    isc_install_execute := @isc_install_execute_stub;
    isc_install_get_info := @isc_install_get_info_stub;
    isc_install_get_message := @isc_install_get_message_stub;
    isc_install_load_external_text := @isc_install_load_external_text_stub;
    isc_install_precheck := @isc_install_precheck_stub;
    isc_install_set_option := @isc_install_set_option_stub;
    isc_uninstall_execute := @isc_uninstall_execute_stub;
    isc_uninstall_precheck := @isc_uninstall_precheck_stub;
    isc_install_unset_option := @isc_install_unset_option_stub;
  end;
end;

procedure FreeFBInstallLibrary;
begin
  if FBInstallLibrary > NilHandle then
  begin
    FreeLibrary(FBInstallLibrary);
    FBInstallLibrary := 0;
  end;
end;

function TryFBLoad: Boolean;
begin
  if (FBLibrary <= NilHandle) then
    LoadFBLibrary;
  if (FBLibrary <= NilHandle) then
    result := False
  else
    result := True;
end;

procedure CheckFBLoaded;
begin
  if not TryFBLoad then
    MDOError(mdoeFirebirdMissing, [nil]);
end;

function GetFBClientVersion: Integer;
begin
  CheckFBLoaded;
  result := FBClientVersion;
end;

procedure CheckFBInstallLoaded;
begin
  if (FBInstallLibrary <= NilHandle) then
    LoadFBInstallLibrary;
  if (FBInstallLibrary <= NilHandle) then
    MDOError(mdoeFirebirdInstallMissing, [nil]);
end;

function isc_rollback_retaining_stub(status_vector   : PISC_STATUS;
              tran_handle     : PISC_TR_HANDLE):
                                     ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_rollback_retaining']); {do not localize}
end;

function isc_service_attach_stub(status_vector      : PISC_STATUS;
                                 isc_arg2           : UShort;
                                 isc_arg3           : PChar;
                                 service_handle     : PISC_SVC_HANDLE;
                                 isc_arg5           : UShort;
                                 isc_arg6           : PChar):
                                 ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_service_attach']); {do not localize}
end;

function isc_service_detach_stub(status_vector      : PISC_STATUS;
                                 service_handle     : PISC_SVC_HANDLE):
                                 ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_service_detach']); {do not localize}
end;

function isc_service_query_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar;
                                isc_arg6             : UShort;
                                isc_arg7             : PChar;
                                isc_arg8             : UShort;
                                isc_arg9             : PChar):
                                ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_service_query']); {do not localize}
end;

function isc_service_start_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar):
                                ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_service_start']); {do not localize}
end;

procedure isc_encode_sql_date_stub(tm_date           : PCTimeStructure;
                 ib_date           : PISC_DATE);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_encode_sql_date']); {do not localize}
end;

procedure isc_encode_sql_time_stub(tm_date           : PCTimeStructure;
                   ib_time           : PISC_TIME);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_encode_sql_time']); {do not localize}
end;

procedure isc_encode_timestamp_stub(tm_date          : PCTimeStructure;
                  ib_timestamp     : PISC_TIMESTAMP);
                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_encode_sql_timestamp']); {do not localize}
end;

procedure isc_decode_sql_date_stub(ib_date           : PISC_DATE;
                                   tm_date           : PCTimeStructure);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_decode_sql_date']); {do not localize}
end;

procedure isc_decode_sql_time_stub(ib_time           : PISC_TIME;
                                   tm_date           : PCTimeStructure);
                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_decode_sql_time']); {do not localize}
end;

procedure isc_decode_timestamp_stub(ib_timestamp     : PISC_TIMESTAMP;
                                    tm_date          : PCTimeStructure);
                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  MDOError(mdoeIB60feature, ['isc_decode_timestamp']); {do not localize}
end;

function isc_install_clear_options_stub(hOption: POPTIONS_HANDLE):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_execute_stub(hOption: OPTIONS_HANDLE;
                             src_dir: TEXT;
                             dest_dir: TEXT;
                             status_func: FP_STATUS;
                             status_data: pointer;
                             error_func: FP_ERROR;
                             error_data: pointer;
                             uninstal_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_get_info_stub(info_type :integer;
                              option :OPT;
                              info_buffer : Pointer;
                              buf_len : Cardinal): MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_get_message_stub(hOption: OPTIONS_HANDLE;
                                 message_no: MSG_NO;
                                 message_txt: Pointer;
                                 message_len: Cardinal):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_load_external_text_stub(msg_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_precheck_stub(hOption: OPTIONS_HANDLE;
                              src_dir: TEXT;
                              dest_dir: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_set_option_stub(hOption: POPTIONS_HANDLE;
                                option: OPT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_uninstall_execute_stub(uninstall_file_name: TEXT;
                               status_func: FP_STATUS;
                               status_data: pointer;
                               error_func: FP_ERROR;
                               error_data: pointer):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_uninstall_precheck_stub(uninstall_file_name: TEXT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

function isc_install_unset_option_stub(hOption: POPTIONS_HANDLE;
                                  option: OPT):MSG_NO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  Result := 0;
  MDOError(mdoeIB60feature, ['isc_install_xxx ']); {do not localize}
end;

initialization

finalization
  FreeFBLibrary;
  FreeFBInstallLibrary;
end.
