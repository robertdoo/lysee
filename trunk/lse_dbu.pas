{==============================================================================}
{        UNIT: lse_dbu                                                         }
{ DESCRIPTION: database functions                                              }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2008-2010, Li Yun Jie                                          }
{ All rights reserved.                                                         }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ Redistributions of source code must retain the above copyright notice, this  }
{ list of conditions and the following disclaimer.                             }
{                                                                              }
{ Redistributions in binary form must reproduce the above copyright notice,    }
{ this list of conditions and the following disclaimer in the documentation    }
{ and/or other materials provided with the distribution.                       }
{                                                                              }
{ Neither the name of Li Yun Jie nor the names of its contributors may         }
{ be used to endorse or promote products derived from this software without    }
{ specific prior written permission.                                           }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    }
{ IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   }
{ ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  }
{ ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       }
{ DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   }
{ SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   }
{ CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           }
{ LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    }
{ OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  }
{ DAMAGE.                                                                      }
{==============================================================================}
{ The Initial Developer of the Original Code is Li Yun Jie (CHINA).            }
{ Portions created by Li Yun Jie are Copyright (C) 2008-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_dbu;

{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, Db, lseu;

type

  PLseDB = ^RLseDB;
  PLseDS = ^RLseDS;

  RLseTUPSP = packed record {<--for C interface}
    target  : PLseString;
    user    : PLseString;
    password: PLseString;
    source  : PLseString;
    params  : PLseString;
  end;
  PLseTUPSP  = ^RLseTUPSP;

  TLseDS_addref = function(dso: PLseDS): integer;cdecl;
  TLseDS_release = function(dso: PLseDS): integer;cdecl;
  TLseDS_count = function(dso: PLseDS): integer;cdecl;
  TLseDS_getfn = function(dso: PLseDS; index: integer): PLseString;cdecl;
  TLseDS_getft = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_getfi = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_getfs = function(dso: PLseDS; index: integer): PLseString;cdecl;
  TLseDS_getfd = function(dso: PLseDS; index: integer): double;cdecl;
  TLseDS_getfb = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_isnull = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_length = function(dso: PLseDS): integer;cdecl;
  TLseDS_bof = function(dso: PLseDS): integer;cdecl;
  TLseDS_eof = function(dso: PLseDS): integer;cdecl;
  TLseDS_seek = procedure(dso: PLseDS; Offset, Origin: integer);cdecl;
  TLseDS_close = procedure(dso: PLseDS);cdecl;
  TLseDS_getSQL = function(dso: PLseDS): PLseString;cdecl;
  TLseDS_setSQL = procedure(dso: PLseDS; const SQL: pchar);cdecl;
  TLseDS_open = procedure(dso: PLseDS);cdecl;
  TLseDS_active = function(dso: PLseDS): integer;cdecl;
  TLseDS_getBMK = function(dso: PLseDS): pointer;cdecl;
  TLseDS_gotoBMK = procedure(dso: PLseDS; Bookmark: pointer);cdecl;
  TLseDS_freeBMK = procedure(dso: PLseDS; Bookmark: pointer);cdecl;

  RLseDS = packed record
    ds_size    : cardinal;       {<--record size}
    ds_error   : pchar;          {<--error message}
    ds_errno   : integer;        {<--error number}
    ds_object  : pointer;        {<--dataset object}
    ds_db      : PLseDB;         {<--database record}
    ds_addref  : TLseDS_addref;  {<--increase reference count}
    ds_release : TLseDS_release; {<--decrease reference count}
    ds_count   : TLseDS_count;   {<--field count}
    ds_getfn   : TLseDS_getfn;   {<--get field name}
    ds_getft   : TLseDS_getft;   {<--get field type}
    ds_getfi   : TLseDS_getfi;   {<--get integer field value}
    ds_getfs   : TLseDS_getfs;   {<--get string field value}
    ds_getfd   : TLseDS_getfd;   {<--get float field value}
    ds_getfb   : TLseDS_getfb;   {<--get logic field value}
    ds_isnull  : TLseDS_isnull;  {<--if is NULL ?}
    ds_length  : TLseDS_length;  {<--result record count}
    ds_bof     : TLseDS_bof;     {<--at first record}
    ds_eof     : TLseDS_eof;     {<--at the end of dataset}
    ds_seek    : TLseDS_seek;    {<--move to specified record}
    ds_getSQL  : TLseDS_getSQL;  {<--get current SQL statement}
    ds_setSQL  : TLseDS_setSQL;  {<--set SQL statement}
    ds_open    : TLseDS_open;    {<--query dataset from database}
    ds_close   : TLseDS_close;   {<--close dataset}
    ds_active  : TLseDS_active;  {<--if is active ?}
    ds_getBMK  : TLseDS_getBMK;  {<--get current bookmark}
    ds_gotoBMK : TLseDS_gotoBMK; {<--seek to specified bookmark}
    ds_freeBMK : TLseDS_freeBMK; {<--free specified bookmark}
  end;

  TLseDB_addref = function(dbo: PLseDB): integer;cdecl;
  TLseDB_release = function(dbo: PLseDB): integer;cdecl;
  TLseDB_getConnStr = function(dbo: PLseDB): PLseString;cdecl;
  TLseDB_setConnStr = procedure(dbo: PLseDB; const ConnString: pchar);cdecl;
  TLseDB_connect = procedure(dbo: PLseDB);cdecl;
  TLseDB_connected = function(dbo: PLseDB): integer;cdecl;
  TLseDB_disconnect = procedure(dbo: PLseDB);cdecl;
  TLseDB_dataset = function(dbo: PLseDB): PLseDS;cdecl;
  TLseDB_execSQL = function(dbo: PLseDB; const SQL: pchar): integer;cdecl;
  TLseDB_transact = procedure(dbo: PLseDB);cdecl;
  TLseDB_transacting = function(dbo: PLseDB): integer;cdecl;
  TLseDB_commit = procedure(dbo: PLseDB);cdecl;
  TLseDB_rollback = procedure(dbo: PLseDB);cdecl;
  TLseDB_storedprocs = function(dbo: PLseDB): PLseString;cdecl;
  TLseDB_tables = function(dbo: PLseDB; IncSysTable: integer): PLseString;cdecl;
  TLseDB_fields = function(dbo: PLseDB; const Table: pchar): PLseString;cdecl;
  TLseDB_escape = function(dbo: PLseDB; const S: pchar): PLseString;cdecl;

  RLseDB = packed record
    db_size       : cardinal;           {<--record size}
    db_error      : pchar;              {<--error message}
    db_errno      : integer;            {<--error number}
    db_object     : pointer;            {<--database object}
    db_addref     : TLseDB_addref;      {<--increase reference count}
    db_release    : TLseDB_release;     {<--decrease reference count}
    db_getConnStr : TLseDB_getConnStr;  {<--get connection string}
    db_setConnStr : TLseDB_setConnStr;  {<--set connection string}
    db_connect    : TLseDB_connect;     {<--connect to database}
    db_connected  : TLseDB_connected;   {<--if is connected ?}
    db_disconnect : TLseDB_disconnect;  {<--close database connection}
    db_dataset    : TLseDB_dataset;     {<--create a dataset}
    db_execSQL    : TLseDB_execSQL;     {<--execute SQL statement}
    db_transact   : TLseDB_transact;    {<--begin transaction}
    db_transacting: TLseDB_transacting; {<--if is in transaction ?}
    db_commit     : TLseDB_commit;      {<--commit transaction}
    db_rollback   : TLseDB_rollback;    {<--rollback transaction}
    db_storedprocs: TLseDB_storedprocs; {<--comma delimited stored procedures}
    db_tables     : TLseDB_tables;      {<--comma delimited tables}
    db_fields     : TLseDB_fields;      {<--comma delimited field names}
    db_escape     : TLseDB_escape;      {<--escape string value}
  end;

  RLseVendor = packed record
    dv_name: array[0..31] of char; // MSSQL FB/IB POSTGRES ACCESS ODBC ....
    dv_desc: array[0..91] of char;
    dv_create: function:PLseDB;cdecl;
  end;
  PLseVendor = ^RLseVendor;

{======================================================================)
(======== database vendor =============================================)
(======================================================================)
( lse_dbv_register: register database vendor
( lse_dbv_check   : check dbv record for registration
( lse_dbv_provide : create new database agent
(----------------------------------------------------------------------}
function lse_dbv_check(dbv: PLseVendor): boolean;
function lse_dbv_register(dbv: PLseVendor): boolean;
function lse_dbv_provide(dbv: PLseVendor): PLseDB;

{======================================================================)
(======== TUPSP - encode/decode connection string =====================)
(======================================================================)
( lse_encode_TUPSP: encode database connection parametres
( lse_decode_TUPSP: decode database connection parametres
(----------------------------------------------------------------------}
function  lse_encode_TUPSP(const Target, User, Password, Source, Params: string): string;
procedure lse_decode_TUPSP(const S: string; var Target, User, Password, Source, Params: string);

procedure lse_set_db(V: PLseValue; Value: PLseDB);
procedure lse_set_ds(V: PLseValue; Value: PLseDS);

{======================================================================)
(======== database ====================================================)
(======================================================================)
( lse_db_addref     : increase refcount
( lse_db_release    : decrease refcount
( lse_db_check      : check database error state
( lse_db_create     : create database by vendor name
( lse_db_execSQL    : execute SQL statement
( lse_db_openSQL    : open SQL statement and return a dataset
( lse_db_tables     : get ',' delimited table names
( lse_db_storedprocs: get ',' delimited stored procedure names
( lse_db_disconnect : close database connection
( lse_db_connecTo   : open database connection
( lse_db_reconnect  : reopen database connection
( lse_db_connected  : test if database connection is active
( lse_db_transact   : start transaction
( lse_db_transacting: test if in transaction
( lse_db_commit     : commit current transaction
( lse_db_rollback   : rollback current transaction
( lse_db_escape     : escape string value for SQL statement usage
( lse_no_comment    : split SQL and clear comments
(----------------------------------------------------------------------}
function  lse_db_addref(const dbo: pointer): integer;cdecl;
function  lse_db_release(const dbo: pointer): integer;cdecl;
procedure lse_db_check(dbo: PLseDB);
function  lse_db_create(const Vendor: string): PLseDB;
function  lse_db_execSQL(dbo: PLseDB; const SQL: pchar): integer;
function  lse_db_openSQL(dbo: PLseDB; const SQL: pchar): PLseDS;
function  lse_db_tables(dbo: PLseDB): string;
function  lse_db_storedprocs(dbo: PLseDB): string;
procedure lse_db_disconnect(dbo: PLseDB);
procedure lse_db_connecTo(dbo: PLseDB; const Target, User, Password, Source, Params: string);
procedure lse_db_reconnect(dbo: PLseDB);
function  lse_db_connected(dbo: PLseDB): boolean;
procedure lse_db_transact(dbo: PLseDB);
function  lse_db_transacting(dbo: PLseDB): boolean;
procedure lse_db_commit(dbo: PLseDB; Check: boolean);
procedure lse_db_rollback(dbo: PLseDB; Check: boolean);
function  lse_db_escape(dbo: PLseDB; SV: PLseString): PLseString;
function  lse_no_comment(const SQL: string; L: TStrings; EscBackslash: boolean): string;

{======================================================================)
(======== dataset =====================================================)
(======================================================================)
( lse_ds_addref     : increase refcount
( lse_ds_release    : decrease refcount
( lse_ds_check      : check dataset error state
( lse_ds_active     : test if dataset is active
( lse_ds_close      : close dataset
( lse_ds_eof        : test if at the end of the dataset
( lse_ds_bof        : test if at the head of the dataset
( lse_ds_count      : record count
( lse_ds_next       : move to next record
( lse_ds_prior      : move to prior record
( lse_ds_first      : move to first record
( lse_ds_last       : move to last record
( lse_ds_field_count: get field count
( lse_ds_field_name : get field name (string) by index
( lse_ds_fname      : get field name (PLseString) by index
( lse_ds_indexof    : get field index by name
( lse_ds_field_type : get field type record by index
( lse_ds_getf?      : get field value by index
(----------------------------------------------------------------------}
function  lse_ds_addref(const dso: pointer): integer;cdecl;
function  lse_ds_release(const dso: pointer): integer;cdecl;
procedure lse_ds_check(dso: PLseDS);
function  lse_ds_active(dso: PLseDS): boolean;
procedure lse_ds_close(dso: PLseDS);
function  lse_ds_eof(dso: PLseDS): boolean;
function  lse_ds_bof(dso: PLseDS): boolean;
function  lse_ds_count(dso: PLseDS): integer;
procedure lse_ds_next(dso: PLseDS);
procedure lse_ds_prior(dso: PLseDS);
procedure lse_ds_first(dso: PLseDS);
procedure lse_ds_last(dso: PLseDS);
function  lse_ds_field_count(dso: PLseDS): integer;
function  lse_ds_vary_index(dso: PLseDS; Index: integer): integer;
function  lse_ds_field_name(dso: PLseDS; Index: integer; VaryIndex: boolean): string;
function  lse_ds_fname(dso: PLseDS; Index: integer; VaryIndex: boolean): PLseString;
function  lse_ds_indexof(dso: PLseDS; const FieldName: string): integer;overload;
function  lse_ds_indexof(dso: PLseDS; const FieldName: pchar): integer;overload;
function  lse_ds_field_vtype(dso: PLseDS; Index: integer): integer;
function  lse_ds_field_class(dso: PLseDS; Index: integer): PLseType;
function  lse_ds_getfi(dso: PLseDS; Index: integer): integer;
function  lse_ds_getfs(dso: PLseDS; Index: integer): PLseString;
function  lse_ds_getfb(dso: PLseDS; Index: integer): boolean;
function  lse_ds_getfd(dso: PLseDS; Index: integer): double;

function __getft_name(field_type: TFieldType): string;
function __getft_lsv(field_type: TFieldType): integer;

{-----------------------------------------------------------------------
( F_NAME: __getfX
(
( F_DESC: get field type or fetch field value
(
( F_ARGS: field: TField - a field
(
( F_TYPE: IBDICS - something wanted
(
( EXCEPT:
(----------------------------------------------------------------------}
function __getft(field: TField): integer;
function __getfb(field: TField): boolean;
function __getfd(field: TField): double;
function __getfi(field: TField): integer;
function __getfm(field: TField): currency;
function __getfs(field: TField): string;

function otos_dataset(inst: pointer): PLseString;cdecl;
function cvgr_dataset(obj, kernel_engine: pointer): PLseVargen;cdecl;
function getiv_ds(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function getpv_ds(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function length_ds(obj: pointer): integer;cdecl;

(*
{ database }

procedure pp_database_execSQL(const Param: PLseParam);cdecl;
procedure pp_database_openSQL(const Param: PLseParam);cdecl;
procedure pp_database_tables(const Param: PLseParam);cdecl;
procedure pp_database_procedures(const Param: PLseParam);cdecl;
procedure pp_database_connecTo(const Param: PLseParam);cdecl;
procedure pp_database_disconnect(const Param: PLseParam);cdecl;
procedure pp_database_reconnect(const Param: PLseParam);cdecl;
procedure pp_database_transact(const Param: PLseParam);cdecl;
procedure pp_database_inTransaction(const Param: PLseParam);cdecl;
procedure pp_database_commit(const Param: PLseParam);cdecl;
procedure pp_database_commitRetaining(const Param: PLseParam);cdecl;
procedure pp_database_commitAndTransact(const Param: PLseParam);cdecl;
procedure pp_database_rollback(const Param: PLseParam);cdecl;
procedure pp_database_rollbackRetaining(const Param: PLseParam);cdecl;
procedure pp_database_rollbackAndTransact(const Param: PLseParam);cdecl;
procedure pp_database_connected(const Param: PLseParam);cdecl;
procedure pp_database_escape(const Param: PLseParam);cdecl;

{ dataset }

procedure pp_dataset_close(const Param: PLseParam);cdecl;
procedure pp_dataset_open(const Param: PLseParam);cdecl;
procedure pp_dataset_first(const Param: PLseParam);cdecl;
procedure pp_dataset_last(const Param: PLseParam);cdecl;
procedure pp_dataset_prior(const Param: PLseParam);cdecl;
procedure pp_dataset_next(const Param: PLseParam);cdecl;
procedure pp_dataset_eof(const Param: PLseParam);cdecl;
procedure pp_dataset_bof(const Param: PLseParam);cdecl;
procedure pp_dataset_count(const Param: PLseParam);cdecl;
procedure pp_dataset_indexOf(const Param: PLseParam);cdecl;
procedure pp_dataset_field_name(const Param: PLseParam);cdecl;
procedure pp_dataset_field_type(const Param: PLseParam);cdecl;
procedure pp_dataset_string(const Param: PLseParam);cdecl;
procedure pp_dataset_int(const Param: PLseParam);cdecl;
procedure pp_dataset_float(const Param: PLseParam);cdecl;

const
  KTN_DB = 'database';
  KTD_DB = 'database';
  KTN_DS = 'dataset';
  KTD_DS = 'dataset';

func_count = 33;
func_array: array[0..func_count - 1] of RLseFunc = (
    { database }

    (fr_prot:'database_create:database |vendor:string|';
     fr_addr:@pp_database_create;
     fr_desc:'create database object';
    ),
    (fr_prot:'database_execSQL:int |db:database, SQL:string|';
     fr_addr:@pp_database_execSQL;
     fr_desc:'execute SQL command';
    ),
    (fr_prot:'database_openSQL:dataset |db:database, SQL:string|';
     fr_addr:@pp_database_openSQL;
     fr_desc:'query dataset';
    ),
    (fr_prot:'database_get_tables:varlist |db:database|';
     fr_addr:@pp_database_tables;
     fr_desc:'get table name list';
    ),
    (fr_prot:'database_get_procedures:varlist |db:database|';
     fr_addr:@pp_database_procedures;
     fr_desc:'get stored procedure name list';
    ),
    (fr_prot:'database_connecTo:void |db:database, target:string, user:string, password:string, source:string, params:string|';
     fr_addr:@pp_database_connecTo;
     fr_desc:'connect to target database: Access, Mssql, ODBC';
    ),
    (fr_prot:'database_disconnect:void |db:database|';
     fr_addr:@pp_database_disconnect;
     fr_desc:'close database connection';
    ),
    (fr_prot:'database_reconnect:void |db:database|';
     fr_addr:@pp_database_reconnect;
     fr_desc:'reopen database connection';
    ),
    (fr_prot:'database_get_connected:int |db:database|';
     fr_addr:@pp_database_connected;
     fr_desc:'test if database connection is active';
    ),
    (fr_prot:'database_transact:void |db:database|';
     fr_addr:@pp_database_transact;
     fr_desc:'begin transaction';
    ),
    (fr_prot:'database_get_inTransaction:int |db:database|';
     fr_addr:@pp_database_inTransaction;
     fr_desc:'test if is in transaction';
    ),
    (fr_prot:'database_commit:void |db:database|';
     fr_addr:@pp_database_commit;
     fr_desc:'commit transaction';
    ),
    (fr_prot:'database_commitRetaining:void |db:database|';
     fr_addr:@pp_database_commitRetaining;
     fr_desc:'commit retaining transactions';
    ),
    (fr_prot:'database_commitAndTransact:void |db:database|';
     fr_addr:@pp_database_commitAndTransact;
     fr_desc:'commit retaining and start a new transaction';
    ),
    (fr_prot:'database_rollback:void |db:database|';
     fr_addr:@pp_database_rollback;
     fr_desc:'rollback transaction';
    ),
    (fr_prot:'database_rollbackRetaining:void |db:database|';
     fr_addr:@pp_database_rollbackRetaining;
     fr_desc:'rollback retaining transactions';
    ),
    (fr_prot:'database_rollbackAndTransact:void |db:database|';
     fr_addr:@pp_database_rollbackAndTransact;
     fr_desc:'rollback retaining and start a new transaction';
    ),
    (fr_prot:'database_escape:string |db:database, stringValue:string|';
     fr_addr:@pp_database_escape;
     fr_desc:'escape string value';
    ),

    { dataset }

    (fr_prot:'dataset_indexOf:int |ds:dataset, fieldName:string|';
     fr_addr:@pp_dataset_indexOf;
     fr_desc:'get field index by name';
    ),
    (fr_prot:'dataset_close:void |ds:dataset|';
     fr_addr:@pp_dataset_close;
     fr_desc:'close current dataset';
    ),
    (fr_prot:'dataset_open:dataset |ds:dataset, SQL:string|';
     fr_addr:@pp_dataset_open;
     fr_desc:'close current dataset and start a new query';
    ),
    (fr_prot:'dataset_get_count:int |ds:dataset|';
     fr_addr:@pp_dataset_count;
     fr_desc:'get record count';
    ),
    (fr_prot:'dataset_first:void |ds:dataset|';
     fr_addr:@pp_dataset_first;
     fr_desc:'seek to first record';
    ),
    (fr_prot:'dataset_last:void |ds:dataset|';
     fr_addr:@pp_dataset_last;
     fr_desc:'seek to last record';
    ),
    (fr_prot:'dataset_prior:void |ds:dataset|';
     fr_addr:@pp_dataset_prior;
     fr_desc:'seek to prior record';
    ),
    (fr_prot:'dataset_next:void |ds:dataset|';
     fr_addr:@pp_dataset_next;
     fr_desc:'seek to next record';
    ),
    (fr_prot:'dataset_get_eof:int |ds:dataset|';
     fr_addr:@pp_dataset_eof;
     fr_desc:'test if is at the end of the dataset';
    ),
    (fr_prot:'dataset_get_bof:int |ds:dataset|';
     fr_addr:@pp_dataset_bof;
     fr_desc:'test if is at the head of the dataset';
    ),
    (fr_prot:'dataset_name:string |ds:dataset, index:int|';
     fr_addr:@pp_dataset_field_name;
     fr_desc:'get field name by index';
    ),
    (fr_prot:'dataset_type:type |ds:dataset, fieldNameOrIndex|';
     fr_addr:@pp_dataset_field_type;
     fr_desc:'get field type by name or index';
    ),
    (fr_prot:'dataset_string:string |ds:dataset, fieldNameOrIndex|';
     fr_addr:@pp_dataset_string;
     fr_desc:'get string field value by name or index';
    ),
    (fr_prot:'dataset_int:int |ds:dataset, fieldNameOrIndex|';
     fr_addr:@pp_dataset_int;
     fr_desc:'get integer field value by name or index';
    ),
    (fr_prot:'dataset_float:float |ds:dataset, fieldNameOrIndex|';
     fr_addr:@pp_dataset_float;
     fr_desc:'get float field value by name or index';
    )
  );
*)

var
    KR_DS, KR_DB: PLseType;
implementation

uses
  Math;

function stod(const S: string): extended;
begin
  Result := StrToFloatDef(S, 0);
end;

function otos_dataset(inst: pointer): PLseString;cdecl;
var
  this: PLseDS;
  bm: pointer;
  ss: TStringStream;
  index, count: integer;
  sr: PLseString;
begin
  Result := nil;
  this := PLseDS(inst);
  if this <> nil then
  begin
    count := lse_ds_field_count(this);
    ss := TStringStream.Create('');
    try
      bm := this^.ds_getBMK(this);
      lse_ds_check(this);
      try
        // 1. list field names
        for index := 0 to count - 1 do
        begin
          if index > 0 then
            lse_stream_write(ss, ',');
          sr := lse_ds_fname(this, index, false);
          lse_strec_inclife(sr);
          lse_stream_write(ss, sr);
          lse_strec_declife(sr);
        end;
        lse_stream_writeln(ss);

        // 2. seek to first record
        lse_ds_first(this);

        // 3. show all records
        while not lse_ds_eof(this) do
        begin
          for index := 0 to count - 1 do
          begin
            if index > 0 then
              lse_stream_write(ss, ',');
            sr := lse_ds_getfs(this, index);
            lse_strec_inclife(sr);
            lse_stream_write(ss, sr);
            lse_strec_declife(sr);
          end;
          lse_stream_writeln(ss);
          lse_ds_next(this);
        end;
      finally
        this^.ds_gotoBMK(this, bm);
        lse_ds_check(this);
        this^.ds_freeBMK(this, bm);
        lse_ds_check(this);
      end;
      Result := lse_strec_alloc(ss.DataString);
    finally
      ss.Free;
    end;
  end;
end;

type
  RLiVG_dataset = packed record
    vgrec: RLseVargen;
    vgref: integer;
    dsrec: PLseDS;
    nextf: boolean;
  end;
  PLiVG_dataset = ^RLiVG_dataset;

function cvgr_dataset_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    nextf := false;
    lse_ds_first(dsrec);
    Result := Ord(not lse_ds_eof(dsrec));
  end;
end;

function cvgr_dataset_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    if nextf then
    begin
      nextf := false;
      lse_ds_next(dsrec);
    end;
    Result := Ord(not lse_ds_eof(dsrec));
  end;
end;

function cvgr_dataset_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    if nextf then
    begin
      nextf := false;
      lse_ds_next(dsrec);
    end;
    if not lse_ds_eof(dsrec) then
    begin
      lse_set_object(Value, KR_DS, dsrec);
      nextf := true;
      Result := 1;
    end
    else Result := 0;
  end;
end;

function cvgr_dataset_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_dataset;
begin
  cvgr := PLiVG_dataset(vrec^.vg_data);
  with cvgr^ do
  begin
    lse_ds_addref(dsrec);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_dataset));
  end;
end;

function cvgr_dataset_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_dataset;
begin
  cvgr := PLiVG_dataset(vrec^.vg_data);
  with cvgr^ do
  begin
    lse_ds_release(dsrec);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_dataset));
  end;
end;

function cvgr_dataset(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  dsrec: PLseDS;
  cvgr: PLiVG_dataset;
begin
  dsrec := PLseDS(obj);
  if dsrec <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_dataset));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_dataset_rewind;
    cvgr^.vgrec.vg_has_next := @cvgr_dataset_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_dataset_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_dataset_addref;
    cvgr^.vgrec.vg_release := @cvgr_dataset_release;
    cvgr^.vgref := 0;
    cvgr^.dsrec := dsrec;
    cvgr^.nextf := false;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

function getiv_ds(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  D: PLseDS;
  L: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    D := PLseDS(obj);
    L := lse_ds_field_count(D);
    index := lse_vary_index(index, L);
    if (index >= 0) and (index < L) then
    begin
      case lse_ds_field_vtype(D, index) of
        LSV_INT  : lse_set_int64(value, lse_ds_getfi(D, index));
        LSV_FLOAT: lse_set_float(value, lse_ds_getfd(D, index));
       {lSV_STRING, LSV_CHAR}
        else lse_set_string(value, lse_ds_getfs(D, index));
      end;
      Result := 1;
    end;
  end;
end;

function getpv_ds(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  this: PLseDS;
  index: integer;
begin
  Result := 0;
  if obj <> nil then
  begin
    this := PLseDS(obj);
    index := lse_ds_indexof(this, name);
    if index >= 0 then
    begin
      case lse_ds_field_vtype(this, index) of
        LSV_INT  : lse_set_int64(value, lse_ds_getfi(this, index));
        LSV_FLOAT: lse_set_float(value, lse_ds_getfd(this, index));
       {lSV_STRING, LSV_CHAR}
        else lse_set_string(value, lse_ds_getfs(this, index));
      end;
      Result := 1;
    end;
  end;
end;

function length_ds(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := lse_ds_field_count(obj) else
    Result := 0;
end;

{======================================================================)
(======== lse_dbv - database vendor functions =========================)
(======================================================================}

function lse_dbv_check(dbv: PLseVendor): boolean;
begin
  Result := (dbv <> nil) and
            (dbv^.dv_name[0] <> #0) and
            Assigned(dbv^.dv_create);
end;

function lse_dbv_register(dbv: PLseVendor): boolean;
begin
  Result := false; // lse_entries^.cik_dbv_register(dbv) <> 0;
end;

function lse_dbv_provide(dbv: PLseVendor): PLseDB;
begin
  Result := dbv^.dv_create();
end;

{======================================================================)
(======== TUPSP - encode/decode connection string =====================)
(======================================================================}

function lse_encode_TUPSP(const Target, User, Password, Source, Params: string): string;

  function Encode(const Text: string): string;
  var
    index: integer;
    S: string;
  begin
    Result := '';
    S := Trim(Text);
    for index := 1 to Length(S) do
      case S[index] of
        ';': Result := Result + '%c';
        '%': Result := Result + '%%';
        else Result := Result + S[index];
      end;
  end;

begin
  Result := Format('%s,%s,%s,%s,%s', [Encode(Target), Encode(User),
    Encode(Password), Encode(Source), Encode(Params)]);
end;

procedure lse_decode_TUPSP(const S: string;
  var Target, User, Password, Source, Params: string);

  function Decode(const Text: string): string;
  var
    index: integer;
    S: string;
  begin
    Result := '';
    S := Trim(Text);
    index := 1;
    while index <= Length(S) do
    begin
      if S[index] = '%' then
      begin
        Inc(index);
        if index <= Length(S) then
          case S[index] of
            'c': Result := Result + ';';
            '%': Result := Result + '%';
          end;
      end
      else Result := Result + S[index];
      Inc(index);
    end;
  end;

  function GetNext: string;
  var
    index: integer;
  begin
    index := Pos(',', Params);
    if index > 0 then
    begin
      Result := Trim(Copy(Params, 1, index - 1));
      Params := Trim(Copy(Params, index + 1, MaxInt));
    end
    else
    begin
      Result := Params;
      Params := '';
    end;
    Result := Decode(Result);
  end;

begin
  Params   := Trim(S);
  Target   := GetNext;
  User     := GetNext;
  Password := GetNext;
  Source   := GetNext;
  Params   := Decode(Params);
end;

procedure lse_set_db(V: PLseValue; Value: PLseDB);
begin
  lse_set_object(V, KR_DB, Value);
end;

procedure lse_set_ds(V: PLseValue; Value: PLseDS);
begin
  lse_set_object(V, KR_DS, Value);
end;

{======================================================================)
(======== database ====================================================)
(======================================================================}

function lse_db_addref(const dbo: pointer): integer;cdecl;
var
  this: PLseDB;
begin
  this := PLseDB(dbo);
  if this <> nil then
    Result := this^.db_addref(this) else
    Result := 0;
end;

function lse_db_release(const dbo: pointer): integer;cdecl;
var
  this: PLseDB;
begin
  this := PLseDB(dbo);
  if this <> nil then
    Result := this^.db_release(this) else
    Result := 0;
end;

procedure lse_db_check(dbo: PLseDB);
begin
  if dbo^.db_errno <> 0 then
    raise TLseException.Create(dbo^.db_error);
end;

function lse_db_create(const Vendor: string): PLseDB;
begin
  Result := nil; //lse_entries^.cik_dbv_provide(pchar(Vendor));
end;

function lse_db_execSQL(dbo: PLseDB; const SQL: pchar): integer;
begin
  Result := dbo^.db_execSQL(dbo, SQL);
  lse_db_check(dbo);
end;

function lse_db_openSQL(dbo: PLseDB; const SQL: pchar): PLseDS;
begin
  Result := dbo^.db_dataset(dbo);
  lse_db_check(dbo);
  try
    Result^.ds_setSQL(Result, SQL);
    lse_ds_check(Result);
    Result^.ds_open(Result);
    lse_ds_check(Result);
  except
    lse_ds_addref(Result);
    lse_ds_release(Result);
    raise;
  end;
end;

function lse_db_tables(dbo: PLseDB): string;
var
  srec: PLseString;
begin
  if Assigned(dbo^.db_tables) then
  begin
    srec := dbo^.db_tables(dbo, 0);
    lse_db_check(dbo);
    lse_strec_inclife(srec);
    Result := LowerCase(lse_strec_string(srec));
    lse_strec_declife(srec);
  end
  else Result := '';
end;

function lse_db_storedprocs(dbo: PLseDB): string;
var
  srec: PLseString;
begin
  if Assigned(dbo^.db_storedprocs) then
  begin
    srec := dbo^.db_storedprocs(dbo);
    lse_db_check(dbo);
    lse_strec_inclife(srec);
    Result := LowerCase(lse_strec_string(srec));
    lse_strec_declife(srec);
  end
  else Result := '';
end;

procedure lse_db_disconnect(dbo: PLseDB);
begin
  dbo^.db_disconnect(dbo);
  lse_db_check(dbo);
end;

procedure lse_db_connecTo(dbo: PLseDB; const Target, User, Password, Source, Params: string);
var
  cons: string;
begin
  lse_db_disconnect(dbo);

  cons := '[' + lse_encode_TUPSP(Target, User, Password, Source, Params) + ']';

  dbo^.db_setConnStr(dbo, pchar(cons));
  lse_db_check(dbo);

  dbo^.db_connect(dbo);
  lse_db_check(dbo);
end;

procedure lse_db_reconnect(dbo: PLseDB);
begin
  if not lse_db_connected(dbo) then
  begin
    dbo^.db_connect(dbo);
    lse_db_check(dbo);
  end;
end;

function lse_db_connected(dbo: PLseDB): boolean;
begin
  Result := (dbo^.db_connected(dbo) <> 0);
  lse_db_check(dbo);
end;

procedure lse_db_transact(dbo: PLseDB);
begin
  dbo^.db_transact(dbo);
  lse_db_check(dbo);
end;

function lse_db_transacting(dbo: PLseDB): boolean;
begin
  Result := (dbo^.db_transacting(dbo) <> 0);
  lse_db_check(dbo);
end;

procedure lse_db_commit(dbo: PLseDB; Check: boolean);
begin
  if not Check or lse_db_transacting(dbo) then
  begin
    dbo^.db_commit(dbo);
    lse_db_check(dbo);
  end;
end;

procedure lse_db_rollback(dbo: PLseDB; Check: boolean);
begin
  if not Check or lse_db_transacting(dbo) then
  begin
    dbo^.db_rollback(dbo);
    lse_db_check(dbo);
  end;
end;

function lse_db_escape(dbo: PLseDB; SV: PLseString): PLseString;
begin
  if Assigned(dbo^.db_escape) then
  begin
    Result := dbo^.db_escape(dbo, lse_strec_data(SV));
    lse_db_check(dbo);
  end
  else Result := SV;
end;

function lse_no_comment(const SQL: string; L: TStrings; EscBackslash: boolean): string;
const
  TrimChar = [#$09, #$0A, #$0C, #$0D, #$20, ';'];
var
  index, count, base: integer;
  cache: string;

  procedure clear_line_comment;
  begin
    repeat
      cache[index] := ' ';
      Inc(index);
    until (index > count) or (cache[index] in [#13, #10]);
  end;

  procedure clear_block_comment;
  begin
    repeat
      if cache[index] = '*' then
        if (index < count) and (cache[index + 1] = '/') then
        begin
          cache[index] := ' ';
          Inc(index);
          cache[index] := ' ';
          Break;
        end;
      if not (cache[index] in [#13, #10]) then
        cache[index] := ' ';
      Inc(index);
    until index > count;
  end;

  procedure skip_string(strc: char);
  begin
    Inc(index);
    while (index <= count) and (cache[index] <> strc) do
      if (cache = '\') and EscBackslash then
        Inc(index, 2) else
        Inc(index);
  end;

  procedure add_sql;
  var
    tmp: string;
  begin
    if L <> nil then
    begin
      tmp := Trim(Copy(cache, base, index - base));
      if tmp <> '' then
        L.Add(tmp);
    end;
    base := index + 1;
  end;

begin
  if L <> nil then L.Clear;
  cache := SQL;
  count := Length(cache);
  if count > 0 then
  begin
    index := 1;
    base := 1;
    while index < count do
    begin
      case cache[index] of
       '''': skip_string('''');
        '"': skip_string('"');
        '-': if cache[index + 1] = '-' then clear_line_comment;
        '/': if cache[index + 1] = '*' then clear_block_comment else
             if cache[index + 1] = '/' then clear_line_comment;
        ';': add_sql;
      end;
      Inc(index);
    end;
    Inc(index);
    add_sql;
    count := Length(cache);
    while (count > 0) and (cache[count] in TrimChar) do
      Dec(count);
    SetLength(cache, count);
  end;
  Result := cache;
end;

{======================================================================)
(======== dataset =====================================================)
(======================================================================}

function lse_ds_addref(const dso: pointer): integer;cdecl;
var
  this: PLseDS;
begin
  this := PLseDS(dso);
  if this <> nil then
    Result := this^.ds_addref(this) else
    Result := 0;
end;

function lse_ds_release(const dso: pointer): integer;cdecl;
var
  this: PLseDS;
begin
  this := PLseDS(dso);
  if this <> nil then
    Result := this^.ds_release(this) else
    Result := 0;
end;

procedure lse_ds_check(dso: PLseDS);
begin
  if dso^.ds_errno <> 0 then
    raise Exception.Create(dso^.ds_error);
end;

function lse_ds_active(dso: PLseDS): boolean;
begin
  Result := (dso^.ds_active(dso) <> 0);
  lse_ds_check(dso);
end;

procedure lse_ds_close(dso: PLseDS);
begin
  dso^.ds_close(dso);
  lse_ds_check(dso);
end;

function lse_ds_eof(dso: PLseDS): boolean;
begin
  Result := (dso^.ds_eof(dso) <> 0);
  lse_ds_check(dso);
end;

function lse_ds_bof(dso: PLseDS): boolean;
begin
  Result := (dso^.ds_bof(dso) <> 0);
  lse_ds_check(dso);
end;

function lse_ds_count(dso: PLseDS): integer;
begin
  Result := dso^.ds_length(dso);
  lse_ds_check(dso);
end;

procedure lse_ds_next(dso: PLseDS);
begin
  dso^.ds_seek(dso, 1, 1);
  lse_ds_check(dso);
end;

procedure lse_ds_prior(dso: PLseDS);
begin
  dso^.ds_seek(dso, -1, 1);
  lse_ds_check(dso);
end;

procedure lse_ds_first(dso: PLseDS);
begin
  dso^.ds_seek(dso, 0, 0);
  lse_ds_check(dso);
end;

procedure lse_ds_last(dso: PLseDS);
begin
  dso^.ds_seek(dso, 0, 2);
  lse_ds_check(dso);
end;

function lse_ds_field_count(dso: PLseDS): integer;
begin
  Result := dso^.ds_count(dso);
  lse_ds_check(dso);
end;

function  lse_ds_vary_index(dso: PLseDS; Index: integer): integer;
var
  count: integer;
begin
  count := lse_ds_field_count(dso);
  Result := lse_vary_index(Index, count);
  lse_check_index(Result, count);
end;

function lse_ds_field_name(dso: PLseDS; Index: integer; VaryIndex: boolean): string;
var
  srec: PLseString;
begin
  srec := lse_ds_fname(dso, Index, VaryIndex);
  lse_strec_inclife(srec);
  Result := lse_strec_string(srec);
  lse_strec_declife(srec);
end;

function lse_ds_fname(dso: PLseDS; Index: integer; VaryIndex: boolean): PLseString;
var
  count: integer;
begin
  if VaryIndex then
  begin
    count := lse_ds_field_count(dso);
    index := lse_vary_index(index, count);
    lse_check_index(index, count);
  end;
  Result := dso^.ds_getfn(dso, Index);
  lse_ds_check(dso);
end;

function lse_ds_indexof(dso: PLseDS; const FieldName: string): integer;
begin
  Result := lse_ds_indexof(dso, pchar(FieldName));
end;

function lse_ds_indexof(dso: PLseDS; const FieldName: pchar): integer;overload;
var
  index, count: integer;
  fname: PLseString;
begin
  Result := -1;
  if (FieldName <> nil) and (FieldName^ <> #0) then
  begin
    count := lse_ds_field_count(dso);
    index := 0;
    while index < count do
    begin
      fname := lse_ds_fname(dso, index, false);
      lse_strec_inclife(fname);
      if StrIComp(FieldName, lse_strec_data(fname)) = 0 then
      begin
        lse_strec_declife(fname);
        Result := index;
        Exit;
      end;
      lse_strec_declife(fname);
      Inc(index);
    end;
  end;
end;

function lse_ds_field_vtype(dso: PLseDS; Index: integer): integer;
begin
  Result := dso^.ds_getft(dso, Index);
  lse_ds_check(dso);
  if not (Result in [LSV_INT, LSV_FLOAT]) then
    Result := LSV_STRING;
end;

function lse_ds_field_class(dso: PLseDS; Index: integer): PLseType;
begin
  case lse_ds_field_vtype(dso, Index) of
    LSV_INT   : Result := lse_type(LSV_INT);
    lSV_STRING: Result := lse_type(LSV_STRING);
    LSV_FLOAT : Result := lse_type(LSV_FLOAT);
           else Result := lse_type(LSV_STRING);
  end;
end;

function lse_ds_getfi(dso: PLseDS; Index: integer): integer;
begin
  Result := dso^.ds_getfi(dso, Index);
  lse_ds_check(dso);
end;

function lse_ds_getfs(dso: PLseDS; Index: integer): PLseString;
begin
  Result := dso^.ds_getfs(dso, Index);
  lse_ds_check(dso);
end;

function lse_ds_getfb(dso: PLseDS; Index: integer): boolean;
begin
  Result := (dso^.ds_getfb(dso, Index) <> 0);
  lse_ds_check(dso);
end;

function lse_ds_getfd(dso: PLseDS; Index: integer): double;
begin
  Result := dso^.ds_getfd(dso, Index);
  lse_ds_check(dso);
end;

function __getft_name(field_type: TFieldType): string;
begin
  Result := '';
  case field_type of
    ftUnknown      : Result := 'unknown';
    ftString       : Result := 'string';
    ftSmallint     : Result := 'smallint';
    ftInteger      : Result := 'integer';
    ftWord         : Result := 'word';
    ftBoolean      : Result := 'boolean';
    ftFloat        : Result := 'float';
    ftCurrency     : Result := 'currency';
    ftBCD          : Result := 'bcd';
    ftDate         : Result := 'date';
    ftTime         : Result := 'time';
    ftDateTime     : Result := 'datetime';
    ftBytes        : Result := 'bytes';
    ftVarBytes     : Result := 'varbytes';
    ftAutoInc      : Result := 'autoinc';
    ftBlob         : Result := 'blob';
    ftMemo         : Result := 'memo';
    ftGraphic      : Result := 'graphic';
    ftFmtMemo      : Result := 'fmtmemo';
    ftParadoxOle   : Result := 'paradoxole';
    ftDBaseOle     : Result := 'dbaseole';
    ftTypedBinary  : Result := 'typedbinary';
    ftCursor       : Result := 'cursor';
    ftFixedChar    : Result := 'fixedchar';
    ftWideString   : Result := 'widestring';
    ftLargeint     : Result := 'largeint';
    ftADT          : Result := 'adt';
    ftArray        : Result := 'array';
    ftReference    : Result := 'reference';
    ftDataSet      : Result := 'dataset';
    ftOraBlob      : Result := 'orablob';
    ftOraClob      : Result := 'oraclob';
    ftVariant      : Result := 'variant';
    ftInterface    : Result := 'interface';
    ftIDispatch    : Result := 'idispatch';
    ftGuid         : Result := 'guid';
    ftTimeStamp    : Result := 'timestamp';
    ftFMTBcd       : Result := 'fmtbcd';
    ftFixedWideChar: Result := 'fixedwidechar';
    ftWideMemo     : Result := 'widememo';
    else
    case Ord(field_type) of
      40: Result := 'oratimestamp';  {ftOraTimeStamp}
      41: Result := 'orainterval';   {ftOraInterval}
    end;
  end;
end;

function __getft_lsv(field_type: TFieldType): integer;
begin
  Result := LSV_VOID;
  case field_type of
    ftUnknown      :;
    ftString       : Result := lSV_STRING;
    ftSmallint     : Result := LSV_INT;
    ftInteger      : Result := LSV_INT;
    ftWord         : Result := LSV_INT;
    ftBoolean      : Result := LSV_INT;
    ftFloat        : Result := LSV_FLOAT;
    ftCurrency     : Result := LSV_FLOAT;
    ftBCD          : Result := LSV_FLOAT;
    ftDate         : Result := LSV_FLOAT;
    ftTime         : Result := LSV_FLOAT;
    ftDateTime     : Result := LSV_FLOAT;
    ftBytes        :;
    ftVarBytes     :;
    ftAutoInc      : Result := LSV_INT;
    ftBlob         :;
    ftMemo         : Result := lSV_STRING;
    ftGraphic      :;
    ftFmtMemo      : Result := lSV_STRING;
    ftParadoxOle   :;
    ftDBaseOle     :;
    ftTypedBinary  :;
    ftCursor       :;
    ftFixedChar    : Result := lSV_STRING;
    ftWideString   : Result := lSV_STRING;
    ftLargeint     : Result := LSV_INT;
    ftADT          :;
    ftArray        :;
    ftReference    :;
    ftDataSet      :;
    ftOraBlob      :;
    ftOraClob      :;
    ftVariant      :;
    ftInterface    :;
    ftIDispatch    :;
    ftGuid         : Result := lSV_STRING;
    ftTimeStamp    :;
    ftFMTBcd       : Result := LSV_FLOAT;
    ftFixedWideChar: Result := lSV_STRING;
    ftWideMemo     : Result := lSV_STRING;
  end;
end;

function __getft(field: TField): integer;
begin
  Result := __getft_lsv(field.DataType);
end;

function __getfb(field: TField): boolean;
begin
  try
    if field.IsNull then Result := false else
    case __getft(field) of
      LSV_INT: Result := (field.AsInteger <> 0);
      LSV_FLOAT  : Result := not IsZero(field.AsFloat);
      lSV_STRING : Result := (field.AsString <> '');
            else   Result := false;
    end;
  except
    Result := false;
  end;
end;

function __getfd(field: TField): double;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := field.AsFloat;
      lSV_STRING : Result := stod(field.AsString);
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfi(field: TField): integer;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := Trunc(field.AsFloat);
      lSV_STRING : Result := Trunc(stod(field.AsString));
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfm(field: TField): currency;
begin
  try
    if field.IsNull then Result := 0 else
    case __getft(field) of
      LSV_INT: Result := field.AsInteger;
      LSV_FLOAT  : Result := field.AsFloat;
      lSV_STRING : Result := stod(field.AsString);
            else   Result := 0;
    end;
  except
    Result := 0;
  end;
end;

function __getfs(field: TField): string;
const
  BOOLEAN_STR: array[boolean] of string = ('0', '1');
begin
  try
    if field.IsNull then Result := '' else
    if field.DataType = ftBoolean then
      Result := BOOLEAN_STR[field.AsBoolean] else
      Result := field.AsString;
  except
    Result := '';
  end;
end;

{ database }
(*
procedure pp_database_execSQL(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  SQL: pchar;
begin
  if __GetThis(Param, this) then
  begin
    SQL := lse_strec_data(Param^.p_param[1]^.VObject);
    lse_set_int64(Param^.p_result, lse_db_execSQL(this, SQL));
  end;
end;

procedure pp_database_openSQL(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_ds(Param^.p_result,
      lse_db_openSQL(this, lse_strec_data(Param^.p_param[1]^.VObject)));
end;

procedure pp_database_tables(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  list: KLiVarList;
  strs: TStrings;
  X: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiVarList.Create(__AsEngine(Param));
    list.SaveTo(Param^.p_result);
    strs := TStringList.Create;
    try
      strs.CommaText := lse_db_tables(this);
      for X := 0 to strs.Count - 1 do
        list.Add(strs[X]);
    finally
      strs.Free;
    end;
  end;
end;

procedure pp_database_procedures(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  list: KLiVarList;
  strs: TStrings;
  X: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiVarList.Create(__AsEngine(Param));
    list.SaveTo(Param^.p_result);
    strs := TStringList.Create;
    try
      strs.CommaText := lse_db_storedprocs(this);
      for X := 0 to strs.Count - 1 do
        list.Add(strs[X]);
    finally
      strs.Free;
    end;
  end;
end;

procedure pp_database_connecTo(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_connecTo(this, __AsString(Param^.p_param[1]),  // target
                          __AsString(Param^.p_param[2]),  // user
                          __AsString(Param^.p_param[3]),  // password
                          __AsString(Param^.p_param[4]),  // source
                          __AsString(Param^.p_param[5])); // params
end;

procedure pp_database_disconnect(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_disconnect(this);
end;

procedure pp_database_reconnect(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_reconnect(this);
end;

procedure pp_database_transact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_transact(this);
end;

procedure pp_database_inTransaction(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.p_result, lse_db_transacting(this));
end;

procedure pp_database_commit(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_commit(this, false);
end;

procedure pp_database_commitRetaining(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_commit(this, true);
end;

procedure pp_database_commitAndTransact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
  begin
    lse_db_commit(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_rollback(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_rollback(this, false);
end;

procedure pp_database_rollbackRetaining(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_rollback(this, true);
end;

procedure pp_database_rollbackAndTransact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
  begin
    lse_db_rollback(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_connected(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.p_result, lse_db_connected(this));
end;

procedure pp_database_escape(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  srec: PLseString;
begin
  if __GetThis(Param, this) then
  begin
    srec := lse_db_escape(this, Param^.p_param[1]^.VObject);
    lse_set_string(Param^.p_result, srec);
  end;
end;

{ dataset }

procedure pp_dataset_close(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_close(this);
end;

procedure pp_dataset_open(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
  begin
    lse_ds_close(this);
    this^.ds_setSQL(this, lse_strec_data(Param^.p_param[1]^.VObject));
    lse_ds_check(this);
    this^.ds_open(this);
    lse_ds_check(this);
    lse_set_ds(Param^.p_result, this);
  end;
end;

procedure pp_dataset_first(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_first(this);
end;

procedure pp_dataset_last(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_last(this);
end;

procedure pp_dataset_prior(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_prior(this);
end;

procedure pp_dataset_next(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_next(this);
end;

procedure pp_dataset_eof(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.p_result, lse_ds_eof(this));
end;

procedure pp_dataset_bof(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.p_result, lse_ds_bof(this));
end;

procedure pp_dataset_count(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result, lse_ds_count(this));
end;

procedure pp_dataset_indexOf(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result,
      lse_ds_indexof(this, lse_strec_data(Param^.p_param[1]^.VObject)));
end;

procedure pp_dataset_field_name(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result,
      lse_ds_fname(this, Param^.p_param[1]^.VInteger, true));
end;

procedure pp_dataset_field_type(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  ftype: PLseType;
  clss: KLiType;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsType(Param^.p_param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.p_param[1]^.VInteger);
      ftype := lse_ds_field_class(this, index);
      lse_set_object(Param^.p_result, KR_CLASS, ftype^.cr_class);
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
      begin
        ftype := lse_ds_field_class(this, index);
        lse_set_object(Param^.p_result, KR_CLASS, ftype^.cr_class);
      end
      else __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_string(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiType;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsType(Param^.p_param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.p_param[1]^.VInteger);
      lse_set_string(Param^.p_result, lse_ds_getfs(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_string(Param^.p_result, lse_ds_getfs(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_int(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiType;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsType(Param^.p_param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.p_param[1]^.VInteger);
      lse_set_int64(Param^.p_result, lse_ds_getfi(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_int64(Param^.p_result, lse_ds_getfi(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_float(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiType;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsType(Param^.p_param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.p_param[1]^.VInteger);
      lse_set_float(Param^.p_result, lse_ds_getfd(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_float(Param^.p_result, lse_ds_getfd(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;
*)

end.
