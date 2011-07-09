{==============================================================================}
{        UNIT: lse_dbu                                                         }
{ DESCRIPTION: database functions                                              }
{ DESCRIPTION: functions of ODBC database verdor (FPC)                         }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_dbu;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
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

{======================================================================)
(======== TUPSP - encode/decode connection string =====================)
(======================================================================)
( lse_encode_TUPSP: encode database connection parametres
( lse_decode_TUPSP: decode database connection parametres
(----------------------------------------------------------------------}
function  lse_encode_TUPSP(const Target, User, Password, Source, Params: string): string;
procedure lse_decode_TUPSP(const S: string; var Target, User, Password, Source, Params: string);

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

implementation

uses
  Math;

function stod(const S: string): extended;
begin
  Result := StrToFloatDef(S, 0);
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

end.
