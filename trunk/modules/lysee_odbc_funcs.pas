{==============================================================================}
{        UNIT: lysee_odbc_funcs                                                }
{ DESCRIPTION: functions of ODBC database verdor (FPC)                         }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_odbc_funcs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lseu, lse_dbu, sqldb, odbcconn;

const
  DBV_NAME = 'odbc';
  DBV_DESC = 'ODBC vendor (FPC) for lysee';
  
type
  TVendorDB = class(TODBCConnection)
  private
    FDB: PLseDB;
    FError: string;
    FRefcount: integer;
    FConnStr: string;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    procedure HandleError;
    function ExecSQL(const SQL: string): integer;
    procedure SetConnStr(const ConnectionStr: string);
  end;

  TVendorDS = class(TSQLQuery)
  private
    FDB: PLseDB;
    FDS: PLseDS;
    FError: string;
    FRefcount: integer;
    procedure HandleError;
  end;

function vdb_addref(DB: PLseDB): integer;cdecl;
function vdb_release(DB: PLseDB): integer;cdecl;

{ database }

procedure pp_database_create(const invoker: TLseInvoke);cdecl;
procedure pp_database_execSQL(const invoker: TLseInvoke);cdecl;
procedure pp_database_openSQL(const invoker: TLseInvoke);cdecl;
procedure pp_database_tables(const invoker: TLseInvoke);cdecl;
procedure pp_database_procedures(const invoker: TLseInvoke);cdecl;
procedure pp_database_connecTo(const invoker: TLseInvoke);cdecl;
procedure pp_database_disconnect(const invoker: TLseInvoke);cdecl;
procedure pp_database_reconnect(const invoker: TLseInvoke);cdecl;
procedure pp_database_transact(const invoker: TLseInvoke);cdecl;
procedure pp_database_inTransaction(const invoker: TLseInvoke);cdecl;
procedure pp_database_commit(const invoker: TLseInvoke);cdecl;
procedure pp_database_commitRetaining(const invoker: TLseInvoke);cdecl;
procedure pp_database_commitAndTransact(const invoker: TLseInvoke);cdecl;
procedure pp_database_rollback(const invoker: TLseInvoke);cdecl;
procedure pp_database_rollbackRetaining(const invoker: TLseInvoke);cdecl;
procedure pp_database_rollbackAndTransact(const invoker: TLseInvoke);cdecl;
procedure pp_database_connected(const invoker: TLseInvoke);cdecl;
procedure pp_database_escape(const invoker: TLseInvoke);cdecl;

{ dataset }

procedure pp_dataset_close(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_open(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_first(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_last(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_prior(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_next(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_eof(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_bof(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_count(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_indexOf(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_field_name(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_field_type(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_string(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_int(const invoker: TLseInvoke);cdecl;
procedure pp_dataset_float(const invoker: TLseInvoke);cdecl;

const

  func_count = 33;
  func_array: array[0..func_count - 1] of RLseFunc = (

    { database }

    (fr_prot:'database_create:database ||';
     fr_addr:@pp_database_create;
     fr_desc:'create database instance';
    ),
    (fr_prot:'database_execSQL:int |db:database, SQL:string|';
     fr_addr:@pp_database_execSQL;
     fr_desc:'execute SQL command';
    ),
    (fr_prot:'database_openSQL:dataset |db:database, SQL:string|';
     fr_addr:@pp_database_openSQL;
     fr_desc:'query dataset';
    ),
    (fr_prot:'database_get_tables:string |db:database|';
     fr_addr:@pp_database_tables;
     fr_desc:'get table name list';
    ),
    (fr_prot:'database_get_procedures:string |db:database|';
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

function otos_dataset(inst: pointer): PLseString;cdecl;
function cvgr_dataset(obj: pointer): PLseVargen;cdecl;
function getiv_ds(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
function getpv_ds(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
function length_ds(obj: pointer): integer;cdecl;

var
  odbc_types: array[0..1] of RLseType = (
   (cr_type     : LSV_OBJECT;
    cr_name     : 'database';
    cr_desc     : 'database';
    cr_addref   :@lse_db_addref;
    cr_release  :@lse_db_release;
    cr_write_to : nil;
    cr_vargen   : nil;
    cr_otos     : nil;
    cr_stoo     : nil;
    cr_add      : nil;
    cr_getiv    : nil;
    cr_setiv    : nil;
    cr_getpv    : nil;
    cr_setpv    : nil;
    cr_length   : nil
   ),
   (cr_type     : LSV_OBJECT;
    cr_name     : 'dataset';
    cr_desc     : 'dataset';
    cr_addref   :@lse_ds_addref;
    cr_release  :@lse_ds_release;
    cr_write_to : nil;
    cr_vargen   :@cvgr_dataset;
    cr_otos     :@otos_dataset;
    cr_stoo     : nil;
    cr_add      : nil;
    cr_getiv    :@getiv_ds;
    cr_setiv    : nil;
    cr_getpv    :@getpv_ds;
    cr_setpv    : nil;
    cr_length   :@length_ds
   )
  );

implementation

function db_type: PLseType;
begin
  Result := @odbc_types[0];
end;

function ds_type: PLseType;
begin
  Result := @odbc_types[1];
end;

procedure set_db(V: PLseValue; Value: PLseDB);
begin
  lse_set_object(V, db_type, Value);
end;

procedure set_ds(V: PLseValue; Value: PLseDS);
begin
  lse_set_object(V, ds_type, Value);
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
      set_ds(Value, dsrec);
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

function cvgr_dataset(obj: pointer): PLseVargen;cdecl;
var
  dsrec: PLseDS;
  cvgr: PLiVG_dataset;
begin
  dsrec := PLseDS(obj);
  if dsrec <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_dataset));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := nil;
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

function getiv_ds(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
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
        LSV_INT  : lse_set_int(value, lse_ds_getfi(D, index));
        LSV_FLOAT: lse_set_float(value, lse_ds_getfd(D, index));
       {lSV_STRING, LSV_CHAR}
        else lse_set_string(value, lse_ds_getfs(D, index));
      end;
      Result := 1;
    end;
  end;
end;

function getpv_ds(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
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
        LSV_INT  : lse_set_int(value, lse_ds_getfi(this, index));
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

function get_vds(DS: PLseDS): TVendorDS;
begin
  Result := TVendorDS(DS^.ds_object);
  DS^.ds_errno := 0;
  DS^.ds_error := nil;
end;

function get_vdb(DB: PLseDB): TVendorDB;
begin
  Result := TVendorDB(DB^.db_object);
  DB^.db_errno := 0;
  DB^.db_error := nil;
end;

// vds -------------------------------------------------------------------------

function vds_addref(DS: PLseDS): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vdb_addref(vds.FDB);
    Inc(vds.FRefcount);
    Result := vds.FRefcount;
    if Result = 0 then
    begin
      FreeMem(DS, sizeof(RLseDS));
      FreeAndNil(vds);
    end;
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_release(DS: PLseDS): integer;cdecl;
var
  vds: TVendorDS;
  dbc: PLseDB;
begin
  vds := get_vds(DS);
  try
    dbc := vds.FDB;
    Dec(vds.FRefcount);
    Result := vds.FRefcount;
    if vds.FRefcount = 0 then
    begin
      FreeMem(DS, sizeof(RLseDS));
      FreeAndNil(vds);
    end;
    vdb_release(dbc);
  except
    vds.HandleError;
  end;
end;

function vds_count(DS: PLseDS): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := vds.FieldCount;
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_getfn(DS: PLseDS; index: integer): PLseString;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := lse_strec_alloc(vds.Fields[index].FieldName);
  except
    vds.HandleError;
    Result := nil;
  end;
end;

function vds_getft(DS: PLseDS; index: integer): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := __getft(vds.Fields[index]);
  except
    vds.HandleError;
    Result := LSV_VOID;
  end;
end;

function vds_getfi(DS: PLseDS; index: integer): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := __getfi(vds.Fields[index]);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_getfs(DS: PLseDS; index: integer): PLseString;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := lse_strec_alloc(__getfs(vds.Fields[index]));
  except
    vds.HandleError;
    Result := nil;
  end;
end;

function vds_getfd(DS: PLseDS; index: integer): double;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := __getfd(vds.Fields[index]);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_isnull(DS: PLseDS; index: integer): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := Ord(vds.Fields[index].IsNull);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_length(DS: PLseDS): integer;cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := vds.RecordCount;
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_bof(DS: PLseDS): integer;
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := Ord(vds.Bof);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_eof(DS: PLseDS): integer;
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := Ord(vds.Eof);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

procedure vds_seek(DS: PLseDS; Offset, Origin: integer);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    if Origin in [0, 1, 2] then
    begin
      if Origin = 0 then vds.First else
      if Origin = 2 then vds.Last;
      if Offset <> 0 then
        vds.MoveBy(Offset);
    end;
  except
    vds.HandleError;
  end;
end;

procedure vds_close(DS: PLseDS);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vds.Close;
  except
    vds.HandleError;
  end;
end;

function vds_getSQL(DS: PLseDS): PLseString;
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := lse_strec_alloc(vds.SQL.Text);
  except
    vds.HandleError;
    Result := nil;
  end;
end;

procedure vds_setSQL(DS: PLseDS; const SQL: pchar);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vds.SQL.Text := SQL;
  except
    vds.HandleError;
  end;
end;

procedure vds_open(DS: PLseDS);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vds.Open;
  except
    vds.HandleError;
  end;
end;

function vds_active(DS: PLseDS): integer;
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := Ord(vds.Active);
  except
    vds.HandleError;
    Result := 0;
  end;
end;

function vds_getBMK(DS: PLseDS): pointer;
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    Result := vds.GetBookmark;
  except
    vds.HandleError;
    Result := nil;
  end;
end;

procedure vds_gotoBMK(DS: PLseDS; Bookmark: pointer);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vds.GotoBookmark(Bookmark);
  except
    vds.HandleError;
  end;
end;

procedure vds_freeBMK(DS: PLseDS; Bookmark: pointer);
cdecl;
var
  vds: TVendorDS;
begin
  vds := get_vds(DS);
  try
    vds.FreeBookmark(Bookmark);
  except
    vds.HandleError;
  end;
end;

// vdb -------------------------------------------------------------------------

function vdb_addref(DB: PLseDB): integer;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Inc(vdb.FRefcount);
    Result := vdb.FRefcount;
    if Result = 0 then
    begin
      FreeMem(DB, sizeof(RLseDB));
      FreeAndNil(vdb);
    end;
  except
    vdb.HandleError;
    Result := -1;
  end;
end;

function vdb_release(DB: PLseDB): integer;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Dec(vdb.FRefcount);
    Result := vdb.FRefcount;
    if vdb.FRefcount = 0 then
    begin
      FreeMem(DB, sizeof(RLseDB));
      FreeAndNil(vdb);
    end;
  except
    vdb.HandleError;
  end;
end;

function vdb_getConnStr(DB: PLseDB): PLseString;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Result := lse_strec_alloc(vdb.FConnStr);
  except
    vdb.HandleError;
    Result := nil;
  end;
end;

procedure vdb_setConnStr(DB: PLseDB; const ConnString: pchar);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    vdb.SetConnStr(ConnString);
  except
    vdb.HandleError;
  end;
end;

procedure vdb_connect(DB: PLseDB);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    vdb.Open;
  except
    vdb.HandleError;
  end;
end;

function vdb_connected(DB: PLseDB): integer;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Result := Ord(vdb.Connected);
  except
    vdb.HandleError;
    Result := 0;
  end;
end;

procedure vdb_disconnect(DB: PLseDB);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    if vdb.Connected then
      vdb.Close;
  except
    vdb.HandleError;
  end;
end;

function vdb_dataset(DB: PLseDB): PLseDS;cdecl;
var
  vdb: TVendorDB;
  vds: TVendorDS;
begin
  vdb := get_vdb(DB);
  try
    GetMem(Result, sizeof(RLseDS));
    FillChar(Result^, sizeof(RLseDS), 0);
    Result^.ds_size   := sizeof(RLseDS);
    vds := TVendorDS.Create(vdb);
    vds.DataBase := vdb;
    vds.UsePrimaryKeyAsKey := false;
    vds.FDB := DB;
    vds.FDS := Result;
    Result^.ds_object   := vds;
    Result^.ds_db       := DB;
    Result^.ds_addref   :=@vds_addref;
    Result^.ds_release  :=@vds_release;
    Result^.ds_count    :=@vds_count;
    Result^.ds_getfn    :=@vds_getfn;
    Result^.ds_getft    :=@vds_getft;
    Result^.ds_getfi    :=@vds_getfi;
    Result^.ds_getfs    :=@vds_getfs;
    Result^.ds_getfd    :=@vds_getfd;
    Result^.ds_isnull   :=@vds_isnull;
    Result^.ds_length   :=@vds_length;
    Result^.ds_bof      :=@vds_bof;
    Result^.ds_eof      :=@vds_eof;
    Result^.ds_seek     :=@vds_seek;
    Result^.ds_getSQL   :=@vds_getSQL;
    Result^.ds_setSQL   :=@vds_setSQL;
    Result^.ds_open     :=@vds_open;
    Result^.ds_close    :=@vds_close;
    Result^.ds_active   :=@vds_active;
    Result^.ds_getBMK   :=@vds_getBMK;
    Result^.ds_gotoBMK  :=@vds_gotoBMK;
    Result^.ds_freeBMK  :=@vds_freeBMK;
  except
    vdb.HandleError;
    Result := nil;
  end;
end;

function vdb_execSQL(DB: PLseDB; const SQL: pchar): integer;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Result := vdb.ExecSQL(SQL);
  except
    vdb.HandleError;
    Result := -1;
  end;
end;

procedure vdb_transact(DB: PLseDB);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    vdb.FTransaction.StartTransaction;
  except
    vdb.HandleError;
  end;
end;

function vdb_transacting(DB: PLseDB): integer;cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    Result := Ord(vdb.FTransaction.Active);
  except
    vdb.HandleError;
    Result := 0;
  end;
end;

procedure vdb_commit(DB: PLseDB);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    vdb.FTransaction.Commit;
  except
    vdb.HandleError;
  end;
end;

procedure vdb_rollback(DB: PLseDB);cdecl;
var
  vdb: TVendorDB;
begin
  vdb := get_vdb(DB);
  try
    vdb.FTransaction.Rollback;
  except
    vdb.HandleError;
  end;
end;

function vdb_storedprocs(DB: PLseDB): PLseString;cdecl;
var
  vdb: TVendorDB;
  list: TStrings;
begin
  vdb := get_vdb(DB);
  try
    list := TStringList.Create;
    try
      vdb.GetProcedureNames(list);
      Result := lse_strec_alloc(list.CommaText);
    finally
      list.Free;
    end;
  except
    vdb.HandleError;
    Result := nil;
  end;
end;

function vdb_tables(DB: PLseDB; IncSysTable: integer): PLseString;cdecl;
var
  vdb: TVendorDB;
  list: TStrings;
begin
  vdb := get_vdb(DB);
  try
    list := TStringList.Create;
    try
      vdb.GetTableNames(list, IncSysTable <> 0);
      Result := lse_strec_alloc(list.CommaText);
    finally
      list.Free;
    end;
  except
    vdb.HandleError;
    Result := nil;
  end;
end;

function vdb_fields(DB: PLseDB; const Table: pchar): PLseString;cdecl;
var
  vdb: TVendorDB;
  list: TStrings;
begin
  vdb := get_vdb(DB);
  try
    list := TStringList.Create;
    try
      vdb.GetFieldNames(Table, list);
      Result := lse_strec_alloc(list.CommaText);
    finally
      list.Free;
    end;
  except
    vdb.HandleError;
    Result := nil;
  end;
end;

{ TVendorDS }

procedure TVendorDS.HandleError;
begin
  if self <> nil then
  begin
    FError := lse_exception_str;
    FDS^.ds_errno := 1;
    FDS^.ds_error := pchar(FError);
  end;
end;

{ database }

procedure pp_database_create(const invoker: TLseInvoke);cdecl;
var
  dbr: PLseDB;
  vdb: TVendorDB;
begin
  GetMem(dbr, sizeof(RLseDB));
  FillChar(dbr^, sizeof(RLseDB), 0);
  dbr^.db_size        := sizeof(RLseDB);
  vdb := TVendorDB.Create(nil);
  vdb.FTransaction    := TSQLTransaction.Create(vdb);
  vdb.Transaction     := vdb.FTransaction;
  vdb.FDB             := dbr;
  dbr^.db_object      := vdb;
  dbr^.db_addref      :=@vdb_addref;
  dbr^.db_release     :=@vdb_release;
  dbr^.db_getConnStr  :=@vdb_getConnStr;
  dbr^.db_setConnStr  :=@vdb_setConnStr;
  dbr^.db_connect     :=@vdb_connect;
  dbr^.db_connected   :=@vdb_connected;
  dbr^.db_disconnect  :=@vdb_disconnect;
  dbr^.db_dataset     :=@vdb_dataset;
  dbr^.db_execSQL     :=@vdb_execSQL;
  dbr^.db_transact    :=@vdb_transact;
  dbr^.db_transacting :=@vdb_transacting;
  dbr^.db_commit      :=@vdb_commit;
  dbr^.db_rollback    :=@vdb_rollback;
  dbr^.db_storedprocs :=@vdb_storedprocs;
  dbr^.db_tables      :=@vdb_tables;
  dbr^.db_fields      :=@vdb_fields;
  set_db(invoker.Param^.p_result, dbr);
end;

procedure pp_database_execSQL(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
  SQL: pchar;
begin
  if invoker.GetThis(this) then
  begin
    SQL := lse_strec_data(invoker.ParamStrec(1));
    invoker.ReturnInt64(lse_db_execSQL(this, SQL));
  end;
end;

procedure pp_database_openSQL(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    set_ds(invoker.Param^.p_result,
      lse_db_openSQL(this, lse_strec_data(invoker.paramStrec(1))));
end;

procedure pp_database_tables(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    invoker.ReturnStr(lse_db_tables(this));
end;

procedure pp_database_procedures(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    invoker.ReturnStr(lse_db_storedprocs(this));
end;

procedure pp_database_connecTo(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_connecTo(this, invoker.ParamStr(1),  // target
                          invoker.ParamStr(2),  // user
                          invoker.ParamStr(3),  // password
                          invoker.ParamStr(4),  // source
                          invoker.ParamStr(5)); // params
end;

procedure pp_database_disconnect(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_disconnect(this);
end;

procedure pp_database_reconnect(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_reconnect(this);
end;

procedure pp_database_transact(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_transact(this);
end;

procedure pp_database_inTransaction(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    invoker.ReturnBool(lse_db_transacting(this));
end;

procedure pp_database_commit(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_commit(this, false);
end;

procedure pp_database_commitRetaining(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_commit(this, true);
end;

procedure pp_database_commitAndTransact(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
  begin
    lse_db_commit(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_rollback(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_rollback(this, false);
end;

procedure pp_database_rollbackRetaining(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_db_rollback(this, true);
end;

procedure pp_database_rollbackAndTransact(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
  begin
    lse_db_rollback(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_connected(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(lse_db_connected(this));
end;

procedure pp_database_escape(const invoker: TLseInvoke);cdecl;
var
  this: PLseDB;
begin
  if invoker.GetThis(this) then
    lse_set_string(invoker.Param^.p_result,
      lse_db_escape(this, invoker.paramStrec(1)));
end;

{ dataset }

procedure pp_dataset_close(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_ds_close(this);
end;

procedure pp_dataset_open(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
  begin
    lse_ds_close(this);
    this^.ds_setSQL(this, lse_strec_data(invoker.paramStrec(1)));
    lse_ds_check(this);
    this^.ds_open(this);
    lse_ds_check(this);
    set_ds(invoker.Param^.p_result, this);
  end;
end;

procedure pp_dataset_first(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_ds_first(this);
end;

procedure pp_dataset_last(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_ds_last(this);
end;

procedure pp_dataset_prior(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_ds_prior(this);
end;

procedure pp_dataset_next(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_ds_next(this);
end;

procedure pp_dataset_eof(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    invoker.ReturnBool(lse_ds_eof(this));
end;

procedure pp_dataset_bof(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    invoker.ReturnBool(lse_ds_bof(this));
end;

procedure pp_dataset_count(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    invoker.ReturnInt64(lse_ds_count(this));
end;

procedure pp_dataset_indexOf(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    invoker.ReturnInt64(lse_ds_indexof(this,
      lse_strec_data(invoker.paramStrec(1))));
end;

procedure pp_dataset_field_name(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
begin
  if invoker.GetThis(this) then
    lse_set_string(invoker.Param^.p_result,
      lse_ds_fname(this, invoker.Param^.p_param[1]^.VInteger, true));
end;

procedure pp_dataset_field_type(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
  index: integer;
  ftype: PLseType;
  clss: TLseValue;
  name: pchar;
begin
  if invoker.GetThis(this) then
  begin
    clss := lse_vtype(invoker.Param^.p_param[1]);
    if clss = LSV_INT then
    begin
      index := lse_ds_vary_index(this, invoker.paramInt(1));
      ftype := lse_ds_field_class(this, index);
      invoker.ReturnObject(KT_TYPE, ftype);
    end
    else
    if clss = LSV_STRING then
    begin
      name := lse_strec_data(invoker.paramStrec(1));
      index := lse_ds_indexof(this, name);
      if index >= 0 then
      begin
        ftype := lse_ds_field_class(this, index);
        invoker.ReturnObject(KT_TYPE, ftype);
      end
      else invoker.ReturnError('',0, Format('field "%s" not found', [name]));
    end
    else invoker.ReturnError('',0, 'Invalid parametre type');
  end;
end;

procedure pp_dataset_string(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: TLseValue;
  name: pchar;
begin
  if invoker.GetThis(this) then
  begin
    clss := lse_vtype(invoker.Param^.p_param[1]);
    if clss = LSV_INT then
    begin
      index := lse_ds_vary_index(this, invoker.Param^.p_param[1]^.VInteger);
      lse_set_string(invoker.Param^.p_result, lse_ds_getfs(this, index));
    end
    else
    if clss = LSV_STRING then
    begin
      name := lse_strec_data(invoker.Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_string(invoker.Param^.p_result, lse_ds_getfs(this, index)) else
        invoker.returnError('', 0, Format('field "%s" not found', [name]));
    end
    else invoker.ReturnError('',0, 'Invalid parametre type');
  end;
end;

procedure pp_dataset_int(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: TLseValue;
  name: pchar;
begin
  if invoker.GetThis(this) then
  begin
    clss := lse_vtype(invoker.Param^.p_param[1]);
    if clss = LSV_INT then
    begin
      index := lse_ds_vary_index(this, invoker.Param^.p_param[1]^.VInteger);
      lse_set_int(invoker.Param^.p_result, lse_ds_getfi(this, index));
    end
    else
    if clss = LSV_STRING then
    begin
      name := lse_strec_data(invoker.Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_int(invoker.Param^.p_result, lse_ds_getfi(this, index)) else
        invoker.returnError('', 0, Format('field "%s" not found', [name]));
    end
    else invoker.ReturnError('',0, 'Invalid parametre type');
  end;
end;

procedure pp_dataset_float(const invoker: TLseInvoke);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: TLseValue;
  name: pchar;
begin
  if invoker.GetThis(this) then
  begin
    clss := lse_vtype(invoker.Param^.p_param[1]);
    if clss = LSV_INT then
    begin
      index := lse_ds_vary_index(this, invoker.Param^.p_param[1]^.VInteger);
      lse_set_float(invoker.Param^.p_result, lse_ds_getfd(this, index));
    end
    else
    if clss = LSV_STRING then
    begin
      name := lse_strec_data(invoker.Param^.p_param[1]^.VObject);
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_float(invoker.Param^.p_result, lse_ds_getfd(this, index)) else
        invoker.returnError('', 0, Format('field "%s" not found', [name]));
    end
    else invoker.ReturnError('',0, 'Invalid parametre type');
  end;
end;

{ TVendorDB }

function TVendorDB.ExecSQL(const SQL: string): integer;
begin
  if FQuery = nil then
  begin
    FQuery := TSQLQuery.Create(Self);
    FQuery.DataBase := Self;
    FQuery.UsePrimaryKeyAsKey := false;
  end;
  FQuery.SQL.Text := SQL;
  try
    FQuery.ExecSQL;
    Result := -1; {<--BUG: RowsAffected not supplied}
  finally
    FQuery.Close;
  end;
end;

procedure TVendorDB.HandleError;
begin
  if self <> nil then
  begin
    FError := lse_exception_str;
    FDB^.db_errno := 1;
    FDB^.db_error := pchar(FError);
  end;
end;

procedure TVendorDB.SetConnStr(const ConnectionStr: string);
var
  T, U, P, S, M: string;
  L: integer;
begin
  FConnStr := Trim(ConnectionStr);
  L := Length(FConnStr);
  if (L > 6) and (FConnStr[1] = '[') and (FConnStr[L] = ']') then
  begin
    lse_decode_TUPSP(Copy(FConnStr, 2, L - 2), T, U, P, S, M);
    if T = '' then 
      raise Exception.Create('Target DSN not supplied');
    DatabaseName := T;
    UserName := U;
    Password := P;
    Params.Text := M;
    LoginPrompt := false;
  end
  else lse_error('Invalid connection string: %s', [FConnStr]);
end;

end.
