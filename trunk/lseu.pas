{==============================================================================}
{        UNIT: lseu                                                            }
{ DESCRIPTION: lysee script engine unit                                        }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2010/11/02                                                      }
{==============================================================================}
{ Copyright (c) 2003-2010, Li Yun Jie                                          }
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
{ Portions created by Li Yun Jie are Copyright (C) 2003-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lseu;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, DateUtils, Math,
  {$IFDEF LYSEE_LAZ}LResources,{$ENDIF}
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF};

const

  { LSE: Lysee Script Engine }

  LSE_ID             = 'lysee';
  LSE_DLLEXT         = {$IFDEF WINDOWS}'.dll'{$ELSE}'.so'{$ENDIF};
  LSE_KERNEL         = LSE_ID + LSE_DLLEXT;
  LSE_CONFILE        = 'lysee.config';
  LSE_MIMEFILE       = 'lysee.mime';
  LSE_SEARCH_PATH    = '${knpath}modules';
  LSE_TEMP_PATH      = {$IFDEF WINDOWS}'${knpath}temp'{$ELSE}'/tmp'{$ENDIF};
  LSE_COPYRIGHT      = 'Copyright (C) 2003-2010 Li Yun Jie - http://www.lysee.net';
  LSE_VERSION        = '1.2.1';
  LSE_BIRTHDAY       = 20030228;
  LSE_BUILDDAY       = 20101102;
  LSE_PATH_DELIMITER = {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};
  LSE_MAX_PARAMS     = 12;
  LSE_MAX_CODES      = MaxInt div 2;
  
  { LSV: Lysee Script Value }

  LSV_VOID           = 0;  {<--void}
  lSV_STRING         = 1;  {<--string}
  LSV_INT            = 2;  {<--int}
  LSV_FLOAT          = 3;  {<--float}
  LSV_MONEY          = 4;  {<--money}
  LSV_TIME           = 5;  {<--time}
  LSV_BOOL           = 6;  {<--bool}
  LSV_CHAR           = 7;  {<--char}
  LSV_VARIANT        = 8;  {<--variant}
  LSV_OBJECT         = 9;  {<--object}

  { SSF: seek stream from }

  SSF_BEGINNING      = 0;
  SSF_CURRENT        = 1;
  SSF_END            = 2;

  { SCT: Simple Code Test }

  SCT_ERROR          = $01;  {<--has error}
  SCT_OK             = $02;  {<--OK! might be right}
  SCT_DOTCOMMA       = $04;  {<--last symbol is ';'}
  SCT_ENDBLOCK       = $08;  {<--last symbol is 'end'}
  SCT_UNFINISHED     = $10;  {<--not finished}

type

  { class forward }

  TLseObject   = class;
  TLseEngine   = class;
  TLseModule   = class;
  TLseFunc     = class;
  TLseClass    = class;
  TLseMethod   = class;
  TLseInvoke   = class;

  { record forward }

  PLseClassRec = ^RLseClassRec;
  PLseEntryRec = ^RLseEntryRec;
  PLseStream   = ^RLseStream;

{======================================================================)
(======== string interface ============================================)
(======================================================================}

  PLseString = ^RLseString;
  RLseString = packed record
    sr_life: integer;
    sr_slen: integer;
    sr_free: procedure(const SR: PLseString);cdecl;
  end;

{======================================================================)
(======== value interface =============================================)
(======================================================================}

  TLseValue = LSV_VOID..LSV_OBJECT;

  RLseValue = packed record
    value_class: PLseClassRec;
    case TLseValue of
      LSV_STRING: (VString : PLseString);
      LSV_INT   : (VInteger: int64);
      LSV_FLOAT : (VFloat  : double);
      LSV_MONEY : (VMoney  : currency);
      LSV_TIME  : (VTime   : TDateTime);
      LSV_BOOL  : (VBool   : boolean);
      LSV_CHAR  : (VChar   : char);
      LSV_OBJECT: (VObject : pointer);
  end;
  PLseValue = ^RLseValue;

{======================================================================)
(======== param interface =============================================)
(======================================================================}

  RLseParam = packed record
    param : array[0..LSE_MAX_PARAMS - 1] of PLseValue;
    count : integer;   // parametre count
    result: PLseValue; // result value
    func  : pointer;   // --> lse_kernel.KLiFunc
    runner: pointer;   // --> lse_kernel.KLiRunner
    exprec: pointer;   // --> lse_kernel.PLiExprRec
  end;
  PLseParam = ^RLseParam;

{======================================================================)
(======== function interface ==========================================)
(======================================================================}

  TLseFuncCall = procedure(const Param: PLseParam);cdecl;
  TLseFuncInvoke = procedure(const Param: TLseInvoke);cdecl;

  RLseFuncRec = packed record
    fr_prot: pchar;   // prototype
    fr_addr: pointer; // TLseFuncCall | TLseFuncInvoke
    fr_desc: pchar;   // description
  end;
  PLseFuncRec = ^RLseFuncRec;

  ALseFuncList = array[0..1023] of RLseFuncRec;
  PLseFuncList = ^ALseFuncList;

  RLseFuncListRec = packed record
    count: integer;
    entry: PLseFuncList;
  end;
  PLseFuncListRec = ^RLseFuncListRec;

{======================================================================)
(======== vargen interface ============================================)
(======================================================================}

  PLseVargen = ^RLseVargen;
  RLseVargen = packed record
    vg_data    : pointer;
    vg_engine  : pointer;
    vg_rewind  : function(vgrec: PLseVargen): integer;cdecl;
    vg_has_next: function(vgrec: PLseVargen): integer;cdecl;
    vg_get_next: function(vgrec: PLseVargen; Value: PLseValue): integer;cdecl;
    vg_addref  : function(vgrec: PLseVargen): integer;cdecl;
    vg_release : function(vgrec: PLseVargen): integer;cdecl;
    vg_contains: function(vgrec: PLseVargen; Value: PLseValue): integer;cdecl;
  end;

{======================================================================)
(======== class interface =============================================)
(======================================================================}

  TLseIncRefcount = function(const obj: pointer): integer;cdecl;
  TLseDecRefcount = TLseIncRefcount;
  
  TLseWriteTo = function(obj: pointer; stream: PLseStream): integer;cdecl;
  TLseToVargen = function(obj, engine: pointer): PLseVargen;cdecl;
  TLseToString = function(obj: pointer): PLseString;cdecl;
  TLseStringTo = function(str: PLseString; engine: pointer): pointer;cdecl;
  TLseAddItem = function(obj: pointer; value: PLseValue; engine: pointer): integer;cdecl;

  RLseClassRec = packed record
    vtype      : integer;         {<--value type: LSV_XXXX}
    name       : pchar;           {<--class name}
    desc       : pchar;           {<--class description}
    incRefcount: TLseIncRefcount; {<--increase reference count}
    decRefcount: TLseDecRefcount; {<--decrease reference count}
    funcs      : RLseFuncListRec; {<--method functions}
    writeTo    : TLseWriteTo;     {<--write to stream}
    toVargen   : TLseToVargen;    {<--convert object to vargen}
    toString   : TLseToString;    {<--convert object to string}
    stringTo   : TLseStringTo;    {<--convert string to object}
    addItem    : TLseAddItem;     {<--add new item}
    lysee_class: pointer;         {<--lse_kernel.KLiClass}
  end;

  ALseClassList = array[0..1023] of RLseClassRec;
  PLseClassList = ^ALseClassList;

  RLseClassListRec = packed record
    count: integer;
    entry: PLseClassList;
  end;
  PLseClassListRec = ^RLseClassListRec;

{======================================================================)
(======== builtin classes =============================================)
(======================================================================}

  TLseKernelClass = (kcVoid, kcString, kcInteger, kcFloat, kcMoney, kcTime,
                     kcBool, kcChar, kcVariant, kcStrlist, kcClass, kcModule,
                     kcFunc, kcVariable, kcError, kcStream, kcVarlist, kcDB,
                     kcDS, kcHashed, kcVargen);

  RLseKernelClassList = array[TLseKernelClass] of PLseClassRec;
  PLseKernelClassList = ^RLseKernelClassList;
  
{======================================================================)
(======== module interface ============================================)
(======================================================================}

  TLseOnImport = procedure(const Module: pointer);cdecl;
  TLseOnInvoke = procedure(const Call: TLseFuncInvoke;
                           const Param: PLseParam);cdecl;

  RLseModuleRec = packed record
    iw_version : pchar;            {<--production version}
    iw_desc    : pchar;            {<--description}
    iw_classes : RLseClassListRec; {<--class list}
    iw_libfuncs: RLseFuncListRec;  {<--module function list}
    iw_import  : TLseOnImport;     {<--import module notify}
    iw_invoke  : TLseOnInvoke;     {<--invoke function notify}
  end;
  PLseModuleRec = ^RLseModuleRec;

  TLseQueryEntry = function(const ID: pchar): pointer;cdecl;
  {
    DLL module initializing procedure "InitExchange"
  }
  TLseInitExchange = procedure(const MR: PLseModuleRec;
                               const QE: TLseQueryEntry);cdecl;

{======================================================================)
(======== stream interface ============================================)
(======================================================================}

  RLseStream = packed record
    data: pointer;
    read: function(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
    readln: function(S: PLseStream): PLseString;cdecl;
    write: function(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
    seek: function(S: PLseStream; Offset: int64; Origin: integer): int64;cdecl;
    get_size: function(S: PLseStream): int64;cdecl;
    set_size: procedure(S: PLseStream; NewSize: int64);cdecl;
    eof: function(S: PLseStream): integer;cdecl;
    close: procedure(S: PLseStream);cdecl;
    closed: function(S: PLseStream): integer;cdecl;
    flush: procedure(S: PLseStream);cdecl;
    addref: function(S: PLseStream): integer;cdecl;
    release: function(S: PLseStream): integer;cdecl;
  end;

{======================================================================)
(======== database interface ==========================================)
(======================================================================}

  RLseTUPSP = packed record {<--for C interface}
    target  : PLseString;
    user    : PLseString;
    password: PLseString;
    source  : PLseString;
    params  : PLseString;
  end;
  PLseTUPSP = ^RLseTUPSP;

  PLseDB = ^RLseDB;
  PLseDS = ^RLseDS;

  TLseDS_addref = function(dso: PLseDS): integer;cdecl;
  TLseDS_release = function(dso: PLseDS): integer;cdecl;
  TLseDS_count = function(dso: PLseDS): integer;cdecl;
  TLseDS_getfn = function(dso: PLseDS; index: integer): PLseString;cdecl;
  TLseDS_getft = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_getfi = function(dso: PLseDS; index: integer): integer;cdecl;
  TLseDS_getfs = function(dso: PLseDS; index: integer): PLseString;cdecl;
  TLseDS_getfd = function(dso: PLseDS; index: integer): double;cdecl;
  TLseDS_getfm = function(dso: PLseDS; index: integer): currency;cdecl;
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
    ds_getfm   : TLseDS_getfm;   {<--get money field value}
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

  RLseDBVendor = packed record
    dv_name: array[0..31] of char; // MSSQL FB/IB POSTGRES ACCESS ODBC ....
    dv_desc: array[0..91] of char;
    dv_create: function:PLseDB;cdecl;
  end;
  PLseDBVendor = ^RLseDBVendor;

{======================================================================)
(======== TLseObject ==================================================)
(======================================================================}

  TLseObject = class
  private
    FRefcount: integer; {<--free this object when 0}
  public
    function IncRefcount: integer;virtual;
    function DecRefcount: integer;virtual;
    property Refcount: integer read FRefCount;
  end;

  TLseException = class(Exception);

{======================================================================)
(======== TLseLock ====================================================)
(======================================================================}

  TLseLock = class(TLseObject)
  public
    procedure Enter;virtual;abstract;
    procedure Leave;virtual;abstract;
    function TryEnter: boolean;virtual;abstract;
  end;

  TLseLockError = class(TLseException);

{======================================================================)
(======== TLseComponent ===============================================)
(======================================================================}

  TLseComponentType = (lsoNone, lsoEngine, lsoModule, lsoFunction,
                       lsoClass, lsoMethod);

  TLseComponent = class(TComponent)
  private
    FComponentType: TLseComponentType;
  public
    procedure Setup;virtual;abstract;
    function IsDestroying: boolean;
    function IsDesignLoading: boolean;
    property ComponentType: TLseComponentType read FComponentType;
  end;
  
{======================================================================)
(======== engine interface: TLseEngine ================================)
(======================================================================}

  PLseEngine = ^RLseEngine;

  TLseEngineEvent = procedure(Engine: PLseEngine);cdecl;
  TLsePassword = function(Engine: PLseEngine): PLseString;cdecl;

  RLseEngine = packed record
    er_engine   : TLseEngine;      {<--L: TLseEngine instance}
    er_executing: TLseEngineEvent; {<--L: OnExecuting}
    er_executed : TLseEngineEvent; {<--L: OnExecuted}
    er_password : TLsePassword;    {<--L: OnPassword}
    er_stdin    : PLseStream;      {<--L: stdin stream}
    er_stdout   : PLseStream;      {<--L: stdout stream}
    er_stderr   : PLseStream;      {<--L: stderr stream}
    lysee_engine: pointer;         {<--K: kernel engine instance}
  end;

  TLseRead = procedure(Sender: TObject; const Buf: pchar; var Count: integer) of object;
  TLseReadln = procedure(Sender: TObject; var S: string) of object;
  TLseWrite = procedure(Sender: TObject; const Buf: pchar; var Count: integer) of object;
  TLseEof = procedure(Sender: TObject; var Eof: boolean) of object;

  TLseEngine = class(TLseComponent)
  private
    FEngineRec: RLseEngine;
    FStdin: RLseStream;
    FStdout: RLseStream;
    FStderr: RLseStream;
    FOnExecuting: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FOnPassword: TLseReadln;
    FOnReadln: TLseReadln;
    FOnRead: TLseRead;
    FOnWrite: TLseWrite;
    function GetArgs: string;
    function GetMainFile: string;
    function GetSearchPath: string;
    procedure SetArgs(const Value: string);
    function GetTempPath: string;
    function GetEngineRec: PLseEngine;
    procedure SetSearchPath(const Value: string);
    procedure SetMainFile(const Value: string);
    procedure EventExecuting;
    procedure EventExecuted;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;override;
    procedure Clear;virtual;
    procedure Terminate;
    function CompileCode(const Code: string; IsLspCode: boolean = false): boolean;
    function CompileFile(const FileName: string; IsLspFile: boolean = false): boolean;
    function ExecuteCode(const Code: string; IsLspCode: boolean = false): boolean;
    function ExecuteFile(const FileName: string; IsLspFile: boolean = false): boolean;
    function ExecLspFile(const FileName, Arguments: string; DoClear: boolean = true): boolean;
    function ExecLsFile(const FileName, Arguments: string; DoClear: boolean = true): boolean;
    function Ready: boolean;
    function Running: boolean;
    function Terminated: boolean;
    function Terminating: boolean;
    function Exited: boolean;
    function ResultType: string;
    function ResultText: string;
    function SetupArgs(const MainFile: string; StartIndex: integer): string;
    procedure ExecCommandLine(StartParamIndex: integer = 1);
    procedure WriteText(const Text: string);
    procedure WriteLine(const Text: string);
    procedure WriteLineBreak;
    procedure WriteData(Data: pointer; Count: integer);
    procedure WriteStream(AStream: TStream);
    procedure WriteFile(const FileName: string);
    function Errno: integer;
    function Error: string;
    function ErrorRow: integer;
    function ErrorCol: integer;
    function ErrorModule: string;
    function ErrorName: string;
    function ErrorMsg: string;
    function ErrorIncludedFile: string;
    property TempPath: string read GetTempPath;
    property EngineRec: PLseEngine read GetEngineRec;
    property MainFile: string read GetMainFile write SetMainFile;
    property Arguments: string read GetArgs write SetArgs;
    property SearchPath: string read GetSearchPath write SetSearchPath;
  published
    property OnExecuting: TNotifyEvent read FOnExecuting write FOnExecuting;
    property OnExecuted: TNotifyEvent read FOnExecuted write FOnExecuted;
    property OnPassword: TLseReadln read FOnPassword write FOnPassword;
    property OnReadln: TLseReadln read FOnReadln write FOnReadln;
    property OnRead: TLseRead read FOnRead write FOnRead;
    property OnWrite: TLseWrite read FOnWrite write FOnWrite;
  end;

{======================================================================)
(======== TLseModule ==================================================)
(======================================================================}

  TLseModule = class(TLseComponent)
  private
    FModule: pointer;
    FFuncList: TList;
    FClassList: TList;
    function GetClass(Index: integer): TLseClass;
    function GetClassCount: integer;
    function GetFunc(Index: integer): TLseFunc;
    function GetFuncCount: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure SetupModule;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;override;
    function AddFunc(const Prototype: string; Proc: pointer): pointer;
    function Exists(AComponent: TLseComponent): boolean;
    function FuncExists(AFunc: TLseFunc): boolean;
    function ClassExists(AClass: TLseClass): boolean;
    function MethodExists(AMethod: TLseMethod): boolean;
    property Module: pointer read FModule;
    property ClassCount: integer read GetClassCount;
    property Classes[Index: integer]: TLseClass read GetClass;
    property FuncCount: integer read GetFuncCount;
    property Funcs[Index: integer]: TLseFunc read GetFunc;
  end;

{======================================================================)
(======== TLseFunc ====================================================)
(======================================================================}

  TLseExecFunc = procedure(Invoker: TLseInvoke) of object;

  TLseFunc = class(TLseComponent)
  private
    FFunc: pointer;
    FModule: TLseModule;
    FPrototype: string;
    FOnExecute: TLseExecFunc;
    procedure SetModule(const AValue: TLseModule);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;override;
    procedure Execute(Param: PLseParam);
    property Func: pointer read FFunc;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property Module: TLseModule read FModule write SetModule;
    property OnExecute: TLseExecFunc read FOnExecute write FOnExecute;
  end;

{======================================================================)
(======== TLseClass ===================================================)
(======================================================================}

  TLseGetObject = procedure(Sender: TObject; var Lobj: TLseObject) of object;

  TLseClass = class(TLseComponent)
  private
    FClass: PLseClassRec;
    FModule: TLseModule;
    FMethodList: TList;
    FOnGetObject: TLseGetObject;
    function GetMethod(Index: integer): TLseMethod;
    function GetMethodCount: integer;
    procedure SetModule(const AValue: TLseModule);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
    procedure SetupClass;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;override;
    function AddMethod(const Prototype: string; Proc: pointer): pointer;
    function FindMethod(AMethod: pointer): TLseMethod;
    function Exists(AMethod: TLseMethod): boolean;
    property ClassRec: PLseClassRec read FClass;
    property MethodCount: integer read GetMethodCount;
    property Methods[Index: integer]: TLseMethod read GetMethod;
  published
    property Module: TLseModule read FModule write SetModule;
    property OnGetObject: TLseGetObject read FOnGetObject write FOnGetObject;
  end;

{======================================================================)
(======== TLseMethod ==================================================)
(======================================================================}

  TLseExecMethod = procedure(Lobj: TLseObject; Invoker: TLseInvoke) of object;

  TLseMethod = class(TLseComponent)
  private
    FMethod: pointer;
    FClass: TLseClass;
    FPrototype: string;
    FOnExecute: TLseExecMethod;
    procedure SetClass(const AValue: TLseClass);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;override;
    procedure Execute(Param: PLseParam);
    function ClassRec: PLseClassRec;
    property Method: pointer read FMethod;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property LyseeClass: TLseClass read FClass write SetClass;
    property OnExecute: TLseExecMethod read FOnExecute write FOnExecute;
  end;
  
{======================================================================)
(======== TLseInvoke ==================================================)
(======================================================================}

  TLseInvoke = class(TLseObject)
  private
    FParam: PLseParam;
  public
    constructor Create(Param: PLseParam);virtual;
    procedure ReturnInt(const Value: integer);
    procedure ReturnInt64(const Value: int64);
    procedure ReturnFloat(const Value: double);
    procedure ReturnMoney(const Value: currency);
    procedure ReturnTime(const Value: TDateTime);
    procedure ReturnStr(const Value: string);
    procedure ReturnChar(const Value: char);
    procedure ReturnBool(const Value: boolean);
    procedure ReturnObject(KernelClass: pointer; Value: pointer);
    procedure ReturnObj(CR: PLseClassRec; Value: pointer);
    procedure ReturnStream(Value: TStream);overload;
    procedure ReturnStream(Value: PLseStream);overload;
    procedure ReturnError(const ID: string; Errno: integer; const Msg: string);
    procedure Print(const Str: string);
    function Read(const Buf: pchar; Count: integer): integer;
    function Readln: string;
    function FormatStr(const Str: string): string;
    function EngineRec: PLseEngine;
    function KernelEngine: pointer;
    function GetThis(var obj): boolean;
    function ParamCount: integer;
    function ParamInt(Index: integer): integer;
    function ParamInt64(Index: integer): int64;
    function ParamFloat(Index: integer): double;
    function ParamMoney(Index: integer): currency;
    function ParamStr(Index: integer): string;
    function ParamCStr(Index: integer; var Size: integer): pchar;
    function ParamStrec(Index: integer): PLseString;
    function ParamFmt(Index: integer): string;
    function ParamChar(Index: integer): char;
    function ParamBool(Index: integer): boolean;
    function ParamObject(Index: integer): pointer;
    function ParamClass(Index: integer): PLseClassRec;
    function ParamTime(Index: integer): TDateTime;
    function ParamStream(Index: integer): PLseStream;
    property Param: PLseParam read FParam write FParam;
  end;

{======================================================================)
(======== CIK interface ===============================================)
(======================================================================}

  RLseEntryRec = packed record
    cik_classes: PLseKernelClassList;
    { engine }
    cik_create: function(const EngineRec: PLseEngine): pointer;cdecl;
    cik_destroy: procedure(const Engine: pointer);cdecl;
    cik_compile: function(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
    cik_fcompile: function(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
    cik_execute: function(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
    cik_fexecute: function(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
    cik_terminate: procedure(const Engine: pointer);cdecl;
    cik_clear: procedure(const Engine: pointer);cdecl;
    cik_get_args: function(const Engine: pointer): PLseString;cdecl;
    cik_set_args: procedure(const Engine: pointer; const Args: pchar);cdecl;
    cik_errno: function(const Engine: pointer): integer;cdecl;
    cik_error_row: function(const Engine: pointer): integer;cdecl;
    cik_error_col: function(const Engine: pointer): integer;cdecl;
    cik_error_name: function(const Engine: pointer): pchar;cdecl;
    cik_error_msg: function(const Engine: pointer): pchar;cdecl;
    cik_error_module: function(const Engine: pointer): pchar;cdecl;
    cik_error_file: function(const Engine: pointer): pchar;cdecl;
    cik_result_type: function(const Engine: pointer): pchar;cdecl;
    cik_result_text: function(const Engine: pointer): pchar;cdecl;
    cik_get_search_path: function(const Engine: pointer): pchar;cdecl;
    cik_set_search_path: procedure(const Engine: pointer; const Path: pchar);cdecl;
    cik_get_main_file: function(const Engine: pointer): pchar;cdecl;
    cik_set_main_file: procedure(const Engine: pointer; const fname: pchar);cdecl;
    cik_ready: function(const Engine: pointer): integer;cdecl;
    cik_running: function(const Engine: pointer): integer;cdecl;
    cik_terminated: function(const Engine: pointer): integer;cdecl;
    cik_exited: function(const Engine: pointer): integer;cdecl;
    cik_write: procedure(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
    cik_read: function(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
    cik_readln: function(const Engine: pointer): PLseString;cdecl;
    cik_begin_cgi: procedure(const Engine: pointer);cdecl;
    cik_end_cgi: procedure(const Engine: pointer);cdecl;
    { module }
    cik_module_count: function: integer;cdecl;
    cik_module: function(Index: integer): pointer;cdecl;
    cik_module_setup: function(const Name: pchar; const MR: PLseModuleRec): pointer;cdecl;
    cik_module_find: function(const Name: pchar): pointer;cdecl;
    cik_module_class: function(const Module: pointer): PLseClassRec;cdecl;
    cik_module_desc: function(const Module: pointer): pchar;cdecl;
    { class }
    cik_class_count: function(const Module: pointer): integer;cdecl;
    cik_class: function(const Module: pointer; Index: integer): PLseClassRec;cdecl;
    cik_class_setup: function(const Module: pointer; const CR: PLseClassRec): PLseClassRec;cdecl;
    cik_class_find: function(const Name: pchar): PLseClassRec;cdecl;
    cik_class_module: function(const CR: PLseClassRec): pointer;cdecl;
    cik_class_rec: function(const KernelClass: pointer): PLseClassRec;cdecl;
    { method }
    cik_method_count: function(const CR: PLseClassRec): integer;cdecl;
    cik_method: function(const CR: PLseClassRec; Index: integer): pointer;cdecl;
    cik_method_setup: function(const CR: PLseClassRec; const FR: PLseFuncRec): pointer;cdecl;
    cik_method_find: function(const CR: PLseClassRec; const Name: pchar): pointer;cdecl;
    cik_method_name: function(const Method: pointer): pchar;cdecl;
    cik_method_desc: function(const Method: pointer): pchar;cdecl;
    cik_method_type: function(const Method: pointer): PLseClassRec;cdecl;
    cik_method_class: function(const Method: pointer): PLseClassRec;cdecl;
    cik_method_param_count: function(const Method: pointer): integer;cdecl;
    cik_method_param_name: function(const Method: pointer; Index: integer): pchar;cdecl;
    cik_method_param_type: function(const Method: pointer; Index: integer): PLseClassRec;cdecl;
    cik_method_bind: function(const Method: pointer; Data: pointer): pointer;cdecl;
    cik_method_get_bind: function(const Method: pointer): pointer;cdecl;
    cik_method_is_creator: function(const Method: pointer): integer;cdecl;
    cik_method_get_proc: function(const Method: pointer): pointer;cdecl;
    cik_method_set_proc: function(const Method, NewProc: pointer): pointer;cdecl;
    { param }
    cik_param_engine: function(const Param: PLseParam): PLseEngine;cdecl;
    cik_param_format: function(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
    cik_param_error: procedure(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
    cik_param_push: function(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
    cik_param_goon: function(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
    { database }
    cik_dbv_provide: function(const Vendor: pchar): PLseDB;cdecl;
    cik_dbv_register: function(const dbv: PLseDBVendor): integer;cdecl;
    cik_encode_TUPSP: function (const Target, User, Password, Source, Params: pchar): PLseString;cdecl;
    cik_decode_TUPSP: procedure(const ConnectionStr: pchar; const TUPSP: PLseTUPSP);cdecl;
    { others }
    cik_production: function: pchar;cdecl;
    cik_version: function: pchar;cdecl;
    cik_copyright: function: pchar;cdecl;
    cik_tmpath: function: pchar;cdecl;
    cik_query: TLseQueryEntry;
    cik_simple_test: function(const Code: pchar): integer;cdecl;
    cik_startup: function: integer;cdecl;
    cik_cleanup: procedure;cdecl;
    cik_keywords: function: pchar;cdecl;
    cik_get_kernel_file: function: pchar;cdecl;
    cik_set_kernel_file: procedure(const KernelFile: pchar);cdecl;
    cik_get_program_file: function: pchar;cdecl;
    cik_set_program_file: procedure(const ProgramFile: pchar);cdecl;
    cik_load_config: procedure(const ConfigFile: pchar);cdecl;
    cik_log: procedure(const Msg: pchar; Count: integer);cdecl;
  end;

var
  lse_entries: PLseEntryRec = nil;

const
  lse_vtype_names: array[LSV_VOID..LSV_OBJECT] of string = (
    'void', 'string', 'int', 'float', 'money', 'time', 'bool',
    'char', 'variant', 'object'
  );

{======================================================================)
(======== kernel - load/manifest lysee kernel =========================)
(======================================================================)
( lse_prepare          : setup query entry
( lse_query            : query entry by ID
( lse_load_kernel      : load lysee kernel
( lse_load_config      : load configure values
( lse_startup          : startup lysee kernel
( lse_cleanup          : cleanup lysee kernel
( lse_kernel_file      : get kernel file name
( lse_set_kernel_file  : set kernel file name
( lse_program_file     : get program file name
( lse_set_program_file : set program file name
( lse_kernel_production: get kernel production name
( lse_kernel_version   : get kernel version
( lse_kernel_copyright : get kernel copyright
( lse_kernel_log       : save log message
( lse_simple_test      : simple test lysee script
( lse_keywords         : get ',' delimited keyword list
(----------------------------------------------------------------------}
function  lse_prepare(QE: TLseQueryEntry): boolean;
function  lse_query(const ID: string): pointer;
procedure lse_load_kernel(const KernelFile: string);
procedure lse_load_config(const ConfigFile: string);
function  lse_startup: boolean;
procedure lse_cleanup;
function  lse_kernel_file: string;
procedure lse_set_kernel_file(const KernelFile: string);
function  lse_program_file: string;
procedure lse_set_program_file(const ProgramFile: string);
function  lse_kernel_production(IncludeVC: boolean = false): string;
function  lse_kernel_version: string;
function  lse_kernel_copyright: string;
procedure lse_kernel_log(const Msg: pchar; Count: integer);overload;
procedure lse_kernel_log(const Msg: string);overload;
function  lse_simple_test(const Script: string): integer;
function  lse_keywords: string;

{======================================================================)
(======== module ======================================================)
(======================================================================)
( lse_module_count      : get module count
( lse_module_get        : get module by index
( lse_module_setup      : setup module
( lse_module_find       : find module by name
( lse_module_class      : get module class
( lse_module_name       : get module name
( lse_module_desc       : get module description
( lse_module_func_setup : setup module function
(----------------------------------------------------------------------}
function lse_module_count: integer;
function lse_module_get(Index: integer): pointer;
function lse_module_setup(const Name, FileName: string; MR: PLseModuleRec): pointer;overload;
function lse_module_setup(const Name, FileName: string): pointer;overload;
function lse_module_find(const Name: string): pointer;
function lse_module_class(const Module: pointer): PLseClassRec;
function lse_module_name(const Module: pointer): string;
function lse_module_desc(const Module: pointer): string;
function lse_module_func_setup(const Module: pointer; const FR: PLseFuncRec): pointer;overload;
function lse_module_func_setup(const Module: pointer; const Prototype: string; Proc: pointer): pointer;overload;

{======================================================================)
(======== class =======================================================)
(======================================================================)
( lse_class_count       : get class count of a module
( lse_class_get         : get class by index
( lse_class_fill        : fill class registration record
( lse_class_setup       : setup class
( lse_class_find        : find kernel class by 'module::name'
( lse_class_name        : get class name
( lse_class_desc        : get class description
( lse_class_module      : get owner module of a class
( lse_class_ismc        : is module class?
(----------------------------------------------------------------------}
function lse_class_count(const Module: pointer): integer;
function lse_class_get(const Module: pointer; Index: integer): PLseClassRec;
function lse_class_fill(CR: PLseClassRec; Name, Desc: pchar; VT: TLseValue): PLseClassRec;overload;
function lse_class_fill(CR: PLseClassRec; VT: TLseValue): PLseClassRec;overload;
function lse_class_setup(const Module: pointer; const CR: PLseClassRec): PLseClassRec;overload;
function lse_class_setup(const Module: pointer; const Name: string): PLseClassRec;overload;
function lse_class_find(const Name: string): PLseClassRec;
function lse_class_name(const CR: PLseClassRec): string;
function lse_class_desc(const CR: PLseClassRec): string;
function lse_class_module(const CR: PLseClassRec): pointer;
function lse_class_ismc(const CR: PLseClassRec): boolean;
function lse_class_rec(const KernelClass: pointer): PLseClassRec;

{======================================================================)
(======== method ======================================================)
(======================================================================)
( lse_method_count      : get class method count
( lse_method_get        : get class method
( lse_method_setup      : setup class method
( lse_method_find       : find method by name
( lse_method_name       : get method name
( lse_method_desc       : get method description
( lse_method_type       : get method result type
( lse_method_class      : get method owner class
( lse_method_param_count: get method argument count
( lse_method_param_name : get method parametre name
( lse_method_param_type : get method parametre type
( lse_method_bind       : set binding data of kernel method
( lse_method_get_bind   : get binding data of kernel method
( lse_method_is_creator : is constructor function?
( lse_method_get_proc   : get method procedure
( lse_method_set_proc   : set method procedure
(----------------------------------------------------------------------}
function lse_method_count(const CR: PLseClassRec): integer;
function lse_method_get(const CR: PLseClassRec; Index: integer): PLseClassRec;
function lse_method_setup(const CR: PLseClassRec; const MR: PLseFuncRec): pointer;overload;
function lse_method_setup(const CR: PLseClassRec; const Prototype: string; Proc: pointer): pointer;overload;
function lse_method_find(const CR: PLseClassRec; const Name: string): pointer;
function lse_method_name(const Method: pointer): string;
function lse_method_desc(const Method: pointer): string;
function lse_method_type(const Method: pointer): PLseClassRec;
function lse_method_class(const Method: pointer): PLseClassRec;
function lse_method_module(const Method: pointer): pointer;
function lse_method_param_count(const Method: pointer): integer;
function lse_method_param_name(const Method: pointer; Index: integer): string;
function lse_method_param_type(const Method: pointer; Index: integer): PLseClassRec;
function lse_method_bind(const Method: pointer; Data: pointer): pointer;
function lse_method_get_bind(const Method: pointer): pointer;
function lse_method_is_creator(const Method: pointer): boolean;
function lse_method_get_proc(const Method: pointer): pointer;
function lse_method_set_proc(const Method, NewProc: pointer): pointer;

{======================================================================)
(======== reference ===================================================)
(======================================================================)
( lse_incRefcount: increase reference count of a TLseObject
( lse_decRefcount: decrease reference count of a TLseObject
(----------------------------------------------------------------------}
function lse_incRefcount(const obj: pointer): integer;cdecl;
function lse_decRefcount(const obj: pointer): integer;cdecl;

{======================================================================)
(======== memory ======================================================)
(======================================================================)
( lse_mem_alloc     : allocate memroy
( lse_mem_alloc_zero: allocate memroy and fill with zero
( lse_mem_free      : free memory
(----------------------------------------------------------------------}
function  lse_mem_alloc(count: integer): pointer;
function  lse_mem_alloc_zero(count: integer): pointer;
procedure lse_mem_free(const memory: pointer; count: integer);

{======================================================================)
(======== ENV - get/set environment values ============================)
(======================================================================)
( lse_getenv       : get environment value
( lse_getenv_count : get environment value count
( lse_getenv_string: get environment value by index
(----------------------------------------------------------------------}
function lse_getenv(const env_name: string): string;
function lse_getenv_count: integer;
function lse_getenv_string(Index: integer): string;

{======================================================================)
(======== GMT - encode/decode GMT datetime ============================)
(======================================================================)
( lse_encode_GMT: encode GMT datetime to string
( lse_decode_GMT: decode GMT string to datetime
(----------------------------------------------------------------------}
function lse_encode_GMT(Date: TDateTime): string;
function lse_decode_GMT(const GMT: string): TDateTime;

{======================================================================)
(======== dynamic library =============================================)
(======================================================================)
( lse_load_library: load dynamic library
( lse_free_library: close dynamic library
( lse_get_proc    : get procedure address
(----------------------------------------------------------------------}
function  lse_load_library(const FileName: string; var Handle: THandle): boolean;
procedure lse_free_library(Handle: THandle);
function  lse_get_proc(Handle: THandle; const ProcName: string): pointer;

{======================================================================)
(======== TUPSP - encode/decode connection string =====================)
(======================================================================)
( lse_encode_TUPSP: encode database connection parametres
( lse_decode_TUPSP: decode database connection parametres
(----------------------------------------------------------------------}
function  lse_encode_TUPSP(const Target, User, Password, Source, Params: string): string;
procedure lse_decode_TUPSP(const S: string; var Target, User, Password, Source, Params: string);

{======================================================================)
(======== database vendor =============================================)
(======================================================================)
( lse_dbv_register: register database vendor
( lse_dbv_check   : check dbv record for registration
( lse_dbv_provide : create new database agent
(----------------------------------------------------------------------}
function lse_dbv_check(dbv: PLseDBVendor): boolean;
function lse_dbv_register(dbv: PLseDBVendor): boolean;
function lse_dbv_provide(dbv: PLseDBVendor): PLseDB;

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
( lse_ds_field_class: get field class record by index
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
function  lse_ds_field_type(dso: PLseDS; Index: integer): integer;
function  lse_ds_field_class(dso: PLseDS; Index: integer): PLseClassRec;
function  lse_ds_getfi(dso: PLseDS; Index: integer): integer;
function  lse_ds_getfs(dso: PLseDS; Index: integer): PLseString;
function  lse_ds_getfb(dso: PLseDS; Index: integer): boolean;
function  lse_ds_getfd(dso: PLseDS; Index: integer): double;
function  lse_ds_getfm(dso: PLseDS; Index: integer): currency;

{======================================================================)
(======== PLseString ==================================================)
(======================================================================)
( lse_strec_alloc  : convert C/Pascal/buffer to lysee string
( lse_strec_inclife: increase string life
( lse_strec_declife: decrease string life
( lse_strec_addref : increase string life for builtin string class
( lse_strec_release: decrease string life for builtin string class
( lse_strec_data   : translate PLseString to C string
( lse_strec_length : get string length
( lse_strec_string : translate PLseString to PASCAL string
( lse_strec_dup    : clone string
( lse_strec_cat    : concat two string
( lse_strec_same   : compare two string
(----------------------------------------------------------------------}
function  lse_strec_alloc(const AStr: pchar; Count: integer): PLseString;overload;
function  lse_strec_alloc(const AStr: string): PLseString;overload;
procedure lse_strec_inclife(strec: PLseString);
procedure lse_strec_declife(strec: PLseString);
function  lse_strec_addref(const strec: pointer): integer;cdecl;
function  lse_strec_release(const strec: pointer): integer;cdecl;
function  lse_strec_data(strec: PLseString): pchar;
function  lse_strec_length(strec: PLseString): integer;
function  lse_strec_string(strec: PLseString): string;
function  lse_strec_dup(strec: PLseString): PLseString;
function  lse_strec_cat(S1, S2: PLseString): PLseString;overload;
function  lse_strec_cat(S1: PLseString; const S2: string): PLseString;overload;
function  lse_strec_cat(const S1: string; S2: PLseString): PLseString;overload;
function  lse_strec_same(S1, S2: PLseString): boolean;

{======================================================================)
(======== stream ======================================================)
(======================================================================)
( lse_wrap_stream   : wrap TStream to PLseStream
( lse_memory_stream : wrap a TMemoryStream
( lse_file_stream   : wrap a TFileStream
( lse_stream_addref : increase refcount
( lse_stream_release: decrease refcount
( lse_stream_write  : write to stream
( lse_stream_writeln: write line to stream
( lse_stream_read   : read from stream
( lse_stream_readln : read line from stream
( lse_stream_fill   : fill data from another stream
( lse_stream_eof    : if reach end of stream
( lse_stream_flush  : flush dirty data into disk file or ...
( lse_stream_close  : close stream
( lse_stream_closed : check if stream has been closed
( lse_stream_seek   : seek stream position
( lse_stream_pos    : get stream position
( lse_stream_size   : get stream size
( lse_stream_resize : reset stream size
(----------------------------------------------------------------------}
function  lse_wrap_stream(const Stream: TStream; FreeStream: boolean): PLseStream;
function  lse_memory_stream: PLseStream;
function  lse_file_stream(const FileName: string; Mode: word): PLseStream;
function  lse_stream_addref(const obj: pointer): integer;cdecl;
function  lse_stream_release(const obj: pointer): integer;cdecl;
function  lse_stream_write(Stream: PLseStream; const S: PLseString): integer;overload;
function  lse_stream_write(Stream: PLseStream; const S: string): integer;overload;
function  lse_stream_write(Stream: PLseStream; const S: pchar; Count: integer): integer;overload;
function  lse_stream_write(Stream: TStream; const S: PLseString): integer;overload;
function  lse_stream_write(Stream: TStream; const S: string): integer;overload;
function  lse_stream_write(Stream: TStream; const S: pchar; Count: integer): integer;overload;
function  lse_stream_writeln(Stream: PLseStream): integer;overload;
function  lse_stream_writeln(Stream: PLseStream; const S: string): integer;overload;
function  lse_stream_writeln(Stream: PLseStream; const S: PLseString): integer;overload;
function  lse_stream_writeln(Stream: TStream): integer;overload;
function  lse_stream_writeln(Stream: TStream; const S: string): integer;overload;
function  lse_stream_writeln(Stream: TStream; const S: PLseString): integer;overload;
function  lse_stream_read(Stream: PLseStream; const S: pchar; Count: integer): integer;
function  lse_stream_readln(Stream: PLseStream): PLseString;
function  lse_stream_read_text(Stream: PLseStream; Count: integer): string;
function  lse_stream_read_line(Stream: PLseStream): string;
function  lse_stream_fill(Stream, Source: PLseStream; Count: integer = MaxInt): integer;
function  lse_stream_eof(Stream: PLseStream): boolean;
procedure lse_stream_flush(Stream: PLseStream);
procedure lse_stream_close(Stream: PLseStream);
function  lse_stream_closed(Stream: PLseStream): boolean;
function  lse_stream_seek(Stream: PLseStream; Offset: int64; Origin: integer = SSF_BEGINNING): int64;
function  lse_stream_pos(Stream: PLseStream): int64;
function  lse_stream_size(Stream: PLseStream): int64;
function  lse_stream_resize(Stream: PLseStream; NewSize: int64): int64;

{======================================================================)
(======== value =======================================================)
(======================================================================)
( lse_init_value : initialize PLseValue record
( lse_clear_value: clear PLseValue record, result type void
( lse_is_defv    : test if is default value
( lse_is_object  : test if is object type
( lse_is_void    : test if is void type
( lse_vtype      : get value type
( lse_class      : get value class record
( lse_set_XXXX   : change value
(----------------------------------------------------------------------}
function lse_init_value(V: PLseValue): PLseValue;
function lse_clear_value(V: PLseValue): PLseValue;
function lse_inclife(V: PLseValue): PLseValue;
function lse_declife(V: PLseValue): PLseValue;
function lse_set_nil(V: PLseValue): PLseValue;
function lse_is_defv(V: PLseValue): boolean;
function lse_is_object(V: PLseValue): boolean;
function lse_is_void(V: PLseValue): boolean;
function lse_vtype(V: PLseValue): TLseValue;
function lse_class(V: TLseKernelClass): PLseClassRec;overload;
function lse_class(V: TLseValue): PLseClassRec;overload;
function lse_class(V: PLseValue): PLseClassRec;overload;
function lse_set_void(V: PLseValue): PLseValue;
function lse_set_int64(V: PLseValue; Value: int64): PLseValue;
function lse_set_integer(V: PLseValue; Value: integer): PLseValue;
function lse_set_float(V: PLseValue; Value: double): PLseValue;
function lse_set_money(V: PLseValue; Value: currency): PLseValue;
function lse_set_time(V: PLseValue; Value: TDateTime): PLseValue;
function lse_set_bool(V: PLseValue; Value: boolean): PLseValue;
function lse_set_char(V: PLseValue; Value: char): PLseValue;
function lse_set_string(V: PLseValue; Value: PLseString): PLseValue;overload;
function lse_set_string(V: PLseValue; const Value: string): PLseValue;overload;
function lse_set_string(V: PLseValue; const Value: pchar): PLseValue;overload;
function lse_set_string(V: PLseValue; const Value: pchar; Length: integer): PLseValue;overload;
function lse_set_object(V: PLseValue; CR: PLseClassRec; Value: pointer): PLseValue;
function lse_set_class(V: PLseValue; Value: PLseClassRec): PLseValue;
function lse_set_db(V: PLseValue; Value: PLseDB): PLseValue;
function lse_set_ds(V: PLseValue; Value: PLseDS): PLseValue;
function lse_set_stream(V: PLseValue; Value: PLseStream): PLseValue;
function lse_set_vargen(V: PLseValue; Value: PLseVargen): PLseValue;
function lse_set_value(V: PLseValue; Value: PLseValue): PLseValue;

{======================================================================)
(======== vargen - variant generator ==================================)
(======================================================================)
( lse_vargen_addref : increase reference count
( lse_vargen_release: decrease reference count
( lse_vargen_eof    : check end of the vargen
( lse_vargen_send   : get next value
( lse_vargen_rewind : rewind to head of the vargen
( lse_vargen_none   : an empty variant generator
( lse_vargen_ensure : ensure a non-zero vargen
( lse_vargen_this   : get a non-zero vargen as this
(----------------------------------------------------------------------}
function lse_vargen_addref(const VG: pointer): integer;cdecl;
function lse_vargen_release(const VG: pointer): integer;cdecl;
function lse_vargen_eof(VG: PLseVargen): boolean;
function lse_vargen_send(VG: PLseVargen; Value: PLseValue): boolean;
function lse_vargen_rewind(VG: PLseVargen): boolean;
function lse_vargen_contains(VG: PLseVargen; Value: PLseValue): boolean;
function lse_vargen_none: PLseVargen;
function lse_vargen_ensure(VG: PLseVargen): PLseVargen;
function lse_vargen_this(Param: PLseParam): PLseVargen;

{======================================================================)
(======== components ==================================================)
(======================================================================)
( lse_setup_components: setup LSE components
( Register            : register LSE Delphi/Lazarus components
(----------------------------------------------------------------------}
procedure lse_setup_components(const Components: array of TLseComponent);
procedure Register;

{======================================================================)
(======== stdio =======================================================)
(======================================================================)
( lse_stdin : standard input handler
( lse_stdout: standard output handler
( lse_stderr: standard error handler
(----------------------------------------------------------------------}
function lse_stdin: integer;
function lse_stdout: integer;
function lse_stderr: integer;

{======================================================================)
(======== misc ========================================================)
(======================================================================)
( lse_error         : raise exception
( lse_exception_str : get execption string
( lse_call_gate     : convert Param to TLseInvoke and call it
( lse_veryPD        : correct path delimiter
( lse_veryUD        : correct URL delimiter
( lse_expand_fname  : expand file name
( lse_complete_fname: add file extension when necessary
( lse_is_lsp_file   : is LSP file?
( lse_remove_item   : remove list item
( lse_vary_index    : adjust list index
( lse_vary_range    : adjust list index and range count
( lse_check_index   : check index in list range
(----------------------------------------------------------------------}
procedure lse_error(const error: string);overload;
procedure lse_error(const error: string; const Args: array of const);overload;
function  lse_exception_str: string;
procedure lse_call_gate(const Call: TLseFuncInvoke; const Param: PLseParam);cdecl;
function  lse_veryPD(const Path: string): string;
function  lse_veryUD(const URL: string): string;
function  lse_expand_fname(const fname: string): string;
function  lse_complete_fname(const fname: string): string;
function  lse_is_lsp_file(const fname: string): boolean;
procedure lse_remove_item(List: TList; Item: pointer);
function  lse_vary_index(index, length: int64): int64;
function  lse_vary_range(index, length: int64; var count: int64): int64;
procedure lse_check_index(index, range: int64);

implementation

procedure lse_executing(Engine: PLseEngine);cdecl;
begin
  Engine^.er_engine.EventExecuting;
end;

procedure lse_executed(Engine: PLseEngine);cdecl;
begin
  Engine^.er_engine.EventExecuted;
end;

function lse_password(Engine: PLseEngine): PLseString;cdecl;
var
  E: TLseEngine;
  S: string;
begin
  E := Engine^.er_engine;
  if Assigned(E.FOnPassword) then
  begin
    S := '';
    E.FOnPassword(E, S);
    Result := lse_strec_alloc(S);
  end
  else Result := lse_stream_readln(@E.FStdin);
end;

procedure lse_exec_function(const Param: PLseParam);cdecl;
var
  func: TLseFunc;
begin
  func := TLseFunc(lse_method_get_bind(Param^.func));
  func.Execute(Param);
end;

procedure lse_exec_method(const Param: PLseParam);cdecl;
var
  func: TLseMethod;
begin
  func := TLseMethod(lse_method_get_bind(Param^.func));
  func.Execute(Param);
end;

procedure lse_exec_constructor(const Param: PLseParam);cdecl;
var
  func: TLseMethod;
  clss: TLseClass;
  nobj: TLseObject;
begin
  func := TLseMethod(lse_method_get_bind(Param^.func));
  nobj := nil;
  clss := func.FClass;
  if Assigned(clss.FOnGetObject) then
    clss.FOnGetObject(clss, nobj);
  if nobj = nil then
    nobj := TLseObject.Create;
  nobj.IncRefcount;
  try
    lse_set_object(Param^.param[0], clss.ClassRec, nobj);
    func.Execute(Param);
  finally
    lse_set_object(Param^.result, clss.ClassRec, nobj);
    nobj.DecRefcount;
  end;
end;

{======================================================================)
(======== module ======================================================)
(======================================================================}

function lse_module_count: integer;
begin
  Result := lse_entries^.cik_module_count();
end;

function lse_module_get(Index: integer): pointer;
begin
  Result := lse_entries^.cik_module(Index);
end;

function lse_module_setup(const Name, FileName: string; MR: PLseModuleRec): pointer;
var
  mname: string;
begin
  if FileName <> '' then
    mname := Name + '=' + FileName else
    mname := Name;
  Result := lse_entries^.cik_module_setup(pchar(mname), MR);
end;

function lse_module_setup(const Name, FileName: string): pointer;
var
  MR: RLseModuleRec;
begin
  FillChar(MR, sizeof(RLseModuleRec), 0);
  MR.iw_version := LSE_VERSION;
  Result := lse_module_setup(Name, FileName, @MR);
end;

function lse_module_find(const Name: string): pointer;
begin
  Result := lse_entries^.cik_module_find(pchar(Name));
end;

function lse_module_class(const Module: pointer): PLseClassRec;
begin
  Result := lse_entries^.cik_module_class(Module);
end;

function lse_module_name(const Module: pointer): string;
begin
  Result := lse_class_name(lse_module_class(Module));
end;

function lse_module_desc(const Module: pointer): string;
begin
  Result := lse_entries^.cik_module_desc(Module);
end;

function lse_module_func_setup(const Module: pointer; const FR: PLseFuncRec): pointer;overload;
begin
  Result := lse_method_setup(lse_module_class(Module), FR);
end;

function lse_module_func_setup(const Module: pointer; const Prototype: string; Proc: pointer): pointer;overload;
begin
  Result := lse_method_setup(lse_module_class(Module), Prototype, Proc);
end;

{======================================================================)
(======== class =======================================================)
(======================================================================}

function lse_class_count(const Module: pointer): integer;
begin
  Result := lse_entries^.cik_class_count(Module);
end;

function lse_class_get(const Module: pointer; Index: integer): PLseClassRec;
begin
  Result := lse_entries^.cik_class(Module, Index);
end;

function lse_class_fill(CR: PLseClassRec; Name, Desc: pchar; VT: TLseValue): PLseClassRec;
begin
  Result := CR;
  FillChar(CR^, sizeof(RLseClassRec), 0);
  CR^.vtype := Ord(VT);
  CR^.name := Name;
  CR^.desc := Desc;
  CR^.incRefcount := @lse_incRefcount;
  CR^.decRefcount := @lse_decRefcount;
  CR^.lysee_class := nil;
end;

function lse_class_fill(CR: PLseClassRec; VT: TLseValue): PLseClassRec;
begin
  Result := lse_class_fill(CR, pchar(lse_vtype_names[VT]), nil, VT);
end;

function lse_class_setup(const Module: pointer; const CR: PLseClassRec): PLseClassRec;
begin
  Result := lse_entries^.cik_class_setup(Module, CR);
end;

function lse_class_setup(const Module: pointer; const Name: string): PLseClassRec;
var
  R: RLseClassRec;
begin
  FillChar(R, sizeof(RLseClassRec), 0);
  R.vtype := LSV_OBJECT;
  R.name := pchar(Name);
  R.desc := nil;
  R.incRefcount := @lse_incRefcount;
  R.decRefcount := @lse_decRefcount;
  Result := lse_class_setup(Module, PLseClassRec(@R));
end;

function lse_class_find(const Name: string): PLseClassRec;
begin
  Result := lse_entries^.cik_class_find(pchar(Name));
end;

function lse_class_name(const CR: PLseClassRec): string;
begin
  if CR <> nil then
    Result := CR^.name else
    Result := '';
end;

function lse_class_desc(const CR: PLseClassRec): string;
begin
  if CR <> nil then
    Result := CR^.desc else
    Result := '';
end;

function lse_class_module(const CR: PLseClassRec): pointer;
begin
  Result := lse_entries^.cik_class_module(CR);
end;

function lse_class_ismc(const CR: PLseClassRec): boolean;
begin
  Result := (CR <> nil) and (CR = lse_module_class(lse_class_module(CR)));
end;

function lse_class_rec(const KernelClass: pointer): PLseClassRec;
begin
  Result := lse_entries^.cik_class_rec(KernelClass);
end;

{======================================================================)
(======== method ======================================================)
(======================================================================}

function lse_method_count(const CR: PLseClassRec): integer;
begin
  Result := lse_entries^.cik_method_count(CR);
end;

function lse_method_get(const CR: PLseClassRec; Index: integer): PLseClassRec;
begin
  Result := lse_entries^.cik_method(CR, Index);
end;

function lse_method_setup(const CR: PLseClassRec; const MR: PLseFuncRec): pointer;
begin
  Result := lse_entries^.cik_method_setup(CR, MR);
end;

function lse_method_setup(const CR: PLseClassRec; const Prototype: string; Proc: pointer): pointer;
var
  MR: RLseFuncRec;
begin
  MR.fr_prot := pchar(Prototype);
  MR.fr_addr := Proc;
  MR.fr_desc := nil;
  Result := lse_method_setup(CR, @MR);
end;

function lse_method_find(const CR: PLseClassRec; const Name: string): pointer;
begin
  Result := lse_entries^.cik_method_find(CR, pchar(Name));
end;

function lse_method_name(const Method: pointer): string;
begin
  Result := lse_entries^.cik_method_name(Method);
end;

function lse_method_desc(const Method: pointer): string;
begin
  Result := lse_entries^.cik_method_desc(Method);
end;

function lse_method_type(const Method: pointer): PLseClassRec;
begin
  Result := lse_entries^.cik_method_type(Method);
end;

function lse_method_class(const Method: pointer): PLseClassRec;
begin
  Result := lse_entries^.cik_method_class(Method);
end;

function lse_method_module(const Method: pointer): pointer;
begin
  Result := lse_class_module(lse_method_class(Method));
end;

function lse_method_param_count(const Method: pointer): integer;
begin
  Result := lse_entries^.cik_method_param_count(Method);
end;

function lse_method_param_name(const Method: pointer; Index: integer): string;
begin
  Result := lse_entries^.cik_method_param_name(Method, Index);
end;

function lse_method_param_type(const Method: pointer; Index: integer): PLseClassRec;
begin
  Result := lse_entries^.cik_method_param_type(Method, Index);
end;

function lse_method_bind(const Method: pointer; Data: pointer): pointer;
begin
  Result := lse_entries^.cik_method_bind(Method, Data);
end;

function lse_method_get_bind(const Method: pointer): pointer;
begin
  Result := lse_entries^.cik_method_get_bind(Method);
end;

function lse_method_is_creator(const Method: pointer): boolean;
begin
  Result := (lse_entries^.cik_method_is_creator(Method) > 0);
end;

function lse_method_get_proc(const Method: pointer): pointer;
begin
  Result := lse_entries^.cik_method_get_proc(Method);
end;

function lse_method_set_proc(const Method, NewProc: pointer): pointer;
begin
  Result := lse_entries^.cik_method_set_proc(Method, NewProc);
end;

{======================================================================)
(======== reference ===================================================)
(======================================================================}

function lse_incRefcount(const obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := TLseObject(obj).IncRefcount else
    Result := 0;
end;

function lse_decRefcount(const obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := TLseObject(obj).DecRefcount else
    Result := 0;
end;

{======================================================================)
(======== memory ======================================================)
(======================================================================}

function lse_mem_alloc(count: integer): pointer;
begin
  Result := nil;
  if count > 0 then
    GetMem(Result, count);
end;

function lse_mem_alloc_zero(count: integer): pointer;
begin
  Result := lse_mem_alloc(count);
  if Result <> nil then
    FillChar(Result^, count, 0);
end;

procedure lse_mem_free(const memory: pointer; count: integer);
begin
  if memory <> nil then
    if count > 0 then
      FreeMem(memory, count) else
      FreeMem(memory);
end;

{======================================================================)
(======== ENV - get/set environment values ============================)
(======================================================================}

function lse_getenv(const env_name: string): string;
begin
  Result := SysUtils.GetEnvironmentVariable(env_name);
end;

function lse_getenv_count: integer;
{$IFNDEF FPC}
var
  H, P: pchar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentVariableCount;
  {$ELSE}
  Result := 0;
  P := GetEnvironmentStrings;
  H := P;
  if H <> nil then
    while H^ <> #0 do
    begin
      Inc(Result);
      H := H + strlen(H) + 1;
    end;
  FreeEnvironmentStrings(P);
  {$ENDIF}
end;

function lse_getenv_string(Index: integer): string;
{$IFNDEF FPC}
var
  H, P: pchar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentString(Index + 1);
  {$ELSE}
  Result := '';
  P := GetEnvironmentStrings;
  H := P;
  if H <> nil then
  begin
    while (H^ <> #0) and (Index > 0) do
    begin
      H := H + strlen(H) + 1;
      Dec(Index);
    end;
    if (H^ <> #0) and (Index = 0) then
      Result := H;
  end;
  FreeEnvironmentStrings(P);
  {$ENDIF}
end;

{======================================================================)
(======== GMT - encode/decode GMT datetime ============================)
(======================================================================}

const
  GMT_WEEKDAY: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'
  );

  GMT_MONTH: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );

  GMT_FORMAT = '"%s", dd "%s" yyyy hh:nn:ss "GMT"';
  
function lse_encode_GMT(Date: TDateTime): string;
var
  week, month: string;
  
  function weekday_str(DateTime: TDateTime): string;
  begin
    Result := GMT_WEEKDAY[DayOfWeek(DateTime)];
  end;

  function month_str(DateTime: TDateTime): string;
  var
    year, month, day: word;
  begin
    DecodeDate(DateTime, year, month, day);
    Result := GMT_MONTH[month];
  end;

begin
  if Date > 0 then
  begin
    week := weekday_str(Date);
    month := month_str(Date);
    Result := Format(FormatDateTime(GMT_FORMAT, Date), [week, month])
  end
  else Result := '';
end;

function lse_decode_GMT(const GMT: string): TDateTime;
var
  yy, mm, dd, hh, nn, ss: Integer;
  line: string;

  function get_token(var token: string): boolean;
  const
    TOKENCH = ['A'..'Z', 'a'..'z', '0'..'9'];
  var
    index, count: integer;
  begin
    count := Length(line);
    index := 1;
    while (index < count) and (line[index] in TOKENCH) do Inc(index);
    Result := (index < count) and (index > 1);
    if Result then
    begin
      token := Copy(line, 1, index - 1);
      while (index < count) and not (line[index] in TOKENCH) do Inc(index);
      line := Copy(line, index, MaxInt);
    end;
  end;

  function skip_weekday: boolean;
  var
    token: string;
    index: integer;
  begin
    Result := get_token(token);
    if Result then
    begin
      token := Copy(token, 1, 3);
      for index := Low(GMT_WEEKDAY) to High(GMT_WEEKDAY) do
        if AnsiSameText(token, GMT_WEEKDAY[index]) then
          Exit;
      Result := false;
    end;
  end;

  function get_month(var month: integer): boolean;
  var
    token: string;
    index: integer;
  begin
    Result := get_token(token);
    if Result then
    begin
      token := Copy(token, 1, 3);
      for index := Low(GMT_MONTH) to High(GMT_MONTH) do
        if AnsiSameText(token, GMT_MONTH[index]) then
        begin
          month := index;
          Exit;
        end;
      Result := false;
    end;
  end;

  function get_int(var value: integer; min, max: integer): boolean;
  var
    token: string;
  begin
    Result := get_token(token);
    if Result then
    begin
      Result := TryStrToInt(token, value);
      if Result then
        Result := (value >= min) and (value <= max);
    end;
  end;

begin
  { Sun, 20 Jan 2008 14:58:45 GMT }
  Result := -1;
  line := Trim(GMT);
  if not (skip_weekday and get_int(dd, 1, 31) and get_month(mm) and
          get_int(yy, 1, 9999) and get_int(hh, 0, 24) and
          get_int(nn, 0, 60) and get_int(ss, 0, 60) and
          TryEncodeDateTime(yy, mm, dd, hh, nn, ss, 0, Result)) then
    Result := -1;
end;

{======================================================================)
(======== DLL: load/free dynamic library ==============================)
(======================================================================}

function lse_load_library(const FileName: string; var Handle: THandle): boolean;
begin
  Handle := LoadLibrary(pchar(FileName));
  Result := (Handle <> 0);
end;

procedure lse_free_library(Handle: THandle);
begin
  FreeLibrary(Handle);
end;

function lse_get_proc(Handle: THandle; const ProcName: string): pointer;
begin
  Result := GetProcAddress(Handle, pchar(ProcName));
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
(======== lse_dbv - database vendor functions =========================)
(======================================================================}

function lse_dbv_check(dbv: PLseDBVendor): boolean;
begin
  Result := (dbv <> nil) and Assigned(dbv^.dv_create) and
            (dbv^.dv_name[0] <> #0);
end;

function lse_dbv_register(dbv: PLseDBVendor): boolean;
begin
  Result := lse_entries^.cik_dbv_register(dbv) <> 0;
end;

function lse_dbv_provide(dbv: PLseDBVendor): PLseDB;
begin
  Result := dbv^.dv_create();
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
  Result := lse_entries^.cik_dbv_provide(pchar(Vendor));
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

function lse_ds_field_type(dso: PLseDS; Index: integer): integer;
begin
  Result := dso^.ds_getft(dso, Index);
  lse_ds_check(dso);
  if not (Result in [LSV_INT, LSV_FLOAT, LSV_MONEY, LSV_BOOL]) then
    Result := LSV_STRING;
end;

function lse_ds_field_class(dso: PLseDS; Index: integer): PLseClassRec;
begin
  case lse_ds_field_type(dso, Index) of
    LSV_INT   : Result := lse_class(LSV_INT);
    lSV_STRING: Result := lse_class(LSV_STRING);
    LSV_FLOAT : Result := lse_class(LSV_FLOAT);
    LSV_MONEY : Result := lse_class(LSV_MONEY);
    LSV_BOOL  : Result := lse_class(LSV_BOOL);
           else Result := lse_class(LSV_STRING);
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

function lse_ds_getfm(dso: PLseDS; Index: integer): currency;
begin
  Result := dso^.ds_getfm(dso, Index);
  lse_ds_check(dso);
end;

{======================================================================)
(======== PLseString ==================================================)
(======================================================================}

procedure lse_strec_free(const strec: PLseString);cdecl;
begin
  if strec <> nil then
    lse_mem_free(strec, sizeof(RLseString) + strec^.sr_slen + 1);
end;

function lse_strec_alloc(const AStr: pchar; Count: integer): PLseString;overload;
var
  data: pchar;
begin
  Result := nil;
  if Count > 0 then
  begin
    Result := lse_mem_alloc(sizeof(RLseString) + Count + 1);
    Result^.sr_life := 0;
    Result^.sr_slen := Count;
    Result^.sr_free := @lse_strec_free;
    data := lse_strec_data(Result);
    if AStr <> nil then
      Move(AStr^, data^, Count);
    data[Count] := #0;
  end;
end;

function lse_strec_alloc(const AStr: string): PLseString;
begin
  Result := lse_strec_alloc(pchar(AStr), Length(AStr));
end;

procedure lse_strec_inclife(strec: PLseString);
begin
  if strec <> nil then
    Inc(strec^.sr_life);
end;

procedure lse_strec_declife(strec: PLseString);
begin
  if strec <> nil then
  begin
    Dec(strec^.sr_life);
    if strec^.sr_life < 1 then
      if Assigned(strec^.sr_free) then
        strec^.sr_free(strec);
  end;
end;

function lse_strec_addref(const strec: pointer): integer;cdecl;
var
  S: PLseString;
begin
  S := PLseString(strec);
  if S <> nil then
  begin
    Inc(S^.sr_life);
    Result := S^.sr_life;
  end
  else Result := 0;
end;

function lse_strec_release(const strec: pointer): integer;cdecl;
var
  S: PLseString;
begin
  S := PLseString(strec);
  if S <> nil then
  begin
    Dec(S^.sr_life);
    Result := S^.sr_life;
    if Result < 1 then
      if Assigned(S^.sr_free) then
        S^.sr_free(S);
  end
  else Result := 0;
end;

function lse_strec_data(strec: PLseString): pchar;
begin
  if strec <> nil then
    Result := pchar(strec) + sizeof(RLseString) else
    Result := nil;
end;

function lse_strec_length(strec: PLseString): integer;
begin
  if strec <> nil then
    Result := strec^.sr_slen else
    Result := 0;
end;

function lse_strec_string(strec: PLseString): string;
begin
  if strec <> nil then
    SetString(Result, lse_strec_data(strec), strec^.sr_slen) else
    Result := '';
end;

function lse_strec_dup(strec: PLseString): PLseString;
begin
  if strec <> nil then
    Result := lse_strec_alloc(lse_strec_data(strec),
      lse_strec_length(strec)) else
    Result := nil;
end;

function lse_strec_cat(S1, S2: PLseString): PLseString;
var
  L1, l2: integer;
  base: pchar;
begin
  L1 := lse_strec_length(S1);
  if L1 = 0 then
  begin
    Result := S2; // lse_strec_dup(S2);
    Exit;
  end;

  L2 := lse_strec_length(S2);
  if L2 = 0 then
  begin
    Result := S1; // lse_strec_dup(S1);
    Exit;
  end;

  Result := lse_strec_alloc(nil, L1 + L2);
  base := lse_strec_data(Result);
  Move(lse_strec_data(S1)^, base^, L1);
  Inc(base, L1);
  Move(lse_strec_data(S2)^, base^, L2);
end;

function lse_strec_cat(S1: PLseString; const S2: string): PLseString;overload;
var
  L1, l2: integer;
  base: pchar;
begin
  L1 := lse_strec_length(S1);
  if L1 = 0 then
  begin
    Result := lse_strec_alloc(S2);
    Exit;
  end;

  L2 := Length(S2);
  if L2 = 0 then
  begin
    Result := S1; // lse_strec_dup(S1);
    Exit;
  end;

  Result := lse_strec_alloc(nil, L1 + L2);
  base := lse_strec_data(Result);
  Move(lse_strec_data(S1)^, base^, L1);
  Inc(base, L1);
  Move(pchar(S2)^, base^, L2);
end;

function lse_strec_cat(const S1: string; S2: PLseString): PLseString;overload;
var
  L1, l2: integer;
  base: pchar;
begin
  L1 := Length(S1);
  if L1 = 0 then
  begin
    Result := S2; // lse_strec_dup(S2);
    Exit;
  end;

  L2 := lse_strec_length(S2);
  if L2 = 0 then
  begin
    Result := lse_strec_alloc(S1);
    Exit;
  end;

  Result := lse_strec_alloc(nil, L1 + L2);
  base := lse_strec_data(Result);
  Move(pchar(S1)^, base^, L1);
  Inc(base, L1);
  Move(lse_strec_data(S2)^, base^, L2);
end;

function lse_strec_same(S1, S2: PLseString): boolean;
begin
  if (S1 <> S2) and (lse_strec_length(S1) = lse_strec_length(S2)) then
    Result := (StrComp(lse_strec_data(S1), lse_strec_data(S2)) = 0) else
    Result := false;
end;

{======================================================================)
(======== kernel - load/manifest lysee kernel =========================)
(======================================================================}

function lse_prepare(QE: TLseQueryEntry): boolean;
begin
  if lse_entries = nil then
    if Assigned(QE) then
      lse_entries := PLseEntryRec(QE('qe_entries'));
  Result := (lse_entries <> nil);
end;

function lse_query(const ID: string): pointer;
begin
  if ID <> '' then
    Result := lse_entries^.cik_query(pchar(ID)) else
    Result := nil;
end;

procedure lse_load_kernel(const KernelFile: string);
const
  FLLK = 'Failed loading lysee kernel: %s';
var
  handle: THandle;
  knfile: string;
begin
  if lse_entries = nil then
  begin
    knfile := lse_expand_fname(KernelFile);
    if FileExists(knfile) then
    begin
      handle := 0;
      if lse_load_library(knfile, handle) then
      try
        lse_prepare(TLseQueryEntry(lse_get_proc(handle, 'QueryEntry')));
        if lse_startup then
          lse_set_kernel_file(knfile) else
          lse_error(FLLK, [knfile]);
      except
        lse_free_library(handle);
        raise;
      end
      else lse_error(FLLK, [knfile]);
    end;
  end
  else lse_startup;
end;

procedure lse_load_config(const ConfigFile: string);
begin
  lse_entries^.cik_load_config(pchar(ConfigFile));
end;

function lse_startup: boolean;
begin
  if lse_entries <> nil then
    Result := (lse_entries^.cik_startup() > 0) else
    Result := false;
end;

procedure lse_cleanup;
begin
  if lse_entries <> nil then
    lse_entries^.cik_cleanup();
end;

function lse_kernel_file: string;
begin
  Result := lse_entries^.cik_get_kernel_file();
end;

procedure lse_set_kernel_file(const KernelFile: string);
begin
  lse_entries^.cik_set_kernel_file(pchar(KernelFile));
end;

function lse_program_file: string;
begin
  Result := lse_entries^.cik_get_program_file();
end;

procedure lse_set_program_file(const ProgramFile: string);
begin
  lse_entries^.cik_set_program_file(pchar(ProgramFile));
end;

function lse_kernel_production(IncludeVC: boolean): string;
const
  FMT = '%s %s - %s';
begin
  Result := lse_entries^.cik_production();
  if IncludeVC then
    Result := Format(FMT, [Result, lse_kernel_version, lse_kernel_copyright]);
end;

function lse_kernel_version: string;
begin
  Result := lse_entries^.cik_version();
end;

function lse_kernel_copyright: string;
begin
  Result := lse_entries^.cik_copyright();
end;

procedure lse_kernel_log(const Msg: pchar; Count: integer);
begin
  lse_entries^.cik_log(Msg, Count);
end;

procedure lse_kernel_log(const Msg: string);
begin
  lse_entries^.cik_log(pchar(Msg), Length(Msg));
end;

function lse_simple_test(const Script: string): integer;
begin
  Result := lse_entries^.cik_simple_test(pchar(Script));
end;

function lse_keywords: string;
begin
  Result := lse_entries^.cik_keywords();
end;

{======================================================================)
(======== PLseStream - wrap TStream or else to PLseStream =============)
(======================================================================}

type
  RLseStreamRec = packed record
    wraped: RLseStream; { data -> @RLseStreamRec }
    stream: TStream;
    refcount: integer;
    free_stream: boolean;
  end;
  PLseStreamRec = ^RLseStreamRec;

function LWS_read(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      Result := stream.Read(Buffer^, Count) else
      Result := 0;
  except
    Result := 0;
  end;
end;

function LWS_readln(S: PLseStream): PLseString;cdecl;
var
  stream: TStream;
  ms: TMemoryStream;
  start, bytes: integer;
  base, next: pchar;
  done: boolean;

  function read_data(Count: integer): boolean;
  var
    B: array[0..1023] of char;
    N, L: integer;
  begin
    L := 0;
    repeat
      N := stream.Read(B, Min(Count, sizeof(B)));
      if N > 0 then
      begin
        Dec(Count, N);
        Inc(L, N);
        ms.WriteBuffer(B, N);
      end;
    until (Count = 0) or (N < sizeof(B));
    Result := (L > 0);
    if Result then
    begin
      bytes := ms.Size;
      base := ms.Memory;
      next := base + start;
    end;
  end;

begin
  try
    Result := nil;
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
    begin
      ms := TMemoryStream.Create;
      try
        start := 0;
        bytes := 0;
        done := false;
        while not done and read_data(1024) do
          while not done and (start < bytes) do
            if next^ in [#13, #10] then
            begin
              Result := lse_strec_alloc(base, start);
              if next^ = #13 then
              begin
                if start = bytes - 1 then read_data(1);
                if start < bytes - 1 then
                begin
                  Inc(next);
                  if next^ = #10 then
                    Inc(start) else
                    Dec(next);
                end;
              end;
              stream.Seek((start + 1) - bytes, soCurrent);
              done := true;
            end
            else
            begin
              Inc(start);
              Inc(next);
            end;
        if not done then
          Result := lse_strec_alloc(ms.Memory, ms.Size);
      finally
        ms.Free;
      end;
    end;
  except
    Result := nil;
  end;
end;

function LWS_readln_ms(S: PLseStream): PLseString;cdecl;
var
  stream: TMemoryStream;
  base, next, endp: pchar;
begin
  try
    Result := nil;
    stream := TMemoryStream(PLseStreamRec(S^.data)^.stream);
    if stream <> nil then
    begin
      endp := pchar(stream.Memory) + stream.Size;
      base := pchar(stream.Memory) + stream.Position;
      next := base;
      while next < endp do
        if next^ in [#13, #10] then
        begin
          Result := lse_strec_alloc(base, next - base);
          if (next^ = #13) and ((endp - next) > 1) then
          begin
            Inc(next);
            if next^ <> #10 then
              Dec(next);
          end;
          stream.Seek((next - base) + 1, soCurrent);
          Exit;
        end
        else Inc(next);
      Result := lse_strec_alloc(base, endp - base);
      stream.Seek(0, soEnd);
    end;
  except
    Result := nil;
  end;
end;

function LWS_write(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      Result := stream.Write(Buffer^, Count) else
      Result := 0;
  except
    Result := 0;
  end;
end;

function LWS_seek(S: PLseStream; Offset: int64; Origin: integer): int64;cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      Result := stream.Seek(Offset, TSeekOrigin(Origin)) else
      Result := 0;
  except
    Result := 0;
  end;
end;

function LWS_get_size(S: PLseStream): int64;cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      Result := stream.Size else
      Result := 0;
  except
    Result := 0;
  end;
end;

procedure LWS_set_size(S: PLseStream; NewSize: int64);cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      stream.Size := NewSize;
  except
    { do nothing }
  end;
end;

function LWS_eof(S: PLseStream): integer;cdecl;
var
  stream: TStream;
begin
  try
    stream := PLseStreamRec(S^.data)^.stream;
    if stream <> nil then
      Result := Ord(stream.Position >= stream.Size) else
      Result := 1;
  except
    Result := 0;
  end;
end;

procedure LWS_close(S: PLseStream);cdecl;
var
  wraped: PLseStreamRec;
begin
  try
    wraped := PLseStreamRec(S^.data);
    if wraped^.stream <> nil then
      if wraped^.free_stream then
        FreeAndNil(wraped^.stream) else
        wraped^.stream := nil;
  except
    { do nothing }
  end;
end;

function LWS_closed(S: PLseStream): integer;cdecl;
begin
  Result := Ord(PLseStreamRec(S^.data)^.stream = nil);
end;

procedure LWS_flush(S: PLseStream);cdecl;
begin
  { do nothing }
end;

function LWS_addref(S: PLseStream): integer;cdecl;
var
  wraped: PLseStreamRec;
begin
  try
    wraped := PLseStreamRec(S^.data);
    Inc(wraped^.refcount);
    Result := wraped^.refcount;
    if Result <= 0 then
    begin
      if wraped^.free_stream then
        FreeAndNil(wraped^.stream);
      lse_mem_free(wraped, sizeof(RLseStreamRec));
    end; 
  except
    Result := 0;
  end;
end;

function LWS_release(S: PLseStream): integer;cdecl;
var
  wraped: PLseStreamRec;
begin
  try
    wraped := PLseStreamRec(S^.data);
    Dec(wraped^.refcount);
    Result := wraped^.refcount;
    if Result <= 0 then
    begin
      if wraped^.free_stream then
        FreeAndNil(wraped^.stream);
      lse_mem_free(wraped, sizeof(RLseStreamRec));
    end; 
  except
    Result := 0;
  end;
end;

function lse_wrap_stream(const Stream: TStream; FreeStream: boolean): PLseStream;
var
  wraped: PLseStreamRec;
begin
  if Stream <> nil then
  begin
    wraped := lse_mem_alloc_zero(sizeof(RLseStreamRec));
    wraped^.stream := Stream;
    wraped^.free_stream := FreeStream;
    Result := @wraped^.wraped;
    Result^.data     := wraped;
    Result^.read     := @LWS_read;
    if Stream is TMemoryStream then
      Result^.readln := @LWS_readln_ms else
      Result^.readln := @LWS_readln;
    Result^.write    := @LWS_write;
    Result^.seek     := @LWS_seek;
    Result^.get_size := @LWS_get_size;
    Result^.set_size := @LWS_set_size;
    Result^.eof      := @LWS_eof;
    Result^.close    := @LWS_close;
    Result^.closed   := @LWS_closed;
    Result^.flush    := @LWS_flush;
    Result^.addref   := @LWS_addref;
    Result^.release  := @LWS_release;
  end
  else Result := nil;
end;

function lse_memory_stream: PLseStream;
begin
  Result := lse_wrap_stream(TMemoryStream.Create, true);
end;

function lse_file_stream(const FileName: string; Mode: word): PLseStream;
begin
  Result := lse_wrap_stream(TFileStream.Create(lse_veryPD(FileName), Mode), true);
end;

function lse_stream_addref(const obj: pointer): integer;cdecl;
var
  srec: PLseStream;
begin
  srec := PLSeStream(obj);
  if srec = nil then
    Result := 0 else
  if Assigned(srec^.addref) then
    Result := srec^.addref(srec) else
    Result := 1;
end;

function lse_stream_release(const obj: pointer): integer;cdecl;
var
  srec: PLseStream;
begin
  srec := PLSeStream(obj);
  if srec = nil then
    Result := 0 else
  if Assigned(srec^.release) then
    Result := srec^.release(srec) else
    Result := 1;
end;

function lse_stream_write(Stream: PLseStream; const S: PLseString): integer;
begin
  if Assigned(Stream^.write) then
    Result := Stream^.write(Stream, lse_strec_data(S), lse_strec_length(S)) else
    Result := 0;
end;

function lse_stream_write(Stream: PLseStream; const S: string): integer;
begin
  if Assigned(Stream^.write) then
    Result := Stream^.write(Stream, pchar(S), Length(S)) else
    Result := 0;
end;

function lse_stream_write(Stream: PLseStream; const S: pchar; Count: integer): integer;
begin
  if Assigned(Stream^.write) then
    Result := Stream^.write(Stream, S, Count) else
    Result := 0;
end;

function lse_stream_write(Stream: TStream; const S: PLseString): integer;overload;
begin
  Result := Stream.Write(lse_strec_data(S)^, lse_strec_length(S));
end;

function lse_stream_write(Stream: TStream; const S: string): integer;
begin
  Result := Stream.Write(pointer(S)^, Length(S));
end;

function lse_stream_write(Stream: TStream; const S: pchar; Count: integer): integer;
begin
  Result := Stream.Write(S^, Count);
end;

function lse_stream_writeln(Stream: PLseStream): integer;
begin
  Result := lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_writeln(Stream: PLseStream; const S: string): integer;
begin
  Result := lse_stream_write(Stream, S) +
            lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_writeln(Stream: PLseStream; const S: PLseString): integer;
begin
  Result := lse_stream_write(Stream, S) +
            lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_writeln(Stream: TStream): integer;overload;
begin
  Result := lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_writeln(Stream: TStream; const S: string): integer;
begin
  Result := lse_stream_write(Stream, S) +
            lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_writeln(Stream: TStream; const S: PLseString): integer;overload;
begin
  Result := lse_stream_write(Stream, S) +
            lse_stream_write(Stream, sLineBreak);
end;

function lse_stream_read(Stream: PLseStream; const S: pchar; Count: integer): integer;
begin
  if Assigned(Stream^.read) then
    Result := Stream^.read(Stream, S, Count) else
    Result := 0;
end;

function lse_stream_readln(Stream: PLseStream): PLseString;
begin
  if Assigned(Stream^.readln) then
    Result := Stream^.readln(Stream) else
    Result := nil;
end;

function lse_stream_read_text(Stream: PLseStream; Count: integer): string;
begin
  if Count > 0 then
  begin
    SetLength(Result, Count);
    SetLength(Result, lse_stream_read(Stream, pchar(Result), Count));
  end
  else Result := '';
end;

function lse_stream_read_line(Stream: PLseStream): string;
var
  sr: PLseString;
begin
  Result := '';
  if Assigned(Stream^.readln) then
  begin
    sr := lse_stream_readln(Stream);
    if sr <> nil then
    begin
      lse_strec_inclife(sr);
      Result := lse_strec_string(sr);
      lse_strec_declife(sr);
    end;
  end;
end;

function lse_stream_fill(Stream, Source: PLseStream; Count: integer): integer;
var
  bytes : integer;
  buffer : array[0..1023] of byte;
begin
  Result := 0;
  if Assigned(Stream^.write) then
    while Count > 0 do
    begin
      bytes := lse_stream_read(Source, @buffer[0], Min(Count, sizeof(buffer)));
      if bytes > 0 then
      begin
        lse_stream_write(Stream, @buffer[0], bytes);
        Dec(Count, bytes);
        Inc(Result, bytes);
      end
      else Exit;
    end;
end;

function lse_stream_eof(Stream: PLseStream): boolean;
begin
  if Assigned(Stream^.eof) then
    Result := (Stream^.eof(Stream) <> 0) else
    Result := false;
end;

procedure lse_stream_flush(Stream: PLseStream);
begin
  if Assigned(Stream^.flush) then
    Stream^.flush(Stream);
end;

procedure lse_stream_close(Stream: PLseStream);
begin
  if Assigned(Stream^.close) then
    Stream^.close(Stream);
end;

function lse_stream_closed(Stream: PLseStream): boolean;
begin
  if Assigned(Stream^.closed) then
    Result := (Stream^.closed(Stream) <> 0) else
    Result := false;
end;

function lse_stream_seek(Stream: PLseStream; Offset: int64; Origin: integer): int64;
begin
  if Assigned(Stream^.seek) then
    Result := Stream^.seek(Stream, Offset, Origin) else
    Result := 0;
end;

function lse_stream_pos(Stream: PLseStream): int64;
begin
  if Assigned(Stream^.seek) then
    Result := lse_stream_seek(Stream, 0, SSF_CURRENT) else
    Result := 0;
end;

function lse_stream_size(Stream: PLseStream): int64;
begin
  if Assigned(Stream^.get_size) then
    Result := Stream^.get_size(Stream) else
    Result := 0;
end;

function lse_stream_resize(Stream: PLseStream; NewSize: int64): int64;
begin
  if Assigned(Stream^.set_size) then
    Stream^.set_size(Stream, NewSize);
  Result := lse_stream_size(Stream);
end;

{ TLseEngine.FStdin }

function stdin_read(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  engine: TLseEngine;
begin
  try
    Result := 0;
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      if Assigned(engine.FOnRead) then
        engine.FOnRead(engine, Buffer, Count) else
        Count := FileRead(lse_stdin, Buffer^, Count);
      Result := Count;
    end;
  except
    lse_kernel_log(lse_exception_str);
    Result := 0;
  end;
end;

function stdin_readln(S: PLseStream): PLseString;cdecl;
var
  engine: TLseEngine;
  line: string;
begin
  try
    line := '';
    engine := TLseEngine(S^.data);
    if engine <> nil then
      if Assigned(engine.FOnReadln) then
        engine.FOnReadln(engine, line) else
        Readln(line);
    if line <> '' then
      Result := lse_strec_alloc(line) else
      Result := nil;
  except
    lse_kernel_log(lse_exception_str);
    Result := nil;
  end;
end;

function stdin_eof(S: PLseStream): integer;cdecl;
begin
  try
    if S^.data <> nil then
      if System.Eof(Input) then
        S^.data := nil;
    Result := Ord(S^.data = nil);
  except
    lse_kernel_log(lse_exception_str);
    Result := 1;
  end;
end;

procedure stdin_close(S: PLseStream);cdecl;
var
  engine: TLseEngine;
begin
  try
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      S^.data := nil;
      System.Close(Input);
    end;
  except
    lse_kernel_log(lse_exception_str);
  end;
end;

{ TLseEngine.FStdout }

function stdout_write(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  engine: TLseEngine;
begin
  try
    Result := 0;
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      if Assigned(engine.FOnWrite) then
        engine.FOnWrite(engine, Buffer, Count) else
        FileWrite(lse_stdout, Buffer^, Count);
      Result := Count;
    end;
  except
    lse_kernel_log(lse_exception_str);
    Result := 0;
  end;
end;

function stdout_eof(S: PLseStream): integer;cdecl;
begin
  try
    if S^.data <> nil then
      if System.Eof(Output) then
        S^.data := nil;
    Result := Ord(S^.data = nil);
  except
    lse_kernel_log(lse_exception_str);
    Result := 1;
  end;
end;

procedure stdout_close(S: PLseStream);cdecl;
var
  engine: TLseEngine;
begin
  try
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      S^.data := nil;
      System.Close(Output);
    end;
  except
    lse_kernel_log(lse_exception_str);
  end;
end;

{ TLseEngine.FStderr }

function stderr_write(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  engine: TLseEngine;
begin
  try
    Result := 0;
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      if Assigned(engine.FOnWrite) then
        engine.FOnWrite(engine, Buffer, Count) else
        FileWrite(lse_stderr, Buffer^, Count);
      Result := Count;
    end;
  except
    lse_kernel_log(lse_exception_str);
    Result := 0;
  end;
end;

function stderr_eof(S: PLseStream): integer;cdecl;
begin
  try
    if S^.data <> nil then
      if System.Eof(ErrOutput) then
        S^.data := nil;
    Result := Ord(S^.data = nil);
  except
    lse_kernel_log(lse_exception_str);
    Result := 1;
  end;
end;

procedure stderr_close(S: PLseStream);cdecl;
var
  engine: TLseEngine;
begin
  try
    engine := TLseEngine(S^.data);
    if engine <> nil then
    begin
      S^.data := nil;
      System.Close(ErrOutput);
    end;
  except
    lse_kernel_log(lse_exception_str);
  end;
end;

{======================================================================)
(======== PLseValue - get/set/test value ==============================)
(======================================================================}

function lse_init_value(V: PLseValue): PLseValue;
begin
  V^.value_class := nil;
  V^.VObject := nil;
  Result := V;
end;

procedure lse_clear_value_VOID(V: PLseValue);
begin
  V^.value_class := nil;
  V^.VObject := nil;
end;

procedure lse_clear_value_STRING(V: PLseValue);
begin
  lse_strec_declife(V^.VString);
  V^.value_class := nil;
  V^.VObject := nil;
end;

procedure lse_clear_value_OBJECT(V: PLseValue);
begin
  V^.value_class^.decRefcount(V^.VObject);
  V^.value_class := nil;
  V^.VObject := nil;
end;

function lse_clear_value(V: PLseValue): PLseValue;
const
  PROCS: array[TLseValue] of procedure(V: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_STRING,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_VOID,
    {$IFDEF FPC}@{$ENDIF}lse_clear_value_OBJECT
  );
begin
  Result := V;
  PROCS[lse_vtype(V)](V);
end;

procedure lse_do_nothing(V: PLseValue);
begin
  { do nothing }
end;

procedure lse_inclife_STRING(V: PLseValue);
begin
  lse_strec_inclife(V^.VString);
end;

procedure lse_inclife_OBJECT(V: PLseValue);
begin
  V^.value_class^.incRefcount(V^.VObject);
end;

function lse_inclife(V: PLseValue): PLseValue;
const
  PROCS: array[TLseValue] of procedure(V: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_inclife_STRING,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_inclife_OBJECT
  );
begin
  Result := V;
  PROCS[lse_vtype(V)](V);
end;

procedure lse_declife_STRING(V: PLseValue);
begin
  lse_strec_declife(V^.VString);
end;

procedure lse_declife_OBJECT(V: PLseValue);
begin
  V^.value_class^.decRefcount(V^.VObject);
end;

function lse_declife(V: PLseValue): PLseValue;
const
  PROCS: array[TLseValue] of procedure(V: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_declife_STRING,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_declife_OBJECT
  );
begin
  Result := V;
  PROCS[lse_vtype(V)](V);
end;

procedure lse_set_nil_STRING(V: PLseValue);
begin
  lse_strec_declife(V^.VString);
  V^.VString := nil;
end;

procedure lse_set_nil_INT(V: PLseValue);
begin
  V^.VInteger := 0;
end;

procedure lse_set_nil_FLOAT(V: PLseValue);
begin
  V^.VFloat := 0;
end;

procedure lse_set_nil_MONEY(V: PLseValue);
begin
  V^.VMoney := 0;
end;

procedure lse_set_nil_TIME(V: PLseValue);
begin
  V^.VTime := 0;
end;

procedure lse_set_nil_BOOL(V: PLseValue);
begin
  V^.VBool := false;
end;

procedure lse_set_nil_CHAR(V: PLseValue);
begin
  V^.VChar := #0;
end;

procedure lse_set_nil_OBJECT(V: PLseValue);
begin
  V^.value_class^.decRefcount(V^.VObject);
  V^.VObject := nil;
end;

function lse_set_nil(V: PLseValue): PLseValue;
const
  PROCS: array[TLseValue] of procedure(V: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_STRING,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_INT,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_FLOAT,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_MONEY,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_TIME,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_BOOL,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_CHAR,
    {$IFDEF FPC}@{$ENDIF}lse_do_nothing,
    {$IFDEF FPC}@{$ENDIF}lse_set_nil_OBJECT
  );
begin
  Result := V;
  PROCS[lse_vtype(V)](V);
end;

function lse_is_defv(V: PLseValue): boolean;
var
  crec: PLseClassRec;
begin
  crec := lse_class(V);
  case crec^.vtype of
    LSV_VOID   : Result := true;
    LSV_STRING : Result := (V^.VString = nil);
    LSV_INT    : Result := (V^.VInteger = 0);
    LSV_FLOAT  : Result := IsZero(V^.VFloat);
    LSV_MONEY  : Result := (V^.VMoney = 0);
    LSV_TIME   : Result := IsZero(V^.VTime);
    LSV_BOOL   : Result := not V^.VBool;
    LSV_CHAR   : Result := (V^.VChar = #0);
    LSV_VARIANT: Result := true;
    LSV_OBJECT : Result := (V^.VObject = nil) or (crec^.lysee_class = V^.VObject);
    else         Result := false;
  end;
end;

function lse_is_object(V: PLseValue): boolean;
begin
  Result := (V <> nil) and
            (V^.value_class <> nil) and
            (V^.value_class^.vtype = LSV_OBJECT);
end;

function lse_is_void(V: PLseValue): boolean;
begin
  Result := (V = nil) or
            (V^.value_class = nil) or
            (V^.value_class^.vtype = LSV_VOID);
end;

function lse_vtype(V: PLseValue): TLseValue;
begin
  if V^.value_class <> nil then
    Result := V^.value_class^.vtype else
    Result := LSV_VOID;
end;

function lse_class(V: TLseKernelClass): PLseClassRec;
begin
  Result := lse_entries^.cik_classes^[V];
end;

function lse_class(V: TLseValue): PLseClassRec;
begin
  if V <> LSV_OBJECT then
    Result := lse_entries^.cik_classes^[TLseKernelClass(Ord(V))] else
    Result := nil;
end;

function lse_class(V: PLseValue): PLseClassRec;
begin
  Result := V^.value_class;
  if Result = nil then
    Result := lse_entries^.cik_classes^[kcVoid];
end;

function lse_set_void(V: PLseValue): PLseValue;
begin
  Result := lse_clear_value(V);
end;

function lse_set_int64(V: PLseValue; Value: int64): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_INT);
  V^.VInteger := Value;
end;

function lse_set_integer(V: PLseValue; Value: integer): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_INT);
  V^.VInteger := Value;
end;

function lse_set_float(V: PLseValue; Value: double): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_FLOAT);
  V^.VFloat := Value;
end;

function lse_set_money(V: PLseValue; Value: currency): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_MONEY);
  V^.VMoney := Value;
end;

function lse_set_time(V: PLseValue; Value: TDateTime): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_TIME);
  V^.VTime := Value;
end;

function lse_set_bool(V: PLseValue; Value: boolean): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_BOOL);
  V^.VBool := Value;
end;

function lse_set_char(V: PLseValue; Value: char): PLseValue;
begin
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_CHAR);
  V^.VChar := Value;
end;

function lse_set_string(V: PLseValue; Value: PLseString): PLseValue;
begin
  lse_strec_inclife(Value);
  Result := lse_declife(V);
  V^.value_class := lse_class(LSV_STRING);
  V^.VString := Value;
end;

function lse_set_string(V: PLseValue; const Value: string): PLseValue;
begin
  Result := lse_set_string(V, lse_strec_alloc(Value));
end;

function lse_set_string(V: PLseValue; const Value: pchar): PLseValue;
begin
  Result := lse_set_string(V, lse_strec_alloc(Value, StrLen(Value)));
end;

function lse_set_string(V: PLseValue; const Value: pchar; Length: integer): PLseValue;overload;
begin
  Result := lse_set_string(V, lse_strec_alloc(Value, Length));
end;

function lse_set_object(V: PLseValue; CR: PLseClassRec; Value: pointer): PLseValue;
begin
  CR^.incRefcount(Value);
  Result := lse_declife(V);
  V^.value_class := CR;
  V^.VObject := Value;
end;

function lse_set_class(V: PLseValue; Value: PLseClassRec): PLseValue;
begin
  Result := lse_set_object(V, lse_class(kcClass), Value^.lysee_class);
end;

function lse_set_db(V: PLseValue; Value: PLseDB): PLseValue;
begin
  Result := lse_set_object(V, lse_class(kcDB), Value);
end;

function lse_set_ds(V: PLseValue; Value: PLseDS): PLseValue;
begin
  Result := lse_set_object(V, lse_class(kcDS), Value);
end;

function lse_set_stream(V: PLseValue; Value: PLseStream): PLseValue;
begin
  Result := lse_set_object(V, lse_class(kcStream), Value);
end;

function lse_set_vargen(V: PLseValue; Value: PLseVargen): PLseValue;
begin
  Result := lse_set_object(V, lse_class(kcVargen), Value);
end;

function lse_set_value(V: PLseValue; Value: PLseValue): PLseValue;
begin
  if V <> Value then
  begin
    lse_inclife(Value);
    lse_declife(V);
    V^ := Value^;
  end;
  Result := V;
end;

{======================================================================)
(======== vargen - variant generator ==================================)
(======================================================================}

function lse_vargen_addref(const VG: pointer): integer;cdecl;
begin
  if VG <> nil then
    Result := PLseVargen(VG)^.vg_addref(VG) else
    Result := 0;
end;

function lse_vargen_release(const VG: pointer): integer;cdecl;
begin
  if VG <> nil then
    Result := PLseVargen(VG)^.vg_release(VG) else
    Result := 0;
end;

function lse_vargen_eof(VG: PLseVargen): boolean;
begin
  Result := (VG = nil) or (VG^.vg_has_next(VG) = 0);
end;

function lse_vargen_send(VG: PLseVargen; Value: PLseValue): boolean;
begin
  Result := (VG <> nil) and (VG^.vg_get_next(VG, Value) <> 0);
end;

function lse_vargen_rewind(VG: PLseVargen): boolean;
begin
  Result := (VG <> nil) and (VG^.vg_rewind(VG) <> 0);
end;

function lse_vargen_contains(VG: PLseVargen; Value: PLseValue): boolean;
begin
  Result := Assigned(VG^.vg_contains) and
            (VG^.vg_contains(VG, Value) <> 0);
end;

function empty_vargen_zero(vrec: PLseVargen): integer;cdecl;
begin
  Result := 0;
end;

function empty_vargen_pick(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := 0;
end;

function empty_vargen_life(vrec: PLseVargen): integer;cdecl;
begin
  Result := 1;
end;

const
  empty_vargen: RLseVargen = (
    vg_data    : nil;
    vg_engine  : nil;
    vg_rewind  : {$IFDEF FPC}@{$ENDIF}empty_vargen_zero;
    vg_has_next: {$IFDEF FPC}@{$ENDIF}empty_vargen_zero;
    vg_get_next: {$IFDEF FPC}@{$ENDIF}empty_vargen_pick;
    vg_addref  : {$IFDEF FPC}@{$ENDIF}empty_vargen_life;
    vg_release : {$IFDEF FPC}@{$ENDIF}empty_vargen_life;
    vg_contains: nil;
  );
  
function lse_vargen_none: PLseVargen;
begin
  Result := @empty_vargen;
end;

function lse_vargen_ensure(VG: PLseVargen): PLseVargen;
begin
  if VG = nil then
    Result := lse_vargen_none else
    Result := VG;
end;

function lse_vargen_this(Param: PLseParam): PLseVargen;
begin
  Result := lse_vargen_ensure(PLseVargen(Param^.param[0]^.VObject));
end;

{======================================================================)
(======== components ==================================================)
(======================================================================}

procedure lse_setup_components(const Components: array of TLseComponent);
var
  X: integer;
  M: TLseComponent;
begin
  for X := 0 to Length(Components) - 1 do
  begin
    M := Components[X]; 
    if M <> nil then M.Setup;
  end;
end;

procedure Register;
begin
  RegisterComponents('Lysee', [TLseEngine, TLseModule]);
  RegisterNoIcon([TLseFunc, TLseClass, TLseMethod]);
end;

{======================================================================)
(======== stdio =======================================================)
(======================================================================}

function lse_stdin: integer;
begin
  {$IFDEF FPC}
  Result := StdInputHandle;
  {$ELSE}
  Result := GetStdhandle(STD_INPUT_HANDLE);
  {$ENDIF}
end;

function lse_stdout: integer;
begin
  {$IFDEF FPC}
  Result := StdOutputHandle;
  {$ELSE}
  Result := GetStdhandle(STD_OUTPUT_HANDLE);
  {$ENDIF}
end;

function lse_stderr: integer;
begin
  {$IFDEF FPC}
  Result := StdErrorHandle;
  {$ELSE}
  Result := GetStdhandle(STD_ERROR_HANDLE);
  {$ENDIF}
end;

{======================================================================)
(======== misc ========================================================)
(======================================================================}

procedure lse_error(const error: string);
begin
  raise TLseException.Create(error);
end;

procedure lse_error(const error: string; const Args: array of const);
begin
  lse_error(Format(error, Args));
end;

function lse_exception_str: string;
var
  EO: TObject;
begin
  EO := ExceptObject;
  if EO = nil then Result := '' else
  if EO is Exception then
  begin
    if EO is EAbort then
      Result := 'abort' else
      Result := Exception(EO).Message;
  end
  else Result := EO.ClassName + ' error';
end;

procedure lse_call_gate(const Call: TLseFuncInvoke; const Param: PLseParam);cdecl;
var
  invoker: TLseInvoke;
begin
  try
    invoker := TLseInvoke.Create(Param);
    try
      try
        Call(invoker);
      except
        invoker.ReturnError('', 0, lse_exception_str);
      end;
    finally
      invoker.Free;
    end;
  except
    { safe call }
  end;
end;

function lse_veryPD(const Path: string): string;
var
  base, next, slen: integer;
  ispd: boolean;
  curr: char;
begin
  ispd := false;
  base := 0;
  slen := Length(Path);
  SetLength(Result, slen);
  for next := 1 to slen do
  begin
    curr := Path[next];
    if not (curr in ['\', '/']) then
    begin
      ispd := false;
      Inc(base);
      Result[base] := curr;
    end
    else
    if not ispd then
    begin
      ispd := true;
      Inc(base);
      Result[base] := LSE_PATH_DELIMITER;
    end;
  end;
  SetLength(Result, base);
end;

function lse_veryUD(const URL: string): string;
var
  index: integer;
begin
  Result := URL;
  for index := 1 to Length(URL) do
    if URL[index] = '\' then
      Result[index] := '/';
end;

function lse_expand_fname(const fname: string): string;
begin
  Result := Trim(fname);
  if Result <> '' then
    Result := ExpandFileName(lse_veryPD(Result));
end;

function lse_complete_fname(const fname: string): string;
const
  EXTS: array[0..3] of string = ('.ls', '.lsp', '.htm', '.html');
var
  temp: string;
  index: integer;
begin
  Result := lse_expand_fname(fname);
  if (Result <> '') and not FileExists(Result) then
  begin
    if ExtractFileExt(Result) = '' then
    begin
      temp := Result;
      for index := 0 to Length(EXTS) - 1 do
      begin
        Result := temp + EXTS[index];
        if FileExists(Result) then Exit;
      end;
    end;
    Result := '';
  end;
end;

function lse_is_lsp_file(const fname: string): boolean;
begin
  Result := (LowerCase(Trim(ExtractFileExt(fname))) <> '.ls');
end;

procedure lse_remove_item(List: TList; Item: pointer);
begin
  if List <> nil then
    List.Remove(Item);
end;

function lse_vary_index(index, length: int64): int64;
begin
  if index < 0 then
    Result := index + length else
    Result := index;
end;

function lse_vary_range(index, length: int64; var count: int64): int64;
begin
  if index < 0 then
  begin
    Result := index + length;
    if Result < 0 then
    begin
      count := count + Result;
      Result := 0;
    end;
  end
  else Result := index;
  count := Max(0, Min(length - Result, count));
end;

procedure lse_check_index(index, range: int64);
begin
  if (index < 0) or (index >= range) then
    lse_error('index %d is out of range %d', [index, range]);
end;

{ TLseObject }

function TLseObject.DecRefcount: integer;
begin
  Dec(FRefcount);
  Result := FRefcount;
  if Result = 0 then Free;
end;

function TLseObject.IncRefcount: integer;
begin
  Inc(FRefcount);
  Result := FRefcount;
  if Result = 0 then Free;
end;

{ TLseComponent }

function TLseComponent.IsDesignLoading: boolean;
begin
  Result := (csDesigning in ComponentState) or
            (csLoading in ComponentState);
end;

function TLseComponent.IsDestroying: boolean;
begin
  Result := (csDestroying in ComponentState);
end;

{ TLseEngine }

procedure TLseEngine.EventExecuting;
begin
  if Assigned(FOnExecuting) then
    FOnExecuting(Self);
end;

procedure TLseEngine.EventExecuted;
begin
  if Assigned(FOnExecuted) then
    FOnExecuted(Self);
end;

procedure TLseEngine.Clear;
begin
  lse_entries^.cik_clear(FEngineRec.lysee_engine);
end;

function TLseEngine.CompileCode(const Code: string; IsLspCode: boolean): boolean;
begin
  Result := (lse_entries^.cik_compile(FEngineRec.lysee_engine, pchar(Code), Ord(IsLspCode)) <> 0);
end;

constructor TLseEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoEngine;
  FEngineRec.lysee_engine := nil;
  Setup;
end;

destructor TLseEngine.Destroy;
begin
  if lse_entries <> nil then
    if FEngineRec.lysee_engine <> nil then
      lse_entries^.cik_destroy(FEngineRec.lysee_engine);
  inherited;
end;

procedure TLseEngine.Setup;
begin
  if (FEngineRec.lysee_engine = nil) and lse_startup then
  begin
    FStdin.data := Self;
    FStdin.read := @stdin_read;
    FStdin.readln := @stdin_readln;
    FStdin.eof := @stdin_eof;
    FStdin.close := @stdin_close;

    FStdout.data := Self;
    FStdout.write := @stdout_write;
    FStdout.eof := @stdout_eof;
    FStdout.close := @stdout_close;

    FStderr.data := Self;
    FStderr.write := @stderr_write;
    FStderr.eof := @stderr_eof;
    FStderr.close := @stderr_close;

    FEngineRec.er_engine := Self;
    FEngineRec.er_executing := @lse_executing;
    FEngineRec.er_executed := @lse_executed;
    FEngineRec.er_password := @lse_password;
    FEngineRec.er_stdin := @FStdin;
    FEngineRec.er_stdout := @FStdout;
    FEngineRec.er_stderr := @FStderr;
    lse_entries^.cik_create(@FEngineRec);
  end;
end;

procedure TLseEngine.ExecCommandLine(StartParamIndex: integer);
var
  fname, args: string;
begin
  if StartParamIndex < 1 then
    StartParamIndex := 1;
  fname := lse_complete_fname(ParamStr(StartParamIndex));
  if fname = '' then
  begin
    WriteLine('[ERROR]: File "' + ParamStr(StartParamIndex) + '" not exists!');
    Exit;
  end;
  args := SetupArgs(fname, StartParamIndex + 1);
  if LowerCase(ExtractFileExt(fname)) <> '.ls' then
    ExecLspFile(fname, args, false) else
    ExecLsFile(fname, args, false);
end;

function TLseEngine.ExecLsFile(const FileName, Arguments: string; DoClear: boolean): boolean;
begin
  if DoClear then Clear;
  SetArgs(Arguments);
  Result := ExecuteFile(FileName, false);
  if not Result then
    WriteLine(Error);
end;

function TLseEngine.ExecLspFile(const FileName, Arguments: string; DoClear: boolean): boolean;
begin
  if DoClear then Clear;
  SetArgs(Arguments);
  Result := ExecuteFile(FileName, true);
  if not Result then
    WriteLine(Error);
end;

function TLseEngine.ExecuteCode(const Code: string; IsLspCode: boolean): boolean;
begin
  Result := (lse_entries^.cik_execute(
    FEngineRec.lysee_engine,
    pchar(Code), Ord(IsLspCode)) <> 0);
end;

function TLseEngine.GetArgs: string;
var
  sr: PLseString;
begin
  sr := lse_entries^.cik_get_args(FEngineRec.lysee_engine);
  Result := lse_strec_string(sr);
  lse_strec_declife(sr);
end;

function TLseEngine.GetMainFile: string;
begin
  Result := lse_entries^.cik_get_main_file(FEngineRec.lysee_engine);
end;

function TLseEngine.GetSearchPath: string;
begin
  Result := lse_entries^.cik_get_search_path(FEngineRec.lysee_engine);
end;

function TLseEngine.GetEngineRec: PLseEngine;
begin
  Result := @FEngineRec;
end;

function TLseEngine.GetTempPath: string;
begin
  Result := lse_entries^.cik_tmpath();
end;

function TLseEngine.Errno: integer;
begin
  Result := lse_entries^.cik_errno(FEngineRec.lysee_engine);
end;

function TLseEngine.ErrorCol: integer;
begin
  Result := lse_entries^.cik_error_col(FEngineRec.lysee_engine) + 1;
end;

function TLseEngine.ErrorIncludedFile: string;
begin
  Result := lse_entries^.cik_error_file(FEngineRec.lysee_engine);
end;

function TLseEngine.ErrorMsg: string;
begin
  Result := lse_entries^.cik_error_msg(FEngineRec.lysee_engine);
end;

function TLseEngine.ErrorName: string;
begin
  Result := lse_entries^.cik_error_name(FEngineRec.lysee_engine);
end;

function TLseEngine.ErrorModule: string;
begin
  Result := lse_entries^.cik_error_module(FEngineRec.lysee_engine);
end;

function TLseEngine.ErrorRow: integer;
begin
  Result := lse_entries^.cik_error_row(FEngineRec.lysee_engine) + 1;
end;

function TLseEngine.Error: string;
begin
  if Errno <> 0  then
  begin
    Result := ErrorIncludedFile;
    if Result <> '' then
      Result := ' file=' + Result;
    Result := Format('[%s]: (module=%s%s row=%d col=%d errno=%d) %s',
      [ErrorName, ErrorModule, Result, ErrorRow, ErrorCol, Errno, ErrorMsg]);
  end
  else Result := '';
end;

function TLseEngine.Ready: boolean;
begin
  Result := (lse_entries^.cik_ready(FEngineRec.lysee_engine) <> 0);
end;

function TLseEngine.Running: boolean;
begin
  Result := (lse_entries^.cik_running(FEngineRec.lysee_engine) <> 0);
end;

procedure TLseEngine.SetArgs(const Value: string);
begin
  lse_entries^.cik_set_args(FEngineRec.lysee_engine, pchar(Value));
end;

procedure TLseEngine.SetMainFile(const Value: string);
begin
  lse_entries^.cik_set_main_file(FEngineRec.lysee_engine, pchar(Value));
end;

procedure TLseEngine.SetSearchPath(const Value: string);
begin
  lse_entries^.cik_set_search_path(FEngineRec.lysee_engine, pchar(Value));
end;

function TLseEngine.SetupArgs(const MainFile: string; StartIndex: integer): string;
begin
  Result := ParamStr(0) + sLineBreak + MainFile;
  if StartIndex < 1 then
    StartIndex := 1;
  while StartIndex <= ParamCount do
  begin
    Result := Result + sLineBreak + ParamStr(StartIndex);
    Inc(StartIndex);
  end;
end;

procedure TLseEngine.Terminate;
begin
  lse_entries^.cik_terminate(FEngineRec.lysee_engine);
end;

procedure TLseEngine.WriteData(Data: pointer; Count: integer);
begin
  stdout_write(FEngineRec.er_stdout, Data, Count);
end;

procedure TLseEngine.WriteFile(const FileName: string);
var
  source: TFileStream;
begin
  source := TFileStream.Create(lse_veryPD(FileName), fmShareDenyWrite);
  try
    WriteStream(source);
  finally
    source.Free;
  end;
end;

procedure TLseEngine.WriteLine(const Text: string);
begin
  WriteText(Text);
  WriteLineBreak;
end;

procedure TLseEngine.WriteLineBreak;
begin
  stdout_write(FEngineRec.er_stdout, pchar(sLineBreak), Length(sLineBreak));
end;

procedure TLseEngine.WriteStream(AStream: TStream);
var
  buf: array[0..1023] of char;
  bytes: integer;
begin
  bytes := AStream.Read(buf, sizeof(buf));
  while bytes > 0 do
  begin
    stdout_write(FEngineRec.er_stdout, @buf[0], bytes);
    bytes := AStream.Read(buf, sizeof(buf));
  end;
end;

procedure TLseEngine.WriteText(const Text: string);
begin
  stdout_write(FEngineRec.er_stdout, pchar(Text), Length(Text));
end;

function TLseEngine.CompileFile(const FileName: string; IsLspFile: boolean): boolean;
var
  fname: string;
begin
  SetMainFile(FileName);
  fname := GetMainFile;
  Result := (lse_entries^.cik_fcompile(
    FEngineRec.lysee_engine,
    pchar(fname), Ord(IsLspFile)) <> 0);
end;

function TLseEngine.ExecuteFile(const FileName: string; IsLspFile: boolean): boolean;
var
  fname: string;
begin
  SetMainFile(FileName);
  fname := GetMainFile;
  Result := (lse_entries^.cik_fexecute(
    FEngineRec.lysee_engine,
    pchar(fname), Ord(IsLspFile)) <> 0);
end;

function TLseEngine.Exited: boolean;
begin
  Result := (lse_entries^.cik_exited(FEngineRec.lysee_engine) <> 0);
end;

function TLseEngine.ResultType: string;
begin
  Result := lse_entries^.cik_result_type(FEngineRec.lysee_engine);
end;

function TLseEngine.ResultText: string;
begin
  Result := lse_entries^.cik_result_text(FEngineRec.lysee_engine);
end;

function TLseEngine.Terminated: boolean;
begin
  Result := (lse_entries^.cik_terminated(FEngineRec.lysee_engine) <> 0);
end;

function TLseEngine.Terminating: boolean;
begin
  Result := Terminated and Running;
end;

{ TLseModule }

function TLseModule.AddFunc(const Prototype: string; Proc: pointer): pointer;
begin
  if FModule <> nil then
    Result := lse_module_func_setup(FModule, Prototype, Proc) else
    Result := nil;
end;

function TLseModule.Exists(AComponent: TLseComponent): boolean;
begin
  Result := false;
  if AComponent <> nil then
    if AComponent is TLseFunc then
      Result := FuncExists(AComponent as TLseFunc) else
    if AComponent is TLseClass then
      Result := ClassExists(AComponent as TLseClass) else
    if AComponent is TLseMethod then
      Result := MethodExists(AComponent as TLseMethod);
end;

function TLseModule.FuncExists(AFunc: TLseFunc): boolean;
begin
  if (AFunc <> nil) and (FFuncList <> nil) then
    Result := (FFuncList.IndexOf(AFunc) >= 0) else
    Result := false;
end;

function TLseModule.ClassExists(AClass: TLseClass): boolean;
begin
  if (AClass <> nil) and (FClassList <> nil) then
    Result := (FClassList.IndexOf(AClass) >= 0) else
    Result := false;
end;

function TLseModule.MethodExists(AMethod: TLseMethod): boolean;
var
  index: integer;
begin
  if AMethod <> nil then
    for index := 0 to GetClassCount - 1 do
      if GetClass(index).Exists(AMethod) then
      begin
        Result := true;
        Exit;
      end;
  Result := false;
end;

procedure TLseModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent is TLseComponent then
    begin
      lse_remove_item(FFuncList, AComponent);
      lse_remove_item(FClassList, AComponent);
    end;
end;

function TLseModule.GetClassCount: integer;
begin
  if FClassList <> nil then
    Result := FClassList.Count else
    Result := 0;
end;

function TLseModule.GetFunc(Index: integer): TLseFunc;
begin
  Result := TLseFunc(FFuncList[Index]);
end;

function TLseModule.GetFuncCount: integer;
begin
  if FFuncList <> nil then
    Result := FFuncList.Count else
    Result := 0;
end;

function TLseModule.GetClass(Index: integer): TLseClass;
begin
  Result := TLseClass(FClassList[Index]);
end;

constructor TLseModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoModule;
  FFuncList := TList.Create;
  FClassList := TList.Create;
end;

destructor TLseModule.Destroy;
begin
  FreeAndNil(FFuncList);
  FreeAndNil(FClassList);
  inherited Destroy;
end;

procedure TLseModule.SetupModule;
var
  M: RLseModuleRec;
begin
  if not IsDesignLoading then
    if (FModule = nil) and (Name <> '') then
    begin
      FillChar(M, sizeof(RLseModuleRec), 0);
      M.iw_version := LSE_VERSION;
      M.iw_desc := nil;
      FModule := lse_module_setup(Name, '', @M);
    end;
end;

procedure TLseModule.Setup;
var
  X: integer;
begin
  SetupModule;
  if FModule <> nil then
  begin
    for X := 0 to GetClassCount - 1 do
      GetClass(X).SetupClass;

    for X := 0 to GetClassCount - 1 do
      GetClass(X).Setup;

    for X := 0 to GetFuncCount - 1 do
      GetFunc(X).Setup;
  end;
end;

{ TLseFunc }

procedure TLseFunc.SetModule(const AValue: TLseModule);
begin
  if (FFunc = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FFuncList.Add(Self);
    end;
  end;
end;

procedure TLseFunc.SetPrototype(const AValue: string);
begin
  if FFunc = nil then
    FPrototype := Trim(AValue);
end;

procedure TLseFunc.Leave;
begin
  if FModule <> nil then
  begin
    FModule.Notification(Self, opRemove);
    FModule := nil;
  end;
end;

procedure TLseFunc.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FModule then
      Leave;
end;

constructor TLseFunc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoFunction;
end;

destructor TLseFunc.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLseFunc.Setup;
begin
  if not IsDesignLoading then
    if (FFunc = nil) and (FPrototype <> '') and (FModule <> nil) then
    begin
      FFunc := FModule.AddFunc(FPrototype, @lse_exec_function);
      lse_method_bind(FFunc, Self);
    end;
end;

procedure TLseFunc.Execute(Param: PLseParam);
var
  ivk: TLseInvoke;
begin
  if Assigned(FOnExecute) then
  begin
    ivk := TLseInvoke.Create(Param);
    try
      FOnExecute(ivk);
    finally
      ivk.Free;
    end;
  end;
end;

{ TLseClass }

function TLseClass.AddMethod(const Prototype: string; Proc: pointer): pointer;
begin
  if FClass <> nil then
    Result := lse_method_setup(FClass, Prototype, Proc) else
    Result := nil;
end;

function TLseClass.GetMethodCount: integer;
begin
  if FMethodList <> nil then
    Result := FMethodList.Count else
    Result := 0;
end;

function TLseClass.GetMethod(Index: integer): TLseMethod;
begin
  Result := TLseMethod(FMethodList[Index]);
end;

procedure TLseClass.SetModule(const AValue: TLseModule);
begin
  if (FClass = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FClassList.Add(Self);
    end;
  end;
end;

procedure TLseClass.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FModule then Leave else
    if AComponent is TLseModule then
      lse_remove_item(FMethodList, AComponent);
end;

procedure TLseClass.Leave;
begin
  if FModule <> nil then
  begin
    FModule.Notification(Self, opRemove);
    FModule := nil;
  end;
end;

constructor TLseClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoClass;
  FMethodList := TList.Create;
end;

destructor TLseClass.Destroy;
begin
  Leave;
  FreeAndNil(FMethodList);
  inherited Destroy;
end;

procedure TLseClass.SetupClass;
var
  R: RLseClassRec;
begin
  if not IsDesignLoading then
    if (FClass = nil) and (Name <> '') then
      if (FModule <> nil) and (FModule.FModule <> nil) then
      begin
        FillChar(R, sizeof(RLseClassRec), 0);
        R.vtype := LSV_OBJECT;
        R.name := pchar(Name);
        R.desc := nil;
        R.incRefcount := @lse_incRefcount;
        R.decRefcount := @lse_decRefcount;
        FClass := lse_class_setup(FModule.FModule, @R);
      end;
end;

procedure TLseClass.Setup;
var
  X: integer;
begin
  SetupClass;
  if FClass <> nil then
    for X := 0 to GetMethodCount - 1 do
      GetMethod(X).Setup;
end;

function TLseClass.FindMethod(AMethod: pointer): TLseMethod;
var
  X: integer;
begin
  if AMethod <> nil then
    for X := 0 to GetMethodCount - 1 do
    begin
      Result := GetMethod(X);
      if Result.FMethod = AMethod then Exit;
    end;
  Result := nil;
end;

function TLseClass.Exists(AMethod: TLseMethod): boolean;
begin
  if (AMethod <> nil) and(FMethodList <> nil) then
    Result := (FMethodList.IndexOf(AMethod) >= 0) else
    Result := false;
end;

{ TLseMethod }

procedure TLseMethod.SetClass(const AValue: TLseClass);
begin
  if (FMethod = nil) and (FClass <> AValue) then
  begin
    Leave;
    FClass := AValue;
    if FClass <> nil then
    begin
      FreeNotification(FClass);
      FClass.FMethodList.Add(Self);
    end;
  end;
end;

procedure TLseMethod.SetPrototype(const AValue: string);
begin
  if FMethod = nil then
    FPrototype := Trim(AValue);
end;

procedure TLseMethod.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FClass then
      Leave;
end;

procedure TLseMethod.Leave;
begin
  if FClass <> nil then
  begin
    FClass.Notification(Self, opRemove);
    FClass := nil;
  end;
end;

constructor TLseMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoMethod;
end;

destructor TLseMethod.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLseMethod.Setup;
begin
  if not IsDesignLoading then
    if (FMethod = nil) and (FPrototype <> '') and (FClass <> nil) then
    begin
      FMethod := FClass.AddMethod(FPrototype, @lse_exec_method);
      if lse_method_is_creator(FMethod) then
        lse_method_set_proc(FMethod, @lse_exec_constructor);
      lse_method_bind(FMethod, Self);
    end;
end;

procedure TLseMethod.Execute(Param: PLseParam);
var
  ivk: TLseInvoke;
begin
  if Assigned(FOnExecute) then
  begin
    ivk := TLseInvoke.Create(Param);
    try
      FOnExecute(TLseObject(Param^.param[0]^.VObject), ivk);
    finally
      ivk.Free;
    end;
  end;
end;

function TLseMethod.ClassRec: PLseClassRec;
begin
  if FClass <> nil then
    Result := FClass.FClass else
    Result := nil;
end;

{ TLseInvoke }

function TLseInvoke.FormatStr(const Str: string): string;
var
  sr: PLseString;
begin
  sr := lse_entries^.cik_param_format(FParam, pchar(Str));
  Result := lse_strec_string(sr);
  lse_strec_declife(sr);
end;

function TLseInvoke.EngineRec: PLseEngine;
begin
  Result := lse_entries^.cik_param_engine(FParam);
end;

procedure TLseInvoke.ReturnBool(const Value: boolean);
begin
  lse_set_bool(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnChar(const Value: char);
begin
  lse_set_char(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnError(const ID: string; Errno: integer; const Msg: string);
begin
  lse_entries^.cik_param_error(FParam, pchar(ID), Errno, pchar(Msg));
end;

function TLseInvoke.Read(const Buf: pchar; Count: integer): integer;
begin
  Result := lse_entries^.cik_read(KernelEngine, Buf, Count);
end;

function TLseInvoke.Readln: string;
var
  S: PLseString;
begin
  S := lse_entries^.cik_readln(KernelEngine);
  if S <> nil then
  begin
    lse_strec_inclife(S);
    Result := lse_strec_string(S);
    lse_strec_declife(S);
  end
  else Result := '';
end;

procedure TLseInvoke.ReturnFloat(const Value: double);
begin
  lse_set_float(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnInt64(const Value: int64);
begin
  lse_set_int64(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnMoney(const Value: currency);
begin
  lse_set_money(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnObj(CR: PLseClassRec; Value: pointer);
begin
  lse_set_object(FParam^.result, CR, Value);
end;

procedure TLseInvoke.ReturnObject(KernelClass, Value: pointer);
begin
  lse_set_object(FParam^.result, lse_class_rec(KernelClass), Value);
end;

procedure TLseInvoke.ReturnStr(const Value: string);
begin
  lse_set_string(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnStream(Value: TStream);
begin
  lse_set_stream(FParam^.result, lse_wrap_stream(Value, true));
end;

procedure TLseInvoke.ReturnStream(Value: PLseStream);
begin
  lse_set_stream(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnTime(const Value: TDateTime);
begin
  lse_set_time(FParam^.result, Value);
end;

procedure TLseInvoke.ReturnInt(const Value: integer);
begin
  lse_set_integer(FParam^.result, Value);
end;

function TLseInvoke.GetThis(var obj): boolean;
var
  this_object: pointer;
begin
  this_object := paramObject(0);
  Result := (this_object <> nil);
  if Result then
    pointer(obj) := this_object else
    returnError('', 0, 'object "this" is not specified!');
end;

constructor TLseInvoke.Create(Param: PLseParam);
begin
  FParam := Param;
end;

function TLseInvoke.KernelEngine: pointer;
begin
  Result := lse_entries^.cik_param_engine(FParam)^.lysee_engine;
end;

function TLseInvoke.ParamBool(Index: integer): boolean;
begin
  Result := FParam^.param[Index]^.VBool;
end;

function TLseInvoke.ParamChar(Index: integer): char;
begin
  Result := FParam^.param[Index]^.VChar;
end;

function TLseInvoke.ParamClass(Index: integer): PLseClassRec;
begin
  Result:= lse_class(FParam^.param[Index]);
end;

function TLseInvoke.ParamCount: integer;
begin
  Result := FParam^.count;
end;

function TLseInvoke.ParamFloat(Index: integer): double;
begin
  Result := FParam^.param[Index]^.VFloat;
end;

function TLseInvoke.ParamFmt(Index: integer): string;
begin
  Result := FormatStr(paramStr(Index));
end;

function TLseInvoke.ParamInt(Index: integer): integer;
begin
  Result := FParam^.param[Index]^.VInteger;
end;

function TLseInvoke.ParamInt64(Index: integer): int64;
begin
  Result := FParam^.param[Index]^.VInteger;
end;

function TLseInvoke.ParamMoney(Index: integer): currency;
begin
  Result := FParam^.param[Index]^.VMoney;
end;

function TLseInvoke.ParamObject(Index: integer): pointer;
begin
  Result := FParam^.param[Index]^.VObject;
end;

function TLseInvoke.ParamStr(Index: integer): string;
begin
  Result := lse_strec_string(FParam^.param[Index]^.VString);
end;

function TLseInvoke.ParamCStr(Index: integer; var Size: integer): pchar;
var
  strec: PLseString;
begin
  strec := FParam^.param[Index]^.VString;
  Result := lse_strec_data(strec);
  Size := lse_strec_length(strec);
end;

function TLseInvoke.ParamStrec(Index: integer): PLseString;
begin
  Result := FParam^.param[Index]^.VString;
end;

function TLseInvoke.ParamStream(Index: integer): PLseStream;
begin
  Result := PLseStream(FParam^.param[Index]^.VObject);
end;

function TLseInvoke.ParamTime(Index: integer): TDateTime;
begin
  Result := FParam^.param[Index]^.VTime;
end;

procedure TLseInvoke.Print(const Str: string);
begin
  lse_entries^.cik_write(KernelEngine, pchar(Str), Length(Str));
end;

initialization
{$IFDEF LYSEE_LAZ}
{$I lseu.lrs}
{$ENDIF}

end.
