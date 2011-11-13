{==============================================================================}
{        UNIT: lseu                                                            }
{ DESCRIPTION: lysee script engine unit                                        }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/11/07                                                      }
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
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF};

const

  { LSE: Lysee Script Engine }

  LSE_ID             = 'lysee';
  LSE_OBJEXT         = '.lo';
  LSE_KERNEL         = LSE_ID + LSE_OBJEXT;
  LSE_CONFILE        = 'lysee.config';
  LSE_MIMEFILE       = 'lysee.mime';
  LSE_PATH_DELIMITER = {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};
  LSE_SEARCH_PATH    = '${kndir}/modules';
  LSE_TEMP_PATH      = {$IFDEF WINDOWS}'${kndir}\temp'{$ELSE}'/tmp'{$ENDIF};
  LSE_COPYRIGHT      = 'Copyright (c) 2003-2011 Li Yun Jie';
  LSE_VERSION        = '2.1.3';
  LSE_BIRTHDAY       = 20030228;
  LSE_BUILDDAY       = 20111107;
  LSE_MAX_PARAMS     = 12;
  LSE_MAX_CODES      = MaxInt div 2;

  { LCS: Lysee Char Set }

  LCS_DIGIT          = ['0'..'9'];
  LCS_ALPHA          = ['A'..'Z', 'a'..'z'];
  LCS_DISTANCE       = Ord('a') - Ord('A');
  LCS_ALNUM          = LCS_ALPHA + LCS_DIGIT;
  LCS_ID             = LCS_ALNUM + ['_'];
  LCS_HEAD           = LCS_ALPHA + ['_'];
  LCS_ENV            = LCS_ID  + ['.', '-', ':', '/', '\'];
  LCS_PUNCT          = ['!'..'~'] - LCS_ALNUM;
  LCS_CNTRL          = [#$00..#$1F, #$7F];
  LCS_QUOTE          = ['"', ''''];
  LCS_SPACE          = [#$09, #$0A, #$0C, #$0D, #$20];
  LCS_HEX            = ['A'..'F', 'a'..'f'] + LCS_DIGIT;

  { LSV: Lysee Script Value }

  LSV_VOID           = 0;  {<--void}
  LSV_STRING         = 1;  {<--string}
  LSV_INT            = 2;  {<--int}
  LSV_FLOAT          = 3;  {<--float}
  LSV_VARIANT        = 4;  {<--variant}
  LSV_OBJECT         = 5;  {<--object}

  { SSF: Seek Stream From }

  SSF_BEGINNING      = 0;
  SSF_CURRENT        = 1;
  SSF_END            = 2;

  { SCT: Simple Code Test }

  SCT_ERROR          = $01;  {<--has error}
  SCT_OK             = $02;  {<--OK! might be right}
  SCT_RBLOCK         = $04;  {<--last symbol is syRBlock}
  SCT_UNFINISHED     = $08;  {<--not finished}

type

  { class forward }

  TLseObject = class;
  TLseEngine = class;
  TLseInvoke = class;

  { record forward }

  PLseString = ^RLseString;
  PLseValue  = ^RLseValue;
  PLseVarb   = ^RLseVarb;
  PLseParam  = ^RLseParam;
  PLseFunc   = ^RLseFunc;
  PLseVargen = ^RLseVargen;
  PLseType   = ^RLseType;
  PLseModule = ^RLseModule;
  PLseStream = ^RLseStream;
  PLseEngine = ^RLseEngine;
  PLseEntry  = ^RLseEntry;

  TLseCompareResult = (crEqual, crLess, crMore, crDiff);
  TLseCompareResults = set of TLseCompareResult;

  TLseCharSet = set of char;

{======================================================================)
(======== string interface ============================================)
(======================================================================}

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
    vtype: PLseType;
    case TLseValue of
      LSV_INT   : (VInteger: int64);
      LSV_FLOAT : (VFloat  : double);
      LSV_OBJECT: (VObject : pointer);
  end;

  {======================================================================)
  (======== variable interface ==========================================)
  (======================================================================}

  RLseVarb = packed record
    v_name: string;
    v_type: PLseType;
  end;

{======================================================================)
(======== param interface =============================================)
(======================================================================}

  RLseParam = packed record
    p_param : array[0..LSE_MAX_PARAMS - 1] of PLseValue;
    p_count : integer;   // parametre count
    p_result: PLseValue; // result value
    p_func  : pointer;   // --> lse_kernel.KLiFunc
    p_runner: pointer;   // --> lse_kernel.KLiRunner
    p_exprec: pointer;   // --> lse_kernel.PLiExprRec
  end;

{======================================================================)
(======== function interface ==========================================)
(======================================================================}

  TLseFuncCall = procedure(const Param: PLseParam);cdecl;
  TLseFuncInvoke = procedure(const Invoker: TLseInvoke);cdecl;

  RLseFunc = packed record
    fr_prot: pchar;   // prototype
    fr_addr: pointer; // TLseFuncCall | TLseFuncInvoke
    fr_desc: pchar;   // description
  end;

  ALseFuncList = array[0..1023] of RLseFunc;
  PLseFuncList = ^ALseFuncList;

  RLseFuncListRec = packed record
    fl_count: integer;
    fl_entry: PLseFuncList;
  end;
  PLseFuncListRec = ^RLseFuncListRec;

{======================================================================)
(======== vargen interface ============================================)
(======================================================================}

  RLseVargen = packed record
    vg_data    : pointer;
    vg_type    : PLseType;
    vg_object  : pointer;
    vg_addref  : function(vgrec: PLseVargen): integer;cdecl;
    vg_release : function(vgrec: PLseVargen): integer;cdecl;
    vg_has_next: function(vgrec: PLseVargen): integer;cdecl;
    vg_generate: function(vgrec: PLseVargen; Value: PLseValue): integer;cdecl;
    vg_rewind  : procedure(vgrec: PLseVargen);cdecl;
    vg_contains: function(vgrec: PLseVargen; Value: PLseValue): integer;cdecl;
  end;

{======================================================================)
(======== type interface ==============================================)
(======================================================================}

  TLseAddref = function(const obj: pointer): integer;cdecl;
  TLseRelease = TLseAddref;
  TLseToVargen = function(obj: pointer): PLseVargen;cdecl;
  TLseToString = function(obj: pointer): PLseString;cdecl;
  TLseStringTo = function(str: PLseString): pointer;cdecl;
  TLseAddItem = function(obj: pointer; value: PLseValue): integer;cdecl;
  TLseLength = function(obj: pointer): integer;cdecl;
  TLseGetItem = function(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
  TLseGetProp = function(obj: pointer; prop: PLseString; value: PLseValue): integer;cdecl;

  RLseType = packed record
    cr_type    : TLseValue;       {<--value type: LSV_XXXX}
    cr_name    : pchar;           {<--type name}
    cr_desc    : pchar;           {<--description}
    cr_addref  : TLseAddref;      {<--increase reference count}
    cr_release : TLseRelease;     {<--decrease reference count}
    cr_vargen  : TLseToVargen;    {<--convert object to vargen}
    cr_otos    : TLseToString;    {<--convert object to string}
    cr_stoo    : TLseStringTo;    {<--convert string to object}
    cr_add     : TLseAddItem;     {<--add item <<}
    cr_getiv   : TLseGetItem;     {<--get item by index}
    cr_setiv   : TLseGetItem;     {<--set item by index}
    cr_getpv   : TLseGetProp;     {<--get property}
    cr_setpv   : TLseGetProp;     {<--set property}
    cr_length  : TLseLength;      {<--get item count}
    cr_module  : pointer;         {<--lse_kernel.KLiModule}
  end;

  ALseTypeList = array[0..1023] of RLseType;
  PLseTypeList = ^ALseTypeList;

  RLseTypeListRec = packed record
    cl_count: integer;
    cl_entry: PLseTypeList;
  end;
  PLseTypeListRec = ^RLseTypeListRec;

{======================================================================)
(======== kernel types ================================================)
(======================================================================}

  TLseKernelType = (kcVoid, kcString, kcInteger, kcFloat, kcVariant,
                    kcType, kcModule, kcFunc, kcError, kcStream,
                    kcVarlist, kcVarsnap, kcHashed, kcVargen);

  RLseKernelTypeList = array[TLseKernelType] of PLseType;
  PLseKernelTypeList = ^RLseKernelTypeList;
  
{======================================================================)
(======== module interface ============================================)
(======================================================================}

  TLseOnInvoke = procedure(const Call: TLseFuncInvoke;
                           const Param: PLseParam);cdecl;

  RLseModule = packed record
    iw_version: pchar;           {<--production version}
    iw_desc   : pchar;           {<--description}
    iw_types  : RLseTypeListRec; {<--type list}
    iw_funcs  : RLseFuncListRec; {<--function list}
    iw_invoke : TLseOnInvoke;    {<--invoke function notify}
  end;

  TLseQueryEntry = function(const ID: pchar): pointer;cdecl;
  {
    DLL module initializing procedure "InitExchange"
  }
  TLseInitExchange = procedure(const MR: PLseModule;
                               const ER: PLseEntry);cdecl;

{======================================================================)
(======== stream interface ============================================)
(======================================================================}

  RLseStream = packed record
    s_data: pointer;
    s_read: function(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
    s_readln: function(S: PLseStream): PLseString;cdecl;
    s_write: function(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
    s_seek: function(S: PLseStream; Offset: int64; Origin: integer): int64;cdecl;
    s_get_size: function(S: PLseStream): int64;cdecl;
    s_set_size: procedure(S: PLseStream; NewSize: int64);cdecl;
    s_eof: function(S: PLseStream): integer;cdecl;
    s_close: procedure(S: PLseStream);cdecl;
    s_closed: function(S: PLseStream): integer;cdecl;
    s_flush: procedure(S: PLseStream);cdecl;
    s_addref: function(S: PLseStream): integer;cdecl;
    s_release: function(S: PLseStream): integer;cdecl;
  end;

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
(======== engine interface: TLseEngine ================================)
(======================================================================}

  TLseEngineEvent = procedure(Engine: PLseEngine);cdecl;
  TLsePassword = function(Engine: PLseEngine): PLseString;cdecl;

  RLseEngine = packed record
    er_engine   : TLseEngine;      {<--L: TLseEngine instance}
    er_executing: TLseEngineEvent; {<--L: OnExecuting}
    er_executed : TLseEngineEvent; {<--L: OnExecuted}
    er_input    : PLseStream;      {<--L: input stream}
    er_output   : PLseStream;      {<--L: output stream}
    er_data     : pointer;         {<--L: binding data}
    er_kernel   : pointer;         {<--K: kernel engine instance}
  end;

  TLseRead = procedure(Sender: TObject; const Buf: pchar; var Count: integer) of object;
  TLseReadln = procedure(Sender: TObject; var S: string) of object;
  TLseWrite = procedure(Sender: TObject; const Buf: pchar; var Count: integer) of object;
  TLseEof = procedure(Sender: TObject; var Eof: boolean) of object;

  TLseEngine = class(TComponent)
  private
    FEngineRec: RLseEngine;
    FInput: RLseStream;
    FOutput: RLseStream;
    FOnExecuting: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FOnReadln: TLseReadln;
    FOnRead: TLseRead;
    FOnWrite: TLseWrite;
    function GetArgs: string;
    procedure SetArgs(const Value: string);
    function GetMainFile: string;
    procedure SetMainFile(const Value: string);
    function GetSearchPath: string;
    procedure SetSearchPath(const Value: string);
    function GetTempPath: string;
    function GetEngineRec: PLseEngine;
    procedure EventExecuting;
    procedure EventExecuted;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Clear;virtual;
    procedure Terminate;
    function CompileCode(const Code: string): boolean;
    function CompileFile(const FileName: string): boolean;
    function ExecuteCode(const Code: string): boolean;
    function ExecuteFile(const FileName: string): boolean;
    function Ready: boolean;
    function Running: boolean;
    function Terminated: boolean;
    function Terminating: boolean;
    function Exited: boolean;
    function ResultType: string;
    function ResultText: string;
    function SetupArgs(const MainFile: string; StartIndex: integer): string;
    procedure ExecCommandLine(StartIndex: integer = 1);
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
    property OnReadln: TLseReadln read FOnReadln write FOnReadln;
    property OnRead: TLseRead read FOnRead write FOnRead;
    property OnWrite: TLseWrite read FOnWrite write FOnWrite;
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
    procedure ReturnStr(const Value: string);
    procedure ReturnChar(const Value: char);
    procedure ReturnBool(const Value: boolean);
    procedure ReturnObject(T: PLseType; Value: pointer);
    procedure ReturnObj(CR: PLseType; Value: pointer);
    procedure ReturnStream(Value: TStream);overload;
    procedure ReturnStream(Value: PLseStream);overload;
    procedure ReturnError(const ID: string; Errno: integer; const Msg: string);
    procedure Print(const Str: string);
    function Read(const Buf: pchar; Count: integer): integer;
    function Readln: string;
    function FormatStr(const Str: string): string;
    function EngineRec: PLseEngine;
    function GetThis(var obj): boolean;
    function ParamCount: integer;
    function ParamInt(Index: integer): integer;
    function ParamInt64(Index: integer): int64;
    function ParamBool(Index: integer): boolean;
    function ParamFloat(Index: integer): double;
    function ParamStr(Index: integer): string;
    function ParamStrec(Index: integer): PLseString;
    function ParamChar(Index: integer): char;
    function ParamFmt(Index: integer): string;
    function ParamObject(Index: integer): pointer;
    function ParamClass(Index: integer): PLseType;
    function ParamStream(Index: integer): PLseStream;
    property Param: PLseParam read FParam write FParam;
  end;

{======================================================================)
(======== TLsePatten ==================================================)
(======================================================================}

  RLseMatch = packed record
    m_str: pchar;
    m_len: integer;
  end;
  PLseMatch = ^RLseMatch;

  TLsePatten = class(TLseObject)
  private
    FPatten: pchar;
    FError: boolean;  // invalid patten?
    FAnchor: boolean; // FPatten^ = '^'?
    FSource: pchar;
    FEos: pchar;      // end of source
    FMatches: array of RLseMatch;
    FCount: integer;
    function Match(Src, Pat: pchar): pchar;
    procedure Check(ok: boolean);
    procedure ResetResult;
  public
    constructor Create(const Patten: pchar);
    destructor Destroy;override;
    function Init(const Patten: pchar): boolean;
    function Exec(S: pchar; L: integer): boolean;
    function ExecStrec(S: PLseString): boolean;
    function Next: boolean;
    function Replace(Src, New: PLseString; Times: int64): PLseString;
    function MatchCount: integer;
    function MatchStr(Index: integer): pchar;
    function MatchPos(Index: integer): integer;
    function MatchLen(Index: integer): integer;
    function SourceLength: integer;
    property Source: pchar read FSource;
  end;

{======================================================================)
(======== TLseNamed ==================================================)
(======================================================================}

  TLseNamed = class(TLseObject)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName write FName;
  end;

{======================================================================)
(======== TLseHashNamed ==============================================)
(======================================================================}

  PLiNameItem = ^RLiNameItem;
  RLiNameItem = packed record
    ni_next: PLiNameItem;
    ni_nobj: TLseNamed;
  end;

  TLseHashNamed = class(TLseObject)
  private
    FBuckets: array of PLiNameItem;
    FSize: cardinal;
    FCount: cardinal;
  protected
    function HashOf(const Key: string): cardinal;
    function NewItem: PLiNameItem;
    procedure FreeItem(Item: PLiNameItem);
    function FindItem(const Key: string): PLiNameItem;
  public
    constructor Create(Size: cardinal);
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(const Key: string);
    procedure Put(AObj: TLseNamed);
    function Get(const Key: string): TLseNamed;
  end;

{======================================================================)
(======== CIK interface ===============================================)
(======================================================================}

  RLseEntry = packed record
    { engine }
    cik_create: function(const EngineRec: PLseEngine): pointer;cdecl;
    cik_destroy: procedure(const Engine: pointer);cdecl;
    cik_compile: function(const Engine: pointer; const code: pchar): integer;cdecl;
    cik_fcompile: function(const Engine: pointer; const fname: pchar): integer;cdecl;
    cik_execute: function(const Engine: pointer; const code: pchar): integer;cdecl;
    cik_fexecute: function(const Engine: pointer; const fname: pchar): integer;cdecl;
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
    cik_register_module: function(const Name: pchar; const MR: PLseModule): pointer;cdecl;
    cik_register_type: function(const CR: PLseType): integer;cdecl;
    cik_register_func: function(const FR: PLseFunc): pointer;cdecl;
    { param }
    cik_param_engine: function(const Param: PLseParam): PLseEngine;cdecl;
    cik_param_format: function(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
    cik_param_error: procedure(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
    cik_param_push: function(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
    cik_param_goon: function(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
    { others }
    cik_production: function: pchar;cdecl;
    cik_version: function: pchar;cdecl;
    cik_copyright: function: pchar;cdecl;
    cik_tmpath: function: pchar;cdecl;
    cik_query: TLseQueryEntry;
    cik_kernel_type: function(Index: TLseKernelType): PLseType;cdecl;
    cik_simple_test: function(const Code: pchar): integer;cdecl;
    cik_startup: function: integer;cdecl;
    cik_cleanup: procedure;cdecl;
    cik_keywords: function: pchar;cdecl;
    cik_get_kernel_file: function: pchar;cdecl;
    cik_set_kernel_file: procedure(const KernelFile: pchar);cdecl;
    cik_get_program_file: function: pchar;cdecl;
    cik_set_program_file: procedure(const ProgramFile: pchar);cdecl;
    cik_load_config: procedure(const ConfigFile: pchar);cdecl;
  end;

var
  lse_entries: PLseEntry = nil;

const
  lse_vtype_names: array[LSV_VOID..LSV_OBJECT] of string = (
    'void', 'string', 'int', 'float', 'variant', 'object'
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
( lse_register_module  : register new module
( lse_register_type    : register new type in 'sys' module
( lse_register_func    : register new function in 'sys' module
( lse_hash_of          : calculate hash value
(======== MISC ========================================================)
( lse_error            : raise exception
( lse_exception_str    : get execption string
( lse_call_gate        : convert Param to TLseInvoke and call it
( lse_veryPD           : correct path delimiter
( lse_veryUD           : correct URL delimiter
( lse_expand_fname     : expand file name
( lse_complete_fname   : add file extension when necessary
( lse_vary_index       : adjust list index
( lse_vary_range       : adjust list index and range count
( lse_check_index      : check index in list range
(----------------------------------------------------------------------}
procedure lse_prepare(Entry: PLseEntry);
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
function  lse_kernel_type(Index: TLseKernelType): PLseType;
function  lse_simple_test(const Script: string): integer;
function  lse_keywords: string;
function  lse_register_module(const Name, FileName: string; MR: PLseModule): pointer;
function  lse_register_type(const TR: PLseType): boolean;
function  lse_register_func(const FR: PLseFunc): pointer;
function  lse_hash_of(Key: pchar): cardinal;
procedure lse_check(ok: boolean; const msg: string);
procedure lse_error(const error: string);overload;
procedure lse_error(const error: string; const Args: array of const);overload;
function  lse_exception_str: string;
procedure lse_call_gate(const Call: TLseFuncInvoke; const Param: PLseParam);cdecl;
function  lse_veryPD(const Path: string): string;
function  lse_veryUD(const URL: string): string;
function  lse_expand_fname(const FileName: string): string;
function  lse_full_path(const path: string): string;
function  lse_complete_fname(const FileName: string): string;
function  lse_vary_index(index, length: int64): int64;
function  lse_vary_range(index, length: int64; var count: int64): int64;
procedure lse_check_index(index, range: int64);
function  lse_exe_name: string;
function  lse_lib_name: string;
function  lse_in_charset(S: pchar; Len: integer; Chars: TLseCharSet): boolean;overload;
function  lse_in_charset(S: pchar; Chars: TLseCharSet): boolean;overload;
function  lse_is_ident(S: pchar): boolean;
function  lse_is_idhead(C: char): boolean;
procedure lse_zero_ref(aobj: TLseObject);

{======================================================================)
(======== type ========================================================)
(======================================================================}
function  lse_type_init(CR: PLseType; Name, Desc: pchar; VT: TLseValue): PLseType;overload;
function  lse_type_init(CR: PLseType; VT: TLseValue): PLseType;overload;
function  lse_type_prot(T: PLseType; const ID: string): string;overload;
function  lse_type_prot(V: PLseVarb): string;overload;
function  lse_type_otos(T: PLseType; obj: pointer): string;
function  lse_type_stoo(T: PLseType; const S: PLseString): pointer;
function  lse_type_match(T, AType: PLseType): boolean;
function  lse_type_desc(T: PLseType): string;
procedure lse_type_cast(T: PLseType; V: PLseValue);
function  lse_getpv(T: PLseType; aobj: pointer; prop: PLseString; value: PLseValue): boolean;
function  lse_setpv(T: PLseType; aobj: pointer; prop: PLseString; value: PLseValue): boolean;
function  lse_getiv(T: PLseType; aobj: pointer; X: integer; value: PLseValue): boolean;
function  lse_setiv(T: PLseType; aobj: pointer; X: integer; value: PLseValue): boolean;

{======================================================================)
(======== reference ===================================================)
(======================================================================}
function lse_addref_obj(const obj: pointer): integer;cdecl;
function lse_release_obj(const obj: pointer): integer;cdecl;
function lse_long_life(const obj: pointer): integer;cdecl;

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
procedure lse_mem_zero(const memory: pointer; count: integer);
function  lse_mem_comp(B1, B2: pointer; Len: integer; IgnoreCase: boolean): integer;overload;
function  lse_mem_comp(B1: pointer; L1: integer; B2: pointer; L2: integer; IgnoreCase: boolean): integer;overload;
function  lse_mem_same(B1, B2: pointer; Len: integer; IgnoreCase: boolean): boolean;overload;
function  lse_mem_same(B1: pointer; L1: integer; B2: pointer; L2: integer; IgnoreCase: boolean): boolean;overload;

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
function  lse_strec_same(S1, S2: PLseString): boolean;overload;
function  lse_strec_same(S1, S2: PLseString; IgnoreCase: boolean): boolean;overload;
function  lse_strec_lower(S: PLseString): PLseString;
function  lse_strec_upper(S: PLseString): PLseString;
function  lse_strec_comp(S1, S2: PLseString; IgnoreCase: boolean): integer;
function  lse_strec_pos(S, SubStr: PLseString): integer;
function  lse_strec_last_pos(S, SubStr: PLseString): integer;
function  lse_strec_tabs(S: PLseString; var L, M, R: integer): integer;
function  lse_strec_trim(S: PLseString): PLseString;
function  lse_strec_trimL(S: PLseString): PLseString;
function  lse_strec_trimR(S: PLseString): PLseString;
function  lse_strec_trim_all(S: PLseString): PLseString;
function  lse_strec_copy(S: PLseString; index, count: int64): PLseString;
function  lse_strec_left(S: PLseString; count: int64): PLseString;
function  lse_strec_right(S: PLseString; count: int64): PLseString;
function  lse_strec_name(S: PLseString): PLseString;
function  lse_strec_value(S: PLseString): PLseString;
function  lse_strec_set(S: PLseString; X: integer; ch: char): PLseString;
function  lse_strec_get(S: PLseString; X: integer): char;
function  lse_strec_delete(S: PLseString; X, N: int64): PLseString;
function  lse_strec_insert(S, R: PLseString; X: int64): PLseString;
procedure lse_strec_save(S: PLseString; const FileName: string);
function  lse_strec_load(const FileName: string): PLseString;
function  lse_strec_hash(S: PLseString): cardinal;

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
( lse_init_value  : initialize PLseValue record
( lse_clear_value : clear PLseValue record, result type void
( lse_clear_string: clear string
( lse_clear_object: clear object
( lse_is_defv     : test if is default value
( lse_is_obj      : test if is object type
( lse_is_void     : test if is void type
( lse_vtype       : get value type
( lse_type        : get value type record
( lse_set_XXXX    : change value
( lse_casto_string: cast value to string
(----------------------------------------------------------------------}
procedure lse_init_value(V: PLseValue);
procedure lse_clear_value(V: PLseValue);
procedure lse_clear_string(V: PLseValue);
procedure lse_clear_object(V: PLseValue);
procedure lse_addref(V: PLseValue);
procedure lse_release(V: PLseValue);
function  lse_is_defv(V: PLseValue): boolean;
function  lse_is_obj(V: PLseValue): boolean;
function  lse_is_void(V: PLseValue): boolean;
function  lse_vtype(V: PLseValue): TLseValue;
function  lse_type(V: PLseValue): PLseType;
function  lse_length(V: PLseValue): integer;
{ new }
function  lse_new_value: PLseValue;overload;
function  lse_new_value(Value: int64): PLseValue;overload;
function  lse_new_value(Value: double): PLseValue;overload;
function  lse_new_value(const Value: string): PLseValue;overload;
function  lse_new_value(const Value: PLseString): PLseValue;overload;
function  lse_new_value(const Value: PLseValue): PLseValue;overload;
function  lse_new_value(const Value: pointer; AType: PLseType): PLseValue;overload;
procedure lse_free_value(V: PLseValue);
{ get }
function  lse_get_strec(V: PLseValue): PLseString;
function  lse_get_pchar(V: PLseValue): pchar;
function  lse_get_str(V: PLseValue): string;
function  lse_get_int(V: PLseValue): int64;
function  lse_get_float(V: PLseValue): double;
function  lse_get_time(V: PLseValue): TDateTime;
function  lse_get_fname(V: PLseValue): string;
function  lse_get_char(V: PLseValue): char;
function  lse_get_bool(V: PLseValue): boolean;
function  lse_get_obj(V: PLseValue): pointer;overload;
function  lse_get_obj(V: PLseValue; P: PLseType): pointer;overload;
function  lse_get_type(V: PLseValue): PLseType;
function  lse_get_vargen(V: PLseValue): PLseVargen;
function  lse_get_engine(const Param: PLseParam): pointer;
{ set }
procedure lse_set_int(V: PLseValue; Value: int64);
procedure lse_set_float(V: PLseValue; Value: double);
procedure lse_set_bool(V: PLseValue; Value: boolean);
procedure lse_set_char(V: PLseValue; Value: char);
procedure lse_set_string(V: PLseValue; Value: PLseString);overload;
procedure lse_set_string(V: PLseValue; const Value: string);overload;
procedure lse_set_string(V: PLseValue; const Value: pchar);overload;
procedure lse_set_string(V: PLseValue; const Value: pchar; Length: integer);overload;
procedure lse_set_object(V: PLseValue; CR: PLseType; Value: pointer);
procedure lse_set_type(V: PLseValue; Value: PLseType);
procedure lse_set_stream(V: PLseValue; Value: PLseStream);
procedure lse_set_vargen(V: PLseValue; Value: PLseVargen);
procedure lse_set_value(V: PLseValue; Value: PLseValue);
procedure lse_set_nil(V: PLseValue);overload;
procedure lse_set_nil(V: PLseValue; T: PLseType);overload;
{ compare }
function  lse_compare(V1, V2: PLseValue): TLseCompareResult;
function  lse_match(V1, V2: PLseValue; Test: TLseCompareResults): boolean;
procedure lse_equal(V1, V2: PLseValue);      // V1 <=     V1 ==   V2
procedure lse_diff(V1, V2: PLseValue);       // V1 <=     V1 !=   V2
procedure lse_less(V1, V2: PLseValue);       // V1 <=     V1 <    V2
procedure lse_eqless(V1, V2: PLseValue);     // V1 <=     V1 <=   V2
procedure lse_more(V1, V2: PLseValue);       // V1 <=     V1 >    V2
procedure lse_eqmore(V1, V2: PLseValue);     // V1 <=     V1 >=   V2
procedure lse_abseq(V1, V2: PLseValue);      // V1 <=     V1 ===  V2
{ operators }
procedure lse_add(V1, V2: PLseValue);        // V1 <=     V1  +   V2
procedure lse_dec(V1, V2: PLseValue);        // V1 <=     V1  -   V2
procedure lse_mul(V1, V2: PLseValue);        // V1 <=     V1  *   V2
procedure lse_div(V1, V2: PLseValue);        // V1 <=     V1  /   V2
procedure lse_mod(V1, V2: PLseValue);        // V1 <=     V1  %   V2
procedure lse_neg(V1: PLseValue);            // V1 <=   - V1
procedure lse_bit_xor(V1, V2: PLseValue);    // V1 <=     V1  ^   V2
procedure lse_bit_and(V1, V2: PLseValue);    // V1 <=     V1  &   V2
procedure lse_bit_or(V1, V2: PLseValue);     // V1 <=     V1  |   V2
procedure lse_bit_shl(V1, V2: PLseValue);    // V1 <=     V1  <<  V2
procedure lse_bit_shr(V1, V2: PLseValue);    // V1 <=     V1  >>  V2
procedure lse_bit_not(V1: PLseValue);        // V1 <=   ~ V1
procedure lse_logic_and(V1, V2: PLseValue);  // V1 <=     V1  and V2
procedure lse_logic_or(V1, V2: PLseValue);   // V1 <=     V1  or  V2
procedure lse_logic_not(V1: PLseValue);      // V1 <= not V1
procedure lse_is(V1, V2: PLseValue);         // V1 <=     V1 is   V2
procedure lse_as(V1, V2: PLseValue);         // V1 <=     V1 as   V2
procedure lse_fill(V1, V2: PLseValue);       // V1 <=     V1 <<<  V2
procedure lse_like(V1, V2: PLseValue);       // V1 <=     V1 like V2
procedure lse_upto(V1, V2: PLseValue);       // V1 <=     V1 ..   V2

{======================================================================)
(======== vargen - variant generator ==================================)
(======================================================================)
( lse_vargen_addref  : increase reference count
( lse_vargen_release : decrease reference count
( lse_vargen_has_next: has value?
( lse_vargen_generate: get next value
( lse_vargen_rewind  : rewind to head of the vargen
( lse_vargen_none    : an empty variant generator
( lse_vargen_ensure  : ensure a non-zero vargen
( lse_vargen_this    : get a non-zero vargen as this
(----------------------------------------------------------------------}
function lse_vargen_addref(const VG: pointer): integer;cdecl;
function lse_vargen_release(const VG: pointer): integer;cdecl;
function lse_vargen_has_next(VG: PLseVargen): boolean;
function lse_vargen_generate(VG: PLseVargen; Value: PLseValue): boolean;
function lse_vargen_rewind(VG: PLseVargen): boolean;
function lse_vargen_contains(VG: PLseVargen; Value: PLseValue): boolean;
function lse_vargen_none: PLseVargen;
function lse_vargen_ensure(VG: PLseVargen): PLseVargen;
function lse_vargen_this(Param: PLseParam): PLseVargen;
function lse_vargen_upto(begv, endv, step: int64): PLseVargen;
function lse_vargen_downto(begv, endv, step: int64): PLseVargen;

{======================================================================)
(======== stdio =======================================================)
(======================================================================)
( lse_input : standard input handler
( lse_output: standard output handler
( lse_errput: standard error handler
(----------------------------------------------------------------------}
function lse_input: integer;
function lse_output: integer;
function lse_errput: integer;

var
  KT_VOID, KT_STRING, KT_INT, KT_FLOAT, KT_VARIANT, KT_TYPE,
  KT_MODULE, KT_FUNC, KT_ERROR, KT_STREAM, KT_VARLIST, KT_HASHED,
  KT_VARGEN, KT_VARSNAP: PLseType;

implementation

procedure lse_executing(Engine: PLseEngine);cdecl;
begin
  Engine^.er_engine.EventExecuting;
end;

procedure lse_executed(Engine: PLseEngine);cdecl;
begin
  Engine^.er_engine.EventExecuted;
end;

{======================================================================)
(======== type ========================================================)
(======================================================================}

function lse_type_init(CR: PLseType; Name, Desc: pchar; VT: TLseValue): PLseType;
begin
  Result := CR;
  FillChar(CR^, sizeof(RLseType), 0);
  CR^.cr_type := VT;
  CR^.cr_name := Name;
  CR^.cr_desc := Desc;
  CR^.cr_addref := @lse_addref_obj;
  CR^.cr_release := @lse_release_obj;
end;

function lse_type_init(CR: PLseType; VT: TLseValue): PLseType;
begin
  Result := lse_type_init(CR, pchar(lse_vtype_names[VT]), nil, VT);
end;

function lse_type_prot(T: PLseType; const ID: string): string;
begin
  if T <> KT_VARIANT then
    Result := ID + ':' + T^.cr_name else
    Result := ID;
end;

function lse_type_prot(V: PLseVarb): string;
begin
  Result := lse_type_prot(V^.v_type, V^.v_name);
end;

function lse_type_otos(T: PLseType; obj: pointer): string;
var
  sr: PLseString;
begin
  if Assigned(T^.cr_otos) then
  begin
    sr := T^.cr_otos(obj);
    lse_strec_inclife(sr);
    try
      Result := lse_strec_string(sr);
    finally
      lse_strec_declife(sr);
    end;
  end
  else Result := '';
end;

function lse_type_stoo(T: PLseType; const S: PLseString): pointer;
begin
  if Assigned(T^.cr_stoo) then
    Result := T^.cr_stoo(S) else
    Result := nil;
end;

function lse_type_match(T, AType: PLseType): boolean;
begin
  Result := (T = AType) or (T = KT_VARIANT);
end;

function lse_type_desc(T: PLseType): string;
begin
  Result := T^.cr_desc;
end;

procedure lse_type_cast(T: PLseType; V: PLseValue);
var
  K: PLseType;
begin
  if T <> KT_VARIANT then
  begin
    K := lse_type(V);
    if T <> K then
      if T = KT_VOID   then lse_clear_value(V) else
      if T = KT_VARGEN then lse_set_vargen(V, lse_get_vargen(V)) else
      if T = KT_STRING then lse_set_string(V, lse_get_str(V)) else
      if T = KT_INT    then lse_set_int(V, lse_get_int(V)) else
      if T = KT_FLOAT  then lse_set_float(V, lse_get_float(V)) else
      if T = KT_TYPE   then lse_set_type(V, K) else
      if K = KT_STRING then
        lse_set_object(V, T, lse_type_stoo(T, V^.VObject)) else
        lse_set_object(V, T, nil);
  end;
end;

function lse_getpv(T: PLseType; aobj: pointer; prop: PLseString; value: PLseValue): boolean;
begin
  Result := Assigned(T^.cr_getpv) and (T^.cr_getpv(aobj, prop, value) > 0);
end;

function lse_setpv(T: PLseType; aobj: pointer; prop: PLseString; value: PLseValue): boolean;
begin
  Result := Assigned(T^.cr_setpv) and (T^.cr_setpv(aobj, prop, value) > 0);
end;

function lse_getiv(T: PLseType; aobj: pointer; X: integer; value: PLseValue): boolean;
begin
  Result := Assigned(T^.cr_getiv) and (T^.cr_getiv(aobj, X, value) > 0);
end;

function lse_setiv(T: PLseType; aobj: pointer; X: integer; value: PLseValue): boolean;
begin
  Result := Assigned(T^.cr_setiv) and (T^.cr_setiv(aobj, X, value) > 0);
end;

{======================================================================)
(======== reference ===================================================)
(======================================================================}

function lse_addref_obj(const obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := TLseObject(obj).IncRefcount else
    Result := 0;
end;

function lse_release_obj(const obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := TLseObject(obj).DecRefcount else
    Result := 0;
end;

function lse_long_life(const obj: pointer): integer;cdecl;
begin
  Result := Ord(obj <> nil);
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

procedure lse_mem_zero(const memory: pointer; count: integer);
begin
  FillChar(memory^, count, 0);
end;

function lse_mem_comp(B1, B2: pointer; Len: integer; IgnoreCase: boolean): integer;
var
  S, R: pchar;
begin
  S := B1;
  R := B2;
  while Len > 0 do
  begin
    if IgnoreCase and (S^ in LCS_ALPHA) and (R^ in LCS_ALPHA) then
      Result := (byte(S^) or $20) - (byte(R^) or $20) else
      Result := byte(S^) - byte(R^);
    if Result <> 0 then Exit;
    Inc(S);
    Inc(R);
    Dec(Len);
  end;
  Result := 0;
end;

function lse_mem_comp(B1: pointer; L1: integer; B2: pointer; L2: integer; IgnoreCase: boolean): integer;
begin
  Result := lse_mem_comp(B1, B2, Min(L1, L2), IgnoreCase);
  if Result = 0 then
    Result := (L1 - L2);
end;

function lse_mem_same(B1, B2: pointer; Len: integer; IgnoreCase: boolean): boolean;
begin
  if IgnoreCase then
    Result := (lse_mem_comp(B1, B2, Len, true) = 0) else
    Result := CompareMem(B1, B2, Len);
end;

function lse_mem_same(B1: pointer; L1: integer; B2: pointer; L2: integer; IgnoreCase: boolean): boolean;
begin
  Result := (L1 = L2);
  if Result then
    if IgnoreCase then
      Result := (lse_mem_comp(B1, B2, L1, true) = 0) else
      Result := CompareMem(B1, B2, L1);
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
  Result := (lse_strec_comp(S1, S2, false) = 0);
end;

function lse_strec_same(S1, S2: PLseString; IgnoreCase: boolean): boolean;
begin
  Result := (lse_strec_comp(S1, S2, IgnoreCase) = 0);
end;

function lse_strec_lower(S: PLseString): PLseString;

  procedure __lower(buf: pchar; count: integer);
  begin
    if (buf <> nil) and (count > 0) then
    repeat
      if buf^ in ['A'..'Z'] then
        Inc(buf^, LCS_DISTANCE);
      Inc(buf);
      Dec(count);
    until count < 1;
  end;

begin
  Result := lse_strec_dup(S);
  __lower(lse_strec_data(Result), lse_strec_length(Result));
end;

function lse_strec_upper(S: PLseString): PLseString;

  procedure __upper(buf: pchar; count: integer);
  begin
    if (buf <> nil) and (count > 0) then
    repeat
      if buf^ in ['a'..'z'] then
        Dec(buf^, LCS_DISTANCE);
      Inc(buf);
      Dec(count);
    until count < 1;
  end;
  
begin
  Result := lse_strec_dup(S);
  __upper(lse_strec_data(Result), lse_strec_length(Result));
end;

function lse_strec_comp(S1, S2: PLseString; IgnoreCase: boolean): integer;
begin
  if S1 = S2 then Result := 0 else
  Result := lse_mem_comp(lse_strec_data(S1), lse_strec_length(S1),
                         lse_strec_data(S2), lse_strec_length(S2), IgnoreCase);
end;

function lse_strec_pos(S, SubStr: PLseString): integer;
var
  S1, S2, SP: pchar;
  L1, L2: integer;
begin
  Result := -1;
  if (S <> nil) and (SubStr <> nil) then
    if S <> SubStr then
    begin
      S1 := lse_strec_data(S);
      S2 := lse_strec_data(SubStr);
      L1 := lse_strec_length(S);
      L2 := lse_strec_length(SubStr);
      SP := S1;
      while L1 >= L2 do
      begin
        if CompareMem(SP, S2, L2) then
        begin
          Result := (SP - S1);
          Exit;
        end;
        Dec(L1);
        Inc(SP);
      end;
    end
    else Result := 0;
end;

function lse_strec_last_pos(S, SubStr: PLseString): integer;
var
  S1, S2, SP: pchar;
  L2: integer;
begin
  Result := -1;
  if (S <> nil) and (SubStr <> nil) then
    if S <> SubStr then
    begin
      S1 := lse_strec_data(S);
      S2 := lse_strec_data(SubStr);
      L2 := lse_strec_length(SubStr);
      SP := S1 + (lse_strec_length(S) - L2);
      while SP >= S1 do
      begin
        if CompareMem(SP, S2, L2) then
        begin
          Result := (SP - S1);
          Exit;
        end;
        Dec(SP);
      end;
    end
    else Result := 0;
end;

function lse_strec_tabs(S: PLseString; var L, M, R: integer): integer;
var
  base, head, last: pchar;
  size: integer;
begin
  L := 0;
  M := 0;
  R := 0;
  base := lse_strec_data(S);
  if base <> nil then
  begin
    size := lse_strec_length(S);
    head := base;
    last := base + (size - 1);
    while (head <= last) and (head^ in LCS_SPACE) do Inc(head);
    L := head - base;     // base .. head .. last
    base := last;
    while (head <= base) and (base^ in LCS_SPACE) do Dec(base);
    R := last - base;     // head .. base .. last
    Inc(head);
    while head < base do
    begin
      if head^ in LCS_SPACE then Inc(M);
      Inc(head);
    end;
  end;
  Result := L + M + R;
end;

function lse_strec_trim(S: PLseString): PLseString;
var
  L, M, R: integer;
begin
  if (lse_strec_tabs(S, L, M, R) > 0) and ((L + R) > 0) then
  begin
    M := lse_strec_length(S) - (L + R);
    Result := lse_strec_alloc(lse_strec_data(S) + L, M);
  end
  else Result := S;
end;

function lse_strec_trimL(S: PLseString): PLseString;
var
  L, M, R: integer;
begin
  if (lse_strec_tabs(S, L, M, R) > 0) and (L > 0) then
  begin
    M := lse_strec_length(S) - L;
    Result := lse_strec_alloc(lse_strec_data(S) + L, M);
  end
  else Result := S;
end;

function lse_strec_trimR(S: PLseString): PLseString;
var
  L, M, R: integer;
begin
  if (lse_strec_tabs(S, L, M, R) > 0) and (R > 0) then
  begin
    M := lse_strec_length(S) - R;
    Result := lse_strec_alloc(lse_strec_data(S), M);
  end
  else Result := S;
end;

function lse_strec_trim_all(S: PLseString): PLseString;
var
  L, M, R, Z: integer;
  P, N: pchar;
begin
  if lse_strec_tabs(S, L, M, R) > 0 then
  begin
    P := lse_strec_data(S); // source
    Z := lse_strec_length(S); // count
    S := lse_strec_alloc(nil, Z - (L + M + R));
    if S <> nil then
    begin
      N := lse_strec_data(S); // desti
      while Z > 0 do
      begin
        if not (P^ in LCS_SPACE) then
        begin
          N^ := P^;
          Inc(N);
        end;
        Inc(P);
        Dec(Z);
      end;
    end;
  end;
  Result := S;
end;

function lse_strec_copy(S: PLseString; index, count: int64): PLseString;
var
  L: int64;
begin
  Result := nil;
  L := lse_strec_length(S);
  if L > 0 then
  begin
    index := lse_vary_range(index, L, count);
    if count > 0 then
      if count < L then
        Result := lse_strec_alloc(lse_strec_data(S) + index, count) else
        Result := S;
  end;
end;

function lse_strec_left(S: PLseString; count: int64): PLseString;
begin
  Result := lse_strec_copy(S, 0, count);
end;

function lse_strec_right(S: PLseString; count: int64): PLseString;
begin
  Result := lse_strec_copy(S, - count, count);
end;

function lse_strec_name(S: PLseString): PLseString;
var
  base, next: pchar;
begin
  Result := nil;
  base := lse_strec_data(S);
  if (base <> nil) and (base^ <> #0) then
  begin
    next := base;
    while not (next^ in [#0, '=']) do Inc(next);
    if (next^ = '=') and (next <> base) then
      Result := lse_strec_alloc(base, next - base);
  end;
end;

function lse_strec_value(S: PLseString): PLseString;
var
  base, next: pchar;
begin
  Result := nil;
  base := lse_strec_data(S);
  if (base <> nil) and (base^ <> #0) then
  begin
    next := base;
    while not (next^ in [#0, '=']) do Inc(next);
    if (next^ = '=') and (next <> base) then
      Result := lse_strec_alloc(next + 1, lse_strec_length(S) - (next - base) - 1);
  end;
end;

function lse_strec_set(S: PLseString; X: integer; ch: char): PLseString;
var
  L: integer;
begin
  L := lse_strec_length(S);
  X := lse_vary_index(X, L);
  lse_check_index(X, L);
  if lse_strec_data(S)[X] <> ch then
  begin
    S := lse_strec_alloc(lse_strec_data(S), L);
    lse_strec_data(S)[X] := ch;
  end;
  Result := S;
end;

function lse_strec_get(S: PLseString; X: integer): char;
var
  L: integer;
begin
  L := lse_strec_length(S);
  X := lse_vary_index(X, L);
  lse_check_index(X, L);
  Result := lse_strec_data(S)[X];
end;

function lse_strec_delete(S: PLseString; X, N: int64): PLseString;
var
  P, T: pchar;
  L: int64;
begin
  L := lse_strec_length(S);
  if L > 0 then
  begin
    X := lse_vary_range(X, L, N);
    if N > 0 then
    begin
      Dec(L, N);
      if L > 0 then
      begin
        P := lse_strec_data(S);
        S := lse_strec_alloc(nil, L);
        T := lse_strec_data(S);
        if X > 0 then
        begin
          Move(P^, T^, X);
          Dec(L, X);
          Inc(T, X);
          Inc(P, X + N);
        end;
        Move(P^, T^, L);
      end
      else S := nil;
    end;
  end;
  Result := S;
end;

function lse_strec_insert(S, R: PLseString; X: int64): PLseString;
var
  P, T: pchar;
  N, L: int64;
begin
  L := lse_strec_length(S);
  N := lse_strec_length(R);
  if N > 0 then
  begin
    X := lse_vary_index(X, L);
    if (X >= 0) and (X <= L) then
      if L > 0 then
      begin
        P := lse_strec_data(S);
        S := lse_strec_alloc(nil, L + N);
        T := lse_strec_data(S);
        if X > 0 then
        begin
          Move(P^, T^, X);
          Dec(L, X);
          Inc(P, X);
          Inc(T, X);
        end;
        Move(lse_strec_data(R)^, T^, N);
        Inc(T, N);
        Move(P^, T^, L);
      end
      else S := R;
  end;
  Result := S;
end;

procedure lse_strec_save(S: PLseString; const FileName: string);
begin
  with TFileStream.Create(FileName, fmCreate) do
  try
    WriteBuffer(lse_strec_data(S)^, lse_strec_length(S));
  finally
    Free;
  end;
end;

function lse_strec_load(const FileName: string): PLseString;
begin
  with TFileStream.Create(FileName, fmShareDenyWrite) do
  try
    Result := lse_strec_alloc(nil, size);
    if Result <> nil then
      Read(lse_strec_data(Result)^, size);
  finally
    Free;
  end;
end;

function lse_strec_hash(S: PLseString): cardinal;
begin
  Result := lse_hash_of(lse_strec_data(S));
end;

{======================================================================)
(======== kernel - load/manifest lysee kernel =========================)
(======================================================================}

procedure lse_prepare(Entry: PLseEntry);
begin
  lse_entries := Entry;
  if lse_startup then
  begin
    KT_VOID    := lse_kernel_type(kcVoid);
    KT_STRING  := lse_kernel_type(kcString);
    KT_INT     := lse_kernel_type(kcInteger);
    KT_FLOAT   := lse_kernel_type(kcFloat);
    KT_VARIANT := lse_kernel_type(kcVariant);
    KT_TYPE    := lse_kernel_type(kcType);
    KT_MODULE  := lse_kernel_type(kcModule);
    KT_FUNC    := lse_kernel_type(kcFunc);
    KT_ERROR   := lse_kernel_type(kcError);
    KT_STREAM  := lse_kernel_type(kcStream);
    KT_VARLIST := lse_kernel_type(kcVarList);
    KT_HASHED  := lse_kernel_type(kcHashed);
    KT_VARGEN  := lse_kernel_type(kcVarGen);
    KT_VARSNAP := lse_kernel_type(kcVarSnap);
  end;
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
  H: THandle;
  F: string;
  Q: TLseQueryEntry;
begin
  if lse_entries = nil then
  begin
    F := lse_expand_fname(KernelFile);
    if FileExists(F) then
    begin
      H := 0;
      if lse_load_library(F, H) then
      try
        Q := TLseQueryEntry(lse_get_proc(H, 'QueryEntry'));
        if Assigned(Q) then
          lse_prepare(PLseEntry(Q('cik_entries'))) else
          lse_entries := nil;
        if lse_startup then
          lse_set_kernel_file(F) else
          lse_error(FLLK, [F]);
      except
        lse_free_library(H);
        raise;
      end
      else lse_error(FLLK, [F]);
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

function lse_kernel_type(Index: TLseKernelType): PLseType;
begin
  Result := lse_entries^.cik_kernel_type(Index);
end;

function lse_simple_test(const Script: string): integer;
begin
  Result := lse_entries^.cik_simple_test(pchar(Script));
end;

function lse_keywords: string;
begin
  Result := lse_entries^.cik_keywords();
end;

function lse_register_module(const Name, FileName: string; MR: PLseModule): pointer;
var
  mname: string;
begin
  if FileName <> '' then
    mname := Name + '=' + FileName else
    mname := Name;
  Result := lse_entries^.cik_register_module(pchar(mname), MR);
end;

function lse_register_type(const TR: PLseType): boolean;
begin
  Result := (lse_entries^.cik_register_type(TR) <> 0);
end;

function lse_register_func(const FR: PLseFunc): pointer;
begin
  Result := lse_entries^.cik_register_func(FR);
end;

function lse_hash_of(Key: pchar): cardinal;
begin
  Result := 0;
  if (Key <> nil) and (Key^ <> #0) then
  repeat
    Result := ((Result shl 2) or (Result shr (sizeof(Result) * 8 - 2))) xor Ord(Key^);
    Inc(Key);
  until Key^ = #0;
end;

{======================================================================)
(======== misc ========================================================)
(======================================================================}

procedure lse_check(ok: boolean; const msg: string);
begin
  if not ok then lse_error(msg);
end;

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
  invoker := TLseInvoke.Create(Param);
  try
    Call(invoker);
  except
    invoker.returnError('', 0, lse_exception_str);
  end;
  invoker.Free;
end;

function lse_veryPD(const Path: string): string;
var
  X: integer;
  C: char;
begin
  Result := Path;
  for X := 1 to Length(Path) do
  begin
    C := Path[X];
    if C in ['\', '/'] then
      if C <> LSE_PATH_DELIMITER then
        Result[X] := LSE_PATH_DELIMITER;
  end;
end;

function lse_veryUD(const URL: string): string;
var
  X: integer;
begin
  Result := URL;
  for X := 1 to Length(URL) do
    if URL[X] = '\' then
      Result[X] := '/';
end;

function lse_expand_fname(const FileName: string): string;
begin
  if FileName <> '' then
    Result := ExpandFileName(lse_veryPD(FileName)) else
    Result := '';
end;

function lse_full_path(const path: string): string;
begin
  Result := lse_expand_fname(path);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function lse_complete_fname(const FileName: string): string;
begin
  Result := lse_expand_fname(FileName);
  if Result <> '' then
    if not FileExists(Result) then
    begin
      if ExtractFileExt(Result) = '' then
      begin
        Result := Result + '.ls';
        if FileExists(Result) then Exit;
      end;
      Result := '';
    end;
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
      Inc(count, Result);
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

function lse_exe_name: string;
{$IFDEF WINDOWS}
var
  buffer: array[0..MAX_PATH] of char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if IsLibrary then
  begin
    GetModuleFileName(MainInstance, buffer, sizeof(buffer));
    Result := lse_expand_fname(buffer);
    Exit;
  end;
  {$ENDIF}
  Result := lse_expand_fname(ParamStr(0));
end;

function lse_lib_name: string;
{$IFDEF WINDOWS}
var
  buffer: array[0..MAX_PATH] of char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if IsLibrary then
  begin
    GetModuleFileName(HInstance, buffer, sizeof(buffer));
    Result := lse_expand_fname(buffer);
    Exit;
  end;
  {$ENDIF}
  Result := lse_exe_name;
end;

function lse_in_charset(S: pchar; Len: integer; Chars: TLseCharSet): boolean;
begin
  Result := (Len > 0) and (S <> nil) and (S^ in Chars);
  while Result and (Len > 1) do
  begin
    Dec(Len);
    Inc(S);
    Result := S^ in Chars;
  end;
end;

function lse_in_charset(S: pchar; Chars: TLseCharSet): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ <> #0) and (S^ in Chars) then
  begin
    Chars := Chars - [#0];
    repeat Inc(S) until not (S^ in Chars);
    Result := (S^ = #0);
  end;
end;

function lse_is_ident(S: pchar): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ in LCS_HEAD) then
  begin
    Inc(S);
    Result := (S^ = #0) or lse_in_charset(S, LCS_ID);
  end;
end;

function lse_is_idhead(C: char): boolean;
begin
  Result := (C in LCS_HEAD);
end;

procedure lse_zero_ref(aobj: TLseObject);
begin
  aobj.FRefcount := 0;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := TMemoryStream(PLseStreamRec(S^.s_data)^.stream);
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    stream := PLseStreamRec(S^.s_data)^.stream;
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
    wraped := PLseStreamRec(S^.s_data);
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
  Result := Ord(PLseStreamRec(S^.s_data)^.stream = nil);
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
    wraped := PLseStreamRec(S^.s_data);
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
    wraped := PLseStreamRec(S^.s_data);
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
    Result^.s_data     := wraped;
    Result^.s_read     := @LWS_read;
    if Stream is TMemoryStream then
      Result^.s_readln := @LWS_readln_ms else
      Result^.s_readln := @LWS_readln;
    Result^.s_write    := @LWS_write;
    Result^.s_seek     := @LWS_seek;
    Result^.s_get_size := @LWS_get_size;
    Result^.s_set_size := @LWS_set_size;
    Result^.s_eof      := @LWS_eof;
    Result^.s_close    := @LWS_close;
    Result^.s_closed   := @LWS_closed;
    Result^.s_flush    := @LWS_flush;
    Result^.s_addref   := @LWS_addref;
    Result^.s_release  := @LWS_release;
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
  if Assigned(srec^.s_addref) then
    Result := srec^.s_addref(srec) else
    Result := 1;
end;

function lse_stream_release(const obj: pointer): integer;cdecl;
var
  srec: PLseStream;
begin
  srec := PLSeStream(obj);
  if srec = nil then
    Result := 0 else
  if Assigned(srec^.s_release) then
    Result := srec^.s_release(srec) else
    Result := 1;
end;

function lse_stream_write(Stream: PLseStream; const S: PLseString): integer;
begin
  if Assigned(Stream^.s_write) then
    Result := Stream^.s_write(Stream, lse_strec_data(S), lse_strec_length(S)) else
    Result := 0;
end;

function lse_stream_write(Stream: PLseStream; const S: string): integer;
begin
  if Assigned(Stream^.s_write) then
    Result := Stream^.s_write(Stream, pchar(S), Length(S)) else
    Result := 0;
end;

function lse_stream_write(Stream: PLseStream; const S: pchar; Count: integer): integer;
begin
  if Assigned(Stream^.s_write) then
    Result := Stream^.s_write(Stream, S, Count) else
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
  if Assigned(Stream^.s_read) then
    Result := Stream^.s_read(Stream, S, Count) else
    Result := 0;
end;

function lse_stream_readln(Stream: PLseStream): PLseString;
begin
  if Assigned(Stream^.s_readln) then
    Result := Stream^.s_readln(Stream) else
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
  if Assigned(Stream^.s_readln) then
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
  if Assigned(Stream^.s_write) then
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
  if Assigned(Stream^.s_eof) then
    Result := (Stream^.s_eof(Stream) <> 0) else
    Result := false;
end;

procedure lse_stream_flush(Stream: PLseStream);
begin
  if Assigned(Stream^.s_flush) then
    Stream^.s_flush(Stream);
end;

procedure lse_stream_close(Stream: PLseStream);
begin
  if Assigned(Stream^.s_close) then
    Stream^.s_close(Stream);
end;

function lse_stream_closed(Stream: PLseStream): boolean;
begin
  if Assigned(Stream^.s_closed) then
    Result := (Stream^.s_closed(Stream) <> 0) else
    Result := false;
end;

function lse_stream_seek(Stream: PLseStream; Offset: int64; Origin: integer): int64;
begin
  if Assigned(Stream^.s_seek) then
    Result := Stream^.s_seek(Stream, Offset, Origin) else
    Result := 0;
end;

function lse_stream_pos(Stream: PLseStream): int64;
begin
  if Assigned(Stream^.s_seek) then
    Result := lse_stream_seek(Stream, 0, SSF_CURRENT) else
    Result := 0;
end;

function lse_stream_size(Stream: PLseStream): int64;
begin
  if Assigned(Stream^.s_get_size) then
    Result := Stream^.s_get_size(Stream) else
    Result := 0;
end;

function lse_stream_resize(Stream: PLseStream; NewSize: int64): int64;
begin
  if Assigned(Stream^.s_set_size) then
    Stream^.s_set_size(Stream, NewSize);
  Result := lse_stream_size(Stream);
end;

{ TLseEngine.FInput }

function input_read(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  engine: TLseEngine;
begin
  try
    Result := 0;
    engine := TLseEngine(S^.s_data);
    if engine <> nil then
    begin
      if Assigned(engine.FOnRead) then
        engine.FOnRead(engine, Buffer, Count) else
        Count := FileRead(lse_input, Buffer^, Count);
      Result := Count;
    end;
  except
    Result := 0;
  end;
end;

function input_readln(S: PLseStream): PLseString;cdecl;
var
  engine: TLseEngine;
  line: string;
begin
  try
    line := '';
    engine := TLseEngine(S^.s_data);
    if engine <> nil then
      if Assigned(engine.FOnReadln) then
        engine.FOnReadln(engine, line) else
        Readln(line);
    if line <> '' then
      Result := lse_strec_alloc(line) else
      Result := nil;
  except
    Result := nil;
  end;
end;

function input_eof(S: PLseStream): integer;cdecl;
begin
  try
    if S^.s_data <> nil then
      if System.Eof(Input) then
        S^.s_data := nil;
    Result := Ord(S^.s_data = nil);
  except
    Result := 1;
  end;
end;

procedure input_close(S: PLseStream);cdecl;
var
  engine: TLseEngine;
begin
  try
    engine := TLseEngine(S^.s_data);
    if engine <> nil then
    begin
      S^.s_data := nil;
      System.Close(Input);
    end;
  except
    { do nothing }
  end;
end;

{ TLseEngine.FOutput }

function output_write(S: PLseStream; Buffer: pointer; Count: integer): integer;cdecl;
var
  engine: TLseEngine;
begin
  try
    Result := 0;
    engine := TLseEngine(S^.s_data);
    if engine <> nil then
    begin
      if Assigned(engine.FOnWrite) then
        engine.FOnWrite(engine, Buffer, Count) else
        FileWrite(lse_output, Buffer^, Count);
      Result := Count;
    end;
  except
    Result := 0;
  end;
end;

function output_eof(S: PLseStream): integer;cdecl;
begin
  try
    if S^.s_data <> nil then
      if System.Eof(Output) then
        S^.s_data := nil;
    Result := Ord(S^.s_data = nil);
  except
    Result := 1;
  end;
end;

procedure output_close(S: PLseStream);cdecl;
var
  engine: TLseEngine;
begin
  try
    engine := TLseEngine(S^.s_data);
    if engine <> nil then
    begin
      S^.s_data := nil;
      System.Close(Output);
    end;
  except
    { do nothing }
  end;
end;

{======================================================================)
(======== PLseValue - get/set/test value ==============================)
(======================================================================}

procedure lse_init_value(V: PLseValue);
begin
  V^.vtype := nil;
end;

procedure lse_clear_value(V: PLseValue);
begin
  if V^.vtype <> nil then
  begin
    case V^.vtype^.cr_type of
      LSV_STRING: lse_strec_declife(V^.VObject);
      LSV_OBJECT: V^.vtype^.cr_release(V^.VObject);
    end;
    V^.vtype := nil;
  end;
end;

procedure lse_clear_string(V: PLseValue);
begin
  lse_strec_declife(V^.VObject);
  V^.vtype := nil;
end;

procedure lse_clear_object(V: PLseValue);
begin
  V^.vtype^.cr_release(V^.VObject);
  V^.vtype := nil;
end;

procedure lse_addref(V: PLseValue);
begin
  case lse_vtype(V) of
    LSV_STRING: lse_strec_inclife(V^.VObject);
    LSV_OBJECT: V^.vtype^.cr_addref(V^.VObject);
  end;
end;

procedure lse_release(V: PLseValue);
begin
  case lse_vtype(V) of
    LSV_STRING: lse_strec_declife(V^.VObject);
    LSV_OBJECT: V^.vtype^.cr_release(V^.VObject);
  end;
end;

function lse_is_defv(V: PLseValue): boolean;
begin
  Result := (V^.vtype = nil);
  if not Result then
    case V^.vtype^.cr_type of
      LSV_STRING : Result := (V^.VObject = nil);
      LSV_INT    : Result := (V^.VInteger = 0);
      LSV_FLOAT  : Result := IsZero(V^.VFloat);
      LSV_OBJECT : Result := (V^.VObject = nil) or (V^.vtype = V^.VObject);
    end;
end;

function lse_is_obj(V: PLseValue): boolean;
begin
  Result := (V^.vtype <> nil) and
            (V^.vtype^.cr_type = LSV_OBJECT);
end;

function lse_is_void(V: PLseValue): boolean;
begin
  Result := (V^.vtype = nil) or
            (V^.vtype^.cr_type = LSV_VOID);
end;

function lse_vtype(V: PLseValue): TLseValue;
begin
  if V^.vtype <> nil then
    Result := V^.vtype^.cr_type else
    Result := LSV_VOID;
end;

function lse_type(V: PLseValue): PLseType;
begin
  Result := V^.vtype;
  if Result = nil then
    Result := KT_VOID;
end;

function lse_length(V: PLseValue): integer;
begin
  if (V <> nil) and (V^.vtype <> nil) and Assigned(V^.vtype^.cr_length) then
    Result := V^.vtype^.cr_length(V^.VObject) else
    Result := 0;
end;

function lse_new_value: PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := nil;
end;

function lse_new_value(Value: int64): PLseValue;
begin
  Result := lse_new_value();
  Result^.vtype := KT_INT;
  Result^.VInteger := Value; 
end;

function lse_new_value(Value: double): PLseValue;
begin
  Result := lse_new_value();
  Result^.vtype := KT_FLOAT;
  Result^.VFloat := Value;
end;

function lse_new_value(const Value: string): PLseValue;
begin
  Result := lse_new_value();
  Result^.vtype := KT_STRING;
  Result^.VObject := lse_strec_alloc(Value);
  lse_strec_inclife(Result^.VObject);
end;

function lse_new_value(const Value: PLseString): PLseValue;
begin
  Result := lse_new_value();
  Result^.vtype := KT_STRING;
  Result^.VObject := Value;
  lse_strec_inclife(Value);
end;

function lse_new_value(const Value: PLseValue): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Move(Value^, Result^, sizeof(RLseValue));
  lse_addref(Result);
end;

function lse_new_value(const Value: pointer; AType: PLseType): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := AType;
  Result^.VObject := Value;
  AType^.cr_addref(Value);
end;

procedure lse_free_value(V: PLseValue);
begin
  if V <> nil then
  begin
    lse_set_nil(V);
    lse_mem_free(V, sizeof(RLseValue));
  end;
end;

function lse_get_strec(V: PLseValue): PLseString;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := V^.VObject else
    Result := nil;
end;

function lse_get_pchar(V: PLseValue): pchar;
begin
  Result := lse_strec_data(lse_get_strec(V));
end;

function lse_get_str(V: PLseValue): string;
var
  S: PLseString;
begin
  Result := '';
  case lse_vtype(V) of
    LSV_STRING: Result := lse_strec_string(V^.VObject);
    LSV_INT   : Result := IntToStr(V^.VInteger);
    LSV_FLOAT : Result := FloatToStr(V^.VFloat);
    LSV_OBJECT: if Assigned(V^.vtype^.cr_otos) then
                begin
                  S := V^.vtype^.cr_otos(V^.VObject);
                  lse_strec_inclife(S);
                  Result := lse_strec_string(S);
                  lse_strec_declife(S);
                end;
  end;
end;

function lse_get_int(V: PLseValue): int64;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := StrToInt64Def(lse_strec_string(V^.VObject), 0);
    LSV_INT   : Result := V^.VInteger;
    LSV_FLOAT : Result := Trunc(V^.VFloat);
    else Result := 0;
  end;
end;

function lse_get_float(V: PLseValue): double;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := StrToFloatDef(lse_strec_string(V^.VObject), 0);
    LSV_INT   : Result := V^.VInteger;
    LSV_FLOAT : Result := V^.VFloat;
    else Result := 0;
  end;
end;

function lse_get_time(V: PLseValue): TDateTime;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := lse_decode_GMT(lse_get_str(V));
    LSV_FLOAT : Result := V^.VFloat;
    LSV_INT   : Result := UnixToDateTime(V^.VInteger);
    else Result := 0;
  end;
end;

function lse_get_fname(V: PLseValue): string;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := lse_veryPD(lse_strec_string(V^.VObject)) else
    Result := '';
end;

function lse_get_char(V: PLseValue): char;
begin
  case lse_vtype(V) of
    LSV_STRING: if V^.VObject <> nil then
                  Result := lse_strec_data(V^.VObject)^ else
                  Result := #0;
    LSV_INT   : Result := char(V^.VInteger);
    else        Result := #0;
  end;
end;

function lse_get_bool(V: PLseValue): boolean;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := (lse_strec_length(V^.VObject) > 0);
    LSV_INT   : Result := (V^.VInteger <> 0);
    LSV_FLOAT : Result := not IsZero(V^.VFloat);
    LSV_OBJECT: Result := (V^.VObject <> nil);
    else        Result := false;
  end;
end;

function lse_get_obj(V: PLseValue): pointer;
begin
  if lse_vtype(V) = LSV_OBJECT then
    Result := V^.VObject else
    Result := nil;
end;

function lse_get_obj(V: PLseValue; P: PLseType): pointer;
var
  R: PLseType;
begin
  R := lse_type(V);
  if (R^.cr_type = LSV_OBJECT) and (R = P) then
    Result := V^.VObject else
    Result := nil;
end;

function lse_get_type(V: PLseValue): PLseType;
begin
  Result := lse_type(V);
  if Result = KT_TYPE then
    Result := PLseType(V^.VObject);
end;

function lse_get_vargen(V: PLseValue): PLseVargen;
var
  T: PLseType;
begin
  Result := nil;
  T := lse_type(V);
  if T = KT_VARGEN then
    Result := PLseVargen(V^.VObject) else
  if Assigned(T^.cr_vargen) then
    if T = KT_INT then
      Result := T^.cr_vargen(@V^.VInteger) else
    if T^.cr_type in [LSV_STRING, LSV_OBJECT] then
      Result := T^.cr_vargen(V^.VObject) else
    if T = KT_FLOAT then
      Result := T^.cr_vargen(@V^.VFloat);
  Result := lse_vargen_ensure(Result);
end;

function  lse_get_engine(const Param: PLseParam): pointer;
begin
  Result := lse_entries^.cik_param_engine(Param)^.er_kernel;
end;

procedure lse_set_int(V: PLseValue; Value: int64);
begin
  lse_release(V);
  V^.vtype := KT_INT;
  V^.VInteger := Value;
end;

procedure lse_set_float(V: PLseValue; Value: double);
begin
  lse_release(V);
  V^.vtype := KT_FLOAT;
  V^.VFloat := Value;
end;

procedure lse_set_bool(V: PLseValue; Value: boolean);
begin
  lse_release(V);
  V^.vtype := KT_INT;
  V^.VInteger := Ord(Value);
end;

procedure lse_set_char(V: PLseValue; Value: char);
begin
  lse_release(V);
  V^.vtype := KT_INT;
  V^.VInteger := Ord(Value);
end;

procedure lse_set_string(V: PLseValue; Value: PLseString);
begin
  lse_strec_inclife(Value);
  lse_release(V);
  V^.vtype := KT_STRING;
  V^.VObject := Value;
end;

procedure lse_set_string(V: PLseValue; const Value: string);
begin
  lse_set_string(V, lse_strec_alloc(Value));
end;

procedure lse_set_string(V: PLseValue; const Value: pchar);
begin
  lse_set_string(V, lse_strec_alloc(Value, StrLen(Value)));
end;

procedure lse_set_string(V: PLseValue; const Value: pchar; Length: integer);
begin
  lse_set_string(V, lse_strec_alloc(Value, Length));
end;

procedure lse_set_object(V: PLseValue; CR: PLseType; Value: pointer);
begin
  CR^.cr_addref(Value);
  lse_release(V);
  V^.vtype := CR;
  V^.VObject := Value;
end;

procedure lse_set_type(V: PLseValue; Value: PLseType);
begin
  lse_set_object(V, KT_TYPE, Value);
end;

procedure lse_set_stream(V: PLseValue; Value: PLseStream);
begin
  lse_set_object(V, KT_STREAM, Value);
end;

procedure lse_set_vargen(V: PLseValue; Value: PLseVargen);
begin
  lse_set_object(V, KT_VARGEN, Value);
end;

procedure lse_set_value(V: PLseValue; Value: PLseValue);
begin
  lse_addref(Value);
  lse_release(V);
  Move(Value^, V^, sizeof(RLseValue));
end;

procedure lse_set_nil(V: PLseValue);
begin
  case lse_vtype(V) of
    LSV_STRING: begin
                  lse_strec_declife(V^.VObject);
                  V^.VObject := nil;
                end;
    LSV_INT   : V^.VInteger := 0;
    LSV_FLOAT : V^.VFloat := 0;
    LSV_OBJECT: begin
                  V^.vtype^.cr_release(V^.VObject);
                  V^.VObject := nil;
                end;
  end;
end;

procedure lse_set_nil(V: PLseValue; T: PLseType);
begin
  case T^.cr_type of
    LSV_STRING: lse_set_string(V, '');
    LSV_INT   : lse_set_int(V, 0);
    LSV_FLOAT : lse_set_float(V, 0);
    LSV_OBJECT: lse_set_object(V, T, nil);
    else lse_clear_value(V);
  end;
end;

function lse_compare(V1, V2: PLseValue): TLseCompareResult;
var
  T1, T2: PLseType;
  R1, R2: TLseValue;

  function compareS(const A, B: string): TLseCompareResult;
  var
    X: integer;
  begin
    X := AnsiCompareStr(A, B);
    if X = 0 then
      Result := crEqual else if X > 0 then
      Result := crMore else
      Result := crLess;
  end;

  function compareF(A, B: double): TLseCompareResult;
  begin
    A := A - B;
    if IsZero(A) then
      Result := crEqual else if A > 0 then
      Result := crMore  else
      Result := crLess;
  end;

  function compareI(A, B: int64): TLseCompareResult;
  begin
    A := A - B;
    if A = 0 then
      Result := crEqual else if A > 0 then
      Result := crMore else
      Result := crLess;
  end;

begin
  T1 := lse_type(V1);
  R1 := T1^.cr_type;
  
  if R1 = LSV_VOID then
  begin
    if lse_is_defv(V2) then 
      Result := crEqual else
      Result := crDiff;
    Exit;
  end;
  
  T2 := lse_type(V2);
  R2 := T2^.cr_type;
  
  if R2 = LSV_VOID then
  begin
    if lse_is_defv(V1) then 
      Result := crEqual else
      Result := crDiff;
    Exit;
  end;

  if R1 = LSV_STRING then
  begin
    if R2 = LSV_STRING then
      Result := compareS(lse_get_str(V1), lse_get_str(V2)) else
      Result := crDiff;
    Exit;
  end;

  if R1 = LSV_OBJECT then
  begin
    if (T1 = T2) and (V1^.VObject = V2^.VObject) then
      Result := crEqual else
      Result := crDiff;
    Exit;
  end;

  if R2 in [LSV_STRING, LSV_OBJECT] then
  begin
    Result := crDiff;
    Exit;
  end;

  if (R1 = LSV_FLOAT) or (R2 = LSV_FLOAT) then
    Result := compareF(lse_get_float(V1), lse_get_float(V2)) else
    Result := compareI(lse_get_int(V1), lse_get_int(V2));
end;

function lse_match(V1, V2: PLseValue; Test: TLseCompareResults): boolean;
begin
  Result := (lse_compare(V1, V2) in Test);
end;

procedure lse_equal(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crEqual]));
end;

procedure lse_diff(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crLess, crMore, crDiff]));
end;

procedure lse_less(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crLess]));
end;

procedure lse_eqless(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crLess, crEqual]));
end;

procedure lse_more(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crMore]));
end;

procedure lse_eqmore(V1, V2: PLseValue);
begin
  lse_set_bool(V1, lse_match(V1, V2, [crMore, crEqual]));
end;

procedure lse_abseq(V1, V2: PLseValue);
var
  clss: PLseType;
  same: boolean;
begin
  same := false;
  clss := lse_type(V1);
  if clss = lse_type(V2) then
    case clss^.cr_type of
      LSV_VOID   : same := true;
      LSV_STRING : same := lse_strec_same(V1^.VObject, V2^.VObject);
      LSV_INT    : same := (V1^.VInteger = V2^.VInteger);
      LSV_FLOAT  : same := IsZero(V1^.VFloat - V2^.VFloat);
      LSV_VARIANT: same := true;
      LSV_OBJECT : same := (V1^.VObject = V2^.VObject);
    end;
  lse_set_bool(V1, same);
end;

procedure lse_add(V1, V2: PLseValue);

  procedure on_string(R: TLseValue);
  begin
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(V1^.VObject, V2^.VObject)) else
      lse_set_string(V1, lse_strec_cat(V1^.VObject, lse_get_str(V2)));
  end;

  procedure on_int(R: TLseValue);
  begin
    if R = LSV_FLOAT then
      lse_set_float(V1, V1^.VInteger + V2^.VFloat) else
    if R = LSV_INT then
      lse_set_int(V1, V1^.VInteger + V2^.VInteger) else
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(IntToStr(V1^.VInteger), V2^.VObject)) else
      V1^.vtype := nil;
  end;

  procedure on_float(R: TLseValue);
  begin
    if R in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat + lse_get_float(V2)) else
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(FloatToStr(V1^.VFloat), V2^.VObject)) else
      V1^.vtype := nil;
  end;

  procedure on_object(R: TLseValue);
  begin
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(lse_get_str(V1), V2^.VObject)) else
      lse_clear_object(V1);
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING: on_string(lse_vtype(V2));
    LSV_INT   : on_int   (lse_vtype(V2));
    LSV_FLOAT : on_float (lse_vtype(V2));
    LSV_OBJECT: on_object(lse_vtype(V2));
  end;
end;

procedure lse_dec(V1, V2: PLseValue);

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R = LSV_FLOAT then
      lse_set_float(V1, V1^.VInteger - V2^.VFloat) else
    if R = LSV_INT then
      lse_set_int(V1, V1^.VInteger - V2^.VInteger) else
      V1^.vtype := nil;
  end;

  procedure on_float;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat - lse_get_float(V2)) else
      V1^.vtype := nil;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : on_int;
    LSV_FLOAT : on_float;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_mul(V1, V2: PLseValue);

  procedure on_string;
  var
    S, T: PLseString;
    B, F: pchar;
    L, N: integer;
  begin
    if lse_vtype(V2) = LSV_INT then
    begin
      S := V1^.VObject;
      L := lse_strec_length(S);
      N := V2^.VInteger;
      if (L > 0) and (N > 0) then
      begin
        T := lse_strec_alloc(nil, L * N);
        B := lse_strec_data(T);
        F := lse_strec_data(S);
        while N > 0 do
        begin
          Move(F^, B^, L);
          Inc(B, L);
          Dec(N);
        end;
        lse_set_string(V1, T);
      end
      else lse_set_string(V1, '');
    end
    else lse_clear_string(V1);
  end;

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R = LSV_FLOAT then
      lse_set_float(V1, V1^.VInteger * lse_get_float(V2)) else
    if R = LSV_INT then
      lse_set_int(V1, V1^.VInteger * lse_get_int(V2)) else
      V1^.vtype := nil;
  end;

  procedure on_float;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat * lse_get_float(V2)) else
      V1^.vtype := nil;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING: on_string;
    LSV_INT   : on_int;
    LSV_FLOAT : on_float;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_div(V1, V2: PLseValue);

  procedure on_int;
  var
    F: double;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
    begin
      F := lse_get_float(V2);
      if IsZero(F) then
        lse_set_float(V1, 0) else
        lse_set_float(V1, V1^.VInteger / F);
    end
    else V1^.vtype := nil;
  end;

  procedure on_float;
  var
    F: double;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
    begin
      F := lse_get_float(V2);
      if IsZero(F) then
        lse_set_float(V1, 0) else
        lse_set_float(V1, V1^.VFloat / F);
    end
    else V1^.vtype := nil;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : on_int;
    LSV_FLOAT : on_float;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_mod(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) <> LSV_INT then
                  V1^.vtype := nil else
                if V2^.VInteger = 0 then
                  lse_set_int(V1, 0) else
                  lse_set_int(V1, V1^.VInteger mod V2^.VInteger);
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_neg(V1: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : V1^.VInteger := - V1^.VInteger;
    LSV_FLOAT : V1^.VFloat := - V1^.VFloat;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_bit_xor(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) = LSV_INT then
                  lse_set_int(V1, V1^.VInteger xor V2^.VInteger) else
                  V1^.vtype := nil;
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_bit_and(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) = LSV_INT then
                  lse_set_int(V1, V1^.VInteger and V2^.VInteger) else
                  V1^.vtype := nil;
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_bit_or(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) = LSV_INT then
                  lse_set_int(V1, V1^.VInteger or V2^.VInteger) else
                  V1^.vtype := nil;
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_bit_shl(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) = LSV_INT then
                  lse_set_int(V1, V1^.VInteger shl V2^.VInteger) else
                  V1^.vtype := nil;
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: if V1^.VObject <> nil then
                  if Assigned(V1^.vtype^.cr_add) then
                    V1^.vtype^.cr_add(V1^.VObject, V2);
  end;
end;

procedure lse_bit_shr(V1, V2: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING: lse_clear_string(V1);
    LSV_INT   : if lse_vtype(V2) = LSV_INT then
                  lse_set_int(V1, V1^.VInteger shr V2^.VInteger) else
                  V1^.vtype := nil;
    LSV_FLOAT : V1^.vtype := nil;
    LSV_OBJECT: lse_clear_object(V1);
  end;
end;

procedure lse_bit_not(V1: PLseValue);
begin
  if lse_vtype(V1) = LSV_INT then
    lse_set_int(V1, not V1^.VInteger) else
    lse_clear_value(V1);
end;

procedure lse_logic_and(V1, V2: PLseValue);
begin
  if lse_get_bool(V1) and not lse_get_bool(V2) then
    lse_set_value(V1, V2);
end;

procedure lse_logic_or(V1, V2: PLseValue);
begin
  if not lse_get_bool(V1) and lse_get_bool(V2) then
    lse_set_value(V1, V2);
end;

procedure lse_logic_not(V1: PLseValue);
begin
  lse_set_bool(V1, not lse_get_bool(V1));
end;

procedure lse_is(V1, V2: PLseValue);
var
  T2: PLseType;
begin
  T2 := lse_get_type(V2); 
  lse_set_bool(V1, (lse_get_type(V1) = T2) or (T2 = KT_VARIANT));
end;

procedure lse_as(V1, V2: PLseValue);
var
  T: PLseType;
begin
  T := lse_get_type(V2);
  if T <> nil then
    lse_type_cast(T, V1);
end;

procedure lse_fill(V1, V2: PLseValue);
var
  K: PLseType;
  G: PLseVargen;
begin
  K := lse_type(V1);
  if Assigned(K^.cr_add) and (V1^.VObject <> nil) then
  begin
    G := lse_get_vargen(V2);
    lse_vargen_addref(G);
    try
      while lse_vargen_generate(G, V2) do
        K^.cr_add(V1^.VObject, V2);
    finally
      lse_vargen_release(G);
    end;
  end;
end;

procedure lse_like(V1, V2: PLseValue);
var
  P: TLsePatten;
begin
  P := TLsePatten.Create(lse_get_pchar(V2));
  try
    lse_set_bool(V1, P.ExecStrec(lse_get_strec(V1)));
  finally
    P.Free;
  end;
end;

procedure lse_upto(V1, V2: PLseValue);
var
  begv, endv: int64;
begin
  if (lse_vtype(V1) = LSV_INT) and (lse_vtype(V2) = LSV_INT) then
  begin
    begv := V1^.VInteger;
    endv := V2^.VInteger;
    if begv > endv then
      lse_set_vargen(V1, lse_vargen_downto(begv, endv, 1)) else
      lse_set_vargen(V1, lse_vargen_upto(begv, endv, 1));
  end
  else lse_set_vargen(V1, lse_vargen_none);
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

function lse_vargen_has_next(VG: PLseVargen): boolean;
begin
  Result := (VG <> nil) and (VG^.vg_has_next(VG) <> 0);
end;

function lse_vargen_generate(VG: PLseVargen; Value: PLseValue): boolean;
begin
  Result := (VG <> nil) and (VG^.vg_generate(VG, Value) <> 0);
end;

function lse_vargen_rewind(VG: PLseVargen): boolean;
begin
  Result := (VG <> nil) and Assigned(VG^.vg_rewind);
  if Result then VG^.vg_rewind(VG);
end;

function lse_vargen_contains(VG: PLseVargen; Value: PLseValue): boolean;
begin
  Result := (VG <> nil) and Assigned(VG^.vg_contains) and
            (VG^.vg_contains(VG, Value) <> 0);
end;

function empty_vargen_zero(vrec: PLseVargen): integer;cdecl;
begin
  Result := 0;
end;

function empty_vargen_generate(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
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
    vg_type    : nil;
    vg_object  : nil;
    vg_addref  : {$IFDEF FPC}@{$ENDIF}empty_vargen_life;
    vg_release : {$IFDEF FPC}@{$ENDIF}empty_vargen_life;
    vg_has_next: {$IFDEF FPC}@{$ENDIF}empty_vargen_zero;
    vg_generate: {$IFDEF FPC}@{$ENDIF}empty_vargen_generate;
    vg_rewind  : nil;
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
  Result := lse_vargen_ensure(PLseVargen(Param^.p_param[0]^.VObject));
end;

type
  RLiVG_range = packed record
    vgrec: RLseVargen;
    vgref: integer;
    begv, endv, step, curr: int64;
  end;
  PLiVG_range = ^RLiVG_range;

function range_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_range;
begin
  cvgr := PLiVG_range(vrec^.vg_data); 
  with cvgr^ do
  begin
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_range));
  end;
end;

function range_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_range;
begin
  cvgr := PLiVG_range(vrec^.vg_data); 
  with cvgr^ do
  begin
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_range));
  end;
end;

function range_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;

  function in_range(V: int64): boolean;
  var
    cvgr: PLiVG_range;
    BV, EV: int64;
  begin
    cvgr := PLiVG_range(vrec^.vg_data);
    BV := cvgr^.begv;
    EV := cvgr^.endv;
    if BV <= EV then
      Result := (V >= BV) and (V <= EV) else
      Result := (V >= EV) and (V <= BV);
  end;

begin
  case lse_vtype(Value) of
    LSV_INT : Result := Ord(in_range(Value^.VInteger));
         else Result := 0;
  end;
end;

procedure upto_rewind(vrec: PLseVargen);cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do curr := begv;
end;

function upto_has_next(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    Result := Ord(curr <= endv);
end;

function upto_generate(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    if curr <= endv then
    begin
      lse_set_int(Value, curr);
      Inc(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function lse_vargen_upto(begv, endv, step: int64): PLseVargen;
var
  cvgr: PLiVG_range;
begin
  if (begv <= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_addref := @range_addref;
    cvgr^.vgrec.vg_release := @range_release;
    cvgr^.vgrec.vg_has_next := @upto_has_next;
    cvgr^.vgrec.vg_generate := @upto_generate;
    cvgr^.vgrec.vg_rewind := @upto_rewind;
    cvgr^.vgrec.vg_contains := @range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

procedure downto_rewind(vrec: PLseVargen);cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do curr := begv;
end;

function downto_has_next(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    Result := Ord(curr >= endv);
end;

function downto_generate(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    if curr >= endv then
    begin
      lse_set_int(Value, curr);
      Dec(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function lse_vargen_downto(begv, endv, step: int64): PLseVargen;
var
  cvgr: PLiVG_range;
begin
  if (begv >= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_addref := @range_addref;
    cvgr^.vgrec.vg_release := @range_release;
    cvgr^.vgrec.vg_has_next := @downto_has_next;
    cvgr^.vgrec.vg_generate := @downto_generate;
    cvgr^.vgrec.vg_rewind := @downto_rewind; 
    cvgr^.vgrec.vg_contains := @range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

{======================================================================)
(======== stdio =======================================================)
(======================================================================}

function lse_input: integer;
begin
  {$IFDEF FPC}
  Result := StdInputHandle;
  {$ELSE}
  Result := GetStdhandle(STD_INPUT_HANDLE);
  {$ENDIF}
end;

function lse_output: integer;
begin
  {$IFDEF FPC}
  Result := StdOutputHandle;
  {$ELSE}
  Result := GetStdhandle(STD_OUTPUT_HANDLE);
  {$ENDIF}
end;

function lse_errput: integer;
begin
  {$IFDEF FPC}
  Result := StdErrorHandle;
  {$ELSE}
  Result := GetStdhandle(STD_ERROR_HANDLE);
  {$ENDIF}
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
  lse_entries^.cik_clear(FEngineRec.er_kernel);
end;

function TLseEngine.CompileCode(const Code: string): boolean;
begin
  Result := (lse_entries^.cik_compile(FEngineRec.er_kernel, pchar(Code)) <> 0);
end;

constructor TLseEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEngineRec.er_kernel := nil;
  if lse_startup then
  begin
    FInput.s_data := Self;
    FInput.s_read := @input_read;
    FInput.s_readln := @input_readln;
    FInput.s_eof := @input_eof;
    FInput.s_close := @input_close;
    FOutput.s_data := Self;
    FOutput.s_write := @output_write;
    FOutput.s_eof := @output_eof;
    FOutput.s_close := @output_close;
    FEngineRec.er_engine := Self;
    FEngineRec.er_executing := @lse_executing;
    FEngineRec.er_executed := @lse_executed;
    FEngineRec.er_input := @FInput;
    FEngineRec.er_output := @FOutput;
    FEngineRec.er_data := nil;
    lse_entries^.cik_create(@FEngineRec);
  end;
end;

destructor TLseEngine.Destroy;
begin
  if lse_entries <> nil then
    if FEngineRec.er_kernel <> nil then
      lse_entries^.cik_destroy(FEngineRec.er_kernel);
  inherited;
end;

procedure TLseEngine.ExecCommandLine(StartIndex: integer);
var
  fname: string;
begin
  StartIndex := Max(1, StartIndex);
  fname := lse_complete_fname(ParamStr(StartIndex));
  if fname <> '' then
  begin
    SetMainFile(fname);
    SetArgs(SetupArgs(fname, StartIndex + 1));
    if lse_entries^.cik_fexecute(FEngineRec.er_kernel, pchar(fname)) = 0 then
      WriteLine(Error);
  end
  else WriteLine('[ERROR]: File "' + ParamStr(StartIndex) + '" not exists!');
end;

function TLseEngine.ExecuteCode(const Code: string): boolean;
begin
  Result := (lse_entries^.cik_execute(FEngineRec.er_kernel, pchar(Code)) <> 0);
end;

function TLseEngine.GetArgs: string;
var
  sr: PLseString;
begin
  sr := lse_entries^.cik_get_args(FEngineRec.er_kernel);
  Result := lse_strec_string(sr);
  lse_strec_declife(sr);
end;

function TLseEngine.GetMainFile: string;
begin
  Result := lse_entries^.cik_get_main_file(FEngineRec.er_kernel);
end;

function TLseEngine.GetSearchPath: string;
begin
  Result := lse_entries^.cik_get_search_path(FEngineRec.er_kernel);
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
  Result := lse_entries^.cik_errno(FEngineRec.er_kernel);
end;

function TLseEngine.ErrorCol: integer;
begin
  Result := lse_entries^.cik_error_col(FEngineRec.er_kernel) + 1;
end;

function TLseEngine.ErrorIncludedFile: string;
begin
  Result := lse_entries^.cik_error_file(FEngineRec.er_kernel);
end;

function TLseEngine.ErrorMsg: string;
begin
  Result := lse_entries^.cik_error_msg(FEngineRec.er_kernel);
end;

function TLseEngine.ErrorName: string;
begin
  Result := lse_entries^.cik_error_name(FEngineRec.er_kernel);
end;

function TLseEngine.ErrorModule: string;
begin
  Result := lse_entries^.cik_error_module(FEngineRec.er_kernel);
end;

function TLseEngine.ErrorRow: integer;
begin
  Result := lse_entries^.cik_error_row(FEngineRec.er_kernel) + 1;
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
  Result := (lse_entries^.cik_ready(FEngineRec.er_kernel) <> 0);
end;

function TLseEngine.Running: boolean;
begin
  Result := (lse_entries^.cik_running(FEngineRec.er_kernel) <> 0);
end;

procedure TLseEngine.SetArgs(const Value: string);
begin
  lse_entries^.cik_set_args(FEngineRec.er_kernel, pchar(Value));
end;

procedure TLseEngine.SetMainFile(const Value: string);
begin
  lse_entries^.cik_set_main_file(FEngineRec.er_kernel, pchar(Value));
end;

procedure TLseEngine.SetSearchPath(const Value: string);
begin
  lse_entries^.cik_set_search_path(FEngineRec.er_kernel, pchar(Value));
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
  lse_entries^.cik_terminate(FEngineRec.er_kernel);
end;

procedure TLseEngine.WriteData(Data: pointer; Count: integer);
begin
  output_write(FEngineRec.er_output, Data, Count);
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
  output_write(FEngineRec.er_output, pchar(sLineBreak), Length(sLineBreak));
end;

procedure TLseEngine.WriteStream(AStream: TStream);
var
  buf: array[0..1023] of char;
  bytes: integer;
begin
  bytes := AStream.Read(buf, sizeof(buf));
  while bytes > 0 do
  begin
    output_write(FEngineRec.er_output, @buf[0], bytes);
    bytes := AStream.Read(buf, sizeof(buf));
  end;
end;

procedure TLseEngine.WriteText(const Text: string);
begin
  output_write(FEngineRec.er_output, pchar(Text), Length(Text));
end;

function TLseEngine.CompileFile(const FileName: string): boolean;
var
  fname: string;
begin
  SetMainFile(FileName);
  fname := GetMainFile;
  Result := (lse_entries^.cik_fcompile(FEngineRec.er_kernel, pchar(fname)) <> 0);
end;

function TLseEngine.ExecuteFile(const FileName: string): boolean;
var
  fname: string;
begin
  SetMainFile(FileName);
  fname := GetMainFile;
  Result := (lse_entries^.cik_fexecute(FEngineRec.er_kernel, pchar(fname)) <> 0);
end;

function TLseEngine.Exited: boolean;
begin
  Result := (lse_entries^.cik_exited(FEngineRec.er_kernel) <> 0);
end;

function TLseEngine.ResultType: string;
begin
  Result := lse_entries^.cik_result_type(FEngineRec.er_kernel);
end;

function TLseEngine.ResultText: string;
begin
  Result := lse_entries^.cik_result_text(FEngineRec.er_kernel);
end;

function TLseEngine.Terminated: boolean;
begin
  Result := (lse_entries^.cik_terminated(FEngineRec.er_kernel) <> 0);
end;

function TLseEngine.Terminating: boolean;
begin
  Result := Terminated and Running;
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
  lse_set_bool(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnChar(const Value: char);
begin
  lse_set_char(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnError(const ID: string; Errno: integer; const Msg: string);
begin
  lse_entries^.cik_param_error(FParam, pchar(ID), Errno, pchar(Msg));
end;

function TLseInvoke.Read(const Buf: pchar; Count: integer): integer;
begin
  Result := lse_entries^.cik_read(lse_get_engine(FParam), Buf, Count);
end;

function TLseInvoke.Readln: string;
var
  S: PLseString;
begin
  S := lse_entries^.cik_readln(lse_get_engine(FParam));
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
  lse_set_float(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnInt64(const Value: int64);
begin
  lse_set_int(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnObj(CR: PLseType; Value: pointer);
begin
  lse_set_object(FParam^.p_result, CR, Value);
end;

procedure TLseInvoke.ReturnObject(T: PLseType; Value: pointer);
begin
  lse_set_object(FParam^.p_result, T, Value);
end;

procedure TLseInvoke.ReturnStr(const Value: string);
begin
  lse_set_string(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnStream(Value: TStream);
begin
  lse_set_stream(FParam^.p_result, lse_wrap_stream(Value, true));
end;

procedure TLseInvoke.ReturnStream(Value: PLseStream);
begin
  lse_set_stream(FParam^.p_result, Value);
end;

procedure TLseInvoke.ReturnInt(const Value: integer);
begin
  lse_set_int(FParam^.p_result, Value);
end;

function TLseInvoke.GetThis(var obj): boolean;
var
  this: pointer;
begin
  this := paramObject(0);
  Result := (this <> nil);
  if Result then
    pointer(obj) := this else
    returnError('', 0, 'this was not specified!');
end;

constructor TLseInvoke.Create(Param: PLseParam);
begin
  FParam := Param;
end;

function TLseInvoke.ParamClass(Index: integer): PLseType;
begin
  Result:= lse_type(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamCount: integer;
begin
  Result := FParam^.p_count;
end;

function TLseInvoke.ParamFloat(Index: integer): double;
begin
  Result := lse_get_float(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamFmt(Index: integer): string;
begin
  Result := FormatStr(paramStr(Index));
end;

function TLseInvoke.ParamInt(Index: integer): integer;
begin
  Result := lse_get_int(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamInt64(Index: integer): int64;
begin
  Result := lse_get_int(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamBool(Index: integer): boolean;
begin
  Result := lse_get_bool(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamObject(Index: integer): pointer;
begin
  Result := lse_get_obj(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamStr(Index: integer): string;
begin
  Result := lse_get_str(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamStrec(Index: integer): PLseString;
begin
  Result := lse_get_strec(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamChar(Index: integer): char;
begin
  Result := lse_get_char(FParam^.p_param[Index]);
end;

function TLseInvoke.ParamStream(Index: integer): PLseStream;
begin
  Result := lse_get_obj(FParam^.p_param[Index], KT_STREAM);
end;

procedure TLseInvoke.Print(const Str: string);
begin
  lse_entries^.cik_write(lse_get_engine(FParam), pchar(Str), Length(Str));
end;

{ TLsePatten }

constructor TLsePatten.Create(const Patten: pchar);
begin
  inherited Create;
  SetLength(FMatches, 1);
  Init(Patten);
end;

destructor TLsePatten.Destroy;
begin
  SetLength(FMatches, 0);
  inherited Destroy;
end;

const zero_char: char = #0;

function TLsePatten.Exec(S: pchar; L: integer): boolean;
var
  ps: pchar;   // patten string
  ep: pchar;   // end position
  px: integer; // paren index
begin
  Result := false;
  ResetResult;
  if not FError and (FPatten <> nil) and (((L > 0) and (S <> nil)) or (L = 0)) then
  try
    if (L = 0) and (S = nil) then
      S := @zero_char;
    FSource := S;
    FEos := S + L;
    ps := FPatten;
    if FAnchor then
    begin
      Inc(ps);
      if (ps^ = '$') and ((ps + 1)^ = #0) then
      begin
        if L = 0 then
        begin
          FMatches[0].m_str := S;
          Result := true;
        end;
        Exit;
      end;
    end;

    repeat
      ep := Match(S, ps);
      if ep <> nil then
      begin
        for px := 1 to FCount - 1 do
          Check(FMatches[px].m_len >= 0);
        FMatches[0].m_str := S;
        FMatches[0].m_len := ep - S;
        Result := true;
      end
      else
      begin
        Inc(S);
        FCount := 1;
      end;
    until Result or FAnchor or (S >= FEos);
  except
    Result := false;
    FError := true;
    ResetResult;
  end;
end;

function TLsePatten.ExecStrec(S: PLseString): boolean;
begin
  Result := Exec(lse_strec_data(S), lse_strec_length(S)); 
end;

function TLsePatten.Next: boolean;
var
  S, P: pchar;
begin
  Result := false;
  if not FError and not FAnchor and (FMatches[0].m_str <> nil) then
  begin
    S := FSource;
    P := FMatches[0].m_str + FMatches[0].m_len;
    if P < FEos then
    try
      Result := Exec(P, FEos - P);
    finally
      FSource := S;
    end;
  end;
end;

function TLsePatten.Replace(Src, New: PLseString; Times: int64): PLseString;
var
  match_list: array of RLseMatch;
  match_len, X: integer;
  src_str, new_str, res_str: pchar;
  src_len, new_len, res_len: integer;

  procedure save_match_result;
  var
    X: integer;
  begin
    X := Length(match_list);
    SetLength(match_list, X + 1);
    match_list[X] := FMatches[0];
    Inc(match_len, FMatches[0].m_len);
  end;

begin
  if (Times < 1) or not ExecStrec(Src) then
  begin
    Result := Src;
    Exit;
  end;

  Dec(Times);
  match_len := 0;
  SetLength(match_list, 0);
  save_match_result;
  while (Times > 0) and Next do
  begin
    save_match_result;
    Dec(Times);
  end;

  Times := Length(match_list);
  new_str := lse_strec_data(New);
  new_len := lse_strec_length(New);
  res_len := (FEos - FSource) - match_len + integer(new_len * Times);
  if res_len > 0 then
  begin
    Result := lse_strec_alloc(nil, res_len);
    res_str := lse_strec_data(Result);
    src_str := FSource;
    for X := 0 to Times - 1 do
    begin
      src_len := match_list[X].m_str - src_str;
      Move(src_str^, res_str^, src_len);
      Dec(res_len, src_len);
      Inc(res_str, src_len);
      Move(new_str^, res_str^, new_len);
      Dec(res_len, new_len);
      Inc(res_str, new_len);
      src_str := match_list[X].m_str + match_list[X].m_len;
    end;
    Move(src_str^, res_str^, res_len);
  end
  else Result := nil;
end;

function TLsePatten.MatchCount: integer;
begin
  if FMatches[0].m_str <> nil then
    Result := FCount else
    Result := 0;
end;

function TLsePatten.MatchStr(Index: integer): pchar;
begin
  if FMatches[0].m_str <> nil then
    Result := FMatches[Index].m_str else
    Result := nil;
end;

function TLsePatten.MatchPos(Index: integer): integer;
begin
  if FMatches[0].m_str <> nil then
    Result := FMatches[Index].m_str - FSource else
    Result := -1;
end;

function TLsePatten.MatchLen(Index: integer): integer;
begin
  if FMatches[0].m_str <> nil then
    Result := FMatches[Index].m_len else
    Result := -1;
end;

function TLsePatten.SourceLength: integer;
begin
  Result := FEos - FSource;
end;

function TLsePatten.Init(const Patten: pchar): boolean;
begin
  Result := false;
  FPatten := nil;
  FAnchor := false;
  if (Patten <> nil) and (Patten^ <> #0) then
    if ((Patten + 1)^ <> #0) or not (Patten^ in ['^', '$', '(', '[', '\']) then
    begin
      Result := true;
      FPatten := Patten;
      FAnchor := (Patten^ = '^');
    end;
  ResetResult;
  FError := not Result;
end;

function TLsePatten.Match(Src, Pat: pchar): pchar;
// pc: patten class
// ch: source char
// ss: source string
// ps: patten string
// ep: end position of patten
// lc: level char

  function match_escape(ch, pc: char): boolean;
  var
    L: char;
  begin
    if pc in ['A'..'Z'] then
      L := char(Ord(pc) + LCS_DISTANCE) else
      L := pc;
    case L of
      'a': Result := ch in LCS_ALPHA;
      'c': Result := ch in LCS_CNTRL;
      'd': Result := ch in LCS_DIGIT;
      'l': Result := ch in ['a'..'z'];
      'p': Result := ch in LCS_PUNCT;
      's': Result := ch in LCS_SPACE;
      'u': Result := ch in ['A'..'Z'];
      'w': Result := ch in LCS_ALNUM;
      'x': Result := ch in LCS_HEX;
      'z': Result := (ch = #0);
      else Result := (ch = pc);
    end;
    Result := Result xor (pc <> L);
  end;

  function match_bracket(ch: char; ps, ep: pchar): boolean;
  begin
    Result := true;

    Inc(ps);
    if ps^ = '^' then
    begin
      Result := false;
      Inc(ps);
    end;

    while ps < ep do
    begin
      if ps^ = '\' then
      begin
        Inc(ps);
        if match_escape(ch, ps^) then Exit;
      end
      else
      if ((ps + 1)^ = '-') and ((ps + 2) < ep) then
      begin
        Inc(ps, 2);
        if ((ps - 2)^ <= ch) and (ch <= ps^) then Exit;
      end
      else
      if ps^ = ch then Exit;
      Inc(ps);
    end;

    Result := not Result;
  end;

  function match_single(ch: char; ps, ep: pchar): boolean;
  begin
    if ps^ = '.' then Result := true else
    if ps^ = '[' then
      Result := match_bracket(ch, ps, ep - 1) else
    if ps^ = '\' then
      Result := match_escape(ch, (ps + 1)^) else
      Result := (ps^ = ch);
  end;

  function match_balance(var ss: pchar; ps: pchar): boolean;
  var
    begc, endc: char;
    rest: integer;
  begin
    Check((ps^ <> #0) and ((ps + 1)^ <> #0));
    if ss^ = ps^ then
    begin
      begc := ps^;
      endc := (ps + 1)^;
      rest := 1;
      Inc(ss);
      while ss < FEos do
      begin
        if ss^ = endc then
        begin
          Dec(rest);
          if rest = 0 then
          begin
            Result := true;
            Inc(ss);
            Exit;
          end;
        end
        else
        if ss^ = begc then Inc(rest);
        Inc(ss);
      end;
    end;
    Result := false;
  end;

  function start_paren(ss, ps: pchar): pchar;
  var
    X: integer;
  begin
    X := FCount;
    if X = Length(FMatches) then
      SetLength(FMatches, X + 4);
    FMatches[X].m_str := ss;
    FMatches[X].m_len := -1;
    Inc(FCount);
    Result := Match(ss, ps);
    if Result = nil then
      Dec(FCount);
  end;

  function close_paren(ss, ps: pchar): pchar;
  var
    X: integer;
  begin
    X := FCount - 1;
    while (X >= 1) and (FMatches[X].m_len >= 0) do Dec(X);
    Check(X >= 1);
    FMatches[X].m_len := ss - FMatches[X].m_str;
    Result := Match(ss, ps);
    if Result = nil then
      FMatches[X].m_len := -1;
  end;

  function max_expand(ss, ps, ep: pchar): pchar;
  var
    X: integer;
  begin
    X := 0;
    while ((ss + X) < FEos) and match_single((ss + X)^, ps, ep) do Inc(X);
    while X >= 0 do
    begin
      Result := Match(ss + X, ep + 1);
      if Result <> nil then Exit;
      Dec(X);
    end;
    Result := nil;
  end;

  function min_expand(ss, ps, ep: pchar): pchar;
  begin
    Result := Match(ss, ep + 1);
    while Result = nil do
      if (ss < FEos) and match_single(ss^, ps, ep) then
      begin
        Inc(ss);
        Result := Match(ss, ep + 1);
      end
      else Exit;
  end;

label MATCH_AGAIN;
var
  ep: pchar;   // end Pat
  sm: boolean; // single match
begin
  MATCH_AGAIN:

  if Pat^ = '(' then
  begin
    Result := start_paren(Src, Pat + 1);
    Exit;
  end;

  if Pat^ = ')' then
  begin
    Result := close_paren(Src, Pat + 1);
    Exit;
  end;

  if (Pat^ = #0) or ((Pat^ = '$') and ((Pat + 1)^ = #0) and (Src = FEos)) then
  begin
    Result := Src;
    Exit;
  end;

  Result := nil;

  if (Pat^ = '\') and ((Pat + 1)^ = 'b') then
    if match_balance(Src, Pat + 2) then
    begin
      Inc(Pat, 4);
      goto MATCH_AGAIN; // Match(mp, Src, Pat + 4);
    end
    else Exit;

  // get end of class
  ep := Pat;
  if ep^ = '\' then
  begin
    Inc(ep);
    Check(ep^ <> #0);
  end
  else
  if ep^ = '[' then
  begin
    Inc(ep);
    if ep^ = '^' then Inc(ep);
    repeat
      if ep^ = '\' then Inc(ep);
      Check(ep^ <> #0);
      Inc(ep);
    until ep^ = ']';
  end;
  Inc(ep);

  if ep^ = '*' then
  begin
    Result := max_expand(Src, Pat, ep); // 0..max
    Exit;
  end;

  if ep^ = '-' then
  begin
    Result := min_expand(Src, Pat, ep); // 0..min
    Exit;
  end;

  sm := (Src < FEos) and match_single(Src^, Pat, ep);

  if ep^ = '?' then
  begin
    if sm then
    begin
      Result := Match(Src + 1, ep + 1);
      if Result <> nil then Exit;
    end;
    Pat := ep + 1;
    goto MATCH_AGAIN; // Match(mp, Src, ep + 1);
  end;

  if sm then
  begin
    if ep^ = '+' then
    begin
      Result := max_expand(Src + 1, Pat, ep); // 1..max
      Exit;
    end;
    Inc(Src);
    Pat := ep;
    goto MATCH_AGAIN; // Match(mp, Src + 1, ep);
  end;
end;

procedure TLsePatten.Check(ok: boolean);
begin
  if not ok then Abort;
end;

procedure TLsePatten.ResetResult;
begin
  FMatches[0].m_str := nil;
  FMatches[0].m_len := 0;
  FCount := 1;
end;

{ TLseNamed }

constructor TLseNamed.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TLseHashNamed }

procedure TLseHashNamed.Clear;
var
  X: integer;
  B, N: PLiNameItem;
begin
  for X := 0 to FSize - 1 do
  begin
    B := FBuckets[X];
    FBuckets[X] := nil;
    while B <> nil do
    begin
      N := B^.ni_next;
      FreeItem(B);
      B := N;
    end;
  end;
end;

constructor TLseHashNamed.Create(Size: cardinal);
begin
  inherited Create;
  FSize := Max(2, Size);
  SetLength(FBuckets, FSize);
  FillChar(FBuckets[0], FSize * sizeof(PLiNameItem), 0);
end;

destructor TLseHashNamed.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

function TLseHashNamed.FindItem(const Key: string): PLiNameItem;
begin
  Result := FBuckets[HashOf(Key)];
  while Result <> nil do
  begin
    if Key = Result^.ni_nobj.FName then Exit;
    Result := Result^.ni_next;
  end;
end;

procedure TLseHashNamed.FreeItem(Item: PLiNameItem);
begin
  Dec(FCount);
  lse_mem_free(Item, sizeof(RLiNameItem));
end;

function TLseHashNamed.Get(const Key: string): TLseNamed;
var
  M: PLiNameItem;
begin
  M := FindItem(Key);
  if M <> nil then
    Result := M^.ni_nobj else
    Result := nil;
end;

function TLseHashNamed.HashOf(const Key: string): cardinal;
begin
  Result := lse_hash_of(pchar(Key)) mod FSize;
end;

function TLseHashNamed.NewItem: PLiNameItem;
var
  Z: cardinal;
  L: TList;
  M: PLiNameItem;
  X, H: integer;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiNameItem));
  Inc(FCount);
  Z := FCount div 16;
  if Z > FSize then
  begin
    L := TList.Create;
    try
      for X := 0 to FSize - 1 do
      begin
        M := FBuckets[X];
        while M <> nil do
        begin
          L.Add(M);
          M := M^.ni_next;
        end;
      end;
      SetLength(FBuckets, Z);
      FillChar(FBuckets[0], Z * sizeof(PLiNameItem), 0);
      FSize := Z;
      for X := 0 to L.Count - 1 do
      begin
        M := PLiNameItem(L[X]);
        H := HashOf(M^.ni_nobj.FName);
        M^.ni_next := FBuckets[H];
        FBuckets[H] := M;
      end;
    finally
      L.Free;
    end;
  end; 
end;

procedure TLseHashNamed.Put(AObj: TLseNamed);
var
  X: integer;
  M: PLiNameItem;
begin
  if AObj <> nil then
    if FindItem(AObj.FName) = nil then
    begin
      X := HashOf(AObj.FName);
      M := NewItem;
      M^.ni_nobj := AObj;
      M^.ni_next := FBuckets[X];
      FBuckets[X] := M;
    end;
end;

procedure TLseHashNamed.Remove(const Key: string);
var
  X: integer;
  M, P: PLiNameItem;
begin
  X := HashOf(Key);
  P := FBuckets[X];
  if P <> nil then
    if Key = P^.ni_nobj.FName then
    begin
      FBuckets[X] := P^.ni_next;
      FreeItem(P);
    end
    else
    while P^.ni_next <> nil do
    begin
      M := P^.ni_next;
      if Key = M^.ni_nobj.FName then
      begin
        P^.ni_next := M^.ni_next;
        FreeItem(M);
        Exit;
      end;
      P := M;
    end;
end;

end.
