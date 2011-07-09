{==============================================================================}
{        UNIT: lse_kernel                                                      }
{ DESCRIPTION: kernel of lysee                                                 }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/29                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_kernel;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu, lse_symbol, lse_funcs, lse_syncobj, lse_patten
  {$IFDEF WINDOWS},Windows{$ENDIF};

const
  RPN_MAIN = 'main'; {<--main function & module name}

type
  KLiVarb         = class; {forward}
  KLiVarbList     = class;
  KLiFunc         = class;
  KLiExprList     = class;
  KLiType         = class;
  KLiSyntax       = class;
  KLiModule       = class;
  KLiModuleList   = class;
  KLiError        = class;
  KLiEngine       = class;
  KLiVarList      = class;
  KLiVarSnap      = class;
  KLiCallStack    = class;
  KLiRunner       = class;
  KLiSatisfy      = class;
  KLiHashed       = class;

  KLiObjRecState  = (orsInChain, orsMarked);
  KLiObjRecStates = set of KLiObjRecState;
  
  PLiObjRec = ^RLiObjRec;
  RLiObjRec = packed record
    or_object: pointer;
    or_type  : KLiType;
    or_prev  : PLiObjRec;
    or_next  : PLiObjRec;
    or_state : KLiObjRecStates;
  end;

  KLiFindObject = (foNone, foVarb, foFunc, foType, foSyntax, foModule);
  KLiFindObjects = set of KLiFindObject;

  KLiFindRec = packed record
    case fo_type: KLiFindObject of
      foVarb  : (VVarb  : KLiVarb);
      foFunc  : (VFunc  : KLiFunc);
      foType  : (VType  : KLiType);
      foSyntax: (VSyntax: KLiSyntax);
      foModule: (VModule: KLiModule);
  end;
  PLiFindRec = ^KLiFindRec;

  KLiExprFlag = (
    xrSatisfied,
    xrInFinally
  );
  KLiExprFlags = set of KLiExprFlag;

  PLiExprRec = ^RLiExprRec;
  RLiExprRec = record
    Sym: KLiSymbol;
    Pos: KLiSymPos;
    Name: string;        {<--syCall, syFloat, syTime, syInt, syStr}
    ParamCount: integer; {<--syCall, syReturn}
    flags: KLiExprFlags; {<--express flags}
    case KLiSymbol of
      syID    : (VVarb   : KLiVarb);
      syFunc  : (VFunc   : KLiFunc);    // syCall
      syFloat : (VFLoat  : double);
      syInt   : (VInteger: int64);
      syStr   : (VStr    : PLseString);
      syType  : (VType   : KLiType);
      syModule: (VModule : KLiModule);
      syJump  : (VOffset : integer);
      syGoto  : (VLabel  : PLiExprRec);
  end;

  { KLiParser }

  KLiParser = class(KLiObject)
  private
    FTokenizer: KLiTokenizer; {<--tokens analyzer}
    FSymbols: KLiTokens;      {<--token list}
    FLast: PLiToken;          {<--last token}
    FExpr: TList;             {<--temp expression}
    FCurrent: KLiFunc;        {<--current function}
    FModule: KLiModule;       {<--current module}
    FRunner: KLiRunner;       {<--current runner}
    FTryCount: integer;       {<--embeded try blocks}
    FCatchCount: integer;     {<--embeded catch blocks}
    FIsShadow: boolean;       {<--is a shadow parser}
    FBreakLabel: string;      {<--break label name}
    FContinueLabel: string;   {<--continue label name}
    function GetLastRow: integer;
    function GetLastCol: integer;
    function GetLastVal: string;
    function Shadow: KLiParser;
    function CurCodes: KLiExprList;
    function Error: KLiError;
    { symbolization }
    function GetNextSym: boolean;
    function GetSym(sym: PLiToken): boolean;
    function PeekNextSym: KLiSymbol;
    function PeekNextTwoSym(var one, two: KLiSymbol): integer;
    procedure SymExpected(Sym: PLiToken; Syms: KLiSymbols);
    procedure SymGotoNext;
    procedure SymTestLast(Syms: KLiSymbols);
    procedure SymTestNext(Syms: KLiSymbols);
    procedure SymTestLastPureID;
    procedure SymTestNextPureID;
    function CloneSym(sym: PLiToken): PLiToken;
    procedure ExpandSyntax(ASyntax: KLiSyntax; IsStatement: boolean);
    function FindSyntax(const ID: string): KLiSyntax;
    { labels }
    procedure SaveLabels(var BreakLabel, ContinueLabel: string; CreateNewLabels: boolean);
    procedure RestoreLabels(const BreakLabel, ContinueLabel: string);
    { parsing }
    procedure ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
    function  ParseLambda: KLiFunc;
    function  ParseLambdaFunc: KLiFunc;
    function  ParseVarType(Token: PLiToken): KLiType;
    procedure ParseVarb(var varb: KLiVarb; var vrec: PLiToken;
                        EndSyms: KLiSymbols; OnHead: boolean);

    procedure ParseDefine;
    procedure ParseImport;
    procedure ParseConst;
    procedure ParseSyntax;
    procedure ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
    procedure ParseStatement(OnHead: boolean);
    procedure ParseAny;
    procedure ParseIf;
    procedure ParseFor;
    procedure ParseWhile;
    procedure ParseRepeatUntil;
    procedure ParseSwitch;
    procedure ParseBreak;
    procedure ParseContinue;
    procedure ParseReturn;
    procedure ParseTry;
    procedure ParseEcho;

    procedure ParseExpr(Expr: TList; EndSyms: KLiSymbols; OnHead: boolean);
    procedure ParseDotItem(Expr: TList);
    procedure ParseAsk(Expr: TList);
    { tail if }
    procedure BeginTailIf(var end_if_label: string; EndSym: KLiSymbol);
    procedure EndTailIf(const end_if_label: string);
    procedure TailIf(ExprList: TList);
  public
    constructor Create(AModule: KLiModule);
    destructor Destroy;override;
    function Parse(const Code: string): KLiFunc;
    function ParseAndFree(const Code: string): KLiFunc;
    property LastRow: integer read GetLastRow;
    property LastCol: integer read GetLastCol;
    property LastVal: string read GetLastVal;
    function LastModule: KLiModule;
  end;

  { KLiVarb }

  KLiVarb = class(KLiNameObject)
  private
    FType: KLiType;
    FList: KLiVarbList;
    FPos: KLiSymPos;
    FIndex: integer;
  public
    constructor Create(AList: KLiVarbList; const AName: string;
                       ValueType: KLiType);virtual;
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    function Prototype(HideType: boolean): string;
    function Func: KLiFunc;
    property ValueType: KLiType read FType;
    property Pos: KLiSymPos read FPos;
    property Index: integer read FIndex write FIndex;
  end;

  KLiVarbList = class(KLiObject)
  private
    FFunc: KLiFunc;
    FList: TList;
    function GetCount: integer;
    function GetVarb(Index: integer): KLiVarb;
  public
    constructor Create(AFunc: KLiFunc);
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    function Add(const Name: string; ValueType: KLiType): KLiVarb;
    function IndexOf(const Name: string): integer;
    function Find(const Name: string): KLiVarb;
    function Exists(const Name: string): boolean;
    function ToVarlist(Engine: KLiEngine): KLiVarList;
    function AsString(HideType: boolean): string;
    function IsParam: boolean;
    function FirstIs(AType: KLiType): boolean;
    property Func: KLiFunc read FFunc;
    property Count: integer read GetCount;
    property Varbs[Index: integer]: KLiVarb read GetVarb;default;
  end;

  { KLiFunc }

  KLiFuncState = (fusMainFunc, fusInitFunc, fusCurry, fusNameCall, fusLambda,
                  fusConst, fusExpandThis, fusEmpty);
  KLiFuncStates = set of KLiFuncState;

  KLiFunc = class(KLiNameObject)
  private
    FModule: KLiModule;
    FParams: KLiVarbList;
    FResultType: KLiType;
    FDescription: string;
    FState: KLiFuncStates;
    FCodes: KLiExprList;
    FProc: pointer;
    FNext: KLiFunc;
    FPrev: KLiFunc;
    function HasState(Index: KLiFuncState): boolean;
    procedure SetState(Index: KLiFuncState; Value: boolean);
  public
    constructor Create(Parent: KLiModule; AResultType: KLiType;
      const AName: string; Params: TStringList; Proc: pointer);
    destructor Destroy;override;
    procedure Garbaged;virtual;
    procedure SaveTo(V: PLseValue);
    procedure Satisfy;
    function Execute(Param: PLseParam): boolean;
    procedure DumpCode(list: TStrings; const margin: string);
    function Prototype(ShowFullName: boolean): string;
    function AddParam(const AName: string; varType: KLiType): KLiVarb;
    function AddSuper: KLiVarb;
    function FullName: string;
    function FindInside(const ID: string; rec: PLiFindRec = nil): boolean;
    function FindBy(const ID: string; rec: PLiFindRec; Range: KLiFindObjects = []): boolean;
    function FindMethod(const AName: string; AType: KLiType): KLiFunc;
    function FindCreate(AType: KLiType): KLiFunc;
    function Engine: KLiEngine;
    property Module: KLiModule read FModule;
    property Params: KLiVarbList read FParams;
    property ResultType: KLiType read FResultType write FResultType;
    property IsMainFunc: boolean index fusMainFunc read HasState;
    property IsInitFunc: boolean index fusInitFunc read HasState write SetState;
    property IsCurryFunc: boolean index fusCurry read HasState write SetState;
    property IsNameCall: boolean index fusNameCall read HasState write SetState;
    property IsConstFunc: boolean index fusConst read HasState write SetState;
    property IsLambdaFunc: boolean index fusLambda read HasState write SetState;
    property IsEmptyFunc: boolean index fusEmpty read HasState;
    property ExpandThis: boolean index fusExpandThis read HasState write SetState;
    property Description: string read FDescription write FDescription;
    property Proc: pointer read FProc write FProc;
    property Codes: KLiExprList read FCodes;
    property Next: KLiFunc read FNext;
  end;

  KLiExprList = class(KLiObject)
  private
    FFunc: KLiFunc;
    FLocals: KLiVarbList;
    FItems: KLiList;
    FSatisfyIndex: integer;
    function GetCount: integer;
    function GetItem(Index: integer): PLiExprRec;
    function GetLast: PLiExprRec;
    function GetLastIndex: integer;
    function GetModule: KLiModule;
    function GetEngine: KLiEngine;
    function GetError: KLiError;
  public
    constructor Create(AFunc: KLiFunc);
    destructor Destroy;override;
    procedure Satisfy;
    procedure Clear(Sender: TObject = nil);
    procedure Delete(Index: integer);
    function Add(AExprRec: PLiExprRec): integer;
    procedure Insert(Index: integer; AExprRec: PLiExprRec);
    function AddNew(sym: KLiSymbol; SymPos: PLiSymPos): PLiExprRec;
    function InsertNew(Index: integer; sym: KLiSymbol; SymPos: PLiSymPos): PLiExprRec;
    procedure LoadToken(token: PLiToken);
    procedure LoadExpr(List: TList; Start: integer = 0);
    procedure DumpCode(List: TStrings; const Margin: string);
    procedure EndStatement;
    function IsEmpty: boolean;
    { locals }
    function AddLocal(const Name: string; varType: KLiType): KLiVarb;
    procedure PushVarb(AVarb: KLiVarb; Pos: KLiSymPos);
    property Locals: KLiVarbList read FLocals;
    { label }
    function AddGoto(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function ChangeGoto(const OrgLabel, NewLabel: string): integer;
    function AddTry(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function AddLabel(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function FindLabel(const Name: string): PLiExprRec;
    function LastLabel: string;
    { property }
    property Engine: KLiEngine read GetEngine;
    property Module: KLiModule read GetModule;
    property Error: KLiError read GetError;
    property Count: integer read GetCount;
    property Last: PLiExprRec read GetLast;
    property LastIndex: integer read GetLastIndex;
    property Items[Index: integer]: PLiExprRec read GetItem;default;
  end;

  KLiFunc_curry = class(KLiFunc)
  private
    FCurry: array of PLseValue;
    FCurryFunc: KLiFunc;
    FObjRec: RLiObjRec;
    function GetObjRec: PLiObjRec;
    function GetCurryCount: integer;
    function GetCurryData(Index: integer): PLseValue;
  public
    constructor Create(AModule: KLiModule; const AName: string; AFunc: KLiFunc);
    destructor Destroy;override;
    procedure Garbaged;override;
    function AddCurry(value: PLseValue): integer;
    property ObjRec: PLiObjRec read GetObjRec;
    property CurryFunc: KLiFunc read FCurryFunc;
    property CurryCount: integer read GetCurryCount;
    property CurryData[Index: integer]: PLseValue read GetCurryData;
  end;

  KLiFunc_operator = class(KLiFunc)
  private
    FOper: KLiSymbol;
  public
    constructor Create(AOper: KLiSymbol);
  end;
  
  { KLiType }

  KLiType = class(KLiNameObject)
  private
    FModule: KLiModule;
    FTypeRec: PLseType;
    function GetDataType: TLseValue;
    function GetDescription: string;
    function GetFullName: string;
  public
    constructor Create(AModule: KLiModule; const AName: string; AType: TLseValue);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    function Prototype(const ID: string): string;
    function ObjectToString(obj: pointer): string;
    function StrecToObject(const S: PLseString; Engine: KLiEngine): pointer;
    function Cast(Param: PLseParam; Index: integer): PLseValue;
    function IsSimpleType: boolean;
    property FullName: string read GetFullName;
    property Module: KLiModule read FModule;
    property TypeRec: PLseType read FTypeRec;
    property DataType: TLseValue read GetDataType;
    property Description: string read GetDescription;
  end;

  { KLiSyntax }

  KLiSyntax = class(KLiNameObject)
  private
    FModule: KLiModule;
    FPrev: KLiSyntax;
    FNext: KLiSyntax;
    FArgs: array of PLiToken;
    FBody: TList;
    FLocals: TList;
    FRenameCount: integer;
  public
    constructor Create(AModule: KLiModule; const AName: string);
    destructor Destroy;override;
    procedure AddArgument(Arg: PLiToken);
    procedure AddToken(Token: PLiToken);
    procedure RenameLocals;
    function ArgCount: integer;
    function ArgToken(Index: integer): PLiToken;
  end;
  
  { KLiModule }

  KLiModuleType = (
    moyKernel,        {<--K: builtin module}
    moyRegistered,    {<--R: by lse_module_setup()}
    moyLibrary,       {<--L: extending library}
    moyScript         {<--S: Lysee script moudle}
  );

  KLiModuleState = (mosParsing);
  KLiModuleStates = set of KLiModuleState;

  KLiModule = class(KLiNameObject)
  private
    FFileName: string;          {<--KRLS: module file name}
    FModuleType: KLiModuleType; {<--KRLS: module type}
    FVersion: string;           {<--KRLS: module version}
    FDescription: string;       {<--KRLS: description}
    FTypeList: TStringList;     {<--KRLS: type list}
    FFuncList: KLiNameHashed;   {<--KRLS: function list}
    FFirstFunc: KLiFunc;        {<--KRLS: first function}
    FLastFunc: KLiFunc;         {<--KRLS: last function}
    FInvokeProc: TLseOnInvoke;  {<--*RL*: call gate function}
    FHandle: THandle;           {<--**L*: library (DLL) handle}
    FEngine: KLiEngine;         {<--***S: owner script engine}
    FModules: KLiModuleList;    {<--***S: modules imported by this module}
    FImporters: TList;          {<--***S: modules importing this module}
    FMainFunc: KLiFunc;         {<--***S: modules entry function}
    FParsing: boolean;          {<--***S: parsing}
    FFirstSyntax: KLiSyntax;    {<--***S: first syntax}
    FLastSyntax: KLiSyntax;     {<--***S: last syntax}
  public
    constructor Create(const AName: string; Engine: KLiEngine; ModuleType: KLiModuleType);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    procedure Satisfy;
    procedure DeleteFunctions;
    procedure DumpCodeToStream(stream: TStream; const margin: string);
    function NewTempID(const Prefix: string): string;
    function NewFuncName: string;
    function NewLabelName: string;
    function NewFunc: KLiFunc;
    function SetupFunc(Func: PLseFunc): KLiFunc;
    function SetupModuleFuncs(Rec: PLseFuncListRec): integer;
    function FindModule(const ID: string; FindPossible: boolean): KLiModule;
    function AddImporter(module: KLiModule): KLiModule;
    function IsMainModule: boolean;
    function SetupType(const TR: PLseType): KLiType;
    function SetupModuleTypes(const TLR: PLseTypeListRec): integer;
    function TypeCount: integer;
    function GetType(Index: integer): KLiType;
    function FindType(const ID: string): KLiType;
    function FindTypeBy(const ID, module_name: string): KLiType;
    function FindFunc(const ID: string): KLiFunc;
    function FindSyntax(const ID: string): KLiSyntax;
    function Find(const ID: string; rec: PLiFindRec = nil): boolean;
    function FindBy(const ID, module_name: string; rec: PLiFindRec): boolean;
    property ModuleType: KLiModuleType read FModuleType;
    property Modules: KLiModuleList read FModules;
    property FileName: string read FFileName write FFileName;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Engine: KLiEngine read FEngine;
    property Parsing: boolean read FParsing write FParsing;
    property MainFunc: KLiFunc read FMainFunc;
    property FirstFunc: KLiFunc read FFirstFunc;
  end;

  { KLiModuleList }

  KLiModuleList = class(KLiObject)
  private
    FEngine: KLiEngine;
    FImporter: KLiModule;
    FModules: TStringList;
    function GetModule(Index: integer): KLiModule;
    function GetCount: integer;
  public
    constructor Create(Engine: KLiEngine);
    destructor Destroy;override;
    function IndexOf(const ModuleName: string): integer;
    function IndexOfModule(AModule: KLiModule): integer;
    function Find(const ModuleName: string): KLiModule;
    function Add(AModule: KLiModule): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure DeleteFunctions;
    function ToVarlist(Engine: KLiEngine): KLiVarList;
    property Count: integer read GetCount;
    property Modules[Index: integer]: KLiModule read GetModule;default;
    property Engine: KLiEngine read FEngine;
    property Importer: KLiModule read FImporter;
  end;

  { KLiError }

  KLiError = class(KLiObject)
  private
    FErrno: integer;
    FName: string;
    FMsg: string;
    FModule: string;
    FModuleFile: string;
    FRow: integer;
    FCol: integer;
    FEngine: KLiEngine;
  protected
    function ErrorModule(func: KLiFunc; expr: PLiExprRec): KLiModule;
    { SyntaxError }
    procedure SymNotFound(Parser: KLiParser);
    procedure TypeNotExists(Parser: KLiParser);
    procedure SymExpected(Parser: KLiParser; const syms: string);
    procedure SymUnexpected(Parser: KLiParser);
    procedure Redeclared(Parser: KLiParser);
    procedure BreakNoLoop(Parser: KLiParser);
    procedure ContinueNoLoop(Parser: KLiParser);
    procedure NeedPureID(Parser: KLiParser);
    procedure ModuleReimport(Parser: KLiParser);
    procedure ModuleNotFound(Parser: KLiParser);
    procedure WrongLibrary(Parser: KLiParser);
    procedure ImportEachOther(Parser: KLiParser; module: KLiModule);
    { SatisfyError }
    procedure ObjectNotExists(func: KLiFunc; expr: PLiExprRec);
    procedure LabelNotExists(func: KLiFunc; expr: PLiExprRec);
    procedure CanNotAsk(func: KLiFunc; expr: PLiExprRec; clss: KLiType);
    procedure FuncNotFound(func: KLiFunc; expr: PLiExprRec);
  public
    constructor Create(AEngine: KLiEngine);
    procedure Clear;
    procedure Write(const Name: string; Errno, Row, Col: integer;
      const Module, Msg, FileName: string);
    procedure Error(const Name: string; Errno, Row, Col: integer;
      const Module, Msg, FileName: string);
    procedure SyntaxErr(Errno, Row, Col: integer;
      const Module, Fmt, FileName: string; const Args: array of const);
    procedure ImportErr(Errno, Row, Col: integer;
      const Module, Fmt, FileName: string; const Args: array of const);
    function ErrorText: string;
    property Errno: integer read FErrno write FErrno;
    property Name: string read FName write FName;
    property Msg: string read FMsg write FMsg;
    property Module: string read FModule write FModule;
    property ModuleFile: string read FModuleFile write FModuleFile;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
  end;

  { KLiEngine }

  KLiNotifyEvent = procedure(Sender: pointer) of object;
  KLiReadBuf = procedure(Sender: TObject; buf: pchar; var Count: integer) of object;

  KLiEngine = class(KLiObject)
  private
    FEngineRec: PLseEngine;      {<--kernel engine stub}
    FMainModule: KLiModule;      {<--main module}
    FMainFunc: KLiFunc;          {<--main function}
    FMainSnap: KLiVarSnap;
    FMainRunner: KLiRunner;      {<--main runner}
    FMainFile: string;           {<--main script file}
    FMainSearchPath: string;     {<--primary search path}
    FMainValues: KLiHashed;
    FTempValues: KLiVarList;
    FError: KLiError;            {<--error object}
    FExitResult: PLseValue;      {<--exit result}
    FExitResultType: string;
    FExitResultText: string;
    FExited: boolean;
    FArguments: TStringList;      {<--argument list}
    FModules: KLiModuleList;     {<--module list}
    FCompiledObjects: KLiList;   {<--compiled objects}
    FReady: boolean;
    FOnReadBuf: KLiReadBuf;
    FOrChain: PLiObjRec;
    FNameSeed: cardinal;         {<--label seed}
    FCGI: TLseObject;            {<--used by KLiCGI}
    FInput: PLseStream;
    FOutput: PLseStream;
    FErrput: PLseStream;
    function GetResultText: string;
    function GetResultType: KLiType;
    procedure SetMainFile(const AValue: string);
    procedure SetMainSearchPath(const AValue: string);
    function GetMainFunc: KLiFunc;
    function GetMainSnap: KLiVarSnap;
    function GetInputStream: PLseStream;
    procedure SetInputStream(const Value: PLseStream);
    function GetOutputStream: PLseStream;
    procedure SetOutputStream(const Value: PLseStream);
    function GetErrputStream: PLseStream;
    procedure SetErrputStream(const Value: PLseStream);
  public
    constructor Create(const AEngineRec: PLseEngine);
    destructor Destroy;override;
    procedure Reset(IncludeVar: boolean);
    procedure Clear;
    procedure PrepareCompile;
    function DoCompile(const Code: string): KLiFunc;
    function Compile(const Code: string): KLiFunc;
    procedure Go;
    function Running: boolean;
    function Terminated: boolean;
    procedure Terminate;
    procedure DumpCodeToStream(stream: TStream; const margin: string);
    procedure SetResult(value: PLseValue);
    procedure GetValue(const Name: string; Value: PLseValue);
    function ReadValue(const Name: string): string;
    procedure AddCompiled(AObject: KLiObject);
    function GetSearchPath: string;
    procedure SetResultTypeText(const RType, RText: string);

    { events }

    procedure BeginExecute;virtual;
    procedure EndExecute;virtual;

    { tryings }

    function TryCompileCode(const code: string): boolean;
    function TryExecuteCode(const code: string): boolean;
    function TryCompileFile(const fname: string): boolean;
    function TryExecuteFile(const fname: string): boolean;
    function TryGo(resetVar: boolean): boolean;

    { Garbage Collection }

    function GarbageCollect: integer;
    function OrEnter(Rec: PLiObjRec): PLiObjRec;
    function OrLeave(Rec: PLiObjRec): PLiObjRec;
    function OrMark(list: TList): integer;
    function OrMarkValue(VD: PLseValue): boolean;
    function OrMarkVarlist(VL: KLiVarList): boolean;
    function OrMarkHashed(HS: KLiHashed): boolean;
    function OrMarkFunc(FC: KLiFunc): boolean;
    function OrIsMarked(Rec: PLiObjRec): boolean;
    procedure OrIncLife(or_list: TList);
    procedure OrDecLife(or_list: TList);
    procedure OrCollect(or_list: TList);

    property EngineRec: PLseEngine read FEngineRec write FEngineRec;
    property Modules: KLiModuleList read FModules;
    property MainRunner: KLiRunner read FMainRunner;
    property MainModule: KLiModule read FMainModule;
    property MainFunc: KLiFunc read GetMainFunc;
    property MainSnap: KLiVarSnap read GetMainSnap;
    property MainFile: string read FMainFile write SetMainFile;
    property MainSearchPath: string read FMainSearchPath write SetMainSearchPath;
    property MainValues: KLiHashed read FMainValues;
    property Error: KLiError read FError;
    property ResultType: KLiType read GetResultType;
    property ResultText: string read GetResultText;
    property ExitResultType: string read FExitResultType;
    property ExitResultText: string read FExitResultText;
    property Arguments: TStringList read FArguments;
    property Ready: boolean read FReady;
    property Exited: boolean read FExited write FExited;
    property CGI: TLseObject read FCGI write FCGI;
    property OnReadBuf: KLiReadBuf read FOnReadBuf write FOnReadBuf;
    property OrChain: PLiObjRec read FOrChain write FOrChain;
    property Input: PLseStream read GetInputStream write SetInputStream;
    property Output: PLseStream read GetOutputStream write SetOutputStream;
    property Errput: PLseStream read GetErrputStream write SetErrputStream;
    property CompiledObjects: KLiList read FCompiledObjects write FCompiledObjects;
  end;

  { KLiVarList }

  KLiVarList = class(KLiObject)
  private
    FEngine: KLiEngine;
    FObjRec: RLiObjRec;
    FItems: TList;
    function GetCount: integer;
    procedure SetCount(ItemCount: integer);
    function GetData(Index: integer): PLseValue;
  public
    constructor Create(AEngine: KLiEngine);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure DeleteLast;
    procedure Press(ItemCount: integer = 1);
    procedure Exchange(Index1, Index2: integer);
    procedure ExchangeLastTwo;
    procedure Move(CurIndex, NewIndex: integer);
    function Insert(Index: integer): PLseValue;
    function Pop: PLseValue;overload;
    function Pop(Index: integer): PLseValue;overload;
    function Add: PLseValue;overload;
    function Add(Value: int64): PLseValue;overload;
    function Add(Value: double): PLseValue;overload;
    function Add(const Value: string): PLseValue;overload;
    function Add(const Value: PLseString): PLseValue;overload;
    function Add(const Value: pointer; Klass: PLseType): PLseValue;overload;
    function Add(const Value: PLseValue): PLseValue;overload;
    function AddDefault(Klass: PLseType): PLseValue;
    function AddFrom(List: KLiVarList; ItemCount: integer = 0): integer;
    function AddSend(VG: PLseVargen): boolean;
    function AddAll(VG: PLseVargen): integer;
    function AddStrings(List: TStrings): integer;
    function AsString: string;
    function Copy(Index, ItemCount: integer): KLiVarList;
    function Left(ItemCount: integer): KLiVarList;
    function Right(ItemCount: integer): KLiVarList;
    property Engine: KLiEngine read FEngine;
    property Count: integer read GetCount write SetCount;
    property Datas[Index: integer]: PLseValue read GetData;default;
  end;

  { KLiVarSnap }

  KLiVarSnap = class(KLiVarList)
  private
    FParams: KLiVarbList;
    FLocals: KLiVarbList;
    FActualParamCount: integer;
    FExpandThis: boolean;
    function GetVarb(index: integer): KLiVarb;
  public
    constructor Create(Func: KLiFunc);
    destructor Destroy;override;
    function HasVarb(varb: KLiVarb): boolean;
    function GetVV(varb: KLiVarb): PLseValue;
    function GetByName(const Name: string; var Varb: KLiVarb): PLseValue;
    function GetByIndex(Index: integer; var Varb: KLiVarb): PLseValue;
    function FindVarb(const Name: string): KLiVarb;
    procedure Prepare;
    procedure ClearValues;
    function GetParamValues(Values: KLiVarList): integer;
    function ParamCount: integer;
    property Varbs[index: integer]: KLiVarb read GetVarb;
    property ActualParamCount: integer read FActualParamCount;
  end;

  { snap }
  
  PLiSnap = ^RLiSnap;
  RLiSnap = packed record
    func  : KLiFunc;
    base  : integer;
    next  : integer;
    values: KLiVarSnap; // Param & Local variable values
    prior : PLiSnap;    // prior FCurrent
    exprec: PLiExprRec; // prior FExprrec
    output: PLseValue;  // result
    outype: KLiType;   // result type
  end;

  { KLiCallStack }

  RLiCallSnap = packed record
    call: PLseParam;
    snap: PLiSnap;
  end;
  PLiCallSnap = ^RLiCallSnap;

  KLiCallStack = class(KLiObject)
  private
    FRunner: KLiRunner;
    FStack: TList;
    FCount: integer;
    procedure AddCallSnap(Call: PLseParam; Snap: PLiSnap);
    function GetMaxCount: integer;
    function GetItem(Index: integer): PLiCallSnap;
  public
    constructor Create(ARunner: KLiRunner);
    destructor Destroy;override;
    procedure Clear;
    procedure Push(Call: PLseParam);overload;
    procedure Push(Snap: PLiSnap);overload;
    procedure Pop;
    function GetCallSnap(Index: integer): KLiVarList;
    property Count: integer read FCount;
    property MaxCount: integer read GetMaxCount;
    property Items[Index: integer]: PLiCallSnap read GetItem;default;
  end;
  
  { KLiRunner }

  KLiRunnerProc = procedure(Runner: KLiRunner);

  KLiRunner = class(KLiObject)
  private
    FEngine: KLiEngine;
    FStack: KLiVarList;
    FCallStack: KLiCallStack;
    FCurrent: PLiSnap;
    FExprrec: PLiExprRec;
    FTerminated: boolean;
    FExcepted: boolean;
    FMatchPatten: RLiMatchPatten;
    function ExecGoonNext: boolean;
  public
    constructor Create(Engine: KLiEngine);
    destructor Destroy;override;
    procedure ErrorRT(const ErrorStr: string);
    procedure Terminate;
    procedure Eval(const Code: string; Output: PLseValue);
    function Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
    function GetString(const ID: string): string;
    function FormatFor(const Fmt: string; Values: KLiVarList): string;
    function GetValue(varb: KLiVarb): PLseValue;
    function ListMatchResult: KLiVarList;
    function HasNext: boolean;
    function MatchPatten: PLiMatchPatten;
    function CurrentModule: KLiModule;
    function CurrentFunc: KLiFunc;
    property Current: PLiSnap read FCurrent;
    property Engine: KLiEngine read FEngine;
    property Stack: KLiVarList read FStack;
    property CallStack: KLiCallStack read FCallStack;
    property Exprrec: PLiExprRec read FExprrec;
    property Excepted: boolean read FExcepted write FExcepted;
    property Terminated: boolean read FTerminated;
  end;

  { KLiSatisfy }
  
  RLiSatisfy = packed record
    s_type: KLiType;
    s_expr: PLiExprRec;
    s_func: KLiFunc;
  end;
  PLiSatisfy = ^RLiSatisfy;
  
  KLiSatisfy = class(KLiObject)
  private
    FItems: TList;
    function GetCount: integer;
    function GetType(Index: integer): KLiType;
    function GetExpr(Index: integer): PLiExprRec;
    function GetFunc(Index: integer): KLiFunc;
    function GetLastType: KLiType;
    function GetLastExpr: PLiExprRec;
    function GetLastFunc: KLiFunc;
  protected
    function NewData(T: KLiType; X: PLiExprRec; F: KLiFunc): PLiSatisfy;
    function GetData(Index: integer): PLiSatisfy;
    function GetLastData: PLiSatisfy;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Press(Count: integer = 1);
    procedure DupLast(N: integer);
    function Add(T: KLiType; X: PLiExprRec; F: KLiFunc): integer;
    property Count: integer read GetCount;
    property Types[Index: integer]: KLiType read GetType;
    property Exprs[Index: integer]: PLiExprRec read GetExpr;
    property Funcs[Index: integer]: KLiFunc read GetFunc;
    property LastType: KLiType read GetLastType;
    property LastExpr: PLiExprRec read GetLastExpr;
    property LastFunc: KLiFunc read GetLastFunc;
  end;

  { KLiHashed }

  KLiHashed = class(KLiHashTable)
  private
    FEngine: KLiEngine;
    FObjRec: RLiObjRec;
  protected
    procedure FreeItem(Item: PLiHashItem);override;
    procedure DoListKeys(const Key: string; Value, Param: pointer);
  public
    constructor Create(AEngine: KLiEngine; Size: cardinal = 1);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    function ForceValue(const Key: string): PLseValue;
    function FindValue(const Key: string): PLseValue;
    function SetValue(const Key: string; Value: PLseValue): PLseValue;
    function SetStr(const Key, Value: string): PLseValue;
    function SetInt64(const Key: string; Value: int64): PLseValue;
    function SetObject(const Key: string; Obj: pointer; Clss: PLseType): PLseValue;
    procedure ListKeys(List: KLiVarList);
  end;

  TInitProc = procedure;
  TExitProc = TInitProc;

procedure InitLyseeKernel;
procedure ExitLyseeKernel;

{-----------------------------------------------------------------------
( F_NAME: __LoadConfig
( 
( F_DESC: load configuration, called once by __SetupLseClasses
( 
( F_ARGS:
( 
( F_TYPE:
(----------------------------------------------------------------------}
procedure __LoadConfig(const ConfigFile: string);

{-----------------------------------------------------------------------
( F_NAME: __ReadConfig
(
( F_DESC: read configure value
(
( F_ARGS: const ID: string - config name
(
( F_TYPE:
(----------------------------------------------------------------------}
function __ReadConfig(const ID: string): string;

{-----------------------------------------------------------------------
( F_NAME: __ExpandValue
(
( F_DESC: expand with configure value
(
( F_ARGS: const S: string - source string
(         E: KLiEngine - a engine
(
( F_TYPE: string - expanded result
(----------------------------------------------------------------------}
function __ExpandValue(const S: string; E: KLiEngine): string;

{-----------------------------------------------------------------------
( F_NAME: __IsReserved
( 
( F_DESC: check if is reserved word
(
( F_ARGS: const ID: string - the word to be tested
(         IncludeKeywords: boolean - if check keywords
( 
( F_TYPE: boolean - true if is reserved
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __IsReserved(const ID: string; IncludeKeywords: boolean): boolean;

{----------------------------------------------------------------------}
{ manage exprrec                                                       }
{----------------------------------------------------------------------}
function __NewExprec: PLiExprRec;
procedure __FreeExprec(R: PLiExprRec);

{----------------------------------------------------------------------}
{ manage value                                                         }
{----------------------------------------------------------------------}
function __NewNil: PLseValue;
function __NewInt64(Value: int64): PLseValue;
function __NewFloat(Value: double): PLseValue;
function __NewStr(const Value: string): PLseValue;
function __NewStrec(const Value: PLseString): PLseValue;
function __NewValue(const Value: PLseValue): PLseValue;
function __NewObject(const Value: pointer; AType: PLseType): PLseValue;
procedure __FreeValue(V: PLseValue);

{-----------------------------------------------------------------------
( F_NAME: __InitParam
(
( F_DESC: initialize parameter record
(
( F_ARGS: Param: PLseParam
(         Runner: KLiRunner
(         Func: KLiFunc
(
( F_TYPE: 
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __InitParam(Param: PLseParam; Runner: KLiRunner; Func: KLiFunc);

{-----------------------------------------------------------------------
( F_NAME: __ExecParam
(
( F_DESC: execute function with current param
(
( F_ARGS: Param: PLseParam
(         Func: KLiFunc
(
( F_TYPE:
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __ExecParam(Param: PLseParam; Func: KLiFunc);

{----------------------------------------------------------------------}
{ read value                                                           }
{----------------------------------------------------------------------}
function __AsInt64(V: PLseValue): int64;
function __AsInteger(V: PLseValue): integer;
function __AsFloat(V: PLseValue): double;
function __AsTime(V: PLseValue): TDateTime;
function __AsString(V: PLseValue): string;
function __AsFileName(V: PLseValue): string;
function __AsStrec(V: PLseValue): PLseString;
function __AsPChar(V: PLseValue): pchar;
function __AsChar(V: PLseValue): char;
function __AsBool(V: PLseValue): boolean;
function __AsObject(V: PLseValue): pointer;overload;
function __AsObject(V: PLseValue; T: KLiType): pointer;overload;
function __AsRunner(Param: PLseParam): KLiRunner;
function __AsEngine(Param: PLseParam): KLiEngine;
function __AsFunc(V: PLseValue): KLiFunc;
function __AsVarlist(V: PLseValue): KLiVarList;
function __AsVargen(Engine: KLiEngine; data: PLseValue): PLseVargen;
function __AsType(V: PLseValue): KLiType;
function __NewVarlist(Engine: KLiEngine): KLiVarList;
function __GetThis(Param: PLseParam; var This): boolean;

{----------------------------------------------------------------------)
(                                                                      )
(                   USER DEFINED FUNCTIONS                             )
(                                                                      )
(----------------------------------------------------------------------}

procedure udc_curry(const Param: PLseParam);cdecl;
procedure udc_const(const Param: PLseParam);cdecl;
procedure udc_oper(const Param: PLseParam);cdecl;
procedure udc_empty(const Param: PLseParam);cdecl;

{-----------------------------------------------------------------------
( F_NAME: __SetTypeValue
( 
( F_DESC: convert to specified data type
(
( F_ARGS: E: KLiEngine
(         V: PLseValue
(         AType: KLiType - new data type
( 
( F_TYPE: PLseValue
(
( EXCEPT: 
(----------------------------------------------------------------------}
function __SetTypeValue(E: KLiEngine; V: PLseValue; AType: KLiType): PLseValue;

{-----------------------------------------------------------------------
( F_NAME: __SetDefaultValue
( 
( F_DESC: clear and set default value
(
( F_ARGS: V: PLseValue
(         AType: KLiType - new data type
( 
( F_TYPE: PLseValue
( 
( EXCEPT: 
(----------------------------------------------------------------------}
function __SetDefaultValue(V: PLseValue; AType: KLiType): PLseValue;

{-----------------------------------------------------------------------
( F_NAME: __SetError
( 
( F_DESC: fill error informations
(
( F_ARGS: Param: PLseParam
(         const ErrorName: string
(         Errno: integer
(         const ErrorMsg: string
( 
( F_TYPE:
( 
( EXCEPT:
(----------------------------------------------------------------------}
procedure __SetError(Param: PLseParam);overload;
procedure __SetError(Param: PLseParam; const ErrorMsg: string);overload;
procedure __SetError(Param: PLseParam; const ErrorFmt: string;
  const Args: array of const);overload;
procedure __SetError(Param: PLseParam; const ErrorName: string;
  Errno: integer; const ErrorMsg: string);overload;

{-----------------------------------------------------------------------
( F_NAME: __SetErrorThis
( 
( F_DESC: set error because this is nil
(
( F_ARGS: Param: PLseParam
( 
( F_TYPE:
( 
( EXCEPT:
(----------------------------------------------------------------------}
procedure __SetErrorThis(Param: PLseParam);

{----------------------------------------------------------------------)
(                                                                      )
(                           mathmetics                                 )
(                                                                      )
(----------------------------------------------------------------------}
procedure __inc(V, Value: PLseValue);                // V  :=     V  +   Value
procedure __dec(V, Value: PLseValue);                // V  :=     V  -   Value
procedure __mul(V, Value: PLseValue);                // V  :=     V  *   Value
procedure __div(V, Value: PLseValue);                // V  :=     V  /   Value
procedure __mod(V, Value: PLseValue; R: KLiRunner);  // V  :=     V  %   Value
procedure __neg(V: PLseValue);                       // V  :=   - V
procedure __xor(V, Value: PLseValue);                // V  :=     V  ^   Value
procedure __and(V, Value: PLseValue);                // V  :=     V  &   Value
procedure __or(V, Value: PLseValue);                 // V  :=     V  |   Value
procedure __shl(V, Value: PLseValue; R: KLiRunner);  // V  :=     V  <<  Value
procedure __shr(V, Value: PLseValue);                // V  :=     V  >>  Value
procedure __not(V: PLseValue);                       // V  :=   ~ V
procedure __logicAnd(V, Value: PLseValue);           // V  :=     V  and Value
procedure __logicOr(V, Value: PLseValue);            // V  :=     V  or  Value
procedure __logicNot(V: PLseValue);                  // V  := not V
procedure __equal(V1, V2: PLseValue);                // V1 :=     V1 ==   V2
procedure __diff(V1, V2: PLseValue);                 // V1 :=     V1 !=   V2
procedure __less(V1, V2: PLseValue);                 // V1 :=     V1 <    V2
procedure __eqless(V1, V2: PLseValue);               // V1 :=     V1 <=   V2
procedure __more(V1, V2: PLseValue);                 // V1 :=     V1 >    V2
procedure __eqmore(V1, V2: PLseValue);               // V1 :=     V1 >=   V2
procedure __abseq(V1, V2: PLseValue);                // V1 :=     V1 ===  V2
procedure __like(V1, V2: PLseValue; R: KLiRunner);   // V1 :=     V1 like V2
procedure __fill(V1, V2: PLseValue; R: KLiRunner);   // V1 :=     V1 <<<  V2
procedure __is(V1, V2: PLseValue);                   // V1 :=     V1 is   V2
procedure __as(V1, V2: PLseValue; E: KLiEngine);     // V1 :=     V1 as   V2

{----------------------------------------------------------------------)
(                                                                      )
(                      mathmetics type test                            )
(                                                                      )
(----------------------------------------------------------------------}
function __type_inc(L, R: KLiType): KLiType;
function __type_dec(L, R: KLiType): KLiType;
function __type_mul(L, R: KLiType): KLiType;
function __type_div(L, R: KLiType): KLiType;
function __type_mod(L, R: KLiType): KLiType;
function __type_neg(L: KLiType): KLiType;
function __type_xor(L, R: KLiType): KLiType;
function __type_and(L, R: KLiType): KLiType;
function __type_or(L, R: KLiType): KLiType;
function __type_shl(L, R: KLiType): KLiType;
function __type_shr(L, R: KLiType): KLiType;
function __type_not(V: KLiType): KLiType;

{----------------------------------------------------------------------)
(                                                                      )
(                         compare                                      )
(                                                                      )
(----------------------------------------------------------------------)
( F_NAME: __compare
( 
( F_DESC: compare two values
( 
( F_ARGS: V1: PLseValue
(         V2: PLseValue
(         Test: KLiCompResults - expected result
( 
( F_TYPE: KLiCompResult
(         PLseValue - true if get expected result
(
( EXCEPT:
(----------------------------------------------------------------------}
type
  KLiCompResult = (crEqual, crLess, crMore, crDiff);
  KLiCompResults = set of KLiCompResult;

function __compare(V1, V2: PLseValue): KLiCompResult;overload;
function __compare(V1, V2: PLseValue; Test: KLiCompResults): boolean;overload;

{-----------------------------------------------------------------------
( F_NAME: __contains
( 
( F_DESC: check if host contains a specified value
( 
( F_ARGS: Host: something
(         Value: PLseValue
( 
( F_TYPE: boolean - true if host contains the value
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __contains(Host: TStringList; Value: PLseValue): boolean;overload;
function __contains(Host: KLiVarList; Value: PLseValue; FindItemVG: boolean): boolean;overload;
function __contains(Host: PLseString; Value: PLseValue): boolean;overload;
function __contains(Host: int64; Value: PLseValue): boolean;overload;
function __contains(Host: KLiHashed; Value: PLseValue): boolean;overload;

{----------------------------------------------------------------------)
(                                                                      )
(             以下函数用于对系统内嵌函数、类型和库进行管理             )
(                                                                      )
(----------------------------------------------------------------------)
( F_NAME: __decodeTypeName
( 
( F_DESC: 解析类型名称
( 
( F_ARGS: const TypeName: string - 目标类名称
(           var Module: string - 所在库名
( 
( F_TYPE: string - 类型名称
(
( EXCEPT:
(----------------------------------------------------------------------}
function __decodeTypeName(const TypeName: string; var Module: string): string;

{-----------------------------------------------------------------------
( F_NAME: __SetupLseClasses
( 
( F_DESC: 向LseClassList注册LSE的内部类
(----------------------------------------------------------------------}
procedure __SetupLseClasses;

{-----------------------------------------------------------------------
( F_NAME: __loadLibrary
(
( F_DESC: 装载指定的动态连接库模块
(
( F_ARGS:  const   name: string - 库名
(         const libfile: string - 文件名
(
( F_TYPE: 注册成功后返回库中类的列表
(----------------------------------------------------------------------}
function __loadLibrary(const name: string; const libfile: string): KLiModule;

{-----------------------------------------------------------------------
( F_NAME: __SetupModule
(
( F_DESC: 建立用户定制模块: CIK_SETUP_MODULE
(
( F_ARGS: const name: string     - 库名
(            MR: PLseModuleRe - 初始化结构
(
( F_TYPE: 注册成功后返回库中类的列表
(
( EXCEPT:
(----------------------------------------------------------------------}
function __SetupModule(const name: string; MR: PLseModule): KLiModule;

{-----------------------------------------------------------------------
(  F_NAME: __searchModule
(
(  F_DESC: search Lysee module
(
(  F_ARGS: var Name: string - module name and take back module file name
(          const SearchPath: string
(          var DLL: boolean - true if is DLL
(
(  F_TYPE: boolean - true if find a module
(
(  EXCEPT:
(----------------------------------------------------------------------}
function __searchModule(var Name: string;
  const SearchPath: string; var DLL: boolean): boolean;

{-----------------------------------------------------------------------
( F_NAME: __runner_XXXX
(
( F_DESC: execute command syXXXX in a runner
(
( F_ARGS: Sender: KLiRunner - the executing runner
(
( F_TYPE:
(
( EXCEPT:
(----------------------------------------------------------------------}
procedure __runner_error(Sender: KLiRunner);
procedure __runner_press(Sender: KLiRunner);
procedure __runner_ID(Sender: KLiRunner);
procedure __runner_become(Sender: KLiRunner);
procedure __runner_float(Sender: KLiRunner);
procedure __runner_call(Sender: KLiRunner);
procedure __runner_echo(Sender: KLiRunner);
procedure __runner_int(Sender: KLiRunner);
procedure __runner_str(Sender: KLiRunner);
procedure __runner_add(Sender: KLiRunner);
procedure __runner_dec(Sender: KLiRunner);
procedure __runner_mul(Sender: KLiRunner);
procedure __runner_div(Sender: KLiRunner);
procedure __runner_mod(Sender: KLiRunner);
procedure __runner_bnot(Sender: KLiRunner);
procedure __runner_bxor(Sender: KLiRunner);
procedure __runner_bor(Sender: KLiRunner);
procedure __runner_band(Sender: KLiRunner);
procedure __runner_bshl(Sender: KLiRunner);
procedure __runner_bshr(Sender: KLiRunner);
procedure __runner_not(Sender: KLiRunner);
procedure __runner_neg(Sender: KLiRunner);
procedure __runner_eq(Sender: KLiRunner);
procedure __runner_ne(Sender: KLiRunner);
procedure __runner_less(Sender: KLiRunner);
procedure __runner_le(Sender: KLiRunner);
procedure __runner_more(Sender: KLiRunner);
procedure __runner_me(Sender: KLiRunner);
procedure __runner_in(Sender: KLiRunner);
procedure __runner_and(Sender: KLiRunner);
procedure __runner_or(Sender: KLiRunner);
procedure __runner_type(Sender: KLiRunner);
procedure __runner_func(Sender: KLiRunner);
procedure __runner_varlist(Sender: KLiRunner);
procedure __runner_nil(Sender: KLiRunner);
procedure __runner_getenv(Sender: KLiRunner);
procedure __runner_getsv(Sender: KLiRunner);
procedure __runner_setsv(Sender: KLiRunner);
procedure __runner_format(Sender: KLiRunner);
procedure __runner_module(Sender: KLiRunner);
procedure __runner_is(Sender: KLiRunner);
procedure __runner_as(Sender: KLiRunner);
procedure __runner_statement(Sender: KLiRunner);
procedure __runner_vargen(Sender: KLiRunner);
procedure __runner_ask(Sender: KLiRunner);
procedure __runner_try(Sender: KLiRunner);
procedure __runner_return(Sender: KLiRunner);
procedure __runner_jump(Sender: KLiRunner);
procedure __runner_jmpf(Sender: KLiRunner);
procedure __runner_jmpt(Sender: KLiRunner);
procedure __runner_jmpfpop(Sender: KLiRunner);
procedure __runner_jmptpop(Sender: KLiRunner);
procedure __runner_goto(Sender: KLiRunner);
procedure __runner_gototp(Sender: KLiRunner);
procedure __runner_gotofp(Sender: KLiRunner);
procedure __runner_hashed(Sender: KLiRunner);
procedure __runner_RINR(Sender: KLiRunner);
procedure __runner_SETV(Sender: KLiRunner);
procedure __runner_GETV(Sender: KLiRunner);
procedure __runner_like(Sender: KLiRunner);
procedure __runner_getiv(Sender: KLiRunner);
procedure __runner_setiv(Sender: KLiRunner);
procedure __runner_duplast(Sender: KLiRunner);
procedure __runner_fill(Sender: KLiRunner);
procedure __runner_send(Sender: KLiRunner);
procedure __runner_case(Sender: KLiRunner);

const
  ParamRecSize  = sizeof(RLseParam);

  FloatTypes = [LSV_FLOAT];

  SyntaxError  = 'SyntaxError';
  ImportError  = 'ImportError';
  RuntimeError = 'RuntimeError';
  FileNotFound = 'FileNotFound';

  ESYNTAX               = 1000;
    EsSymNotFound       = 'symbol expected but file end';
    EvSymNotFound       = ESYNTAX + 1;
    EsClassNotExists    = 'class "%s" not exists';
    EvClassNotExists    = ESYNTAX + 2;
    EsWrongIDName       = 'wrong identity name "%s"';
    EvWrongIDName       = ESYNTAX + 3;
    EsSymExpected       = 'symbol %s expected, but "%s" found';
    EvSymExpected       = ESYNTAX + 4;
    EsSymUnexpected     = 'unexpected symbol "%s"';
    EvSymUnexpected     = ESYNTAX + 5;
    EsRedeclared        = 'object "%s" already exists';
    EvRedeclared        = ESYNTAX + 6;
    EsTooManyParam      = 'function "%s" has too many parametres';
    EvTooManyParam      = ESYNTAX + 8;
    EsBreakNoLoop       = 'break is not in loop';
    EvBreakNoLoop       = ESYNTAX + 9;
    EsContinueNoLoop    = 'continue is not in loop';
    EvContinueNoLoop    = ESYNTAX + 10;
    EsThrowNothing      = 'nothing to throw outside catch block';
    EvThrowNothing      = ESYNTAX + 11;
    EsWrongException    = 'invalid exception name "%s"';
    EvWrongException    = ESYNTAX + 12;
    EsCatchNotFound     = 'catch not found';
    EvCatchNotFound     = ESYNTAX + 13;
    EsObjectNotExists   = 'object "%s" not exists';
    EvObjectNotExists   = ESYNTAX + 14;
    EsCanNotCast        = 'invalide casting from %s to %s';
    EvCanNotCast        = ESYNTAX + 15;
    EsFuncNotFound      = 'function "%s" not found';
    EvFuncNotFound      = ESYNTAX + 17;
    EsUnknownOper       = 'unknown operator "%s"';
    EvUnknownOper       = ESYNTAX + 18;
    EsNotImplement      = 'class "%s" does not implement "%s" interface';
    EvNotImplement      = ESYNTAX + 22;
    EsImportEachOther   = 'module "%s" and "%s" imports each other';
    EvImportEachOther   = ESYNTAX + 23;
    EsNeedPureID        = '%s is not a pure identity';
    EvNeedPureID        = ESYNTAX + 28;
    EsLabelNotExists    = 'label "%s" not exists';
    EvLabelNotExists    = ESYNTAX + 29;
    EsCanNotAsk         = 'class "%s" can not be asked';
    EvCanNotAsk         = ESYNTAX + 30;
    EsResetMethod       = 'method %s can not be reset';
    EvResetMethod       = ESYNTAX + 31;
    EsLocalNotFound     = 'local variable not found: %s';
    EvLocalNotFound     = ESYNTAX + 34;
  EIMPORT               = 1200;
    EsWrongModuleName   = 'wrong module name "%s"';
    EvWrongModuleName   = EIMPORT + 1;
    EsModuleReimport    = 'module "%s" reimported from another file';
    EvModuleReimport    = EIMPORT + 2;
    EsModuleNotFound    = 'module "%s" not found';
    EvModuleNotFound    = EIMPORT + 3;
    EsWrongLibrary      = 'library "%s" is invalid';
    EvWrongLibrary      = EIMPORT + 4;
    EsFileNotFound      = 'file "%s" not found';
    EvFileNotFound      = EIMPORT + 5;
  ERUNTIME              = 1250;
    EsRuntimeError      = 'unknown runtime error';
    EvRuntimeError      = ERUNTIME + 0;
    EsInvalidInvoke     = 'invalid invoke to %s';
    EvInvalidInvoke     = ERUNTIME + 1;
    EsUserTerminate     = 'terminated by user';
    EvUserTerminate     = ERUNTIME + 2;
    EsFuncNotSpecify    = 'function not specified';
    EvFuncNotSpecify    = ERUNTIME + 3;
    EsClassNotSpecify   = 'class not specified';
    EvClassNotSpecify   = ERUNTIME + 4;

var
  sys_kernel       : string;      {<--kernel file name}
  sys_version      : string;      {<--kernel version}
  sys_knpath       : string;      {<--kernel file path}
  sys_kndir        : string;      {<--kernel file directory}
  sys_home_path    : string;      {<--home path}
  sys_tmpath       : string;      {<--temporary path}
  sys_search_path  : string;      {<--module search path}
  sys_confile      : string;      {<--config file name}
  sys_mimefile     : string;      {<--MIME file name}
  sys_mimes        : TStringList; {<--MIME list}
  sys_configures   : TStringList; {<--configure value list}
  sys_program      : string;      {<--program file name}
  sys_process_ID   : string;      {<--process ID}
  sys_libraries    : TStringList; {<--kernel & library module list}
  sys_type_list    : RLseKernelTypeList;
  sys_module       : KLiModule;   {<--builtin [sys] module}
  cgi_module       : KLiModule;   {<--builtin [cgi] module}
  sys_spinlock     : TLseLock;    {<--kernel's spinlock}
  sys_oper_inc     : KLiFunc;     {<--sys::+}
  sys_nil          : RLseValue;   {<--default empty data}
  sys_LB           : string = sLineBreak;
  sys_runner_procs : array[KLiSymbol] of KLiRunnerProc;

  { KT: KERNEL TYPE }

  KT_VOID, KT_STRING, KT_INT, KT_FLOAT,
  KT_VARIANT, KT_CLASS, KT_MODULE, KT_FUNC,
  KT_VARIABLE, KT_ERROR, KT_STREAM, KT_VARLIST,
  KT_HASHED, KT_VARGEN, KT_VARSNAP: KLiType;

  { KR: KERNEL TYPE RECORD }

  KR_VOID, KR_STRING, KR_INT, KR_FLOAT, 
  KR_VARIANT, KR_CLASS, KR_MODULE, KR_FUNC,
  KR_VARIABLE, KR_ERROR, KR_STREAM, KR_VARLIST,
  KR_HASHED, KR_VARGEN, KR_VARSNAP: PLseType;

procedure lock_kernel;
procedure unlock_kernel;

procedure lock_engine(engine: KLiEngine);
procedure unlock_engine(engine: KLiEngine);

implementation

uses
  Math, DateUtils, lse_export, lse_api, lse_cgi;

procedure lock_kernel;
begin
  sys_spinlock.Enter;
end;

procedure unlock_kernel;
begin
  sys_spinlock.Leave;
end;

procedure lock_engine(engine: KLiEngine);
begin

end;

procedure unlock_engine(engine: KLiEngine);
begin

end;

procedure __LoadConfig(const ConfigFile: string);

  procedure Load(const FileName: string);
  var
    list: TStringList;
    index: integer;
    ID, value: string;
  begin
    sys_confile := lse_expand_fname(FileName);
    sys_configures.Clear;
    if FileExists(sys_confile) then
    begin
      list := TStringList.Create;
      try
        list.LoadFromFile(sys_confile);
        for index := 0 to list.Count - 1 do
          if __parseConfig(list[index], ID, value) then
            sys_configures.Values[ID] := __ExpandValue(value, nil);
      finally
        list.Free;
      end;
    end;
  end;

  function ReadCex(const Key, Value: string): string;
  var
    index: integer;
  begin
    if sys_configures <> nil then
    begin
      index := sys_configures.IndexOfName(Key);
      if index >= 0 then
      begin
        Result := __ExpandValue(Copy(sys_configures[index], Length(Key) + 2, MaxInt), nil);
        Exit;
      end;
    end;
    Result := __ExpandValue(Value, nil);
  end;

begin
  try
    if sys_process_ID = '' then
    begin
      sys_process_ID := __genid;
      sys_program := __programFile;
      sys_kernel := __libraryFile;
      {$IFDEF WINDOWS}
      sys_home_path := __fullPath(lse_getenv('HOMEDRIVER') + lse_getenv('HOMEPATH'));
      {$ELSE}
      sys_home_path := __fullPath(lse_getenv('HOME'));
      {$ENDIF}
      sys_configures := TStringList.Create;
      sys_configures.CaseSensitive := true;
    end;

    sys_knpath := ExtractFilePath(sys_kernel);
    sys_kndir := ExcludeTrailingPathDelimiter(sys_knpath);
    sys_confile := sys_knpath + LSE_CONFILE;
    sys_mimefile := sys_knpath + LSE_MIMEFILE;

    if ConfigFile = '' then
      Load(sys_confile) else
      Load(ConfigFile);

    sys_tmpath := __fullPath(ReadCex('lse_tmpath', LSE_TEMP_PATH));
    sys_search_path := lse_veryPD(ReadCex('lse_search', LSE_SEARCH_PATH));
    sys_mimefile := lse_expand_fname(ReadCex('lse_mimefile', sys_mimefile));

    FreeAndNil(sys_mimes);

    {$IFDEF WINDOWS}
    ForceDirectories(sys_tmpath);
    {$ENDIF}
  except
    { ignore exceptions }
  end;
end;

function __ReadConfig(const ID: string): string;
var
  index: integer;
begin
  if ID = 'confile'  then Result := sys_confile else
  if ID = 'mimefile' then Result := sys_mimefile else
  if ID = 'kernel'   then Result := sys_kernel else
  if ID = 'knpath'   then Result := sys_knpath else
  if ID = 'kndir'    then Result := sys_kndir else
  if ID = 'home'     then Result := sys_home_path else
  if ID = 'program'  then Result := sys_program else
  if ID = 'search'   then Result := sys_search_path else
  if ID = 'tmpath'   then Result := sys_tmpath else
  if ID = 'keywords' then Result := ReservedWords else
  begin
    if sys_configures <> nil then
    begin
      index := sys_configures.IndexOfName(ID);
      if index >= 0 then
      begin
        Result := Copy(sys_configures[index], Length(ID) + 2, MaxInt);
        Exit;
      end;
    end;
    Result := lse_getenv(ID);
  end;
end;

function __ExpandValue(const S: string; E: KLiEngine): string;
var
  base, next: pchar;
  name: string;
begin
  Result := '';
  next := pchar(S);
  if next <> nil then while next^ <> #0 do
  begin
    if next^ = '$' then
    begin
      Inc(next);
      if next^ = '{' then
      begin
        Inc(next);
        base := next;
        while not (next^ in [#0, '}']) do Inc(next);
        if next^ = '}' then
        begin
          SetString(name, base, next - base);
          if E <> nil then
            Result := Result + E.ReadValue(name) else
            Result := Result + __ReadConfig(name);
          Inc(next);
        end;
      end
      else
      begin
        Result := Result + '$';
        if next^ = '$' then Inc(next);
      end;
    end
    else
    begin
      Result := Result + next^;
      Inc(next);
    end;
  end;
end;

function __IsReserved(const ID: string; IncludeKeywords: boolean): boolean;
var
  index: TLseValue;
begin
  for index := LSV_VOID to LSV_OBJECT do
    if ID = lse_vtype_names[index] then
    begin
      Result := true;
      Exit;
    end;
  Result := IncludeKeywords and
    (StrToSym(ID, syError) <> syError);
end;

function __NewExprec: PLiExprRec;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiExprRec));
end;

procedure __FreeExprec(R: PLiExprRec);
begin
  if R <> nil then
  begin
    R^.Name := '';
    if R^.Sym = syStr then
      lse_strec_declife(R^.VStr);
    lse_mem_free(R, sizeof(RLiExprRec));
  end;
end;

function __NewNil: PLseValue;
begin
  Result := lse_mem_alloc_zero(sizeof(RLseValue));
end;

function __NewInt64(Value: int64): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_INT;
  Result^.VInteger := Value; 
end;

function __NewFloat(Value: double): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_FLOAT;
  Result^.VFloat := Value;
end;

function __NewStr(const Value: string): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_STRING;
  Result^.VObject := lse_strec_alloc(Value);
  lse_strec_inclife(Result^.VObject);
end;

function __NewStrec(const Value: PLseString): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_STRING;
  Result^.VObject := Value;
  lse_strec_inclife(Value);
end;

function __NewValue(const Value: PLseValue): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Move(Value^, Result^, sizeof(RLseValue));
  lse_addref(Result);
end;

function __NewObject(const Value: pointer; AType: PLseType): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := AType;
  Result^.VObject := Value;
  AType^.cr_addref(Value);
end;

procedure __FreeValue(V: PLseValue);
begin
  if V <> nil then
  begin
    lse_set_nil(V);
    lse_mem_free(V, sizeof(RLseValue));
  end;
end;

procedure __InitParam(Param: PLseParam; Runner: KLiRunner; Func: KLiFunc);
begin
  Param^.p_count := 0;
  Param^.p_result := nil;
  Param^.p_func := Func;
  Param^.p_runner := Runner;
  Param^.p_exprec := Runner.FExprrec;
end;

procedure __ExecParam(Param: PLseParam; Func: KLiFunc);
begin
  TLseFuncCall(Func.FProc)(Param);
end;

function __int64_void(V: PLseValue): int64;
begin
  Result := 0;
end;

function __int64_string(V: PLseValue): int64;
begin
  Result := __parseInt(lse_strec_data(PLseString(V^.VObject)));
end;

function __int64_int64(V: PLseValue): int64;
begin
  Result := V^.VInteger;
end;

function __int64_float(V: PLseValue): int64;
begin
  Result := Trunc(V^.VFloat);
end;

function __int64_variant(V: PLseValue): int64;
begin
  Result := 0;
end;

function __int64_object(V: PLseValue): int64;
begin
  Result := integer(V^.VObject);
end;

function __AsInt64(V: PLseValue): int64;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): int64 = (
    {$IFDEF FPC}@{$ENDIF}__int64_void,
    {$IFDEF FPC}@{$ENDIF}__int64_string,
    {$IFDEF FPC}@{$ENDIF}__int64_int64,
    {$IFDEF FPC}@{$ENDIF}__int64_float,
    {$IFDEF FPC}@{$ENDIF}__int64_variant,
    {$IFDEF FPC}@{$ENDIF}__int64_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsInteger(V: PLseValue): integer;
begin
  Result := __AsInt64(V);
end;

function __float_void(V: PLseValue): double;
begin
  Result := 0;
end;

function __float_string(V: PLseValue): double;
begin
  Result := __parseExt(lse_strec_data(V^.VObject));
end;

function __float_int64(V: PLseValue): double;
begin
  Result := V^.VInteger;
end;

function __float_float(V: PLseValue): double;
begin
  Result := V^.VFloat;
end;

function __float_variant(V: PLseValue): double;
begin
  Result := 0;
end;

function __float_object(V: PLseValue): double;
begin
  Result := 0;
end;

function __AsFloat(V: PLseValue): double;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): double = (
    {$IFDEF FPC}@{$ENDIF}__float_void,
    {$IFDEF FPC}@{$ENDIF}__float_string,
    {$IFDEF FPC}@{$ENDIF}__float_int64,
    {$IFDEF FPC}@{$ENDIF}__float_float,
    {$IFDEF FPC}@{$ENDIF}__float_variant,
    {$IFDEF FPC}@{$ENDIF}__float_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsTime(V: PLseValue): TDateTime;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := lse_decode_GMT(__AsString(V));
    LSV_FLOAT : Result := V^.VFloat;
    LSV_INT   : Result := UnixToDateTime(V^.VInteger);
           else Result := 0;
  end;
end;

function __string_void(V: PLseValue): string;
begin
  Result := '';
end;

function __string_string(V: PLseValue): string;
begin
  Result := lse_strec_string(V^.VObject);
end;

function __string_int64(V: PLseValue): string;
begin
  Result := IntToStr(V^.VInteger);
end;

function __string_float(V: PLseValue): string;
begin
  Result := FloatToStr(V^.VFloat);
end;

function __string_variant(V: PLseValue): string;
begin
  Result := '';
end;

function __string_object(V: PLseValue): string;
begin
  Result := __AsType(V).ObjectToString(V^.VObject);
end;

function __AsString(V: PLseValue): string;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): string = (
    {$IFDEF FPC}@{$ENDIF}__string_void,
    {$IFDEF FPC}@{$ENDIF}__string_string,
    {$IFDEF FPC}@{$ENDIF}__string_int64,
    {$IFDEF FPC}@{$ENDIF}__string_float,
    {$IFDEF FPC}@{$ENDIF}__string_variant,
    {$IFDEF FPC}@{$ENDIF}__string_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsFileName(V: PLseValue): string;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := lse_veryPD(Trim(lse_strec_data(V^.VObject))) else
    Result := '';
end;

function __AsStrec(V: PLseValue): PLseString;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := V^.VObject else
    Result := nil;
end;

function __AsPChar(V: PLseValue): pchar;
begin
  Result := lse_strec_data(__AsStrec(V));
end;

function __char_void(V: PLseValue): char;
begin
  Result := #0;
end;

function __char_string(V: PLseValue): char;
var
  data: pchar;
begin
  data := lse_strec_data(V^.VObject);
  if data <> nil then
    Result := data^ else
    Result := #0;
end;

function __char_int64(V: PLseValue): char;
begin
  Result := char(V^.VInteger);
end;

function __char_float(V: PLseValue): char;
begin
  Result := char(Trunc(V^.VFloat));
end;

function __char_variant(V: PLseValue): char;
begin
  Result := #0;
end;

function __char_object(V: PLseValue): char;
begin
  Result := #0;
end;

function __AsChar(V: PLseValue): char;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): char = (
    {$IFDEF FPC}@{$ENDIF}__char_void,
    {$IFDEF FPC}@{$ENDIF}__char_string,
    {$IFDEF FPC}@{$ENDIF}__char_int64,
    {$IFDEF FPC}@{$ENDIF}__char_float,
    {$IFDEF FPC}@{$ENDIF}__char_variant,
    {$IFDEF FPC}@{$ENDIF}__char_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __bool_void(V: PLseValue): boolean;
begin
  Result := false;
end;

function __bool_string(V: PLseValue): boolean;
begin
  Result := (lse_strec_length(V^.VObject) > 0);
end;

function __bool_int64(V: PLseValue): boolean;
begin
  Result := (V^.VInteger <> 0);
end;

function __bool_float(V: PLseValue): boolean;
begin
  Result := not IsZero(V^.VFloat);
end;

function __bool_variant(V: PLseValue): boolean;
begin
  Result := false;
end;

function __bool_object(V: PLseValue): boolean;
begin
  Result := (V^.VObject <> nil);
end;

function __AsBool(V: PLseValue): boolean;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): boolean = (
    {$IFDEF FPC}@{$ENDIF}__bool_void,
    {$IFDEF FPC}@{$ENDIF}__bool_string,
    {$IFDEF FPC}@{$ENDIF}__bool_int64,
    {$IFDEF FPC}@{$ENDIF}__bool_float,
    {$IFDEF FPC}@{$ENDIF}__bool_variant,
    {$IFDEF FPC}@{$ENDIF}__bool_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsObject(V: PLseValue): pointer;
begin
  if lse_vtype(V) = LSV_OBJECT then
    Result := V^.VObject else
    Result := nil;
end;

function __AsObject(V: PLseValue; T: KLiType): pointer;
var
  R: PLseType;
begin
  R := lse_type(V);
  if (R^.cr_type = LSV_OBJECT) and (KLiType(R^.cr_class) = T) then
    Result := V^.VObject else
    Result := nil;
end;

function __AsRunner(Param: PLseParam): KLiRunner;
begin
  Result := KLiRunner(Param^.p_runner);
end;

function __AsEngine(Param: PLseParam): KLiEngine;
begin
  Result := KLiRunner(Param^.p_runner).FEngine;
end;

function __AsFunc(V: PLseValue): KLiFunc;
begin
  Result := KLiFunc(__AsObject(V));
end;

function __AsVarlist(V: PLseValue): KLiVarList;
begin
  Result := KLiVarList(__AsObject(V));
end;

function __AsVargen(Engine: KLiEngine; data: PLseValue): PLseVargen;
var
  crec: PLseType;
  clss: KLiType;
  varg: PLseVargen;
  list: pointer;
begin
  varg := nil;
  crec := lse_type(data);
  clss := KLiType(crec^.cr_class);
  if clss = KT_VARGEN then
    varg := PLseVargen(data^.VObject) else
  if clss = KT_STRING then
    varg := cvgr_string(data^.VObject, Engine) else
  if clss = KT_INT then
    varg := cvgr_upto(0, data^.VInteger - 1, 1, Engine) else
  if clss = KT_FLOAT then
    varg := cvgr_upto(0, Trunc(data^.VFloat) - 1, 1, Engine) else
  if crec^.cr_type = LSV_OBJECT then
  begin
    list := data^.VObject;
    if list <> nil then
      if Assigned(crec^.cr_vargen) then
        varg := crec^.cr_vargen(list, Engine) else
        varg := lse_vargen_none;
  end;
  if varg = nil then
    Result := lse_vargen_none else
    Result := varg;
end;

function __AsType(V: PLseValue): KLiType;
begin
  Result := KLiType(lse_type(V)^.cr_class);
end;

function __NewVarlist(Engine: KLiEngine): KLiVarList;
begin
  Result := KLiVarList.Create(Engine);
end;

function __GetThis(Param: PLseParam; var This): boolean;
var
  this_obj: pointer;
begin
  this_obj := __AsObject(Param^.p_param[0]);
  Result := (this_obj <> nil);
  if Result then
    pointer(This) := this_obj else
    __SetErrorThis(Param);
end;

{----------------------------------------------------------------------)
(                                                                      )
(                   以下函数用于支持用户自定义类型                     )
(                                                                      )
(----------------------------------------------------------------------}

procedure udc_curry(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
  func: KLiFunc_curry;
  X, N: integer;
begin
  rnnr := KLiRunner(Param^.p_runner);
  func := KLiFunc_curry(Param^.p_func);
  N := func.GetCurryCount;
  for X := 0 to N - 1 do
    rnnr.FStack.Add(func.GetCurryData(X));
  for X := 0 to Param^.p_count - 1 do
    rnnr.FStack.Add(Param^.p_param[X]);
  rnnr.Goon(func.FCurryFunc, N + Param^.p_count, Param^.p_result);
end;
         
procedure udc_const(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  name: string;
  data: PLseValue;
begin
  func := KLiFunc(Param^.p_func);
  name := Format('@%p', [pointer(func)]);
  data := __AsEngine(Param).FMainValues.FindValue(name);
  if data <> nil then
    lse_set_value(Param^.p_result, data) else
    lse_clear_value(Param^.p_result);
end;

procedure udc_oper(const Param: PLseParam);cdecl;
var
  func: KLiFunc_operator;
  rnnr: KLiRunner;
  L, R: PLseValue;
begin
  L := Param^.p_result;
  lse_set_value(L, Param^.p_param[0]);
  R := Param^.p_param[1];
  rnnr := KLiRunner(Param^.p_runner);
  func := KLiFunc_operator(Param^.p_func);
  case func.FOper of
    syMul   : __mul(L, R);
    syDiv   : __div(L, R);
    syMod   : __mod(L, R, rnnr);
    syAdd   : __inc(L, R);
    syDec   : __dec(L, R);
    syBXor  : __xor(L, R);
    syBAnd  : __and(L, R);
    syBOr   : __or(L, R);
    syBShl  : __shl(L, R, rnnr);
    syBShr  : __shr(L, R);
    syFill  : __fill(L, R, rnnr);
    syEQ    : __equal(L, R);
    syNE    : __diff(L, R);
    syLess  : __less(L, R);
    syLE    : __eqless(L, R);
    syMore  : __more(L, R);
    syME    : __eqmore(L, R);
    syIs    : __is(L, R);
    syAs    : __as(L, R, rnnr.FEngine);
    syLike  : __like(L, R, rnnr);
    syAnd   : __logicAnd(L, R);
    syOr    : __logicOr(L, R);
  end;
end;

procedure udc_empty(const Param: PLseParam);cdecl;
begin
  { do nothing }
end;

function __SetTypeValue(E: KLiEngine; V: PLseValue; AType: KLiType): PLseValue;
var
  vtype: KLiType;
  vgrec: PLseVargen;
begin
  Result := V;
  if AType = KT_VARIANT then Exit;
  
  if AType = KT_VOID then
  begin
    lse_clear_value(V);
    Exit;
  end;
  
  if AType = KT_VARGEN then
  begin
    lse_set_vargen(V, lse_vargen_ensure(__AsVargen(E, V)));
    Exit;
  end;

  vtype := __AsType(V);
  
  if AType = KT_CLASS then
  begin
    if vtype <> KT_CLASS then
      vtype.SaveTo(V);
    Exit;
  end;

  if vtype = KT_VARGEN then
  begin
    vgrec := PLseVargen(V^.VObject);
    if vgrec <> nil then
      lse_vargen_send(vgrec, V) else
      lse_clear_value(V);
    vtype := __AsType(V);
  end;

  if vtype = AType then Exit;

  if AType = KT_STRING then lse_set_string(V, __AsString(V)) else
  if AType = KT_INT    then lse_set_int64(V, __AsInt64(V)) else
  if AType = KT_FLOAT  then lse_set_float(V, __AsFloat(V)) else
  if vtype = KT_STRING then
    lse_set_object(V, AType.TypeRec, AType.StrecToObject(V^.VObject, E)) else
    lse_set_object(V, AType.TypeRec, nil);
end;

function __SetDefaultValue(V: PLseValue; AType: KLiType): PLseValue;
begin
  case AType.DataType of
    LSV_VOID   : lse_clear_value(V);
    LSV_STRING : lse_set_string(V, '');
    LSV_INT    : lse_set_int64(V, 0);
    LSV_FLOAT  : lse_set_float(V, 0);
    LSV_VARIANT: lse_clear_value(V);
    LSV_OBJECT : lse_set_object(V, AType.TypeRec, nil);
  end;
  Result := V;
end;

procedure __SetError(Param: PLseParam);overload;
begin
  __SetError(Param, '', 0, '');
end;

procedure __SetError(Param: PLseParam; const ErrorMsg: string);
begin
  __SetError(Param, '', 0, ErrorMsg);
end;

procedure __SetError(Param: PLseParam; const ErrorFmt: string;
  const Args: array of const);
begin
  __SetError(Param, '', 0, Format(ErrorFmt, Args));
end;

procedure __SetError(Param: PLseParam; const ErrorName: string;
  Errno: integer; const ErrorMsg: string);
var
  func: KLiFunc;
  module: KLiModule;
  runner: KLiRunner;
  errid, error: string;
begin
  runner := KLiRunner(Param^.p_runner);
  if not runner.FExcepted then
  begin
    func := KLiFunc(Param^.p_func);
    errid := Trim(ErrorName);
    if not __IsIDStr(pchar(errid)) then
      errid := func.FModule.Name + 'Error';
    if Errno = 0 then
      Errno := ERUNTIME;
    error := Trim(ErrorMsg);
    if error = '' then
      error := lse_exception_str;
    if error = '' then
      error := func.Name + '() - ' + errid else
      error := func.Name + '() - ' + error;
    if PLiExprRec(Param^.p_exprec)^.Pos.module <> nil then
      module := KLiModule(PLiExprRec(Param^.p_exprec)^.Pos.module) else
      module := runner.CurrentFunc.FModule;
    runner.Engine.Error.write(errid, Errno,
      PLiExprRec(Param^.p_exprec)^.Pos.row,
      PLiExprRec(Param^.p_exprec)^.Pos.col,
      module.Name, error, module.FileName);
    runner.FExcepted := true;
  end;
end;

procedure __SetErrorThis(Param: PLseParam);
begin
  __SetError(Param, 'this is nil');
end;

procedure __inc(V, Value: PLseValue);

  procedure on_string;
  var
    R: TLseValue;
  begin
    R := lse_vtype(Value);
    if R <> LSV_VOID then
    begin
      if R = LSV_STRING then
        lse_set_string(V, lse_strec_cat(V^.VObject, Value^.VObject)) else
        lse_set_string(V, lse_strec_cat(V^.VObject, __AsString(Value)));
    end
    else lse_clear_string(V);
  end;

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(Value);
    if R = LSV_FLOAT then
      lse_set_float(V, __AsFloat(V) + __AsFloat(Value)) else
    if R = LSV_INT then
      lse_set_int64(V, V^.VInteger + __AsInt64(Value)) else
    if R = LSV_STRING then
      lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VObject)) else
      lse_init_value(V);
  end;

  procedure on_float;
  var
    R: TLseValue;
  begin
    R := lse_vtype(Value);
    if R in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V, V^.VFloat + __AsFloat(Value)) else
    if R = LSV_STRING then
      lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VObject)) else
      lse_init_value(V);
  end;
  
begin
  case lse_vtype(V) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __dec(V, Value: PLseValue);

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(Value);
    if R = LSV_FLOAT then
      lse_set_float(V, V^.VInteger - __AsFloat(Value)) else
    if R = LSV_INT then
      lse_set_int64(V, V^.VInteger - __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V, V^.VFloat - __AsFloat(Value)) else
      lse_init_value(V);
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __mul(V, Value: PLseValue);

  procedure on_string;
  var
    S, T: PLseString;
    B, F: pchar;
    L, N: integer;
  begin
    if lse_vtype(Value) = LSV_INT then
    begin
      S := V^.VObject;
      L := lse_strec_length(S);
      N := Value^.VInteger;
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
        lse_set_string(V, T);
      end
      else lse_set_string(V, '');
    end
    else lse_clear_string(V);
  end;

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(Value);
    if R = LSV_FLOAT then
      lse_set_float(V, V^.VInteger * __AsFloat(Value)) else
    if R = LSV_INT then
      lse_set_int64(V, V^.VInteger * __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V, V^.VFloat * __AsFloat(Value)) else
      lse_init_value(V);
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __div(V, Value: PLseValue);

  procedure on_int;
  var
    F: double;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
    begin
      F := __AsFloat(Value);
      if IsZero(F) then
        lse_set_float(V, 0) else
        lse_set_float(V, V^.VInteger / F);
    end
    else lse_init_value(V);
  end;

  procedure on_float;
  var
    F: double;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
    begin
      F := __AsFloat(Value);
      if IsZero(F) then
        lse_set_float(V, 0) else
        lse_set_float(V, V^.VFloat / F);
    end
    else lse_init_value(V);
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __mod(V, Value: PLseValue; R: KLiRunner);

  procedure on_string;
  var
    L: KLiVarlist;
    S: string;
  begin
    if KLiType(Value^.vtype^.cr_class) = KT_VARLIST then
    begin
      L := __AsVarlist(Value);
      S := R.FormatFor(__AsString(V), L);
    end
    else
    begin
      L := __NewVarlist(R.FEngine);
      try
        L.Add(Value);
        S := R.FormatFor(__AsString(V), L);
      finally
        L.Free;
      end;
    end;
    lse_strec_release(V^.VObject);
    V^.VObject := lse_strec_alloc(S);
    lse_strec_addref(V^.VObject);
  end;

  procedure on_int;
  var
    F: int64;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
    begin
      F := __AsInt64(Value);
      if F = 0 then
        lse_set_int64(V, 0) else
        lse_set_int64(V, V^.VInteger mod F);
    end
    else lse_init_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __neg(V: PLseValue);
begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : V^.VInteger := - V^.VInteger;
    LSV_FLOAT  : V^.VFloat := - V^.VFloat;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __xor(V, Value: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V, V^.VInteger xor __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __and(V, Value: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V, V^.VInteger and __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __or(V, Value: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V, V^.VInteger or __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __shl(V, Value: PLseValue; R: KLiRunner);

  procedure on_int;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V, V^.VInteger shl __AsInt64(Value)) else
      lse_init_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

  procedure on_object;
  begin
    if V^.VObject <> nil then
      if Assigned(V^.vtype^.cr_add) then
        V^.vtype^.cr_add(V^.VObject, Value, R.FEngine);
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : on_object;
  end;
end;

procedure __shr(V, Value: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(Value) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V, V^.VInteger shr __AsInt64(Value)) else
      lse_clear_value(V);
  end;

  procedure on_float;
  begin
    V^.vtype := KR_INT;
    V^.VInteger := Trunc(V^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V) of
    LSV_STRING : lse_clear_string(V);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V);
  end;
end;

procedure __not(V: PLseValue);
begin
  if lse_vtype(V) in [LSV_FLOAT, LSV_INT] then
    lse_set_int64(V, not __AsInt64(V)) else
    lse_clear_value(V);
end;

procedure __logicAnd(V, Value: PLseValue);
begin
  if __AsBool(V) and not __AsBool(Value) then
    lse_set_value(V, Value);
end;

procedure __logicOr(V, Value: PLseValue);
begin
  if not __AsBool(V) and __AsBool(Value) then
    lse_set_value(V, Value);
end;

procedure __logicNot(V: PLseValue);
begin
  lse_set_bool(V, not __AsBool(V));
end;

procedure __equal(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crEqual]));
end;

procedure __diff(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crLess, crMore, crDiff]));
end;

procedure __less(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crLess]));
end;

procedure __eqless(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crLess, crEqual]));
end;

procedure __more(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crMore]));
end;

procedure __eqmore(V1, V2: PLseValue);
begin
  lse_set_bool(V1, __compare(V1, V2, [crMore, crEqual]));
end;

procedure __abseq(V1, V2: PLseValue);
var
  clss: KLiType;
  same: boolean;
begin
  same := false;
  clss := __AsType(V1);
  if clss = __AsType(V2) then
    case clss.TypeRec^.cr_type of
      LSV_VOID   : same := true;
      LSV_STRING : same := (V1^.VObject = V2^.VObject)
                        or __bufSame(lse_strec_data(V1^.VObject),
                                     lse_strec_length(V1^.VObject),
                                     lse_strec_data(V2^.VObject),
                                     lse_strec_length(V2^.VObject),
                                     false);
      LSV_INT    : same := (V1^.VInteger = V2^.VInteger);
      LSV_FLOAT  : same := IsZero(V1^.VFloat - V2^.VFloat);
      LSV_VARIANT: same := true;
      LSV_OBJECT : same := (V1^.VObject = V2^.VObject);
    end;
  lse_set_bool(V1, same);
end;

procedure __like(V1, V2: PLseValue; R: KLiRunner);
begin
  lse_set_bool(V1, init_patten(@R.FMatchPatten, V2) and
                   exec_patten(@R.FMatchPatten, V1));
end;

procedure __fill(V1, V2: PLseValue; R: KLiRunner);
var
  K: PLseType;
  G: PLseVargen;
begin
  K := lse_type(V1);
  if Assigned(K^.cr_add) and (V1^.VObject <> nil) then
  begin
    G := __AsVargen(R.FEngine, V2);
    lse_vargen_addref(G);
    try
      while lse_vargen_send(G, V2) do
        K^.cr_add(V1^.VObject, V2, R.FEngine);
    finally
      lse_vargen_release(G);
    end;
  end;
end;

procedure __is(V1, V2: PLseValue);
var
  T1, T2: KLiType;
begin
  T2 := __AsType(V2);
  if T2 = KT_CLASS then
  begin
    T2 := KLiType(__AsObject(V2));
    T1 := __AsType(V1);
    if T1 = KT_CLASS then
      T1 := KLiType(__AsObject(V1));
    lse_set_bool(V1, T1 = T2);
  end
  else __abseq(V1, V2);
end;

procedure __as(V1, V2: PLseValue; E: KLiEngine);
var
  T2: KLiType;
begin
  T2 := __AsType(V2);
  if T2 = KT_CLASS then
    T2 := KLiType(__AsObject(V2));
  __SetTypeValue(E, V1, T2);
end;

// __type_inc() ----------------------------------------------------------------

function __type_inc_void(R: KLiType): KLiType;
begin
  Result := KT_VOID;
end;

function __type_inc_string(R: KLiType): KLiType;
begin
  if R = KT_VOID then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
    Result := KT_STRING;
end;

function __type_inc_int(R: KLiType): KLiType;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
  if R.DataType = LSV_FLOAT then
    Result := KT_FLOAT else
    Result := KT_INT;
end;

function __type_inc_float(R: KLiType): KLiType;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
    Result := KT_FLOAT;
end;

function __type_inc_variant(R: KLiType): KLiType;
begin
  Result := KT_VARIANT;
end;

function __type_inc(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_string,
    {$IFDEF FPC}@{$ENDIF}__type_inc_int,
    {$IFDEF FPC}@{$ENDIF}__type_inc_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_dec() ----------------------------------------------------------------

function __type_dec_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT] then
    Result := KT_FLOAT else
  if R.DataType in [LSV_INT] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_dec_float(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_dec(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_dec_int,
    {$IFDEF FPC}@{$ENDIF}__type_dec_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_mul() ----------------------------------------------------------------

function __type_mul_string(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_INT] then
    Result := KT_STRING else
    Result := KT_VOID;
end;

function __type_mul_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT] then
    Result := KT_FLOAT else
  if R.DataType in [LSV_INT] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_mul_float(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_mul(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_mul_string,
    {$IFDEF FPC}@{$ENDIF}__type_mul_int,
    {$IFDEF FPC}@{$ENDIF}__type_mul_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_div() ----------------------------------------------------------------

function __type_div_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_div_float(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_div(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_div_int,
    {$IFDEF FPC}@{$ENDIF}__type_div_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

function __type_mod_string(R: KLiType): KLiType;
begin
  Result := KT_STRING;
end;

function __type_mod_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_mod_float(R: KLiType): KLiType;
begin
  Result := __type_mod_int(R);
end;

function __type_mod(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_mod_string,
    {$IFDEF FPC}@{$ENDIF}__type_mod_int,
    {$IFDEF FPC}@{$ENDIF}__type_mod_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

function __type_neg(L: KLiType): KLiType;
begin
  if L = KT_VARIANT then
    Result := KT_VARIANT else
  if L.DataType in [LSV_FLOAT, LSV_INT] then
    Result := L else
    Result := KT_VOID;
end;

// __type_xor() ----------------------------------------------------------------

function __type_xor_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_xor_float(R: KLiType): KLiType;
begin
  Result := __type_xor_int(R);
end;

function __type_xor(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_xor_int,
    {$IFDEF FPC}@{$ENDIF}__type_xor_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void
  );
begin
  Result := OPRS[L.DataType](R);
end;

function __type_and(L, R: KLiType): KLiType;
begin
  Result := __type_xor(L, R);
end;

function __type_or(L, R: KLiType): KLiType;
begin
  Result := __type_xor(L, R);
end;

function __type_shl_int(R: KLiType): KLiType;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_INT] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_shl_float(R: KLiType): KLiType;
begin
  Result := __type_shl_int(R);
end;

function __type_shl_object(R: KLiType): KLiType;
begin
  Result := R;
end;

function __type_shl(L, R: KLiType): KLiType;
const
  OPRS : array[TLseValue] of function(R: KLiType): KLiType = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_shl_int,
    {$IFDEF FPC}@{$ENDIF}__type_shl_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_shl_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

function __type_shr(L, R: KLiType): KLiType;
begin
  Result := __type_xor(L, R);
end;

function __type_not(V: KLiType): KLiType;
begin
  if (V = KT_INT) or (V = KT_FLOAT) then
    Result := KT_INT else
    Result := KT_VOID;
end;

{----------------------------------------------------------------------)
(                                                                      )
(                         以下函数用于比较运算                         )
(                                                                      )
(----------------------------------------------------------------------}

function __compare(V1, V2: PLseValue): KLiCompResult;
var
  T1, T2: PLseType;
  R1, R2: integer;

  function compareS(const A, B: string): KLiCompResult;
  var
    X: integer;
  begin
    X := AnsiCompareStr(A, B);
    if X = 0 then
      Result := crEqual else if X > 0 then
      Result := crMore else
      Result := crLess;
  end;

  function compareF(A, B: double): KLiCompResult;
  begin
    A := A - B;
    if IsZero(A) then
      Result := crEqual else if A > 0 then
      Result := crMore  else
      Result := crLess;
  end;

  function compareI(A, B: int64): KLiCompResult;
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
    if R2 in [LSV_STRING] then
      Result := compareS(__AsString(V1), __AsString(V2)) else
      Result := crDiff;
    Exit;
  end;

  if R1 = LSV_OBJECT then
  begin
    if (T1^.cr_class = T2^.cr_class) and (V1^.VObject = V2^.VObject) then
      Result := crEqual else
      Result := crDiff;
    Exit;
  end;

  if R2 in [LSV_STRING, LSV_OBJECT] then
  begin
    Result := crDiff;
    Exit;
  end;

  if (R1 in [LSV_FLOAT]) or (R2 in [LSV_FLOAT]) then
    Result := compareF(__AsFloat(V1), __AsFloat(V2)) else
    Result := compareI(__AsInt64(V1), __AsInt64(V2));
end;

function __compare(V1, V2: PLseValue; Test: KLiCompResults): boolean;
begin
  Result := (__compare(V1, V2) in Test);
end;

function __contains(Host: TStringList; Value: PLseValue): boolean;
begin
  Result := false;
  case lse_type(Value)^.cr_type of
    LSV_STRING: Result := (Host.IndexOf(lse_strec_string(Value^.VObject)) >= 0);
  end;
end;

function __contains(Host: KLiVarList; Value: PLseValue; FindItemVG: boolean): boolean;
var
  clss, tmpc: KLiType;
  next, size: integer;
  data: PLseValue;
begin
  Result := false;
  clss := __AsType(Value);
  size := Host.Count;
  next := 0;
  while not Result and (next < size) do
  begin
    data := Host[next];
    tmpc := __AsType(data);
    if clss = tmpc then
      case clss.TypeRec^.cr_type of
        LSV_VOID   : Result := true;
        LSV_STRING : Result := (Value^.VObject = data^.VObject)
                            or __bufSame(lse_strec_data(Value^.VObject),
                                         lse_strec_length(Value^.VObject),
                                         lse_strec_data(data^.VObject),
                                         lse_strec_length(data^.VObject),
                                         false);
        LSV_INT    : Result := (Value^.VInteger = data^.VInteger);
        LSV_FLOAT  : Result := IsZero(Value^.VFloat - data^.VFloat);
//      LSV_VARIANT: Result := true;
        LSV_OBJECT : Result := (Value^.VObject = data^.VObject);
      end;
    if not Result and FindItemVG then
      if (tmpc = KT_VARGEN) and (data^.VObject <> nil) then
        Result := lse_vargen_contains(PLseVargen(data^.VObject), Value);
    Inc(next);
  end;
end;

function __contains(Host: PLseString; Value: PLseValue): boolean;
var
  slen: integer;
  base: pchar;

  function hasc(C: char): boolean;
  var
    index: integer;
  begin
    for index := 0 to slen - 1 do
      if C = base[index] then
      begin
        Result := true;
        Exit;
      end;
    Result := false;
  end;

  function hass(S: PLseString): boolean;
  begin
    Result := (nil <> __pos(base, slen, lse_strec_data(S),
                            lse_strec_length(S), false));
  end;
  
begin
  Result := false;
  if Host <> nil then
  begin
    base := lse_strec_data(Host);
    slen := lse_strec_length(Host);
    case lse_type(Value)^.cr_type of
      LSV_STRING: Result := hass(Value^.VObject);
      LSV_INT   : Result := (Value^.VInteger in [0..255]) and
                             hasc(char(Value^.VInteger));
    end;
  end;
end;

function __contains(Host: int64; Value: PLseValue): boolean;
begin
  Result := false;
  if Host > 0 then
    case lse_type(Value)^.cr_type of
      LSV_INT : Result := (Host > Value^.VInteger) and (Value^.VInteger >= 0);
    end;
end;

function __contains(Host: KLiHashed; Value: PLseValue): boolean;
begin
  Result := (Value^.vtype <> nil) and
            (Value^.vtype^.cr_type in [LSV_STRING]) and
            Host.IsSet(__AsString(Value));
end;

function __decodeTypeName(const TypeName: string; var Module: string): string;
var
  X: integer;
begin
  X := Pos('::', TypeName);
  if X > 0 then
  begin
    Result := Copy(TypeName, X + 2, MaxInt);
    Module := Copy(TypeName, 1, X - 1);
  end
  else
  begin
    Result := TypeName;
    Module := '';
  end;
end;

function __loadLibrary(const name: string; const libfile: string): KLiModule;
label
  UNLOCK;
var
  handle: THandle;
  IE: TLseInitExchange;
  MR: RLseModule;
begin
  lock_kernel;
  try
    // 1. search current instance

    Result := nil;

    if not __IsIDStr(pchar(name)) then
      goto UNLOCK;

    Result := KLiModule(__findNamed(sys_libraries, name));
    if Result <> nil then
      goto UNLOCK;

    // 2. search library file

    handle := 0;
    if not lse_load_library(libfile, handle) then
      goto UNLOCK;

    IE := TLseInitExchange(lse_get_proc(handle, 'InitExchange'));
    if not Assigned(IE) then
    begin
      lse_free_library(handle);
      goto UNLOCK;
    end;

    // 4. initialize the library

    __zero(@MR, sizeof(RLseModule));
    IE(@MR, @qe_entries);

    // 5. setup classes list

    Result := KLiModule.Create(name, nil, moyLibrary);
    try
      Result.FFileName := libfile;
      Result.FVersion := MR.iw_version;
      Result.FDescription := MR.iw_desc;
      Result.FInvokeProc := MR.iw_invoke;
      Result.FHandle := handle;
      Result.SetupModuleTypes(Addr(MR.iw_types));
      Result.SetupModuleFuncs(Addr(MR.iw_funcs));
    except
      FreeAndNil(Result);
    end;
    UNLOCK:
  finally
    unlock_kernel;
  end;
end;

function __SetupModule(const name: string; MR: PLseModule): KLiModule;
label
  UNLOCK;
var
  ID, fname: string;
begin
  lock_kernel;
  try
    // 1. check if already exists
    Result := nil;
    if Pos('=', name) > 0 then
    begin
      ID := Trim(__extractNameValue(name, fname));
      fname := Trim(fname);
      if fname = '' then fname := ID else
      if FileExists(fname) then
        fname := lse_expand_fname(fname);
    end
    else ID := name;

    if not __IsIDStr(pchar(ID)) or __IsReserved(ID, true)
      or __namedExists(sys_libraries, ID)
        or (MR = nil) then
          goto UNLOCK;

    // 2. setup classes list
    Result := KLiModule.Create(ID, nil, moyRegistered);
    Result.FFileName := fname;
    Result.FVersion := MR^.iw_version;
    Result.FDescription := MR^.iw_desc;
    Result.FInvokeProc := MR^.iw_invoke;
    Result.SetupModuleTypes(@MR^.iw_types);
    Result.SetupModuleFuncs(Addr(MR^.iw_funcs));

    UNLOCK:
  finally
    unlock_kernel;
  end;
end;

function __searchModule(var Name: string;
  const SearchPath: string; var DLL: boolean): boolean;
var
  path, temp: string;
  base, next, slen: integer;
begin
  slen := Length(SearchPath);
  base := 0;
  repeat
    next := base + 1;
    while (next <= slen) and (SearchPath[next] <> ';') do Inc(next);
    path := Trim(Copy(SearchPath, base + 1, next - base - 1));
    Result := path <> '';
    if Result then
    begin
      path := __fullPath(path);

      temp := path + Name + LSE_DLLEXT;
      Result := FileExists(temp);
      if Result then
      begin
        Name := temp;
        DLL := true;
        Exit;
      end;

      temp := path + Name + '.ls';
      Result := FileExists(temp);
      if Result then
      begin
        Name := temp;
        DLL := false;
        Exit;
      end;
    end;
    base := next;
  until next >= slen;
end;

procedure __SetupLseClasses;
var
  class_rec: RLseType;

  function classup(M: KLiModule; KC: TLseKernelType; var KR: PLseType): KLiType;
  begin
    Result := KLiType.Create(M, class_rec.cr_name, TLseValue(class_rec.cr_type));
    KR := Result.FTypeRec;
    class_rec.cr_class := Result;
    Move(class_rec, KR^, sizeof(RLseType));
    sys_type_list[KC] := KR;
  end;

begin
  if sys_module = nil then
  begin
    sys_module := KLiModule.Create('sys', nil, moyKernel);
    cgi_module := KLiModule.Create('cgi', nil, moyKernel);

    lse_fill_typerec(@class_rec, LSV_VOID);
    KT_VOID := classup(sys_module, kcVoid, KR_VOID);

    lse_fill_typerec(@class_rec, LSV_STRING);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_strec_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_strec_release;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}cvgr_string;
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}getiv_string;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}length_string;
    KT_STRING := classup(sys_module, kcString, KR_STRING);

    lse_fill_typerec(@class_rec, LSV_INT);
    KT_INT := classup(sys_module, kcInteger, KR_INT);

    lse_fill_typerec(@class_rec, LSV_FLOAT);
    KT_FLOAT := classup(sys_module, kcFloat, KR_FLOAT);

    lse_fill_typerec(@class_rec, LSV_VARIANT);
    KT_VARIANT := classup(sys_module, kcVariant, KR_VARIANT);

    lse_fill_typerec(@class_rec, KTN_TYPE, KTD_TYPE, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_type;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}stoo_type;
    KT_CLASS := classup(sys_module, kcType, KR_CLASS);

    lse_fill_typerec(@class_rec, KTN_MODULE, KTD_MODULE, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_module;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}stoo_module;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}getpv_module;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}setpv_module;
    KT_MODULE := classup(sys_module, kcModule, KR_MODULE);

    lse_fill_typerec(@class_rec, KTN_FUNC, KTD_FUNC, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_function;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}cvgr_func;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}addi_function;
    KT_FUNC := classup(sys_module, kcFunc, KR_FUNC);

    lse_fill_typerec(@class_rec, KTN_VARIABLE, KTD_VARIABLE, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_variable;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}stoo_variable;
    KT_VARIABLE := classup(sys_module, kcVariable, KR_VARIABLE);

    lse_fill_typerec(@class_rec, KTN_ERROR, KTD_ERROR, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_error;
    KT_ERROR := classup(sys_module, kcError, KR_ERROR);

    lse_fill_typerec(@class_rec, KTN_STREAM, KTD_STREAM, LSV_OBJECT);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_stream_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_stream_release;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}cvgr_stream;
    class_rec.cr_write_to := {$IFDEF FPC}@{$ENDIF}wtos_stream;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}addi_stream;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}length_stream;
    KT_STREAM := classup(sys_module, kcStream, KR_STREAM);

    lse_fill_typerec(@class_rec, KTN_VARLIST, KTD_VARLIST, LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}otos_varlist;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}stoo_varlist;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}cvgr_varlist;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}addi_varlist;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}length_varlist;
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}getiv_varlist;
    class_rec.cr_setiv := {$IFDEF FPC}@{$ENDIF}setiv_varlist;
    KT_VARLIST := classup(sys_module, kcVarlist, KR_VARLIST);

    lse_fill_typerec(@class_rec, KTN_VARSNAP, KTD_VARSNAP, LSV_OBJECT);
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}getiv_varsnap;
    class_rec.cr_setiv := {$IFDEF FPC}@{$ENDIF}setiv_varsnap;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}getpv_varsnap;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}setpv_varsnap;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}length_varsnap;
    KT_VARSNAP := classup(sys_module, kcVarsnap, KR_VARSNAP);

    lse_fill_typerec(@class_rec, KTN_HASHED, KTD_HASHED, LSV_OBJECT);
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}length_hashed;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}getpv_hashed;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}setpv_hashed;
    KT_HASHED := classup(sys_module, kcHashed, KR_HASHED);

    lse_fill_typerec(@class_rec, KTN_VARGEN, KTD_VARGEN, LSV_OBJECT);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_vargen_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_vargen_release;
    KT_VARGEN := classup(sys_module, kcVargen, KR_VARGEN);

    { setup class methods }

    sys_module.SetupModuleFuncs(@sys_module_funcs);
    cgi_module.SetupModuleFuncs(@cgi_module_funcs);

    { prepare }

    sys_module.FindFunc('eol').IsNameCall := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure __runner_next(Sender: KLiRunner);
begin
  with Sender do
    if (FCurrent <> nil) and not (FExcepted or FTerminated) then
      Inc(FCurrent^.next);
end;

procedure __runner_error(Sender: KLiRunner);
begin
  with Sender do
  begin
    ErrorRT(Format(EsUnknownOper, [Symbols[FExprrec^.Sym].ID]));
    Terminate;
  end;
end;

procedure __runner_press(Sender: KLiRunner);
begin
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_ID(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(GetValue(FExprrec^.VVarb));
  __runner_next(Sender);
end;

procedure __runner_become(Sender: KLiRunner);
var
  varb: KLiVarb;
  data: PLseValue;
begin
  with Sender do
  begin
    data := FStack[-1];
    varb := FExprrec^.VVarb;
    __SetTypeValue(FEngine, data, varb.ValueType);
    if not FExcepted then
      lse_set_value(GetValue(varb), data);
  end;
  __runner_next(Sender);
end;

procedure __runner_float(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VFLoat);
  __runner_next(Sender);
end;

procedure __runner_call(Sender: KLiRunner);
begin
  with Sender do with FExprrec^ do
    Goon(VFunc, ParamCount, nil);
  __runner_next(Sender);
end;

procedure __runner_echo(Sender: KLiRunner);
var
  A, index: integer;
  data: PLseValue;
begin
  with Sender do
  begin
    index := FStack.Count - FExprrec^.ParamCount;
    for A := 0 to FExprrec^.ParamCount - 1 do
    begin
      if A > 0 then
        lse_stream_write(FEngine.Output, ' ', 1);
      data := FStack[index + A];
      if lse_vtype(data) = LSV_STRING then
        lse_stream_write(FEngine.Output, PLseString(data^.VObject)) else
        lse_stream_write(FEngine.Output, __AsString(data));
    end;
    FStack.Press(FExprrec^.ParamCount);
  end;
  __runner_next(Sender);
end;

procedure __runner_int(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VInteger);
  __runner_next(Sender);
end;

procedure __runner_str(Sender: KLiRunner);
begin
  with Sender do
    lse_set_string(FStack.Add, FExprrec^.VStr);
  __runner_next(Sender);
end;

procedure __runner_add(Sender: KLiRunner);
begin
  __inc(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_dec(Sender: KLiRunner);
begin
  __dec(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_mul(Sender: KLiRunner);
begin
  __mul(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_div(Sender: KLiRunner);
begin
  __div(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_mod(Sender: KLiRunner);
begin
  __mod(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_bnot(Sender: KLiRunner);
begin
  __not(Sender.FStack[-1]);
  __runner_next(Sender);
end;

procedure __runner_bxor(Sender: KLiRunner);
begin
  __xor(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_bor(Sender: KLiRunner);
begin
  __or(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_band(Sender: KLiRunner);
begin
  __and(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_bshl(Sender: KLiRunner);
begin
  __shl(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_bshr(Sender: KLiRunner);
begin
  __shr(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_not(Sender: KLiRunner);
begin
  __logicNot(Sender.FStack[-1]);
  __runner_next(Sender);
end;

procedure __runner_neg(Sender: KLiRunner);
begin
  __neg(Sender.FStack[-1]);
  __runner_next(Sender);
end;

procedure __runner_eq(Sender: KLiRunner);
begin
  __equal(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_ne(Sender: KLiRunner);
begin
  __diff(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_less(Sender: KLiRunner);
begin
  __less(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_le(Sender: KLiRunner);
begin
  __eqless(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_more(Sender: KLiRunner);
begin
  __more(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_me(Sender: KLiRunner);
begin
  __eqmore(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_in(Sender: KLiRunner);
var
  V1, V2: PLseValue;
  T2: KLiType;
  done: boolean;
begin
  done := false;
  with Sender do
  begin
    V1 := FStack[-2];
    V2 := FStack[-1];
    T2 := __AsType(V2);
    case T2.FTypeRec^.cr_type of
      LSV_STRING: done := __contains(V2^.VObject, V1);
      LSV_INT   : done := __contains(V2^.VInteger, V1);
      LSV_OBJECT: if V2^.VObject <> nil then
                    if T2 = KT_VARLIST then
                      done := __contains(KLiVarList(V2^.VObject), V1, true) else
                    if T2 = KT_HASHED then
                      done := __contains(KLiHashed(V2^.VObject), V1) else
                      done := lse_vargen_contains(__AsVargen(FEngine, V2), V1);
    end;
    FStack.DeleteLast;
  end;
  lse_set_bool(V1, done);
  __runner_next(Sender);
end;

procedure __runner_and(Sender: KLiRunner);
begin
  __logicAnd(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_or(Sender: KLiRunner);
begin
  __logicOr(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_type(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VType, KR_CLASS);
  __runner_next(Sender);
end;

procedure __runner_func(Sender: KLiRunner);
var
  func, curr: KLiFunc;
  data: RLseValue;
begin
  with Sender do
  begin
    func := FExprrec^.VFunc;
    if func.IsLambdaFunc and func.ExpandThis then
    begin
      data.vtype := KR_VARSNAP;
      data.VObject := FCurrent^.values;
      curr := curry_one(func, @data, CurrentModule);
      curr.SaveTo(FStack.Add);
      if curr <> func then
        curr.DecRefcount;
    end
    else FStack.Add(func, KR_FUNC);
  end;
  __runner_next(Sender);
end;

procedure __runner_varlist(Sender: KLiRunner);
var
  L: KLiVarList;
  X, N: integer;
begin
  L := __NewVarlist(Sender.Engine);
  with Sender do
  begin
    N := FStack.Count;
    X := N - FExprrec^.ParamCount;
    while X < N do
    begin
      lse_set_value(L.Add, FStack[X]);
      Inc(X);
    end;
    FStack.Press(FExprrec^.ParamCount);
    FStack.Add(L, KR_VARLIST);
  end;
  __runner_next(Sender);
end;

procedure __runner_nil(Sender: KLiRunner);
begin
  Sender.FStack.AddDefault(KR_VOID);
  __runner_next(Sender);
end;

procedure __runner_getenv(Sender: KLiRunner);
begin
  with Sender do
    FEngine.GetValue(FExprrec^.Name, FStack.Add);
  __runner_next(Sender);
end;

procedure __runner_getsv(Sender: KLiRunner);
var
  A: KLiVarSnap;
  V: PLseValue;
  R: KLiVarb;
begin
  with Sender do
  begin
    A := KLiVarSnap(__AsObject(FCurrent^.values[0]));
    if A = nil then
      lse_error('varsnap not supplied');
    V := A.GetByName(FExprrec^.Name, R);
    if V = nil then
      lse_error('variable %s not found', [FExprrec^.Name]);
    FStack.Add(V);
  end;
  __runner_next(Sender);
end;

procedure __runner_setsv(Sender: KLiRunner);
var
  A: KLiVarSnap;
  V: PLseValue;
  R: KLiVarb;
begin
  with Sender do
  begin
    A := KLiVarSnap(__AsObject(FCurrent^.values[0]));
    if A = nil then
      lse_error('varsnap not supplied');
    V := A.GetByName(FExprrec^.Name, R);
    if V = nil then
      lse_error('variable %s not found', [FExprrec^.Name]);
    lse_set_value(V, __SetTypeValue(FEngine, FStack[-1], R.ValueType));
  end;
  __runner_next(Sender);
end;

procedure __runner_format(Sender: KLiRunner);
var
  V: PLseValue;
begin
  V := Sender.FStack[-1];
  lse_set_string(V, Sender.FormatFor(__AsString(V), nil));
  __runner_next(Sender);
end;

procedure __runner_module(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VModule, KR_MODULE);
  __runner_next(Sender);
end;

procedure __runner_is(Sender: KLiRunner);
begin
  __is(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_as(Sender: KLiRunner);
begin
  __as(Sender.FStack[-2], Sender.FStack[-1], Sender.FEngine);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_statement(Sender: KLiRunner);
begin
  Sender.FStack.SetCount(Sender.FCurrent^.base);
  __runner_next(Sender);
end;

procedure __runner_vargen(Sender: KLiRunner);
var
  last: PLseValue;
begin
  with Sender do
  begin
    last := FStack[-1];
    lse_set_vargen(last, __AsVargen(FEngine, last));
  end;
  __runner_next(Sender);
end;

procedure __runner_ask(Sender: KLiRunner);
var
  base, prmc: integer;
  data: PLseValue;
  clss: KLiType;
  func: KLiFunc;
begin
  with Sender do
  begin
    prmc := FExprrec^.ParamCount;
    base := FStack.Count - prmc;
    data := FStack[base];
    clss := __AsType(data);
    if clss = KT_FUNC then
    begin
      func := __AsFunc(data);
      __check(func <> nil, EsFuncNotSpecify);
      Goon(func, prmc - 1, data);
    end
    else
    if clss = KT_CLASS then
    begin
      clss := KLiType(data^.VObject);
      __check(clss <> nil, EsClassNotSpecify);
      __SetDefaultValue(data, clss);
      func := CurrentFunc.FindCreate(clss);
      if func <> nil then
      begin
        FStack.Delete(base);
        Goon(func, prmc - 1, nil);
      end
      else FStack.Press(prmc - 1);
    end
    else lse_error('invalid call to %s', [clss.FullName]);
  end;
  __runner_next(Sender);
end;

procedure __runner_try(Sender: KLiRunner);
var
  begx, endx: integer;
  in_finally: boolean;
begin
  with Sender do
  begin
    Inc(FCurrent^.next);
    begx := FCurrent^.next;
    endx := FExprrec^.VLabel^.VOffset;
    in_finally := xrInFinally in FExprrec^.flags;
    while ExecGoonNext do
      if (FCurrent^.next < begx) or (FCurrent^.next > endx) then
        Break;
    if FExcepted and not FTerminated and HasNext then
    begin
      FExcepted := false;
      FCurrent^.next := endx + 1;
      FStack.SetCount(FCurrent^.base);
      ExecGoonNext; // syRINR
      if in_finally then
        FExcepted := true;
    end;
  end;
end;

procedure __runner_return(Sender: KLiRunner);
begin
  with Sender do
  begin
    if FExprrec^.ParamCount > 0 then
      lse_set_value(FCurrent^.output, FStack[-1]) else
      lse_clear_value(FCurrent^.output);
    FStack.SetCount(FCurrent^.base);
    FCurrent^.next := LSE_MAX_CODES;
    FCurrent := nil; {<--returned}
  end;
end;

procedure __runner_jump(Sender: KLiRunner);
begin
  with Sender do
    Inc(FCurrent^.next, FExprrec^.VOffset);
end;

procedure __runner_jmpf(Sender: KLiRunner);
begin
  with Sender do
    if not __AsBool(FStack[-1]) then
      Inc(FCurrent^.next, FExprrec^.VOffset) else
      Inc(FCurrent^.next);
end;

procedure __runner_jmpt(Sender: KLiRunner);
begin
  with Sender do
    if __AsBool(FStack[-1]) then
      Inc(FCurrent^.next, FExprrec^.VOffset) else
      Inc(FCurrent^.next);
end;

procedure __runner_jmpfpop(Sender: KLiRunner);
begin
  __runner_jmpf(Sender);
  Sender.FStack.DeleteLast;
end;

procedure __runner_jmptpop(Sender: KLiRunner);
begin
  __runner_jmpt(Sender);
  Sender.FStack.DeleteLast;
end;

procedure __runner_goto(Sender: KLiRunner);
begin
  with Sender do
    FCurrent^.next := FExprrec^.VLabel^.VOffset;
end;

procedure __runner_gototp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if __AsBool(FStack[-1]) then
      FCurrent^.next := FExprrec^.VLabel^.VOffset else
      Inc(FCurrent^.next);
    FStack.DeleteLast;
  end;
end;

procedure __runner_gotofp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if not __AsBool(FStack[-1]) then
      FCurrent^.next := FExprrec^.VLabel^.VOffset else
      Inc(FCurrent^.next);
    FStack.DeleteLast;
  end;
end;

procedure __runner_hashed(Sender: KLiRunner);
var
  H: KLiHashed;
  X, B, N: integer;
begin
  H := KLiHashed.Create(Sender.FEngine, 1);
  with Sender do
  begin
    N := FExprrec^.ParamCount;
    B := FStack.Count - N;
    X := 0;
    while X < N do
    begin
      H.SetValue(__AsString(FStack[B]), FStack[B + 1]);
      Inc(B, 2);
      Inc(X, 2);
    end;
    FStack.Press(N);
    FStack.Add(H, KR_HASHED);
  end;
  __runner_next(Sender);
end;

procedure __runner_RINR(Sender: KLiRunner);
var
  begx, endx: integer;
begin
  with Sender do
  begin
    FEngine.FTempValues.Add;
    try
      Inc(FCurrent^.next);
      begx := FCurrent^.next;
      endx := FExprrec^.VLabel^.VOffset;
      while ExecGoonNext do
        if (FCurrent^.next < begx) or (FCurrent^.next > endx) then
          Break;
    finally
      FEngine.FTempValues.Press;
    end;
  end;
end;

procedure __runner_GETV(Sender: KLiRunner);
begin
  Sender.FStack.Add(Sender.FEngine.FTempValues[-1]);
  __runner_next(Sender);
end;

procedure __runner_SETV(Sender: KLiRunner);
begin
  lse_set_value(Sender.FEngine.FTempValues[-1], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_like(Sender: KLiRunner);
begin
  __like(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  __runner_next(Sender);
end;

procedure __runner_getiv(Sender: KLiRunner);
var
  data, keyr: PLseValue;
  clss: KLiType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-2];
  clss := __AsType(data);
  keyr := Sender.FStack[-1];
  case lse_vtype(keyr) of
    LSV_INT:
      if Assigned(clss.FTypeRec^.cr_getiv) then
      begin
        index := keyr^.VInteger;
        if clss.FTypeRec^.cr_getiv(data^.VObject, index, keyr, Sender.FEngine) > 0 then
        begin
          Sender.FStack.ExchangeLastTwo;
          Sender.FStack.DeleteLast;
        end
        else lse_error('failed get item by index of %d', [index]);
      end
      else lse_error('%s.getiv not supplied', [clss.Name]);
    LSV_STRING:
      begin
        name := lse_strec_data(keyr^.VObject);
        if Assigned(clss.FTypeRec^.cr_getpv) and
          (clss.FTypeRec^.cr_getpv(data^.VObject, pchar(name), keyr, Sender.FEngine) > 0) then
        begin
          Sender.FStack.ExchangeLastTwo;
          Sender.FStack.DeleteLast;
        end
        else
        begin
          curr := Sender.CurrentFunc;
          func := curr.FindMethod(name, clss);
          if func <> nil then
          begin
            curr := curry_one(func, data, curr.Module);
            curr.SaveTo(data);
            if curr <> func then
              curr.DecRefcount; // adjust refcount
            Sender.FStack.Press;
          end
          else
          begin
            func := curr.FindMethod('get_' + name, clss);
            if func <> nil then
            begin
              Sender.FStack.Press;
              Sender.Goon(func, 1, nil);
            end
            else lse_error('failed get property "%s"', [name]);
          end;
        end;
      end;
    else lse_error('invalid index or property name');
  end;
  __runner_next(Sender);
end;

procedure __runner_setiv(Sender: KLiRunner);
var
  data, keyr: PLseValue;
  clss: KLiType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-3];
  clss := __AsType(data);
  keyr := Sender.FStack[-2];
  case lse_vtype(keyr) of
    LSV_INT:
      if Assigned(clss.FTypeRec^.cr_setiv) then
      begin
        index := keyr^.VInteger; 
        if clss.FTypeRec^.cr_setiv(data^.VObject, index, Sender.FStack[-1], Sender.FEngine) > 0 then
          Sender.FStack.Press(2) else
          lse_error('failed set item by index of %d', [index]);
      end
      else lse_error('%s.setiv not supplied', [__AsType(data).Name]);
    LSV_STRING:
      begin
        name := lse_strec_data(keyr^.VObject);
        if Assigned(clss.FTypeRec^.cr_setpv) and
           (clss.FTypeRec^.cr_setpv(data^.VObject, pchar(name), Sender.FStack[-1], Sender.FEngine) > 0) then
          Sender.FStack.Press(2) else
        begin
          curr := Sender.CurrentFunc;
          func := curr.FindMethod('set_' + name, clss);
          if func <> nil then
          begin
            Sender.FStack.ExchangeLastTwo;
            Sender.FStack.Press;
            Sender.Goon(func, 2, nil);
          end
          else lse_error('failed set property "%s"', [name]);
        end;
      end;
    else lse_error('invalid index or property name');
  end;
  __runner_next(Sender);
end;

procedure __runner_duplast(Sender: KLiRunner);
var
  X, N: integer;
begin
  N := Sender.FExprrec^.ParamCount;
  X := Sender.FStack.Count - N;
  while N > 0 do
  begin
    Sender.FStack.Add(Sender.FStack[X]);
    Inc(X);
    Dec(N);
  end;
  __runner_next(Sender);
end;

procedure __runner_fill(Sender: KLiRunner);
begin
  with Sender do
  begin
    __fill(FStack[-2], FStack[-1], Sender);
    Sender.FStack.DeleteLast;
  end;
  __runner_next(Sender);
end;

procedure __runner_send(Sender: KLiRunner);
var
  V: KLiVarb;
  D: PLseValue;
begin
  V := Sender.FExprrec^.VVarb; 
  D := Sender.GetValue(V);
  if lse_vargen_send(Sender.FEngine.FTempValues[-1]^.VObject, D) then
  begin
    __SetTypeValue(Sender.FEngine, D, V.ValueType);
    Sender.FStack.Add(1);
  end
  else Sender.FStack.Add(0);
  __runner_next(Sender);
end;

procedure __runner_case(Sender: KLiRunner);
var
  L, R: PLseValue;
begin
  L := Sender.FStack[-1];
  R := Sender.FEngine.FTempValues[-1];
  __abseq(L, R);
  __runner_next(Sender);
end;

////////////////////////////////////////////////////////////////////////////////

{ KLiParser }

constructor KLiParser.Create(AModule: KLiModule);
begin
  FModule := AModule;
  FRunner := FModule.FEngine.FMainRunner;
  FExpr := TList.Create;
  FSymbols := KLiTokens.Create;
end;

function KLiParser.CurCodes: KLiExprList;
begin
  Result := FCurrent.FCodes;
end;

destructor KLiParser.Destroy;
begin
  if not FIsShadow then
    __freeAndNil(FTokenizer);
  __freeAndNil(FExpr);
  __freeAndNil(FSymbols);
  inherited;
end;

function KLiParser.Parse(const Code: string): KLiFunc;
begin
  FExpr.Clear;
  FSymbols.Clear;
  FCurrent := nil;
  FreeAndNil(FTokenizer);
  FTokenizer := KLiTokenizer.Create(Code);

  if FRunner <> nil then
  begin
    Result := FModule.NewFunc;
    if FModule.FMainFunc = nil then
      FModule.FMainFunc := Result;
  end
  else
  if FModule.IsMainModule then
  begin
    Result := FModule.FEngine.GetMainFunc;
    Result.FCodes.Clear;
  end
  else
  if FModule.FMainFunc = nil then
  begin
    Result := FModule.NewFunc;
    FModule.FMainFunc := Result;
  end
  else Result := nil;

  while GetNextSym and (FLast^.Sym <> syEof) do
  begin
    SymTestLast([syLBlock]);
    SymGotoNext;
    case FLast^.Sym of
      syDefine: ParseDefine;
      syImport: ParseImport;
      syConst : ParseConst;
      sySyntax: ParseSyntax
      else
      if (FLast^.Sym <> syRBlock) and (Result <> nil) then
      begin
        FCurrent := Result;
        ParseStatement(true);
      end;
    end;
  end;
end;

function KLiParser.GetSym(sym: PLiToken): boolean;
var
  token: PLiToken;
  isstr: boolean;
begin
  token := FTokenizer.GetNextToken(isstr);
  Result := (token <> nil);
  if Result then
  begin
    copy_token(token, sym);
    if sym^.Pos.module = nil then
      sym^.Pos.module := FModule;
    FLast := sym;
  end;
end;

function KLiParser.LastModule: KLiModule;
begin
  Result := KLiModule(FLast^.Pos.module);
  if Result = nil then
    Result := FModule;
end;

function KLiParser.GetNextSym: boolean;
begin
  Result := GetSym(FSymbols.Next);
end;

procedure KLiParser.ParseVarb(var varb: KLiVarb; var vrec: PLiToken;
  EndSyms: KLiSymbols; OnHead: boolean);
var
  clss: KLiType;
  data: PLiToken;
  rec: KLiFindRec;
begin
  if not OnHead then
    SymTestNextPureID else
    SymTestLastPureID;

  data := FLast;
  clss := KT_VARIANT;
  
  if PeekNextSym = syDot2 then
  begin
    if FCurrent.FindInside(data^.Val) then
    begin
      FLast := data;
      Error.Redeclared(Self);
    end;
    SymGotoNext;
    SymGotoNext;
    clss := ParseVarType(FLast);
    varb := CurCodes.AddLocal(data^.Val, clss);
    varb.FPos := data^.Pos;
  end
  else
  if not FCurrent.FindBy(data^.Val, @rec, [foVarb]) then
  begin
    if FCurrent.FindInside(data^.Val) then
    begin
      FLast := data;
      Error.Redeclared(Self);
    end;
    varb := CurCodes.AddLocal(data^.Val, clss);
    varb.FPos := data^.Pos;
  end
  else varb := rec.VVarb;

  vrec := data;
  SymTestNext(EndSyms);
end;

function KLiParser.ParseVarType(Token: PLiToken): KLiType;
var
  m_name, c_name: string;
  L: PLiToken;
begin
  c_name := __decodeTypeName(Token^.Val, m_name);
  Result := FModule.FindTypeBy(c_name, m_name);
  if Result = nil then
  begin
    L := FLast;
    try
      FLast := Token;
      Error.TypeNotExists(Self);
    finally
      FLast := L;
    end;
  end;
end;

procedure KLiParser.ParseEcho;
var
  L: PLiToken;
begin
  FExpr.Clear;
  L := FLast;
  L^.VParamCount := 0;
  repeat
    SymGotoNext;
    ParseExpr(FExpr, [syIf, syRBlock, syComma], true);
    Inc(L^.VParamCount);
  until FLast^.Sym in [syIf, syRBlock];
  FExpr.Add(L);
  L^.Sym := syEcho;
  TailIf(FExpr);
end;

procedure KLiParser.ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
begin
  if not OnHead then
  begin
    FSymbols.Clear;
    SymGotoNext;
  end;
  
  while not (FLast^.Sym in EndSyms) do
  begin
    ParseStatement(false);
    SymGotoNext;
  end;
end;

procedure KLiParser.ParseStatement(OnHead: boolean);
label
  SYNTAX_BACK;
var
  syntax: KLiSyntax;
begin
  try
    if not OnHead then
    begin
      SymTestLast([syLBlock]);
      SymGotoNext;
    end;
    SYNTAX_BACK:
    case FLast^.Sym of
      syIf      : ParseIf;
      syFor     : ParseFor;
      syWhile   : ParseWhile;
      syRepeat  : ParseRepeatUntil;
      sySwitch  : ParseSwitch;
      syBreak   : ParseBreak;
      syContinue: ParseContinue;
      syReturn  : ParseReturn;
      syTry     : ParseTry;
      syBecome  : ParseEcho;
      syImport  : ParseImport;
      syDefine  : ParseDefine;
      syConst   : ParseConst;
      sySyntax  : ParseSyntax;
      syDo      : ParseBlock([syRBlock], false);
      syRBlock, syEOF: {ignored};
      else begin
        if FLast^.Sym = syID then
        begin
          syntax := FindSyntax(FLast^.Val);
          if syntax <> nil then
          begin
            ExpandSyntax(syntax, true);
            SymGotoNext;
            goto SYNTAX_BACK;
          end;
        end;
        ParseAny;
      end;
    end;
  finally
    FExpr.Clear;
    FSymbols.Clear;
  end;
end;

procedure KLiParser.ParseWhile;
var
  bl, cl, ll: string;
begin
// --------------------------------------------
// BLOCK: while condition do
//          ...
//        end
// --------------------------------------------
// CONTINUE_LABEL:
//      <condition>
//      GOTO BREAK_LABEL IF FALSE
//      <statements>
//      GOTO CONTINUE_LABEL
// BREAK_LABEL:
// --------------------------------------------
  SaveLabels(bl, cl, true);
  try
    ll := CurCodes.LastLabel;
    if ll = '' then
      CurCodes.AddLabel(FContinueLabel, FLast^.Pos) else
      FContinueLabel := ll;
    FExpr.Clear;
    ParseExpr(FExpr, [syDo], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddGoto(FBreakLabel, FLast^.Pos)^.Sym := syGoFP;
    ParseBlock([syRBlock], false);
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
    CurCodes.AddLabel(FBreakLabel, FLast^.Pos);
  finally
    RestoreLabels(bl, cl);
  end;
end;

procedure KLiParser.ParseIf;
var
  bl, cl: string;
  cx: PLiExprRec;
begin
  bl := FModule.NewLabelName;
  repeat
    FExpr.Clear;
    ParseExpr(FExpr, [syThen], false);
    CurCodes.LoadExpr(FExpr);
    cl := FModule.NewLabelName;
    cx := CurCodes.AddGoto(cl, FLast^.Pos);
    cx^.Sym := syGoFP;
    ParseBlock([syRBlock, syElse, syElif], false);
    if FLast^.Sym <> syRBlock then
    begin
      CurCodes.AddGoto(bl, FLast^.Pos);
      CurCodes.AddLabel(cl, FLast^.Pos);
      if FLast^.Sym = syElse then
        ParseBlock([syRBlock], false);
    end
    else cx^.Name := bl;
  until FLast^.Sym = syRBlock;
  cl := CurCodes.LastLabel;
  if cl = '' then
    CurCodes.AddLabel(bl, FLast^.Pos) else
    CurCodes.ChangeGoto(bl, cl);
end;

procedure KLiParser.ParseBreak;
var
  end_if: string;
begin
  if FBreakLabel = '' then
    Error.BreakNoLoop(Self);
  SymTestNext([syIf, syRBlock]);
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syRBlock);
    CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
    EndTailIf(end_if);
  end
  else CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
end;

procedure KLiParser.ParseConst;
var
  clss: KLiType;
  data: PLiToken;
  curr: KLiFunc;
begin
  SymTestNextPureID;
  if FModule.Find(FLast^.Val) then
    Error.Redeclared(Self);
  data := FLast;

  SymGotoNext;
  if FLast^.Sym = syDot2 then
  begin
    SymTestNext([syID]);
    clss := ParseVarType(FLast);
    SymGotoNext;
  end
  else clss := KT_VARIANT;

  SymTestLast([syBecome]);
  SymGotoNext;

  curr := FCurrent;
  try
    FCurrent := KLiFunc.Create(FModule, clss, data^.Val, nil, nil);
    FCurrent.IsConstFunc := true;
    FCurrent.IsNameCall := true;
    FExpr.Clear;
    ParseExpr(FExpr, [syRBlock], true);
    FLast^.Sym := syReturn;
    FLast^.VParamCount := 1;
    FExpr.Add(FLast);
    FCurrent.FCodes.LoadExpr(FExpr);
  finally
    FCurrent := curr;
  end;
end;

procedure KLiParser.ParseContinue;
var
  end_if: string;
begin
  if FContinueLabel = '' then
    Error.ContinueNoLoop(Self);
  SymTestNext([syIf, syRBlock]);
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syRBlock);
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
    EndTailIf(end_if);
  end
  else CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
end;

procedure KLiParser.ParseRepeatUntil;
var
  bl, cl, ll, body: string;
begin
// --------------------------------------------
// BLOCK: repeat
//          ...
//        until condition;
// --------------------------------------------
// BODY_LABEL:
//      <statements>
// CONTINUE_LABEL:
//      <condition>
//      GOTO BODY_LABEL IF FALSE
// BREAK_LABEL:
// --------------------------------------------
  SaveLabels(bl, cl, true);
  try
    body := CurCodes.LastLabel;
    if body = '' then
    begin
      body := FModule.NewLabelName;
      CurCodes.AddLabel(body, FLast^.Pos);
    end;

    ParseBlock([syUntil], false);

    ll := CurCodes.LastLabel;
    if ll <> '' then
    begin
      CurCodes.ChangeGoto(FContinueLabel, ll);
      FContinueLabel := ll;
    end
    else CurCodes.AddLabel(FContinueLabel, FLast^.Pos);

    FExpr.Clear;
    ParseExpr(FExpr, [syRBlock], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddGoto(body, FLast^.Pos)^.Sym := syGoFP;
    CurCodes.AddLabel(FBreakLabel, FLast^.Pos);
  finally
    RestoreLabels(bl, cl);
  end;
end;

procedure KLiParser.ParseReturn;
var
  end_if: string;

  procedure setup_return(has_result: boolean);
  var
    expr: PLiExprRec;
  begin
    expr := CurCodes.AddNew(syReturn, @FLast^.Pos);
    expr^.ParamCount := Ord(has_result);
  end;

begin
  SymGotoNext;
  
  if FLast^.Sym = syRBlock then
  begin
    setup_return(false);
    Exit;
  end;

  FExpr.Clear;
  
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syRBlock);
    setup_return(false);
    EndTailIf(end_if);
    Exit;
  end;
  
  ParseExpr(FExpr, [syRBlock, syIf], true);
  if FLast^.Sym = syRBlock then
  begin
    CurCodes.LoadExpr(FExpr);
    setup_return(true);
    Exit;
  end;
  
  BeginTailIf(end_if, syRBlock);
  CurCodes.LoadExpr(FExpr);
  setup_return(true);
  EndTailIf(end_if);
end;

procedure KLiParser.ParseFor;
var
  varb: KLiVarb;
  vrec: PLiToken;
  bl, cl: string;
  expr: PLiExprRec;
begin
// --------------------------------------------
// BLOCK: for varb in vargen if condition do
//          ...
//        end
// --------------------------------------------
//      SAVE VARGEN TO TV
// CONTINUE_LABEL:
//      <vargen>
//      GOTO BREAK_LABEL IF FALSE 
//      <condition>
//      GOTO CONTINUE_LABEL IF FALSE 
//      varb = <vargen>
//      <statements>
//      GOTO CONTINUE_LABEL
// BREAK_LABEL:
// --------------------------------------------

  SaveLabels(bl, cl, true);
  try
    ParseVarb(varb, vrec, [syIn], false);

    expr := CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
    expr^.Sym := syRINR;

    FExpr.Clear;
    ParseExpr(FExpr, [syIf, syDo], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddNew(syVarGen, @(FLast^.Pos));
    CurCodes.AddNew(sySETV, @(FLast^.Pos));

    CurCodes.AddLabel(FContinueLabel, FLast^.Pos);

    expr := CurCodes.AddNew(sySend, @vrec^.Pos);
    expr^.VVarb := varb;
    expr^.flags := expr^.flags + [xrSatisfied];
    expr := CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
    expr^.Sym := syGoFP;

    if FLast^.Sym = syIf then
    begin
      FExpr.Clear;
      ParseExpr(FExpr, [syDo], false);
      CurCodes.LoadExpr(FExpr);
      expr := CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
      expr^.Sym := syGoFP;
    end;
    
    ParseBlock([syRBlock], false);
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
    CurCodes.AddLabel(FBreakLabel, FLast^.Pos);
  finally
    RestoreLabels(bl, cl);
  end;
end;

procedure KLiParser.ParseImport;
label
  NEXT;
var
  m_name, f_name: string;
  is_lib: boolean;
  curr: KLiModule;
begin
  lock_kernel;
  try
    repeat
      SymTestNextPureID;
      m_name := FLast^.Val;

      if FModule.FindModule(m_name, false) <> nil then
        goto NEXT;

      if FModule.Find(m_name) then
        Error.ModuleReimport(Self);

      curr := KLiModule(__findNamed(sys_libraries, m_name));
      if curr <> nil then
      begin
        FModule.FModules.Add(curr);
        goto NEXT;
      end;

      curr := FModule.FEngine.FModules.Find(m_name);
      if curr <> nil then
      begin
        if curr.Parsing then
          Error.ImportEachOther(Self, curr);
        curr.AddImporter(FModule);
        FModule.FModules.Add(curr);
        goto NEXT;
      end;

      is_lib := false;
      f_name := m_name;
      if not __searchModule(f_name, FModule.FEngine.GetSearchPath, is_lib) then
        Error.ModuleNotFound(Self);

      if is_lib then
      begin
        curr := __loadLibrary(m_name, f_name);
        if curr = nil then
          Error.WrongLibrary(Self);
        FModule.FModules.Add(curr);
      end
      else
      begin
        curr := KLiModule.Create(m_name, FModule.FEngine, moyScript);
        curr.FFileName := f_name;
        curr.Parsing := true;
        KLiParser.Create(curr).ParseAndFree(__fileText(f_name));
        curr.Parsing := false;
        curr.AddImporter(FModule);
        FModule.FModules.Add(curr);
      end;

      NEXT:
      SymTestNext([syComma, syRBlock]);
    until FLast^.Sym = syRBlock;
  finally
    unlock_kernel;
  end;
end;

procedure KLiParser.SymExpected(Sym: PLiToken; Syms: KLiSymbols);
begin
  if not (Sym^.Sym in Syms) then
    Error.SymUnexpected(Self);
end;

procedure KLiParser.SymTestLast(Syms: KLiSymbols);
begin
  SymExpected(FLast, Syms);
end;

procedure KLiParser.SymTestLastPureID;
begin
  SymTestLast([syID]);
  if not IsPureID(FLast) then
    Error.NeedPureID(Self);
end;

procedure KLiParser.SymTestNext(Syms: KLiSymbols);
begin
  SymGotoNext;
  SymTestLast(Syms);
end;

procedure KLiParser.SymTestNextPureID;
begin
  SymGotoNext;
  SymTestLastPureID;
end;

procedure KLiParser.TailIf(ExprList: TList);
var
  F: string;
begin
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(F, syRBlock);
    CurCodes.LoadExpr(ExprList);
    EndTailIf(F);
  end
  else
  begin
    CurCodes.LoadExpr(ExprList);
    CurCodes.EndStatement;
  end;
end;

procedure KLiParser.SymGotoNext;
begin
  if not GetNextSym then
    Error.SymNotFound(Self);
end;

procedure KLiParser.ParseTry;
var
  leave_label, catch_label: string;
  expr: PLiExprRec;
  in_finally: boolean;
begin
// BLOCK: try ... except ... end  |  try ... finally ... end
// ------------------------------+-------------------------
// try                           |  try
//     <statements>              |      <statements>
//     GOTO LEAVE_LABE           |      GOTO FINALLY_LABE
// except                        |  finally 
//   CATCH_LABEL:                |    CATCH_LABEL:
//   [                           |      SET ?ERROR? = true
//     case ExceptionName_1:     |      GOTO DEAL_LABEL
//       <statements>            |    FINALLY_LABEL:
//       GOTO LEAVE_LABEL        |      SET ?ERROR? = false
//     case ExceptionName_2:     |    DEAL_LABEL:
//       <statements>            |      <statements>
//       GOTO LEAVE_LABEL        |    THROW IF ?ERROR?
//     ....                      |  end
//     case ExceptionName_N:     |
//       <statements>            |
//       GOTO LEAVE_LABEL        |
//     default:                  |
//   ]                           |
//     <statements>              |
// end                           |
// LEAVE_LABEL:                  |
// --------------------------------------------
  catch_label := FModule.NewLabelName;
  leave_label := FModule.NewLabelName;

  { try }

  expr := CurCodes.AddTry(catch_label, FLast^.Pos);
  Inc(FTryCount);
  try
    ParseBlock([syFinally, syExcept], false);
    CurCodes.AddGoto(leave_label, FLast^.Pos);
  finally
    Dec(FTryCount);
  end;

  { catch or finally }

  in_finally := (FLast^.Sym = syFinally); 
  if in_finally then
    expr^.flags := expr^.flags + [xrInFinally];

  Inc(FCatchCount);
  try
    if in_finally then
    begin
      CurCodes.ChangeGoto(catch_label, leave_label);
      catch_label := leave_label;
      leave_label := FModule.NewLabelName;
    end;
    CurCodes.AddLabel(catch_label, FLast^.Pos);
    expr := CurCodes.AddGoto(leave_label, FLast^.Pos);
    expr^.Sym := syRINR;
    ParseBlock([syRBlock], false);
    CurCodes.AddLabel(leave_label, FLast^.Pos);
  finally
    Dec(FCatchCount);
  end;
end;

function KLiParser.ParseAndFree(const Code: string): KLiFunc;
begin
  try
    Result := Parse(Code);
  finally
    Free;
  end;
end;

procedure KLiParser.ParseAny;
const
  OPRS = [syMul, syDiv, syMod, syAdd, syDec, syBXor,
          syBAnd, syBOr, syBShl, syBShr];
var
  R, M, L: PLiToken;
  S, X: KLiSymbol;
  V: KLiVarb;
begin
  M := nil;
  FExpr.Clear;
  PeekNextTwoSym(S, X);

  if IsPureID(FLast) and ((S in [syBecome, syDot2]) or ((S in OPRS) and (X = syBecome))) then
  begin
    ParseVarb(V, R, [syBecome] + OPRS, true);
    S := FLast^.Sym;
    if S in OPRS then
    begin
      M := FLast;
      SymTestNext([syBecome]);
      FExpr.Add(CloneSym(R));
    end;
    ParseExpr(FExpr, [syIf, syRBlock], false);
    if S in OPRS then
      FExpr.Add(M);
    FExpr.Add(R);
    R^.Sym := syBecome;
  end
  else
  if (FLast^.Sym = syGetSV) and ((S = syBecome) or ((S in OPRS) and (X = syBecome))) then
  begin
    FCurrent.AddSuper;
    R := FLast;
    SymGotoNext;
    S := FLast^.Sym;
    if S in OPRS then
    begin
      M := FLast;
      SymTestNext([syBecome]);
      FExpr.Add(CloneSym(R));
    end;
    ParseExpr(FExpr, [syIf, syRBlock], false);
    if S in OPRS then
      FExpr.Add(M);
    FExpr.Add(R);
    R^.Sym := sySetSV;
  end
  else
  begin
    ParseExpr(FExpr, [syBecome, syIf, syRBlock] + OPRS, true);
    S := FLast^.Sym;
    if S in [syBecome] + OPRS then
    begin
      R := FExpr.Last;

      if R^.Sym <> syGetIV then
      begin
        FLast := R;
        Error.SymExpected(Self, SymsToStrList([syGetIV]));
      end;

      if S in OPRS then
      begin
        M := FLast;
        SymTestNext([syBecome]);
        L := CloneSym(R);
        FExpr.Add(L);
        R^.Sym := syDupLast;
        R := CloneSym(L);
      end
      else FExpr.Delete(FExpr.Count - 1);

      ParseExpr(FExpr, [syRBlock, syIf], false);

      if S in OPRS then
        FExpr.Add(M);

      FExpr.Add(R);

      Inc(R^.VParamCount);
      R^.Sym := sySetIV;
    end;
  end;

  TailIf(FExpr);
end;

procedure KLiParser.ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
var
  L: PLiToken;
  V: KLiVarb;
  T: KLiType;
begin
  if not OnHead then SymGotoNext;
  if not (FLast^.Sym in EndSym) then
  begin
    SymTestLastPureID;
    while true do
    begin
      if Func.FindInside(FLast^.Val) then
        Error.Redeclared(Self);
      L := FLast;
      T := KT_VARIANT;
      SymTestNext(EndSym + [syComma, syDot2]);
      if FLast^.Sym = syDot2 then
      begin
        SymTestNext([syID]);
        T := ParseVarType(FLast);
        SymTestNext(EndSym + [syComma]);
      end;
      V := Func.AddParam(L^.Val, T);
      V.FPos := L^.Pos;
      if FLast^.Sym in EndSym then Exit else SymTestNextPureID;
    end;
  end;
end;

procedure KLiParser.ParseAsk(Expr: TList);
var
  ask: PLiToken;
begin
  while FLast^.Sym in [syLParen] do
  begin
    ask := FLast;
    ask^.VParamCount := 1;
    SymGotoNext;
    if FLast^.Sym <> syRParen then
    begin
      ParseExpr(Expr, [syRParen, syComma], true);
      Inc(ask^.VParamCount);
      while FLast^.Sym = syComma do
      begin
        ParseExpr(Expr, [syRParen, syComma], false);
        Inc(ask^.VParamCount);
      end;
    end;
    ask^.Sym := syAsk;
    Expr.Add(ask);
    SymGotoNext;
  end;
end;

procedure KLiParser.ParseExpr(Expr: TList; EndSyms: KLiSymbols; OnHead: boolean);

  function IsOperID(Sym: KLiSymbol): boolean;
  var
    M: KLiSymbol;
  begin
    Result := (Sym in OperIDSyms);
    if Result then
    begin
      M := PeekNextSym;
      if Sym = syBOr then
        Result := not (M in [syMul, syID, syBOr]) else
      if not (M in OperIDSyms + EndSyms) then
        if Sym = syDec then
          Result := M in ExprEndSyms else
          Result := M in ExprEndSyms + [syLParen];
    end;
  end;

  procedure parse_term;
  label
    SYNTAX_BACK;
  var
    JT, JF, L: PLiToken;
    TX, FX: integer;
    hashed: boolean;
    syntax: KLiSyntax;
  begin
    SYNTAX_BACK:
    
    if FCurrent.IsLambdaFunc then
      SymTestLast(ExprHeadSyms) else
      SymTestLast(ExprHeadSyms - [syGetSV]);
      
    if IsOperID(FLast^.Sym) then
    begin
      FLast^.Val := lse_symbol.Symbols[FLast^.Sym].ID;
      FLast^.Sym := syID;
    end;
    L := FLast;

    if L^.Sym = syLambda then
    begin
      ParseLambda;
      SymGotoNext;
      ParseAsk(Expr);
    end
    else
    begin
      SymGotoNext;
      if (L^.Sym = syDec) and (FLast^.Sym in [syFloat, syInt]) then
      begin
        L^.Sym := FLast^.Sym;
        if L^.Sym = syFloat then
          L^.VFloat := - FLast^.VFloat else
          L^.VInteger := - FLast^.VInteger;
        SymGotoNext;
      end;
      
      if L^.Sym in ConstantSyms then
      begin
        Expr.Add(L);
        if L^.Sym = syGetSV then
        begin
          FCurrent.AddSuper;
          FCurrent.ExpandThis := true;
        end;
        if L^.Sym in [syGetEnv, syGetSV] then
          ParseAsk(Expr);
      end
      else
      if L^.Sym = syID then
      begin
        Expr.Add(L); // variable/class/function/...
        ParseAsk(Expr);
      end
      else
      if L^.Sym in [syNot, syDec, syBNot, syFormat] then // 单目操作
      begin
        if L^.Sym = syDec then L^.Sym := syNeg; // 负号
        parse_term;
        Expr.Add(L);
      end
      else
      if L^.Sym = syLParen then
      begin
        ParseExpr(Expr, [syRParen], true);
        SymGotoNext;
        ParseAsk(Expr);
      end
      else
      if L^.Sym = syLBlock then
      begin
        if FLast^.Sym = syRBlock then
        begin
          L^.Sym := syNil;
          Expr.Add(L);
        end
        else
        begin
          if FLast^.Sym = syID then
          begin
            syntax := FindSyntax(FLast^.Val);
            if syntax <> nil then
            begin
              ExpandSyntax(syntax, false);
              SymGotoNext;
              goto SYNTAX_BACK;
            end;
          end;
          ParseExpr(Expr, [syAsk, syRBlock], true);
          if FLast^.Sym = syAsk then
          begin
            JF := FLast;
            JF^.Sym := syJmpFP;
            FX := Expr.Add(JF);
            ParseExpr(Expr, [syDot2], false);
            JT := FLast;
            JT^.Sym := syJump;
            TX := Expr.Add(JT);
            JF^.VParamCount := Expr.Count - FX;
            ParseExpr(Expr, [syRBlock], false);
            JT^.VParamCount := Expr.Count - TX;
          end;
        end;
        SymGotoNext;
        ParseAsk(Expr);
      end
      else
      if L^.Sym = syLArray then // varlist | hashed
      begin
        hashed := false;
        L^.VParamCount := 0;
        while FLast^.Sym <> syRArray do
        begin
          if L^.VParamCount = 0 then
          begin
            ParseExpr(Expr, [syDot2, syComma, syRArray], true);
            hashed := (FLast^.Sym = syDot2); // hashed
            if hashed then
              ParseExpr(Expr, [syComma, syRArray], false);
          end
          else
          begin
            if hashed then
              ParseExpr(Expr, [syDot2], false);
            ParseExpr(Expr, [syComma, syRArray], false);
          end;
          Inc(L^.VParamCount, 1 + Ord(hashed));
        end;
        Expr.Add(L);
        if hashed then
          L^.Sym := syHashed else
          L^.Sym := syVarList;
        SymGotoNext;
      end;
    end;

    ParseDotItem(Expr);
  end;

  procedure parse_fact(Level: integer);
  var
    J, L: PLiToken;
    X: integer;
  begin
    if Level > 0 then
      parse_fact(Level - 1) else
      parse_term;
    J := nil;  // jump record
    X := 0;
    while (FLast^.Sym in ExprOperSyms[Level]) and (PeekNextSym <> syBecome) do
    begin
      L := FLast;
      if Level = High(ExprOperSyms) then  // syAnd, syOr
      begin
        J := CloneSym(L);
        if L^.Sym = syAnd then
          J^.Sym := syJmpF else
          J^.Sym := syJmpT;
        X := Expr.Add(J);
      end;
      SymGotoNext;
      if Level > 0 then
        parse_fact(Level - 1) else
        parse_term;
      Expr.Add(L);
      if Assigned(J) then
      begin
        J^.VParamCount := Expr.Count - X;
        J := nil;
      end;
    end;
  end;

begin
  if not OnHead then SymGotoNext;
  parse_fact(High(ExprOperSyms));
  SymTestLast(EndSyms);
end;

procedure KLiParser.BeginTailIf(var end_if_label: string; EndSym: KLiSymbol);
var
  cond: TList;
begin
  cond := TList.Create;
  try
    end_if_label := FModule.NewLabelName;
    ParseExpr(cond, [EndSym], false);
    CurCodes.LoadExpr(cond);
    CurCodes.AddGoto(end_if_label, FLast^.Pos)^.Sym := syGoFP;
  finally
    cond.Free;
  end;
end;

function KLiParser.CloneSym(sym: PLiToken): PLiToken;
begin
  Result := FSymbols.Next;
  Result^.Sym := sym^.Sym;
  Result^.Pos := sym^.Pos;
  Result^.Val := sym^.Val;
  Result^.VInteger := sym^.VInteger;
end;

procedure KLiParser.ParseDefine;
var
  clss: KLiType;
  data: PLiToken;
  curr: KLiFunc;
begin
  SymTestNextPureID;
  if FModule.Find(FLast^.Val) then
    Error.Redeclared(Self);
  data := FLast;

  SymGotoNext;
  if FLast^.Sym = syDot2 then
  begin
    SymTestNext([syID]);
    clss := ParseVarType(FLast);
    SymGotoNext;                               
  end
  else clss := KT_VARIANT;

  curr := FCurrent;
  try
    FCurrent := KLiFunc.Create(FModule, clss, data^.Val, nil, nil);
    if FLast^.Sym = syBOr then
    begin
      ParseArguments(FCurrent, [syBOr], false);
      SymGotoNext;
    end
    else FCurrent.IsNameCall := true;
    ParseBlock([syRBlock], true);
  finally
    FCurrent := curr;
  end;
end;

procedure KLiParser.ParseDotItem(Expr: TList);
var
  L: PLiToken;
begin
  while FLast^.Sym in [syDot, syLArray] do
  begin
    L := FLast;
    if L^.Sym = syDot then
    begin
      SymGotoNext;
      if not (FLast^.Sym in [FirstKeyword..LastKeyword]) then
        SymTestLastPureID;
      Expr.Add(FLast);
      FLast^.Sym := syStr;
    end
    else ParseExpr(Expr, [syRArray], false);
    L^.Sym := syGetIV;
    L^.VParamCount := 2;
    Expr.Add(L);
    SymGotoNext;
    ParseAsk(Expr);
  end;
end;

procedure KLiParser.EndTailIf(const end_if_label: string);
begin
  CurCodes.AddLabel(end_if_label, FLast^.Pos);
end;

function KLiParser.Error: KLiError;
begin
  Result := FModule.FEngine.FError;
end;

procedure KLiParser.ExpandSyntax(ASyntax: KLiSyntax; IsStatement: boolean);
type
  RTokens = packed record
              name: string;
              list: TList
            end;
var
  lists: array of RTokens;
  index, I: integer;
  token: PLiToken;
  lastsym: KLiSymbol;

  procedure read_to(const ID: string; EndSym: KLiSymbol);
  var
    X, pair: integer;
  begin
    X := Length(lists);
    SetLength(lists, X + 1);
    FillChar(lists[X], sizeof(RTokens), 0);
    lists[X].name := ID;
    lists[X].list := TList.Create;
    pair := 0;
    SymGotoNext;
    while (FLast^.Sym <> EndSym) or (pair > 0) do
    begin
      if FLast^.Sym = syLBlock then Inc(pair) else
      if FLast^.Sym = syRBlock then
      begin
        Dec(pair);
        if pair < 0 then
          Error.SymUnexpected(Self);
      end;
      lists[X].list.Add(FLast);
      SymGotoNext;
    end;
  end;

  function id_index(const ID: string): integer;
  var
    X: integer;
  begin
    for X := 0 to Length(lists) - 1 do
      if ID = lists[X].name then
      begin
        Result := X;
        Exit;
      end;
    Result := -1;
  end;

begin
  SetLength(lists, 0);
  try
    // 1.read source tokens
    if Asyntax.ArgCount > 0 then
    begin
      index := 0;
      while index < ASyntax.ArgCount do
      begin
        token := ASyntax.ArgToken(index);
        if token^.Sym <> syID then
        begin
          SymTestNext([token^.Sym]);
          if index = ASyntax.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else
        if index < ASyntax.ArgCount - 1 then
        begin
          read_to(token^.Val, ASyntax.ArgToken(index + 1)^.Sym);
          Inc(index);
          if index = ASyntax.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else read_to(token^.Val, syRBlock);
        Inc(index);
      end;
    end
    else SymTestNext([syRBlock]);

    // 2.put back to FTokenizer
    if IsStatement then
      FTokenizer.PutBack(FLast); // syRBlock
    ASyntax.RenameLocals;
    lastsym := FLast^.Sym;
    for index := ASyntax.FBody.Count - 1 downto 0 do
    begin
      token := PLiToken(ASyntax.FBody[index]);
      if (token^.Sym = syID) and (token^.Val[1] <> '#') and (lastsym <> syDot) then
      begin
        I := id_index(token^.Val);
        if I >= 0 then
          FTokenizer.PutBack(lists[I].list) else
          FTokenizer.PutBack(token);
      end
      else FTokenizer.PutBack(token);
      lastsym := token^.Sym;
    end;
  finally
    for index := 0 to Length(lists) - 1 do
    begin
      lists[index].name := '';
      lists[index].list.Free;
    end;
    SetLength(lists, 0);
  end;
end;

function KLiParser.FindSyntax(const ID: string): KLiSyntax;
var
  R: KLiFindRec;
  S, M: string;
begin
  M := '';
  S := __decodeTypeName(ID, M);
  if FModule.FindBy(S, M, @R) and (R.fo_type = foSyntax) then
    Result := R.VSyntax else
    Result := nil;
end;

procedure KLiParser.ParseSwitch;
var
  bl, cl: string;
  cx: PLiExprRec;
begin
// --------------------------------------------
// BLOCK: switch expr
//          case expr1: ...
//          case ...  : ...
//          case exprN: ...
//          else        ...
//        end
// --------------------------------------------
  bl := FModule.NewLabelName;
  CurCodes.AddGoto(bl, FLast^.Pos)^.Sym := syRINR;
  FExpr.Clear;
  ParseExpr(FExpr, [syCase], false);
  CurCodes.LoadExpr(FExpr);
  CurCodes.AddNew(sySETV, @FLast^.Pos);
  repeat
    FExpr.Clear;
    ParseExpr(FExpr, [syDot2], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddNew(syCase, @(FLast^.Pos));
    cl := FModule.NewLabelName;
    cx := CurCodes.AddGoto(cl, FLast^.Pos);
    cx^.Sym := syGoFP;
    ParseBlock([syCase, syElse, syRBlock], false);
    if FLast^.Sym <> syRBlock then
    begin
      CurCodes.AddGoto(bl, FLast^.Pos);
      CurCodes.AddLabel(cl, FLast^.Pos);
      if FLast^.Sym = syElse then
        ParseBlock([syRBlock], false);
    end
    else cx^.Name := bl;
  until FLast^.Sym = syRBlock;
  CurCodes.AddLabel(bl, FLast^.Pos);
end;

procedure KLiParser.ParseSyntax;
const
  SR = [FirstKeyword..LastOper] - [syLBlock, syRBlock];
var
  synx: KLiSyntax;
  onid: boolean;
  pair: integer;
begin
// {syntax {name ... } syntax }
  SymTestNext([syLBlock]);
  SymTestNextPureID;
  if FModule.Find(FLast^.Val) then
    Error.Redeclared(Self);
  synx := KLiSyntax.Create(FModule, FLast^.Val);

  onid := false;
  SymGotoNext;
  while FLast^.Sym <> syRBlock do
  begin
    if onid then
    begin
      SymTestLast(SR);
      onid := false;
    end
    else
    begin
      SymTestLast(SR + [syID]);
      if FLast^.Sym = syID then
      begin
        SymTestLastPureID;
        if 'v_' = Copy(FLast^.Val, 1, 2) then // locals
          Error.SymUnexpected(Self); 
        onid := true;
      end
      else onid := false;
    end;
    synx.AddArgument(FLast);
    SymGotoNext;
  end;

  pair := 0;
  SymGotoNext;
  while (FLast^.Sym <> syRBlock) or (pair > 0) do
  begin
    if FLast^.Sym = syLBlock then Inc(pair) else
    if FLast^.Sym = syRBlock then
    begin
      Dec(pair);
      if pair < 0 then
        Error.SymUnexpected(Self); 
    end;
    synx.AddToken(FLast);
    SymGotoNext;
  end;
end;

procedure KLiParser.SaveLabels(var BreakLabel, ContinueLabel: string; CreateNewLabels: boolean);
begin
  BreakLabel := FBreakLabel;
  ContinueLabel := FContinueLabel;
  if CreateNewLabels then
  begin
    FBreakLabel := FModule.NewLabelName;
    FContinueLabel := FModule.NewLabelName;
  end;
end;

function KLiParser.PeekNextSym: KLiSymbol;
var
  token: PLiToken;
begin
  token := FTokenizer.PeekNextToken;
  if token <> nil then
    Result := token^.Sym else
    Result := syError;
end;

function KLiParser.PeekNextTwoSym(var one, two: KLiSymbol): integer;
var
  A, B, C: PLiToken;
begin
  Result := FTokenizer.PeekNextThreeTokens(A, B, C);

  if Result > 0 then
    one := A^.Sym else
    one := syError;

  if Result > 1 then
  begin
    two := B^.Sym;
    Result := 2;
  end
  else two := syError;
end;

procedure KLiParser.RestoreLabels(const BreakLabel, ContinueLabel: string);
begin
  FBreakLabel := BreakLabel;
  FContinueLabel := ContinueLabel;
end;

function KLiParser.GetLastRow: integer;
begin
  Result := FLast^.Pos.row;
end;

function KLiParser.GetLastCol: integer;
begin
  Result := FLast^.Pos.col;
end;

function KLiParser.GetLastVal: string;
begin
  Result := FLast^.Val;
end;

function KLiParser.ParseLambdaFunc: KLiFunc;
begin
  Result := FModule.NewFunc;
  Result.IsLambdaFunc := true;
  FCurrent := Result;
  SymTestNext([syLBlock]);
  SymTestNext([syBOr]);
  ParseArguments(Result, [syBOr], false);
  ParseBlock([syRBlock], false);
end;

function KLiParser.Shadow: KLiParser;
begin
  Result := KLiParser.Create(FModule);
  Result.FTokenizer := FTokenizer;
  Result.FModule := FModule;
  Result.FCurrent := FCurrent;
  Result.FIsShadow := true;
  Result.FLast := FLast;
end;

function KLiParser.ParseLambda: KLiFunc;
var
  parser: KLiParser;
begin
  parser := Shadow;
  try
    Result := parser.ParseLambdaFunc;
    FLast := CloneSym(FLast);
    FLast^.Sym := syID;
    FLast^.Val := Result.FullName;
    FExpr.Add(FLast);
  finally
    parser.Free;
  end;
end;

{ KLiVarb }

constructor KLiVarb.Create(AList: KLiVarbList;
  const AName: string; ValueType: KLiType);
begin
  inherited Create(AName);
  IncRefcount;
  FType := ValueType;
  FList := AList;
  if FList <> nil then
    FIndex := FList.FList.Add(Self);
end;

destructor KLiVarb.Destroy;
begin
  if FList <> nil then
    FList.FList.Remove(Self);
  inherited;
end;

function KLiVarb.Func: KLiFunc;
begin
  if FList <> nil then
    Result := FList.FFunc else
    Result := nil;
end;

function KLiVarb.Prototype(HideType: boolean): string;
begin
  if HideType then
    Result := Name else
    Result := ValueType.Prototype(Name);
end;

procedure KLiVarb.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_VARIABLE, Self);
end;

{ KLiVarbList }

function KLiVarbList.Add(const Name: string; ValueType: KLiType): KLiVarb;
begin
  Result := KLiVarb.Create(Self, Name, ValueType);
end;

procedure KLiVarbList.Clear;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    Delete(index);
end;

constructor KLiVarbList.Create(AFunc: KLiFunc);
begin
  IncRefcount;
  FFunc := AFunc;
  FList := TList.Create;
end;

procedure KLiVarbList.Delete(Index: integer);
var
  varb: KLiVarb;
begin
  varb := GetVarb(Index);
  FList.Delete(Index);
  if varb.FList = Self then
    varb.FList := nil;
  varb.decRefcount;
end;

destructor KLiVarbList.Destroy;
begin
  Clear;
  __freeAndNil(FList);
  inherited;
end;

function KLiVarbList.Exists(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function KLiVarbList.Find(const Name: string): KLiVarb;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X >= 0 then
    Result := GetVarb(X) else
    Result := nil;
end;

function KLiVarbList.FirstIs(AType: KLiType): boolean;
begin
  Result := (GetCount > 0) and (GetVarb(0).FType = AType);
end;

function KLiVarbList.GetCount: integer;
begin
  Result := FList.Count;
end;

function KLiVarbList.GetVarb(Index: integer): KLiVarb;
begin
  Result := KLiVarb(FList[Index]);
end;

function KLiVarbList.IndexOf(const Name: string): integer;
begin
  Result := GetCount - 1;
  while Result >= 0 do
  begin
    if GetVarb(Result).Name = Name then Exit;
    Dec(Result);
  end;
end;

function KLiVarbList.IsParam: boolean;
begin
  Result := (FFunc <> nil) and (Self = FFunc.FParams);
end;

function KLiVarbList.AsString(HideType: boolean): string;
var
  A, index: integer;
begin
  Result := '';
  if GetCount > 0 then
  begin
    index := 0;
    if index < GetCount then
    begin
      Result := GetVarb(index).Prototype(HideType);
      for A := index + 1 to GetCount - 1 do
        Result := Result + ', ' + GetVarb(A).Prototype(HideType);
    end;
  end;
end;

function KLiVarbList.ToVarlist(Engine: KLiEngine): KLiVarList;
var
  index: integer;
begin
  Result := __NewVarlist(Engine);
  for index := 0 to GetCount - 1 do
    Result.Add(GetVarb(index), KR_VARIABLE);
end;

{ KLiFunc }

function KLiFunc.AddSuper: KLiVarb;
var
  X: integer;
begin
  if (FParams.Count = 0) or (FParams[0].Name <> '$') then
  begin
    Result := AddParam('$', KT_VARSNAP);
    FParams.FList.Move(Result.FIndex, 0);
    for X := 0 to FParams.Count - 1 do
      FParams[X].FIndex := X;
    if (FCodes <> nil) and (FCodes.FLocals <> nil) then
      for X := 0 to FCodes.FLocals.Count - 1 do
        FCodes.FLocals[X].FIndex := X + FParams.Count;
  end
  else Result := FParams[0];
end;

function KLiFunc.AddParam(const AName: string; varType: KLiType): KLiVarb;
begin
  Result := FParams.Add(AName, varType);
end;

constructor KLiFunc.Create(Parent: KLiModule; AResultType: KLiType;
  const AName: string; Params: TStringList; Proc: pointer);
var
  A: integer;
begin
  if AName = '' then
    inherited Create(Parent.NewFuncName) else
    inherited Create(AName);

  IncRefCount;

  FModule := Parent;
  FModule.FFuncList.Put(Self);
  if FModule.FFirstFunc = nil then
  begin
    FModule.FFirstFunc := Self;
    FModule.FLastFunc := Self;
  end
  else
  begin
    FPrev := FModule.FLastFunc;
    FPrev.FNext := Self;
    FModule.FLastFunc := Self;
  end;

  FResultType := AResultType;

  FParams := KLiVarbList.Create(Self);
  if Params <> nil then
    for A := 0 to Params.Count - 1 do
      AddParam(Params[A], KLiType(Params.Objects[A]));

  FProc := Proc;
  if FProc = nil then
  begin
    FCodes := KLiExprList.Create(Self);
    FCodes.IncRefcount;
    FCodes.FItems.OnDestroy := {$IFDEF FPC}@{$ENDIF}FCodes.Clear;
    FCodes.FLocals.IncRefcount;
  end;

  if FModule.FEngine <> nil then
    FModule.Engine.AddCompiled(Self);
end;

destructor KLiFunc.Destroy;
begin
  FModule.FFuncList.Remove(Name);
  if FPrev = nil then
    FModule.FFirstFunc := FNext else
    FPrev.FNext := FNext;
  if FNext = nil then
    FModule.FLastFunc := FPrev else
    FNext.FPrev := FPrev;

  if IsMainFunc then
  begin
    Engine.FMainFunc := nil;
    if Engine.FMainSnap <> nil then
    begin
      Engine.FMainSnap.DecRefcount;
      Engine.FMainSnap := nil;
    end;
    Engine.FMainValues.Clear;
    Engine.FTempValues.Clear;
  end;

  if FCodes <> nil then
  begin
    FCodes.FLocals.DecRefcount;
    FCodes.DecRefcount;
    FCodes := nil;
  end;

  if FParams <> nil then
  begin
    if FParams.FFunc = Self then
      FParams.FFunc := nil;
    FParams.DecRefcount;
    FParams := nil;
  end;

  Garbaged;
  inherited;
end;

function KLiFunc.Engine: KLiEngine;
begin
  Result := FModule.FEngine;
end;

function KLiFunc.Execute(Param: PLseParam): boolean;
var
  rnnr: KLiRunner;
  func: KLiFunc;

  procedure invoke_proc;
  begin
    if Assigned(FModule.FInvokeProc) then
      FModule.FInvokeProc(TLseFuncInvoke(FProc), Param) else
      TLseFuncCall(FProc)(Param);
  end;

begin
  try
    rnnr := KLiRunner(Param^.p_runner);
    func := KLiFunc(Param^.p_func);
    if Self <> func then
    begin
      Param^.p_func := Self;
      try
        invoke_proc;
      finally
        Param^.p_func := func;
      end;
    end
    else invoke_proc;
    Result := not (rnnr.Terminated or rnnr.FExcepted);
    if Result then
      __SetTypeValue(rnnr.FEngine, Param^.p_result, FResultType) else
      rnnr.FExcepted := true;
  except
    Result := false;
    __SetError(Param);
  end;
end;

function KLiFunc.FullName: string;
begin
  Result := FModule.Name + '::' + Name;
end;

procedure KLiFunc.Garbaged;
begin

end;

function KLiFunc.Prototype(ShowFullName: boolean): string;
var
  A, X: integer;
  T: KLiType;
  N: string;
begin
  if ShowFullName then
  begin
    if FResultType = KT_VARIANT then
      Result := FullName else
      Result := FResultType.Prototype(FullName);
  end
  else
  if FResultType = KT_VARIANT then
    Result := Name else
    Result := FResultType.Prototype(Name);

  if IsConstFunc then
    Result := 'const ' + Result + ' = do' else
    Result := 'def ' + Result;

  if not IsNameCall then
  begin
    Result := Result + ' |';
    if ExpandThis then
    begin
      X := 1;
      if FParams.Count > X then
        Result := Result + '$, ' else
        Result := Result + '$';
    end
    else X := 0;
    if FParams.Count > X then
    begin
      T := FParams[X].FType;
      if T <> KT_VARIANT then
        N := T.Prototype(FParams[X].Name) else
        N := FParams[X].Name;
      Result := Result + N;
      for A := X + 1 to FParams.Count - 1 do
      begin
        T := FParams[A].FType;
        if T <> KT_VARIANT then
          N := T.Prototype(FParams[A].Name) else
          N := FParams[A].Name;
        Result := Format('%s, %s', [Result, N]);
      end;
    end;
    Result := Result + '|';
  end;
end;

procedure KLiFunc.DumpCode(list: TStrings; const margin: string);
var
  H: string;

  function Add(const S: string): integer;
  begin
    Result := list.Add(margin + S);
  end;

begin
  H := Trim(Description);
  if H <> '' then Add('# ' + H);
  if (FCodes <> nil) and not FCodes.IsEmpty then
  begin
    Add(Prototype(false));
    FCodes.DumpCode(list, margin);
    Add('end');
  end
  else Add(Prototype(false) + ' end');
end;

function KLiFunc.FindBy(const ID: string; rec: PLiFindRec; Range: KLiFindObjects): boolean;
var
  o_name, m_name: string;
begin
  Result := (ID <> '');
  if Result then
  begin
    o_name := __decodeTypeName(ID, m_name);
    if m_name <> '' then
      Result := FModule.FindBy(o_name, m_name, rec) else
      Result := FindInside(o_name, rec) or
                FModule.FindBy(o_name, '', rec);
    if Result then
      Result := (Range = []) or (rec^.fo_type in Range);
  end;
end;

function KLiFunc.FindCreate(AType: KLiType): KLiFunc;
begin
  Result := FindMethod('create', AType);
  if (Result <> nil) and (Result.FResultType <> AType) then
    Result := nil;
  if Result = nil then
  begin
    Result := AType.FModule.FindFunc(AType.Name + '_create');
    if (Result <> nil) and (Result.FResultType <> AType) then
      Result := nil;
  end;
end;

function KLiFunc.FindMethod(const AName: string; AType: KLiType): KLiFunc;
var
  N: string;
  M, T: KLiModule;
  F: KLiFunc;
  X: integer;
begin
  N := AType.Name + '_' + AName;
  M := Module;

  F := M.FindFunc(N);
  if (F <> nil) and F.FParams.FirstIs(AType) then
  begin
    Result := F;
    Exit;
  end;

  if M.FModules <> nil then
  begin
    for X := 0 to M.FModules.Count - 1 do
    begin
      T := M.FModules[X];
      if T <> M then
      begin
        F := T.FindFunc(N);
        if (F <> nil) and F.FParams.FirstIs(AType) then
        begin
          Result := F;
          Exit;
        end;
      end;
    end;
    if M.FModules.Find('sys') = nil then
    begin
      F := sys_module.FindFunc(N);
      if (F <> nil) and F.FParams.FirstIs(AType) then
      begin
        Result := F;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function KLiFunc.HasState(Index: KLiFuncState): boolean;
begin
  Result := (Index in FState);
end;

procedure KLiFunc.Satisfy;
begin
  if FCodes <> nil then
    if (FCodes.GetCount = 0) and not IsMainFunc then
    begin
      FProc := @udc_empty;
      SetState(fusEmpty, true);
      FCodes.DecRefcount;
      FCodes := nil;
    end
    else FCodes.Satisfy;
end;

procedure KLiFunc.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_FUNC, Self);
end;

procedure KLiFunc.SetState(Index: KLiFuncState; Value: boolean);
begin
  if Value then
    Include(FState, Index) else
    Exclude(FState, Index);
end;

function KLiFunc.FindInside(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if rec = nil then rec := @findrec;
  if ID = Name then
  begin
    rec^.fo_type := foFunc;
    rec^.VFunc := Self;
  end
  else
  begin
    rec^.VVarb := FParams.Find(ID);
    if (rec^.VVarb = nil) and (FCodes <> nil) then
      rec^.VVarb := FCodes.FLocals.Find(ID);
    if rec^.VVarb <> nil then
      rec^.fo_type := foVarb else
      rec^.fo_type := foNone;
  end;
  Result := (rec^.fo_type <> foNone);
end;

{ KLiExprList }

function KLiExprList.Add(AExprRec: PLiExprRec): integer;
begin
  Result := FItems.Add(AExprRec);
end;

function KLiExprList.AddGoto(const Name: string; Pos: KLiSymPos): PLiExprRec;
begin
  Result := FFunc.FCodes.AddNew(syGoto, @Pos);
  Result^.Name := Name;
  Result^.VLabel := nil;
end;

function KLiExprList.AddLabel(const Name: string; Pos: KLiSymPos): PLiExprRec;
begin
  Result := FFunc.FCodes.AddNew(syLabel, @Pos);
  Result^.VOffset := -1;
  Result^.Name := Name;
  Result^.flags := Result^.flags + [xrSatisfied];
end;

function KLiExprList.AddLocal(const Name: string; varType: KLiType): KLiVarb;
begin
  Result := FLocals.Add(Name, varType);
  Inc(Result.FIndex, FFunc.FParams.Count);
end;

function KLiExprList.AddTry(const Name: string; Pos: KLiSymPos): PLiExprRec;
begin
  Result := AddGoto(Name, Pos);
  Result^.Sym := syTry;
end;

procedure KLiExprList.EndStatement;
begin
  if GetCount > 0 then
    if not (GetLast^.Sym in [syReturn, sySTMT, syLabel, syEcho]) then
      AddNew(sySTMT, nil);
end;

function KLiExprList.ChangeGoto(const OrgLabel, NewLabel: string): integer;
var
  X: integer;
  R: PLiExprRec;
begin
  Result := 0;
  for X := 0 to GetCount - 1 do
  begin
    R := GetItem(X);
    if (R^.Sym in GotoSyms) and (R^.Name = OrgLabel) then
    begin
      R^.Name := NewLabel;
      R^.VLabel := nil;
      Inc(Result);
    end;
  end;
end;

procedure KLiExprList.Clear(Sender: TObject);
var
  index: integer;
  V: KLiVarb;
  F: boolean;
begin
  FSatisfyIndex := 0;
  try
    for index := 0 to FItems.Count - 1 do
      __FreeExprec(FItems[index]);
    if FLocals <> nil then
    begin
      F := false;
      for index := FLocals.Count - 1 downto 0 do
      begin
        V := FLocals[index];
        if V.Name[1] = '#' then
        begin
          FLocals.Delete(index);
          F := true;
        end;
      end;
      if F then
        for index := 0 to FLocals.Count - 1 do
          FLocals[index].FIndex := FFunc.FParams.Count + index;
    end;
  finally
    FItems.Clear;
  end;
end;

constructor KLiExprList.Create(AFunc: KLiFunc);
begin
  FFunc := AFunc;
  FItems := KLiList.Create;
  FItems.IncRefcount;
  FLocals := KLiVarbList.Create(FFunc);
end;

procedure KLiExprList.Delete(Index: integer);
var
  A, X: integer;
  R: PLiExprRec;
begin
  for A := 0 to GetCount - 1 do
    if A <> Index then
    begin
      R := PLiExprRec(FItems[A]);
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFP, syJmpTP] then
      begin
        X := A + R^.VOffset;
        if (A < Index) and (X > Index) then Dec(R^.VOffset) else
        if (A > Index) and (X < Index) then Inc(R^.VOffset);
      end
      else
      if (A > Index) and (R^.Sym = syLabel) then
        R^.VOffset := A - 1;
    end;
  __FreeExprec(FItems[Index]);
  FItems.Delete(Index);
  if Index <= FSatisfyIndex then
    Dec(FSatisfyIndex);
end;

destructor KLiExprList.Destroy;
begin
  FLocals.FFunc := nil;
  FLocals.DecRefcount;
  FLocals := nil;
  Clear;
  FItems.DecRefcount;
  FItems := nil;
  inherited;
end;

procedure KLiExprList.DumpCode(List: TStrings; const Margin: string);
var
  A: integer;
  R: PLiExprRec;
  L: TList;
  H: string;

  function Log(const S: string): integer;
  begin
    Result := List.Add(Margin + S);
  end;

begin
  for A := 0 to FLocals.Count - 1 do
    Log(Format('     VARB %s: %s',
      [FLocals[A].Name, FLocals[A].ValueType.Name]));
      
  L := TList.Create;
  try
    for A := 0 to GetCount - 1 do
    begin
      R := GetItem(A);
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFP, syJmpTP] then
        L.Add(pointer(A + R^.VOffset));
    end;

    for A := 0 to GetCount - 1 do
    begin
      R := GetItem(A);
      case R^.Sym of
        syReturn    : if R^.ParamCount > 0 then
                        H := 'RETL' else // return last
                        H := 'EXIT';
        syJump      : H := Format('JUMP &%.4d:', [A + R^.VOffset]);
        syJmpF      : H := Format('JMPF &%.4d:', [A + R^.VOffset]);
        syJmpT      : H := Format('JMPT &%.4d:', [A + R^.VOffset]);
        syJmpFP     : H := Format('JMPF &%.4d: POP', [A + R^.VOffset]);
        syJmpTP     : H := Format('JMPT &%.4d: POP', [A + R^.VOffset]);
        syCall      : H := Format('CALL %s: %d', [R^.VFunc.FullName, R^.ParamCount]);
        syAsk       : H := Format('CASK [%d]', [R^.ParamCount]);
        syIdle      : H := 'IDLE';
        syPress     : if R^.ParamCount > 1 then
                        H := Format('POPL  [%d]', [R^.ParamCount]) else
                        H := 'POPL';
        syID        : H := Format('PUSH %s', [R^.VVarb.Name]);
        syBecome    : H := Format('SAVE %s', [R^.VVarb.Name]);
        syFloat     : H := Format('PSHF %f', [R^.VFloat]);
        syInt       : H := Format('PSHI %d', [R^.VInteger]);
        syStr       : H := Format('PSHS %s', [__strToComma(lse_strec_data(R^.VStr))]);
        syTry       : if xrInFinally in R^.flags then
                        H := Format('TRYF %s', [R^.Name]) else
                        H := Format('TRYC %s', [R^.Name]);
        syEcho      : H := Format('ECHO [%d]', [R^.ParamCount]);
        syNeg       : H := 'CALC NEG';
        syAdd       : H := 'CALC +';
        syFill      : H := 'CALC <<<';
        syDec       : H := 'CALC -';
        syMul       : H := 'CALC *';
        syDiv       : H := 'CALC /';
        syMod       : H := 'CALC %';
        syBNot      : H := 'CALC ~';
        syBXor      : H := 'CALC ^';
        syBOr       : H := 'CALC |';
        syBAnd      : H := 'CALC &';
        syBShl      : H := 'CALC <<';
        syBShr      : H := 'CALC >>';
        syEQ        : H := 'CALC ==';
        syIn        : H := 'CALC IN';
        syNE        : H := 'CALC !=';
        syLess      : H := 'CALC <';
        syLE        : H := 'CALC <=';
        syMore      : H := 'CALC >';
        syME        : H := 'CALC >=';
        syNot       : H := 'CALC NOT';
        syAnd       : H := 'CALC AND';
        syOr        : H := 'CALC OR';
        syType      : H := Format('PUSH CLASS: %s', [R^.VType.FullName]);
        syFunc      : H := Format('PUSH FUNC: %s', [R^.VFunc.FullName]);
        syVarList   : H := Format('LIST [%d]', [R^.ParamCount]);
        syNil       : H := 'PUSH NIL';
        syGetEnv    : H := Format('PUSH ${%s}', [R^.Name]);
        syGetSV     : H := Format('PUSH $%s', [R^.Name]);
        sySetSV     : H := Format('SAVE $%s', [R^.Name]);
        syFormat    : H := 'FRMT';
        syLabel     : H := R^.Name;
        syGoto      : H := Format('GOTO %s', [R^.Name]);
        syGoTP      : H := Format('GOTP %s', [R^.Name]);
        syGoFP    : H := Format('GOFP %s', [R^.Name]);
        syModule    : H := Format('PSHM %s', [R^.VType.Module.Name]);
        syIs        : H := 'CALC IS';
        syAs        : H := 'CALC AS';
        sySTMT      : H := 'STMT';
        syVarGen    : H := 'VGEN';
        syHashed    : H := Format('HASH [%d]', [R^.ParamCount]);
        syRINR      : H := Format('RINR %s', [R^.Name]);
        syGETV      : H := 'GETV';
        sySETV      : H := 'SETV';
        syLike      : H := 'LIKE';
        syGetIV     : H := 'GETI';
        sySetIV     : H := 'SETI';
        syDupLast   : H := 'DUPL';
        sySend      : H := Format('SEND %s', [R^.VVarb.Name]);
        syCase      : H := 'CASE';
        else          H := 'WRNG: ' + Symbols[R^.sym].SM;
      end;

      if L.IndexOf(pointer(A)) >= 0 then
        H := Format('%.4d:%s', [A, H]) else
        H := '     ' + H;
      Log(H);
    end;
    if L.IndexOf(pointer(GetCount)) >= 0 then
      Log(Format('%.4d:', [GetCount]));
  finally
    L.Free;
  end;
end;

function KLiExprList.FindLabel(const Name: string): PLiExprRec;
var
  X: integer;
begin
  for X := 0 to GetCount - 1 do
  begin
    Result := GetItem(X);
    if Result^.Sym = syLabel then
      if Result^.Name = Name then
        Exit;
  end;
  Result := nil;
end;

function KLiExprList.GetCount: integer;
begin
  if Self <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function KLiExprList.GetEngine: KLiEngine;
begin
  Result := FFunc.FModule.FEngine;
end;

function KLiExprList.GetError: KLiError;
begin
  Result := GetEngine.FError;
end;

function KLiExprList.GetItem(Index: integer): PLiExprRec;
begin
  Result := PLiExprRec(FItems[Index]);
end;

function KLiExprList.GetLast: PLiExprRec;
begin
  Result := PLiExprRec(FItems.Last);
end;

function KLiExprList.GetLastIndex: integer;
begin
  Result := GetCount - 1;
end;

function KLiExprList.GetModule: KLiModule;
begin
  Result := FFunc.FModule;
end;

procedure KLiExprList.Insert(Index: integer; AExprRec: PLiExprRec);
var
  A, X: integer;
  R: PLiExprRec;
begin
  if Index < GetCount then
  begin
    for A := 0 to GetCount - 1 do
    begin
      R := PLiExprRec(FItems[A]);
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFP, syJmpTP] then
      begin
        X := A + R^.VOffset;
        if (A < Index) and (X >= Index) then Inc(R^.VOffset) else
        if (A >= Index) and (X < Index) then Dec(R^.VOffset);
      end
      else
      if (A >= Index) and (R^.Sym = syLabel) then
        R^.VOffset := A + 1;
    end;
    FItems.Insert(Index, AExprRec);
    if Index <= FSatisfyIndex then
      Inc(FSatisfyIndex);
  end
  else FItems.Add(AExprRec);
end;

function KLiExprList.InsertNew(Index: integer; sym: KLiSymbol; SymPos: PLiSymPos): PLiExprRec;
begin
  Result := __NewExprec;
  try
    Result^.Sym := sym;
    if SymPos <> nil then
      Result^.Pos := SymPos^;
    Insert(Index, Result);
  except
    __FreeExprec(Result);
    raise;
  end;
end;

function KLiExprList.IsEmpty: boolean;
begin
  Result := (GetCount = 0) and (FLocals.Count = 0);
end;

function KLiExprList.LastLabel: string;
begin
  if (GetCount > 0) and (GetLast^.Sym = syLabel) then
    Result := GetLast^.Name else
    Result := '';
end;

procedure KLiExprList.LoadExpr(List: TList; Start: integer);
var
  index: integer;
begin
  for index := Start to List.Count - 1 do
    LoadToken(List[index]);
end;

procedure KLiExprList.LoadToken(token: PLiToken);
var
  exprec: PLiExprRec;
begin
  exprec := AddNew(syError, @(token^.Pos));
  exprec^.Sym := token^.Sym;
  if exprec^.Sym = syFloat then exprec^.VFLoat := token^.VFloat else
  if exprec^.Sym = syInt   then exprec^.VInteger := token^.VInteger else
  if exprec^.Sym = syStr   then
  begin
    exprec^.VStr := lse_strec_alloc(token^.Val);
    lse_strec_inclife(exprec^.VStr);
  end
  else
  if exprec^.Sym in [syJump, syJmpT, syJmpF, syJmpTP, syJmpFP] then
    exprec^.VOffset := token^.VParamCount
  else
  begin
    exprec^.Name := token^.Val;
    exprec^.ParamCount := token^.VParamCount;
  end;
end;

procedure KLiExprList.PushVarb(AVarb: KLiVarb; Pos: KLiSymPos);
var
  expr: PLiExprRec;
begin
  expr := AddNew(syID, @Pos);
  expr^.Name := AVarb.Name;
  expr^.VVarb := AVarb;
  expr^.flags := expr^.flags + [xrSatisfied];
end;

function KLiExprList.AddNew(sym: KLiSymbol; SymPos: PLiSymPos): PLiExprRec;
begin
  Result := __NewExprec;
  Result^.Sym := sym;
  if SymPos <> nil then
    Result^.Pos := SymPos^;
  Add(Result);
end;

procedure KLiExprList.Satisfy;
var
  stack: KLiSatisfy;
  exprec: PLiExprRec;
  exprec_func: KLiFunc;

  procedure exec_push(K: KLiType);
  begin
    stack.Add(K, exprec, exprec_func);
  end;
  
  procedure exec_last(K: KLiType; backward: integer);
  begin
    stack.Press(backward);
    exec_push(K);
  end;

  function find_varb: KLiType;
  label FAILURE;
  var
    vname: string; // variant name
    mname: string; // module name
    R: KLiFindRec;
    clss: KLiType;
  begin
    vname := __decodeTypeName(exprec^.Name, mname);
    if not (((mname = '') and FFunc.FindInside(vname, @R))
       or FFunc.FModule.FindBy(vname, mname, @R)) then
         goto FAILURE;

    if exprec^.Sym = syBecome then
      if R.fo_type = foVarb then
      begin
        exprec^.VVarb := R.VVarb;
        exprec^.Name := exprec^.VVarb.Name;
        Result := exprec^.VVarb.ValueType;
        Exit;
      end
      else goto FAILURE;

    if R.fo_type = foVarb then
    begin
      exprec^.VVarb := R.VVarb;
      exprec^.Name := exprec^.VVarb.Name;
      Result := exprec^.VVarb.ValueType;
      Exit;
    end;

    if R.fo_type = foFunc then
    begin
      exprec^.VFunc := R.VFunc;
      exprec^.Name := exprec^.VFunc.Name;
      if exprec^.VFunc.IsNameCall then
      begin
        exprec^.Sym := syCall;
        exprec^.ParamCount := 0;
        Result := exprec^.VFunc.ResultType;
      end
      else
      begin
        exprec^.Sym := syFunc;
        Result := KT_FUNC;
      end;
      exprec_func := exprec^.VFunc;
      Exit;
    end;

    // 2. push class or module
    if R.fo_type = foType then
    begin
      clss := R.VType;
      exprec^.VType := clss;
      exprec^.Name := clss.Name;
      exprec^.Sym := syType;
      Result := KT_CLASS;
      Exit;
    end;

    if R.fo_type = foModule then
    begin
      exprec^.VModule := R.VModule;
      exprec^.Sym := syModule;
      exprec^.Name := R.VModule.Name;
      Result := KT_MODULE;
      Exit;
    end;

  FAILURE:
    Result := nil;
    Error.ObjectNotExists(FFunc, exprec);
  end;

  procedure exec_call;
 {var
    index: integer;
    clss: KLiType;
    mode, name: string;}
  begin
    if not (xrSatisfied in exprec^.flags) then
    begin
     {index := stack.Count - exprec^.ParamCount;
      clss := stack.Types[index];
      mode := Copy(exprec^.Name, 1, 7);
      name := Copy(exprec^.Name, 8, MaxInt);}
      Error.FuncNotFound(FFunc, exprec);
    end
    else
    begin
      exprec_func := exprec^.VFunc;
      exec_last(exprec^.VFunc.FResultType, exprec^.ParamCount);
    end;
  end;

  procedure exec_inc;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_inc(L, R), 2);
  end;

  procedure exec_dec;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_dec(L, R), 2);
  end;

  procedure exec_mul;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_mul(L, R), 2);
  end;

  procedure exec_div;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_div(L, R), 2);
  end;
  
  procedure exec_mod;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_mod(L, R), 2);
  end;

  procedure exec_bit_xor;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_xor(L, R), 2);
  end;

  procedure exec_bit_or;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_or(L, R), 2);
  end;

  procedure exec_bit_and;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_and(L, R), 2);
  end;

  procedure exec_bit_shl;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_shl(L, R), 2);
  end;

  procedure exec_bit_shr;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_shr(L, R), 2);
  end;

  procedure exec_neg;
  begin
    exec_last(__type_neg(stack.Types[stack.Count - 1]), 1);
  end;

  procedure exec_bit_not;
  begin
    exec_last(__type_not(stack.Types[stack.Count - 1]), 1);
  end;

  procedure exec_compare;
  begin
    exec_last(KT_INT, 2);
  end;

  procedure exec_and_or;
  var
    L, R: KLiType;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    if L = R then
      exec_last(L, 2) else
      exec_last(KT_VARIANT, 2);
  end;
  
  procedure exec_goto;
  var
    expr: PLiExprRec;
  begin
    expr := FindLabel(exprec^.Name);
    if expr = nil then
      Error.LabelNotExists(FFunc, exprec);
    exprec^.VLabel := expr;
    if exprec^.Sym in [syGoTP, syGoFP] then
      stack.Press(1);
  end;

  procedure exec_ask;
  var
    base: integer;
    clss: KLiType;
    func: KLiFunc;
    expr: PLiExprRec;
  begin
    base := stack.Count - exprec^.ParamCount;
    clss := stack.Types[base];
    if clss = KT_CLASS then
    begin
      expr := stack.Exprs[base];
      if expr^.Sym = syType then
        clss := expr^.VType else
        clss := KT_VARIANT;
    end
    else
    if clss = KT_FUNC then
    begin
      func := stack.Funcs[base];
      if func <> nil then
      begin
        expr := stack.Exprs[base];
        if expr^.Sym = syFunc then
        begin
          expr^.Sym := syIDLE;
          expr^.Name := '';
          exprec^.Sym := syCall;
          exprec^.VFunc := func;
          exprec^.Name := func.Name;
          exprec_func := func;
          Dec(exprec^.ParamCount);
          stack.Press;
        end;
        clss := func.FResultType;
      end
      else clss := KT_VARIANT;
    end
    else
    if clss <> KT_VARIANT then
      Error.CanNotAsk(FFunc, stack.Exprs[base], clss);
    exec_last(clss, exprec^.ParamCount);
  end;

  procedure exec_curry;
  begin
    exec_last(KT_FUNC, 1);
  end;

  procedure reset_label_offset;
  var
    X, I: integer;
    R, L, M: PLiExprRec;
  begin
    L := nil;
    for X := 0 to GetCount - 1 do
    begin
      R := GetItem(X);
      if R^.Sym = syLabel then
      begin
        R^.VOffset := X;
        if (L <> nil) and (L^.Sym = syLabel) then
        begin
          for I := 0 to GetCount - 1 do
          begin
            M := GetItem(I);
            if (M^.Sym in GotoSyms) and (M^.VLabel = L) then
            begin
              M^.VLabel := R;
              M^.Name := R^.Name;
            end;
          end;
          L^.Sym := syIDLE;
        end;
      end;
      L := R;
    end;
  end;

begin
  if (Self <> nil) and (FSatisfyIndex < GetCount) then
  begin
    stack := KLiSatisfy.Create;
    try
      repeat
        exprec_func := nil;
        exprec := GetItem(FSatisfyIndex);
        case exprec^.Sym of
          syID       : exec_push(find_varb);
          syFloat    : exec_push(KT_FLOAT);
          syInt      : exec_push(KT_INT);
          syStr      : exec_push(KT_STRING);
          syBecome   : find_varb;
          syCall     : exec_call;
          syNil      : exec_push(KT_VOID);
          syAdd      : exec_inc;
          syFill     : stack.Press;
          syDec      : exec_dec;
          syMul      : exec_mul;
          syDiv      : exec_div;
          syMod      : exec_mod;
          syNeg      : exec_neg;
          syBXor     : exec_bit_xor;
          syBOr      : exec_bit_or;
          syBAnd     : exec_bit_and;
          syBShl     : exec_bit_shl;
          syBShr     : exec_bit_shr;
          syBNot     : exec_bit_not;
          syEQ       : exec_compare;
          syNE       : exec_compare;
          syLess     : exec_compare;
          syLE       : exec_compare;
          syMore     : exec_compare;
          syME       : exec_compare;
          syIn       : exec_compare;
          syLike     : exec_compare;
          syAnd      : exec_and_or;
          syOr       : exec_and_or;
          syIs       : exec_last(KT_INT, 2);
          syAs       : exec_last(stack.LastType, 2);
          syNot      : exec_last(KT_INT, 1);
          syReturn   : stack.Clear;
          syJmpTP    : stack.Press;
          syJmpFP    : stack.Press;
          syPress    : stack.Press;
          syEcho     : stack.Press(exprec^.ParamCount);
          syVarList  : exec_last(KT_VARLIST, exprec^.ParamCount);
          syHashed   : exec_last(KT_HASHED, exprec^.ParamCount);
          syVarGen   : exec_last(KT_VARGEN, 1);
          syFunc     : stack.Add(KT_FUNC, exprec, exprec^.VFunc);
          syGetEnv   : exec_push(KT_VARIANT);
          syGetSV    : exec_push(KT_VARIANT);
//        sySetSV    :;
          syFormat   : exec_last(KT_STRING, 1);

          syTry      : exec_goto;
          syGoto     : exec_goto;
          syGoTP     : exec_goto;
          syGoFP     : exec_goto;
          syRINR     : exec_goto;
          syLabel    : stack.Clear;
          
          syAsk      : exec_ask;
          sySTMT     : stack.Clear;
          syGETV     : exec_push(KT_VARIANT);
          sySETV     : stack.Press;
          syGetIV    : exec_last(KT_VARIANT, 2);
          sySetIV    : stack.Press(2);
          syDupLast  : stack.DupLast(exprec^.ParamCount);
          sySend     : exec_push(KT_INT);
          syCase     : exec_last(KT_INT, 1);
        end;
        exprec^.flags := exprec^.flags + [xrSatisfied];
        Inc(FSatisfyIndex);
      until FSatisfyIndex >= GetCount;
    finally
      stack.Free;
    end;
    reset_label_offset;
  end;
end;

{ KLiFunc_curry }

function KLiFunc_curry.AddCurry(value: PLseValue): integer;
begin
  Result := Length(FCurry);
  SetLength(FCurry, Result + 1);
  FCurry[Result] := __NewNil;
  if value <> nil then
    lse_set_value(FCurry[Result], value);
end;

constructor KLiFunc_curry.Create(AModule: KLiModule; const AName: string; AFunc: KLiFunc);
var
  f_name: string;
begin
  if AName = '' then
    f_name := AModule.NewTempID('func') else 
    f_name := AName;
  inherited Create(AModule, AFunc.FResultType, f_name, nil, @udc_curry);
  FCurryFunc := AFunc;
  FCurryFunc.IncRefcount;
  IsCurryFunc := true;
  FObjRec.or_object := Self;
  FObjRec.or_type := KT_FUNC;
  Engine.OrEnter(@FObjRec);
end;

destructor KLiFunc_curry.Destroy;
begin
  Engine.OrLeave(@FObjRec);
  inherited;
end;

procedure KLiFunc_curry.Garbaged;
var
  index: integer;
begin
  for index := Length(FCurry) - 1 downto 0 do
  begin
    __FreeValue(FCurry[index]);
    FCurry[index] := nil;
  end;

  SetLength(FCurry, 0);
  
  if FCurryFunc <> nil then
  begin
    FCurryFunc.DecRefcount;
    FCurryFunc := nil;
  end;
end;

function KLiFunc_curry.GetCurryCount: integer;
begin
  Result := Length(FCurry);
end;

function KLiFunc_curry.GetCurryData(Index: integer): PLseValue;
begin
  Result := FCurry[Index];
end;

function KLiFunc_curry.GetObjRec: PLiObjRec;
begin
  Result := @FObjRec;
end;

{ KLiFunc_operator }

constructor KLiFunc_operator.Create(AOper: KLiSymbol);
begin
  inherited Create(sys_module, KT_VARIANT,
    Symbols[AOper].ID, nil, @udc_oper);
  AddParam('V1', KT_VARIANT);
  AddParam('V2', KT_VARIANT);
  FOper := AOper;
end;

{ KLiType }

function KLiType.Cast(Param: PLseParam; Index: integer): PLseValue;
begin
  Result := __SetTypeValue(__AsEngine(Param), Param^.p_param[Index], Self)
end;

constructor KLiType.Create(AModule: KLiModule; const AName: string; AType: TLseValue);
begin
  inherited Create(AName);
  IncRefcount;
  FModule := AModule;
  FModule.FTypeList.AddObject(Name, Self);
  FTypeRec := lse_mem_alloc_zero(sizeof(RLseType));
  FTypeRec^.cr_type := AType;
end;

destructor KLiType.Destroy;
begin
  qe_entries.cik_types^[TLseKernelType(Ord(DataType))] := nil;
  __removeFrom(Module.FTypeList, Self);
  lse_mem_free(FTypeRec, sizeof(RLseType));
  inherited;
end;

function KLiType.GetDataType: TLseValue;
begin
  Result := FTypeRec^.cr_type;
end;

function KLiType.GetDescription: string;
begin
  Result := FTypeRec^.cr_desc;
end;

function KLiType.GetFullName: string;
begin
  Result := FModule.Name + '::' + Name;
end;

function KLiType.IsSimpleType: boolean;
begin
  Result := (FTypeRec^.cr_type <> LSV_OBJECT);
end;

function KLiType.ObjectToString(obj: pointer): string;
var
  sr: PLseString;
begin
  if Assigned(FTypeRec^.cr_otos) then
  begin
    sr := FTypeRec^.cr_otos(obj);
    lse_strec_inclife(sr);
    try
      Result := lse_strec_string(sr);
    finally
      lse_strec_declife(sr);
    end;
  end
  else Result := '';
end;

function KLiType.Prototype(const ID: string): string;
begin
  Result := ID + ':' + Name;
end;

procedure KLiType.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_CLASS, Self);
end;

function KLiType.StrecToObject(const S: PLseString; Engine: KLiEngine): pointer;
begin
  if Assigned(FTypeRec^.cr_stoo) then
    Result := FTypeRec^.cr_stoo(S, Engine) else
    Result := nil;
end;

{ KLiSyntax }

procedure KLiSyntax.AddArgument(Arg: PLiToken);
var
  X: integer;
begin
  X := Length(FArgs);
  SetLength(FArgs, X + 1);
  FArgs[X] := clone_token(Arg);
end;

procedure KLiSyntax.AddToken(Token: PLiToken);
var
  lastsym: KLiSymbol;
begin
  Token := clone_token(Token);
  FBody.Add(Token);
  if IsPureID(Token) and ('v_' = Copy(Token^.Val, 1, 2)) then
  begin
    if FBody.Count > 1 then
      lastsym := PLiToken(FBody[FBody.Count - 2])^.Sym else
      lastsym := syID;
    if lastsym <> syDot then
    begin
      if FLocals = nil then
        FLocals := TList.Create;
      FLocals.Add(Token);
    end;
  end;
end;

function KLiSyntax.ArgCount: integer;
begin
  Result := Length(FArgs);
end;

function KLiSyntax.ArgToken(Index: integer): PLiToken;
begin
  Result := FArgs[Index];
end;

constructor KLiSyntax.Create(AModule: KLiModule; const AName: string);
begin
  inherited Create(AName);
  FRenameCount := 0;
  FModule := AModule;
  if FModule.FFirstSyntax = nil then
  begin
    FModule.FFirstSyntax := Self;
    FModule.FLastSyntax := Self;
  end
  else
  begin
    FPrev := FModule.FLastSyntax;
    FPrev.FNext := Self;
    FModule.FLastSyntax := Self;
  end;
  FBody := TList.Create;
  FModule.FEngine.AddCompiled(Self);
end;

destructor KLiSyntax.Destroy;
var
  X: integer;
begin
  if FPrev = nil then
    FModule.FFirstSyntax := FNext else
    FPrev.FNext := FNext;
  if FNext = nil then
    FModule.FLastSyntax := FPrev else
    FNext.FPrev := FPrev;

  for X := Length(FArgs) - 1 downto 0 do
    free_token(FArgs[X]);
  SetLength(FArgs, 0);

  for X := FBody.Count - 1 downto 0 do
    free_token(PLiToken(FBody[X]));
  FBody.Free;
  FreeAndNil(FLocals);

  inherited;
end;

procedure KLiSyntax.RenameLocals;
var
  X, I: integer;
  T: PLiToken;
  org_name, new_name: string;
begin
  if FLocals <> nil then
  begin
    for X := FLocals.Count - 1 downto 0 do
    begin
      T := PLiToken(FLocals[X]);
      T^.next := nil;
    end;

    Inc(FRenameCount);

    for X := FLocals.Count - 1 downto 0 do
    begin
      T := PLiToken(FLocals[X]);
      if T^.next = nil then
      begin
        T^.next := T;
        org_name := T^.Val;
        new_name := Format('#%p_%X', [T, FRenameCount]);
        T^.Val := new_name;
        for I := X - 1 downto 0 do
        begin
          T := PLiToken(FLocals[I]);
          if (T^.next = nil) and (T^.Val = org_name) then
          begin
            T^.next := T;
            T^.Val := new_name;
          end;
        end;
      end;
    end;
  end;
end;

{ KLiVarList }

destructor KLiVarList.Destroy;
begin
  FEngine.OrLeave(@FObjRec);
  Clear;
  FreeAndNil(FItems);
end;

function KLiVarList.Add: PLseValue;
begin
  Result := __NewNil;
  FItems.Add(Result);
end;

function KLiVarList.AddDefault(Klass: PLseType): PLseValue;
begin
  Result := __NewNil;
  Result^.vtype := Klass;
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: double): PLseValue;
begin
  Result := __NewFloat(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: int64): PLseValue;
begin
  Result := __NewInt64(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: pointer; Klass: PLseType): PLseValue;
begin
  Result := __NewObject(Value, Klass);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: PLseString): PLseValue;
begin
  Result := __NewStrec(Value);
  FItems.Add(Result);
end;

function KLiVarList.AddSend(VG: PLseVargen): boolean;
begin
  Result := lse_vargen_send(VG, Add);
end;

function KLiVarList.AddStrings(List: TStrings): integer;
var
  X: integer;
begin
  Result := List.Count;
  for X := 0 to Result - 1 do
    Add(List[0]);
end;

function KLiVarList.AddAll(VG: PLseVargen): integer;
begin
  Result := 0;
  while not lse_vargen_eof(VG) do
  begin
    Inc(Result);
    lse_vargen_send(VG, Add);
  end;
end;

function KLiVarList.Add(const Value: string): PLseValue;
begin
  Result := __NewStr(Value);
  FItems.Add(Result);
end;

function KLiVarList.AddFrom(List: KLiVarList; ItemCount: integer): integer;
var
  index: integer;
begin
  if (List <> nil) and (List <> Self) then
  begin
    if ItemCount = 0 then
      Result := List.Count else
      Result := Min(ItemCount, List.Count);
    for index := 0 to Result - 1 do
      Add(List.GetData(index));
  end
  else Result := 0;
end;

function KLiVarList.Right(ItemCount: integer): KLiVarList;
begin
  Result := Copy(GetCount - ItemCount, ItemCount);
end;

function KLiVarList.AsString: string;
var
  S: TStringStream;
  X: integer;

  procedure write_value(V: PLseValue);
  begin
    lse_stream_write(S, __AsString(V));
  end;
  
begin
  S := TStringStream.Create('');
  try
    lse_stream_write(S, '[');
    if GetCount > 0 then
    begin
      write_value(GetData(0));
      for X := 1 to GetCount - 1 do
      begin
        lse_stream_write(S, ',');
        write_value(GetData(X));
      end;
    end;
    lse_stream_write(S, ']');
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure KLiVarList.Clear;
begin
  Press(GetCount);
end;

function KLiVarList.Copy(Index, ItemCount: integer): KLiVarList;
var
  L: integer;
  V: PLseValue;
begin
  Result := KLiVarList.Create(FEngine);
  if Index < 0 then
  begin
    Inc(ItemCount, Index);
    Index := 0;
  end;
  if ItemCount > 0 then
  begin
    L := GetCount;
    if Index < L then
    begin
      ItemCount := Min(ItemCount, L - Index);
      while ItemCount > 0 do
      begin
        V := GetData(Index);
        lse_set_value(Result.Add, V);
        Inc(Index);
        Dec(ItemCount);
      end;
    end;
  end;
end;

constructor KLiVarList.Create(AEngine: KLiEngine);
begin
  FEngine := AEngine;
  FItems := TList.Create;
  FObjRec.or_object := Self;
  FObjRec.or_type := KT_VARLIST;
  FEngine.OrEnter(@FObjRec);
end;

procedure KLiVarList.Delete(Index: integer);
begin
  __FreeValue(Pop(Index));
end;

procedure KLiVarList.DeleteLast;
begin
  __FreeValue(Pop);
end;

procedure KLiVarList.Exchange(Index1, Index2: integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure KLiVarList.ExchangeLastTwo;
var
  X: integer;
begin
  X := FItems.Count;
  FItems.Exchange(X - 2, X - 1);
end;

function KLiVarList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function KLiVarList.GetData(Index: integer): PLseValue;
begin
  if Index >= 0 then
    Result := PLseValue(FItems[Index]) else
    Result := PLseValue(FItems[Index + FItems.Count]);
end;

function KLiVarList.Insert(Index: integer): PLseValue;
begin
  Result := __NewNil;
  FItems.Insert(Index, Result);
end;

function KLiVarList.Left(ItemCount: integer): KLiVarList;
begin
  Result := Copy(0, ItemCount);
end;

procedure KLiVarList.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function KLiVarList.Pop(Index: integer): PLseValue;
begin
  Result := PLseValue(FItems[Index]);
  FItems.Delete(Index);
end;

function KLiVarList.Pop: PLseValue;
var
  X: integer;
begin
  X := FItems.Count - 1;
  Result := PLseValue(FItems[X]);
  FItems.Delete(X);
end;

procedure KLiVarList.Press(ItemCount: integer);
var
  X: integer;
begin
  X := GetCount;
  if X < ItemCount then ItemCount := X;
  while ItemCount > 0 do
  begin
    Dec(X);
    Delete(X);
    Dec(ItemCount);
  end;
end;

function KLiVarList.Add(const Value: PLseValue): PLseValue;
begin
  Result := __NewValue(Value);
  FItems.Add(Result);
end;

procedure KLiVarList.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_VARLIST, Self);
end;

procedure KLiVarList.SetCount(ItemCount: integer);
var
  L: integer;
begin
  L := FItems.Count;
  if L > ItemCount then
    Press(L - ItemCount) else
    for L := L + 1 to ItemCount do Add;
end;

{ KLiModule }

constructor KLiModule.Create(const AName: string; Engine: KLiEngine; ModuleType: KLiModuleType);
begin
  inherited Create(AName);
  IncRefcount;
  FFileName := AName;
  FModuleType := ModuleType;
  if FModuleType = moyKernel then
  begin
    FFileName    := sys_kernel;
    FVersion     := sys_version;
    FDescription := Format('builtin %s module', [Name]);
  end;
  FTypeList := __newNamedList(true);
  if AName = 'sys' then
    FFuncList := KLiNameHashed.Create(16) else
    FFuncList := KLiNameHashed.Create(4);
  if FModuleType = moyScript then
  begin
    FEngine := Engine;
    FEngine.AddCompiled(Self);
    FEngine.FModules.Add(Self);
    FModules := KLiModuleList.Create(FEngine);
    FModules.FImporter := Self;
    FImporters := TList.Create;
  end
  else sys_libraries.AddObject(Name, Self);
end;

destructor KLiModule.Destroy;
var
  A: integer;
  P: KLiModule;
begin
  if FModuleType = moyScript then
  begin
    for A := FEngine.FModules.Count - 1 downto 0 do
    begin
      P := FEngine.FModules[A];
      P.FImporters.Remove(Self);
      __removeFrom(P.FModules.FModules, Self);
    end;
    __removeFrom(FEngine.FModules.FModules, Self);
    if Self = FEngine.FMainModule then
      FEngine.FMainModule := nil;
    FreeAndNil(FModules);
    FreeAndNil(FImporters);
  end;
  DeleteFunctions;
  FreeAndNil(FFuncList);
  __releaseSOList(FTypeList);
  __removeFrom(sys_libraries, Self);
  while FFirstSyntax <> nil do
    FFirstSyntax.Free;
  if IsLibrary and (FHandle <> 0) then
    lse_free_library(FHandle);
  inherited;
end;

procedure KLiModule.DumpCodeToStream(stream: TStream; const margin: string);
var
  A: integer;
  F: KLiFunc;
  L: TStrings;
  NeedNewLine: boolean;

  procedure WriteText(const Text: string);
  begin
    lse_stream_write(stream, margin);
    lse_stream_write(stream, Text);
  end;

  procedure WriteLine(const Text: string = '');
  begin
    lse_stream_write(stream, margin);
    lse_stream_write(stream, Text);
    lse_stream_writeln(stream);
  end;

  procedure GiveNewLine;
  begin
    if NeedNewLine then
    begin
      lse_stream_writeln(stream);
      NeedNewLine := false;
    end;
  end;

begin
  NeedNewLine := false;

  WriteLine('# module: ' + Name);
  WriteLine('# source: ' + FFileName);
  WriteLine('# notice: ' + FDescription);

  lse_stream_writeln(stream);

  if Assigned(FModules) and (FModules.Count > 2) then
  begin
    WriteText('import');
    lse_stream_write(stream, ' ' + FModules[2].Name);
    for A := 3 to FModules.Count - 1 do
      lse_stream_write(stream, ', ' + FModules[A].Name);
    lse_stream_write(stream, ';');
    lse_stream_writeln(stream);
    NeedNewLine := true;
  end;
  
  L := TStringList.Create;
  try
    F := FFirstFunc;
    while F <> nil do
    begin
      L.Clear;
      F.DumpCode(L, margin);
      F := F.FNext;
      GiveNewLine;
      lse_stream_write(stream, L.Text);
      NeedNewLine := true;
    end;
  finally
    L.Free;
  end;

  lse_stream_writeln(stream);
end;

procedure KLiModule.DeleteFunctions;
begin
  FFuncList.Clear;
  while FFirstFunc <> nil do
    FFirstFunc.Free;
end;

procedure KLiModule.Satisfy;
var
  F: KLiFunc;
begin
  if FModuleType = moyScript then
  begin
    F := FFirstFunc;
    while F <> nil do
    begin
      F.Satisfy;
      F := F.FNext;
    end;
  end;
end;

procedure KLiModule.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_MODULE, Self);
end;

function KLiModule.AddImporter(module: KLiModule): KLiModule;
begin
  Result := module;
  if (module <> nil) and (module <> Self) then
    if (FImporters <> nil) and (FImporters.IndexOf(module) < 0) then
      FImporters.Add(module);
end;

function KLiModule.GetType(Index: integer): KLiType;
begin
  Result := KLiType(FTypeList.Objects[Index]);
end;

function KLiModule.FindModule(const ID: string; FindPossible: boolean): KLiModule;
begin
  if FModules <> nil then
    Result := FModules.Find(ID) else
    Result := nil;
  if Result = nil then
  begin
    if ID = 'sys' then
      Result := sys_module else
    if FindPossible then
    begin
      if FEngine <> nil then
        Result := FEngine.FModules.Find(ID);
      if Result = nil then
        Result := KLiModule(__findNamed(sys_libraries, ID));
    end;
  end;
end;

function KLiModule.FindSyntax(const ID: string): KLiSyntax;
begin
  Result := FFirstSyntax;
  while Result <> nil do
  begin
    if Result.Name = ID then Exit;
    Result := Result.FNext;
  end;
  Result := nil;
end;

function KLiModule.IsMainModule: boolean;
begin
  Result := (FEngine <> nil) and (FEngine.FMainModule = Self);
end;

function KLiModule.NewFunc: KLiFunc;
begin
  Result := KLiFunc.Create(Self, KT_VARIANT, '', nil, nil);
end;

function KLiModule.NewLabelName: string;
begin
  Inc(FEngine.FNameSeed);
  Result := Format('@%.3X', [FEngine.FNameSeed]);
end;

function KLiModule.NewFuncName: string;
begin
  Result := NewTempID('Z');
end;

function KLiModule.NewTempID(const Prefix: string): string;
begin
  Inc(FEngine.FNameSeed);
  Result := Format('%s_%.4d', [Prefix, FEngine.FNameSeed]);
end;

function KLiModule.FindFunc(const ID: string): KLiFunc;
begin
  Result := KLiFunc(FFuncList.Get(ID));
end;

function KLiModule.FindType(const ID: string): KLiType;
begin
  Result := KLiType(__findNamed(FTypeList, ID));
end;

function KLiModule.FindTypeBy(const ID, module_name: string): KLiType;
var
  X: integer;
  M: KLiModule;
begin
  Result := nil;
  if module_name <> '' then
  begin
    M := FindModule(module_name, true);
    if M <> nil then
      Result := M.FindType(ID);
  end
  else
  begin
    Result := FindType(ID);
    if Result = nil then
    begin
      if FModules <> nil then
        for X := 0 to FModules.Count - 1 do
        begin
          M := FModules[X];
          if M <> Self then
          begin
            Result := M.FindType(ID);
            if Result <> nil then Exit;
          end;
        end;
      Result := sys_module.FindType(ID);
    end;
  end;
end;

function KLiModule.SetupFunc(Func: PLseFunc): KLiFunc;
var
  p_name, f_name: string;
  p_type, f_type: KLiType;
  endc: char;
  base, curr: pchar;

  function parse_next(var ID: string; var VT: KLiType): char;
  var
    X: integer;
    S: string;
  begin
    if curr^ <> #0 then
    begin
      while not (curr^ in [',', '|', #0]) do Inc(curr);
      Result := curr^;
      if Result = #0 then Exit;

      SetString(S, base, curr - base);
      base := curr + 1;
      curr := base;

      X := Pos(':', S);
      if X > 0 then
      begin
        ID := Trim(Copy(S, 1, X - 1));
        if __IsIDStr(pchar(ID)) then
        begin
          VT := FindTypeBy(Trim(Copy(S, X + 1, Length(S))), '');
          if VT <> nil then Exit;
        end;
      end
      else
      begin
        ID := Trim(S);
        if __IsIDStr(pchar(ID)) then
        begin
          VT := KT_VARIANT;
          Exit;
        end;
      end;
    end;

    Result := #0;
  end;

begin
  Result := nil;
  base := Func^.fr_prot;
  if (base <> nil) and (base^ <> #0) then
  begin
    curr := base;
    endc := parse_next(f_name, f_type);
    if (endc = '|') and not Find(f_name) then
    begin
      Result := KLiFunc.Create(Self, f_type, f_name, nil, Func^.fr_addr);
      Result.FDescription := Func^.fr_desc;
      if curr^ <> '|' then
      repeat
        endc := parse_next(p_name, p_type);
        if not (endc in [',', '|']) or
          (p_type = KT_VOID) or
          (p_name = f_name) or
          Result.Params.Exists(p_name) or
          (Result.Params.Count = LSE_MAX_PARAMS) then
        begin
          FreeAndNil(Result);
          Exit;
        end;
        Result.AddParam(p_name, p_type);
      until endc = '|';
      if Result.Params.Count = 0 then
        if '__' = Copy(f_name, 1, 2) then
          Result.IsNameCall := true;
    end;
  end;
end;

function KLiModule.SetupModuleFuncs(Rec: PLseFuncListRec): integer;
var
  X: integer;
begin
  Result := 0;
  if Rec <> nil then
    for X := 0 to Rec^.fl_count - 1 do
      if SetupFunc(@(Rec^.fl_entry^[X])) <> nil then
        Inc(Result);
end;

function KLiModule.SetupType(const TR: PLseType): KLiType;
begin
  Result := nil;
  if (TR^.cr_type = LSV_OBJECT)
    and Assigned(TR^.cr_addref)
    and Assigned(TR^.cr_release)
    and __IsIDStr(TR^.cr_name)
    and not Find(TR^.cr_name) then
  begin
    Result := KLiType.Create(Self, TR^.cr_name, LSV_OBJECT);
    TR^.cr_class := Result;
    Move(TR^, Result.FTypeRec^, sizeof(RLseType));
  end;
end;

function KLiModule.SetupModuleTypes(const TLR: PLseTypeListRec): integer;
var
  A: integer;
begin
  Result := 0;
  if (TLR <> nil) and (TLR^.cl_entry <> nil) and (TLR^.cl_count > 0) then
    for A := 0 to TLR^.cl_count - 1 do
      if SetupType(@TLR^.cl_entry^[A]) <> nil then
        Inc(Result);
end;

function KLiModule.Find(const ID: string; rec: PLiFindRec): boolean;
var
  FR: KLiFindRec;
begin
  if rec = nil then rec := @FR;
  rec^.fo_type := foNone;
  rec^.VModule := FindModule(ID, false);
  if rec^.VModule = nil then
  begin
    rec^.VSyntax := FindSyntax(ID);
    if rec^.VSyntax = nil then
    begin
      rec^.VFunc := FindFunc(ID);
      if rec^.VFunc = nil then
      begin
        rec^.VType := FindType(ID);
        if rec^.VType <> nil then
          rec^.fo_type := foType;
      end
      else rec^.fo_type := foFunc;
    end
    else rec^.fo_type := foSyntax;
  end
  else rec^.fo_type := foModule;
  Result := rec^.fo_type <> foNone;
end;

function KLiModule.FindBy(const ID, module_name: string; rec: PLiFindRec): boolean;
var
  X: integer;
  M: KLiModule;
begin
  if module_name <> '' then
  begin
    M := FindModule(module_name, true);
    Result := (M <> nil) and M.Find(ID, rec);
  end
  else
  begin
    Result := Find(ID, rec);
    if not Result and (FModules <> nil) then
    begin
      for X := 0 to FModules.Count - 1 do
      begin
        M := FModules.Modules[X];
        if M <> Self then
        begin
          Result := M.Find(ID, rec);
          if Result then Exit;
        end;
      end;
      if FModules.Find('sys') = nil then
        Result := sys_module.Find(ID, rec);
    end;
  end;
end;

function KLiModule.TypeCount: integer;
begin
  Result := FTypeList.Count;
end;

{ KLiModuleList }

function KLiModuleList.Add(AModule: KLiModule): integer;
begin
  Result := IndexOfModule(AModule);
  if Result < 0 then
    Result := FModules.AddObject(AModule.Name, AModule);
end;

procedure KLiModuleList.Clear;
var
  A: integer;
begin
  DeleteFunctions;
  for A := FModules.Count - 1 downto 0 do
    Delete(A);
end;

constructor KLiModuleList.Create(Engine: KLiEngine);
begin
  IncRefcount;
  FEngine := Engine;
  FModules := TStringList.Create;
  FModules.CaseSensitive := true;
end;

procedure KLiModuleList.Delete(Index: integer);
var
  P: KLiModule;
begin
  P := GetModule(Index);
  FModules.Delete(Index);
  if FImporter = nil then P.Free;
end;

destructor KLiModuleList.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited;
end;

function KLiModuleList.Find(const ModuleName: string): KLiModule;
var
  X: integer;
begin
  X := IndexOf(ModuleName);
  if X >= 0 then
    Result := GetModule(X) else
    Result := nil;
end;

function KLiModuleList.GetCount: integer;
begin
  Result := FModules.Count;
end;

function KLiModuleList.GetModule(Index: integer): KLiModule;
begin
  Result := KLiModule(FModules.Objects[Index]);
end;

function KLiModuleList.IndexOf(const ModuleName: string): integer;
begin
  Result := FModules.IndexOf(ModuleName);
end;

function KLiModuleList.IndexOfModule(AModule: KLiModule): integer;
begin
  Result := FModules.IndexOfObject(AModule);
end;

procedure KLiModuleList.DeleteFunctions;
var
  index: integer;
begin
  if FImporter = nil then
    for index := 0 to GetCount - 1 do
      GetModule(index).DeleteFunctions;
end;

function KLiModuleList.ToVarlist(Engine: KLiEngine): KLiVarList;
var
  index: integer;
begin
  Result := __NewVarlist(Engine);
  for index := 0 to GetCount - 1 do
    Result.Add(GetModule(index), KR_MODULE);
end;

{ KLiError }

procedure KLiError.BreakNoLoop(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvBreakNoLoop, LastRow, LastCol, LastModule.Name,
      EsBreakNoLoop, LastModule.FileName, []);
end;

procedure KLiError.CanNotAsk(func: KLiFunc; expr: PLiExprRec; clss: KLiType);
var
  M: KLiModule;
begin
  M := ErrorModule(func, expr);
  with expr^ do
    SyntaxErr(EvCanNotAsk, Pos.row, Pos.col, M.Name,
      EsCanNotAsk, M.Name, [clss.Name]);
end;

procedure KLiError.TypeNotExists(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvClassNotExists, LastRow, LastCol, LastModule.Name,
      EsClassNotExists, LastModule.FileName, [LastVal]);
end;

procedure KLiError.Clear;
begin
  Write('', 0, 0, 0, '', '', '');
end;

procedure KLiError.ContinueNoLoop(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvContinueNoLoop, LastRow, LastCol, LastModule.Name,
      EsContinueNoLoop, LastModule.FileName, []);
end;

constructor KLiError.Create(AEngine: KLiEngine);
begin
  IncRefcount;
  FEngine := AEngine;
end;

procedure KLiError.Error(const Name: string; Errno, Row, Col: integer; const Module, Msg, FileName: string);
begin
  Write(Name, Errno, Row, Col, Module, Msg, FileName);
  if FEngine.FMainRunner <> nil then
  begin
    FEngine.FMainRunner.Excepted := true;
    lse_error(ErrorText);
  end
  else Abort;
end;

function KLiError.ErrorModule(func: KLiFunc; expr: PLiExprRec): KLiModule;
begin
  Result := KLiModule(expr^.Pos.module);
  if Result = nil then
    Result := func.FModule;
end;

procedure KLiError.FuncNotFound(func: KLiFunc; expr: PLiExprRec);
var
  M: KLiModule;
begin
  M := ErrorModule(func, expr);
  with expr^ do
    SyntaxErr(EvFuncNotFound, Pos.row, Pos.col, M.Name,
      EsFuncNotFound, M.Name, [Name]);
end;

function KLiError.ErrorText: string;
const
  E = '[%s]: (module=%s%s row=%d col=%d errno=%d) %s';
begin
  if (Errno <> 0) and (Msg <> '') then
  begin
    Result := FModuleFile;
    if Result <> '' then
      Result := ' file=' + Result;
    Result := Format(E, [Name, module, Result, Row + 1, Col + 1, Errno, Msg]);
  end
  else Result := '';
end;

procedure KLiError.SymExpected(Parser: KLiParser; const syms: string);
begin
  with Parser do
    SyntaxErr(EvSymExpected, LastRow, LastCol, LastModule.Name,
      EsSymExpected, LastModule.FileName, [syms, LastVal]);
end;

procedure KLiError.SymNotFound(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvSymNotFound, LastRow, LastCol, LastModule.Name,
      EsSymNotFound, LastModule.FileName, []);
end;

procedure KLiError.SymUnexpected(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvSymUnexpected, LastRow, LastCol, LastModule.Name,
      EsSymUnexpected, LastModule.FileName, [LastVal]);
end;

procedure KLiError.Redeclared(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvRedeclared, LastRow, LastCol, LastModule.Name,
      EsRedeclared, LastModule.FileName, [LastVal]);
end;

procedure KLiError.Write(const Name: string; Errno, Row, Col: integer;
  const Module, Msg, FileName: string);
begin
  FErrno := Errno;
  FName := Name;
  FMsg := Msg;
  FModule := Module;
  FModuleFile := FileName;
  FRow := Row;
  FCol := Col;
end;

procedure KLiError.ObjectNotExists(func: KLiFunc; expr: PLiExprRec);
var
  M: KLiModule;
begin
  M := ErrorModule(func, expr);
  with expr^ do
    SyntaxErr(EvObjectNotExists, Pos.row, Pos.col, M.Name,
      EsObjectNotExists, M.Name, [Name]);
end;

procedure KLiError.SyntaxErr(Errno, Row, Col: integer; const Module, Fmt, FileName: string;
  const args: array of const);
begin
  Error(SyntaxError, Errno, Row, Col, Module, Format(Fmt, Args), FileName);
end;

procedure KLiError.ImportErr(Errno, Row, Col: integer;
      const Module, Fmt, FileName: string; const Args: array of const);
begin
  Error(ImportError, Errno, Row, Col, Module, Format(Fmt, Args), FileName);
end;

procedure KLiError.ModuleReimport(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvModuleReimport, LastRow, LastCol, LastModule.Name,
      EsModuleReimport, LastModule.FileName, [LastVal]);
end;

procedure KLiError.ModuleNotFound(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvModuleNotFound, LastRow, LastCol, LastModule.Name,
      EsModuleNotFound, LastModule.FileName, [LastVal]);
end;

procedure KLiError.WrongLibrary(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvWrongLibrary, LastRow, LastCol, LastModule.Name,
      EsWrongLibrary, LastModule.FileName, [LastVal]);
end;

procedure KLiError.NeedPureID(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvNeedPureID, LastRow, LastCol, LastModule.Name,
      EsNeedPureID, LastModule.FileName, [LastVal]);
end;

procedure KLiError.LabelNotExists(func: KLiFunc; expr: PLiExprRec);
var
  M: KLiModule;
begin
  M := ErrorModule(func, expr);
  with expr^ do
    SyntaxErr(EvLabelNotExists, Pos.row, Pos.col, M.Name,
      EsLabelNotExists, M.Name, [Name]);
end;

procedure KLiError.ImportEachOther(Parser: KLiParser; module: KLiModule);
begin
  with Parser do
    SyntaxErr(EvImportEachOther, LastRow, LastCol, Module.Name,
      EsImportEachOther, Module.FileName, [Module.Name, module.Name]);
end;

{ KLiEngine }

procedure KLiEngine.Clear;
begin
  lock_engine(Self);
  try
    Reset(true);
    FArguments.Clear;
    FModules.Clear;
    FNameSeed := 0;
    FMainFile := '';
    FMainModule := KLiModule.Create(RPN_MAIN, Self, moyScript);
    if FMainFile = '' then
      FMainModule.FFileName := sys_kernel else
      FMainModule.FFileName := FMainFile;
  finally
    unlock_engine(Self);
  end;
end;

procedure KLiEngine.BeginExecute;
begin
  SetResultTypeText('', '');
  FEngineRec^.er_executing(FEngineRec);
end;

function KLiEngine.Compile(const Code: string): KLiFunc;
var
  old_fname, new_fname: string;
begin
  PrepareCompile;
  old_fname := FMainModule.FileName;
  try
    new_fname := FMainFile;
    if new_fname <> '' then
      if not __sameFileName(new_fname, old_fname) then
        if not __sameFileName(new_fname, sys_kernel) then
          FMainModule.FileName := new_fname;
    Result := DoCompile(Code);
  except
    FMainModule.FileName := old_fname;
    raise;
  end;
end;

constructor KLiEngine.Create(const AEngineRec: PLseEngine);
begin
  FEngineRec := AEngineRec;
  if FEngineRec <> nil then
    FEngineRec^.er_kernel := Self;

  IncRefcount;

  FCompiledObjects := nil;
  FModules := KLiModuleList.Create(Self);

  FExitResult := __NewNil;
  
  FError := KLiError.Create(Self);

  FArguments := TStringList.Create;
  FArguments.CaseSensitive := true;

  FMainFile := sys_kernel;

  FMainModule := KLiModule.Create(RPN_MAIN, Self, moyScript);
  if FMainFile = '' then
    FMainModule.FFileName := sys_kernel else
    FMainModule.FFileName := FMainFile;

  FMainValues := KLiHashed.Create(Self, 64);
  FMainValues.IncRefcount;

  FTempValues := __NewVarlist(Self);
  FTempValues.IncRefcount;
end;

destructor KLiEngine.Destroy;
begin
  Clear;

  __freeAndNil(FMainValues);
  __freeAndNil(FTempValues);
  __freeAndNil(FMainSnap);

  SetInputStream(nil);
  SetOutputStream(nil);
  SetErrputStream(nil);

  __FreeValue(FExitResult);
  FExitResult := nil;

  __freeAndNil(FMainModule);
  __freeAndNil(FModules);
  __freeAndNil(FError);
  __freeAndNil(FArguments);
  inherited;
end;

function KLiEngine.DoCompile(const Code: string): KLiFunc;
var
  index, count: integer;
  module: KLiModule;

  procedure roll_back;
  var
    X: integer;
    A: KLiObject;
  begin
    try
      X := FCompiledObjects.Count - 1;
      while X >= 0 do
      begin
        A := KLiObject(FCompiledObjects[X]);
        FCompiledObjects.Delete(X);
        A.Free;
        X := FCompiledObjects.Count - 1;
      end;
    finally
      FreeAndNil(FCompiledObjects);
    end;
  end;

begin
  __check(FCompiledObjects = nil, 'invalid embeded compiling');
  try
    FError.Clear;
    if FMainRunner <> nil then
      module := FMainRunner.CurrentFunc.FModule else
      module := FMainModule;
    count := FModules.Count;
    FCompiledObjects := KLiList.Create;
    Result := KLiParser.Create(module).ParseAndFree(Code);
    for index := FModules.Count - 1 downto count do
      FModules[index].Satisfy;
    module.Satisfy;
    FreeAndNil(FCompiledObjects);
    FReady := true;
  except
    roll_back;
    raise;
  end;
end;

procedure KLiEngine.DumpCodeToStream(stream: TStream; const margin: string);
var
  A: integer;
begin
  for A := 0 to FModules.Count - 1 do
  begin
    FModules[A].DumpCodeToStream(stream, margin);
    lse_stream_writeln(stream);
  end;
end;

procedure KLiEngine.EndExecute;
begin
  SetResultTypeText(GetResultType.FullName, GetResultText);
  FEngineRec^.er_executed(FEngineRec);
end;

function KLiEngine.TryCompileCode(const code: string): boolean;
begin
  try
    SetResultTypeText('', '');
    Compile(code);
    Result := (Error.errno = 0);
  except
    Result := false;
  end;
end;

function KLiEngine.TryExecuteCode(const code: string): boolean;
begin
  Result := TryCompileCode(code) and TryGo(false);
end;

function KLiEngine.TryCompileFile(const fname: string): boolean;
begin
  try
    SetMainFile(fname);
    Result := TryCompileCode(__fileText(fname));
  except
    Result := false;
    Error.write(RuntimeError, ERUNTIME, 0, 0, MainModule.Name,
                lse_exception_str, fname);
  end;
end;

function KLiEngine.TryExecuteFile(const fname: string): boolean;
begin
  Result := TryCompileFile(fname) and TryGo(false);
end;

function KLiEngine.TryGo(resetVar: boolean): boolean;
begin
  try
    SetResultTypeText('', '');
    Go;
    Result := (Error.errno = 0);
  except
    Result := false;
  end;
end;

procedure KLiEngine.SetResultTypeText(const RType, RText: string);
begin
  FExitResultType := RType;
  FExitResultText := RText;
end;

procedure KLiEngine.SetErrputStream(const Value: PLseStream);
begin
  if Value <> FErrput then
  begin
    if FErrput <> nil then
    try
      FErrput^.s_release(FErrput);
    finally
      FErrput := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.er_errput then
      begin
        Value^.s_addref(Value);
        FErrput := Value;
      end;
  end;
end;

procedure KLiEngine.SetInputStream(const Value: PLseStream);
begin
  if Value <> FInput then
  begin
    if FInput <> nil then
    try
      FInput^.s_release(FInput);
    finally
      FInput := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.er_input then
      begin
        Value^.s_addref(Value);
        FInput := Value;
      end;
  end;
end;

procedure KLiEngine.SetOutputStream(const Value: PLseStream);
begin
  if Value <> FOutput then
  begin
    if FOutput <> nil then
    try
      FOutput^.s_release(FOutput);
    finally
      FOutput := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.er_output then
      begin
        Value^.s_addref(Value);
        FOutput := Value;
      end;
  end;
end;

function KLiEngine.GetMainFunc: KLiFunc;
begin
  if FMainFunc = nil then
  begin
    FMainFunc := KLiFunc.Create(FMainModule,
      KT_VARIANT, RPN_MAIN, nil, nil);
    FMainFunc.SetState(fusMainFunc, true);
  end;
  Result := FMainFunc;
end;

function KLiEngine.GetMainSnap: KLiVarSnap;
var
  func: KLiFunc;
begin
  func := GetMainFunc;
  if FMainSnap = nil then
    FMainSnap := KLiVarSnap.Create(func);
  FMainSnap.Prepare;
  Result := FMainSnap;
end;

function KLiEngine.GetResultText: string;
begin
  Result := __AsString(FExitResult);
end;

function KLiEngine.GetResultType: KLiType;
begin
  Result := __AsType(FExitResult);
end;

procedure KLiEngine.SetMainFile(const AValue: string);
begin
  FMainFile := Trim(AValue);
  if FMainModule <> nil then;
    FMainModule.FFileName := FMainFile;
end;

procedure KLiEngine.SetMainSearchPath(const AValue: string);
begin
  FMainSearchPath := Trim(AValue);
end;

function KLiEngine.GetSearchPath: string;
begin
  if FMainSearchPath <> '' then
    Result := FMainSearchPath + ';' + sys_search_path else
    Result := sys_search_path;
end;

function KLiEngine.GetErrputStream: PLseStream;
begin
  Result := FErrput;
  if Result = nil then
    Result := FEngineRec^.er_errput;
end;

function KLiEngine.GetInputStream: PLseStream;
begin
  Result := FInput;
  if Result = nil then
    Result := FEngineRec^.er_input;
end;

function KLiEngine.GetOutputStream: PLseStream;
begin
  Result := FOutput;
  if Result = nil then
    Result := FEngineRec^.er_output;
end;

procedure KLiEngine.GetValue(const Name: string; Value: PLseValue);
var
  data: PLseValue;
begin
  if Name = 'search' then
    lse_set_string(Value, GetSearchPath) else
  if Name = 'mainfile' then
    lse_set_string(Value, FMainFile) else
  begin
    data := FMainValues.FindValue(Name);
    if data <> nil then
      lse_set_value(Value, data) else
      lse_set_string(Value, __ReadConfig(Name));
  end;
end;

procedure KLiEngine.Go;
begin
  try
    __check(FReady, 'The engine is not ready to run');
    PrepareCompile;
    Reset(false);
    FError.Clear;
    FExited := false;
    BeginExecute;
    try
      FMainRunner := KLiRunner.Create(Self);
      try
        GetMainSnap.Prepare;
        FMainRunner.Goon(FMainFunc, 0, FExitResult);
        if not FMainRunner.FExcepted then
          FError.Clear;
      finally
        FreeAndNil(FMainRunner);
      end;
    finally
      lock_engine(Self);
      try
        EndExecute;
        FMainFunc.FCodes.Clear;
        Reset(false);
      finally
        unlock_engine(Self);
      end;
    end;
  except
    FError.write('KernelError', ERUNTIME, -1, -1, '..kernel..',
                 lse_exception_str, '');
    raise;
  end;
end;

function KLiEngine.Running: boolean;
begin
  Result := (FMainRunner <> nil);
end;

function KLiEngine.Terminated: boolean;
begin
  Result := (FMainRunner = nil) or FMainRunner.FTerminated;
end;

procedure KLiEngine.OrCollect(or_list: TList);
var
  X: integer;
begin
  OrIncLife(or_list);
  try
    for X := 0 to or_list.Count - 1 do
      with PLiObjRec(or_list[X])^ do
        if or_type = KT_VARLIST then
          KLiVarList(or_object).Clear else
        if or_type = KT_HASHED then
          KLiHashed(or_object).Clear else
        if or_type = KT_FUNC then
          KLiFunc(or_object).Garbaged;
  finally
    OrDecLife(or_list);
  end;
end;

procedure KLiEngine.OrDecLife(or_list: TList);
var
  X: integer;
begin
  for X := 0 to or_list.Count - 1 do
    with PLiObjRec(or_list[X])^ do
     or_type.FTypeRec^.cr_release(or_object);
end;

function KLiEngine.OrEnter(Rec: PLiObjRec): PLiObjRec;
begin
  if not (orsInChain in Rec^.or_state) then
  begin
    Rec^.or_prev := nil;
    Rec^.or_next := FOrChain;
    if FOrChain <> nil then
      FOrChain^.or_prev := Rec;
    FOrChain := Rec;
    Include(Rec^.or_state, orsInChain);
  end;
  Result := Rec;
end;

function KLiEngine.GarbageCollect: integer;
var
  list: TList;
begin
  list := TList.Create;
  try
    Result := OrMark(list);
    OrCollect(list);
  finally
    list.Free;
  end;
end;

function KLiEngine.OrMark(list: TList): integer;
var
  node: PLiObjRec;
  root: PLiSnap;
begin
  Result := 0;
  if OrChain = nil then Exit;
  
  // 1. unmark all
  node := FOrChain;
  while node <> nil do
  begin
    Exclude(node^.or_state, orsMarked);
    node := node^.or_next;
  end;

  // 2. mark all possibles
  if FMainRunner <> nil then
  begin
    root := FMainRunner.FCurrent;
    while root <> nil do
    begin
      OrMarkVarlist(root^.values);
      root := root^.prior;
    end;
    OrMarkVarlist(FMainRunner.FStack);
  end;
  OrMarkVarlist(FMainSnap);
  OrMarkVarlist(FTempValues);
  OrMarkHashed(FMainValues);

  // 3. reset counter
  node := FOrChain;
  while node <> nil do
  begin
    if not OrIsMarked(node) then
    begin
      Inc(Result);
      list.Add(node);
    end;
    node := node^.or_next;
  end;
end;

procedure KLiEngine.OrIncLife(or_list: TList);
var
  X: integer;
begin
  for X := 0 to or_list.Count - 1 do
    with PLiObjRec(or_list[X])^ do
     or_type.FTypeRec^.cr_addref(or_object);
end;

function KLiEngine.OrIsMarked(Rec: PLiObjRec): boolean;
begin
  Result := (Rec <> nil) and (orsMarked in Rec^.or_state);
end;

function KLiEngine.OrMarkFunc(FC: KLiFunc): boolean;
var
  index: integer;
  rec: PLiObjRec;
  curry: KLiFunc_curry;
begin
  Result := (FC <> nil);
  if Result then
  begin
    if FC is KLiFunc_curry then
    begin
      curry := FC as KLiFunc_curry;
      rec := curry.ObjRec;
      if not OrIsMarked(rec) then
      begin
        Include(rec^.or_state, orsMarked);
        for index := length(curry.FCurry) - 1 downto 0 do
          OrMarkValue(curry.FCurry[index]);
      end;
    end;
  end;
end;

function KLiEngine.OrMarkValue(VD: PLseValue): boolean;
var
  clss: KLiType;
  vobj: pointer;
begin
  Result := (VD <> nil) and (lse_vtype(VD) = LSV_OBJECT) and (VD^.VObject <> nil);
  if Result then
  begin
    vobj := VD^.VObject;
    clss := __AsType(VD);
    if clss = KT_VARLIST then OrMarkVarlist(KLiVarList(vobj)) else
    if clss = KT_HASHED  then OrMarkHashed(KLiHashed(vobj)) else
    if clss = KT_FUNC    then OrMarkFunc(KLiFunc(vobj))
  end;
end;

function KLiEngine.OrLeave(Rec: PLiObjRec): PLiObjRec;
begin
  if orsInChain in Rec^.or_state then
  begin
    if Rec = FOrChain then
    begin
      FOrChain := Rec^.or_next;
      if FOrChain <> nil then
        FOrChain^.or_prev := nil;
    end
    else
    begin
      if Rec^.or_prev <> nil then
        Rec^.or_prev^.or_next := Rec^.or_next;
      if Rec^.or_next <> nil then
        Rec^.or_next^.or_prev := Rec^.or_prev;
      Rec^.or_prev := nil;
      Rec^.or_next := nil;
    end;
    Exclude(Rec^.or_state, orsInChain);
  end;
  Result := Rec;
end;

function KLiEngine.OrMarkHashed(HS: KLiHashed): boolean;
var
  rec: PLiObjRec;
  list: TList;
  index: integer;
begin
  Result := (HS <> nil);
  if Result then
  begin
    rec := @(HS.FObjRec);
    if not OrIsMarked(rec) then
    begin
      Include(rec^.or_state, orsMarked);
      list := TList.Create;
      try
        for index := HS.ListData(list) - 1 downto 0 do
          OrMarkValue(PLseValue(list[index]));
      finally
        list.Free;
      end;
    end;
  end;
end;

function KLiEngine.OrMarkVarlist(VL: KLiVarList): boolean;
var
  rec: PLiObjRec;
  index: integer;
begin
  Result := (VL <> nil);
  if Result then
  begin
    rec := @(VL.FObjRec);
    if not OrIsMarked(rec) then
    begin
      Include(rec^.or_state, orsMarked);
      for index := 0 to VL.Count - 1 do
        OrMarkValue(VL[index]);
    end;
  end;
end;

procedure KLiEngine.Reset(IncludeVar: boolean);
begin
  if IncludeVar then
  begin
    FMainValues.Clear;
    FTempValues.Clear;
    if FMainSnap <> nil then
    begin
      FMainSnap.ClearValues;
      FMainSnap.DecRefcount;
      FMainSnap := nil;
    end;
    GarbageCollect;
  end;
  lse_set_nil(FExitResult);
end;

procedure KLiEngine.SetResult(value: PLseValue);
begin
  lse_set_value(FExitResult, value);
end;

procedure KLiEngine.Terminate;
begin
  if FMainRunner <> nil then
    FMainRunner.Terminate;
end;

procedure KLiEngine.PrepareCompile;
begin
  __check(not Running, 'the engine is running');
end;

function KLiEngine.ReadValue(const Name: string): string;
var
  data: PLseValue;
begin
  try
    data := FMainValues.FindValue(Name);
    if data <> nil then
      Result := __AsString(data) else
    if Name = 'search' then
      Result := GetSearchPath else
    if Name = 'mainfile' then
      Result := FMainFile else
      Result := __ReadConfig(Name);
  except
    Result := '';
  end;
end;

procedure KLiEngine.AddCompiled(AObject: KLiObject);
begin
  if FCompiledObjects <> nil then
    FCompiledObjects.Add(AObject);
end;

{ KLiVarSnap }

procedure KLiVarSnap.ClearValues;
var
  index: integer;
begin
  for index := GetCount - 1 downto 0 do
    __SetDefaultValue(GetData(index), GetVarb(index).ValueType);
end;

constructor KLiVarSnap.Create(Func: KLiFunc);
begin
  inherited Create(Func.Engine);
  IncRefcount;
  FParams := Func.FParams;
  FParams.IncRefcount;
  FLocals := Func.FCodes.FLocals;
  FLocals.IncRefcount;
  FExpandThis := Func.ExpandThis;
  Prepare;
end;

destructor KLiVarSnap.Destroy;
begin
  FLocals.DecRefcount;
  FParams.DecRefcount;
  inherited;
end;

function KLiVarSnap.FindVarb(const Name: string): KLiVarb;
begin
  Result := FLocals.Find(Name);
  if Result = nil then
    Result := FParams.Find(Name);
end;

function KLiVarSnap.GetByIndex(Index: integer; var Varb: KLiVarb): PLseValue;
begin
  if (Index >= 0) and (Index < GetCount) then
  begin
    Result := GetData(Index);
    if Index < FParams.Count then
      Varb := FParams[Index] else
      Varb := FLocals[Index - FParams.Count];
  end
  else
  begin
    Result := nil;
    Varb := nil;
  end;
end;

function KLiVarSnap.GetByName(const Name: string; var Varb: KLiVarb): PLseValue;
var
  snap: KLiVarSnap;
begin
  Varb := FindVarb(Name);
  if Varb <> nil then
    Result := GetData(Varb.FIndex) else
  if FExpandThis then
  begin
    snap := KLiVarSnap(GetData(0)^.VObject);
    if snap <> nil then
      Result := snap.GetByName(Name, Varb) else
      Result := nil;
  end
  else Result := nil;
end;

function KLiVarSnap.GetParamValues(Values: KLiVarList): integer;
var
  index: integer;
begin
  Result := FParams.Count;
  Values.Clear;
  for index := 0 to Result - 1 do
    Values.Add(GetData(index));
end;

function KLiVarSnap.GetVarb(index: integer): KLiVarb;
begin
  if index < FParams.Count then
    Result := FParams[index] else
    Result := FLocals[index - FParams.Count];
end;

function KLiVarSnap.GetVV(varb: KLiVarb): PLseValue;
begin
  if HasVarb(varb) then
    Result := GetData(varb.FIndex) else
    Result := nil;
end;

function KLiVarSnap.HasVarb(varb: KLiVarb): boolean;
begin
  Result := (varb.FList = FParams) or
            (varb.FList = FLocals);
end;

function KLiVarSnap.ParamCount: integer;
begin
  if FParams <> nil then
    Result := FParams.Count else
    Result := 0;
end;

procedure KLiVarSnap.Prepare;
var
  index, total: integer;
begin
  total := FParams.Count + FLocals.Count;
  for index := GetCount to total - 1 do
    AddDefault(GetVarb(index).ValueType.TypeRec);
end;

{ KLiCallStack }

procedure KLiCallStack.AddCallSnap(Call: PLseParam; Snap: PLiSnap);
var
  cs: PLiCallSnap;
begin
  if FCount = FStack.Count then
  begin
    cs := PLiCallSnap(lse_mem_alloc(sizeof(RLiCallSnap)));
    FStack.Add(cs);
  end
  else cs := PLiCallSnap(FStack[FCount]);
  cs^.call := Call;
  cs^.snap := Snap;
  Inc(FCount);
end;

procedure KLiCallStack.Clear;
var
  index: integer;
begin
  FCount := 0;
  for index := FStack.Count - 1 downto 0 do
  begin
    lse_mem_free(FStack[index], sizeof(RLiCallSnap));
    FStack.Delete(index);
  end;
end;

constructor KLiCallStack.Create(ARunner: KLiRunner);
begin
  FRunner := ARunner;
  FStack := TList.Create;
  FCount := 0;
end;

destructor KLiCallStack.Destroy;
begin
  Clear;
  FStack.Free;
  inherited;
end;

function KLiCallStack.GetCallSnap(Index: integer): KLiVarList;
var
  cs: PLiCallSnap;
  X: integer;
begin
  cs := GetItem(Index);
  Result := KLiVarList.Create(FRunner.FEngine);
  if cs^.call <> nil then
  begin
    KLiFunc(cs^.call^.p_func).SaveTo(Result.Add);
    for X := 0 to cs^.call^.p_count - 1 do
      Result.Add(cs^.call^.p_param[X]);
  end
  else
  begin
    cs^.snap^.func.SaveTo(Result.Add);
    for X := 0 to cs^.snap^.values.FActualParamCount - 1 do
      Result.Add(cs^.snap^.values[X]);
  end;
end;

function KLiCallStack.GetItem(Index: integer): PLiCallSnap;
begin
  if (Index < 0) or (Index >= FCount) then
    lse_error('call stack index (%d) out of bounds (%d)', [Index, FCount]);
  Result := PLiCallSnap(FStack[Index]);
end;

function KLiCallStack.GetMaxCount: integer;
begin
  Result := FStack.Count;
end;

procedure KLiCallStack.Push(Call: PLseParam);
begin
  AddCallSnap(Call, nil);
end;

procedure KLiCallStack.Pop;
begin
  Dec(FCount);
end;

procedure KLiCallStack.Push(Snap: PLiSnap);
begin
  AddCallSnap(nil, Snap);
end;

{ KLiRunner }

procedure KLiRunner.ErrorRT(const ErrorStr: string);
begin
  if not FExcepted then
  begin
    FEngine.FError.write(RuntimeError,
                         ERUNTIME,
                         FExprrec^.Pos.Row,
                         FExprrec^.Pos.Col,
                         CurrentModule.Name,
                         ErrorStr,
                         CurrentModule.FileName);
    FExcepted := true;
  end;
end;

constructor KLiRunner.Create(Engine: KLiEngine);
begin
  FEngine := Engine;
  lock_engine(FEngine);
  try
    FStack := __NewVarlist(FEngine);
    FCallStack := KLiCallStack.Create(Self);
    FEngine.OrLeave(@(FStack.FObjRec));
  finally
    unlock_engine(FEngine);
  end;
end;

function KLiRunner.CurrentFunc: KLiFunc;
begin
  if FCurrent <> nil then
    Result := FCurrent^.func else
    Result := nil;
end;

function KLiRunner.CurrentModule: KLiModule;
begin
  if FCurrent <> nil then
    Result := FCurrent^.func.FModule else
    Result := nil;
end;

destructor KLiRunner.Destroy;
begin
  lock_engine(FEngine);
  try
    FCallStack.Free;
    FStack.Free;
  finally
    unlock_engine(FEngine);
  end;
  inherited;
end;

procedure KLiRunner.Eval(const Code: string; Output: PLseValue);
var
  func: KLiFunc;
begin
  try
    FEngine.FError.Clear;
    func := FEngine.DoCompile(Code);
    try
      Goon(Func, 0, Output);
    finally
      Func.DecRefcount;
    end;
  except
    if FEngine.FError.errno = 0 then
      ErrorRT(lse_exception_str);
    FExcepted := true;
  end;
end;

function KLiRunner.HasNext: boolean;
begin
  Result := (FCurrent <> nil) and
            (FCurrent^.next < FCurrent^.func.FCodes.Count);
end;

function KLiRunner.Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
var
  base, index, count: integer;
  call: RLseParam;
  data: RLseValue;
  snap: RLiSnap;
  clss: KLiType;
  curry: KLiFunc_curry;
begin
  try
    index := ParamCount - Func.Params.Count;
    if index > 0 then
    begin
      Dec(ParamCount, index);
      FStack.Press(index);
    end;

    base := FStack.Count - ParamCount;

    if func.IsCurryFunc then
    begin
      curry := KLiFunc_curry(func);
      count := curry.CurryCount;
      for index := 0 to count - 1 do
        lse_set_value(FStack.Insert(base + index), curry.CurryData[index]);
      Inc(ParamCount, count);
      func := curry.FCurryFunc;
    end
    else count := 0;

    for index := count to func.Params.Count - 1 do
    begin
      clss := func.FParams[index].ValueType;
      if index < ParamCount then
        __SetTypeValue(FEngine, FStack[base + index], clss) else
        __SetDefaultValue(FStack.Add, clss);
    end;

    if Func.FProc <> nil then
    begin
      __InitParam(@call, Self, Func);
      for index := 0 to Func.Params.Count - 1 do
        call.p_param[index] := FStack[index + base];
      call.p_count := ParamCount;
      FCallStack.Push(PLseParam(@call));
      try
        if Output <> nil then
        begin
          lse_clear_value(Output);
          call.p_result := Output;
          Result := Func.Execute(@call);
          if Result then
            FStack.SetCount(base);
        end
        else
        begin
          lse_init_value(@data);
          call.p_result := @data;
          Result := Func.Execute(@call);
          if Result then
          begin
            FStack.SetCount(base);
            FStack.Add^ := data;
          end
          else lse_set_nil(@data);
        end;
      finally
        FCallStack.Pop;
      end;
    end
    else
    begin
      snap.base := base;
      snap.next := 0;
      snap.values := nil;
      snap.output := nil;
      snap.outype := func.FResultType;
      snap.prior := FCurrent;
      snap.exprec := FExprrec;
      snap.func := func;
      if func.IsMainFunc then
      begin
        snap.values := FEngine.MainSnap;
        snap.values.Prepare;
        snap.values.IncRefcount;
      end
      else
      begin
        snap.values := KLiVarSnap.Create(func);
        snap.values.FActualParamCount := ParamCount;
        for index := 0 to Func.Params.Count - 1 do
          lse_set_value(snap.values[index], FStack[base + index]);
      end;
      try
        FStack.SetCount(base);
        if Output = nil then
        begin
          snap.output := FStack.Add;
          Inc(snap.base);
          Inc(base);
        end
        else
        begin
          lse_clear_value(Output);
          snap.output := Output;
        end;
        FCurrent := @snap;
        FCallStack.Push(FCurrent);
        try
          while ExecGoonNext do
          begin
            { nothing }
          end;
          Result := not FExcepted;
          if Result then
          begin
            __SetTypeValue(FEngine, snap.output, snap.outype);
            if func.IsConstFunc then
            begin
              FEngine.FMainValues.SetValue(Format('@%p', [pointer(func)]), snap.output);
              func.FProc := @udc_const;
            end;
          end;
        finally
          FCallStack.Pop;
        end;
      finally
        FCurrent := snap.prior;
        FExprrec := snap.exprec;
        snap.values.DecRefcount;
        FStack.SetCount(base);
      end;
    end;
  except
    Result := false;
    if not FExcepted then
      ErrorRT(lse_exception_str);
  end;
end;

function KLiRunner.ListMatchResult: KLiVarList;
var
  index: integer;
begin
  Result := nil;
  with FMatchPatten do
    if mp_result.mr_str <> nil then
    begin
      Result := __NewVarlist(FEngine);
      Result.Add(mp_result.mr_str - mp_source);
      Result.Add(mp_result.mr_len);
      for index := 0 to mp_level - 1 do
      begin
        Result.Add(mp_captures[index].mr_str - mp_source);
        Result.Add(mp_captures[index].mr_len);
      end;
    end;
end;

function KLiRunner.MatchPatten: PLiMatchPatten;
begin
  Result := @FMatchPatten;
end;

function KLiRunner.FormatFor(const Fmt: string; Values: KLiVarList): string;
var
  index, count: integer;
  next, base: pchar;
  temp: string;
  data: PLseValue;

  procedure ErrorFmt;
  begin
    lse_error('"%s" is invalid format', [Fmt]);
  end;

  function ValueAt(value_index: integer): PLseValue;
  begin
    if (Values <> nil) and (value_index < Values.Count) then
      Result := Values[value_index] else
      Result := @sys_nil;
  end;

begin
  Result := '';
  index := 0;
  base := pchar(fmt);
  if (base <> nil) and (base^ <> #0) then
  repeat
    next := __seekch(base, ['%', #0]);
    if next^ = #0 then
    begin
      Result := Result + base;
      Exit;
    end;
    if next <> base then
    begin
      count := Length(Result);
      SetLength(Result, count + (next - base));
      System.Move(base^, Result[count + 1], next - base);
    end;
    base := next;
    Inc(next);
    if next^ = '%' then Result := Result + '%' else
    if next^ = '[' then {<--absolute index}
    begin
      Inc(next);
      base := next;
      if not (next^ in DigitChar) then ErrorFmt;
      repeat Inc(next) until not (next^ in DigitChar);
      if next^ <> ']' then ErrorFmt;
      SetString(temp, base, next - base);
      Result := Result + __AsString(ValueAt(StrToInt(temp)));
    end
    else
    if next^ = '(' then
    begin
      Inc(next);
      base := next;
      if not (next^ in IDHeadChar + [':']) then ErrorFmt;
      repeat Inc(next) until not (next^ in IDChar + [':']);
      if next^ <> ')' then ErrorFmt;
      SetString(temp, base, next - base);
      Result := Result + GetString(temp);
    end
    else
    if next^ = '{' then
    begin
      Inc(next);
      base := next;
      if not (next^ in IDHeadChar) then ErrorFmt;
      while next^ in (['.'] + IDChar) do Inc(next);
      if next^ <> '}' then ErrorFmt;
      SetString(temp, base, next - base);
      Result := Result + FEngine.ReadValue(temp);
    end
    else
    begin
      while (next^ <> #0) and (next^ in ['-', ':', '.', '0'..'9']) do Inc(next);
      SetString(temp, base, (next - base) + 1);
      data := ValueAt(index);
      case next^ of
        'd': Result := Result + SysUtils.Format(temp, [__AsInt64(data)]);
        'u': Result := Result + SysUtils.Format(temp, [__AsInt64(data)]);
        'e': Result := Result + SysUtils.Format(temp, [__AsFloat(data)]);
        'f': Result := Result + SysUtils.Format(temp, [__AsFloat(data)]);
        'g': Result := Result + SysUtils.Format(temp, [__AsFloat(data)]);
        'n': Result := Result + SysUtils.Format(temp, [__AsFloat(data)]);
        'm': Result := Result + SysUtils.Format(temp, [__AsFloat(data)]);
        'p': Result := Result + SysUtils.Format(temp, [__AsInt64(data)]);
        'c',
        's': Result := Result + SysUtils.Format(temp, [__AsString(data)]);
        'x': Result := Result + SysUtils.Format(temp, [__AsInt64(data)]);
        else ErrorFmt;
      end;
      Inc(index);
    end;
    base := next + 1;
  until (base^ = #0);
end;

function KLiRunner.GetValue(varb: KLiVarb): PLseValue;
begin
  Result := FCurrent^.values.GetVV(varb);
end;

procedure KLiRunner.Terminate;
begin
  FTerminated := true;
end;

function KLiRunner.GetString(const ID: string): string;
var
  vname, mname: string; // variant & module name
  R: KLiFindRec;
  curr: KLiFunc;
  found: boolean;

  function smart_call_string(func: KLiFunc): string;
  var
    V: RLseValue;
    F: boolean;
  begin
    lse_init_value(@V);
    try
      F := Goon(func, 0, @V);
      if not F then
      begin
        Result := FEngine.FError.ErrorText;
        FExcepted := false;
      end
      else Result := __AsString(@V);
    finally
      lse_set_nil(@V);
    end;
  end;
  
begin
  Result := '';
  curr := CurrentFunc;
  vname := __decodeTypeName(ID, mname);
  found := ((mname = '') and (System.Pos('::', ID) < 1) and
           curr.FindInside(vname, @R)) or
           curr.FModule.FindBy(vname, mname, @R);
  if not found then Exit;

  // 1. variable
  if R.fo_type = foVarb then
  begin
    Result := __AsString(GetValue(R.VVarb));
    Exit;
  end;

  // 2. function
  if R.fo_type = foFunc then
  begin
    if R.VFunc.IsNameCall then
      Result := smart_call_string(R.VFunc) else
      Result := R.VFunc.Prototype(true);
    Exit;
  end;

  // 3. class
  if R.fo_type = foType then
    Result := R.VType.FullName;
end;

function KLiRunner.ExecGoonNext: boolean;
begin
  if not FTerminated and not FExcepted and HasNext then
  try
    FExprrec := FCurrent^.func.FCodes[FCurrent^.next];
    sys_runner_procs[FExprrec^.Sym](Self);
  except
    ErrorRT(lse_exception_str);
  end;
  Result := not FExcepted and not FTerminated and HasNext;
end;

{ KLiSatisfy }

function KLiSatisfy.Add(T: KLiType; X: PLiExprRec; F: KLiFunc): integer;
begin
  Result := FItems.Add(NewData(T, X, F));
end;

procedure KLiSatisfy.Clear;
var
  index: integer;
begin
  try
    for index := Count - 1 downto 0 do
      Delete(index);
  finally
    FItems.Clear;
  end;
end;

constructor KLiSatisfy.Create;
begin
  FItems := TList.Create;
end;

procedure KLiSatisfy.Delete(Index: integer);
var
  data: PLiSatisfy;
begin
  data := GetData(Index);
  FItems.Delete(Index);
  lse_mem_free(data, sizeof(RLiSatisfy));
end;

destructor KLiSatisfy.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure KLiSatisfy.DupLast(N: integer);
var
  X: integer;
begin
  X := GetCount - N;
  while N > 0 do
  begin
    with GetData(X)^ do
      Add(s_type, s_expr, s_func);
    Inc(X);
    Dec(N);
  end;
end;

function KLiSatisfy.GetCount: integer;
begin
  Result := FItems.Count;
end;

function KLiSatisfy.GetData(Index: integer): PLiSatisfy;
begin
  Result := FItems[Index];
end;

function KLiSatisfy.GetExpr(Index: integer): PLiExprRec;
begin
  Result := GetData(Index)^.s_expr;
end;

function KLiSatisfy.GetFunc(Index: integer): KLiFunc;
begin
  Result := GetData(Index)^.s_func;
end;

function KLiSatisfy.GetLastData: PLiSatisfy;
begin
  Result := FItems.Last;
end;

function KLiSatisfy.GetLastExpr: PLiExprRec;
begin
  Result := GetLastData^.s_expr;
end;

function KLiSatisfy.GetLastFunc: KLiFunc;
begin
  Result := GetLastData^.s_func;
end;

function KLiSatisfy.GetLastType: KLiType;
begin
  Result := GetLastData^.s_type;
end;

function KLiSatisfy.GetType(Index: integer): KLiType;
begin
  Result := GetData(Index)^.s_type;
end;

function KLiSatisfy.NewData(T: KLiType; X: PLiExprRec; F: KLiFunc): PLiSatisfy;
begin
  Result := lse_mem_alloc(sizeof(RLiSatisfy));
  Result^.s_type := T;
  Result^.s_expr := X;
  Result^.s_func := F;
end;

procedure KLiSatisfy.Press(Count: integer);
begin
  Count := Min(GetCount, Count);
  while Count > 0 do
  begin
    Delete(GetCount - 1);
    Dec(Count);
  end;
end;

{ KLiHashed }

constructor KLiHashed.Create(AEngine: KLiEngine; Size: cardinal);
begin
  inherited Create(Size);
  FObjRec.or_object := Self;
  FObjRec.or_type := KT_HASHED;
  FEngine := AEngine;
  FEngine.OrEnter(@FObjRec);
end;

destructor KLiHashed.Destroy;
begin
  FEngine.OrLeave(@FObjRec);
  inherited;
end;

procedure KLiHashed.DoListKeys(const Key: string; Value, Param: pointer);
var
  L: KLiVarList;
begin
  L := KLiVarList(Param);
  L.Add(Key);
end;

function KLiHashed.FindValue(const Key: string): PLseValue;
begin
  Result := DoGet(Key);
end;

function KLiHashed.ForceValue(const Key: string): PLseValue;
var
  hash: PLiHashItem;
begin
  hash := Find(Key);
  if hash = nil then
  begin
    Result := __NewNil;
    DoPut(Key, Result);
  end
  else Result := hash^.hi_data;
end;

procedure KLiHashed.FreeItem(Item: PLiHashItem);
begin
  if Item^.hi_data <> nil then
  begin
    __FreeValue(Item^.hi_data);
    Item^.hi_data := nil;
  end;
  inherited;
end;

procedure KLiHashed.ListKeys(List: KLiVarList);
begin
  EnumKeyData({$IFDEF FPC}@{$ENDIF}DoListKeys, List);
end;

procedure KLiHashed.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KR_HASHED, Self);
end;

function KLiHashed.SetInt64(const Key: string; Value: int64): PLseValue;
begin
  Result := ForceValue(Key);
  lse_set_int64(Result, Value);
end;

function KLiHashed.SetObject(const Key: string; Obj: pointer; Clss: PLseType): PLseValue;
begin
  Result := ForceValue(Key);
  lse_set_object(Result, Clss, Obj);
end;

function KLiHashed.SetStr(const Key, Value: string): PLseValue;
begin
  Result := ForceValue(Key);
  lse_set_string(Result, Value);
end;

function KLiHashed.SetValue(const Key: string; Value: PLseValue): PLseValue;
begin
  Result := ForceValue(Key);
  lse_set_value(Result, Value);
end;

////////////////////////////////////////////////////////////////////////////////

var
  init_lysee: boolean;

procedure InitLyseeKernel;
var
  X: KLiSymbol;
begin
  try
    if not init_lysee then
    begin
      init_lysee := true;
      lse_funcs.__coinit;
      sys_libraries := __newNamedList(false);
      sys_spinlock := TLseSpinLock.Create;
      sys_version := LSE_VERSION;
      __LoadConfig('');
      __SetupLseClasses;

      for X := Low(KLiSymbol) to High(KLiSymbol) do
      begin
        sys_runner_procs[X] := @__runner_error;
        if X in OperIDSyms then
          KLiFunc_operator.Create(X);
      end;
      sys_oper_inc := sys_module.FindFunc('+');

      sys_runner_procs[syPress]    := {$IFDEF FPC}@{$ENDIF}__runner_press;
      sys_runner_procs[syID]       := {$IFDEF FPC}@{$ENDIF}__runner_ID;
      sys_runner_procs[syBecome]   := {$IFDEF FPC}@{$ENDIF}__runner_become;
      sys_runner_procs[syFloat]    := {$IFDEF FPC}@{$ENDIF}__runner_float;
      sys_runner_procs[syCall]     := {$IFDEF FPC}@{$ENDIF}__runner_call;
      sys_runner_procs[syEcho]     := {$IFDEF FPC}@{$ENDIF}__runner_echo;
      sys_runner_procs[syInt]      := {$IFDEF FPC}@{$ENDIF}__runner_int;
      sys_runner_procs[syStr]      := {$IFDEF FPC}@{$ENDIF}__runner_str;
      sys_runner_procs[syAdd]      := {$IFDEF FPC}@{$ENDIF}__runner_add;
      sys_runner_procs[syDec]      := {$IFDEF FPC}@{$ENDIF}__runner_dec;
      sys_runner_procs[syMul]      := {$IFDEF FPC}@{$ENDIF}__runner_mul;
      sys_runner_procs[syDiv]      := {$IFDEF FPC}@{$ENDIF}__runner_div;
      sys_runner_procs[syMod]      := {$IFDEF FPC}@{$ENDIF}__runner_mod;
      sys_runner_procs[syBNot]     := {$IFDEF FPC}@{$ENDIF}__runner_bnot;
      sys_runner_procs[syBXor]     := {$IFDEF FPC}@{$ENDIF}__runner_bxor;
      sys_runner_procs[syBOr]      := {$IFDEF FPC}@{$ENDIF}__runner_bor;
      sys_runner_procs[syBAnd]     := {$IFDEF FPC}@{$ENDIF}__runner_band;
      sys_runner_procs[syBShl]     := {$IFDEF FPC}@{$ENDIF}__runner_bshl;
      sys_runner_procs[syBShr]     := {$IFDEF FPC}@{$ENDIF}__runner_bshr;
      sys_runner_procs[syNot]      := {$IFDEF FPC}@{$ENDIF}__runner_not;
      sys_runner_procs[syNeg]      := {$IFDEF FPC}@{$ENDIF}__runner_neg;
      sys_runner_procs[syEQ]       := {$IFDEF FPC}@{$ENDIF}__runner_eq;
      sys_runner_procs[syNE]       := {$IFDEF FPC}@{$ENDIF}__runner_ne;
      sys_runner_procs[syLess]     := {$IFDEF FPC}@{$ENDIF}__runner_less;
      sys_runner_procs[syLE]       := {$IFDEF FPC}@{$ENDIF}__runner_le;
      sys_runner_procs[syMore]     := {$IFDEF FPC}@{$ENDIF}__runner_more;
      sys_runner_procs[syME]       := {$IFDEF FPC}@{$ENDIF}__runner_me;
      sys_runner_procs[syIn]       := {$IFDEF FPC}@{$ENDIF}__runner_in;
      sys_runner_procs[syAnd]      := {$IFDEF FPC}@{$ENDIF}__runner_and;
      sys_runner_procs[syOr]       := {$IFDEF FPC}@{$ENDIF}__runner_or;
      sys_runner_procs[syType]     := {$IFDEF FPC}@{$ENDIF}__runner_type;
      sys_runner_procs[syFunc]     := {$IFDEF FPC}@{$ENDIF}__runner_func;
      sys_runner_procs[syVarList]  := {$IFDEF FPC}@{$ENDIF}__runner_varlist;
      sys_runner_procs[syNil]      := {$IFDEF FPC}@{$ENDIF}__runner_nil;
      sys_runner_procs[syGetEnv]   := {$IFDEF FPC}@{$ENDIF}__runner_getenv;
      sys_runner_procs[syGetSV]    := {$IFDEF FPC}@{$ENDIF}__runner_getsv;
      sys_runner_procs[sySetSV]    := {$IFDEF FPC}@{$ENDIF}__runner_setsv;
      sys_runner_procs[syFormat]   := {$IFDEF FPC}@{$ENDIF}__runner_format;
      sys_runner_procs[syModule]   := {$IFDEF FPC}@{$ENDIF}__runner_module;
      sys_runner_procs[syIs]       := {$IFDEF FPC}@{$ENDIF}__runner_is;
      sys_runner_procs[syAs]       := {$IFDEF FPC}@{$ENDIF}__runner_as;
      sys_runner_procs[sySTMT]     := {$IFDEF FPC}@{$ENDIF}__runner_statement;
      sys_runner_procs[syVarGen]   := {$IFDEF FPC}@{$ENDIF}__runner_vargen;
      sys_runner_procs[syAsk]      := {$IFDEF FPC}@{$ENDIF}__runner_ask;
      sys_runner_procs[syLabel]    := {$IFDEF FPC}@{$ENDIF}__runner_statement;
      sys_runner_procs[syIdle]     := {$IFDEF FPC}@{$ENDIF}__runner_next;
      sys_runner_procs[syTry]      := {$IFDEF FPC}@{$ENDIF}__runner_try;
      sys_runner_procs[syReturn]   := {$IFDEF FPC}@{$ENDIF}__runner_return;
      sys_runner_procs[syJump]     := {$IFDEF FPC}@{$ENDIF}__runner_jump;
      sys_runner_procs[syJmpF]     := {$IFDEF FPC}@{$ENDIF}__runner_jmpf;
      sys_runner_procs[syJmpT]     := {$IFDEF FPC}@{$ENDIF}__runner_jmpt;
      sys_runner_procs[syJmpFP]    := {$IFDEF FPC}@{$ENDIF}__runner_jmpfpop;
      sys_runner_procs[syJmpTP]    := {$IFDEF FPC}@{$ENDIF}__runner_jmptpop;
      sys_runner_procs[syGoto]     := {$IFDEF FPC}@{$ENDIF}__runner_goto;
      sys_runner_procs[syGoTP]     := {$IFDEF FPC}@{$ENDIF}__runner_gototp;
      sys_runner_procs[syGoFP]     := {$IFDEF FPC}@{$ENDIF}__runner_gotofp;
      sys_runner_procs[syHashed]   := {$IFDEF FPC}@{$ENDIF}__runner_hashed;
      sys_runner_procs[syRINR]     := {$IFDEF FPC}@{$ENDIF}__runner_RINR;
      sys_runner_procs[syGETV]     := {$IFDEF FPC}@{$ENDIF}__runner_GETV;
      sys_runner_procs[sySETV]     := {$IFDEF FPC}@{$ENDIF}__runner_SETV;
      sys_runner_procs[syLike]     := {$IFDEF FPC}@{$ENDIF}__runner_Like;
      sys_runner_procs[syGetIV]    := {$IFDEF FPC}@{$ENDIF}__runner_getiv;
      sys_runner_procs[sySetIV]    := {$IFDEF FPC}@{$ENDIF}__runner_setiv;
      sys_runner_procs[syDupLast]  := {$IFDEF FPC}@{$ENDIF}__runner_duplast;
      sys_runner_procs[syFill]     := {$IFDEF FPC}@{$ENDIF}__runner_fill;
      sys_runner_procs[sySend]     := {$IFDEF FPC}@{$ENDIF}__runner_send;
      sys_runner_procs[syCase]     := {$IFDEF FPC}@{$ENDIF}__runner_case;
    end;
  except
    { safe & quiet }
  end;
end;

////////////////////////////////////////////////////////////////////////////////

var
  exit_lysee: boolean;

procedure ExitLyseeKernel;
begin
  try
    if not exit_lysee then
    begin
      exit_lysee := true;
      __releaseSOList(sys_libraries);
      __freeAndNil(sys_configures);
      __freeAndNil(sys_mimes);
      __freeAndNil(sys_spinlock);
    end;
  except
    { safe & quiet }
  end;
end;

initialization
begin
  { do nothing }
end;

finalization
begin
  ExitLyseeKernel;
end;

end.

