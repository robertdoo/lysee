{==============================================================================}
{        UNIT: lse_kernel                                                      }
{ DESCRIPTION: kernel of lysee                                                 }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/29                                                      }
{    MODIFIED: 2011/07/17                                                      }
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
  SysUtils, Classes, SyncObjs, lseu
  {$IFDEF WINDOWS},Windows{$ENDIF};

const
  AlphaChar     = ['A'..'Z', 'a'..'z'];
  DigitChar     = ['0'..'9'];
  AlnumChar     = AlphaChar + DigitChar;
  IDChar        = AlnumChar + ['_'];
  IDHeadChar    = AlphaChar + ['_'];
  PunctChar     = ['!'..'~'] - AlnumChar;
  CntrlChar     = [#$00..#$1F, #$7F];
  SpaceChar     = [#$09, #$0A, #$0C, #$0D, #$20];
  QuoteChar     = ['"', ''''];
  HexChar       = ['A'..'F', 'a'..'f'] + DigitChar;
  MaxCaptures   = 32;

type
  KLiTokenizer  = class; {forward}
  KLiTokens     = class;
  KLiParser     = class;
  KLiFunc       = class;
  KLiFunc_curry = class;
  KLiFunc_oper  = class;
  KLiCodes      = class;
  KLiType       = class;
  KLiSyntax     = class;
  KLiModule     = class;
  KLiModuleList = class;
  KLiError      = class;
  KLiEngine     = class;
  KLiVarList    = class;
  KLiVarSnap    = class;
  KLiCallStack  = class;
  KLiRunner     = class;
  KLiHashed     = class;
  KLiHashTable  = class;
  KLiNameObject = class;
  KLiNameHashed = class;
  KLiSpinLock   = class;
  KLiMD5        = class;

  KLiCharSet = set of char;
  KLiExtInt = (eiNone, eiInt, eiExt);

  RLiPos = packed record
    row, col: word;
    module: KLiModule;
  end;
  PLiPos = ^RLiPos;

  RLiVarb = packed record
    va_name: string;
    va_type: KLiType;
  end;
  PLiVarb = ^RLiVarb;

  KLiSymbol = (
    syError, syBegin, syImport, syConst, sySyntax, syLambda, syClass,
    syThis, syObject, syWith, syLet, sySet, syGet, syHas, syDefine,
    syReturn, syIf, syThen, syElif, syElse, syWhile, syRepeat, syUntil,
    syFor, syDo, syBreak, syContinue, sySwitch, syCase, syTry, syExcept,
    syFinally, syIn, syIs, syAs, syLike, syAnd, syOr, syNot, syFrom,
    syTo, syEnd, syBecome, syAdd, syDec, syMul, syDiv, syMod, syBNot,
    syBXor, syBAnd, syBOr, syBShr, syBShl, syFill, syLParen, syRParen,
    syLBlock, syRBlock, syLArray, syRArray, syDot, syAsk, syDot2,
    syComma, sySemicolon, syEQ, syRefer, syNE, syLess, syLE, syMore,
    syME, syFormat, syID, syFloat, syInt, syStr, syNil, syNeg, syJmpT,
    syJmpF, syJmpTP, syJmpFP, syJump, syIdle, syGetEnv, syLabel, syGoto,
    syGoTP, syGoFP, syVarGen, syRINR, syGETV, sySETV, syGetIV, sySetIV,
    syDupLast, syEOF, syEcho, syGetSV, sySetSV, sySend, syVarList,
    syHashed, syReferAsk, sySTMT
  );
  KLiSymbols = set of KLiSymbol;

  PLiToken = ^RLiToken;
  RLiToken = packed record
    tk_next: PLiToken;
    tk_sym : KLiSymbol;
    tk_name: string;
    tk_prmc: integer;
    tk_pos : RLiPos;
    case KLiSymbol of
    syInt  : (VInteger: int64);
    syFloat: (VFLoat  : double);
    syGoto : (VLabel  : PLiToken);
    syID   : (VPureID : boolean);
  end;

  { KLiNameObject }

  KLiNameObject = class(TLseObject)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  { KLiNameHashed }

  PLiNameItem = ^RLiNameItem;
  RLiNameItem = packed record
    ni_next: PLiNameItem;
    ni_nobj: KLiNameObject;
  end;

  KLiNameHashed = class(TLseObject)
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
    procedure Put(AObj: KLiNameObject);
    function Get(const Key: string): KLiNameObject;
  end;

  { KLiTokenizer }

  KLiTokenizer = class(TLseObject)
  private
    FTokenList: array[0..7] of RLiToken; {<--token buffer list}
    FUnused: PLiToken;                   {<--unused tokens}
    FCurrent: PLiToken;                  {<--current token}
    FCode: string;                       {<--script code}
    FBase: pchar;                        {<--code address}
    FSize: integer;                      {<--code size}
    FPosition: integer;                  {<--current position}
    FChar: char;                         {<--current char}
    FRow: integer;                       {<--current row}
    FCol: integer;                       {<--current column}
    FBuffer: TMemoryStream;              {<--used by get_string}
    FEOF: boolean;                       {<--end of file}
    FSkip0A: boolean;                    {<--GetChar skip #10}
    FNextToken: PLiToken;                {<--tokens put back}
    function GetChar: boolean;
    function PeekChar: char;
    function GotoChar(Chars: KLiCharSet): boolean;
    function SkipSpaces: boolean;
    function GetToken(token: PLiToken; var IsStr: boolean): boolean;
    function GetCurrentToken: PLiToken;
  public
    constructor Create(const Script: string);
    destructor Destroy;override;
    function PutBack(Token: PLiToken): PLiToken;overload;
    function PutBack(Tokens: TList): integer;overload;
    function GetNextToken(var IsStr: boolean): PLiToken;
    function PeekNextToken: PLiToken;
    function PeekNextThreeTokens(var one, two, three: PLiToken): integer;
    function PrepareTokens(Count: integer): integer;
    function DupCurrentToken: PLiToken;
    property Current: PLiToken read GetCurrentToken;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Position: integer read FPosition;
    property Code: string read FCode;
  end;

  { KLiTokens }
  
  KLiTokens = class(TLseObject)
  private
    FItems: TList;
    function GetItem(index: integer): PLiToken;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function Next: PLiToken;
    property Items[index: integer]: PLiToken read GetItem;default;
    property Count: integer read GetCount;
  end;

  { KLiParser }

  KLiParser = class(TLseObject)
  private
    FTokenizer: KLiTokenizer; {<--tokens analyzer}
    FSymbols: KLiTokens;      {<--token list}
    FLast: PLiToken;          {<--last token}
    FCurrent: KLiFunc;        {<--current function}
    FModule: KLiModule;       {<--current module}
    FRunner: KLiRunner;       {<--current runner}
    FTryCount: integer;       {<--embeded try blocks}
    FCatchCount: integer;     {<--embeded catch blocks}
    FIsShadow: boolean;       {<--is a shadow parser}
    FBreakLabel: string;      {<--break label name}
    FContinueLabel: string;   {<--continue label name}
    FInTermCount: integer;    {<--statement count in term}
    function Error: KLiError;
    function Shadow: KLiParser;
    function CurCodes: KLiCodes;
    function AddToken(token: PLiToken): PLiToken;
    function AddClone(Expr: PLiToken): PLiToken;
    function GetSym(sym: PLiToken): boolean;
    function GetNextSym: boolean;
    function PeekNextSym: KLiSymbol;
    function PeekNextTwoSym(var one, two: KLiSymbol): integer;
    procedure SymGotoNext;
    procedure SymTestLast(Syms: KLiSymbols);
    procedure SymTestNext(Syms: KLiSymbols);
    procedure SymTestLastPureID;
    procedure SymTestNextPureID;
    function CloneSym(sym: PLiToken): PLiToken;
    procedure ExpandSyntax(ASyntax: KLiSyntax);
    function FindSyntax(const ID: string): KLiSyntax;
    procedure SaveLabels(var BreakLabel, ContinueLabel: string; CreateNewLabels: boolean);
    procedure RestoreLabels(const BreakLabel, ContinueLabel: string);
    procedure ParseBody(EndSym: KLiSymbol; OnHead: boolean);
    procedure ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
    procedure ParseStatement(OnHead: boolean);
    procedure ParseExpr(EndSyms: KLiSymbols; OnHead: boolean);
    procedure ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
  public
    constructor Create(AModule: KLiModule);
    destructor Destroy;override;
    function Parse(const Code: string): KLiFunc;
    function ParseAndFree(const Code: string): KLiFunc;
    function LastRow: integer;
    function LastCol: integer;
    function LastVal: string;
    function LastModule: KLiModule;
  end;

  { KLiFunc }

  KLiFindObject = (foNone, foVarb, foFunc, foType, foSyntax, foModule);
  KLiFindObjects = set of KLiFindObject;

  RLiFind = packed record
    case fo_type: KLiFindObject of
      foVarb  : (VVarb  : PLiVarb);
      foFunc  : (VFunc  : KLiFunc);
      foType  : (VType  : KLiType);
      foSyntax: (VSyntax: KLiSyntax);
      foModule: (VModule: KLiModule);
  end;
  PLiFind = ^RLiFind;

  KLiFuncState = (fusMainFunc, fusInitFunc, fusCurry, fusNameCall, fusLambda,
                  fusConst, fusEmpty);
  KLiFuncStates = set of KLiFuncState;

  KLiFunc = class(KLiNameObject)
  private
    FModule: KLiModule;
    FParams: array of PLiVarb;
    FResultType: KLiType;
    FDescription: string;
    FState: KLiFuncStates;
    FCodes: KLiCodes;
    FProc: pointer;
    FNext: KLiFunc;
    FPrev: KLiFunc;
    function HasState(Index: KLiFuncState): boolean;
    procedure SetState(Index: KLiFuncState; Value: boolean);
  public
    constructor Create(Parent: KLiModule; AResultType: KLiType;
      const AName: string; Params: TStringList; Proc: pointer);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    procedure AddParam(const VName: string; VType: KLiType);
    procedure AddSuper;
    function HasSuper: boolean;
    function FindParam(const VName: string): PLiVarb;
    function ParamCount: integer;
    function GetParam(Index: integer): PLiVarb;
    function Curry(List: KLiVarList; Module: KLiModule): KLiFunc;overload;
    function Curry(Data: PLseValue; Module: KLiModule): KLiFunc;overload;
    procedure DumpCode(list: TStrings; const margin: string);
    function Prototype: string;
    function FullName: string;
    function FindInside(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID: string; rec: PLiFind; Range: KLiFindObjects = []): boolean;
    function FindMethod(const AName: string; AType: KLiType): KLiFunc;
    function FindCreate(AType: KLiType): KLiFunc;
    function Engine: KLiEngine;
    property Module: KLiModule read FModule;
    property ResultType: KLiType read FResultType write FResultType;
    property IsMainFunc: boolean index fusMainFunc read HasState;
    property IsInitFunc: boolean index fusInitFunc read HasState write SetState;
    property IsCurryFunc: boolean index fusCurry read HasState write SetState;
    property IsNameCall: boolean index fusNameCall read HasState write SetState;
    property IsConstFunc: boolean index fusConst read HasState write SetState;
    property IsLambdaFunc: boolean index fusLambda read HasState write SetState;
    property IsEmptyFunc: boolean index fusEmpty read HasState;
    property Description: string read FDescription write FDescription;
    property Proc: pointer read FProc write FProc;
    property Codes: KLiCodes read FCodes;
    property Next: KLiFunc read FNext;
  end;

  { KLiCodes }

  KLiCodes = class(TLseObject)
  private
    FFunc: KLiFunc;
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): PLiToken;
    function GetLast: PLiToken;
  public
    constructor Create(AFunc: KLiFunc);
    destructor Destroy;override;
    procedure DumpCode(List: TStrings; const Margin: string);
    procedure Clear(Sender: TObject);
    procedure Add(AExprRec: PLiToken);
    function AddNew(sym: KLiSymbol; SymPos: PLiPos): PLiToken;
    function AddClone(Expr: PLiToken): PLiToken;
    function AddToken(token: PLiToken): PLiToken;
    function AddGoto(const Name: string; Pos: RLiPos): PLiToken;
    function AddRinr(const Name: string; Pos: RLiPos): PLiToken;
    function AddLabel(const Name: string; const Pos: RLiPos): PLiToken;
    function FindLabel(const Name: string): PLiToken;
    property Count: integer read GetCount;
    property Last: PLiToken read GetLast;
    property Items[Index: integer]: PLiToken read GetItem;default;
  end;

  { KLiFunc_curry }

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

  KLiFunc_curry = class(KLiFunc)
  private
    FCurry: array of PLseValue;
    FCurryFunc: KLiFunc;
    FObjRec: RLiObjRec;
  public
    constructor Create(AModule: KLiModule; const AName: string; AFunc: KLiFunc);
    destructor Destroy;override;
    procedure GarbageCollect;
    function ObjRec: PLiObjRec;
    function AddCurry(value: PLseValue): integer;
    function CurryCount: integer;
    function CurryData(Index: integer): PLseValue;
    property CurryFunc: KLiFunc read FCurryFunc;
  end;

  { KLiFunc_oper }
  
  KLiFunc_oper = class(KLiFunc)
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
    procedure Cast(V: PLseValue; E: KLiEngine);
    procedure Default(V: PLseValue);
    function Prototype(const ID: string): string;
    function ObjectToString(obj: pointer): string;
    function StrecToObject(const S: PLseString; Engine: KLiEngine): pointer;
    function IsSimpleType: boolean;
    function Match(AType: KLiType): boolean;
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
    constructor Create(const MName: string; MEngine: KLiEngine; MType: KLiModuleType);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
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
    function Find(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID, module_name: string; rec: PLiFind): boolean;
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

  KLiModuleList = class(TLseObject)
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

  KLiError = class(TLseObject)
  private
    FErrno: integer;
    FErrID: string;
    FMsg: string;
    FModule: string;
    FModuleFile: string;
    FRow: integer;
    FCol: integer;
    FEngine: KLiEngine;
    procedure SymNotFound(Parser: KLiParser);
    procedure SymUnexpected(Parser: KLiParser);
    procedure Redeclared(Parser: KLiParser);
    procedure LabelNotExists(func: KLiFunc; expr: PLiToken);
  public
    constructor Create(AEngine: KLiEngine);
    procedure Clear;
    procedure Write(const Name: string; Errno, Row, Col: integer;
                    const Module, Msg, FileName: string);
    procedure Error(const Name: string; Errno, Row, Col: integer;
                    const Module, Msg, FileName: string);
    procedure SyntaxErr(Errno, Row, Col: integer;
                    const Module, Fmt, FileName: string;
                    const Args: array of const);
    procedure ImportErr(Errno, Row, Col: integer;
                    const Module, Fmt, FileName: string;
                    const Args: array of const);
    function ErrorModule(func: KLiFunc; expr: PLiToken): KLiModule;
    function ErrorText: string;
    property Errno: integer read FErrno write FErrno;
    property Name: string read FErrID write FErrID;
    property Msg: string read FMsg write FMsg;
    property Module: string read FModule write FModule;
    property ModuleFile: string read FModuleFile write FModuleFile;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
  end;

  { KLiEngine }

  KLiNotifyEvent = procedure(Sender: pointer) of object;
  KLiReadBuf = procedure(Sender: TObject; buf: pchar; var Count: integer) of object;

  KLiEngine = class(TLseObject)
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
    FCompiledObjects: TList;   {<--compiled objects}
    FReady: boolean;
    FOnReadBuf: KLiReadBuf;
    FOrChain: PLiObjRec;
    FNameSeed: cardinal;         {<--label seed}
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
    procedure AddCompiled(AObject: TLseObject);
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
    property OnReadBuf: KLiReadBuf read FOnReadBuf write FOnReadBuf;
    property OrChain: PLiObjRec read FOrChain write FOrChain;
    property Input: PLseStream read GetInputStream write SetInputStream;
    property Output: PLseStream read GetOutputStream write SetOutputStream;
    property Errput: PLseStream read GetErrputStream write SetErrputStream;
    property CompiledObjects: TList read FCompiledObjects write FCompiledObjects;
  end;

  { KLiVarList }

  KLiVarList = class(TLseObject)
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
    FNames: array of string;
  public
    constructor Create(Func: KLiFunc);
    destructor Destroy;override;
    procedure Reset;
    procedure SetValue(const Name: string; V: PLseValue);
    function NewValue(const Name: string): PLseValue;
    function GetValue(const Name: string): PLseValue;
    function IndexOf(const Name: string): integer;
    function Super: KLiVarSnap;
  end;

  { snap }
  
  PLiSnap = ^RLiSnap;
  RLiSnap = packed record
    func  : KLiFunc;
    base  : integer;
    next  : integer;
    values: KLiVarSnap; // Param & Local variable values
    prior : PLiSnap;    // prior FCurrent
    exprec: PLiToken; // prior FExprrec
    output: PLseValue;  // result
    outype: KLiType;    // result type
  end;

  { KLiCallStack }

  RLiCallSnap = packed record
    call: PLseParam;
    snap: PLiSnap;
  end;
  PLiCallSnap = ^RLiCallSnap;

  KLiCallStack = class(TLseObject)
  private
    FRunner: KLiRunner;
    FStack: TList;
    FCount: integer;
    procedure AddCallSnap(Call: PLseParam; Snap: PLiSnap);
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
    property Items[Index: integer]: PLiCallSnap read GetItem;default;
  end;
  
  { KLiRunner }

  KLiRunnerProc = procedure(Runner: KLiRunner);

  RLiMatch = packed record
    mr_str: pchar;
    mr_len: integer;
  end;
  PLiMatch = ^RLiMatch;
  
  RLiRegexpr = packed record
    mp_patten  : pchar;               // patten string
    mp_anchor  : boolean;             // patten start with '^'?
    mp_boscp   : array[0..1] of char; // buffer of single char patten
    mp_source  : pchar;               // source string
    mp_eos     : pchar;               // end of source string
    mp_escape  : char;                // escape char, default '\'
    mp_captures: array[0..MaxCaptures - 1] of RLiMatch;
    mp_level   : integer;             // total number of captures
    mp_result  : RLiMatch;         // match result
  end;
  PLiRegexpr = ^RLiRegexpr;

  KLiRunner = class(TLseObject)
  private
    FEngine: KLiEngine;
    FStack: KLiVarList;
    FStackBase: integer;
    FCallStack: KLiCallStack;
    FCurrent: PLiSnap;
    FExprrec: PLiToken;
    FTerminated: boolean;
    FExcepted: boolean;
    FRegexpr: RLiRegexpr;
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
    function GetValue(varb: PLiVarb): PLseValue;
    function ListMatchResult: KLiVarList;
    function HasNext: boolean;
    function CurrentModule: KLiModule;
    function CurrentFunc: KLiFunc;
    function Regexpr: PLiRegexpr;
    property Current: PLiSnap read FCurrent;
    property Engine: KLiEngine read FEngine;
    property Stack: KLiVarList read FStack;
    property CallStack: KLiCallStack read FCallStack;
    property Exprrec: PLiToken read FExprrec;
    property Excepted: boolean read FExcepted write FExcepted;
    property Terminated: boolean read FTerminated;
  end;

  { KLiHashTable }

  PLiHashItem = ^RLiHashItem;
  RLiHashItem = packed record
    hi_next: PLiHashItem;
    hi_key : pointer;
    hi_data: pointer;
  end;

  RLiHashPair = packed record
    hp_head: PLiHashItem;
    hp_tail: PLiHashItem;
  end;
  PLiHashPair = ^RLiHashPair;
  
  KLiEnumKeyData = procedure(const Key: string; Value, Param: pointer) of object;
  
  KLiHashTable = class(TLseObject)
  private
    FBucketList: array of RLiHashPair;
    FBucketSize: integer;
    FIgnoreCase: boolean;
    FCount: integer;
  protected
    function HashOf(const Key: string): cardinal;
    function NewItem: PLiHashItem;virtual;
    procedure FreeItem(Item: PLiHashItem);virtual;
    function MatchKey(const Key, ID: string): boolean;
    procedure DoPut(const Key: string; Value: pointer);
    function DoGet(const Key: string): pointer;
  public
    constructor Create(Size: integer);
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(const Key: string);
    function Find(const Key: string): PLiHashItem;
    function IsSet(const Key: string): boolean;
    function ListKey(List: TStrings): integer;
    function ListData(List: TList): integer;
    function EnumKeyData(Proc: KLiEnumKeyData; Param: pointer): integer;
    property ItemCount: integer read FCount;
    property IgnoreCase: boolean read FIgnoreCase write FIgnoreCase;
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

  { KLiSpinLock }

  KLiSpinLock = class(TLseObject)
  private
    FCriticalSection: SyncObjs.TCriticalSection;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Enter;
    procedure Leave;
    function TryEnter: boolean;
  end;

  { KLiMD5 }
  
  pardinal = ^cardinal;

  KLiMD5 = class(TLseObject)
  private
    FBuffer: array[0..15] of cardinal;
    FA, FB, FC, FD: cardinal;
    PA, PB, PC, PD: pardinal;
    procedure Init;
    procedure Transform;
    procedure FF(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure GG(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure HH(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    procedure II(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
    function ROL(A: cardinal; Amount: byte): cardinal;
    function GetDigest: string;
  public
    function sumBuf(Abuf: pchar; count: integer): string;
    function sumStr(const S: string): string;
    function sumStream(Stream: TStream): string;
    function sumFile(const fname: string): string;
  end;

  TInitProc = procedure;
  TExitProc = TInitProc;

  KLiCompResult = (crEqual, crLess, crMore, crDiff);
  KLiCompResults = set of KLiCompResult;

{======================================================================)
(======== kernel ======================================================)
(======================================================================}
procedure kernel_startup;
procedure kernel_setup_builtin_types;
procedure kernel_load_config_file(const ConfigFile: string);
function  kernel_read_config(const ID: string): string;
function  kernel_expand_value(const S: string; E: KLiEngine): string;
procedure kernel_lock;
procedure kernel_unlock;

{======================================================================)
(======== engine ======================================================)
(======================================================================}
procedure engine_lock(engine: KLiEngine);
procedure engine_unlock(engine: KLiEngine);

{======================================================================)
(======== module ======================================================)
(======================================================================}
function module_load(const name: string; const FileName: string): KLiModule;
function module_register(const name: string; MR: PLseModule): KLiModule;
function module_search(var Name: string; const PathList: string; var IsDLL: boolean): boolean;

{======================================================================)
(======== value =======================================================)
(======================================================================}
function  value_new: PLseValue;overload;
function  value_new(Value: int64): PLseValue;overload;
function  value_new(Value: double): PLseValue;overload;
function  value_new(const Value: string): PLseValue;overload;
function  value_new(const Value: PLseString): PLseValue;overload;
function  value_new(const Value: PLseValue): PLseValue;overload;
function  value_new(const Value: pointer; AType: PLseType): PLseValue;overload;
procedure value_free(V: PLseValue);
{ convert }
function  value_strec(V: PLseValue): PLseString;
function  value_pchar(V: PLseValue): pchar;
function  value_str(V: PLseValue): string;
function  value_int(V: PLseValue): int64;
function  value_float(V: PLseValue): double;
function  value_time(V: PLseValue): TDateTime;
function  value_fname(V: PLseValue): string;
function  value_char(V: PLseValue): char;
function  value_bool(V: PLseValue): boolean;
function  value_obj(V: PLseValue): pointer;overload;
function  value_obj(V: PLseValue; T: KLiType): pointer;overload;
function  value_func(V: PLseValue): KLiFunc;
function  value_varlist(V: PLseValue): KLiVarList;
function  value_vargen(V: PLseValue; Engine: KLiEngine): PLseVargen;
function  value_type(V: PLseValue): KLiType;
{ compare }
function  value_compare(V1, V2: PLseValue): KLiCompResult;
function  value_match(V1, V2: PLseValue; Test: KLiCompResults): boolean;
{ in }
function  value_in(V: PLseValue; Host: TStringList): boolean;overload;
function  value_in(V: PLseValue; Host: KLiVarList; FindItemVG: boolean): boolean;overload;
function  value_in(V: PLseValue; Host: PLseString): boolean;overload;
function  value_in(V: PLseValue; Host: int64): boolean;overload;
function  value_in(V: PLseValue; Host: KLiHashed): boolean;overload;
{ operators }
procedure value_add(V1, V2: PLseValue);                // V1 <=     V1  +   V2
procedure value_dec(V1, V2: PLseValue);                // V1 <=     V1  -   V2
procedure value_mul(V1, V2: PLseValue);                // V1 <=     V1  *   V2
procedure value_div(V1, V2: PLseValue);                // V1 <=     V1  /   V2
procedure value_mod(V1, V2: PLseValue; R: KLiRunner);  // V1 <=     V1  %   V2
procedure value_neg(V1: PLseValue);                    // V1 <=   - V1
procedure value_xor(V1, V2: PLseValue);                // V1 <=     V1  ^   V2
procedure value_and(V1, V2: PLseValue);                // V1 <=     V1  &   V2
procedure value_or(V1, V2: PLseValue);                 // V1 <=     V1  |   V2
procedure value_shl(V1, V2: PLseValue; R: KLiRunner);  // V1 <=     V1  <<  V2
procedure value_shr(V1, V2: PLseValue);                // V1 <=     V1  >>  V2
procedure value_not(V1: PLseValue);                    // V1 <=   ~ V1
procedure value_logic_and(V1, V2: PLseValue);          // V1 <=     V1  and V2
procedure value_logic_or(V1, V2: PLseValue);           // V1 <=     V1  or  V2
procedure value_logic_not(V1: PLseValue);              // V1 <= not V1
procedure value_equal(V1, V2: PLseValue);              // V1 <=     V1 ==   V2
procedure value_diff(V1, V2: PLseValue);               // V1 <=     V1 !=   V2
procedure value_less(V1, V2: PLseValue);               // V1 <=     V1 <    V2
procedure value_eqless(V1, V2: PLseValue);             // V1 <=     V1 <=   V2
procedure value_more(V1, V2: PLseValue);               // V1 <=     V1 >    V2
procedure value_eqmore(V1, V2: PLseValue);             // V1 <=     V1 >=   V2
procedure value_abseq(V1, V2: PLseValue);              // V1 <=     V1 ===  V2
procedure value_like(V1, V2: PLseValue; R: KLiRunner); // V1 <=     V1 like V2
procedure value_fill(V1, V2: PLseValue; R: KLiRunner); // V1 <=     V1 <<<  V2
procedure value_is(V1, V2: PLseValue);                 // V1 <=     V1 is   V2
procedure value_as(V1, V2: PLseValue; E: KLiEngine);   // V1 <=     V1 as   V2

{======================================================================)
(======== PLseParam ===================================================)
(======================================================================}
function  get_runner(Param: PLseParam): KLiRunner;
function  get_engine(Param: PLseParam): KLiEngine;
function  get_this(Param: PLseParam; var This): boolean;
procedure set_error(Param: PLseParam);overload;
procedure set_error(Param: PLseParam; const Msg: string);overload;
procedure set_error(Param: PLseParam; const Msg: string; const Args: array of const);overload;
procedure set_error(Param: PLseParam; const EID: string; Errno: integer; const Msg: string);overload;
procedure set_error_this(Param: PLseParam);

{======================================================================)
(======== tokens ======================================================)
(======================================================================)
( token_new    : create a empty token
( token_free   : release a token
( token_copy   : copy token data
( token_clone  : clone a token
( token_pure_ID: is pure identity?(not contains '::')
(----------------------------------------------------------------------}
function  token_new: PLiToken;
procedure token_free(Token: PLiToken);
procedure token_copy(SrcToken, DstToken: PLiToken);
function  token_clone(Token: PLiToken): PLiToken;
function  token_pure_ID(token: PLiToken): boolean;
procedure token_reset(Token: PLiToken);

{======================================================================)
(======== regexpr =====================================================)
(======================================================================}
function regexpr_init(mp: PLiRegexpr; const Patten: pchar; EscapeCh: char): boolean;overload;
function regexpr_init(mp: PLiRegexpr; const Patten: pchar): boolean;overload;
function regexpr_exec(mp: PLiRegexpr; S: pchar; L: integer): boolean;overload;
function regexpr_exec(mp: PLiRegexpr; const S: PLseString): boolean;overload;

{======================================================================)
(======== U.D.C =======================================================)
(======================================================================}
procedure udc_curry(const Param: PLseParam);cdecl;
procedure udc_const(const Param: PLseParam);cdecl;
procedure udc_oper(const Param: PLseParam);cdecl;
procedure udc_empty(const Param: PLseParam);cdecl;

{======================================================================)
(======== Runner ======================================================)
(======================================================================}
procedure runner_error(Sender: KLiRunner);
procedure runner_ID(Sender: KLiRunner);
procedure runner_become(Sender: KLiRunner);
procedure runner_float(Sender: KLiRunner);
procedure runner_echo(Sender: KLiRunner);
procedure runner_int(Sender: KLiRunner);
procedure runner_str(Sender: KLiRunner);
procedure runner_add(Sender: KLiRunner);
procedure runner_dec(Sender: KLiRunner);
procedure runner_mul(Sender: KLiRunner);
procedure runner_div(Sender: KLiRunner);
procedure runner_mod(Sender: KLiRunner);
procedure runner_bnot(Sender: KLiRunner);
procedure runner_bxor(Sender: KLiRunner);
procedure runner_bor(Sender: KLiRunner);
procedure runner_band(Sender: KLiRunner);
procedure runner_bshl(Sender: KLiRunner);
procedure runner_bshr(Sender: KLiRunner);
procedure runner_not(Sender: KLiRunner);
procedure runner_neg(Sender: KLiRunner);
procedure runner_eq(Sender: KLiRunner);
procedure runner_ne(Sender: KLiRunner);
procedure runner_less(Sender: KLiRunner);
procedure runner_le(Sender: KLiRunner);
procedure runner_more(Sender: KLiRunner);
procedure runner_me(Sender: KLiRunner);
procedure runner_in(Sender: KLiRunner);
procedure runner_and(Sender: KLiRunner);
procedure runner_or(Sender: KLiRunner);
procedure runner_varlist(Sender: KLiRunner);
procedure runner_nil(Sender: KLiRunner);
procedure runner_getenv(Sender: KLiRunner);
procedure runner_getsv(Sender: KLiRunner);
procedure runner_setsv(Sender: KLiRunner);
procedure runner_format(Sender: KLiRunner);
procedure runner_is(Sender: KLiRunner);
procedure runner_as(Sender: KLiRunner);
procedure runner_vargen(Sender: KLiRunner);
procedure runner_ask(Sender: KLiRunner);
procedure runner_refer_ask(Sender: KLiRunner);
procedure runner_try(Sender: KLiRunner);
procedure runner_return(Sender: KLiRunner);
procedure runner_jump(Sender: KLiRunner);
procedure runner_jmpf(Sender: KLiRunner);
procedure runner_jmpt(Sender: KLiRunner);
procedure runner_jmpfpop(Sender: KLiRunner);
procedure runner_jmptpop(Sender: KLiRunner);
procedure runner_goto(Sender: KLiRunner);
procedure runner_gototp(Sender: KLiRunner);
procedure runner_gotofp(Sender: KLiRunner);
procedure runner_hashed(Sender: KLiRunner);
procedure runner_RINR(Sender: KLiRunner);
procedure runner_SETV(Sender: KLiRunner);
procedure runner_GETV(Sender: KLiRunner);
procedure runner_like(Sender: KLiRunner);
procedure runner_getiv(Sender: KLiRunner);
procedure runner_setiv(Sender: KLiRunner);
procedure runner_duplast(Sender: KLiRunner);
procedure runner_fill(Sender: KLiRunner);
procedure runner_send(Sender: KLiRunner);
procedure runner_case(Sender: KLiRunner);
procedure runner_STMT(Sender: KLiRunner);

{======================================================================)
(======== query =======================================================)
(======================================================================}
function  query_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
procedure query_engine_destroy(const Engine: pointer);cdecl;
function  query_engine_compile(const Engine: pointer; const code: pchar): integer;cdecl;
function  query_engine_compile_file(const Engine: pointer; const fname: pchar): integer;cdecl;
function  query_engine_execute(const Engine: pointer; const code: pchar): integer;cdecl;
function  query_engine_execute_file(const Engine: pointer; const fname: pchar): integer;cdecl;
procedure query_engine_terminate(const Engine: pointer);cdecl;
procedure query_engine_clear(const Engine: pointer);cdecl;
function  query_engine_get_args(const Engine: pointer): PLseString;cdecl;
procedure query_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
function  query_engine_errno(const Engine: pointer): integer;cdecl;
function  query_engine_error_row(const Engine: pointer): integer;cdecl;
function  query_engine_error_col(const Engine: pointer): integer;cdecl;
function  query_engine_error_name(const Engine: pointer): pchar;cdecl;
function  query_engine_error_msg(const Engine: pointer): pchar;cdecl;
function  query_engine_error_module(const Engine: pointer): pchar;cdecl;
function  query_engine_error_file(const Engine: pointer): pchar;cdecl;
function  query_engine_result_type(const Engine: pointer): pchar;cdecl;
function  query_engine_result_text(const Engine: pointer): pchar;cdecl;
function  query_engine_get_search_path(const Engine: pointer): pchar;cdecl;
procedure query_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
function  query_engine_get_main_file(const Engine: pointer): pchar;cdecl;
procedure query_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
function  query_engine_ready(const Engine: pointer): integer;cdecl;
function  query_engine_running(const Engine: pointer): integer;cdecl;
function  query_engine_terminated(const Engine: pointer): integer;cdecl;
function  query_engine_exited(const Engine: pointer): integer;cdecl;
procedure query_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
function  query_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
function  query_engine_readln(const Engine: pointer): PLseString;cdecl;
function  query_register_module(const Name: pchar; const initrec: PLseModule): pointer;cdecl;
function  query_register_type(const CR: PLseType): PLseType;cdecl;
function  query_typerec(const KernelType: pointer): PLseType;cdecl;
function  query_register_func(const FR: PLseFunc): pointer;cdecl;
procedure query_casto_string(const V: PLseValue);cdecl;
function  query_param_engine(const Param: PLseParam): PLseEngine;cdecl;
function  query_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
procedure query_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
function  query_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
function  query_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
function  query_simple_test(const Script: pchar): integer;cdecl;
function  query_startup: integer;cdecl;
procedure query_cleanup;cdecl;
function  query_keywords: pchar;cdecl;
function  query_get_kernel_file: pchar;cdecl;
procedure query_set_kernel_file(const KernelFile: pchar);cdecl;
function  query_get_program_file: pchar;cdecl;
procedure query_set_program_file(const ProgramFile: pchar);cdecl;
procedure query_load_config(const ConfigFile: pchar);cdecl;
function  query_production: pchar;cdecl;
function  query_version: pchar;cdecl;
function  query_copyright: pchar;cdecl;
function  query_tmpath: pchar;cdecl;
function  query_entry(const ID: pchar): pointer;cdecl;

{======================================================================)
(======== type ========================================================)
(======================================================================}
function type_error_otos(obj: pointer): PLseString;cdecl;
function type_type_otos(obj: pointer): PLseString;cdecl;
function type_type_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function type_function_otos(obj: pointer): PLseString;cdecl;
function type_function_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
function type_function_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function type_stream_writo(obj: pointer; stream: PLseStream): integer;cdecl;
function type_stream_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
function type_stream_vargen_lines(obj, kernel_engine: pointer): PLseVargen;cdecl;
function type_stream_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function type_stream_length(obj: pointer): integer;cdecl;
function type_string_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
function type_string_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function type_string_length(obj: pointer): integer;cdecl;
function type_varlist_otos(obj: pointer): PLseString;cdecl;
function type_varlist_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function type_varlist_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
function type_varlist_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function type_varlist_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function type_varlist_setiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function type_varlist_length(obj: pointer): integer;cdecl;
function type_module_otos(obj: pointer): PLseString;cdecl;
function type_module_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function type_module_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_module_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_hashed_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_hashed_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_hashed_length(obj: pointer): integer;cdecl;
function type_varsnap_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function type_varsnap_setiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function type_varsnap_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_varsnap_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function type_varsnap_length(obj: pointer): integer;cdecl;
function type_vargen_upto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
function type_vargen_downto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
                                       
{======================================================================)
(======== Call ========================================================)
(======================================================================}
procedure call_dir(const Param: PLseParam);cdecl;
procedure call_isdir(const Param: PLseParam);cdecl;
procedure call_isfile(const Param: PLseParam);cdecl;
procedure call_print(const Param: PLseParam);cdecl;
procedure call_printf(const Param: PLseParam);cdecl;
procedure call_println(const Param: PLseParam);cdecl;
procedure call_readln(const Param: PLseParam);cdecl;
procedure call_modules(const Param: PLseParam);cdecl;
procedure call_libs(const Param: PLseParam);cdecl;
procedure call_exit(const Param: PLseParam);cdecl;
procedure call_random(const Param: PLseParam);cdecl;
procedure call_sleep(const Param: PLseParam);cdecl;
procedure call_getenv(const Param: PLseParam);cdecl;
procedure call_dumpc(const Param: PLseParam);cdecl;
procedure call_length(const Param: PLseParam);cdecl;
procedure call_genid(const Param: PLseParam);cdecl;
procedure call_load(const Param: PLseParam);cdecl;
procedure call_parse(const Param: PLseParam);cdecl;
procedure call_eval(const Param: PLseParam);cdecl;
procedure call_format(const Param: PLseParam);cdecl;
procedure call_now(const Param: PLseParam);cdecl;
procedure call_max(const Param: PLseParam);cdecl;
procedure call_min(const Param: PLseParam);cdecl;
procedure call_leap(const Param: PLseParam);cdecl;
procedure call_which(const Param: PLseParam);cdecl;
procedure call_curry(const Param: PLseParam);cdecl;
procedure call_curryone(const Param: PLseParam);cdecl;
procedure call_gc(const Param: PLseParam);cdecl;
procedure call_apply(const Param: PLseParam);cdecl;
procedure call_tmpfname(const Param: PLseParam);cdecl;
procedure call_encodeGMT(const Param: PLseParam);cdecl;
procedure call_decodeGMT(const Param: PLseParam);cdecl;
procedure call_encodeUTF8(const Param: PLseParam);cdecl;
procedure call_decodeUTF8(const Param: PLseParam);cdecl;
procedure call_openfs(const Param: PLseParam);cdecl;
procedure call_memory(const Param: PLseParam);cdecl;
procedure call_incPD(const Param: PLseParam);cdecl;
procedure call_excPD(const Param: PLseParam);cdecl;
procedure call_veryPD(const Param: PLseParam);cdecl;
procedure call_veryUD(const Param: PLseParam);cdecl;
procedure call_msecs(const Param: PLseParam);cdecl;
procedure call_current_module(const Param: PLseParam);cdecl;
procedure call_current_func(const Param: PLseParam);cdecl;
procedure call_current_error(const Param: PLseParam);cdecl;
procedure call_current_args(const Param: PLseParam);cdecl;
procedure call_current_line(const Param: PLseParam);cdecl;
procedure call_current_envs(const Param: PLseParam);cdecl;
procedure call_current_file(const Param: PLseParam);cdecl;
procedure call_current_pd(const Param: PLseParam);cdecl;
procedure call_eol(const Param: PLseParam);cdecl;
procedure call_each(const Param: PLseParam);cdecl;
procedure call_map(const Param: PLseParam);cdecl;
procedure call_reduce(const Param: PLseParam);cdecl;
procedure call_filter(const Param: PLseParam);cdecl;
procedure call_sum(const Param: PLseParam);cdecl;
procedure call_maxint(const Param: PLseParam);cdecl;
procedure call_minint(const Param: PLseParam);cdecl;
procedure call_abs(const Param: PLseParam);cdecl;
procedure call_find(const Param: PLseParam);cdecl;
procedure call_gsub(const Param: PLseParam);cdecl;
procedure call_split(const Param: PLseParam);cdecl;
procedure call_getcs(const Param: PLseParam);cdecl;
procedure call_throw(const Param: PLseParam);cdecl;
procedure call_hex(const Param: PLseParam);cdecl;
procedure call_bitlist(const Param: PLseParam);cdecl;
procedure call_upto(const Param: PLseParam);cdecl;
procedure call_downto(const Param: PLseParam);cdecl;
procedure call_typeof(const Param: PLseParam);cdecl;
{ error }
procedure call_error_text(const Param: PLseParam);cdecl;
procedure call_error_module(const Param: PLseParam);cdecl;
procedure call_error_name(const Param: PLseParam);cdecl;
procedure call_error_message(const Param: PLseParam);cdecl;
procedure call_error_row(const Param: PLseParam);cdecl;
procedure call_error_col(const Param: PLseParam);cdecl;
procedure call_error_errno(const Param: PLseParam);cdecl;
{ function }
procedure call_func_name(const Param: PLseParam);cdecl;
procedure call_func_desc(const Param: PLseParam);cdecl;
procedure call_func_type(const Param: PLseParam);cdecl;
procedure call_func_prototype(const Param: PLseParam);cdecl;
procedure call_func_module(const Param: PLseParam);cdecl;
{ hashed }
procedure call_hashed_create(const Param: PLseParam);cdecl;
procedure call_hashed_read(const Param: PLseParam);cdecl;
procedure call_hashed_remove(const Param: PLseParam);cdecl;
procedure call_hashed_clear(const Param: PLseParam);cdecl;
procedure call_hashed_isset(const Param: PLseParam);cdecl;
procedure call_hashed_keys(const Param: PLseParam);cdecl;
procedure call_hashed_values(const Param: PLseParam);cdecl;
{ module }
procedure call_module_name(const Param: PLseParam);cdecl;
procedure call_module_desc(const Param: PLseParam);cdecl;
procedure call_module_file(const Param: PLseParam);cdecl;
procedure call_module_modules(const Param: PLseParam);cdecl;
procedure call_module_funcs(const Param: PLseParam);cdecl;
procedure call_module_types(const Param: PLseParam);cdecl;
procedure call_module_version(const Param: PLseParam);cdecl;
procedure call_module_main(const Param: PLseParam);cdecl;
procedure call_module_imports(const Param: PLseParam);cdecl;
{ vargen }
procedure call_vargen_create(const Param: PLseParam);cdecl;
procedure call_vargen_eof(const Param: PLseParam);cdecl;
procedure call_vargen_next(const Param: PLseParam);cdecl;
procedure call_vargen_rewind(const Param: PLseParam);cdecl;
{ stream }
procedure call_stream_close(const Param: PLseParam);cdecl;
procedure call_stream_eof(const Param: PLseParam);cdecl;
procedure call_stream_get_position(const Param: PLseParam);cdecl;
procedure call_stream_set_position(const Param: PLseParam);cdecl;
procedure call_stream_set_length(const Param: PLseParam);cdecl;
procedure call_stream_read(const Param: PLseParam);cdecl;
procedure call_stream_readln(const Param: PLseParam);cdecl;
procedure call_stream_write(const Param: PLseParam);cdecl;
procedure call_stream_writeln(const Param: PLseParam);cdecl;
procedure call_stream_writeTo(const Param: PLseParam);cdecl;
procedure call_stream_flush(const Param: PLseParam);cdecl;
procedure call_stream_lines(const Param: PLseParam);cdecl;
{ string }
procedure call_string_setAt(const Param: PLseParam);cdecl;
procedure call_string_name(const Param: PLseParam);cdecl;
procedure call_string_value(const Param: PLseParam);cdecl;
procedure call_string_lower(const Param: PLseParam);cdecl;
procedure call_string_upper(const Param: PLseParam);cdecl;
procedure call_string_compare(const Param: PLseParam);cdecl;
procedure call_string_replace(const Param: PLseParam);cdecl;
procedure call_string_pos(const Param: PLseParam);cdecl;
procedure call_string_lastPos(const Param: PLseParam);cdecl;
procedure call_string_left(const Param: PLseParam);cdecl;
procedure call_string_right(const Param: PLseParam);cdecl;
procedure call_string_trim(const Param: PLseParam);cdecl;
procedure call_string_trimLeft(const Param: PLseParam);cdecl;
procedure call_string_trimRight(const Param: PLseParam);cdecl;
procedure call_string_trimAll(const Param: PLseParam);cdecl;
procedure call_string_copy(const Param: PLseParam);cdecl;
procedure call_string_delete(const Param: PLseParam);cdecl;
procedure call_string_insert(const Param: PLseParam);cdecl;
procedure call_string_isAlpha(const Param: PLseParam);cdecl;
procedure call_string_isAlnum(const Param: PLseParam);cdecl;
procedure call_string_isCntrl(const Param: PLseParam);cdecl;
procedure call_string_isDigit(const Param: PLseParam);cdecl;
procedure call_string_isSpace(const Param: PLseParam);cdecl;
procedure call_string_isHex(const Param: PLseParam);cdecl;
procedure call_string_extractName(const Param: PLseParam);cdecl;
procedure call_string_extractValue(const Param: PLseParam);cdecl;
procedure call_string_saveToFile(const Param: PLseParam);cdecl;
procedure call_string_fileText(const Param: PLseParam);cdecl;
procedure call_string_lformat(const Param: PLseParam);cdecl;
procedure call_string_rformat(const Param: PLseParam);cdecl;
procedure call_string_center(const Param: PLseParam);cdecl;
procedure call_string_randomOrder(const Param: PLseParam);cdecl;
procedure call_string_html(const Param: PLseParam);cdecl;
procedure call_string_reverse(const Param: PLseParam);cdecl;
procedure call_string_isLower(const Param: PLseParam);cdecl;
procedure call_string_isUpper(const Param: PLseParam);cdecl;
procedure call_string_translate(const Param: PLseParam);cdecl;
procedure call_string_filePath(const Param: PLseParam);cdecl;
procedure call_string_fullFileName(const Param: PLseParam);cdecl;
procedure call_string_fileName(const Param: PLseParam);cdecl;
procedure call_string_fileExt(const Param: PLseParam);cdecl;
procedure call_string_changeExt(const Param: PLseParam);cdecl;
procedure call_string_hexToInt(const Param: PLseParam);cdecl;
procedure call_string_hash(const Param: PLseParam);cdecl;
procedure call_string_md5sum(const Param: PLseParam);cdecl;
{ type }
procedure call_type_name(const Param: PLseParam);cdecl;
procedure call_type_description(const Param: PLseParam);cdecl;
procedure call_type_simple(const Param: PLseParam);cdecl;
procedure call_type_module(const Param: PLseParam);cdecl;
{ varlist }
procedure call_varlist_create(const Param: PLseParam);cdecl;
procedure call_varlist_get_length(const Param: PLseParam);cdecl;
procedure call_varlist_set_length(const Param: PLseParam);cdecl;
procedure call_varlist_exchange(const Param: PLseParam);cdecl;
procedure call_varlist_move(const Param: PLseParam);cdecl;
procedure call_varlist_add(const Param: PLseParam);cdecl;
procedure call_varlist_addFrom(const Param: PLseParam);cdecl;
procedure call_varlist_fill(const Param: PLseParam);cdecl;
procedure call_varlist_insert(const Param: PLseParam);cdecl;
procedure call_varlist_delete(const Param: PLseParam);cdecl;
procedure call_varlist_clear(const Param: PLseParam);cdecl;
procedure call_varlist_copy(const Param: PLseParam);cdecl;
procedure call_varlist_left(const Param: PLseParam);cdecl;
procedure call_varlist_right(const Param: PLseParam);cdecl;
procedure call_varlist_filter(const Param: PLseParam);cdecl;
procedure call_varlist_first(const Param: PLseParam);cdecl;
procedure call_varlist_last(const Param: PLseParam);cdecl;
procedure call_varlist_shift(const Param: PLseParam);cdecl;
procedure call_varlist_pop(const Param: PLseParam);cdecl;

{======================================================================)
(======== MISC ========================================================)
(======================================================================}
function  ID_to_sym(const ID: string; DefSymbol: KLiSymbol): KLiSymbol;
function  extract_name_module(const ID: string; var Module: string): string;
function  reserved_words: string;
function  is_reserved(const ID: string; IncludeKeywords: boolean): boolean;
function  delete_comment(const S: string): string;
function  parse_config(const S: string; var ID, value: string): boolean;
function  extract_name_value(const S: string; var V: string; const separator: string = '='): string;
function  extract_name(const S: string; const separator: string = '='): string;
function  extract_value(const S: string; const separator: string = '='): string;
function  genid: string;
function  program_file: string;
function  library_file: string;
function  full_path(const path: string): string;
function  parse_int(S: pchar): int64;
function  parse_ext(S: pchar): extended;
function  parse_ext_int(var S: pchar; var iv: int64; var ei: KLiExtInt): extended;
function  parse_str(var S: pchar; desti: TStream; allow_esc_char: boolean): boolean;
function  skip_ch(const S: pchar; CharSet: KLiCharSet): pchar;
function  hex_value(ch: char): integer;
function  is_ID(S: pchar; Head: KLiCharSet = IDHeadChar): boolean;
function  is_ID_head(C: char): boolean;
function  in_charset(S: pchar; Len: integer; Chars: KLiCharSet): boolean;overload;
function  in_charset(S: pchar; Chars: KLiCharSet): boolean;overload;
function  buf_comp(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): integer;
function  buf_same(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): boolean;
function  first_pos(this: pchar; thisLen: integer; patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
function  last_pos(this: pchar; thisLen: integer; patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
function  new_named_list(Sorted: boolean): TStringList;
function  find_named(list: TStrings; const name: string): pointer;
function  named_index(list: TStrings; const name: string): integer;
function  named_exists(list: TStrings; const name: string): boolean;
function  fill_zero(buf: pointer; count: integer): pointer;
procedure check(ok: boolean; const msg: string);
function  is_file(const Source: string): boolean;
function  is_relative_fname(const FileName: string): boolean;
function  absolute_fname(const FileName, BasePath: string): string;
function  same_fname(const F1, F2: string): boolean;
function  file_text(const FileName: string): string;
function  encode_UTF8(const S: string): string;
function  decode_UTF8(const S: string): string;
function  get_open_file_mode(const openMode: string; var fileMode: word; var R, W: boolean): boolean;
function  strec_comp(S1, S2: PLseString; IgnoreCase: boolean): integer;
function  strec_count_tab(this: PLseString; var LCount, MCount, RCount: integer): integer;
function  count_tab(buf: pchar; count: integer; var LCount, MCount, RCount: integer): integer;
function  encode_HTML(Buf: pchar; Count: integer; TranslateMBC: boolean): string;
function  hash_of(const Key: string): cardinal;
function  new_string(Source: pchar; Count: integer): string;
procedure free_and_nil(var obj);
function  str_to_comma(const AStr: string): string;
function  remove_from(list: TStrings; item: pointer): boolean;
procedure release_string_object_list(var list: TStringList);
function  seek_ch(const S: pchar; CharSet: KLiCharSet): pchar;

const

  SyntaxError  = 'SyntaxError';
  ImportError  = 'ImportError';
  RuntimeError = 'RuntimeError';
  FileNotFound = 'FileNotFound';

  ESYNTAX               = 1000;
    EsSymNotFound       = 'symbol expected but file end';
    EvSymNotFound       = ESYNTAX + 1;
    EsSymUnexpected     = 'unexpected symbol "%s"';
    EvSymUnexpected     = ESYNTAX + 2;
    EsRedeclared        = 'object "%s" already exists';
    EvRedeclared        = ESYNTAX + 3;
    EsBreakNoLoop       = 'break is not in loop';
    EvBreakNoLoop       = ESYNTAX + 4;
    EsContinueNoLoop    = 'continue is not in loop';
    EvContinueNoLoop    = ESYNTAX + 5;
    EsUnknownOper       = 'unknown operator "%s"';
    EvUnknownOper       = ESYNTAX + 6;
    EsImportEachOther   = 'module "%s" and "%s" imports each other';
    EvImportEachOther   = ESYNTAX + 7;
    EsNeedPureID        = '%s is not a pure identity';
    EvNeedPureID        = ESYNTAX + 8;
    EsLabelNotExists    = 'label "%s" not exists';
    EvLabelNotExists    = ESYNTAX + 9;
  EIMPORT               = 1200;
    EsModuleReimport    = 'module "%s" reimported from another file';
    EvModuleReimport    = EIMPORT + 1;
    EsModuleNotFound    = 'module "%s" not found';
    EvModuleNotFound    = EIMPORT + 2;
    EsWrongLibrary      = 'library "%s" is invalid';
    EvWrongLibrary      = EIMPORT + 3;
  ERUNTIME              = 1300;
    EsRuntimeError      = 'unknown runtime error';
    EvRuntimeError      = ERUNTIME + 1;
    EsFuncNotSpecify    = 'function not specified';
    EvFuncNotSpecify    = ERUNTIME + 2;
    EsClassNotSpecify   = 'class not specified';
    EvClassNotSpecify   = ERUNTIME + 3;

var
  sys_kernel            : string;      {<--kernel file name}
  sys_version           : string;      {<--kernel version}
  sys_knpath            : string;      {<--kernel file path}
  sys_kndir             : string;      {<--kernel file directory}
  sys_home_path         : string;      {<--home path}
  sys_tmpath            : string;      {<--temporary path}
  sys_search_path       : string;      {<--module search path}
  sys_confile           : string;      {<--config file name}
  sys_mimefile          : string;      {<--MIME file name}
  sys_mimes             : TStringList; {<--MIME list}
  sys_configures        : TStringList; {<--configure value list}
  sys_program           : string;      {<--program file name}
  sys_process_ID        : string;      {<--process ID}
  sys_libraries         : TStringList; {<--kernel & library module list}
  sys_module            : KLiModule;   {<--builtin [sys] module}
  sys_spinlock          : KLiSpinLock; {<--kernel's spinlock}
  sys_oper_inc          : KLiFunc;     {<--sys::+}
  sys_nil               : RLseValue;   {<--default empty data}
  sys_LB                : string = sLineBreak;
  sys_runner_procs      : array[KLiSymbol] of KLiRunnerProc;
  sys_reserved_words    : string = '';
  sys_exit_lysee        : boolean = false;
  sys_init_lysee        : boolean = false;
  sys_type_list         : RLseKernelTypeList;
  sys_cik_entries       : RLseEntry = (
    cik_types           : @sys_type_list;
    cik_create          : {$IFDEF FPC}@{$ENDIF}query_engine_create;
    cik_destroy         : {$IFDEF FPC}@{$ENDIF}query_engine_destroy;
    cik_compile         : {$IFDEF FPC}@{$ENDIF}query_engine_compile;
    cik_fcompile        : {$IFDEF FPC}@{$ENDIF}query_engine_compile_file;
    cik_execute         : {$IFDEF FPC}@{$ENDIF}query_engine_execute;
    cik_fexecute        : {$IFDEF FPC}@{$ENDIF}query_engine_execute_file;
    cik_terminate       : {$IFDEF FPC}@{$ENDIF}query_engine_terminate;
    cik_clear           : {$IFDEF FPC}@{$ENDIF}query_engine_clear;
    cik_get_args        : {$IFDEF FPC}@{$ENDIF}query_engine_get_args;
    cik_set_args        : {$IFDEF FPC}@{$ENDIF}query_engine_set_args;
    cik_errno           : {$IFDEF FPC}@{$ENDIF}query_engine_errno;
    cik_error_row       : {$IFDEF FPC}@{$ENDIF}query_engine_error_row;
    cik_error_col       : {$IFDEF FPC}@{$ENDIF}query_engine_error_col;
    cik_error_name      : {$IFDEF FPC}@{$ENDIF}query_engine_error_name;
    cik_error_msg       : {$IFDEF FPC}@{$ENDIF}query_engine_error_msg;
    cik_error_module    : {$IFDEF FPC}@{$ENDIF}query_engine_error_module;
    cik_error_file      : {$IFDEF FPC}@{$ENDIF}query_engine_error_file;
    cik_result_type     : {$IFDEF FPC}@{$ENDIF}query_engine_result_type;
    cik_result_text     : {$IFDEF FPC}@{$ENDIF}query_engine_result_text;
    cik_get_search_path : {$IFDEF FPC}@{$ENDIF}query_engine_get_search_path;
    cik_set_search_path : {$IFDEF FPC}@{$ENDIF}query_engine_set_search_path;
    cik_get_main_file   : {$IFDEF FPC}@{$ENDIF}query_engine_get_main_file;
    cik_set_main_file   : {$IFDEF FPC}@{$ENDIF}query_engine_set_main_file;
    cik_ready           : {$IFDEF FPC}@{$ENDIF}query_engine_ready;
    cik_running         : {$IFDEF FPC}@{$ENDIF}query_engine_running;
    cik_terminated      : {$IFDEF FPC}@{$ENDIF}query_engine_terminated;
    cik_exited          : {$IFDEF FPC}@{$ENDIF}query_engine_exited;
    cik_write           : {$IFDEF FPC}@{$ENDIF}query_engine_write;
    cik_read            : {$IFDEF FPC}@{$ENDIF}query_engine_read;
    cik_readln          : {$IFDEF FPC}@{$ENDIF}query_engine_readln;
    cik_register_module : {$IFDEF FPC}@{$ENDIF}query_register_module;
    cik_register_type   : {$IFDEF FPC}@{$ENDIF}query_register_type;
    cik_typerec         : {$IFDEF FPC}@{$ENDIF}query_typerec;
    cik_register_func   : {$IFDEF FPC}@{$ENDIF}query_register_func;
    cik_casto_string    : {$IFDEF FPC}@{$ENDIF}query_casto_string;
    cik_param_engine    : {$IFDEF FPC}@{$ENDIF}query_param_engine;
    cik_param_format    : {$IFDEF FPC}@{$ENDIF}query_param_format;
    cik_param_error     : {$IFDEF FPC}@{$ENDIF}query_param_error;
    cik_param_push      : {$IFDEF FPC}@{$ENDIF}query_param_push;
    cik_param_goon      : {$IFDEF FPC}@{$ENDIF}query_param_goon;
    cik_production      : {$IFDEF FPC}@{$ENDIF}query_production;
    cik_version         : {$IFDEF FPC}@{$ENDIF}query_version;
    cik_copyright       : {$IFDEF FPC}@{$ENDIF}query_copyright;
    cik_tmpath          : {$IFDEF FPC}@{$ENDIF}query_tmpath;
    cik_query           : {$IFDEF FPC}@{$ENDIF}query_entry;
    cik_simple_test     : {$IFDEF FPC}@{$ENDIF}query_simple_test;
    cik_startup         : {$IFDEF FPC}@{$ENDIF}query_startup;
    cik_cleanup         : {$IFDEF FPC}@{$ENDIF}query_cleanup;
    cik_keywords        : {$IFDEF FPC}@{$ENDIF}query_keywords;
    cik_get_kernel_file : {$IFDEF FPC}@{$ENDIF}query_get_kernel_file;
    cik_set_kernel_file : {$IFDEF FPC}@{$ENDIF}query_set_kernel_file;
    cik_get_program_file: {$IFDEF FPC}@{$ENDIF}query_get_program_file;
    cik_set_program_file: {$IFDEF FPC}@{$ENDIF}query_set_program_file;
    cik_load_config     : {$IFDEF FPC}@{$ENDIF}query_load_config
  );

  { KT: KERNEL TYPE }

  KT_VOID, KT_STRING, KT_INT, KT_FLOAT, KT_VARIANT, KT_CLASS,
  KT_MODULE, KT_FUNC, KT_ERROR, KT_STREAM, KT_VARLIST,
  KT_HASHED, KT_VARGEN, KT_VARSNAP: KLiType;

  { KR: KERNEL TYPE RECORD }

  KR_VOID, KR_STRING, KR_INT, KR_FLOAT, KR_VARIANT, KR_CLASS,
  KR_MODULE, KR_FUNC, KR_ERROR, KR_STREAM, KR_VARLIST,
  KR_HASHED, KR_VARGEN, KR_VARSNAP: PLseType;

implementation

uses
  Math, DateUtils;

const
  RPN_MAIN = 'main'; {<--main function & module name}

  Symbols: array[KLiSymbol] of packed record
    SY: KLiSymbol; // symbol value
    ID: pchar;     // symbol spelling
    SM: pchar;     // symbol description
  end = (
    (SY:syError;     ID:'<ERROR>';      SM:'error'),
    (SY:syBegin;     ID:'begin';        SM:'begin ==> FIRST KEYWORD'),
    (SY:syImport;    ID:'import';       SM:'import module'),
    (sy:syConst;     ID:'const';        SM:'define constants'),
    (SY:sySyntax;    ID:'syntax';       SM:'define syntax'),
    (SY:syLambda;    ID:'lambda';       SM:'define lambda function'),
    (SY:syClass;     ID:'class';        SM:'class'),
    (SY:syThis;      ID:'this';         SM:'this'),
    (SY:syObject;    ID:'object';       SM:'object'),
    (SY:syWith;      ID:'with';         SM:'with'),
    (SY:syLet;       ID:'let';          SM:'let'),
    (SY:sySet;       ID:'set';          SM:'set'),
    (SY:syGet;       ID:'get';          SM:'get'),
    (SY:syHas;       ID:'has';          SM:'has'),
    (sy:syDefine;    ID:'def';          SM:'define functions'),
    (SY:syReturn;    ID:'return';       SM:'return'),
    (SY:syIf;        ID:'if';           SM:'if'),
    (sy:syThen;      ID:'then';         SM:'then clause'),
    (sy:syElif;      ID:'elif';         SM:'else if'),
    (SY:syElse;      ID:'else';         SM:'else'),
    (SY:syWhile;     ID:'while';        SM:'while'),
    (sy:syRepeat;    ID:'repeat';       SM:'begin do..while loop'),
    (sy:syUntil;     ID:'until';        SM:'end do..while loop'),
    (SY:syFor;       ID:'for';          SM:'for'),
    (SY:syDo;        ID:'do';           SM:'do'),
    (SY:syBreak;     ID:'break';        SM:'break loop'),
    (SY:syContinue;  ID:'continue';     SM:'continue loop'),
    (SY:sySwitch;    ID:'switch';       SM:'switch'),
    (SY:syCase;      ID:'case';         SM:'case'),
    (SY:syTry;       ID:'try';          SM:'try'),
    (SY:syExcept;    ID:'except';       SM:'catch execption'),
    (SY:syFinally;   ID:'finally';      SM:'finally'),
    (SY:syIn;        ID:'in';           SM:'in'),
    (SY:syIs;        ID:'is';           SM:'type checking'),
    (SY:syAs;        ID:'as';           SM:'type casting'),
    (sy:syLike;      ID:'like';         SM:'string like patten'),
    (SY:syAnd;       ID:'and';          SM:'logical and'),
    (SY:syOr;        ID:'or';           SM:'logical or'),
    (SY:syNot;       ID:'not';          SM:'logical not'),
    (sy:syFrom;      ID:'from';         SM:'from'),
    (sy:syTo;        ID:'to';           SM:'to'),
    (sy:syEnd;       ID:'end';          SM:'end ==> Last KEYWORD'),
    (SY:syBecome;    ID:'=';            SM:'become'),
    (SY:syAdd;       ID:'+';            SM:'add'),
    (SY:syDec;       ID:'-';            SM:'dec'),
    (SY:syMul;       ID:'*';            SM:'mul'),
    (SY:syDiv;       ID:'/';            SM:'div'),
    (SY:syMod;       ID:'%';            SM:'mod'),
    (SY:syBNot;      ID:'~';            SM:'bit xor -1'),
    (SY:syBXor;      ID:'^';            SM:'bit xor'),
    (SY:syBAnd;      ID:'&';            SM:'bit and'),
    (SY:syBor;       ID:'|';            SM:'bit or'),
    (SY:syBShr;      ID:'>>';           SM:'bit shift right'),
    (SY:syBShl;      ID:'<<';           SM:'bit shift left'),
    (sy:syFill;      ID:'<<<';          SM:'add all'),
    (SY:syLParen;    ID:'(';            SM:'left paren'),
    (SY:syRParen;    ID:')';            SM:'right paren'),
    (SY:syLBlock;    ID:'{';            SM:'left block'),
    (SY:syRBlock;    ID:'}';            SM:'right block'),
    (SY:syLArray;    ID:'[';            SM:'left array'),
    (SY:syRArray;    ID:']';            SM:'right array'),
    (SY:syDot;       ID:'.';            SM:'dot'),
    (SY:syAsk;       ID:'?';            SM:'ask'),
    (SY:syDot2;      ID:':';            SM:'dot 2'),
    (SY:syComma;     ID:',';            SM:'comma'),
    (SY:sySemicolon; ID:';';            SM:'semicolon'),
    (SY:syEQ;        ID:'==';           SM:'equal'),
    (SY:syRefer;     ID:'=>';           SM:'refer to'),
    (SY:syNE;        ID:'!=';           SM:'not equal'),
    (SY:syLess;      ID:'<';            SM:'less'),
    (SY:syLE;        ID:'<=';           SM:'less equal'),
    (SY:syMore;      ID:'>';            SM:'great'),
    (SY:syME;        ID:'>=';           SM:'great equal'),
    (SY:syFormat;    ID:'@';            SM:'format string'),
    (SY:syID;        ID:'<ID>';         SM:'identity name'),
    (SY:syFloat;     ID:'<FLOAT>';      SM:'push float'),
    (SY:syInt;       ID:'<INT>';        SM:'push int'),
    (SY:syStr;       ID:'<STR>';        SM:'push string'),
    (SY:syNil;       ID:'<NIL>';        SM:'push nil'),
    (SY:syNeg;       ID:'<NEG>';        SM:'neg'),
    (SY:syJmpT;      ID:'<JMPT>';       SM:'jump true'),
    (SY:syJmpF;      ID:'<JMPF>';       SM:'jump false'),
    (SY:syJmpTP;     ID:'<JMPTP>';      SM:'jump true pop last'),
    (SY:syJmpFP;     ID:'<JMPFP>';      SM:'jump flase pop last'),
    (SY:syJump;      ID:'<JUMP>';       SM:'jump'),
    (SY:syIdle;      ID:'<IDLE>';       SM:'idle'),
    (SY:syGetEnv;    ID:'<GETENV>';     SM:'push enviromnent value'),
    (SY:syLabel;     ID:'<LABEL>';      SM:'label'),
    (SY:syGoto;      ID:'<GOTO>';       SM:'goto label'),
    (SY:syGoTP;      ID:'<GOTOTP>';     SM:'goto when true and pop'),
    (SY:syGoFP;      ID:'<GOTOFP>';     SM:'goto when false and pop'),
    (SY:syVarGen;    ID:'<VARGEN>';     SM:'cast to vargen object'),
    (sy:syRINR;      ID:'<RINR>';       SM:'run in range'),
    (sy:syGETV;      ID:'<GETV>';       SM:'get temp value'),
    (sy:sySETV;      ID:'<SETV>';       SM:'set temp value'),
    (sy:syGetIV;     ID:'<GETIV>';      SM:'get item value'),
    (sy:sySetIV;     ID:'<SETIV>';      SM:'set item value'),
    (sy:syDupLast;   ID:'<DUPLAST>';    SM:'duplicate last value'),
    (sy:syEOF;       ID:'<EOF>';        SM:'end of file'),
    (SY:syEcho;      ID:'<ECHO>';       SM:'echo'),
    (SY:syGetSV;     ID:'<GETSV>';      SM:'get super value'),
    (SY:sySetSV;     ID:'<SETSV>';      SM:'set super value'),
    (SY:sySend;      ID:'<SEND>';       SM:'send next value'),
    (SY:syVarList;   ID:'<VARLIST>';    SM:'create varlist'),
    (SY:syHashed;    ID:'<HASHED>';     SM:'create hashed'),
    (SY:syReferAsk;  ID:'<REFERASK>';   SM:'refer ask'),
    (SY:sySTMT;      ID:'<STMT>';       SM:'end statement')
  );

  FirstKeyword = syBegin;
  LastKeyword = syEnd;
  FirstOper = Succ(syEnd);
  LastOper = syFormat;
  
  ConstantSyms = [syFloat, syInt, syStr, syNil, syGetEnv, syGetSV];

  ExprOperSyms: array[0..4] of KLiSymbols = (
   {[syNeg, syNot, syBNot, syFormat]}                                   // 1
    [syMul, syDiv, syMod],                                              // 2
    [syAdd, syDec],                                                     // 3
    [syBXor, syBAnd, syBOr, syBShl, syBShr, syFill],                    // 4
    [syEQ, syNE, syLess, syLE, syMore, syME, syIn, syLike, syAs, syIs], // 6
    [syAnd, syOr]                                                       // 7
  );

  OperIDSyms = [syMul, syDiv, syMod, syAdd, syDec, syBXor, syBAnd,
    syBOr, syBShl, syBShr, syFill, syEQ, syNE, syLess, syLE, syMore, syME,
    syLike, syAs, syIs, syAnd, syOr];

  ExprHeadSyms = ConstantSyms + OperIDSyms + [syID, syNot, syDec, syBNot,
    syLParen, syLArray, syLBlock, syBOr, syFormat, syLambda];

  ExprEndSyms = [syIf, syElse, syExcept, syFinally, syBecome, syRParen,
    syRBlock, syRArray, syDot, syAsk, syDot2, syComma, 
    syIn, syThen, syEnd, syUntil, syEOF];

  GotoSyms = [syTry, syGoto, syGoTP, syGoFP, syRINR];

  sys_func_count = 178;
  sys_func_array: array[0..sys_func_count - 1] of RLseFunc = (
    (fr_prot:'dir:string ||';
     fr_addr:@call_dir;
     fr_desc:'get current directory';
    ),
    (fr_prot:'isdir:int |directory:string|';
     fr_addr:@call_isdir;
     fr_desc:'test if directory exists';
    ),
    (fr_prot:'isfile:int |fileName:string|';
     fr_addr:@call_isfile;
     fr_desc:'test if file exists';
    ),
    (fr_prot:'modules:varlist ||';
     fr_addr:@call_modules;
     fr_desc:'return imported module list';
    ),
    (fr_prot:'libs:varlist ||';
     fr_addr:@call_libs;
     fr_desc:'return loaded libraries';
    ),
    (fr_prot:'print:void |text:string|';
     fr_addr:@call_print;
     fr_desc:'print text into standard output';
    ),
    (fr_prot:'println:void |text:string|';
     fr_addr:@call_println;
     fr_desc:'print text and a line break into standard output';
    ),
    (fr_prot:'printf:void |fileName:string|';
     fr_addr:@call_printf;
     fr_desc:'print file content into standard output';
    ),
    (fr_prot:'readln:string ||';
     fr_addr:@call_readln;
     fr_desc:'read a line from standard input';
    ),
    (fr_prot:'exit:void |status:int|';
     fr_addr:@call_exit;
     fr_desc:'exit with status code';
    ),
    (fr_prot:'random:int |low:int, high:int|';
     fr_addr:@call_random;
     fr_desc:'generate random number';
    ),
    (fr_prot:'sleep:void |milliSeconds:int|';
     fr_addr:@call_sleep;
     fr_desc:'sleep a number of milliseconds';
    ),
    (fr_prot:'getenv:string |name:string|';
     fr_addr:@call_getenv;
     fr_desc:'get environment value';
    ),
    (fr_prot:'dumpc:string |any|';
     fr_addr:@call_dumpc;
     fr_desc:'dump object p-codes to standard output';
    ),
    (fr_prot:'length:int |any|';
     fr_addr:@call_length;
     fr_desc:'get string length or item count';
    ),
    (fr_prot:'genid:string ||';
     fr_addr:@call_genid;
     fr_desc:'generate a global unique string';
    ),
    (fr_prot:'load:module |fileName:string|';
     fr_addr:@call_load;
     fr_desc:'load module';
    ),
    (fr_prot:'parse:function |script:string|';
     fr_addr:@call_parse;
     fr_desc:'parse and compile script';
    ),
    (fr_prot:'eval |script:string|';
     fr_addr:@call_eval;
     fr_desc:'evaluate a block of lysee script';
    ),
    (fr_prot:'format:string |fmt:string, args:varlist|';
     fr_addr:@call_format;
     fr_desc:'format string';
    ),
    (fr_prot:'max |v1, v2|';
     fr_addr:@call_max;
     fr_desc:'get max value';
    ),
    (fr_prot:'min |v1, v2|';
     fr_addr:@call_min;
     fr_desc:'get min value';
    ),
    (fr_prot:'leap:int |year:int|';
     fr_addr:@call_leap;
     fr_desc:'test leap year';
    ),
    (fr_prot:'which |name:string|';
     fr_addr:@call_which;
     fr_desc:'find function, module or anything else by name';
    ),
    (fr_prot:'curry:function |func:function, paramList:varlist|';
     fr_addr:@call_curry;
     fr_desc:'curry function with parametre list';
    ),
    (fr_prot:'curryOne:function |func:function, value|';
     fr_addr:@call_curryone;
     fr_desc:'curry function with one parametre';
    ),
    (fr_prot:'gc:int ||';
     fr_addr:@call_gc;
     fr_desc:'execute garbage collection immediately';
    ),
    (fr_prot:'apply |func:function, params:varlist|';
     fr_addr:@call_apply;
     fr_desc:'call function with supplied parametres';
    ),
    (fr_prot:'tempFileName:string |fileExt:string|';
     fr_addr:@call_tmpfname;
     fr_desc:'generate temp file name at temp path';
    ),
    (fr_prot:'encodeUTF8:string |ANSI:string|';
     fr_addr:@call_encodeUTF8;
     fr_desc:'encode ANSI string to UTF8 format';
    ),
    (fr_prot:'decodeUTF8:string |UTF8:string|';
     fr_addr:@call_decodeUTF8;
     fr_desc:'decode UTF8 string to ANSI format';
    ),
    (fr_prot:'openfs:stream |fileName:string, mode:string|';
     fr_addr:@call_openfs;
     fr_desc:'open file stream by specified mode: CRWE (default R)';
    ),
    (fr_prot:'memory:stream |size:int|';
     fr_addr:@call_memory;
     fr_desc:'create memory stream';
    ),
    (fr_prot:'incPD:string |dir:string|';
     fr_addr:@call_incPD;
     fr_desc:'include trailing path delimiter';
    ),
    (fr_prot:'excPD:string |path:string|';
     fr_addr:@call_excPD;
     fr_desc:'exclude trailing path delimiter';
    ),
    (fr_prot:'veryPD:string |path:string|';
     fr_addr:@call_veryPD;
     fr_desc:'correct path delimiter';
    ),
    (fr_prot:'veryUD:string |RUL:string|';
     fr_addr:@call_veryUD;
     fr_desc:'correct URL delimiter';
    ),
    (fr_prot:'msecs:int |func:function, params:varlist|';
     fr_addr:@call_msecs;
     fr_desc:'count milliseconds used to call a function';
    ),
    (fr_prot:'__module__:module ||';
     fr_addr:@call_current_module;
     fr_desc:'get current module';
    ),
    (fr_prot:'__func__:function ||';
     fr_addr:@call_current_func;
     fr_desc:'get current function';
    ),
    (fr_prot:'__error__:error ||';
     fr_addr:@call_current_error;
     fr_desc:'get current error';
    ),
    (fr_prot:'__args__:varlist ||';
     fr_addr:@call_current_args;
     fr_desc:'get argument list';
    ),
    (fr_prot:'__line__:int ||';
     fr_addr:@call_current_line;
     fr_desc:'get current line number';
    ),
    (fr_prot:'__envs__:varlist ||';
     fr_addr:@call_current_envs;
     fr_desc:'get environment value list';
    ),
    (fr_prot:'__file__:string ||';
     fr_addr:@call_current_file;
     fr_desc:'get current file';
    ),
    (fr_prot:'__pd__:string ||';
     fr_addr:@call_current_pd;
     fr_desc:'get system path delimiter';
    ),
    (fr_prot:'eol:string ||';
     fr_addr:@call_eol;
     fr_desc:'get eol(line break)';
    ),
    (fr_prot:'each:void |any:vargen, proc:function|';
     fr_addr:@call_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'map:varlist |any:vargen, proc:function|';
     fr_addr:@call_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'reduce |any:vargen, initValue, proc:function|';
     fr_addr:@call_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'filter:varlist |any:vargen, proc:function|';
     fr_addr:@call_filter;
     fr_desc:'filter item: |item| ... end';
    ),
    (fr_prot:'sum |any:vargen, proc:function, initValue|';
     fr_addr:@call_sum;
     fr_desc:'sum all: |result, item| ... end';
    ),
    (fr_prot:'__maxint__:int ||';
     fr_addr:@call_maxint;
     fr_desc:'get max integer value';
    ),
    (fr_prot:'__minint__:int ||';
     fr_addr:@call_minint;
     fr_desc:'get min integer value';
    ),
    (fr_prot:'abs |value|';
     fr_addr:@call_abs;
     fr_desc:'get absolute value';
    ),
    (fr_prot:'find:varlist |S:string, patten:string, findAll|';
     fr_addr:@call_find;
     fr_desc:'find patten matches';
    ),
    (fr_prot:'gsub:string |S:string, patten:string, newStr:string, count:int|';
     fr_addr:@call_gsub;
     fr_desc:'replace patten with new string';
    ),
    (fr_prot:'split:varlist |S:string, patten:string|';
     fr_addr:@call_split;
     fr_desc:'split string to varlist';
    ),
    (fr_prot:'getcs:varlist |index:int|';
     fr_addr:@call_getcs;
     fr_desc:'get call stack item';
    ),
    (fr_prot:'throw:void |exceptionMsg:string, exceptionID:string|';
     fr_addr:@call_throw;
     fr_desc:'throw exception';
    ),
    (fr_prot:'hex:string |value:int, size:int|';
     fr_addr:@call_hex;
     fr_desc:'convert to hex string';
    ),
    (fr_prot:'bitlist:string |value:int, size:int|';
     fr_addr:@call_bitlist;
     fr_desc:'convert to bit list string';
    ),
    (fr_prot:'upto:vargen |from:int, to:int, step:int|';
     fr_addr:@call_upto;
     fr_desc:'create a upto range';
    ),
    (fr_prot:'downto:vargen |from:int, to:int, step:int|';
     fr_addr:@call_downto;
     fr_desc:'create a downto range';
    ),
    (fr_prot:'typeof:type |any|';
     fr_addr:@call_typeof;
     fr_desc:'get value type';
    ),
  { error }
    (fr_prot:'error_get_text:string |e:error|';
     fr_addr:@call_error_text;
     fr_desc:'error text';
    ),
    (fr_prot:'error_get_module:string |e:error|';
     fr_addr:@call_error_module;
     fr_desc:'error module';
    ),
    (fr_prot:'error_get_name:string |e:error|';
     fr_addr:@call_error_name;
     fr_desc:'error name';
    ),
    (fr_prot:'error_get_message:string |e:error|';
     fr_addr:@call_error_message;
     fr_desc:'error message';
    ),
    (fr_prot:'error_get_row:int |e:error|';
     fr_addr:@call_error_row;
     fr_desc:'error row';
    ),
    (fr_prot:'error_get_col:int |e:error|';
     fr_addr:@call_error_col;
     fr_desc:'error column';
    ),
    (fr_prot:'error_get_errno:int |e:error|';
     fr_addr:@call_error_errno;
     fr_desc:'error code';
    ),

    { function }

    (fr_prot:'function_get_name:string |f:function|';
     fr_addr:@call_func_name;
     fr_desc:'function name';
    ),
    (fr_prot:'function_get_description:string |f:function|';
     fr_addr:@call_func_desc;
     fr_desc:'function description';
    ),
    (fr_prot:'function_get_type:string |f:function|';
     fr_addr:@call_func_type;
     fr_desc:'get function result type';
    ),
    (fr_prot:'function_get_prototype:string |f:function|';
     fr_addr:@call_func_prototype;
     fr_desc:'function prototype';
    ),
    (fr_prot:'function_get_module:module |f:function|';
     fr_addr:@call_func_module;
     fr_desc:'function module';
    ),
  { hashed }
    (fr_prot:'hashed_create:hashed |buckets:int|';
     fr_addr:@call_hashed_create;
     fr_desc:'create a hashed key-value list';
    ),
    (fr_prot:'hashed_clear:void |h:hashed|';
     fr_addr:@call_hashed_clear;
     fr_desc:'clear list item';
    ),
    (fr_prot:'hashed_remove:void |h:hashed, key:string|';
     fr_addr:@call_hashed_remove;
     fr_desc:'remove specifed key value';
    ),
    (fr_prot:'hashed_isset:int |h:hashed, key:string|';
     fr_addr:@call_hashed_isset;
     fr_desc:'check if the key exists';
    ),
    (fr_prot:'hashed_read |h:hashed, key:string, defaultValue|';
     fr_addr:@call_hashed_read;
     fr_desc:'read key value';
    ),
    (fr_prot:'hashed_get_keys:varlist |h:hashed|';
     fr_addr:@call_hashed_keys;
     fr_desc:'get hashed key list';
    ),
    (fr_prot:'hashed_get_values:varlist |h:hashed|';
     fr_addr:@call_hashed_values;
     fr_desc:'get hashed value list';
    ),
  { module }
    (fr_prot:'module_get_name:string |m:module|';
     fr_addr:@call_module_name;
     fr_desc:'module name';
    ),
    (fr_prot:'module_get_description:string |m:module|';
     fr_addr:@call_module_desc;
     fr_desc:'module description';
    ),
    (fr_prot:'module_get_file:string |m:module|';
     fr_addr:@call_module_file;
     fr_desc:'module file';
    ),
    (fr_prot:'module_get_modules:varlist |m:module|';
     fr_addr:@call_module_modules;
     fr_desc:'imported module list';
    ),
    (fr_prot:'module_get_funcs:varlist |m:module|';
     fr_addr:@call_module_funcs;
     fr_desc:'global function list';
    ),
    (fr_prot:'module_get_classes:varlist |m:module|';
     fr_addr:@call_module_types;
     fr_desc:'get type list';
    ),
    (fr_prot:'module_get_version:string |m:module|';
     fr_addr:@call_module_version;
     fr_desc:'type version';
    ),
    (fr_prot:'module_imports:module |m:module, name:string|';
     fr_addr:@call_module_imports;
     fr_desc:'import module by name';
    ),
  { vargen }
    (fr_prot:'vargen_create:vargen |any|';
     fr_addr:@call_vargen_create;
     fr_desc:'create variant generator';
    ),
    (fr_prot:'vargen_get_eof:int |v:vargen|';
     fr_addr:@call_vargen_eof;
     fr_desc:'test if finished';
    ),
    (fr_prot:'vargen_next |v:vargen|';
     fr_addr:@call_vargen_next;
     fr_desc:'generate next value';
    ),
    (fr_prot:'vargen_rewind:int |v:vargen|';
     fr_addr:@call_vargen_rewind;
     fr_desc:'restart from first element';
    ),
    (fr_prot:'vargen_each:void |v:vargen, proc:function|';
     fr_addr:@call_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'vargen_map:varlist |v:vargen, proc:function|';
     fr_addr:@call_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'vargen_reduce |v:vargen, initValue, proc:function|';
     fr_addr:@call_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'vargen_filter:varlist |v:vargen, proc:function|';
     fr_addr:@call_filter;
     fr_desc:'filter item: |item| ... end';
    ),
  { stream }
    (fr_prot:'stream_close:void |s:stream|';
     fr_addr:@call_stream_close;
     fr_desc:'close stream';
    ),
    (fr_prot:'stream_resize:void |s:stream, length:int|';
     fr_addr:@call_stream_set_length;
     fr_desc:'set stream size';
    ),
    (fr_prot:'stream_get_eof:int |s:stream|';
     fr_addr:@call_stream_eof;
     fr_desc:'test if is at end of the stream';
    ),
    (fr_prot:'stream_position:string |s:stream|';
     fr_addr:@call_stream_get_position;
     fr_desc:'get current position';
    ),
    (fr_prot:'stream_seekTo:void |s:stream, newPosition:int|';
     fr_addr:@call_stream_set_position;
     fr_desc:'set current position';
    ),
    (fr_prot:'stream_read:string |s:stream, count:int|';
     fr_addr:@call_stream_read;
     fr_desc:'read value';
    ),
    (fr_prot:'stream_readln:string |s:stream|';
     fr_addr:@call_stream_readln;
     fr_desc:'read a line';
    ),
    (fr_prot:'stream_write:int |s:stream, text:string|';
     fr_addr:@call_stream_write;
     fr_desc:'write into stream';
    ),
    (fr_prot:'stream_writeln:int |s:stream, text:string|';
     fr_addr:@call_stream_writeln;
     fr_desc:'write text and a line break into stream';
    ),
    (fr_prot:'stream_writeTo:int |s:stream, stream:stream, count:int|';
     fr_addr:@call_stream_writeTo;
     fr_desc:'write part data into another stream';
    ),
    (fr_prot:'stream_flush:void |s:stream|';
     fr_addr:@call_stream_flush;
     fr_desc:'flush stream';
    ),
    (fr_prot:'stream_get_lines:vargen |s:stream|';
     fr_addr:@call_stream_lines;
     fr_desc:'wrap stram as a line generator';
    ),
  { string }
    (fr_prot:'string_setAt:string |s:string, index:int, value:string|';
     fr_addr:@call_string_setAt;
     fr_desc:'set char by index';
    ),
    (fr_prot:'string_get_name:string |s:string|';
     fr_addr:@call_string_name;
     fr_desc:'extract name';
    ),
    (fr_prot:'string_get_value:string |s:string|';
     fr_addr:@call_string_value;
     fr_desc:'extract value';
    ),
    (fr_prot:'string_compare:int |s:string, S2:string, ignoreCase|';
     fr_addr:@call_string_compare;
     fr_desc:'compare two string';
     ),
    (fr_prot:'string_replace:string |s:string, patten:string, newStr:string, ignoreCase, replaceFirstOnly|';
     fr_addr:@call_string_replace;
     fr_desc:'replace patten to new string';
    ),
    (fr_prot:'string_pos:int |s:string, SubStr:string, IgnoreCase|';
     fr_addr:@call_string_pos;
     fr_desc:'get first sub-string position';
    ),
    (fr_prot:'string_lastPos:int |s:string, SubStr:string, IgnoreCase|';
     fr_addr:@call_string_lastPos;
     fr_desc:'get last sub-string position';
    ),
    (fr_prot:'string_left:string |s:string, count:int|';
     fr_addr:@call_string_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'string_right:string |s:string, count:int|';
     fr_addr:@call_string_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'string_trim:string |s:string|';
     fr_addr:@call_string_trim;
     fr_desc:'trim left and right';
    ),
    (fr_prot:'string_trimLeft:string |s:string|';
     fr_addr:@call_string_trimLeft;
     fr_desc:'trim left';
    ),
    (fr_prot:'string_trimRight:string |s:string|';
     fr_addr:@call_string_trimRight;
     fr_desc:'trim right';
    ),
    (fr_prot:'string_trimAll:string |s:string|';
     fr_addr:@call_string_trimAll;
     fr_desc:'trim all spaces';
    ),
    (fr_prot:'string_copy:string |s:string, index:int, count:int|';
     fr_addr:@call_string_copy;
     fr_desc:'copy sub-string';
    ),
    (fr_prot:'string_delete:string |s:string, index:int, count:int|';
     fr_addr:@call_string_delete;
     fr_desc:'delete by range';
    ),
    (fr_prot:'string_insert:string |s:string, substr:string, index:int|';
     fr_addr:@call_string_insert;
     fr_desc:'insert sub-string';
    ),
    (fr_prot:'string_extractName:string |s:string, separator:string|';
     fr_addr:@call_string_extractName;
     fr_desc:'extract name with specified separator';
    ),
    (fr_prot:'string_extractValue:string |s:string, separator:string|';
     fr_addr:@call_string_extractValue;
     fr_desc:'extract value with specified separator';
    ),
    (fr_prot:'string_lformat:string |s:string, width:int, filler:int|';
     fr_addr:@call_string_lformat;
     fr_desc:'format to left';
    ),
    (fr_prot:'string_rformat:string |s:string, width:int, filler:int|';
     fr_addr:@call_string_rformat;
     fr_desc:'format to right';
    ),
    (fr_prot:'string_center:string |s:string, width:int, filler:int|';
     fr_addr:@call_string_center;
     fr_desc:'format to center';
    ),
    (fr_prot:'string_html:string |s:string, translateMBC|';
     fr_addr:@call_string_html;
     fr_desc:'encode to HTML code';
    ),
    (fr_prot:'string_random:string |s:string|';
     fr_addr:@call_string_randomOrder;
     fr_desc:'randomize string charactors';
    ),
    (fr_prot:'string_lower:string |s:string|';
     fr_addr:@call_string_lower;
     fr_desc:'convert to lower case string';
    ),
    (fr_prot:'string_upper:string |s:string|';
     fr_addr:@call_string_upper;
     fr_desc:'convert to upper case string';
    ),
    (fr_prot:'string_isAlpha:int |s:string|';
     fr_addr:@call_string_isAlpha;
     fr_desc:'test if the string contains only alpha charactors';
    ),
    (fr_prot:'string_isAlnum:int |s:string|';
     fr_addr:@call_string_isAlnum;
     fr_desc:'test if the string contains only alpha and digit charactors';
    ),
    (fr_prot:'string_isCntrl:int |s:string|';
     fr_addr:@call_string_isCntrl;
     fr_desc:'test if the string contains only control charactors';
    ),
    (fr_prot:'string_isSpace:int |s:string|';
     fr_addr:@call_string_isSpace;
     fr_desc:'test if the string contains only space charactors';
    ),
    (fr_prot:'string_isDigit:int |s:string|';
     fr_addr:@call_string_isDigit;
     fr_desc:'test if the string contains only digit charactors';
    ),
    (fr_prot:'string_isHex:int |s:string|';
     fr_addr:@call_string_isHex;
     fr_desc:'test if the string contains only HEX charactors';
    ),
    (fr_prot:'string_fileText:string |s:string|';
     fr_addr:@call_string_fileText;
     fr_desc:'get file text';
    ),
    (fr_prot:'string_saveToFile:void |s:string, fileName:string|';
     fr_addr:@call_string_saveToFile;
     fr_desc:'save string to file';
    ),
    (fr_prot:'string_fullFileName:string |s:string|';
     fr_addr:@call_string_fullFileName;
     fr_desc:'expand to full file name';
    ),
    (fr_prot:'string_filePath:string |s:string|';
     fr_addr:@call_string_filePath;
     fr_desc:'extract file path';
    ),
    (fr_prot:'string_fileName:string |s:string|';
     fr_addr:@call_string_fileName;
     fr_desc:'extract file name';
    ),
    (fr_prot:'string_fileExt:string |s:string|';
     fr_addr:@call_string_fileExt;
     fr_desc:'extract file extension';
    ),
    (fr_prot:'string_changeExt:string |s:string, newFileExt:string|';
     fr_addr:@call_string_changeExt;
     fr_desc:'change file extension';
    ),
    (fr_prot:'string_hexToInt:int |s:string, defaultValue:int|';
     fr_addr:@call_string_hexToInt;
     fr_desc:'convert HEX string to integer value';
    ),
    (fr_prot:'string_reverse:string |s:string|';
     fr_addr:@call_string_reverse;
     fr_desc:'reverse string charactors';
    ),
    (fr_prot:'string_isLower:int |s:string|';
     fr_addr:@call_string_isLower;
     fr_desc:'test if is lower case string';
    ),
    (fr_prot:'string_isUpper:int |s:string|';
     fr_addr:@call_string_isUpper;
     fr_desc:'test if is upper case string';
    ),
    (fr_prot:'string_translate:string |s:string, OrdCharList:string, newCharList:string|';
     fr_addr:@call_string_translate;
     fr_desc:'translate original char to new char';
    ),
    (fr_prot:'string_md5sum:string |s:string, isFile: int|';
     fr_addr:@call_string_md5sum;
     fr_desc:'calculate MD5 sum';
    ),
  { type }
    (fr_prot:'type_get_name:string |t:type|';
     fr_addr:@call_type_name;
     fr_desc:'type name';
    ),
    (fr_prot:'type_get_module:module |t:type|';
     fr_addr:@call_type_module;
     fr_desc:'owner module';
    ),
    (fr_prot:'type_get_description:string |t:type|';
     fr_addr:@call_type_description;
     fr_desc:'type description';
    ),
  { varlist }
    (fr_prot:'varlist_create:varlist |count:int|';
     fr_addr:@call_varlist_create;
     fr_desc:'create variant list';
    ),
    (fr_prot:'varlist_get_length:int |l:varlist|';
     fr_addr:@call_varlist_get_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_set_length:void |l:varlist, count:int|';
     fr_addr:@call_varlist_set_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_exchange:void |l:varlist, X1:int, X2:int|';
     fr_addr:@call_varlist_exchange;
     fr_desc:'exchange by index';
    ),
    (fr_prot:'varlist_move:void |l:varlist, curIndex:int, newIndex:int|';
     fr_addr:@call_varlist_move;
     fr_desc:'move variant to new position';
    ),
    (fr_prot:'varlist_add:int |l:varlist, value|';
     fr_addr:@call_varlist_add;
     fr_desc:'add variant';
    ),
    (fr_prot:'varlist_addFrom:int |l:varlist, variants:varlist|';
     fr_addr:@call_varlist_addFrom;
     fr_desc:'add variants from';
    ),
    (fr_prot:'varlist_fill:void |l:varlist, source:vargen, clearBeforeFill|';
     fr_addr:@call_varlist_fill;
     fr_desc:'fill generated value';
    ),
    (fr_prot:'varlist_insert:void |l:varlist, index:int, value|';
     fr_addr:@call_varlist_insert;
     fr_desc:'insert variant at specified position';
    ),
    (fr_prot:'varlist_delete:void |l:varlist, index:int|';
     fr_addr:@call_varlist_delete;
     fr_desc:'delete variant by index';
    ),
    (fr_prot:'varlist_clear:void |l:varlist|';
     fr_addr:@call_varlist_clear;
     fr_desc:'clear variant list';
    ),
    (fr_prot:'varlist_copy:varlist |l:varlist, index:int, count:int|';
     fr_addr:@call_varlist_copy;
     fr_desc:'copy to another variant list';
    ),
    (fr_prot:'varlist_left:varlist |l:varlist, count:int|';
     fr_addr:@call_varlist_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'varlist_right:varlist |l:varlist, count:int|';
     fr_addr:@call_varlist_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'varlist_filter:varlist |l:varlist, filterFunc:function|';
     fr_addr:@call_varlist_filter;
     fr_desc:'filter variant list';
    ),
    (fr_prot:'varlist_get_first |l:varlist|';
     fr_addr:@call_varlist_first;
     fr_desc:'get first variant item';
    ),
    (fr_prot:'varlist_get_last |l:varlist|';
     fr_addr:@call_varlist_last;
     fr_desc:'get last variant item';
    ),
    (fr_prot:'varlist_shift |l:varlist|';
     fr_addr:@call_varlist_shift;
     fr_desc:'get and delete first item';
    ),
    (fr_prot:'varlist_pop |l:varlist|';
     fr_addr:@call_varlist_pop;
     fr_desc:'get and delete last item';
    )
  );

  sys_module_funcs: RLseFuncListRec = (
    fl_count: sys_func_count;
    fl_entry:@sys_func_array;
  );
  
procedure kernel_lock;
begin
  sys_spinlock.Enter;
end;

procedure kernel_unlock;
begin
  sys_spinlock.Leave;
end;

procedure engine_lock(engine: KLiEngine);
begin

end;

procedure engine_unlock(engine: KLiEngine);
begin

end;

procedure kernel_load_config_file(const ConfigFile: string);

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
          if parse_config(list[index], ID, value) then
            sys_configures.Values[ID] := kernel_expand_value(value, nil);
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
        Result := kernel_expand_value(Copy(sys_configures[index], Length(Key) + 2, MaxInt), nil);
        Exit;
      end;
    end;
    Result := kernel_expand_value(Value, nil);
  end;

begin
  try
    if sys_process_ID = '' then
    begin
      sys_process_ID := genid;
      sys_program := program_file;
      sys_kernel := library_file;
      {$IFDEF WINDOWS}
      sys_home_path := full_path(lse_getenv('HOMEDRIVER') + lse_getenv('HOMEPATH'));
      {$ELSE}
      sys_home_path := full_path(lse_getenv('HOME'));
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

    sys_tmpath := full_path(ReadCex('lse_tmpath', LSE_TEMP_PATH));
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

function kernel_read_config(const ID: string): string;
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
  if ID = 'keywords' then Result := reserved_words else
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

function kernel_expand_value(const S: string; E: KLiEngine): string;
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
            Result := Result + kernel_read_config(name);
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

function is_reserved(const ID: string; IncludeKeywords: boolean): boolean;
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
    (ID_to_sym(ID, syError) <> syError);
end;

function delete_comment(const S: string): string;
var
  index: integer;
begin
  index := Pos('#', S);
  if index > 0 then
    Result := Trim(Copy(S, 1, index - 1)) else
    Result := Trim(S);
end;

function parse_config(const S: string; var ID, value: string): boolean;
begin
  ID := Trim(extract_name_value(delete_comment(S), value));
  Result := is_ID(pchar(ID));
  if Result then
    value := Trim(value);
end;

function extract_name_value(const S: string; var V: string; const separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 0 then
  begin
    V := Copy(S, X + Length(separator), MaxInt);
    Result := Copy(S, 1, X - 1);
  end
  else
  begin
    V := '';
    Result := '';
  end;
end;

function extract_name(const S, separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 1 then
    Result := Copy(S, 1, X - 1) else
    Result := '';
end;

function extract_value(const S, separator: string): string;
var
  X: Integer;
begin
  X := AnsiPos(separator, S);
  if X > 0 then
    Result := Copy(S, X + Length(separator), MaxInt) else
    Result := '';
end;

function genid: string;
var
  guid: TGuid;
  index: integer;
begin
  CreateGuid(guid);
  Result := UpperCase(GuidToString(guid));
  for index := Length(Result) downto 1 do
    if not (Result[index] in HexChar) then
      System.Delete(Result, index, 1);
end;

function program_file: string;
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

function library_file: string;
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
  Result := program_file;
end;

function full_path(const path: string): string;
begin
  Result := lse_expand_fname(path);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function parse_int(S: pchar): int64;
var
  ei: KLiExtInt;
begin
  Result := 0;
  parse_ext_int(S, Result, ei);
end;

function parse_ext(S: pchar): extended;
var
  iv: int64;
  ei: KLiExtInt;
begin
  Result := parse_ext_int(S, iv, ei);
end;

function parse_ext_int(var S: pchar; var iv: int64; var ei: KLiExtInt): extended;
label
  LEAVE;
var
  signf, divf: boolean;
  times: int64;
begin
  Result := 0;
  iv := 0;
  ei := eiNone;

  // 1. skip heading spaces

  if S = nil then Exit;
  S := skip_ch(S, SpaceChar);

  // 2. read value flag

  signf := (S^ = '-');
  if signf then Inc(S);
  if not (S^ in DigitChar) then goto LEAVE;

  // 3. read hex

  ei := eiInt;

  if S^ = '0' then
  begin
    Inc(S);
    if S^ in ['x', 'X'] then
    begin
      Inc(S);
      if S^ in HexChar then
      begin
        repeat
          iv := (iv * 16) + hex_value(S^);
          Inc(S);
        until not (S^ in HexChar);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;
    
    if S^ in ['o', 'O'] then
    begin
      Inc(S);
      if S^ in ['0'..'7'] then
      begin
        repeat
          iv := (iv * 8) + Ord(S^) - Ord('0');
          Inc(S);
        until not (S^ in ['0'..'7']);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;
    
    if S^ in ['b', 'B'] then
    begin
      Inc(S);
      if S^ in ['0', '1'] then
      begin
        repeat
          iv := (iv * 2) + Ord(S^) - Ord('0');
          Inc(S);
        until not (S^ in ['0', '1']);
        Result := iv;
      end
      else ei := eiNone;
      goto LEAVE;
    end;

    while S^ = '0' do Inc(S);
  end;

  // 3. read integer part

  while S^ in DigitChar do
  begin
    iv := (iv * 10) + Ord(S^) - Ord('0');
    Inc(S);
  end;
  Result := iv;

  // 4. read frac part

  if S^ = '.' then
  begin
    Inc(S);
    if S^ in DigitChar then
    begin
      ei := eiExt;
      times := 1;
      while (S^ in DigitChar) and (times < 1000000000000000000) do
      begin
        times := times * 10;
        Result := Result + ((Ord(S^) - Ord('0')) / times);
        Inc(S);
      end;
      while S^ in DigitChar do Inc(S);
    end
    else
    begin
      Dec(S);
      goto LEAVE;
    end;
  end;

  // 5. check science fomrat

  if S^ in ['e', 'E'] then
  begin
    ei := eiExt;
    Inc(S);
    divf := (S^ = '-');
    if divf then Inc(S);
    if S^ in DigitChar then
    begin
      times := 0;
      repeat
        times := (times * 10) + Ord(S^) - Ord('0');
        Inc(S);
      until not (S^ in DigitChar);
      while times > 0 do
      begin
        if divf then
          Result := Result / 10 else
          Result := Result * 10;
        Dec(times);
      end;
      iv := Round(int(Result));
    end
    else ei := eiNone;
  end;

  LEAVE:

  if ei = eiNone then
  begin
    Inc(S);
    iv := 0;
    Result := 0;
  end
  else
  if signf then
  begin
    iv := - iv;
    Result := - Result;
  end;
end;

function parse_str(var S: pchar; desti: TStream; allow_esc_char: boolean): boolean;
var
  times: integer;
  quotc, ch: char;

  function str_end: boolean;
  var
    count: integer;
    next: pchar;
  begin
    Result := (S^ = quotc);
    if Result and (times > 2) then
    begin
      count := times;
      next := S;
      repeat
        Dec(count);
        Inc(next);
      until (count = 0) or (next^ <> quotc);
      Result := (count = 0) and (next^ <> quotc);
    end;
  end;
  
begin
  quotc := #0;
  times := 0;
  if S <> nil then
  begin
    S := skip_ch(S, SpaceChar);
    if S^ in ['"', ''''] then
    begin
      quotc := S^;
      while (S^ = quotc) and (times < 3) do
      begin
        Inc(S);
        Inc(times);
      end;
    end;
  end;
  
  Result := (times = 2);
  if Result or not (times in [1, 3]) then Exit;
  if (times = 3) or (quotc = '''') then
    allow_esc_char := false;
    
  while not str_end do
  begin
    if S^ = #0 then Exit; {<--not terminated}
    ch := S^;
    if (ch = '\') and allow_esc_char then
    begin
      Inc(S);
      if S^ = #0 then Exit; {<--not terminated}
      case S^ of
        '0': ch := #0;   {<--NULL}
        'a': ch := #7;   {<--BELL}
        'b': ch := #8;   {<--BACKSPACE}
        'e': ch := #27;  {<--ESCAPE}
        'f': ch := #12;  {<--FORMFEED}
        'n': ch := #10;  {<--NEWLINE}
        'r': ch := #13;  {<--CARRIGE RETURN}
        't': ch := #9;   {<--TAB}
        'v': ch := #11;  {<--VERTICAL TAB}
        'x': begin      {<--HEX}
               Inc(S);
               if not (S^ in HexChar) then
               begin
                 Inc(S);
                 Exit;
               end;
               Inc(S);
               if not (S^ in HexChar) then
               begin
                 Inc(S);
                 Exit;
               end;
               ch := char(StrToInt('$' + (S - 1)^ + S^));
             end;
        else if S^ in ['\', '''', '"'] then
               ch := S^ else
               Exit;
      end;
    end;
    desti.WriteBuffer(ch, sizeof(ch));
    Inc(S);
  end;

  Inc(S, times);
  Result := true;
end;

function skip_ch(const S: pchar; CharSet: KLiCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
    while Result^ in CharSet do
      Inc(Result);
end;

function hex_value(ch: char): integer;
begin
  case ch of
    '0'..'9': Result := Ord(ch) - Ord('0');
    'A'..'F': Result := Ord(ch) - Ord('A') + 10;
    'a'..'f': Result := Ord(ch) - Ord('a') + 10;
         else Result := -1;
  end;
end;

function is_ID(S: pchar; Head: KLiCharSet): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ in Head) then
  begin
    Inc(S);
    Result := (S^ = #0) or in_charset(S, IDChar);
  end;
end;

function is_ID_head(C: char): boolean;
begin
  Result := (C in IDHeadChar);
end;

function in_charset(S: pchar; Len: integer; Chars: KLiCharSet): boolean;
begin
  Result := (Len > 0) and (S <> nil) and (S^ in Chars);
  while Result and (Len > 1) do
  begin
    Dec(Len);
    Inc(S);
    Result := S^ in Chars;
  end;
end;

function in_charset(S: pchar; Chars: KLiCharSet): boolean;
begin
  Result := false;
  if (S <> nil) and (S^ <> #0) and (S^ in Chars) then
  begin
    Chars := Chars - [#0];
    repeat Inc(S) until not (S^ in Chars);
    Result := (S^ = #0);
  end;
end;

function buf_comp(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): integer;
var
  len: integer;
begin
  Result := 0;
  if B1 <> B2 then
  begin
    if (B1 = nil) or (L1 < 1) then
    begin
      if (B2 <> nil) and (L2 > 0) then Result := -1;
      Exit;
    end;

    if (B2 = nil) or (L2 < 1) then
    begin
      Result := 1;
      Exit;
    end;

    for len := min(L1, L2) downto 1 do
    begin
      if IgnoreCase and (B1^ in AlphaChar) and (B2^ in AlphaChar) then
        Result := (Ord(B1^) or $20) - (Ord(B2^) or $20) else
        Result := Ord(B1^) - Ord(B2^);
      if Result <> 0 then Exit;
      Inc(B1);
      Inc(B2);
    end;

    Result := L1 - L2;
  end;
end;

function buf_same(B1: pchar; L1: integer; B2: pchar; L2: integer; IgnoreCase: boolean): boolean;
begin
  Result := (buf_comp(B1, L1, B2, L2, IgnoreCase) = 0);
end;

function first_pos(this: pchar; thisLen: integer; patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
var
  endpos: pchar;
begin
  if (this <> nil) and (patten <> nil) and (pattenLen > 0) and (pattenLen <= thisLen) then
  begin
    endpos := this + (thisLen - pattenLen);
    Result := this;
    while Result <= endpos do
    begin
      if buf_same(Result, pattenLen, patten, pattenLen, IgnoreCase) then Exit;
      Inc(Result);
    end;
  end;
  Result := nil;
end;

function last_pos(this: pchar; thisLen: integer;
  patten: pchar; pattenLen: integer; IgnoreCase: boolean): pchar;
var
  endpos: pchar;
begin
  if (this <> nil) and (patten <> nil) and (pattenLen > 0) and (pattenLen <= thisLen) then
  begin
    Result := this + (thisLen - pattenLen);
    endpos := this;
    while Result >= endpos do
    begin
      if buf_same(Result, pattenLen, patten, pattenLen, IgnoreCase) then Exit;
      Dec(Result);
    end;
  end;
  Result := nil;
end;

function new_named_list(Sorted: boolean): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := true;
  Result.Sorted := Sorted;
end;

function find_named(list: TStrings; const name: string): pointer;
var
  X: integer;
begin
  X := named_index(list, name);
  if X >= 0 then
    Result := list.Objects[X] else
    Result := nil;
end;

function named_index(list: TStrings; const name: string): integer;
begin
  if list <> nil then
    Result := list.IndexOf(name) else
    Result := -1;
end;

function named_exists(list: TStrings; const name: string): boolean;
begin
  Result := (named_index(list, name) >= 0);
end;

function fill_zero(buf: pointer; count: integer): pointer;
begin
  FillChar(buf^, count, 0);
  Result := buf;
end;

procedure check(ok: boolean; const msg: string);
begin
  if not ok then lse_error(msg);
end;

function is_file(const Source: string): boolean;
begin
  {$IFNDEF WINDOWS}
  Result := FileExists(Source) and not DirectoryExists(Source);
  {$ELSE}
  Result := FileExists(Source);
  {$ENDIF}
end;

function is_relative_fname(const FileName: string): boolean;
begin
  Result := (FileName <> '') and (FileName[1] <> LSE_PATH_DELIMITER);
  {$IFDEF WINDOWS}
  if Result then
    if (FileName[1] in AlphaChar) and (Length(FileName) > 1) then
      Result := (FileName[2] <> ':');
  {$ENDIF}
end;

function absolute_fname(const FileName, BasePath: string): string;
begin
  Result := Trim(FileName);
  if is_relative_fname(Result) then
    Result := IncludeTrailingPathDelimiter(Trim(BasePath)) + Result;
  Result := lse_expand_fname(Result);
end;

function same_fname(const F1, F2: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result := CompareText(F1, F2) = 0;
  {$ELSE}
  Result := (F1 = F2);
  {$ENDIF}
end;

function file_text(const FileName: string): string;
var
  N: integer;
begin
  with TFileStream.Create(lse_veryPD(FileName), fmShareDenyWrite) do
  try
    N := size;
    if N > 0 then
    begin
      SetString(Result, nil, N);
      SetLength(Result, Read(pchar(Result)^, N));
    end
    else Result := '';
  finally
    Free;
  end;
end;

function encode_UTF8(const S: string): string;
begin
  Result := AnsiToUtf8(S);
end;

function decode_UTF8(const S: string): string;
begin
  Result := Utf8ToAnsi(S);
end;

function get_open_file_mode(const openMode: string;
  var fileMode: word; var R, W: boolean): boolean;
var
  index: integer;
  C, E: boolean;
begin
  C := false;
  E := false;
  R := false;
  W := false;
  for index := 1 to Length(openMode) do
    case openMode[index] of
      'c', 'C': C := true; // create
      'e', 'E': E := true; // exclusive
      'r', 'R': R := true; // read
      'w', 'W': W := true; // write
    end;
  Result := (C or E or R or W);
  if Result then
  begin
    if C then
    begin
      fileMode := fmCreate;
      R := true;
      W := true;
    end
    else
    if R then
    begin
      if W then fileMode := fmOpenReadWrite or fmShareExclusive else
      if E then fileMode := fmOpenRead or fmShareExclusive else
                fileMode := fmShareDenyWrite;
    end
    else
    if W then
      fileMode := fmOpenWrite or fmShareExclusive else
//  if E then
      fileMode := fmOpenRead or fmShareExclusive;
  end
end;

function strec_comp(S1, S2: PLseString; IgnoreCase: boolean): integer;
begin
  Result := buf_comp(lse_strec_data(S1), lse_strec_length(S1),
    lse_strec_data(S2), lse_strec_length(S2), IgnoreCase);
end;

function strec_count_tab(this: PLseString; var LCount, MCount, RCount: integer): integer;
begin
  Result := count_tab(lse_strec_data(this), lse_strec_length(this), LCount, MCount, RCount);
end;

function count_tab(buf: pchar; count: integer; var LCount, MCount, RCount: integer): integer;
var
  head, last: pchar;
begin
  LCount := 0;
  MCount := 0;
  RCount := 0;
  if (buf <> nil) and (count > 0) then
  begin
    head := buf;
    last := buf + (count - 1);
    while (head <= last) and (head^ in SpaceChar) do Inc(head);
    LCount := head - buf;     // buf .. head .. last
    buf := last;
    while (head <= buf) and (buf^ in SpaceChar) do Dec(buf);
    RCount := last - buf;     // head .. buf .. last
    Inc(head);
    while head < buf do
    begin
      if head^ in SpaceChar then Inc(MCount);
      Inc(head);
    end;
  end;
  Result := LCount + MCount + RCount;
end;

function encode_HTML(Buf: pchar; Count: integer; TranslateMBC: boolean): string;
var
  index: integer;
  ch: char;
//temp: string;
//ustr: WideString;
begin
  Result := '';
  if (Buf <> nil) and (Buf^ <> #0) and (Count > 0) then
  begin
    index := 0;
    while (index < Count) and (Buf^ <> #0) do
    begin
      ch := Buf[index];
      case ch of
        '<': Result := Result + '&lt;';
        '>': Result := Result + '&gt;';
        '&': Result := Result + '&amp;';
        ' ': Result := Result + '&nbsp;';
        '"': Result := Result + '&quot;';
        #9 : Result := Result + '&nbsp;&nbsp;&nbsp;&nbsp;';
        #13: begin
               Result := Result + '<br>' + sLineBreak;
               if index < (Count - 1) then
                 if Buf[index + 1] = #10 then Inc(index);
             end;
        #10: Result := Result + '<br>' + sLineBreak;
        else {
          if TranslateMBC and (ch in LeadBytes) and (index < Count - 1) and (Buf[index + 1] in LeadBytes) then
          begin
            SetString(temp, Buf + index, 2);
            ustr := temp;
            Result := Result + '&#' + IntToStr(Ord(ustr[1])) + ';';
            Inc(index);
          end
          else
          if Buf[index] in [#92, #160 .. #255] then
            Result := Result + '&#' + IntToStr(Ord(ch)) +';' else
        }   Result := Result + ch;
      end;
      Inc(index);
    end;
  end;
end;

function hash_of(const Key: string): cardinal;
var
  index, count: integer;
begin
  Result := 0;
  count := Length(Key);
  if count > 64 then count := 64;
  for index := 1 to count do
    Result := ((Result shl 2) or (Result shr (sizeof(Result) * 8 - 2)))
      xor Ord(Key[index]);
end;

function new_string(Source: pchar; Count: integer): string;
begin
  if (Source <> nil) and (Count > 0) then
    SetString(Result, Source, Count) else
    Result := '';
end;

procedure free_and_nil(var obj);
var
  temp: TObject;
begin
  temp := TObject(obj);
  if temp <> nil then
  begin
    temp.Free;
    pointer(obj) := nil;
  end;
end;

function str_to_comma(const AStr: string): string;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try
    list.Text := AStr;
    Result := list.CommaText;
  finally
    list.Free;
  end;
end;

function remove_from(list: TStrings; item: pointer): boolean;
var
  X: integer;
begin
  Result := list <> nil;
  if Result then
  begin
    X := list.IndexOfObject(TObject(item));
    Result := X >= 0;
    if Result then list.Delete(X);
  end;
end;

procedure release_string_object_list(var list: TStringList);
var
  A: integer;
  O: TObject;
begin
  if list <> nil then
  begin
    for A := list.Count - 1 downto 0 do
    begin
      O := list.Objects[A];
      list.Delete(A);
      if O <> nil then O.Free;
    end;
    FreeAndNil(list);
  end;
end;

function seek_ch(const S: pchar; CharSet: KLiCharSet): pchar;
begin
  Result := S;
  if Result <> nil then
    while not (Result^ in CharSet) do
      Inc(Result);
end;

function value_new: PLseValue;
begin
  Result := lse_mem_alloc_zero(sizeof(RLseValue));
end;

function value_new(Value: int64): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_INT;
  Result^.VInteger := Value; 
end;

function value_new(Value: double): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_FLOAT;
  Result^.VFloat := Value;
end;

function value_new(const Value: string): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_STRING;
  Result^.VObject := lse_strec_alloc(Value);
  lse_strec_inclife(Result^.VObject);
end;

function value_new(const Value: PLseString): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := KR_STRING;
  Result^.VObject := Value;
  lse_strec_inclife(Value);
end;

function value_new(const Value: PLseValue): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Move(Value^, Result^, sizeof(RLseValue));
  lse_addref(Result);
end;

function value_new(const Value: pointer; AType: PLseType): PLseValue;
begin
  Result := lse_mem_alloc(sizeof(RLseValue));
  Result^.vtype := AType;
  Result^.VObject := Value;
  AType^.cr_addref(Value);
end;

procedure value_free(V: PLseValue);
begin
  if V <> nil then
  begin
    lse_set_nil(V);
    lse_mem_free(V, sizeof(RLseValue));
  end;
end;

function value_int(V: PLseValue): int64;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := parse_int(lse_strec_data(V^.VObject));
    LSV_INT   : Result := V^.VInteger;
    LSV_FLOAT : Result := Trunc(V^.VFloat);
    else         Result := 0;
  end;
end;

function value_float(V: PLseValue): double;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := parse_ext(lse_strec_data(V^.VObject));
    LSV_INT   : Result := V^.VInteger;
    LSV_FLOAT : Result := V^.VFloat;
    else         Result := 0;
  end;
end;

function value_time(V: PLseValue): TDateTime;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := lse_decode_GMT(value_str(V));
    LSV_FLOAT : Result := V^.VFloat;
    LSV_INT   : Result := UnixToDateTime(V^.VInteger);
    else        Result := 0;
  end;
end;

function value_str(V: PLseValue): string;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := lse_strec_string(V^.VObject);
    LSV_INT   : Result := IntToStr(V^.VInteger);
    LSV_FLOAT : Result := FloatToStr(V^.VFloat);
    LSV_OBJECT: Result := value_type(V).ObjectToString(V^.VObject);
    else        Result := '';
  end;
end;

function value_fname(V: PLseValue): string;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := lse_veryPD(lse_strec_string(V^.VObject)) else
    Result := '';
end;

function value_strec(V: PLseValue): PLseString;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := V^.VObject else
    Result := nil;
end;

function value_pchar(V: PLseValue): pchar;
begin
  Result := lse_strec_data(value_strec(V));
end;

function value_char(V: PLseValue): char;
begin
  case lse_vtype(V) of
    LSV_STRING: if V^.VObject <> nil then
                  Result := lse_strec_data(V^.VObject)^ else
                  Result := #0;
    LSV_INT   : Result := char(V^.VInteger);
    LSV_FLOAT : Result := char(Trunc(V^.VFloat));
    else        Result := #0;
  end;
end;

function value_bool(V: PLseValue): boolean;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := (lse_strec_length(V^.VObject) > 0);
    LSV_INT   : Result := (V^.VInteger <> 0);
    LSV_FLOAT : Result := not IsZero(V^.VFloat);
    LSV_OBJECT: Result := (V^.VObject <> nil);
    else        Result := false;
  end;
end;

function value_obj(V: PLseValue): pointer;
begin
  if lse_vtype(V) = LSV_OBJECT then
    Result := V^.VObject else
    Result := nil;
end;

function value_obj(V: PLseValue; T: KLiType): pointer;
var
  R: PLseType;
begin
  R := lse_type(V);
  if (R^.cr_type = LSV_OBJECT) and (KLiType(R^.cr_class) = T) then
    Result := V^.VObject else
    Result := nil;
end;

function get_runner(Param: PLseParam): KLiRunner;
begin
  Result := KLiRunner(Param^.p_runner);
end;

function get_engine(Param: PLseParam): KLiEngine;
begin
  Result := KLiRunner(Param^.p_runner).FEngine;
end;

function value_func(V: PLseValue): KLiFunc;
begin
  Result := KLiFunc(value_obj(V, KT_FUNC));
end;

function value_varlist(V: PLseValue): KLiVarList;
begin
  Result := KLiVarList(value_obj(V, KT_VARLIST));
end;

function value_vargen(V: PLseValue; Engine: KLiEngine): PLseVargen;
var
  crec: PLseType;
  clss: KLiType;
  varg: PLseVargen;
  list: pointer;
begin
  varg := nil;
  crec := lse_type(V);
  clss := KLiType(crec^.cr_class);
  if clss = KT_VARGEN then varg := PLseVargen(V^.VObject) else
  if clss = KT_STRING then varg := type_string_vargen(V^.VObject, Engine) else
  if clss = KT_INT then varg := type_vargen_upto(0, V^.VInteger - 1, 1, Engine) else
  if clss = KT_FLOAT then varg := type_vargen_upto(0, Trunc(V^.VFloat) - 1, 1, Engine) else
  if crec^.cr_type = LSV_OBJECT then
  begin
    list := V^.VObject;
    if list <> nil then
      if Assigned(crec^.cr_vargen) then
        varg := crec^.cr_vargen(list, Engine) else
        varg := lse_vargen_none;
  end;
  if varg = nil then
    Result := lse_vargen_none else
    Result := varg;
end;

function value_type(V: PLseValue): KLiType;
begin
  Result := KLiType(lse_type(V)^.cr_class);
end;

function get_this(Param: PLseParam; var This): boolean;
var
  this_obj: pointer;
begin
  this_obj := value_obj(Param^.p_param[0]);
  Result := (this_obj <> nil);
  if Result then
    pointer(This) := this_obj else
    set_error_this(Param);
end;

procedure udc_curry(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
  func: KLiFunc_curry;
  X, N: integer;
begin
  rnnr := KLiRunner(Param^.p_runner);
  func := KLiFunc_curry(Param^.p_func);
  N := func.CurryCount;
  for X := 0 to N - 1 do
    rnnr.FStack.Add(func.CurryData(X));
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
  data := get_engine(Param).FMainValues.FindValue(name);
  if data <> nil then
    lse_set_value(Param^.p_result, data) else
    lse_clear_value(Param^.p_result);
end;

procedure udc_oper(const Param: PLseParam);cdecl;
var
  func: KLiFunc_oper;
  rnnr: KLiRunner;
  L, R: PLseValue;
begin
  L := Param^.p_result;
  lse_set_value(L, Param^.p_param[0]);
  R := Param^.p_param[1];
  rnnr := KLiRunner(Param^.p_runner);
  func := KLiFunc_oper(Param^.p_func);
  case func.FOper of
    syMul   : value_mul(L, R);
    syDiv   : value_div(L, R);
    syMod   : value_mod(L, R, rnnr);
    syAdd   : value_add(L, R);
    syDec   : value_dec(L, R);
    syBXor  : value_xor(L, R);
    syBAnd  : value_and(L, R);
    syBOr   : value_or(L, R);
    syBShl  : value_shl(L, R, rnnr);
    syBShr  : value_shr(L, R);
    syFill  : value_fill(L, R, rnnr);
    syEQ    : value_equal(L, R);
    syNE    : value_diff(L, R);
    syLess  : value_less(L, R);
    syLE    : value_eqless(L, R);
    syMore  : value_more(L, R);
    syME    : value_eqmore(L, R);
    syIs    : value_is(L, R);
    syAs    : value_as(L, R, rnnr.FEngine);
    syLike  : value_like(L, R, rnnr);
    syAnd   : value_logic_and(L, R);
    syOr    : value_logic_or(L, R);
  end;
end;

procedure udc_empty(const Param: PLseParam);cdecl;
begin
  { do nothing }
end;

procedure set_error(Param: PLseParam);overload;
begin
  set_error(Param, '', 0, '');
end;

procedure set_error(Param: PLseParam; const Msg: string);
begin
  set_error(Param, '', 0, Msg);
end;

procedure set_error(Param: PLseParam; const Msg: string; const Args: array of const);
begin
  set_error(Param, '', 0, Format(Msg, Args));
end;

procedure set_error(Param: PLseParam; const EID: string; Errno: integer; const Msg: string);
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
    errid := EID;
    if not is_ID(pchar(errid)) then
      errid := func.FModule.Name + 'Error';
    if Errno = 0 then
      Errno := ERUNTIME;
    error := Msg;
    if error = '' then
      error := lse_exception_str;
    if error = '' then
      error := func.Name + '() - ' + errid else
      error := func.Name + '() - ' + error;
    if PLiToken(Param^.p_exprec)^.tk_pos.module <> nil then
      module := KLiModule(PLiToken(Param^.p_exprec)^.tk_pos.module) else
      module := runner.CurrentFunc.FModule;
    runner.Engine.Error.write(errid, Errno,
      PLiToken(Param^.p_exprec)^.tk_pos.row,
      PLiToken(Param^.p_exprec)^.tk_pos.col,
      module.Name, error, module.FileName);
    runner.FExcepted := true;
  end;
end;

procedure set_error_this(Param: PLseParam);
begin
  set_error(Param, 'this is nil');
end;

procedure value_add(V1, V2: PLseValue);

  procedure on_string;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R <> LSV_VOID then
    begin
      if R = LSV_STRING then
        lse_set_string(V1, lse_strec_cat(V1^.VObject, V2^.VObject)) else
        lse_set_string(V1, lse_strec_cat(V1^.VObject, value_str(V2)));
    end
    else lse_clear_string(V1);
  end;

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R = LSV_FLOAT then
      lse_set_float(V1, value_float(V1) + value_float(V2)) else
    if R = LSV_INT then
      lse_set_int64(V1, V1^.VInteger + value_int(V2)) else
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(value_str(V1), V2^.VObject)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat + value_float(V2)) else
    if R = LSV_STRING then
      lse_set_string(V1, lse_strec_cat(value_str(V1), V2^.VObject)) else
      lse_init_value(V1);
  end;
  
begin
  case lse_vtype(V1) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_dec(V1, V2: PLseValue);

  procedure on_int;
  var
    R: TLseValue;
  begin
    R := lse_vtype(V2);
    if R = LSV_FLOAT then
      lse_set_float(V1, V1^.VInteger - value_float(V2)) else
    if R = LSV_INT then
      lse_set_int64(V1, V1^.VInteger - value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat - value_float(V2)) else
      lse_init_value(V1);
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_mul(V1, V2: PLseValue);

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
      lse_set_float(V1, V1^.VInteger * value_float(V2)) else
    if R = LSV_INT then
      lse_set_int64(V1, V1^.VInteger * value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_float(V1, V1^.VFloat * value_float(V2)) else
      lse_init_value(V1);
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_div(V1, V2: PLseValue);

  procedure on_int;
  var
    F: double;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
    begin
      F := value_float(V2);
      if IsZero(F) then
        lse_set_float(V1, 0) else
        lse_set_float(V1, V1^.VInteger / F);
    end
    else lse_init_value(V1);
  end;

  procedure on_float;
  var
    F: double;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
    begin
      F := value_float(V2);
      if IsZero(F) then
        lse_set_float(V1, 0) else
        lse_set_float(V1, V1^.VFloat / F);
    end
    else lse_init_value(V1);
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_mod(V1, V2: PLseValue; R: KLiRunner);

  procedure on_string;
  var
    L: KLiVarlist;
    S: string;
  begin
    if KLiType(V2^.vtype^.cr_class) = KT_VARLIST then
    begin
      L := value_varlist(V2);
      S := R.FormatFor(value_str(V1), L);
    end
    else
    begin
      L := KLiVarList.Create(R.FEngine);
      try
        L.Add(V2);
        S := R.FormatFor(value_str(V1), L);
      finally
        L.Free;
      end;
    end;
    lse_strec_release(V1^.VObject);
    V1^.VObject := lse_strec_alloc(S);
    lse_strec_addref(V1^.VObject);
  end;

  procedure on_int;
  var
    F: int64;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
    begin
      F := value_int(V2);
      if F = 0 then
        lse_set_int64(V1, 0) else
        lse_set_int64(V1, V1^.VInteger mod F);
    end
    else lse_init_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : on_string;
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_neg(V1: PLseValue);
begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : V1^.VInteger := - V1^.VInteger;
    LSV_FLOAT  : V1^.VFloat := - V1^.VFloat;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_xor(V1, V2: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V1, V1^.VInteger xor value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_and(V1, V2: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V1, V1^.VInteger and value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_or(V1, V2: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V1, V1^.VInteger or value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_shl(V1, V2: PLseValue; R: KLiRunner);

  procedure on_int;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V1, V1^.VInteger shl value_int(V2)) else
      lse_init_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

  procedure on_object;
  begin
    if V1^.VObject <> nil then
      if Assigned(V1^.vtype^.cr_add) then
        V1^.vtype^.cr_add(V1^.VObject, V2, R.FEngine);
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : on_object;
  end;
end;

procedure value_shr(V1, V2: PLseValue);

  procedure on_int;
  begin
    if lse_vtype(V2) in [LSV_FLOAT, LSV_INT] then
      lse_set_int64(V1, V1^.VInteger shr value_int(V2)) else
      lse_clear_value(V1);
  end;

  procedure on_float;
  begin
    V1^.vtype := KR_INT;
    V1^.VInteger := Trunc(V1^.VFloat);
    on_int;
  end;

begin
  case lse_vtype(V1) of
    LSV_STRING : lse_clear_string(V1);
    LSV_INT    : on_int;
    LSV_FLOAT  : on_float;
    LSV_OBJECT : lse_clear_object(V1);
  end;
end;

procedure value_not(V1: PLseValue);
begin
  if lse_vtype(V1) in [LSV_FLOAT, LSV_INT] then
    lse_set_int64(V1, not value_int(V1)) else
    lse_clear_value(V1);
end;

procedure value_logic_and(V1, V2: PLseValue);
begin
  if value_bool(V1) and not value_bool(V2) then
    lse_set_value(V1, V2);
end;

procedure value_logic_or(V1, V2: PLseValue);
begin
  if not value_bool(V1) and value_bool(V2) then
    lse_set_value(V1, V2);
end;

procedure value_logic_not(V1: PLseValue);
begin
  lse_set_bool(V1, not value_bool(V1));
end;

procedure value_equal(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crEqual]));
end;

procedure value_diff(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crLess, crMore, crDiff]));
end;

procedure value_less(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crLess]));
end;

procedure value_eqless(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crLess, crEqual]));
end;

procedure value_more(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crMore]));
end;

procedure value_eqmore(V1, V2: PLseValue);
begin
  lse_set_bool(V1, value_match(V1, V2, [crMore, crEqual]));
end;

procedure value_abseq(V1, V2: PLseValue);
var
  clss: KLiType;
  same: boolean;
begin
  same := false;
  clss := value_type(V1);
  if clss = value_type(V2) then
    case clss.TypeRec^.cr_type of
      LSV_VOID   : same := true;
      LSV_STRING : same := (V1^.VObject = V2^.VObject)
                        or buf_same(lse_strec_data(V1^.VObject),
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

procedure value_like(V1, V2: PLseValue; R: KLiRunner);
var
  P: PLiRegexpr;
begin
  P := @R.FRegexpr;
  lse_set_bool(V1, regexpr_init(P, value_pchar(V2)) and
                   regexpr_exec(P, value_strec(V1)));
end;

procedure value_fill(V1, V2: PLseValue; R: KLiRunner);
var
  K: PLseType;
  G: PLseVargen;
begin
  K := lse_type(V1);
  if Assigned(K^.cr_add) and (V1^.VObject <> nil) then
  begin
    G := value_vargen(V2, R.FEngine);
    lse_vargen_addref(G);
    try
      while lse_vargen_send(G, V2) do
        K^.cr_add(V1^.VObject, V2, R.FEngine);
    finally
      lse_vargen_release(G);
    end;
  end;
end;

procedure value_is(V1, V2: PLseValue);
var
  T1, T2: KLiType;
begin
  T2 := value_type(V2);
  if T2 = KT_CLASS then
  begin
    T2 := KLiType(value_obj(V2));
    T1 := value_type(V1);
    if T1 = KT_CLASS then
      T1 := KLiType(value_obj(V1));
    lse_set_bool(V1, T1 = T2);
  end
  else value_abseq(V1, V2);
end;

procedure value_as(V1, V2: PLseValue; E: KLiEngine);
var
  T2: KLiType;
begin
  T2 := value_type(V2);
  if T2 = KT_CLASS then
    T2 := KLiType(value_obj(V2));
  T2.Cast(V1, E);
end;

function value_compare(V1, V2: PLseValue): KLiCompResult;
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
      Result := compareS(value_str(V1), value_str(V2)) else
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
    Result := compareF(value_float(V1), value_float(V2)) else
    Result := compareI(value_int(V1), value_int(V2));
end;

function value_match(V1, V2: PLseValue; Test: KLiCompResults): boolean;
begin
  Result := (value_compare(V1, V2) in Test);
end;

function value_in(V: PLseValue; Host: TStringList): boolean;
begin
  Result := (lse_type(V)^.cr_type = LSV_STRING) and
            (Host.IndexOf(lse_strec_string(V^.VObject)) >= 0);
end;

function value_in(V: PLseValue; Host: KLiVarList; FindItemVG: boolean): boolean;
var
  clss, tmpc: KLiType;
  next, size: integer;
  data: PLseValue;
begin
  Result := false;
  clss := value_type(V);
  size := Host.Count;
  next := 0;
  while not Result and (next < size) do
  begin
    data := Host[next];
    tmpc := value_type(data);
    if clss = tmpc then
      case clss.TypeRec^.cr_type of
        LSV_VOID   : Result := true;
        LSV_STRING : Result := (V^.VObject = data^.VObject)
                            or buf_same(lse_strec_data(V^.VObject),
                                        lse_strec_length(V^.VObject),
                                        lse_strec_data(data^.VObject),
                                        lse_strec_length(data^.VObject),
                                        false);
        LSV_INT    : Result := (V^.VInteger = data^.VInteger);
        LSV_FLOAT  : Result := IsZero(V^.VFloat - data^.VFloat);
//      LSV_VARIANT: Result := true;
        LSV_OBJECT : Result := (V^.VObject = data^.VObject);
      end;
    if not Result and FindItemVG then
      if (tmpc = KT_VARGEN) and (data^.VObject <> nil) then
        Result := lse_vargen_contains(PLseVargen(data^.VObject), V);
    Inc(next);
  end;
end;

function value_in(V: PLseValue; Host: PLseString): boolean;
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
    Result := (nil <> first_pos(base, slen, lse_strec_data(S),
                            lse_strec_length(S), false));
  end;
  
begin
  Result := false;
  if Host <> nil then
  begin
    base := lse_strec_data(Host);
    slen := lse_strec_length(Host);
    case lse_type(V)^.cr_type of
      LSV_STRING: Result := hass(V^.VObject);
      LSV_INT   : Result := (V^.VInteger in [0..255]) and
                             hasc(char(V^.VInteger));
    end;
  end;
end;

function value_in(V: PLseValue; Host: int64): boolean;
begin
  Result := (Host > 0) and
            (lse_type(V)^.cr_type = LSV_INT) and
            (Host > V^.VInteger) and
            (V^.VInteger >= 0);
end;

function value_in(V: PLseValue; Host: KLiHashed): boolean;
begin
  Result := (Host <> nil) and
            (V^.vtype <> nil) and
            (V^.vtype^.cr_type = LSV_STRING) and
            Host.IsSet(value_str(V));
end;

function extract_name_module(const ID: string; var Module: string): string;
var
  X: integer;
begin
  X := Pos('::', ID);
  if X > 0 then
  begin
    Result := Copy(ID, X + 2, MaxInt);
    Module := Copy(ID, 1, X - 1);
  end
  else
  begin
    Result := ID;
    Module := '';
  end;
end;

function module_load(const name: string; const FileName: string): KLiModule;
label
  UNLOCK;
var
  handle: THandle;
  IE: TLseInitExchange;
  MR: RLseModule;
begin
  kernel_lock;
  try
    // 1. search current instance

    Result := nil;

    if not is_ID(pchar(name)) then
      goto UNLOCK;

    Result := KLiModule(find_named(sys_libraries, name));
    if Result <> nil then
      goto UNLOCK;

    // 2. search library file

    handle := 0;
    if not lse_load_library(FileName, handle) then
      goto UNLOCK;

    IE := TLseInitExchange(lse_get_proc(handle, 'InitExchange'));
    if not Assigned(IE) then
    begin
      lse_free_library(handle);
      goto UNLOCK;
    end;

    // 4. initialize the library

    fill_zero(@MR, sizeof(RLseModule));
    IE(@MR, @sys_cik_entries);

    // 5. setup classes list

    Result := KLiModule.Create(name, nil, moyLibrary);
    try
      Result.FFileName := FileName;
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
    kernel_unlock;
  end;
end;

function module_register(const name: string; MR: PLseModule): KLiModule;
label
  UNLOCK;
var
  ID, fname: string;
begin
  kernel_lock;
  try
    // 1. check if already exists
    Result := nil;
    if Pos('=', name) > 0 then
    begin
      ID := Trim(extract_name_value(name, fname));
      fname := Trim(fname);
      if fname = '' then fname := ID else
      if FileExists(fname) then
        fname := lse_expand_fname(fname);
    end
    else ID := name;

    if not is_ID(pchar(ID)) or is_reserved(ID, true)
      or named_exists(sys_libraries, ID)
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
    kernel_unlock;
  end;
end;

function module_search(var Name: string; const PathList: string; var IsDLL: boolean): boolean;
var
  path, temp: string;
  base, next, slen: integer;
begin
  slen := Length(PathList);
  base := 0;
  repeat
    next := base + 1;
    while (next <= slen) and (PathList[next] <> ';') do Inc(next);
    path := Trim(Copy(PathList, base + 1, next - base - 1));
    Result := path <> '';
    if Result then
    begin
      path := full_path(path);

      temp := path + Name + LSE_DLLEXT;
      Result := FileExists(temp);
      if Result then
      begin
        Name := temp;
        IsDLL := true;
        Exit;
      end;

      temp := path + Name + '.ls';
      Result := FileExists(temp);
      if Result then
      begin
        Name := temp;
        IsDLL := false;
        Exit;
      end;
    end;
    base := next;
  until next >= slen;
end;

procedure kernel_setup_builtin_types;
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

    lse_fill_typerec(@class_rec, LSV_VOID);
    KT_VOID := classup(sys_module, kcVoid, KR_VOID);

    lse_fill_typerec(@class_rec, LSV_STRING);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_strec_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_strec_release;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_string_vargen;
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_string_getiv;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}type_string_length;
    KT_STRING := classup(sys_module, kcString, KR_STRING);

    lse_fill_typerec(@class_rec, LSV_INT);
    KT_INT := classup(sys_module, kcInteger, KR_INT);

    lse_fill_typerec(@class_rec, LSV_FLOAT);
    KT_FLOAT := classup(sys_module, kcFloat, KR_FLOAT);

    lse_fill_typerec(@class_rec, LSV_VARIANT);
    KT_VARIANT := classup(sys_module, kcVariant, KR_VARIANT);

    lse_fill_typerec(@class_rec, 'type', 'type', LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}type_type_otos;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}type_type_stoo;
    KT_CLASS := classup(sys_module, kcType, KR_CLASS);

    lse_fill_typerec(@class_rec, 'module', 'module', LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}type_module_otos;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}type_module_stoo;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_module_getpv;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}type_module_setpv;
    KT_MODULE := classup(sys_module, kcModule, KR_MODULE);

    lse_fill_typerec(@class_rec, 'function', 'function', LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}type_function_otos;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_function_vargen;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}type_function_add;
    KT_FUNC := classup(sys_module, kcFunc, KR_FUNC);

    lse_fill_typerec(@class_rec, 'error', 'error', LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}type_error_otos;
    KT_ERROR := classup(sys_module, kcError, KR_ERROR);

    lse_fill_typerec(@class_rec, 'stream', 'stream', LSV_OBJECT);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_stream_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_stream_release;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_stream_vargen;
    class_rec.cr_write_to := {$IFDEF FPC}@{$ENDIF}type_stream_writo;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}type_stream_add;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}type_stream_length;
    KT_STREAM := classup(sys_module, kcStream, KR_STREAM);

    lse_fill_typerec(@class_rec, 'varlist', 'varlist', LSV_OBJECT);
    class_rec.cr_otos := {$IFDEF FPC}@{$ENDIF}type_varlist_otos;
    class_rec.cr_stoo := {$IFDEF FPC}@{$ENDIF}type_varlist_stoo;
    class_rec.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_varlist_vargen;
    class_rec.cr_add := {$IFDEF FPC}@{$ENDIF}type_varlist_add;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}type_varlist_length;
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_varlist_getiv;
    class_rec.cr_setiv := {$IFDEF FPC}@{$ENDIF}type_varlist_setiv;
    KT_VARLIST := classup(sys_module, kcVarlist, KR_VARLIST);

    lse_fill_typerec(@class_rec, 'varsnap', 'varsnap', LSV_OBJECT);
    class_rec.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_varsnap_getiv;
    class_rec.cr_setiv := {$IFDEF FPC}@{$ENDIF}type_varsnap_setiv;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_varsnap_getpv;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}type_varsnap_setpv;
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}type_varsnap_length;
    KT_VARSNAP := classup(sys_module, kcVarsnap, KR_VARSNAP);

    lse_fill_typerec(@class_rec, 'hashed', 'hashed', LSV_OBJECT);
    class_rec.cr_length := {$IFDEF FPC}@{$ENDIF}type_hashed_length;
    class_rec.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_hashed_getpv;
    class_rec.cr_setpv := {$IFDEF FPC}@{$ENDIF}type_hashed_setpv;
    KT_HASHED := classup(sys_module, kcHashed, KR_HASHED);

    lse_fill_typerec(@class_rec, 'vargen', 'vargen', LSV_OBJECT);
    class_rec.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_vargen_addref;
    class_rec.cr_release := {$IFDEF FPC}@{$ENDIF}lse_vargen_release;
    KT_VARGEN := classup(sys_module, kcVargen, KR_VARGEN);

    { setup class methods }

    sys_module.SetupModuleFuncs(@sys_module_funcs);

    { prepare }

    sys_module.FindFunc('eol').IsNameCall := true;
  end;
end;

procedure runner_next(Sender: KLiRunner);
begin
  with Sender do
    if (FCurrent <> nil) and not (FExcepted or FTerminated) then
      Inc(FCurrent^.next);
end;

procedure runner_error(Sender: KLiRunner);
begin
  with Sender do
  begin
    ErrorRT(Format(EsUnknownOper, [Symbols[FExprrec^.tk_sym].ID]));
    Terminate;
  end;
end;

procedure runner_ID(Sender: KLiRunner);
var
  func: KLiFunc;
  srec: RLiFind;
  data: RLseValue;
  vrec: PLseValue;
begin
  with Sender do
  begin
    vrec := FCurrent^.values.GetValue(FExprrec^.tk_name);
    if vrec <> nil then FStack.Add(vrec) else
    if FCurrent^.func.FindBy(FExprrec^.tk_name, @srec) then
    begin
      if srec.fo_type = foFunc then
      begin
        if srec.VFunc.IsNameCall then
          Sender.Goon(srec.VFunc, 0, nil) else
        if srec.VFunc.HasSuper then
        begin
          data.vtype := KR_VARSNAP;
          data.VObject := FCurrent^.values;
          func := srec.VFunc.Curry(@data, CurrentModule);
          func.SaveTo(FStack.Add);
          if func <> srec.VFunc then
            func.DecRefcount;
        end
        else FStack.Add(srec.VFunc, KR_FUNC);
      end
      else
      if srec.fo_type = foType then
        srec.VType.SaveTo(FStack.Add) else
      if srec.fo_type = foModule then
        srec.VModule.SaveTo(FStack.Add) else
        FStack.Add;
    end
    else lse_error('Object %s not found', [FExprrec^.tk_name]);
  end;
  runner_next(Sender);
end;

procedure runner_become(Sender: KLiRunner);
begin
  with Sender do
    FCurrent^.values.SetValue(FExprrec^.tk_name, FStack[-1]);
  runner_next(Sender);
end;

procedure runner_float(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VFLoat);
  runner_next(Sender);
end;

procedure runner_echo(Sender: KLiRunner);
var
  A, index: integer;
  data: PLseValue;
begin
  with Sender do
  begin
    index := FStack.Count - FExprrec^.tk_prmc;
    for A := 0 to FExprrec^.tk_prmc - 1 do
    begin
      if A > 0 then
        lse_stream_write(FEngine.Output, ' ', 1);
      data := FStack[index + A];
      if lse_vtype(data) = LSV_STRING then
        lse_stream_write(FEngine.Output, PLseString(data^.VObject)) else
        lse_stream_write(FEngine.Output, value_str(data));
    end;
    FStack.Press(FExprrec^.tk_prmc);
    FStack.Add;
  end;
  runner_next(Sender);
end;

procedure runner_int(Sender: KLiRunner);
begin
  with Sender do
    FStack.Add(FExprrec^.VInteger);
  runner_next(Sender);
end;

procedure runner_str(Sender: KLiRunner);
begin
  with Sender do
    lse_set_string(FStack.Add, FExprrec^.tk_name);
  runner_next(Sender);
end;

procedure runner_add(Sender: KLiRunner);
begin
  value_add(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_dec(Sender: KLiRunner);
begin
  value_dec(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_mul(Sender: KLiRunner);
begin
  value_mul(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_div(Sender: KLiRunner);
begin
  value_div(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_mod(Sender: KLiRunner);
begin
  value_mod(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bnot(Sender: KLiRunner);
begin
  value_not(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_bxor(Sender: KLiRunner);
begin
  value_xor(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bor(Sender: KLiRunner);
begin
  value_or(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_band(Sender: KLiRunner);
begin
  value_and(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bshl(Sender: KLiRunner);
begin
  value_shl(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bshr(Sender: KLiRunner);
begin
  value_shr(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_not(Sender: KLiRunner);
begin
  value_logic_not(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_neg(Sender: KLiRunner);
begin
  value_neg(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_eq(Sender: KLiRunner);
begin
  value_equal(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_ne(Sender: KLiRunner);
begin
  value_diff(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_less(Sender: KLiRunner);
begin
  value_less(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_le(Sender: KLiRunner);
begin
  value_eqless(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_more(Sender: KLiRunner);
begin
  value_more(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_me(Sender: KLiRunner);
begin
  value_eqmore(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_in(Sender: KLiRunner);
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
    T2 := value_type(V2);
    case T2.FTypeRec^.cr_type of
      LSV_STRING: done := value_in(V1, V2^.VObject);
      LSV_INT   : done := value_in(V1, V2^.VInteger);
      LSV_OBJECT: if V2^.VObject <> nil then
                    if T2 = KT_VARLIST then
                      done := value_in(V1, KLiVarList(V2^.VObject), true) else
                    if T2 = KT_HASHED then
                      done := value_in(V1, KLiHashed(V2^.VObject)) else
                      done := lse_vargen_contains(value_vargen(V2, FEngine), V1);
    end;
    FStack.DeleteLast;
  end;
  lse_set_bool(V1, done);
  runner_next(Sender);
end;

procedure runner_and(Sender: KLiRunner);
begin
  value_logic_and(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_or(Sender: KLiRunner);
begin
  value_logic_or(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_varlist(Sender: KLiRunner);
var
  L: KLiVarList;
  X, N: integer;
begin
  L := KLiVarList.Create(Sender.Engine);
  with Sender do
  begin
    N := FStack.Count;
    X := N - FExprrec^.tk_prmc;
    while X < N do
    begin
      lse_set_value(L.Add, FStack[X]);
      Inc(X);
    end;
    FStack.Press(FExprrec^.tk_prmc);
    FStack.Add(L, KR_VARLIST);
  end;
  runner_next(Sender);
end;

procedure runner_nil(Sender: KLiRunner);
begin
  Sender.FStack.AddDefault(KR_VOID);
  runner_next(Sender);
end;

procedure runner_getenv(Sender: KLiRunner);
begin
  with Sender do
    FEngine.GetValue(FExprrec^.tk_name, FStack.Add);
  runner_next(Sender);
end;

procedure runner_getsv(Sender: KLiRunner);
var
  A: KLiVarSnap;
  V: PLseValue;
begin
  with Sender do
  begin
    A := FCurrent^.values.Super;
    if A <> nil then
    begin
      V := A.GetValue(FExprrec^.tk_name);
      if V <> nil then
        FStack.Add(V) else
        lse_error('$%s not found', [FExprrec^.tk_name]);
    end
    else lse_error('super is nil');
  end;
  runner_next(Sender);
end;

procedure runner_setsv(Sender: KLiRunner);
var
  A: KLiVarSnap;
begin
  with Sender do
  begin
    A := FCurrent^.values.Super;
    if A <> nil then
      A.SetValue(FExprrec^.tk_name, FStack[-1]) else
      lse_error('super is nil');
  end;
  runner_next(Sender);
end;

procedure runner_format(Sender: KLiRunner);
var
  V: PLseValue;
begin
  V := Sender.FStack[-1];
  lse_set_string(V, Sender.FormatFor(value_str(V), nil));
  runner_next(Sender);
end;

procedure runner_is(Sender: KLiRunner);
begin
  value_is(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_as(Sender: KLiRunner);
begin
  value_as(Sender.FStack[-2], Sender.FStack[-1], Sender.FEngine);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_vargen(Sender: KLiRunner);
var
  last: PLseValue;
begin
  with Sender do
  begin
    last := FStack[-1];
    lse_set_vargen(last, value_vargen(last, FEngine));
  end;
  runner_next(Sender);
end;

procedure runner_ask(Sender: KLiRunner);
var
  base, prmc: integer;
  data: PLseValue;
  clss: KLiType;
  func: KLiFunc;
begin
  with Sender do
  begin
    prmc := FExprrec^.tk_prmc;
    base := FStack.Count - prmc;
    data := FStack[base];
    clss := value_type(data);
    if clss = KT_FUNC then
    begin
      func := value_func(data);
      check(func <> nil, EsFuncNotSpecify);
      Goon(func, prmc - 1, data);
    end
    else
    if clss = KT_CLASS then
    begin
      clss := KLiType(data^.VObject);
      check(clss <> nil, EsClassNotSpecify);
      clss.Default(data);
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
  runner_next(Sender);
end;

procedure runner_refer_ask(Sender: KLiRunner);
var
  base, prmc: integer;
begin
  with Sender do
  begin
    prmc := FExprrec^.tk_prmc;
    base := FStack.Count - prmc;
    FStack.Exchange(base, base + 1);
  end;
  runner_ask(Sender);
end;

procedure runner_try(Sender: KLiRunner);
var
  begx, endx: integer;
  in_finally: boolean;
begin
  with Sender do
  begin
    Inc(FCurrent^.next);
    begx := FCurrent^.next;
    endx := FExprrec^.VLabel^.tk_prmc;
    in_finally := (FExprrec^.tk_prmc > 0);
    while ExecGoonNext do
      if (FCurrent^.next < begx) or (FCurrent^.next > endx) then
        Break;
    if FExcepted and not FTerminated and HasNext then
    begin
      FExcepted := false;
      FStack.SetCount(FCurrent^.base);
      FCurrent^.next := endx + 1;
      ExecGoonNext; // syRINR
      if in_finally then
        FExcepted := true;
    end;
  end;
end;

procedure runner_return(Sender: KLiRunner);
begin
  with Sender do
  begin
    FCurrent^.next := LSE_MAX_CODES;
    FCurrent := nil; {<--returned}
  end;
end;

procedure runner_jump(Sender: KLiRunner);
begin
  with Sender do
    Inc(FCurrent^.next, FExprrec^.tk_prmc);
end;

procedure runner_jmpf(Sender: KLiRunner);
begin
  with Sender do
    if not value_bool(FStack[-1]) then
      Inc(FCurrent^.next, FExprrec^.tk_prmc) else
      Inc(FCurrent^.next);
end;

procedure runner_jmpt(Sender: KLiRunner);
begin
  with Sender do
    if value_bool(FStack[-1]) then
      Inc(FCurrent^.next, FExprrec^.tk_prmc) else
      Inc(FCurrent^.next);
end;

procedure runner_jmpfpop(Sender: KLiRunner);
begin
  runner_jmpf(Sender);
  Sender.FStack.DeleteLast;
end;

procedure runner_jmptpop(Sender: KLiRunner);
begin
  runner_jmpt(Sender);
  Sender.FStack.DeleteLast;
end;

procedure runner_goto(Sender: KLiRunner);
begin
  with Sender do
    FCurrent^.next := FExprrec^.VLabel^.tk_prmc;
end;

procedure runner_gototp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if value_bool(FStack[-1]) then
      FCurrent^.next := FExprrec^.VLabel^.tk_prmc else
      Inc(FCurrent^.next);
    FStack.DeleteLast;
  end;
end;

procedure runner_gotofp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if not value_bool(FStack[-1]) then
      FCurrent^.next := FExprrec^.VLabel^.tk_prmc else
      Inc(FCurrent^.next);
    FStack.DeleteLast;
  end;
end;

procedure runner_hashed(Sender: KLiRunner);
var
  H: KLiHashed;
  X, B, N: integer;
begin
  H := KLiHashed.Create(Sender.FEngine, 1);
  with Sender do
  begin
    N := FExprrec^.tk_prmc;
    B := FStack.Count - N;
    X := 0;
    while X < N do
    begin
      H.SetValue(value_str(FStack[B]), FStack[B + 1]);
      Inc(B, 2);
      Inc(X, 2);
    end;
    FStack.Press(N);
    FStack.Add(H, KR_HASHED);
  end;
  runner_next(Sender);
end;

procedure runner_RINR(Sender: KLiRunner);
var
  base, begx, endx: integer;
begin
  with Sender do
  begin
    base := FStackBase;
    FEngine.FTempValues.Add;
    try
      Inc(FCurrent^.next);
      begx := FCurrent^.next;
      endx := FExprrec^.VLabel^.tk_prmc;
      FStackBase := FStack.Count;
      while ExecGoonNext do
        if (FCurrent^.next < begx) or (FCurrent^.next > endx) then
          Break;
      if HasNext then
        if FStack.Count > FStackBase + 1 then
        begin
          FStack.Exchange(FStackBase, FStack.Count - 1);
          FStack.Count := FStackBase + 1;
        end
        else
        if FStack.Count = FStackBase then
          FStack.Add; 
    finally
      FEngine.FTempValues.Press;
      FStackBase := base;
    end;
  end;
end;

procedure runner_GETV(Sender: KLiRunner);
begin
  Sender.FStack.Add(Sender.FEngine.FTempValues[-1]);
  runner_next(Sender);
end;

procedure runner_SETV(Sender: KLiRunner);
begin
  lse_set_value(Sender.FEngine.FTempValues[-1], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_like(Sender: KLiRunner);
begin
  value_like(Sender.FStack[-2], Sender.FStack[-1], Sender);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_getiv(Sender: KLiRunner);
var
  data, keyr: PLseValue;
  clss: KLiType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-2];
  clss := value_type(data);
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
            curr := func.Curry(data, curr.Module);
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
  runner_next(Sender);
end;

procedure runner_setiv(Sender: KLiRunner);
var
  data, keyr: PLseValue;
  clss: KLiType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-3];
  clss := value_type(data);
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
      else lse_error('%s.setiv not supplied', [value_type(data).Name]);
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
  runner_next(Sender);
end;

procedure runner_duplast(Sender: KLiRunner);
var
  X, N: integer;
begin
  N := Sender.FExprrec^.tk_prmc;
  X := Sender.FStack.Count - N;
  while N > 0 do
  begin
    Sender.FStack.Add(Sender.FStack[X]);
    Inc(X);
    Dec(N);
  end;
  runner_next(Sender);
end;

procedure runner_fill(Sender: KLiRunner);
begin
  with Sender do
  begin
    value_fill(FStack[-2], FStack[-1], Sender);
    Sender.FStack.DeleteLast;
  end;
  runner_next(Sender);
end;

procedure runner_send(Sender: KLiRunner);
var
  V: PLseValue;
begin
  with Sender do
  begin
    V := FCurrent^.values.NewValue(FExprrec^.tk_name);
    if lse_vargen_send(FEngine.FTempValues[-1]^.VObject, V) then
      FStack.Add(1) else
      FStack.Add(0);
  end;
  runner_next(Sender);
end;

procedure runner_case(Sender: KLiRunner);
var
  L, R: PLseValue;
begin
  L := Sender.FStack[-1];
  R := Sender.FEngine.FTempValues[-1];
  value_abseq(L, R);
  runner_next(Sender);
end;

procedure runner_STMT(Sender: KLiRunner);
var
  base: integer;
begin
  with Sender do
  begin
    base := Max(FStackBase, FCurrent^.base);
    if FStack.Count > base + 1 then
    begin
      FStack.Exchange(base, FStack.Count - 1);
      FStack.Count := base + 1;
    end;
  end;
  runner_next(Sender);
end;

function token_new: PLiToken;
begin
  Result := token_clone(nil);
end;

procedure token_free(Token: PLiToken);
begin
  Token^.tk_name := '';
  lse_mem_free(Token, sizeof(RLiToken));
end;

procedure token_copy(SrcToken, DstToken: PLiToken);
var
  name: string;
  next: PLiToken;
begin
  next := DstToken^.tk_next;
  name := SrcToken^.tk_name;
  SrcToken^.tk_name := '';
  DstToken^.tk_name := '';
  Move(SrcToken^, DstToken^, sizeof(RLiToken));
  SrcToken^.tk_name := name;
  DstToken^.tk_name := name;
  DstToken^.tk_next := next;
end;

function token_clone(Token: PLiToken): PLiToken;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiToken));
  if Token <> nil then
    token_copy(Token, Result);
end;

function token_pure_ID(token: PLiToken): boolean;
begin
  Result := (token^.tk_sym = syID) and token^.VPureID;
end;

procedure token_reset(Token: PLiToken);
var
  next: PLiToken;
begin
  next := Token^.tk_next;
  try
    Token^.tk_name := '';
    FillChar(Token^, sizeof(RLiToken), 0);
  finally
    Token^.tk_next := next;
  end;
end;

function match(mp: PLiRegexpr; Source, Patten: pchar): pchar;
// pc: patten class
// ch: source char
// ss: source string
// ps: patten string
// ep: end position of patten
// lc: level char

  function class_end(ps: pchar): pchar;
  begin
    if ps^ = mp^.mp_escape then
    begin
      Inc(ps);
      if ps^ = #0 then
        raise Exception.Create('malformed pattern (ends with ''\0'')');
    end
    else
    if ps^ = '[' then
    begin
      Inc(ps);
      if ps^ = '^' then Inc(ps);
      repeat
        if ps^ = mp^.mp_escape then Inc(ps);
        if ps^ = #0 then
          raise Exception.Create('malformed pattern (missing '']'')');
        Inc(ps);
      until ps^ = ']';
    end;
    Result := ps + 1;
  end;

  { match }

  function match_escape(ch, pc: char): boolean;
  var
    cc: char;
  begin
    if pc in ['A'..'Z'] then
      cc := char(Ord(pc) - Ord('A') + Ord('a')) else
      cc := pc;

    case cc of
      'a': Result := ch in AlphaChar;
      'c': Result := ch in CntrlChar;
      'd': Result := ch in DigitChar;
      'l': Result := ch in ['a'..'z'];
      'p': Result := ch in PunctChar;
      's': Result := ch in SpaceChar;
      'u': Result := ch in ['A'..'Z'];
      'w': Result := ch in AlnumChar;
      'x': Result := ch in HexChar;
      'z': Result := (ch = #0);
      else Result := (ch = pc);
    end;

    Result := Result xor (pc <> cc);
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
      if ps^ = mp^.mp_escape then
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
    if ps^ = '.' then Result := true else // matches any char
    if ps^ = '[' then
      Result := match_bracket(ch, ps, ep - 1) else
    if ps^ = mp^.mp_escape then
      Result := match_escape(ch, (ps + 1)^) else
      Result := (ps^ = ch);
  end;

  function match_balance(var ss: pchar; ps: pchar): boolean;
  var
    b, e: char;
    cont: integer;
  begin
    if (ps^ = #0) or ((ps + 1)^ = #0) then
      raise Exception.Create('unbalanced pattern');

    if ss^ = ps^ then
    begin
      b := ps^;
      e := (ps + 1)^;
      cont := 1;
      Inc(ss);
      while ss < mp^.mp_eos do
      begin
        if ss^ = e then
        begin
          Dec(cont);
          if cont = 0 then
          begin
            Result := true;
            Inc(ss);
            Exit;
          end;
        end
        else
        if ss^ = b then Inc(cont);
        Inc(ss);
      end;
    end;

    Result := false;
  end;

  function match_capture(var ss: pchar; lc: char): boolean;
  var
    cx: integer; // capture index
    cl: integer; // capture length
    cs: pchar;   // capture string
  begin
    cx := Ord(lc) - Ord('1');
    if (cx < 0) or (cx >= mp^.mp_level)
      or (mp^.mp_captures[cx].mr_len < 0) then
        raise Exception.Create('invalid capture index');

    Result := false;

    cl := mp^.mp_captures[cx].mr_len;
    if (mp^.mp_eos - ss) >= cl then
    begin
      cs := mp^.mp_captures[cx].mr_str;
      while cl > 0 do
        if cs^ = ss^ then
        begin
          Inc(cs);
          Inc(ss);
          Dec(cl);
        end
        else Exit;
      Result := true;
    end;
  end;

  { capture }

  function start_capture(ss, ps: pchar): pchar;
  var
    cl: integer;
  begin
    cl := mp^.mp_level;
    if cl >= MaxCaptures then
      raise Exception.Create('too many captures');
    mp^.mp_captures[cl].mr_str := ss;
    mp^.mp_captures[cl].mr_len := -1;
    Inc(mp^.mp_level);
    Result := match(mp, ss, ps);
    if Result = nil then
      Dec(mp^.mp_level); // undo capture
  end;

  function close_capture(ss, ps: pchar): pchar;
  var
    cx: integer;
  begin
    cx := mp^.mp_level - 1;
    while (cx >= 0) and (mp^.mp_captures[cx].mr_len >= 0) do
      Dec(cx);
    if cx < 0 then
      raise Exception.Create('invalid pattern capture');
    mp^.mp_captures[cx].mr_len := ss - mp^.mp_captures[cx].mr_str;
    Result := match(mp, ss, ps);
    if Result = nil then
      mp^.mp_captures[cx].mr_len := -1; // undo capture
  end;

  { expand }

  function max_expand(ss, ps, ep: pchar): pchar;
  var
    i: integer;
  begin
    i := 0; // counts maximum expand for item
    while ((ss + i) < mp^.mp_eos) and match_single((ss + i)^, ps, ep) do
      Inc(i);
    // keeps trying to match with the maximum repetitions
    while i >= 0 do
    begin
      Result := match(mp, (ss + i), ep + 1);
      if Result <> nil then Exit;
      Dec(i); // else didn't match; reduce 1 repetition to try again
    end;

    Result := nil;
  end;

  function min_expand(ss, ps, ep: pchar): pchar;
  begin
    Result := match(mp, ss, ep + 1);
    while Result = nil do
      if (ss < mp^.mp_eos) and match_single(ss^, ps, ep) then
      begin
        Inc(ss); // try with one more repetition
        Result := match(mp, ss, ep + 1);
      end
      else Exit;
  end;

label INIT;
var
  ch: char;
  ep: pchar;   // end patten
  sm: boolean; // single match
begin
  INIT:

  if Patten^ = '(' then
  begin
    Result := start_capture(Source, Patten + 1);
    Exit;
  end;

  if Patten^ = ')' then
  begin
    Result := close_capture(Source, Patten + 1);
    Exit;
  end;

  if (Patten^ = #0)
    or ((Patten^ = '$') and ((Patten + 1)^ = #0) and (Source = mp^.mp_eos)) then
    begin
      Result := Source; // succeeded
      Exit;
    end;

  Result := nil;

  if Patten^ = mp^.mp_escape then // escape
  begin
    ch := (Patten + 1)^;

    if ch = 'b' then // balanced?
      if match_balance(Source, Patten + 2) then
      begin
        Inc(Patten, 4);
        goto INIT; // match(mp, Source, Patten + 4);
      end
      else Exit;

    if ch = 'f' then // frontier?
    begin
      Inc(Patten, 2);
      if Patten^ <> '[' then
        raise Exception.Create('missing ''['' after frontier in pattern');
      ep := class_end(Patten); // points to what is next
      if Source = mp^.mp_source then
        ch := #0 else
        ch := (Source - 1)^;
      if match_bracket(ch, Patten, ep - 1) or
        not match_bracket(Source^, Patten, ep - 1) then
          Exit;
      Patten := ep;
      goto INIT;  // match(mp, Source, ep);
    end;

    if ch in DigitChar then
      if match_capture(Source, (Patten + 1)^) then
      begin
        Inc(Patten, 2);
        goto INIT; // match(mp, Source, Patten + 2)
      end
      else Exit;
  end;

  ep := class_end(Patten); // seek to next

  if ep^ = '*' then
  begin
    Result := max_expand(Source, Patten, ep); // 0..max repetitions
    Exit;
  end;

  if ep^ = '-' then
  begin
    Result := min_expand(Source, Patten, ep); // 0..min repetitions
    Exit;
  end;

  sm := (Source < mp^.mp_eos) and
        match_single(Source^, Patten, ep);

  if ep^ = '?' then
  begin
    if sm then
    begin
      Result := match(mp, Source + 1, ep + 1);
      if Result <> nil then Exit;
    end;
    Patten := ep + 1;
    goto INIT; // match(mp, Source, ep + 1);
  end;

  if sm then
  begin
    if ep^ = '+' then
    begin
      Result := max_expand(Source + 1, Patten, ep); // 1..max repetitions
      Exit;
    end;
    Inc(Source);
    Patten := ep;
    goto INIT; // match(mp, Source + 1, ep);
  end;
end;

function regexpr_init(mp: PLiRegexpr; const Patten: pchar; EscapeCh: char): boolean;
begin
  Result := false;
  mp^.mp_patten := nil;
  mp^.mp_anchor := false;
  mp^.mp_escape := EscapeCh;
  if (Patten <> nil) and (Patten^ <> #0) then
    if ((Patten + 1)^ <> #0) or not (Patten^ in ['^', '$', '(', '[', EscapeCh]) then
    begin
      Result := true;
      mp^.mp_patten := Patten;
      mp^.mp_anchor := (Patten^ = '^');
    end;
end;

function regexpr_init(mp: PLiRegexpr; const Patten: pchar): boolean;
begin
  Result := regexpr_init(mp, Patten, '\');
end;

var
  zero: char = #0;
  
function regexpr_exec(mp: PLiRegexpr; S: pchar; L: integer): boolean;
var
  ps: pchar;   // patten string    
  ep: pchar;   // end position
  cx: integer; // capture index
begin
  Result := false;
  mp^.mp_result.mr_str := nil;
  mp^.mp_result.mr_len := 0;
  mp^.mp_level := 0;
  if (mp^.mp_patten <> nil) and (((L > 0) and (S <> nil)) or (L = 0)) then
  begin
    if L = 0 then S := @zero;

    mp^.mp_source := S;
    mp^.mp_eos := S + L;

    ps := mp^.mp_patten;
    if mp^.mp_anchor then
    begin
      Inc(ps);
      if (ps^ = '$') and ((ps + 1)^ = #0) then
      begin
        if L = 0 then
        begin
          mp^.mp_result.mr_str := S;
          Result := true;
        end;
        Exit;
      end;
    end;

    repeat
      ep := match(mp, S, ps);
      if ep <> nil then
      begin
        for cx := 0 to mp^.mp_level - 1 do
          if mp^.mp_captures[cx].mr_len < 0 then
            raise Exception.CreateFmt('unfinished capture: %d', [cx]);
        mp^.mp_result.mr_str := S;
        mp^.mp_result.mr_len := ep - S;
        Result := true;
      end
      else
      begin
        Inc(S);
        mp^.mp_level := 0;
      end;
    until Result or mp^.mp_anchor or (S >= mp^.mp_eos);
  end;
end;

function regexpr_exec(mp: PLiRegexpr; const S: PLseString): boolean;
begin
  Result := regexpr_exec(mp, lse_strec_data(S), lse_strec_length(S));
end;

function ID_to_sym(const ID: string; DefSymbol: KLiSymbol): KLiSymbol;
var
  X: KLiSymbol;
  L: integer;
begin
  L := Length(ID);
  if (L > 0) and ((ID[1] <> '<') or (ID[L] <> '>')) then
  begin
    for X := Low(KLiSymbol) to High(KLiSymbol) do
      if Symbols[X].ID = ID then
      begin
        Result := X;
        Exit;
      end;
  end;
  Result := DefSymbol;
end;

function reserved_words: string;
var
  sym: KLiSymbol;
  list: TStringList;
begin
  if sys_reserved_words = '' then
  begin
    list := new_named_list(false);
    try
      list.CommaText := 'this,void,int,float,string,object,variant,sys,class,main';
      for sym := FirstKeyword to LastKeyword do
        if list.IndexOf(Symbols[sym].ID) < 0 then
          list.Add(Symbols[sym].ID);
      list.Sort;
      sys_reserved_words := list.CommaText;
    finally
      list.Free;
    end;
  end;
  Result := sys_reserved_words;
end;

function query_param_engine(const Param: PLseParam): PLseEngine;cdecl;
begin
  try
    if Param <> nil then
      Result := get_engine(Param).EngineRec else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure query_engine_destroy(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Free;
  except
    { do nothing }
  end;
end;

function query_engine_compile(const Engine: pointer; const code: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileCode(code));
  except
    Result := 0;
  end;
end;

function query_engine_compile_file(const Engine: pointer; const fname: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileFile(fname));
  except
    Result := 0;
  end;
end;

function query_engine_execute(const Engine: pointer; const code: pchar): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    E := KLiEngine(Engine);
    E.PrepareCompile;
    Result := Ord(E.TryExecuteCode(code));
  except
    Result := 0;
  end;
end;

function query_engine_execute_file(const Engine: pointer; const fname: pchar): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    E := KLiEngine(Engine);
    E.PrepareCompile;
    Result := Ord(E.TryExecuteFile(fname));
  except
    Result := 0;
  end;
end;

procedure query_engine_terminate(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Terminate;
  except
    { do nothing }
  end;
end;

procedure query_engine_clear(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Clear;
  except
    { do nothing }
  end;
end;

function query_engine_get_args(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_strec_alloc(KLiEngine(Engine).Arguments.Text);
  except
    Result := nil;
  end;
end;

procedure query_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Arguments.Text := Args;
  except
    { do nothing }
  end;
end;

function query_engine_errno(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.errno;
  except
    Result := -1;
  end;
end;

function query_engine_error_row(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Row;
  except
    Result := -1;
  end;
end;

function query_engine_error_col(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Col;
  except
    Result := -1;
  end;
end;

function query_engine_error_name(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Name);
  except
    Result := nil;
  end;
end;

function query_engine_error_msg(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Msg);
  except
    Result := nil;
  end;
end;

function query_engine_error_module(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Module);
  except
    Result := nil;
  end;
end;

function query_engine_error_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.ModuleFile);
  except
    Result := nil;
  end;
end;

function query_engine_result_type(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultType);
  except
    Result := nil;
  end;
end;

function query_engine_result_text(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultText);
  except
    Result := nil;
  end;
end;

function query_engine_get_search_path(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainSearchPath);
  except
    Result := nil;
  end;
end;

procedure query_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainSearchPath := Trim(Path);
  except
    { do nothing }
  end;
end;

function query_engine_get_main_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainFile);
  except
    Result := nil;
  end;
end;

procedure query_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainFile := fname;
  except
    { do nothing }
  end;
end;

function query_engine_ready(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Ready);
  except
    Result := 0;
  end;
end;

function query_engine_running(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Running);
  except
    Result := 0;
  end;
end;

function query_engine_terminated(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Terminated);
  except
    Result := 0;
  end;
end;

function query_engine_exited(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Exited);
  except
    Result := 0;
  end;
end;

procedure query_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
begin
  try
    lse_stream_write(KLiEngine(Engine).Output, Text, Count);
  except
    { do nothing }
  end;
end;

function query_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
begin
  try
    Result := lse_stream_read(KLiEngine(Engine).Input, Buf, Count);
  except
    Result := 0;
  end;
end;

function query_engine_readln(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_stream_readln(KLiEngine(Engine).Input);
  except
    Result := nil;
  end;
end;

function query_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
begin
  try
    Result := KLiEngine.Create(EngineRec);
  except
    Result := nil;
  end;
end;

function query_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
begin
  try
    if Param <> nil then
      Result := lse_strec_alloc(get_runner(Param).FormatFor(Fmt, nil)) else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure query_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    set_error(Param, ID, Errno, Msg);
  except
    { do nothing }
  end;
end;

function query_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := get_runner(Param);
    Result := rnnr.Stack.Count;
    rnnr.Stack.Add(Value);
  except
    Result := -1;
  end;
end;

function query_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := get_runner(Param);
    Result := Ord(rnnr.Goon(KLiFunc(Func), Params, ResValue));
  except
    Result := 0;
  end;
end;

function query_register_module(const Name: pchar; const initrec: PLseModule): pointer;cdecl;
begin
  try
    Result := module_register(Name, initrec);
  except
    Result := nil;
  end;
end;

function query_register_type(const CR: PLseType): PLseType;cdecl;
begin
  Result := nil;
  if CR <> nil then
  begin
    CR^.cr_class := sys_module.SetupType(CR);
    if CR^.cr_class <> nil then
      Result := KLiType(CR^.cr_class).TypeRec;
  end;
end;

function query_typerec(const KernelType: pointer): PLseType;cdecl;
begin
  if KernelType <> nil then
    Result := KLiType(KernelType).TypeRec else
    Result := nil;
end;

function query_register_func(const FR: PLseFunc): pointer;cdecl;
begin
  try
    Result := sys_module.SetupFunc(FR);
  except
    Result := nil;
  end;
end;

procedure query_casto_string(const V: PLseValue);cdecl;
begin
  KT_STRING.Cast(V, nil);
end;

function query_simple_test(const Script: pchar): integer;cdecl;
var
  parser: KLiTokenizer;
  pairs: array[0..255] of KLiSymbol;
  count, mask: integer;
  last: KLiSymbol;
  token: PLiToken;
  isstr: boolean;

  procedure begin_pair(begin_sym: KLiSymbol);
  begin
    if count < Length(pairs) then
    begin
      pairs[count] := begin_sym;
      Inc(count);
    end
    else mask := SCT_ERROR;
  end;

  procedure close_pair(begin_sym: KLiSymbol);
  begin
    if (count > 0) and (pairs[count - 1] = begin_sym) then
      Dec(count) else
      mask := SCT_ERROR;
  end;

  function get_next_token: PLiToken;
  begin
    repeat Result := parser.GetNextToken(isstr);
    until (Result <> nil) or not parser.GetChar;
  end;

begin
  try
    parser := KLiTokenizer.Create(Script);
    try
      parser.FEOF := true; // hide syEOF
      last := syError;
      mask := SCT_OK;
      count := 0;
      token := get_next_token;
      while (token <> nil) and (mask = SCT_OK) do
      begin
        last := token^.tk_sym;
        case last of
          syLBlock: begin_pair(syLBlock);
          syRBlock: close_pair(syLBlock);
          syLParen: begin_pair(syLParen);
          syRParen: close_pair(syLParen);
          syLArray: begin_pair(syLArray);
          syRArray: close_pair(syLArray);
          syError : if isstr then
                      mask := SCT_UNFINISHED else
                      mask := SCT_ERROR;
        end;
        token := get_next_token;
      end;
      if (mask = SCT_OK) and (count > 0) then
        mask := SCT_UNFINISHED;
      if last = syRBlock then
        Inc(mask, SCT_RBLOCK);
      Result := mask;
    finally
      parser.Free;
    end;
  except
    Result := SCT_ERROR;
  end;
end;

function query_startup: integer;cdecl;
begin
  try
    kernel_startup;
    Result := 1;
  except
    Result := 0;
  end;
end;

procedure query_cleanup;cdecl;
begin
  { do nothing }
end;

function query_keywords: pchar;cdecl;
begin
  reserved_words;
  Result := pchar(sys_reserved_words);
end;

function query_get_kernel_file: pchar;cdecl;
begin
  Result := pchar(sys_kernel);
end;

procedure query_set_kernel_file(const KernelFile: pchar);cdecl;
begin
  try
    sys_kernel := lse_expand_fname(KernelFile);
    kernel_load_config_file('');
  except
    { do nothing }
  end;
end;

function query_get_program_file: pchar;cdecl;
begin
  Result := pchar(sys_program);
end;

procedure query_set_program_file(const ProgramFile: pchar);cdecl;
begin
  sys_program := lse_expand_fname(ProgramFile);
end;

procedure query_load_config(const ConfigFile: pchar);cdecl;
begin
  try
    kernel_load_config_file(lse_expand_fname(ConfigFile));
  except
    { do nothing }
  end;
end;

function query_production: pchar;cdecl;
begin
  Result := LSE_ID;
end;

function query_version: pchar;cdecl;
begin
  Result := pchar(sys_version);
end;

function query_copyright: pchar;cdecl;
begin
  Result := LSE_COPYRIGHT;
end;

function query_tmpath: pchar;cdecl;
begin
  Result := pchar(sys_tmpath);
end;

function query_entry(const ID: pchar): pointer;cdecl;
var
  name: string;
begin
  name := LowerCase(ID);
  if name = 'cik_entries' then
    Result := @sys_cik_entries else
    Result := nil;
end;

function type_error_otos(obj: pointer): PLseString;cdecl;
begin
  if obj <> nil then
    Result := lse_strec_alloc(KLiError(obj).ErrorText) else
    Result := nil;
end;

function type_function_otos(obj: pointer): PLseString;cdecl;
var
  func: KLiFunc;
begin
  func := KLiFunc(obj);
  if func <> nil then
    Result := lse_strec_alloc(KLiFunc(obj).Prototype) else
    Result := nil;
end;

function type_module_otos(obj: pointer): PLseString;cdecl;
begin
  if obj <> nil then
    Result := lse_strec_alloc(KLiModule(obj).Name) else
    Result := nil;
end;

function type_type_otos(obj: pointer): PLseString;cdecl;
var
  clss: KLiType;
begin
  clss := KLiType(obj);
  if clss <> nil then
    Result := lse_strec_alloc(clss.FullName) else
    Result := nil;
end;

function type_varlist_otos(obj: pointer): PLseString;cdecl;
begin
  if obj <> nil then
    Result := lse_strec_alloc(KLiVarList(obj).AsString) else
    Result := nil;
end;

{ STOO: string to object}

function type_module_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
begin
  Result := KLiEngine(KernelEngine).MainRunner.CurrentFunc.Module.
            FindModule(lse_strec_string(S), false);
end;

function type_type_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  o_name, m_name, T: string;
  module: KLiModule;
begin
  module := KLiEngine(KernelEngine).MainRunner.CurrentFunc.Module;
  T := lse_strec_string(S);
  o_name := extract_name_module(T, m_name);
  if (m_name <> '') or (System.Pos('::', T) > 0) then
  begin
    if m_name <> '' then
      module := module.FindModule(m_name, false); 
    Result := module.FindType(o_name);
  end
  else Result := module.FindTypeBy(o_name, '');
end;

function type_varlist_stoo(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  list: KLiVarList;
  strs: TStrings;
  index: integer;
begin
  list := KLiVarList.Create(KLiEngine(KernelEngine));
  strs := TStringList.Create;
  try
    strs.Text := lse_strec_string(S);
    for index := 0 to strs.Count - 1 do
      list.Add(strs[index]);
  finally
    strs.Free;
  end;
  Result := list;
end;

function type_stream_writo(obj: pointer; stream: PLseStream): integer;cdecl;
begin
  if (obj <> nil) and (stream <> nil) and (obj <> stream) then
    Result := lse_stream_fill(stream, PLseStream(obj)) else
    Result := 0;
end;

{ CVGR: create RLseVargen record}

type
  RLiVG_varlist = packed record
    vgrec: RLseVargen;
    vgref: integer;
    list : KLiVarList;
    index: integer;
  end;
  PLiVG_varlist = ^RLiVG_varlist;
  
function type_vargen_varlist_rewind(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    index := 0;
    Result := Ord(index < list.Count);
  end;
end;

function type_vargen_varlist_hasNext(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
    Result := Ord(index < list.Count);
end;

function type_vargen_varlist_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data);
  with cvgr^ do
    if index < list.Count then
    begin
      lse_set_value(Value, list[index]);
      Result := 1;
      Inc(index);
    end
    else Result := 0;
end;

function type_vargen_varlist_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.IncRefcount;
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_varlist));
  end;
end;

function type_vargen_varlist_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.DecRefcount;
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_varlist));
  end;
end;

function type_vargen_varlist_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := Ord(value_in(Value, PLiVG_varlist(vrec^.vg_data)^.list, false));
end;

function type_varlist_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  list: KLiVarList;
  cvgr: PLiVG_varlist;
begin
  list := KLiVarList(obj);
  if list <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_varlist));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_varlist_rewind;
    cvgr^.vgrec.vg_has_next := @type_vargen_varlist_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_varlist_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_varlist_addref;
    cvgr^.vgrec.vg_release := @type_vargen_varlist_release;
    cvgr^.vgrec.vg_contains := @type_vargen_varlist_contains;
    cvgr^.vgref := 0;
    cvgr^.list := list;
    cvgr^.index := 0;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_func = packed record
    vgrec: RLseVargen;
    vgref: integer;
    func: KLiFunc;
  end;
  PLiVG_func = ^RLiVG_func;

function type_vargen_func_rewind(vrec: PLseVargen): integer;cdecl;
begin
  Result := 1;
end;

function type_vargen_func_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  Result := type_vargen_func_rewind(vrec);
end;

function type_vargen_func_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  func: KLiFunc;
begin
  if type_vargen_func_hasNext(vrec) <> 0 then
  begin
    func := PLiVG_func(vrec^.vg_data)^.func;
    KLiEngine(vrec^.vg_engine).MainRunner.Goon(func, 0, value);
    Result := 1;
  end
  else Result := 0;
end;

function type_vargen_func_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_func;
begin
  cvgr := PLiVG_func(vrec^.vg_data); 
  with cvgr^ do
  begin
    func.IncRefcount;
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_func));
  end;
end;

function type_vargen_func_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_func;
begin
  cvgr := PLiVG_func(vrec^.vg_data); 
  with cvgr^ do
  begin
    func.DecRefcount;
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_func));
  end;
end;

function type_function_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  func: KLiFunc;
  cvgr: PLiVG_func;
begin
  Result := nil;
  func := KLiFunc(obj);
  if func <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_func));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_func_rewind; 
    cvgr^.vgrec.vg_has_next := @type_vargen_func_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_func_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_func_addref;
    cvgr^.vgrec.vg_release := @type_vargen_func_release;
    cvgr^.vgref := 0;
    cvgr^.func := func;
    Result := @(cvgr^.vgrec);
  end;
end;

type
  RLiVG_stream = packed record
    vgrec: RLseVargen;
    vgref: integer;
    stream: PLseStream;
  end;
  PLiVG_stream = ^RLiVG_stream;

function type_vargen_stream_rewind(vrec: PLseVargen): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  lse_stream_seek(stream, 0);
  Result := Ord(not lse_stream_eof(stream));
end;

function type_vargen_stream_hasNext(vrec: PLseVargen): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  Result := Ord(not lse_stream_eof(stream));
end;

function type_vargen_stream_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  stream: PLseStream;
  ch: char;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  if not lse_stream_eof(stream) then
  begin
    Result := lse_stream_read(stream, @ch, sizeof(char));
    if Result > 0 then
      lse_set_char(Value, ch);
  end
  else Result := 0;
end;

function type_vargen_stream_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_stream;
begin
  cvgr := PLiVG_stream(vrec^.vg_data); 
  with cvgr^ do
  begin
    stream^.s_addref(stream);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_stream));
  end;
end;

function type_vargen_stream_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_stream;
begin
  cvgr := PLiVG_stream(vrec^.vg_data); 
  with cvgr^ do
  begin
    stream^.s_release(stream);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_stream));
  end;
end;

function type_stream_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  stream: PLseStream;
  cvgr: PLiVG_stream;
begin
  stream := PLseStream(obj);
  if stream <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_stream));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_stream_rewind; 
    cvgr^.vgrec.vg_has_next := @type_vargen_stream_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_stream_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_stream_addref;
    cvgr^.vgrec.vg_release := @type_vargen_stream_release;
    cvgr^.vgref := 0;
    cvgr^.stream := stream;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

function type_vargen_stream_getLine(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  if not lse_stream_eof(stream) then
  begin
    lse_set_string(Value, lse_stream_readln(stream));
    Result := 1;
  end
  else Result := 0;
end;

function type_stream_vargen_lines(obj, kernel_engine: pointer): PLseVargen;cdecl;
begin
  Result := type_stream_vargen(obj, kernel_engine);
  Result^.vg_get_next := @type_vargen_stream_getLine;
end;

type
  RLiVG_string = packed record
    vgrec: RLseVargen;
    vgref: integer;
    srec: PLseString;
    slen: integer;
    index: integer;
  end;
  PLiVG_string = ^RLiVG_string;

function type_vargen_string_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
  begin
    index := 0;
    Result := Ord(index < slen);
  end;
end;

function type_vargen_string_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
    Result := Ord(index < slen);
end;

function type_vargen_string_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
    if index < slen then
    begin
      lse_set_string(value, lse_strec_data(srec)[index]);
      Inc(index);
      Result := 1;
    end
    else Result := 0;
end;

function type_vargen_string_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_string;
begin
  cvgr := PLiVG_string(vrec^.vg_data); 
  with cvgr^ do
  begin
    lse_strec_inclife(srec);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_string));
  end;
end;

function type_vargen_string_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_string;
begin
  cvgr := PLiVG_string(vrec^.vg_data);
  with cvgr^ do
  begin
    lse_strec_declife(srec);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_string));
  end;
end;

function type_vargen_string_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := Ord(value_in(Value, PLiVG_string(vrec^.vg_data)^.srec));
end;

function type_string_vargen(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  srec: PLseString;
  slen: integer;
  cvgr: PLiVG_string;
begin
  srec := PLseString(obj);
  slen := lse_strec_length(srec);
  if slen > 0 then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_string));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_string_rewind; 
    cvgr^.vgrec.vg_has_next := @type_vargen_string_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_string_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_string_addref;
    cvgr^.vgrec.vg_release := @type_vargen_string_release;
    cvgr^.vgrec.vg_contains := @type_vargen_string_contains;
    cvgr^.vgref := 0;
    cvgr^.srec := srec;
    cvgr^.slen := slen;
    cvgr^.index := 0;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_range = packed record
    vgrec: RLseVargen;
    vgref: integer;
    begv, endv, step, curr: int64;
  end;
  PLiVG_range = ^RLiVG_range;

function type_vargen_upto_rewind(vrec: PLseVargen): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
  begin
    curr := begv;
    Result := Ord(curr <= endv);
  end;
end;

function type_vargen_upto_hasNext(vrec: PLseVargen): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
    Result := Ord(curr <= endv);
end;

function type_vargen_upto_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
    if curr <= endv then
    begin
      lse_set_int64(Value, curr);
      Inc(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function type_vargen_range_addref(vrec: PLseVargen): integer;cdecl;
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

function type_vargen_range_release(vrec: PLseVargen): integer;cdecl;
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

function type_vargen_range_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;

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

function type_vargen_upto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
var
  cvgr: PLiVG_range;
begin
  if (begv <= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_upto_rewind; 
    cvgr^.vgrec.vg_has_next := @type_vargen_upto_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_upto_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_range_addref;
    cvgr^.vgrec.vg_release := @type_vargen_range_release;
    cvgr^.vgrec.vg_contains := @type_vargen_range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

function type_vargen_downto_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
  begin
    curr := begv;
    Result := Ord(curr >= endv);
  end;
end;

function type_vargen_downto_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    Result := Ord(curr >= endv);
end;

function type_vargen_downto_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    if curr >= endv then
    begin
      lse_set_int64(Value, curr);
      Dec(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function type_vargen_downto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
var
  cvgr: PLiVG_range;
begin
  if (begv >= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @type_vargen_downto_rewind; 
    cvgr^.vgrec.vg_has_next := @type_vargen_downto_hasNext;
    cvgr^.vgrec.vg_get_next := @type_vargen_downto_getNext;
    cvgr^.vgrec.vg_addref := @type_vargen_range_addref;
    cvgr^.vgrec.vg_release := @type_vargen_range_release;
    cvgr^.vgrec.vg_contains := @type_vargen_range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

{ ADDI}

function type_varlist_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
begin
  Result := KLiVarList(obj).Count;
  KLiVarList(obj).Add(Value);
end;

function type_stream_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  S: PLseStream;
begin
  S := PLseStream(obj);
  if lse_vtype(Value) = LSV_STRING then
    Result := lse_stream_write(S, Value^.VObject) else
    Result := lse_stream_write(S, value_str(Value));
end;

function type_function_add(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  F: KLiFunc;
  R: KLiRunner;
  B: integer;
begin
  F := KLiFunc(obj);
  R := KLiEngine(Engine).MainRunner;
  B := R.Stack.Count;
  try
    R.Stack.Add(Value);
    Result := Ord(R.Goon(F, 1, nil));
  finally
    R.Stack.Count := B;
  end;
end;

{ GETIV: get item value }

function type_string_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  S: PLseString;
  L, X: int64;
begin
  S := PLseString(obj);
  L := lse_strec_length(S);
  X := lse_vary_index(index, L);
  if (X >= 0) and (X < L) then
  begin
    lse_set_string(value, lse_strec_data(S)[X]);
    Result := 1;
  end
  else Result := 0;
end;

function type_varlist_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  L: KLiVarList;
begin
  Result := 0;
  if obj <> nil then
  begin
    L := KLiVarList(obj);
    index := lse_vary_index(index, L.Count);
    if (index >= 0) and (index < L.Count) then
    begin
      lse_set_value(value, L[index]);
      Result := 1;
    end;
  end;
end;

function type_varsnap_getiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    lse_set_value(value, KLiVarSnap(obj)[index]);
    Result := 1;
  end;
end;

{ SETIV: set item value }

function type_varlist_setiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  L: KLiVarList;
begin
  Result := 0;
  if obj <> nil then
  begin
    L := KLiVarList(obj);
    index := lse_vary_index(index, L.Count);
    if (index >= 0) and (index < L.Count) then
    begin
      lse_set_value(L[index], value);
      Result := 1;
    end;
  end;
end;

function type_varsnap_setiv(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    lse_set_value(KLiVarSnap(obj)[index], value);
    Result := 1;
  end;
end;

{ GETPV: get property value }

function type_hashed_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  V: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    V := KLiHashed(obj).FindValue(name);
    if V <> nil then
    begin
      lse_set_value(value, V);
      Result := 1;
    end;
  end;
end;

function type_module_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  this: KLiModule;
  func: KLiFunc;
  clss: KLiType;
  engi: KLiEngine;
begin
  Result := 0;
  if obj <> nil then
  begin
    this := KLiModule(obj);
    engi := KLiEngine(engine);

    if this = sys_module then
    begin
      Result := 1;
      if name = 'input' then
        lse_set_stream(value, engi.Input) else
      if name = 'output' then
        lse_set_stream(value, engi.Output) else
      if name = 'errput' then
        lse_set_stream(value, engi.Errput) else       
      if name = 'current' then
        lse_set_object(value, KR_VARSNAP, engi.MainRunner.Current^.values) else
        Result := 0;
      if Result > 0 then Exit;
    end;

    clss := this.FindType(name);
    if clss <> nil then
    begin
      Result := 1;
      clss.SaveTo(value);
      Exit;
    end;

    func := this.FindFunc(name);
    if func <> nil then
    begin
      Result := 1;
      if func.IsNameCall then
        engi.MainRunner.Goon(func, 0, value) else
        func.SaveTo(value);
      Exit;
    end;
  end;
end;

function type_varsnap_getpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  snap: KLiVarSnap;
  data: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    snap := KLiVarSnap(obj);
    data := snap.GetValue(name);
    if data <> nil then
    begin
      lse_set_value(value, data);
      Result := 1;
    end;
  end;
end;

{ SETPV: set property value }

function type_hashed_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    KLiHashed(obj).SetValue(name, value);
    Result := 1;
  end;
end;

function type_module_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  this: KLiModule;
  engi: KLiEngine;

  function get_stream: PLseStream;
  begin
    if value_type(value) = KT_STREAM then
      Result := PLseStream(value^.VObject) else
      Result := nil;
  end;
  
begin
  Result := 0;
  if obj <> nil then
  begin
    this := KLiModule(obj);
    if this = sys_module then
    begin
      Result := 1;
      engi := KLiEngine(engine);
      if name = 'input' then
        engi.Input := get_stream else
      if name = 'output' then
        engi.Output := get_stream else
      if name = 'errput' then
        engi.Errput := get_stream else
        Result := 0;
    end;
  end;
end;

function type_varsnap_setpv(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    KLiVarSnap(obj).SetValue(name, value);
    Result := 1;
  end;
end;

{ LENO: length object }

function type_string_length(obj: pointer): integer;cdecl;
begin
  Result := lse_strec_length(PLseString(obj));
end;

function type_hashed_length(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiHashed(obj).ItemCount else
    Result := 0;
end;

function type_stream_length(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := PLseStream(obj)^.s_get_size(obj) else
    Result := 0;
end;

function type_varlist_length(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiVarList(obj).Count else
    Result := 0;
end;

function type_varsnap_length(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiVarSnap(obj).Count else
    Result := 0;
end;

{ sys }

procedure call_dir(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, GetCurrentDir);
end;

procedure call_isdir(const Param: PLseParam);cdecl;
var
  dir: string;
begin
  dir := value_fname(Param^.p_param[0]);
  lse_set_bool(Param^.p_result, DirectoryExists(dir));
end;

procedure call_isfile(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := value_fname(Param^.p_param[0]);
  lse_set_bool(Param^.p_result, is_file(fname));
end;

procedure call_modules(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := get_engine(Param);
  eng.Modules.ToVarlist(eng).SaveTo(Param^.p_result);
end;

procedure call_libs(const Param: PLseParam);cdecl;
var
  list: KLiVarList;
  index: integer;
begin
  list := KLiVarList.Create(get_engine(Param));
  list.SaveTo(Param^.p_result);
  for index := 0 to sys_libraries.Count - 1 do
    list.Add(sys_libraries.Objects[index], KR_MODULE);
end;

procedure call_exit(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := get_engine(Param);
  engine_lock(eng);
  try
    eng.SetResult(Param^.p_param[0]);
    eng.Exited := true;
    eng.Error.Clear;
    eng.Terminate;
  finally
    engine_unlock(eng);
  end;
end;

procedure call_print(const Param: PLseParam);cdecl;
begin
  lse_stream_write(get_engine(Param).Output, Param^.p_param[0]^.VObject);
end;

// void sys::printf(string fileName)
procedure call_printf(const Param: PLseParam);cdecl;
var
  inf: TFileStream;
  buf: array[0..1023] of char;
  len: integer;
  eng: KLiEngine;
begin
  if Param^.p_count > 0 then
  begin
    inf := TFileStream.Create(value_fname(Param^.p_param[0]), fmShareDenyWrite);
    try
      eng := get_engine(Param);
      len := inf.Read(buf, sizeof(buf));
      while len > 0 do
      begin
        lse_stream_write(eng.Output, buf, len);
        len := inf.Read(buf, sizeof(buf));
      end;
    finally
      inf.Free;
    end;
  end;
end;

procedure call_println(const Param: PLseParam);cdecl;
var
  stdout: PLseStream;
begin
  stdout := get_engine(Param).Output;
  lse_stream_write(stdout, Param^.p_param[0]^.VObject);
  lse_stream_writeln(stdout);
end;

procedure call_readln(const Param: PLseParam);cdecl;
var
  E: KLiEngine;
  S: PLseString;
begin
  E := get_engine(Param); 
  S := lse_stream_readln(E.Input);
  lse_set_string(Param^.p_result, S);
end;

var
  random_inited: boolean = false;

procedure call_random(const Param: PLseParam);cdecl;
var
  F, T: integer;
begin
  if not random_inited then
  begin
    Random(MaxInt);
    random_inited := true;
  end;
  F := value_int(Param^.p_param[0]);
  T := value_int(Param^.p_param[1]);
  if F <> T then
    F := Random(Abs(F - T)) + Min(F, T) else
  if F = 0 then
    F := Random(MaxInt);
  lse_set_int64(Param^.p_result, F);
end;

procedure call_sleep(const Param: PLseParam);cdecl;
var
  timeout: integer;
begin
  timeout := value_int(Param^.p_param[0]);
  if timeout > 0 then
    Sleep(timeout);
end;

procedure call_getenv(const Param: PLseParam);cdecl;
var
  ID: string;
begin
  ID := value_str(Param^.p_param[0]);
  if ID <> '' then
    lse_set_string(Param^.p_result, lse_getenv(ID)) else
    lse_set_string(Param^.p_result, '', 0);
end;

procedure call_dumpc(const Param: PLseParam);cdecl;
var
  stream: TStringStream;
  clss: KLiType;
  func: KLiFunc;
  list: TStrings;
  module: KLiModule;
begin
  if Param^.p_count = 0 then
  begin
    stream := TStringStream.Create('');
    try
      get_engine(Param).DumpCodeToStream(stream, '');
      lse_set_string(Param^.p_result, stream.DataString);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    clss := value_type(Param^.p_param[0]);
    if clss = KT_FUNC then
    begin
      func := KLiFunc(value_obj(Param^.p_param[0]));
      if func <> nil then
      begin
        list := TStringList.Create;
        try
          func.DumpCode(list, '');
          lse_set_string(Param^.p_result, list.Text);
        finally
          list.Free;
        end;
      end
    end
    else
    if clss = KT_MODULE then
    begin
      module := KLiModule(value_obj(Param^.p_param[0]));
      if module <> nil then
      begin
        stream := TStringStream.Create('');
        try
          module.DumpCodeToStream(stream, '');
          lse_set_string(Param^.p_result, stream.DataString);
        finally
          stream.Free;
        end;
      end;
    end;
  end;
end;

procedure call_length(const Param: PLseParam);cdecl;
begin
  lse_set_integer(Param^.p_result, lse_length(Param^.p_param[0]));
end;

procedure call_genid(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, genid);
end;

procedure call_load(const Param: PLseParam);cdecl;
var
  engine: KLiEngine;
  runner: KLiRunner;
  module: KLiModule;
  f_name: string;
  m_name: string;
  is_lib, is_str: boolean;

  procedure roll_back;
  var
    X: integer;
    A: TLseObject;
  begin
    try
      X := engine.CompiledObjects.Count - 1;
      while X >= 0 do
      begin
        A := TLseObject(engine.CompiledObjects[X]);
        engine.CompiledObjects.Delete(X);
        A.Free;
        X := engine.CompiledObjects.Count - 1;
      end;
    finally
      engine.CompiledObjects.Free;
      engine.CompiledObjects := nil;
    end;
  end;

begin
  runner := get_runner(Param);
  engine := runner.Engine;

  is_lib := false;

  m_name := value_fname(Param^.p_param[0]);
  is_str := not in_charset(pchar(m_name), IDChar);
  if is_str then
  begin
    f_name := kernel_expand_value(m_name, engine);
    f_name := absolute_fname(f_name, ExtractFilePath(runner.CurrentModule.FileName));
    if not FileExists(f_name) then
      lse_error('module file "%s" not exists', [m_name]);
    m_name := ExtractFileExt(f_name);
    is_lib := AnsiSameText(m_name, LSE_DLLEXT);
    m_name := ChangeFileExt(ExtractFileName(f_name), '');
  end;

  module := engine.Modules.Find(m_name);
  if module = nil then
    module := KLiModule(find_named(sys_libraries, m_name));

  if module <> nil then
    if not is_str or same_fname(f_name, module.FileName) then
    begin
      lse_set_object(Param^.p_result, KR_MODULE, module);
      Exit;
    end
    else lse_error('reload module %s from another file', [m_name]);

  if not is_str then
  begin
    f_name := m_name;
    if not module_search(f_name, engine.GetSearchPath, is_lib) then
      lse_error('module %s not found', [m_name]);
  end;

  kernel_lock;
  try
    if is_lib then
    begin
      module := module_load(m_name, f_name);
      if module = nil then
        lse_error('can not load file "%s"', [f_name]);
    end
    else
    begin
      engine.CompiledObjects := TList.Create;
      try
        module := KLiModule.Create(m_name, engine, moyScript);
        module.FileName := f_name;
        module.Parsing := true;
        KLiParser.Create(module).ParseAndFree(file_text(f_name));
        engine.CompiledObjects.Free;
        engine.CompiledObjects := nil;
      except
        roll_back;
        raise;
      end;
    end;
  finally
    kernel_unlock;
  end;

  if module.MainFunc <> nil then
    runner.Goon(module.MainFunc, 0, nil);
  lse_set_object(Param^.p_result, KR_MODULE, module);
end;

procedure call_parse(const Param: PLseParam);cdecl;
var
  code: string;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 0 then
  begin
    rnnr := get_runner(Param);
    try
      code := value_str(Param^.p_param[0]);
      rnnr.Engine.DoCompile(code).SaveTo(Param^.p_result);
    except
      if rnnr.Engine.Error.errno <> 0 then
      begin
        code := rnnr.Engine.Error.ErrorText;
        rnnr.Excepted := false;
        lse_error(code);
      end;
      raise;
    end;
  end;
end;

procedure call_eval(const Param: PLseParam);cdecl;
var
  code: string;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 0 then
  begin
    code := TrimRight(value_str(Param^.p_param[0]));
    if code <> '' then
    begin
      code := code + sLineBreak + ';';
      rnnr := get_runner(Param);
      rnnr.Eval(code, Param^.p_result);
      if rnnr.Excepted then
      begin
        code := rnnr.Engine.Error.ErrorText;
        rnnr.Excepted := false;
        lse_error(code);
      end;
    end;
  end;
end;

procedure call_format(const Param: PLseParam);cdecl;
var
  frmt: string;
  args: KLiVarList;
begin
  frmt := value_str(Param^.p_param[0]);
  args := KLiVarList(value_obj(Param^.p_param[1]));
  lse_set_string(Param^.p_result, get_runner(Param).FormatFor(frmt, args));
end;

procedure call_now(const Param: PLseParam);cdecl;
begin
//lse_set_time(Param^.result, Now);
end;

procedure call_max(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or value_match(V1, V2, [crEqual, crMore]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure call_min(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or value_match(V1, V2, [crEqual, crLess]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure call_leap(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.p_result, IsLeapYear(value_int(Param^.p_param[0])));
end;

procedure call_which(const Param: PLseParam);cdecl;
var
  v_name: string;
  rec: RLiFind;
begin
  v_name := Trim(value_str(Param^.p_param[0]));
  if get_runner(Param).CurrentFunc.FindBy(v_name, @rec) then
    case rec.fo_type of
      foFunc  : rec.VFunc.SaveTo(Param^.p_result);
      foType  : rec.VType.SaveTo(Param^.p_result);
      foModule: rec.VModule.SaveTo(Param^.p_result)
    end;
end;

procedure call_curry(const Param: PLseParam);cdecl;
var
  func, curr: KLiFunc;
  list: KLiVarList;
begin
  func := value_func(Param^.p_param[0]);
  if (func <> nil) and (func.ParamCount > 0) then
  begin
    list := value_varlist(Param^.p_param[1]);
    if (list <> nil) and (list.Count > 0) then
    begin
      curr := func.Curry(list, get_runner(Param).CurrentModule);
      curr.SaveTo(Param^.p_result);
      if curr <> func then
        curr.DecRefcount; // adjust refcount
    end
    else func.SaveTo(Param^.p_result);
  end
  else func.SaveTo(Param^.p_result);
end;

procedure call_curryone(const Param: PLseParam);cdecl;
var
  func, curr: KLiFunc;
begin
  func := value_func(Param^.p_param[0]);
  if (func <> nil) and (func.ParamCount > 0) and (Param^.p_count > 1) then
  begin
    curr := func.Curry(Param^.p_param[1], get_runner(Param).CurrentModule); 
    curr.SaveTo(Param^.p_result);
    if curr <> func then
      curr.DecRefcount; // adjust refcount
  end
  else func.SaveTo(Param^.p_result);
end;

procedure call_gc(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, get_engine(Param).GarbageCollect);
end;

procedure call_apply(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
begin
  func := value_func(Param^.p_param[0]);
  if func <> nil then
  begin
    rnnr := get_runner(Param);
    list := value_varlist(Param^.p_param[1]);
    if list <> nil then
      prms := Min(func.ParamCount, list.Count) else
      prms := 0;
    rnnr.Stack.AddFrom(list, prms);
    rnnr.Goon(func, prms, Param^.p_result);
  end;
end;

procedure call_tmpfname(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := sys_tmpath + genid + value_fname(Param^.p_param[0]);
  lse_set_string(Param^.p_result, fname);
end;

procedure call_encodeGMT(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_encode_GMT(value_time(Param^.p_param[0])));
end;

procedure call_decodeGMT(const Param: PLseParam);cdecl;
begin
//lse_set_time(Param^.result, lse_decode_GMT(value_str(Param^.param[0])));
end;

procedure call_encodeUTF8(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, encode_UTF8(value_str(Param^.p_param[0])));
end;

procedure call_decodeUTF8(const Param: PLseParam);cdecl;
begin
  if Param^.p_count > 0 then
    lse_set_string(Param^.p_result, decode_UTF8(value_str(Param^.p_param[0])));
end;

const
  EOPENMODE = 'Unknown file open mode "%s"';

procedure call_openfs(const Param: PLseParam);cdecl;
var
  fname, fmode: string;
  open_mode: word;
  read, write: boolean;
  stream: PLseStream;
begin
  fname := value_fname(Param^.p_param[0]);
  fmode := value_str(Param^.p_param[1]);
  if fmode = '' then fmode := 'r';
  if get_open_file_mode(fmode, open_mode, read, write) then
  begin
    stream := lse_file_stream(fname, open_mode);
    lse_set_stream(Param^.p_result, stream);
  end
  else set_error(Param, EOPENMODE, [fmode]);
end;

procedure call_memory(const Param: PLseParam);cdecl;
var
  stream: PLseStream;
begin
  stream := lse_memory_stream;
  lse_set_stream(Param^.p_result, stream);
  if Param^.p_count > 1 then
    lse_stream_resize(stream, Max(0, Param^.p_param[0]^.VInteger));
end;

procedure call_incPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, IncludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure call_excPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, ExcludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure call_veryPD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryPD(value_str(Param^.p_param[0])));
end;

procedure call_veryUD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryUD(value_str(Param^.p_param[0])));
end;

procedure call_msecs(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
  beg_time: TDateTime;
begin
  func := value_func(Param^.p_param[0]);
  if func <> nil then
  begin
    beg_time := Now;
    rnnr := get_runner(Param);
    list := KLiVarList(value_obj(Param^.p_param[1]));
    if list <> nil then
      prms := Min(func.ParamCount, list.Count) else
      prms := 0;
    rnnr.Stack.AddFrom(list, prms);
    rnnr.Goon(func, prms, Param^.p_result);
    lse_set_int64(Param^.p_result, MilliSecondsBetween(Now, beg_time));
  end
  else lse_set_int64(Param^.p_result, 0);
end;

procedure call_current_module(const Param: PLseParam);cdecl;
begin
  get_runner(Param).CurrentFunc.Module.SaveTo(Param^.p_result);
end;

procedure call_current_func(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  rnnr.Current^.func.SaveTo(Param^.p_result);
end;

procedure call_current_error(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.p_result, KR_ERROR, get_engine(Param).Error);
end;

procedure call_current_args(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  E: KLiEngine;
begin
  E := get_engine(Param);
  L := KLiVarList.Create(E);
  L.SaveTo(Param^.p_result);
  L.AddStrings(E.Arguments);
end;

procedure call_current_line(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, int64(get_runner(Param).Exprrec^.tk_pos.row) + 1);
end;

procedure call_current_envs(const Param: PLseParam);cdecl;
var
  index, count: integer;
  list: KLiVarList;
begin
  list := KLiVarList.Create(get_engine(Param));
  list.SaveTo(Param^.p_result);
  count := lse_getenv_count;
  for index := 0 to count - 1 do
    list.Add(lse_getenv_string(index));
end;

procedure call_current_file(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, get_runner(Param).CurrentFunc.Module.FileName);
end;

procedure call_current_pd(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, LSE_PATH_DELIMITER);
end;

procedure call_eol(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, sys_LB);
end;

procedure call_each(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 1 then
  begin
    func := KLiFunc(Param^.p_param[1]^.VObject);
    if func <> nil then
    begin
      rnnr := get_runner(Param);
      this := lse_vargen_this(Param);
      while rnnr.Stack.AddSend(this) do
        rnnr.Goon(func, 1, Param^.p_result);
    end;
  end;
end;

procedure call_map(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  list := KLiVarList.Create(rnnr.Engine);
  lse_set_object(Param^.p_result, KR_VARLIST, list);
  if Param^.p_count > 0 then
  begin
    this := lse_vargen_this(Param);
    lse_init_value(@data);
    try
      func := value_func(Param^.p_param[1]);
      if func = nil then list.AddAll(this) else
      while rnnr.Stack.AddSend(this) do
        if rnnr.Goon(func, 1, @data) then
          list.Add(PLseValue(@data)) else
          Break;
    finally
      lse_set_nil(@data);
    end;
  end;
end;

procedure call_reduce(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 1 then
  begin
    lse_set_value(Param^.p_result, Param^.p_param[1]);
    func := value_func(Param^.p_param[2]);
    if func <> nil then
    begin
      this := lse_vargen_this(Param);
      rnnr := get_runner(Param);
      while true do
      begin
        rnnr.Stack.Add(Param^.p_result);
        if not rnnr.Stack.AddSend(this)
        or not rnnr.Goon(func, 2, Param^.p_result) then Break;
      end;
    end;
  end;
end;

procedure call_filter(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  test, data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  list := KLiVarList.Create(rnnr.Engine);
  lse_set_object(Param^.p_result, KR_VARLIST, list);
  func := value_func(Param^.p_param[1]);
  if func <> nil then
  begin
    lse_init_value(@test);
    lse_init_value(@data);
    try
      this := lse_vargen_this(Param);
      while lse_vargen_send(this, @data) do
      begin
        rnnr.Stack.Add(PLseValue(@data));
        if not rnnr.Goon(func, 1, @test) then Break else
        if value_bool(@test) then
          list.Add(PLseValue(@data));
      end;
    finally
      lse_clear_value(@data);
      lse_clear_value(@test);
    end;
  end;
end;

procedure call_sum(const Param: PLseParam);cdecl;
var
  G: PLseVargen;
  T: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    G := value_vargen(Param^.p_param[0], get_engine(Param));
    lse_set_object(Param^.p_param[0], KR_VARGEN, G);

    if Param^.p_count = 1 then
    begin
      lse_set_object(Param^.p_param[1], KR_FUNC, sys_oper_inc);
      Param^.p_count := 2;
    end;

    if Param^.p_count = 2 then
    begin
      if not lse_vargen_send(G, Param^.p_param[2]) then Exit;
      Param^.p_count := 3;
    end;

    T := Param^.p_param[1];
    Param^.p_param[1] := Param^.p_param[2];
    Param^.p_param[2] := T;
    
    call_reduce(Param);  
  end;
end;

procedure call_maxint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, high(int64));
end;

procedure call_minint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, low(int64));
end;

procedure call_abs(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: PLseType;
begin
  data := Param^.p_param[0];
  clss := lse_type(data);
  case clss^.cr_type of
    LSV_INT  : lse_set_int64(Param^.p_result, Abs(data^.VInteger));
    LSV_FLOAT: lse_set_float(Param^.p_result, Abs(data^.VFloat));
          else lse_set_value(Param^.p_result, data);
  end;
end;

procedure call_find(const Param: PLseParam);cdecl;
var
  runner: KLiRunner;
  mp: PLiRegexpr;
  varlist: KLiVarList;
  source: pchar;
  index: integer;
begin
  if Param^.p_count > 1 then
  begin
    runner := get_runner(Param);
    mp := @runner.FRegexpr;
    if regexpr_init(mp, value_pchar(Param^.p_param[1])) and
       regexpr_exec(mp, value_strec(Param^.p_param[0])) then
    begin
      if value_bool(Param^.p_param[2]) then // find all
      begin
        varlist := KLiVarList.Create(runner.Engine);
        varlist.SaveTo(Param^.p_result);
        varlist.Add(runner.ListMatchResult, KR_VARLIST);
        source := mp^.mp_source;
        while not mp^.mp_anchor do
        begin
          if mp^.mp_result.mr_len > 0 then
            index := (mp^.mp_result.mr_str - source) + mp^.mp_result.mr_len else
            index := (mp^.mp_result.mr_str - source) + 1;
          if regexpr_exec(mp, source + index, (mp^.mp_eos - source) - index) then
          begin
            mp^.mp_source := source;
            varlist.Add(runner.ListMatchResult, KR_VARLIST);
          end
          else Break;
        end;
      end
      else runner.ListMatchResult.SaveTo(Param^.p_result);
      Exit;
    end;
  end;
  lse_set_object(Param^.p_result, KR_VARLIST, nil);
end;

procedure call_gsub(const Param: PLseParam);cdecl;
var
  match_list: array of RLiMatch;
  match_len: integer;
  mp: PLiRegexpr;
  times, X: integer;
  src_str: pchar;      // source string
  src_len: integer;    // length of source string
  new_str: pchar;      // new string
  new_len: integer;    // length of new string;
  result_str: pchar;   // result string
  result_len: integer; // length of result string
  srec: PLseString;

  procedure save_match_result;
  var
    index: integer;
  begin
    index := Length(match_list);
    SetLength(match_list, index + 1);
    match_list[index] := mp^.mp_result;
    Inc(match_len, mp^.mp_result.mr_len);
  end;
  
begin
  mp := get_runner(Param).Regexpr;

  if Param^.p_count > 3 then
    times := value_int(Param^.p_param[3]) else
    times := MaxInt;

  if (times < 1)
    or not regexpr_init(mp, value_pchar(Param^.p_param[1]))
    or not regexpr_exec(mp, value_strec(Param^.p_param[0])) then
    begin
      lse_set_string(Param^.p_result, PLseString(Param^.p_param[0]^.VObject));
      Exit;
    end;

  src_str := mp^.mp_source;
  src_len := mp^.mp_eos - src_str;

  match_len := 0;
  save_match_result;
  Dec(times);
  while not mp^.mp_anchor and (times > 0) do
  begin
    if mp^.mp_result.mr_len > 0 then
      X := (mp^.mp_result.mr_str - src_str) + mp^.mp_result.mr_len else
      X := (mp^.mp_result.mr_str - src_str) + 1;
    if regexpr_exec(mp, src_str + X, (mp^.mp_eos - src_str) - X) then
    begin
      mp^.mp_source := src_str;
      save_match_result;
      Dec(times);
    end
    else Break;
  end;

  new_str := lse_strec_data(Param^.p_param[2]^.VObject);
  new_len := lse_strec_length(Param^.p_param[2]^.VObject);
  times := Length(match_list);
  
  result_len := src_len - match_len + (new_len * times);
  if result_len < 1 then Exit;

  srec := lse_strec_alloc(nil, result_len);
  lse_set_string(Param^.p_result, srec);
  result_str := lse_strec_data(srec);

  for X := 0 to times - 1 do
  begin
    src_len := match_list[X].mr_str - src_str;
     
    Move(src_str^, result_str^, src_len);
    Dec(result_len, src_len);
    Inc(result_str, src_len);
    
    Move(new_str^, result_str^, new_len);
    Dec(result_len, new_len);
    Inc(result_str, new_len);

    src_str := match_list[X].mr_str + match_list[X].mr_len;
  end;

  Move(src_str^, result_str^, result_len);
end;

procedure call_split(const Param: PLseParam);cdecl;
var
  mp: PLiRegexpr;
  len: integer;
  source: pchar;
  varlist: KLiVarList;
  line: string;
begin
  varlist :=KLiVarList.Create(get_engine(Param));
  varlist.SaveTo(Param^.p_result);
  
  mp := get_runner(Param).Regexpr;
  if not regexpr_init(mp, value_pchar(Param^.p_param[1]))
  or not regexpr_exec(mp, value_strec(Param^.p_param[0])) then
  begin
    varlist.Add(value_str(Param^.p_param[0]));
    Exit;
  end;

  if mp^.mp_source < mp^.mp_result.mr_str then
  begin
    SetString(line, mp^.mp_source, mp^.mp_result.mr_str - mp^.mp_source);
    varlist.Add(line);
  end;

  if mp^.mp_anchor then
  begin
    source := mp^.mp_result.mr_str + mp^.mp_result.mr_len;
    if source < mp^.mp_eos then
    begin
      SetString(line, source, mp^.mp_eos - source);
      varlist.Add(line);
    end;
    Exit;
  end;

  while true do
  begin
    len := mp^.mp_result.mr_len; 
    if len > 0 then
      source := mp^.mp_result.mr_str + len else
      source := mp^.mp_result.mr_str + 1;
    if regexpr_exec(mp, source, mp^.mp_eos - source) then
    begin
      SetString(line, mp^.mp_source, mp^.mp_result.mr_str - mp^.mp_source);
      varlist.Add(line);
    end
    else
    begin
      if len < 1 then Dec(source);
      if source < mp^.mp_eos then
      begin
        SetString(line, source, mp^.mp_eos - source);
        varlist.Add(line);
      end;
      Exit;
    end;
  end;
end;

procedure call_getcs(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.p_result, KR_VARLIST,
    get_runner(Param).CallStack.GetCallSnap(value_int(Param^.p_param[0])));
end;

procedure call_throw(const Param: PLseParam);cdecl;
var
  eid, msg: string;
  rnnr: KLiRunner;
begin
  if Param^.p_count < 1 then
  begin
    rnnr := get_runner(Param);
    if rnnr.Engine.Error.errno = 0 then
      set_error(Param, RuntimeError, 0, EsRuntimeError) else
      rnnr.Excepted := true;
  end
  else
  begin
     if Param^.p_count > 1 then
    begin
      eid := Trim(value_str(Param^.p_param[1]));
      if eid = '' then
        eid := RuntimeError;
    end
    else eid := RuntimeError;

    msg := Trim(value_str(Param^.p_param[0]));
    if msg = '' then
      msg := EsRuntimeError;

    set_error(Param, eid, 0, msg);
  end;
end;

procedure call_hex(const Param: PLseParam);cdecl;
var
  this: int64;
  size, digits: integer;
  text: string;
begin
  this := value_int(Param^.p_param[0]);
  size := Max(0, value_int(Param^.p_param[1]));
  digits := Min(16, size);
  if digits > 1 then
    text := Format('%.' + IntToStr(digits) + 'x', [this]) else
    text := Format('%x', [this]);
  digits := Length(text);
  if digits < size then
    text := StringOfChar('0', size - digits) + text;
  lse_set_string(Param^.p_result, text);
end;

procedure call_bitlist(const Param: PLseParam);cdecl;
const
  bitf: array[0..1] of char = ('0', '1');
var
  list: array[0..64] of char;
  index, size: integer;
  this: int64;
  base: pchar;
begin
  this := value_int(Param^.p_param[0]);
  for index := 0 to 63 do
    list[index] := bitf[(this shr (63 - index)) and 1];
  list[64] := #0;
  size := 64 - Min(64, Max(1, value_int(Param^.p_param[1])));
  base := list;
  for index := 1 to size do
    if base^ = '0' then Inc(base) else break;
  lse_set_string(Param^.p_result, base);
end;

procedure call_upto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.p_count > 1 then
  begin
    if Param^.p_count > 2 then
      step := Param^.p_param[2]^.VInteger else
      step := 1;
    varg := type_vargen_upto(Param^.p_param[0]^.VInteger,
                      Param^.p_param[1]^.VInteger,
                      step, get_engine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.p_result, lse_vargen_ensure(varg));
end;

procedure call_downto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.p_count > 1 then
  begin
    if Param^.p_count > 2 then
    begin
      step := Param^.p_param[2]^.VInteger;
      if step < 0 then
        step := - step;
    end
    else step := 1;
    varg := type_vargen_downto(Param^.p_param[0]^.VInteger,
                        Param^.p_param[1]^.VInteger,
                        step, get_engine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.p_result, lse_vargen_ensure(varg));
end;

procedure call_typeof(const Param: PLseParam);cdecl;
var
  T: KLiType;
begin
  T := value_type(Param^.p_param[0]);
  if T = KT_CLASS then
    T := KLiType(Param^.p_param[0]^.VObject);
  T.SaveTo(Param^.p_result);
end;

{ error }

procedure call_error_text(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.ErrorText);
end;

procedure call_error_module(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.module);
end;

procedure call_error_name(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure call_error_message(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.msg);
end;

procedure call_error_row(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_int64(Param^.p_result, this.row);
end;

procedure call_error_col(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_int64(Param^.p_result, this.col);
end;

procedure call_error_errno(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if get_this(Param, this) then
    lse_set_int64(Param^.p_result, this.errno);
end;

{ function }

procedure call_func_name(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure call_func_desc(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure call_func_type(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if get_this(Param, this) then
    this.ResultType.SaveTo(Param^.p_result);
end;

procedure call_func_prototype(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Prototype);
end;

procedure call_func_module(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if get_this(Param, this) then
    this.Module.SaveTo(Param^.p_result);
end;

{ hashed }

procedure call_hashed_create(const Param: PLseParam);cdecl;
var
  buckets: integer;
  hash: KLiHashed;
begin
  if Param^.p_count > 0 then
    buckets := Max(1, value_int(Param^.p_param[0])) else
    buckets := 1;
  hash := KLiHashed.Create(get_engine(Param), buckets); 
  lse_set_object(Param^.p_result, KR_HASHED, hash);
end;

procedure call_hashed_keys(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: KLiVarList;
begin
  if get_this(Param, this) then
  begin
    list := KLiVarList.Create(get_engine(Param));
    list.SaveTo(Param^.p_result);
    this.ListKeys(list);
  end;
end;

procedure call_hashed_values(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: TList;
  values: KLiVarList;
  index: integer;
begin
  if get_this(Param, this) then
  begin
    list := TList.Create;
    try
      this.ListData(list);
      values := KLiVarList.Create(get_engine(Param));
      values.SaveTo(Param^.p_result);
      for index := 0 to list.Count - 1 do
        values.Add(PLseValue(list[index]));
    finally
      list.Free;
    end;
  end;
end;

procedure call_hashed_remove(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if get_this(Param, this) and (Param^.p_count > 1) then
    this.Remove(value_str(Param^.p_param[1]));
end;

procedure call_hashed_clear(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if get_this(Param, this) then
    this.Clear;
end;

procedure call_hashed_read(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if get_this(Param, this) then
  begin
    key := value_str(Param^.p_param[1]);
    data := this.FindValue(key);
    if data = nil then
      lse_set_value(Param^.p_result, Param^.p_param[2]) else
      lse_set_value(Param^.p_result, data);
  end;
end;

procedure call_hashed_isset(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
begin
  if get_this(Param, this) and (Param^.p_count > 1) then
  begin
    key := value_str(Param^.p_param[1]);
    lse_set_bool(Param^.p_result, this.IsSet(key));
  end;
end;

{ module }

procedure call_module_name(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure call_module_desc(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure call_module_file(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.FileName);
end;

procedure call_module_modules(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  engine: KLiEngine;
begin
  if get_this(Param, this) then
  begin
    engine := get_engine(Param);
    this.Modules.ToVarlist(engine).SaveTo(Param^.p_result);
  end;
end;

procedure call_module_funcs(const Param: PLseParam);cdecl;
var
  M: KLiModule;
  L: KLiVarList;
  F: KLiFunc;
begin
  if get_this(Param, M) then
  begin
    L := KLiVarList.Create(get_engine(Param));
    L.SaveTo(Param^.p_result);
    F := M.FirstFunc;
    while F <> nil do
    begin
      L.Add(F, KR_FUNC);
      F := F.Next;
    end;
  end;
end;

procedure call_module_types(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  list: KLiVarList;
  index: integer;
begin
  if get_this(Param, this) then
  begin
    list := KLiVarList.Create(get_engine(Param));
    list.SaveTo(Param^.p_result);
    for index := 0 to this.TypeCount - 1 do
      list.Add(this.GetType(index), KR_CLASS);
  end;
end;

procedure call_module_version(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Version);
end;

procedure call_module_main(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    this.MainFunc.SaveTo(Param^.p_result);
end;

procedure call_module_imports(const Param: PLseParam);cdecl;
var
  this, curr: KLiModule;
begin
  if get_this(Param, this) then
  begin
    if this.ModuleType <> moyScript then
      lse_error('%s can not import other modules', [this.Name]);
    Param^.p_param[0] := Param^.p_param[1];
    call_load(Param);
    curr := KLiModule(Param^.p_result^.VObject);
    if curr.ModuleType = moyScript then
    begin
      curr.AddImporter(this);
      this.Modules.Add(curr);
    end
    else this.Modules.Add(curr);
  end;
end;

{ vargen }

procedure call_vargen_create(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := value_vargen(Param^.p_param[0], get_engine(Param));
  lse_set_vargen(Param^.p_result, this);
end;

procedure call_vargen_eof(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.p_result, lse_vargen_eof(this));
end;

procedure call_vargen_next(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_vargen_send(this, Param^.p_result);
end;

procedure call_vargen_rewind(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.p_result, lse_vargen_rewind(this));
end;

{ stream }

procedure call_stream_close(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_close(this) else
    set_error_this(Param);
end;

procedure call_stream_eof(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_bool(Param^.p_result, this^.s_eof(this) <> 0) else
    set_error_this(Param);
end;

procedure call_stream_get_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.p_result, this^.s_seek(this, 0, SSF_CURRENT)) else
    set_error_this(Param);
end;

procedure call_stream_set_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_seek(this, Param^.p_param[1]^.VInteger, SSF_BEGINNING) else
    set_error_this(Param);
end;

procedure call_stream_set_length(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_set_size(this, Param^.p_param[1]^.VInteger) else
    set_error_this(Param);
end;

procedure call_stream_read(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
  line: string;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    if Assigned(this^.s_read) then
    begin
      size := Param^.p_param[1]^.VInteger;
      if size > 0 then
      begin
        SetLength(line, size);
        size := this^.s_read(this, pointer(line), size);
        if size > 0 then
        begin
          lse_set_string(Param^.p_result, pchar(line), size);
          Exit;
        end;
      end;
    end;
    lse_set_string(Param^.p_result, '');
  end
  else set_error_this(Param);
end;

procedure call_stream_readln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_string(Param^.p_result, this^.s_readln(this)) else
    set_error_this(Param);
end;

procedure call_stream_write(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    size := lse_stream_write(this, Param^.p_param[1]^.VObject);
    lse_set_int64(Param^.p_result, size);
  end
  else set_error_this(Param);
end;

procedure call_stream_writeln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.p_result,
      lse_stream_writeln(this, Param^.p_param[1]^.VObject)) else
    set_error_this(Param);
end;

procedure call_stream_writeTo(const Param: PLseParam);cdecl;
var
  this, desti: PLseStream;
  bytes: integer;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    desti := PLseStream(Param^.p_param[1]^.VObject);
    if (desti <> nil) and (desti <> this) then
      bytes := lse_stream_fill(desti, this, Param^.p_param[2]^.VInteger) else
      bytes := 0;
    lse_set_int64(Param^.p_result, bytes);
  end
  else set_error_this(Param);
end;

procedure call_stream_flush(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_flush(this) else
    set_error_this(Param);
end;

procedure call_stream_lines(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  varg: PLseVargen;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    varg := type_stream_vargen_lines(this, get_engine(Param));
    lse_set_object(Param^.p_result, KR_VARGEN, varg);
  end
  else set_error_this(Param);
end;

{ string }

procedure call_string_setAt(const Param: PLseParam);cdecl;
var
  this: PLseString;
  index, range: int64;
begin
  this := Param^.p_param[0]^.VObject;
  range := lse_strec_length(this);
  index := lse_vary_index(value_int(Param^.p_param[1]), range);
  lse_check_index(index, range);
  this := lse_strec_alloc(lse_strec_data(this), range);
  lse_strec_data(this)[index] := value_char(Param^.p_param[2]);
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_name(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, extract_name(lse_strec_data(this)));
end;

procedure call_string_value(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, extract_value(lse_strec_data(this)));
end;

procedure call_string_lower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := lse_strec_lower(Param^.p_param[0]^.VObject);
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_upper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := lse_strec_upper(Param^.p_param[0]^.VObject);
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_compare(const Param: PLseParam);cdecl;
var
  this, value: PLseString;
  IgnoreCase: boolean;
begin
  this := Param^.p_param[0]^.VObject;
  value := Param^.p_param[1]^.VObject;
  IgnoreCase := value_bool(Param^.p_param[2]);
  lse_set_int64(Param^.p_result, strec_comp(this, Value, IgnoreCase));
end;

procedure call_string_replace(const Param: PLseParam);cdecl;
var
  this, patten, newStr: pchar;
  flags: TReplaceFlags;
begin
  this := lse_strec_data(Param^.p_param[0]^.VObject);
  patten := lse_strec_data(Param^.p_param[1]^.VObject);
  newStr := lse_strec_data(Param^.p_param[2]^.VObject);
  flags := [rfReplaceAll];
  if value_bool(Param^.p_param[3]) then // IgnoreCase
    flags := flags + [rfIgnoreCase];
  if value_bool(Param^.p_param[4]) then // FirstOnly
    flags := flags - [rfReplaceAll];
  lse_set_string(Param^.p_result, StringReplace(this, patten, newStr, flags));
end;

procedure call_string_pos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  patten := Param^.p_param[1]^.VObject;
  IgnoreCase := value_bool(Param^.p_param[2]);
  str := lse_strec_data(this);
  pos := first_pos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.p_result, pos - str) else
    lse_set_int64(Param^.p_result, -1);
end;

procedure call_string_lastPos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  patten := Param^.p_param[1]^.VObject;
  IgnoreCase := value_bool(Param^.p_param[2]);
  str := lse_strec_data(this);
  pos := last_pos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.p_result, pos - str) else
    lse_set_int64(Param^.p_result, -1);
end;

procedure call_string_left(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  size := min(value_int(Param^.p_param[1]), slen);
  if size = slen then
    lse_set_string(Param^.p_result, this) else
  if size > 0 then
    lse_set_string(Param^.p_result, lse_strec_alloc(lse_strec_data(this), size)) else
    lse_set_string(Param^.p_result, '', 0);
end;

procedure call_string_right(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  size := min(value_int(Param^.p_param[1]), slen);
  if size = slen then
    lse_set_string(Param^.p_result, this) else
  if size > 0 then
  begin
    base := lse_strec_data(this) + (slen - size);
    lse_set_string(Param^.p_result, lse_strec_alloc(base, size));
  end
  else lse_set_string(Param^.p_result, '', 0);
end;

procedure call_string_trim(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  if (strec_count_tab(this, L, M, R) > 0) and ((L + R) > 0) then
  begin
    M := lse_strec_length(this) - (L + R);
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.p_result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure call_string_trimLeft(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  if (strec_count_tab(this, L, M, R) > 0) and (L > 0) then
  begin
    M := lse_strec_length(this) - L;
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.p_result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure call_string_trimRight(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
begin
  this := Param^.p_param[0]^.VObject;
  if (strec_count_tab(this, L, M, R) > 0) and (R > 0) then
  begin
    M := lse_strec_length(this) - R;
    lse_set_string(Param^.p_result, lse_strec_alloc(lse_strec_data(this), M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure call_string_trimAll(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base, next: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  if strec_count_tab(this, L, M, R) > 0 then
  begin
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, lse_strec_length(this) - (L + M + R));
    if this <> nil then
    begin
      next := lse_strec_data(this);
      while base^ <> #0  do
      begin
        if not (base^ in SpaceChar) then
        begin
          next^ := base^;
          Inc(next);
        end;
        Inc(base);
      end;
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_copy(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, index, count: int64;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := value_int(Param^.p_param[1]);
    count := value_int(Param^.p_param[2]);
    index := lse_vary_range(index, slen, count);
    if count > 0 then
    begin
      if count < slen then
        this := lse_strec_alloc(lse_strec_data(this) + index, count);
      lse_set_string(Param^.p_result, this);
    end;
  end;
end;

procedure call_string_delete(const Param: PLseParam);cdecl;
var
  this: PLseString;
  base, next: pchar;
  slen, index, count: int64;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := value_int(Param^.p_param[1]);
    if Param^.p_count > 2 then
      count := value_int(Param^.p_param[2]) else
      count := 1;
    index := lse_vary_range(index, slen, count);
    if count > 0 then
    begin
      Dec(slen, count);
      if slen > 0 then
      begin
        base := lse_strec_data(this);
        this := lse_strec_alloc(nil, slen);
        next := lse_strec_data(this);
        if index > 0 then
        begin
          Move(base^, next^, index);
          Dec(slen, index);
          Inc(next, index);
          Inc(base, index);
        end;
        Move((base + count)^, next^, slen);
      end
      else this := nil;
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_insert(const Param: PLseParam);cdecl;
var
  this, text: PLseString;
  base, next: pchar;
  index, slen, size: int64;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  text := Param^.p_param[1]^.VObject;
  slen := lse_strec_length(text);
  if slen > 0 then
  begin
    index := lse_vary_index(value_int(Param^.p_param[2]), size);
    if (index >= 0) and (index <= size) then
    begin
      base := lse_strec_data(this);
      this := lse_strec_alloc(nil, size + slen);
      next := lse_strec_data(this);
      if index > 0 then
      begin
        Move(base^, next^, index);
        Dec(size, index);
        Inc(base, index);
        Inc(next, index);
      end;
      Move(lse_strec_data(text)^, next^, slen);
      Inc(next, slen);
      Move(base^, next^, size);
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_isAlpha(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), AlphaChar));
end;

procedure call_string_isAlnum(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), AlnumChar));
end;

procedure call_string_isCntrl(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), CntrlChar));
end;

procedure call_string_isDigit(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), DigitChar));
end;

procedure call_string_isSpace(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), SpaceChar));
end;

procedure call_string_isHex(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this), HexChar));
end;

procedure call_string_extractName(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.p_param[0]^.VObject;
  if Param^.p_count > 1 then
    midc := value_str(Param^.p_param[1]) else
    midc := '=';
  lse_set_string(Param^.p_result, extract_name(lse_strec_data(this), midc));
end;

procedure call_string_extractValue(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.p_param[0]^.VObject;
  if Param^.p_count > 1 then
    midc := value_str(Param^.p_param[1]) else
    midc := '=';
  lse_set_string(Param^.p_result, extract_value(lse_strec_data(this), midc));
end;

procedure call_string_saveToFile(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(value_fname(Param^.p_param[1]), fmCreate) do
  try
    this := Param^.p_param[0]^.VObject;
    WriteBuffer(lse_strec_data(this)^, lse_strec_length(this));
  finally
    Free;
  end;
end;

procedure call_string_fileText(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(value_fname(Param^.p_param[0]), fmShareDenyWrite) do
  try
    this := lse_strec_alloc(nil, size);
    lse_set_string(Param^.p_result, this);
    if this <> nil then
      Read(lse_strec_data(this)^, size);
  finally
    Free;
  end;
end;

procedure call_string_lformat(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := max(0, value_int(Param^.p_param[1]));
  if width > size then
  begin
    filler := value_char(Param^.p_param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    Move(base^, next^, size);
    Inc(next, size);
    for width := width - size downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_rformat(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := value_int(Param^.p_param[1]);
  if width > size then
  begin
    filler := value_char(Param^.p_param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    for width := width - size downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
    Move(base^, next^, size);
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_center(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width, A: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := value_int(Param^.p_param[1]);
  if width > size then
  begin
    filler := value_char(Param^.p_param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    Dec(width, size);
    for A := width div 2 downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
    Move(base^, next^, size);
    Inc(next, size);
    for A := width - (width div 2) downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_randomOrder(const Param: PLseParam);cdecl;

  function random_order(const S: string): string;
  var
    A, L, X: integer;
    C: char;
  begin
    Result := S;
    L := Length(Result);
    if L > 1 then
    repeat
      Randomize;
      for A := 1 to L do
      begin
        repeat
          X := System.Random(L) + 1;
        until X <> A;
        C := Result[A];
        Result[A] := Result[X];
        Result[X] := C;
      end;
    until Result <> S;
  end;

var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  if lse_strec_length(this) > 1 then
    lse_set_string(Param^.p_result, random_order(lse_strec_data(this))) else
    lse_set_string(Param^.p_result, this);
end;

procedure call_string_html(const Param: PLseParam);cdecl;
var
  smr: PLseString;
begin
  smr := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, encode_HTML(lse_strec_data(smr), lse_strec_length(smr), false));
end;

// string string.reverse()
procedure call_string_reverse(const Param: PLseParam);cdecl;
var
  this: PLseString;
  head, last: pchar;
  temp: char;
begin
  this := Param^.p_param[0]^.VObject;
  if lse_strec_length(this) > 1 then
  begin
    this := lse_strec_dup(this);
    head := lse_strec_data(this);
    last := head + lse_strec_length(this) - 1;
    while head < last do
    begin
      temp := head^;
      head^ := last^;
      last^ := temp;
      Inc(head);
      Dec(last);
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

// bool string.isLower()
procedure call_string_isLower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this),
    lse_strec_length(this), ['a'..'z']));
end;

// bool string.isUpper()
procedure call_string_isUpper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, in_charset(lse_strec_data(this),
    lse_strec_length(this), ['A'..'Z']));
end;

// string string.translate(string OrdCharList, string newCharList)
procedure call_string_translate(const Param: PLseParam);cdecl;
var
  this, old, new: PLseString;
  size, index: integer;
  map: array[char] of char;
  base, next: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  old  := Param^.p_param[1]^.VObject;
  new  := Param^.p_param[2]^.VObject;
  size  := min(lse_strec_length(old), lse_strec_length(new));
  if (size > 0) and (lse_strec_length(this) > 0) then
  begin
    base := lse_strec_data(old);
    next := lse_strec_data(new);
    FillChar(map, sizeof(map), 0);
    for index := 1 to size do
    begin
      map[base^] := next^;
      Inc(base);
      Inc(next);
    end;
    this := lse_strec_dup(this);
    base := lse_strec_data(this);
    size := lse_strec_length(this);
    for index := 1 to size do
    begin
      if map[base^] <> #0 then
        base^ := map[base^];
      Inc(base);
    end;
  end;
  lse_set_string(Param^.p_result, this);
end;

procedure call_string_filePath(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFilePath(value_fname(Param^.p_param[0])));
end;

procedure call_string_fullFileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    lse_expand_fname(value_fname(Param^.p_param[0])));
end;

procedure call_string_fileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileName(value_fname(Param^.p_param[0])));
end;

procedure call_string_fileExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileExt(value_fname(Param^.p_param[0])));
end;

procedure call_string_changeExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ChangeFileExt(value_fname(Param^.p_param[0]),
                  value_fname(Param^.p_param[1])));
end;

procedure call_string_hexToInt(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  if in_charset(lse_strec_data(this), lse_strec_length(this), HexChar) then
    lse_set_int64(Param^.p_result, StrToInt64('$' + lse_strec_data(this))) else
    lse_set_int64(Param^.p_result, value_int(Param^.p_param[1]));
end;

procedure call_string_hash(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, hash_of(value_str(Param^.p_param[0])));
end;

procedure call_string_md5sum(const Param: PLseParam);cdecl;
var
  M: KLiMD5;
  S: string;
begin
  M := KLiMD5.Create;
  try
    S := value_str(Param^.p_param[0]);
    if value_bool(Param^.p_param[1]) then
      S := M.sumFile(S) else
      S := M.sumStr(S);
    lse_set_string(Param^.p_result, S);
  finally
    M.Free;
  end;
end;

{ type }

procedure call_type_name(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure call_type_description(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure call_type_simple(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if get_this(Param, this) then
    lse_set_bool(Param^.p_result, this.IsSimpleType);
end;

procedure call_type_module(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if get_this(Param, this) then
    this.Module.SaveTo(Param^.p_result);
end;

{ varlist }

procedure call_varlist_create(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  this := KLiVarList.Create(get_engine(Param)); 
  this.SaveTo(Param^.p_result);
  if Param^.p_count > 0 then
    this.Count := Max(0, Param^.p_param[0]^.VInteger);
end;

procedure call_varlist_get_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    lse_set_int64(Param^.p_result, this.Count);
end;

procedure call_varlist_set_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
      this.Count := Max(0, Param^.p_param[1]^.VInteger);
end;

procedure call_varlist_exchange(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  X1, X2: int64;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      X1 := lse_vary_index(value_int(Param^.p_param[1]), this.Count);
      X2 := lse_vary_index(value_int(Param^.p_param[2]), this.Count);
      if X1 <> X2 then
        this.Exchange(X1, X2);
    end;
end;

procedure call_varlist_move(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  X1, X2: int64;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      X1 := lse_vary_index(value_int(Param^.p_param[1]), this.Count);
      X2 := lse_vary_index(value_int(Param^.p_param[2]), this.Count);
      if X1 <> X2 then
        this.Move(X1, X2);
    end;
end;

procedure call_varlist_add(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      lse_set_int64(Param^.p_result, this.Count);
      this.Add(Param^.p_param[1]);
    end;
end;

procedure call_varlist_addFrom(const Param: PLseParam);cdecl;
var
  this, list: KLiVarList;
  index, count: integer;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      list := KLiVarList(Param^.p_param[1]^.VObject);
      if (list <> nil) and (list.Count > 0) then
      begin
        count := list.Count;
        for index := 0 to count - 1 do
          this.Add(list[index]);
      end;
    end;
end;

procedure call_varlist_fill(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  varg: PLseVargen;
  data: RLseValue;
begin
  if get_this(Param, this) then
    if (Param^.p_count > 1) and not (this is KLiVarSnap) then
    begin
      if value_bool(Param^.p_param[2]) then
        this.Clear;
      varg := value_vargen(Param^.p_param[1], get_engine(Param));
      lse_init_value(@data);
      try
        while lse_vargen_send(varg, @data) do
          this.Add(PLseValue(@data));
      finally
        lse_set_nil(@data);
      end;
    end;
end;

procedure call_varlist_insert(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      index := lse_vary_index(value_int(Param^.p_param[1]), this.Count);
      lse_check_index(index, int64(this.Count) + 1);
      lse_set_value(this.Insert(index), Param^.p_param[2]);
    end;
end;

procedure call_varlist_delete(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if get_this(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      index := lse_vary_index(value_int(Param^.p_param[1]), this.Count);
      lse_check_index(index, this.Count);
      this.Delete(index);
    end;
end;

procedure call_varlist_clear(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    if this is KLiVarSnap then
      KLiVarSnap(this).Reset else
      this.Clear;
end;

procedure call_varlist_copy(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index, count: int64;
begin
  if get_this(Param, this) then
  begin
    index := value_int(Param^.p_param[1]);
    count := value_int(Param^.p_param[2]);
    index := lse_vary_range(index, this.Count, count);
    this.Copy(index, count).SaveTo(Param^.p_result);
  end;
end;

procedure call_varlist_left(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if get_this(Param, this) then
  begin
    count := value_int(Param^.p_param[1]);
    this.Left(count).SaveTo(Param^.p_result);
  end;
end;

procedure call_varlist_right(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if get_this(Param, this) then
  begin
    count := value_int(Param^.p_param[1]);
    this.Right(count).SaveTo(Param^.p_result);
  end;
end;

procedure call_varlist_filter(const Param: PLseParam);cdecl;
var
  this, list: KLiVarList;
  func: KLiFunc;
  cond: RLseValue;
  rnnr: KLiRunner;
  index: integer;
begin
  if get_this(Param, this) then
  begin
    rnnr := get_runner(Param);
    list := KLiVarList.Create(rnnr.Engine);
    lse_set_object(Param^.p_result, KR_VARLIST, list);
    func := value_func(Param^.p_param[1]);
    if (func <> nil) and (func.ResultType <> KT_VOID) then
    begin
      lse_init_value(@cond);
      try
        index := 0;
        while index < this.Count do
        begin
          rnnr.Stack.Add(this[index]);
          if not rnnr.Goon(func, 1, @cond) then Break else
          if value_bool(@cond) then
            list.Add(this[index]);
          Inc(index);
        end;
      finally
        lse_clear_value(@cond);
      end;
    end;
  end;
end;

procedure call_varlist_first(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    lse_set_value(Param^.p_result, this[0]);
end;

procedure call_varlist_last(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
    lse_set_value(Param^.p_result, this[-1]);
end;

procedure call_varlist_shift(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if get_this(Param, this) then
  begin
    lse_set_value(Param^.p_result, this[0]);
    if not (this is KLiVarSnap) then
      this.Delete(0);
  end;
end;

procedure call_varlist_pop(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: integer;
begin
  if get_this(Param, this) then
  begin
    index := this.Count - 1;
    lse_set_value(Param^.p_result, this[index]);
    if not (this is KLiVarSnap) then
      this.Delete(index);
  end;
end;

{ KLiTokenizer }

constructor KLiTokenizer.Create(const Script: string);
var
  X: integer;
begin
  FCode := Script;
  FSize := Length(FCode);
  if Copy(FCode, 1, 2) = '#!' then
  begin
    X := 1;
    while (X <= FSize) and not (FCode[X] in [#13, #10]) do
    begin
      FCode[X] := ' ';
      Inc(X);
    end;
  end;

  FBase := pchar(FCode);
  FEOF := false;
  FPosition := 1;
  FRow := 0;
  FCol := 0;
  FSkip0A := false;
  if FPosition <= FSize then
    FChar := FCode[FPosition] else
    FChar := #0;
    
  FillChar(FTokenList, sizeof(FTokenList), 0);
  FUnused := @FTokenList[0];
  for X := 1 to Length(FTokenList) - 1 do
    FTokenList[X - 1].tk_next := @FTokenList[X];
  FCurrent := nil;

  FBuffer := TMemoryStream.Create;
end;

destructor KLiTokenizer.Destroy;
var
  X: integer;
  T: PLiToken;
begin
  FreeAndNil(FBuffer);
  for X := 0 to Length(FTokenList) - 1 do
    FTokenList[X].tk_name := '';
  while FNextToken <> nil do
  begin
    T := FNextToken^.tk_next;
    token_free(FNextToken);
    FNextToken := T;
  end;
  inherited;
end;

function KLiTokenizer.DupCurrentToken: PLiToken;
var
  token: PLiToken;
begin
  token := GetCurrentToken;
  if token <> nil then
  begin
    Result := FUnused;
    FUnused := FUnused^.tk_next;
    Result^.tk_next := token^.tk_next;
    Result^.tk_sym := token^.tk_sym;
    Result^.tk_pos := token^.tk_pos;
    Result^.tk_name := token^.tk_name;
    Result^.VInteger := token^.VInteger;
    token^.tk_next := Result;
  end;
  Result := token;
end;

function KLiTokenizer.GetChar: boolean;
var
  F: boolean;
begin
  FSkip0A := false;
  F := (FChar = #13);
  if F or (FChar = #10) then
  begin
    Inc(FRow);
    FCol := 0;
  end
  else Inc(FCol);
  Inc(FPosition);
  Result := (FPosition <= FSize);
  if Result then
  begin
    FChar := FCode[FPosition];
    if F and (FChar = #10) then
    begin
      FSkip0A := true;
      Inc(FPosition);
      Result := (FPosition <= FSize);
      if Result then
        FChar := FCode[FPosition] else
        FChar := #0;
    end;
  end
  else FChar := #0;
end;

function KLiTokenizer.GetCurrentToken: PLiToken;
var
  isstr: boolean;
begin
  if FCurrent = nil then
    GetNextToken(isstr);
  Result := FCurrent;
end;

function KLiTokenizer.GetNextToken(var IsStr: boolean): PLiToken;
var
  token: PLiToken;
  done: boolean;
begin
  if FCurrent = nil then
  begin
    if FNextToken <> nil then
    begin
      token := FNextToken;
      token_copy(token, FUnused);
      FNextToken := token^.tk_next;
      token_free(token);
      done := true;
    end
    else done := GetToken(FUnused, IsStr);
    if done then
    begin
      FCurrent := FUnused;
      FUnused := FUnused^.tk_next;
      FCurrent^.tk_next := nil;
    end;
  end
  else
  if FCurrent^.tk_next <> nil then
  begin
    token := FCurrent;
    FCurrent := FCurrent^.tk_next;
    token^.tk_next := FUnused;
    FUnused := token;
    done := true;
  end
  else
  if FNextToken <> nil then
  begin
    token := FNextToken;
    token_copy(token, FCurrent);
    FNextToken := token^.tk_next;
    token_free(token);
    done := true;
  end
  else done := GetToken(FCurrent, IsStr);

  if not done and (FCurrent <> nil) then
  begin
    FCurrent^.tk_next := FUnused;
    FUnused := FCurrent;
    FCurrent := nil;
  end;
  
  Result := FCurrent;
end;

function KLiTokenizer.GetToken(token: PLiToken; var IsStr: boolean): boolean;
var
  base, next: pchar;

  procedure trace_to_next;
  var
    endp: integer;
  begin
    endp := (next - FBase) + 1;
    while FPosition < endp do GetChar;
  end;

  procedure get_number;
  var
    iv: int64;
    ev: extended;
    ei: KLiExtInt;
  begin
    next := FBase + FPosition - 1;
    base := next;
    ev := parse_ext_int(next, iv, ei);
    if ei <> eiNone then
    begin
      if ei = eiInt then
      begin
        token^.VInteger := iv;
        token^.tk_sym := syInt;
      end
      else
      begin
        token^.VFloat := ev;
        token^.tk_sym := syFloat;
      end;
      trace_to_next;
    end;
    token^.tk_name := new_string(base, next - base);
  end;

  procedure get_string(allow_esc_char: boolean);
  begin
    IsStr := true;
    FBuffer.Clear;
    base := FBase + FPosition - 1;
    next := base;
    if parse_str(next, FBuffer, allow_esc_char) then
    begin
      token^.tk_name := token^.tk_name + new_string(FBuffer.Memory, FBuffer.Size);
      trace_to_next;
      token^.tk_sym := syStr;
    end;
  end;

  procedure get_pure_identity(pure_id: boolean);
  var
    idstr: string;
  begin
    if SkipSpaces and is_ID_head(FChar) then
    begin
      idstr := FChar;
      while GetChar and (FChar in IDChar) do
        idstr := idstr + FChar;
      if pure_id then
      begin
        token^.tk_name := idstr;
        token^.tk_sym := ID_to_sym(idstr, syID);
        if token^.tk_sym in [syID] then
          if SkipSpaces and (FChar = ':') and (PeekChar = ':') then
          begin
            token^.tk_name := token^.tk_name + '::';
            GetChar;
            GetChar;
            get_pure_identity(false);
          end
          else token^.VPureID := true;
      end
      else
      begin
        token^.tk_sym := syID;
        token^.tk_name := token^.tk_name + idstr;
      end;
    end
    else token^.tk_sym := syError;
  end;

  procedure get_operator(defaultSymbol: KLiSymbol;
    const next: array of char;
    const syms: array of KLiSymbol);
  var
    count, index: integer;
  begin
    if GetChar then
    begin
      count := min(Length(next), Length(syms));
      for index := 0 to count - 1 do
        if FChar = next[index] then
        begin
          token^.tk_sym := syms[index];
          token^.tk_name := Symbols[token^.tk_sym].ID;
          GetChar;
          Exit;
        end;
    end;
    token^.tk_sym := defaultSymbol;
    token^.tk_name := Symbols[token^.tk_sym].ID;
  end;

  procedure get_value;
  var
    vs: string;
  begin
    if GetChar then
      if FChar in IDHeadChar then
      begin
        vs := FChar;
        while GetChar and (FChar in IDChar) do
          vs := vs + FChar;
        token^.tk_name := vs;
        token^.tk_sym := syGetSV;
      end
      else
      if (FChar = '{') and GetChar and (FChar <> '}') then
      begin
        vs := FChar;
        while GetChar and (FChar <> '}') do
          vs := vs + FChar;
        if FChar = '}' then
        begin
          token^.tk_name := vs;
          token^.tk_sym := syGetEnv;
          GetChar;
        end;
      end;
  end;

begin
  Result := (FChar <> #0) and SkipSpaces;

  token_reset(token);
  token^.tk_pos.Row := FRow;
  token^.tk_pos.Col := FCol;
  IsStr := false;

  if Result then
  begin
    case FChar of
    '0'..'9': get_number;
    '"','''': get_string(FChar = '"');
    '+'     : get_operator(syAdd, [], []);
    '-'     : get_operator(syDec, [], []);
    '*'     : get_operator(syMul, [], []);
    '/'     : get_operator(syDiv, [], []);
    '%'     : get_operator(syMod, [], []);
    '^'     : get_operator(syBXor, [], []);
    '~'     : get_operator(syBNot, [], []);
    '('     : get_operator(syLParen, [], []);
    ')'     : get_operator(syRParen, [], []);
    '{'     : get_operator(syLBlock, [], []);
    '}'     : get_operator(syRBlock, [], []);
    '['     : get_operator(syLArray, [], []);
    ']'     : get_operator(syRArray, [], []);
    '.'     : get_operator(syDot, [], []);
    '?'     : get_operator(syAsk, [], []);
    ':'     : get_operator(syDot2, [], []);
    ','     : get_operator(syComma,  [], []);
    ';'     : get_operator(sySemicolon,  [], []);
    '='     : get_operator(syBecome, ['=', '>'], [syEQ, syRefer]);
    '!'     : get_operator(syError, ['=', '<', '>'], [syNE, syME, syLE]);
    '<'     : begin
                get_operator(syLess, ['=', '<'], [syLE, syBShl]);
                if (token^.tk_sym = syBShl) and (FChar = '<') then
                begin
                  token^.tk_sym := syFill;
                  token^.tk_name := '<<<';
                  GetChar;
                end;
              end;
    '>'     : get_operator(syMore, ['=', '>'], [syME, syBShr]);
    '&'     : get_operator(syBAnd, [], []);
    '|'     : get_operator(syBOr, [], []);
    '$'     : get_value;
    '@'     : get_operator(syFormat, [], []);
    else
      if is_ID_head(FChar) then
        get_pure_identity(true) else
        token^.tk_name := FChar;
    end;
  end
  else
  if not FEOF then
  begin
    FEOF := true;
    token^.tk_sym := syEOF;
    Result := true;
  end;
end;

function KLiTokenizer.GotoChar(Chars: KLiCharSet): boolean;
begin
  repeat Result := (FChar in Chars);
  until Result or not GetChar;
end;

function KLiTokenizer.PeekChar: char;
begin
  if FPosition < FSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function KLiTokenizer.PeekNextThreeTokens(var one, two, three: PLiToken): integer;
begin
  one := nil;
  two := nil;
  three := nil;
  Result := max(0, PrepareTokens(4) - 1);
  if Result > 0 then
  begin
    one := FCurrent^.tk_next;
    if Result > 1 then
    begin
      two := one^.tk_next;
      if Result > 2 then
        three := two^.tk_next;
    end;
  end;
end;

function KLiTokenizer.PeekNextToken: PLiToken;
begin
  if PrepareTokens(2) > 1 then
    Result := FCurrent^.tk_next else
    Result := nil;
end;

function KLiTokenizer.PrepareTokens(Count: integer): integer;
var
  token, last: PLiToken;
  isstr: boolean;
begin
  Result := 0;
  Count := max(min(Count, Length(FTokenList) - 1), 0);
  if (Count > 0) and (GetCurrentToken <> nil) then
  begin
    Inc(Result);
    last := FCurrent;
    while (Result < Count) and (last^.tk_next <> nil) do
    begin
      last := last^.tk_next;
      Inc(Result);
    end;
    while (Result < Count) and (FUnused <> nil)  do
    begin
      if FNextToken <> nil then
      begin
        token := FNextToken;
        token_copy(token, FUnused);
        FNextToken := token^.tk_next;
        token_free(token);
      end
      else if not GetToken(FUnused, IsStr) then Exit;
      last^.tk_next := FUnused;
      last := FUnused;
      FUnused := FUnused^.tk_next;
      last^.tk_next := nil;
      Inc(Result);
    end;
  end;
end;

function KLiTokenizer.PutBack(Tokens: TList): integer;
var
  X: integer;
begin
  if Tokens <> nil then
  begin
    Result := Tokens.Count;
    for X := Result - 1 downto 0 do
      PutBack(PLiToken(Tokens[X]));
  end
  else Result := 0;
end;

function KLiTokenizer.PutBack(Token: PLiToken): PLiToken;
begin
  Result := token_clone(Token);
  Result^.tk_next := FNextToken;
  FNextToken := Result;
end;

function KLiTokenizer.SkipSpaces: boolean;

  function on_line_comment: boolean;
  begin
    Result := (FChar = '#') or ((FChar = '/') and (PeekChar = '/'));
  end;

  function skip_line_comment: boolean;
  var
    F: boolean;
  begin
    Result := GotoChar([#13, #10, #0]);
    if Result and (FChar <> #0) then
    begin
      F := (FChar = #13);
      Result := GetChar;
      if Result and F and (FChar = #10) then
        Result := GetChar;
    end;
  end;

  function on_block_comment: boolean;
  begin
    Result := (FChar = '/') and (PeekChar = '*');
  end;

  function skip_block_comment: boolean;
  begin
    GetChar;
    while GotoChar(['*']) do
    begin
      Result := GetChar and (FChar = '/');
      if Result then
      begin
        Result := GetChar;
        exit;
      end;
    end;
    Result := false;
  end;

begin
  Result := false;
  while not Result and GotoChar([#1..#255] - SpaceChar) do
    if on_line_comment then
    begin
      if not skip_line_comment then Exit;
    end
    else
    if on_block_comment then
    begin
      if not skip_block_comment then Exit;
    end
    else Result := true;
end;

{ KLiTokens }

procedure KLiTokens.Clear;
var
  index: integer;
  token: PLiToken;
begin
  try
    for index := GetCount - 1 downto 0 do
    begin
      token := FItems[index];
      FItems.Delete(index);
      token_free(token);
    end;
  finally
    FItems.Clear;
  end;
end;

constructor KLiTokens.Create;
begin
  FItems := TList.Create; 
end;

destructor KLiTokens.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function KLiTokens.GetCount: integer;
begin
  Result := FItems.Count;
end;

function KLiTokens.GetItem(index: integer): PLiToken;
begin
  Result := PLiToken(FItems[index]);
end;

function KLiTokens.Next: PLiToken;
begin
  Result := token_clone(nil);
  FItems.Add(Result);
end;

{ KLiParser }

constructor KLiParser.Create(AModule: KLiModule);
begin
  FModule := AModule;
  FRunner := FModule.FEngine.FMainRunner;
  FSymbols := KLiTokens.Create;
end;

function KLiParser.CurCodes: KLiCodes;
begin
  Result := FCurrent.FCodes;
end;

destructor KLiParser.Destroy;
begin
  if not FIsShadow then
    free_and_nil(FTokenizer);
  free_and_nil(FSymbols);
  inherited;
end;

function KLiParser.Parse(const Code: string): KLiFunc;
begin
  FSymbols.Clear;
  FCurrent := nil;
  FInTermCount := 0;
  FreeAndNil(FTokenizer);
  FTokenizer := KLiTokenizer.Create(Code);
  if (FRunner = nil) and FModule.IsMainModule then
  begin
    FCurrent := FModule.FEngine.GetMainFunc;
    FCurrent.FCodes.Clear(nil);
  end
  else
  begin
    FCurrent := FModule.NewFunc;
    if FModule.FMainFunc = nil then
      FModule.FMainFunc := FCurrent;
  end;
  Result := FCurrent;
  ParseBody(syEOF, false);
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
    token_copy(token, sym);
    if sym^.tk_pos.module = nil then
      sym^.tk_pos.module := FModule;
    FLast := sym;
  end;
end;

function KLiParser.LastCol: integer;
begin
  Result := FLast^.tk_pos.col;
end;

function KLiParser.LastModule: KLiModule;
begin
  Result := KLiModule(FLast^.tk_pos.module);
  if Result = nil then
    Result := FModule;
end;

function KLiParser.LastRow: integer;
begin
  Result := FLast^.tk_pos.row;
end;

function KLiParser.LastVal: string;
begin
  Result := FLast^.tk_name;
end;

function KLiParser.GetNextSym: boolean;
begin
  Result := GetSym(FSymbols.Next);
end;

procedure KLiParser.ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
var
  L: string;
begin
  if not OnHead then SymGotoNext;
  L := FModule.NewLabelName;
  CurCodes.AddRinr(L, FLast^.tk_pos);
  while not (FLast^.tk_sym in EndSyms) do
  begin
    ParseStatement(false);
    SymGotoNext;
  end;
  CurCodes.AddLabel(L, FLast^.tk_pos);
end;

procedure KLiParser.ParseBody(EndSym: KLiSymbol; OnHead: boolean);
var
  list: KLiCodes;
  G, L: PLiToken;
  X: integer;
begin
  if not OnHead then SymGotoNext;

  while FLast^.tk_sym <> EndSym do
  begin
    ParseStatement(false);
    SymGotoNext;
  end;

  list := FCurrent.FCodes;
  if (list.Count = 0) and not FCurrent.IsMainFunc then
  begin
    FCurrent.FProc := @udc_empty;
    FCurrent.SetState(fusEmpty, true);
    FCurrent.FCodes := nil;
    list.DecRefcount;
  end
  else
  for X := list.Count - 1 downto 0 do
  begin
    G := list[X];
    if G^.tk_sym = syLabel then G^.tk_prmc := X else
    if G^.tk_sym in GotoSyms then
    begin
      L := CurCodes.FindLabel(G^.tk_name);
      if L = nil then
        Error.LabelNotExists(FCurrent, G);
      G^.VLabel := L;
    end;
  end;
end;

procedure KLiParser.ParseStatement(OnHead: boolean);
  
  procedure parse_if;
  var
    bl, cl: string;
    cx: PLiToken;
  begin
    // if condition then ... else ...
    // ------------------------------
    //     TEST condition
    //     GOFP CL
    //     ....
    //     GOFP BL
    // CL:
    //     ....
    // BL:
    // ------------------------------
    bl := FModule.NewLabelName;
    repeat
      ParseExpr([syThen], false);
      cl := FModule.NewLabelName;
      cx := CurCodes.AddGoto(cl, FLast^.tk_pos);
      cx^.tk_sym := syGoFP;
      ParseBlock([syRBlock, syElse, syElif], false);
      if FLast^.tk_sym <> syRBlock then
      begin
        CurCodes.AddGoto(bl, FLast^.tk_pos);
        CurCodes.AddLabel(cl, FLast^.tk_pos);
        if FLast^.tk_sym = syElse then
          ParseBlock([syRBlock], false);
      end
      else cx^.tk_name := bl;
    until FLast^.tk_sym = syRBlock;
    CurCodes.AddLabel(bl, FLast^.tk_pos);
  end;

  procedure parse_for;
  var
    vrec: PLiToken;
    bl, cl: string;
    expr: PLiToken;
  begin
    // for varb in vargen if condition do ...
    // --------------------------------------------
    //      PUSH vargen
    //      SETV
    // CL:
    //      SEND varb
    //      GOFP BL
    //      TEST condition
    //      GOFP CL
    //      ...
    //      GOTO CL
    // BL:
    // --------------------------------------------
    SaveLabels(bl, cl, true);
    try
      SymTestNextPureID;
      vrec := FLast;
      SymTestNext([syIn]);

      CurCodes.AddRinr(FBreakLabel, FLast^.tk_pos);

      ParseExpr([syIf, syDo], false);
      CurCodes.AddNew(syVarGen, @(FLast^.tk_pos));
      CurCodes.AddNew(sySETV, @(FLast^.tk_pos));

      CurCodes.AddLabel(FContinueLabel, FLast^.tk_pos);

      expr := AddToken(vrec);
      expr^.tk_sym := sySend;
      expr := CurCodes.AddGoto(FBreakLabel, FLast^.tk_pos);
      expr^.tk_sym := syGoFP;

      if FLast^.tk_sym = syIf then
      begin
        ParseExpr([syDo], false);
        expr := CurCodes.AddGoto(FContinueLabel, FLast^.tk_pos);
        expr^.tk_sym := syGoFP;
      end;
    
      ParseBlock([syRBlock], false);
      CurCodes.AddGoto(FContinueLabel, FLast^.tk_pos);
      CurCodes.AddLabel(FBreakLabel, FLast^.tk_pos);
    finally
      RestoreLabels(bl, cl);
    end;
  end;
  
  procedure parse_while;
  var
    bl, cl: string;
  begin
    // while condition do ...
    // --------------------------------------------
    // CL:
    //      TEST condition
    //      GOFP BL
    //      ....
    //      GOTO CONTINUE_LABEL
    // BL:
    // --------------------------------------------
    SaveLabels(bl, cl, true);
    try
      CurCodes.AddLabel(FContinueLabel, FLast^.tk_pos);
      ParseExpr([syDo], false);
      CurCodes.AddGoto(FBreakLabel, FLast^.tk_pos)^.tk_sym := syGoFP;
      ParseBlock([syRBlock], false);
      CurCodes.AddGoto(FContinueLabel, FLast^.tk_pos);
      CurCodes.AddLabel(FBreakLabel, FLast^.tk_pos);
    finally
      RestoreLabels(bl, cl);
    end;
  end;

  procedure parse_repeat_until;
  var
    bl, cl, body: string;
  begin
    // repeat ... until condition
    // --------------------------------------------
    // ML:
    //      ....
    // CL:
    //      TEST condition
    //      GOFP ML
    // BL:
    // --------------------------------------------
    SaveLabels(bl, cl, true);
    try
      body := FModule.NewLabelName;
      CurCodes.AddLabel(body, FLast^.tk_pos);
      ParseBlock([syUntil], false);
      CurCodes.AddLabel(FContinueLabel, FLast^.tk_pos);
      ParseExpr([syRBlock], false);
      CurCodes.AddGoto(body, FLast^.tk_pos)^.tk_sym := syGoFP;
      CurCodes.AddLabel(FBreakLabel, FLast^.tk_pos);
    finally
      RestoreLabels(bl, cl);
    end;
  end;

  procedure parse_switch;
  var
    bl, cl: string;
    cx: PLiToken;
  begin
    // --------------------------------------------
    // switch expr
    //   case expr1: ...
    //   case ...  : ...
    //   case exprN: ...
    //   else        ...
    // --------------------------------------------
    bl := FModule.NewLabelName;
    CurCodes.AddRinr(bl, FLast^.tk_pos);
    ParseExpr([syCase], false);
    CurCodes.AddNew(sySETV, @FLast^.tk_pos);
    repeat
      ParseExpr([syDot2], false);
      CurCodes.AddNew(syCase, @(FLast^.tk_pos));
      cl := FModule.NewLabelName;
      cx := CurCodes.AddGoto(cl, FLast^.tk_pos);
      cx^.tk_sym := syGoFP;
      ParseBlock([syCase, syElse, syRBlock], false);
      if FLast^.tk_sym in [syCase, syElse] then
      begin
        CurCodes.AddGoto(bl, FLast^.tk_pos);
        CurCodes.AddLabel(cl, FLast^.tk_pos);
        if FLast^.tk_sym = syElse then
          ParseBlock([syRBlock], false);
      end
      else cx^.tk_name := bl;
    until FLast^.tk_sym  in [syRBlock];
    CurCodes.AddLabel(bl, FLast^.tk_pos);
  end;

  procedure parse_break;
  begin
    if FBreakLabel = '' then
      Error.SyntaxErr(EvBreakNoLoop, LastRow, LastCol, LastModule.Name,
        EsBreakNoLoop, LastModule.FileName, []);
    SymTestNext([syRBlock]);
    CurCodes.AddGoto(FBreakLabel, FLast^.tk_pos);
  end;

  procedure parse_continue;
  begin
    if FContinueLabel = '' then
      Error.SyntaxErr(EvContinueNoLoop, LastRow, LastCol, LastModule.Name,
        EsContinueNoLoop, LastModule.FileName, []);
    SymTestNext([syRBlock]);
    CurCodes.AddGoto(FContinueLabel, FLast^.tk_pos);
  end;

  procedure parse_return;
  begin
    SymGotoNext;
    if FLast^.tk_sym <> syRBlock then
      ParseExpr([syRBlock], true);
    CurCodes.AddNew(syReturn, @FLast^.tk_pos);
  end;

  procedure parse_try;
  var
    LL, CL: string;
    TX: PLiToken;
  begin
    // try ... except ... | try ... finally ...
    // -------------------+--------------------
    // try                | try
    //     ....           |     ....
    //     GOTO LL        |     GOTO CL
    // except             | finally
    // CL:                | CL:
    //     ....           |     ....
    // LL:                |     RAISE
    // -------------------+--------------------
    LL := FModule.NewLabelName; // leave
    CL := FModule.NewLabelName; // catch

    CurCodes.AddRinr(LL, FLast^.tk_pos);
    TX := CurCodes.AddGoto(CL, FLast^.tk_pos);
    TX^.tk_sym := syTry;

    Inc(FTryCount);
    try
      ParseBlock([syFinally, syExcept], false);
      if FLast^.tk_sym = syFinally then
      begin
        TX^.tk_prmc := 1;
        CurCodes.AddGoto(CL, FLast^.tk_pos);
      end
      else CurCodes.AddGoto(LL, FLast^.tk_pos);
    finally
      Dec(FTryCount);
    end;

    Inc(FCatchCount);
    try
      CurCodes.AddLabel(CL, FLast^.tk_pos);
      ParseBlock([syRBlock], false);
      CurCodes.AddLabel(LL, FLast^.tk_pos);
    finally
      Dec(FCatchCount);
    end;
  end;

  procedure parse_echo;
  var
    L: PLiToken;
  begin
    L := FLast;
    L^.tk_prmc := 0;
    repeat
      SymGotoNext;
      ParseExpr([syRBlock, syComma], true);
      Inc(L^.tk_prmc);
    until FLast^.tk_sym in [syRBlock];
    AddToken(L)^.tk_sym := syEcho;
  end;

  const
    OPRS = [syMul, syDiv, syMod, syAdd, syDec, syBXor,
            syBAnd, syBOr, syBShl, syBShr];

  function is_become(S1, S2: KLiSymbol): boolean;
  begin
    Result := (S1 = syBecome) or ((S2 = syBecome) and (S1 in OPRS));
  end;
  
  procedure parse_default;
  var
    R, M, L: PLiToken;
    S, X: KLiSymbol;
    expr: PLiToken;
  begin
    M := nil;
    PeekNextTwoSym(S, X);

    if token_pure_ID(FLast) and is_become(S, X) then
    begin
      R := FLast;
      SymTestNext([syBecome] + OPRS);
      S := FLast^.tk_sym;
      if S in OPRS then
      begin
        M := FLast;
        SymTestNext([syBecome]);
        AddToken(R);
      end;
      ParseExpr([syRBlock], false);
      if S in OPRS then
        AddToken(M);
      AddToken(R)^.tk_sym := syBecome;
    end
    else
    if (FLast^.tk_sym = syGetSV) and is_become(S, X) then
    begin
      FCurrent.AddSuper;
      R := FLast;
      SymGotoNext;
      S := FLast^.tk_sym;
      if S in OPRS then
      begin
        M := FLast;
        SymTestNext([syBecome]);
        AddToken(R);
      end;
      ParseExpr([syRBlock], false);
      if S in OPRS then
        AddToken(M);
      AddToken(R)^.tk_sym := sySetSV;
    end
    else
    if (FLast^.tk_sym = syID) and (S in ConstantSyms + [syID, syNot, syLBlock, syFormat, syLambda]) then
    begin
      AddToken(FLast);
      L := FLast;
      L^.tk_sym := syAsk;
      L^.tk_prmc := 1;
      repeat
        ParseExpr([syRBlock, syComma], false);
        Inc(L^.tk_prmc);
      until FLast^.tk_sym in [syRBlock];
      AddToken(L);
    end
    else
    begin
      ParseExpr([syBecome, syRBlock] + OPRS, true);
      S := FLast^.tk_sym;
      if S in [syBecome] + OPRS then
      begin
        expr := CurCodes.Last;
        if expr^.tk_sym <> syGetIV then
          Error.SymUnexpected(Self);
        if S in OPRS then
        begin
          M := FLast;
          SymTestNext([syBecome]);
          AddClone(expr);
          expr^.tk_sym := syDupLast;
          ParseExpr([syRBlock], false);
          AddToken(M);
          expr := AddClone(expr);
        end
        else
        begin
          CurCodes.FItems.Delete(CurCodes.Count - 1);
          try
            ParseExpr([syRBlock], false);
          finally
            CurCodes.Add(expr);
          end;
        end;
        expr^.tk_sym := sySetIV;
        expr^.tk_prmc := 3;
      end;
    end;
  end;

  procedure parse_syntax;
  const
    SR = [FirstKeyword..LastOper] - [syLBlock, syRBlock];
  var
    synx: KLiSyntax;
    onid: boolean;
    pair: integer;
  begin
  // {syntax {name ... } syntax }
    SymTestLast([syLBlock]);
    SymTestNextPureID;
    if FModule.Find(FLast^.tk_name) then
      Error.Redeclared(Self);

    synx := KLiSyntax.Create(FModule, FLast^.tk_name);
    onid := false;
    SymGotoNext;
    while FLast^.tk_sym <> syRBlock do
    begin
      if onid then
      begin
        SymTestLast(SR);
        onid := false;
      end
      else
      begin
        SymTestLast(SR + [syID]);
        if FLast^.tk_sym = syID then
        begin
          SymTestLastPureID;
          if 'v_' = Copy(FLast^.tk_name, 1, 2) then // locals
            Error.SymUnexpected(Self);
          onid := true;
        end
        else onid := false;
      end;
      synx.AddArgument(FLast);
      SymGotoNext;
    end;

    pair := 0;
    SymTestNext([syLBlock]);
    SymGotoNext;
    while (FLast^.tk_sym <> syRBlock) or (pair <> 0) do
    begin
      if FLast^.tk_sym = syLBlock then Inc(pair) else
      if FLast^.tk_sym = syRBlock then Dec(pair);
      synx.AddToken(FLast);
      SymGotoNext;
    end;
    SymTestNext([syRBlock]);
  end;

  procedure parse_define;
  begin
    SymGotoNext;
    if FLast^.tk_sym = syLBlock then parse_syntax else
    begin
      SymTestLastPureID;
      if FModule.Find(FLast^.tk_name) then
        Error.Redeclared(Self);
      FCurrent := KLiFunc.Create(FModule, KT_VARIANT, FLast^.tk_name, nil, nil);
      SymGotoNext;
      if FLast^.tk_sym = syBOr then
      begin
        ParseArguments(FCurrent, [syBOr], false);
        SymGotoNext;
      end
      else FCurrent.IsNameCall := true;
      ParseBody(syRBlock, true);
    end;
  end;

  procedure parse_import;
  label
    NEXT;
  var
    m_name, f_name: string;
    is_lib: boolean;
    module: KLiModule;
  begin
    kernel_lock;
    try
      SymGotoNext;
      repeat
        SymTestLastPureID;
        m_name := FLast^.tk_name;

        if FModule.FindModule(m_name, false) <> nil then goto NEXT;
        if FModule.Find(m_name) then
          Error.ImportErr(EvModuleReimport, LastRow, LastCol, LastModule.Name,
            EsModuleReimport, LastModule.FileName, [LastVal]);

        module := KLiModule(find_named(sys_libraries, m_name));
        if module <> nil then
        begin
          FModule.FModules.Add(module);
          goto NEXT;
        end;

        module := FModule.FEngine.FModules.Find(m_name);
        if module <> nil then
        begin
          if module.Parsing then
            Error.SyntaxErr(EvImportEachOther, LastRow, LastCol,
              LastModule.Name, EsImportEachOther, LastModule.FileName,
              [FModule.Name, module.Name]);
          module.AddImporter(FModule);
          FModule.FModules.Add(module);
          goto NEXT;
        end;

        is_lib := false;
        f_name := m_name;
        if not module_search(f_name, FModule.FEngine.GetSearchPath, is_lib) then
          Error.ImportErr(EvModuleNotFound, LastRow, LastCol, LastModule.Name,
            EsModuleNotFound, LastModule.FileName, [LastVal]);

        if is_lib then
        begin
          module := module_load(m_name, f_name);
          if module = nil then
            Error.ImportErr(EvWrongLibrary, LastRow, LastCol, LastModule.Name,
              EsWrongLibrary, LastModule.FileName, [LastVal]);
          FModule.FModules.Add(module);
        end
        else
        begin
          module := KLiModule.Create(m_name, FModule.FEngine, moyScript);
          module.FFileName := f_name;
          module.Parsing := true;
          KLiParser.Create(module).ParseAndFree(file_text(f_name));
          module.Parsing := false;
          module.AddImporter(FModule);
          FModule.FModules.Add(module);
        end;

        NEXT:
        SymTestNext([syID, syRBlock]);
      until FLast^.tk_sym <> syID;
    finally
      kernel_unlock;
    end;
  end;

  procedure parse_const;
  var
    data: PLiToken;
  begin
    SymTestNextPureID;
    if FModule.Find(FLast^.tk_name) then
      Error.Redeclared(Self);
    data := FLast;
    SymTestNext([syBecome]);
    FCurrent := KLiFunc.Create(FModule, KT_VARIANT, data^.tk_name, nil, nil);
    FCurrent.IsConstFunc := true;
    FCurrent.IsNameCall := true;
    ParseExpr([syRBlock], false);
    FCurrent.FCodes.AddNew(syReturn, @FLast^.tk_pos);
  end;

label MACRO_BACK;
var
  curr: KLiFunc;
  syntax: KLiSyntax;
begin
  curr := FCurrent;
  try
    if FLast^.tk_sym = syEOF then
      Error.SymNotFound(Self);

    while not OnHead and ((FLast^.tk_sym <> syLBlock) or (PeekNextSym = syBOr)) do
    begin
      ParseExpr([], true);
      if FLast^.tk_sym in [syRBlock, syElse, syElif, syCase, syEOF] then
      begin
        FTokenizer.DupCurrentToken;
        Exit;
      end;
    end;

    if not OnHead then
    begin
      SymTestLast([syLBlock]);
      SymGotoNext;
    end;

    MACRO_BACK:
    case FLast^.tk_sym of
      syDefine  : parse_define;
      syImport  : parse_import;
      syConst   : parse_const;
      syIf      : parse_if;
      syFor     : parse_for;
      syWhile   : parse_while;
      syRepeat  : parse_repeat_until;
      sySwitch  : parse_switch;
      syBreak   : parse_break;
      syContinue: parse_continue;
      syReturn  : parse_return;
      syTry     : parse_try;
      syBecome  : parse_echo;
      syDo      : ParseBlock([syRBlock], false);
      syRBlock  : AddToken(FLast)^.tk_sym := syNil;
      else begin
        if FLast^.tk_sym = syID then
        begin
          syntax := FindSyntax(FLast^.tk_name);
          if syntax <> nil then
          begin
            ExpandSyntax(syntax);
            SymGotoNext;
            goto MACRO_BACK;
          end;
        end;
        parse_default;
      end;
    end;
  finally
    FCurrent := curr;
    if FInTermCount = 0 then
      FSymbols.Clear;
  end;
end;

procedure KLiParser.SymTestLast(Syms: KLiSymbols);
begin
  if not (FLast^.tk_sym in Syms) then
    Error.SymUnexpected(Self);
end;

procedure KLiParser.SymTestLastPureID;
begin
  SymTestLast([syID]);
  if not token_pure_ID(FLast) then
    Error.SyntaxErr(EvNeedPureID, LastRow, LastCol, LastModule.Name,
      EsNeedPureID, LastModule.FileName, [LastVal]);
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

procedure KLiParser.SymGotoNext;
begin
  if not GetNextSym then
    Error.SymNotFound(Self);
end;

function KLiParser.ParseAndFree(const Code: string): KLiFunc;
begin
  try
    Result := Parse(Code);
  finally
    Free;
  end;
end;

procedure KLiParser.ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
begin
  if not OnHead then SymGotoNext;
  if not (FLast^.tk_sym in EndSym) then
  repeat
    SymTestLastPureID;
    if Func.FindInside(FLast^.tk_name) then
      Error.Redeclared(Self);
    Func.AddParam(FLast^.tk_name, KT_VARIANT);
    SymTestNext(EndSym + [syComma]);
    if FLast^.tk_sym = syComma then SymGotoNext else Exit;
  until false;
end;

procedure KLiParser.ParseExpr(EndSyms: KLiSymbols; OnHead: boolean);

  procedure parse_ask;
  var
    ask: PLiToken;
  begin
    while FLast^.tk_sym in [syLParen] do
    begin
      ask := FLast;
      ask^.tk_prmc := 1;
      SymGotoNext;
      if FLast^.tk_sym <> syRParen then
      begin
        ParseExpr([syRParen, syComma], true);
        Inc(ask^.tk_prmc);
        while FLast^.tk_sym = syComma do
        begin
          ParseExpr([syRParen, syComma], false);
          Inc(ask^.tk_prmc);
        end;
      end;
      ask^.tk_sym := syAsk;
      AddToken(ask);
      SymGotoNext;
    end;
  end;

  procedure parse_statement_in_term;
  var
    L: string;
  begin
    L := FModule.NewLabelName;
    CurCodes.AddRinr(L, FLast^.tk_pos);
    Inc(FInTermCount);
    ParseStatement(true);
    Dec(FInTermCount);
    CurCodes.AddLabel(L, FLast^.tk_pos);
  end;
  
  procedure parse_term;
  var
    L: PLiToken;
    hashed: boolean;
    syntax: KLiSyntax;
    lambda: KLiFunc;
  begin
    if FCurrent.IsLambdaFunc then
      SymTestLast(ExprHeadSyms) else
      SymTestLast(ExprHeadSyms - [syGetSV]);

    L := FLast;
    SymGotoNext;

    if (L^.tk_sym in OperIDSyms) and ((L^.tk_sym <> syDec)
      or (FLast^.tk_sym in OperIDSyms + EndSyms + ExprEndSyms)) then
      begin
        L^.tk_name := Symbols[L^.tk_sym].ID;
        L^.tk_sym := syID;
      end;

    if (L^.tk_sym = syDec) and (FLast^.tk_sym in [syFloat, syInt]) then
    begin
      L^.tk_sym := FLast^.tk_sym;
      if L^.tk_sym = syFloat then
        L^.VFloat := - FLast^.VFloat else
        L^.VInteger := - FLast^.VInteger;
      SymGotoNext;
    end;
      
    if L^.tk_sym in ConstantSyms + [syID] then
    begin
      AddToken(L);
      if L^.tk_sym in [syGetEnv, syGetSV, syID] then
      begin
        if L^.tk_sym = syGetSV then
          FCurrent.AddSuper;
        parse_ask;
      end;
    end
    else
    if L^.tk_sym = syLParen then
    begin
      ParseExpr([syRParen], true);
      SymGotoNext;
      parse_ask;
    end
    else
    if L^.tk_sym = syLBlock then
    begin
      while FLast^.tk_sym = syID do
      begin
        syntax := FindSyntax(FLast^.tk_name);
        if syntax <> nil then
        begin
          ExpandSyntax(syntax);
          SymGotoNext;
        end
        else Break;
      end;
      if FLast^.tk_sym = syBOr then // lambda
      begin
        with Shadow do
        try
          lambda := FModule.NewFunc;
          lambda.IsLambdaFunc := true;
          FCurrent := lambda;
          ParseArguments(lambda, [syBOr], false);
          ParseBody(syRBlock, false);
        finally
          Free;
        end;
        FLast^.tk_sym := syID;
        FLast^.tk_name := lambda.FullName;
        AddToken(FLast);
      end
      else
      if FLast^.tk_sym in [syImport, syDefine, syConst] then
        Error.SymUnexpected(Self) else
      if FLast^.tk_sym = syRBlock then
        AddToken(FLast)^.tk_sym := syNil else
        parse_statement_in_term;
      SymGotoNext;
      parse_ask;
    end
    else
    if L^.tk_sym = syLArray then // varlist | hashed
    begin
      hashed := false;
      L^.tk_prmc := 0;
      while FLast^.tk_sym <> syRArray do
      begin
        if L^.tk_prmc = 0 then
        begin
          ParseExpr([syDot2, syComma, syRArray], true);
          hashed := (FLast^.tk_sym = syDot2); // hashed
          if hashed then
            ParseExpr([syComma, syRArray], false);
        end
        else
        begin
          if hashed then
            ParseExpr([syDot2], false);
          ParseExpr([syComma, syRArray], false);
        end;
        Inc(L^.tk_prmc, 1 + Ord(hashed));
      end;
      if hashed then
        L^.tk_sym := syHashed else
        L^.tk_sym := syVarList;
      AddToken(L);
      SymGotoNext;
    end
    else
    if L^.tk_sym in [syNot, syDec, syBNot, syFormat] then
    begin
      if L^.tk_sym = syDec then L^.tk_sym := syNeg;
      parse_term;
      AddToken(L);
    end
    else
    begin
      FLast := L;
      Error.SymUnexpected(Self);
    end;

    while FLast^.tk_sym in [syDot, syLArray] do
    begin
      L := FLast;
      if L^.tk_sym = syDot then
      begin
        SymGotoNext;
        if not (FLast^.tk_sym in [FirstKeyword..LastKeyword]) then
          SymTestLastPureID;
        FLast^.tk_sym := syStr;
        AddToken(FLast);
      end
      else ParseExpr([syRArray], false);
      L^.tk_sym := syGetIV;
      L^.tk_prmc := 2;
      AddToken(L);
      SymGotoNext;
      parse_ask;
    end;
  end;

  procedure parse_fact(Level: integer);
  var
    J, L: PLiToken;
    X: integer;
    P: PLiToken;
  begin
    if Level > 0 then
      parse_fact(Level - 1) else
      parse_term;
    J := nil;  // jump record
    P := nil;
    X := 0;
    while (FLast^.tk_sym in ExprOperSyms[Level]) and (PeekNextSym <> syBecome) do
    begin
      L := FLast;
      if Level = High(ExprOperSyms) then  // syAnd, syOr
      begin
        J := CloneSym(L);
        if L^.tk_sym = syAnd then
          J^.tk_sym := syJmpF else
          J^.tk_sym := syJmpT;
        P := AddToken(J);
        X := CurCodes.Count - 1;
      end;
      SymGotoNext;
      if Level > 0 then
        parse_fact(Level - 1) else
        parse_term;
      AddToken(L);
      if Assigned(J) then
      begin
        P^.tk_prmc := CurCodes.Count - X;
        J := nil;
      end;
    end;
  end;

var
  expr: PLiToken;
begin
  if not OnHead then SymGotoNext;
  parse_fact(High(ExprOperSyms));
  while FLast^.tk_sym = syRefer do
  begin
    SymGotoNext;
    parse_fact(High(ExprOperSyms));
    expr := CurCodes.Last;
    if expr^.tk_sym = syAsk then
    begin
      Inc(expr^.tk_prmc);
      expr^.tk_sym := syReferAsk;
    end
    else
    begin
      expr := AddClone(expr);
      expr^.tk_sym := syReferAsk;
      expr^.tk_prmc := 2;
    end;
  end;
  if EndSyms <> [] then
    SymTestLast(EndSyms);
end;

function KLiParser.AddClone(Expr: PLiToken): PLiToken;
begin
  Result := CurCodes.AddClone(Expr);
end;

function KLiParser.AddToken(token: PLiToken): PLiToken;
begin
  Result := CurCodes.AddToken(token);
end;

function KLiParser.CloneSym(sym: PLiToken): PLiToken;
begin
  Result := FSymbols.Next;
  token_copy(sym, Result);
end;

function KLiParser.Error: KLiError;
begin
  Result := FModule.FEngine.FError;
end;

procedure KLiParser.ExpandSyntax(ASyntax: KLiSyntax);
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

  procedure read_to(const ID: string; EndSyms: KLiSymbols);
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
    while (pair > 0) or not (FLast^.tk_sym in EndSyms) do
    begin
      if FLast^.tk_sym = syLBlock then Inc(pair) else
      if FLast^.tk_sym = syRBlock then
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
        if token^.tk_sym <> syID then
        begin
          SymTestNext([token^.tk_sym]);
          if index = ASyntax.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else
        if index < ASyntax.ArgCount - 1 then
        begin
          read_to(token^.tk_name, [ASyntax.ArgToken(index + 1)^.tk_sym]);
          Inc(index);
          if index = ASyntax.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else read_to(token^.tk_name, [syRBlock]);
        Inc(index);
      end;
    end
    else SymTestNext([syRBlock]);

    // 2.put back to FTokenizer
    FTokenizer.PutBack(FLast); // sySemicolon | syRBlock
    ASyntax.RenameLocals;
    lastsym := FLast^.tk_sym;
    for index := ASyntax.FBody.Count - 1 downto 0 do
    begin
      token := PLiToken(ASyntax.FBody[index]);
      if (token^.tk_sym = syID) and (token^.tk_name[1] <> '#') and (lastsym <> syDot) then
      begin
        I := id_index(token^.tk_name);
        if I < 0 then FTokenizer.PutBack(token) else
        if lists[I].list.Count = 0 then
        begin
          token := FTokenizer.PutBack(FLast);
          token^.tk_sym := syNil;
          token^.tk_name := 'nil';
        end
        else FTokenizer.PutBack(lists[I].list);
      end
      else FTokenizer.PutBack(token);
      lastsym := token^.tk_sym;
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
  R: RLiFind;
  S, M: string;
begin
  M := '';
  S := extract_name_module(ID, M);
  if FModule.FindBy(S, M, @R) and (R.fo_type = foSyntax) then
    Result := R.VSyntax else
    Result := nil;
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
    Result := token^.tk_sym else
    Result := syError;
end;

function KLiParser.PeekNextTwoSym(var one, two: KLiSymbol): integer;
var
  A, B, C: PLiToken;
begin
  Result := FTokenizer.PeekNextThreeTokens(A, B, C);

  if Result > 0 then
    one := A^.tk_sym else
    one := syError;

  if Result > 1 then
  begin
    two := B^.tk_sym;
    Result := 2;
  end
  else two := syError;
end;

procedure KLiParser.RestoreLabels(const BreakLabel, ContinueLabel: string);
begin
  FBreakLabel := BreakLabel;
  FContinueLabel := ContinueLabel;
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

{ KLiNameObject }

constructor KLiNameObject.Create(const AName: string);
begin
  FName := AName;
end;

{ KLiNameHashed }

procedure KLiNameHashed.Clear;
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

constructor KLiNameHashed.Create(Size: cardinal);
begin
  inherited Create;
  FSize := Max(2, Size);
  SetLength(FBuckets, FSize);
  FillChar(FBuckets[0], FSize * sizeof(PLiNameItem), 0);
end;

destructor KLiNameHashed.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited;
end;

function KLiNameHashed.FindItem(const Key: string): PLiNameItem;
begin
  Result := FBuckets[HashOf(Key)];
  while Result <> nil do
  begin
    if Key = Result^.ni_nobj.FName then Exit;
    Result := Result^.ni_next;
  end;
end;

procedure KLiNameHashed.FreeItem(Item: PLiNameItem);
begin
  Dec(FCount);
  lse_mem_free(Item, sizeof(RLiNameItem));
end;

function KLiNameHashed.Get(const Key: string): KLiNameObject;
var
  M: PLiNameItem;
begin
  M := FindItem(Key);
  if M <> nil then
    Result := M^.ni_nobj else
    Result := nil;
end;

function KLiNameHashed.HashOf(const Key: string): cardinal;
begin
  Result := hash_of(Key) mod FSize;
end;

function KLiNameHashed.NewItem: PLiNameItem;
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

procedure KLiNameHashed.Put(AObj: KLiNameObject);
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

procedure KLiNameHashed.Remove(const Key: string);
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

{ KLiFunc }

procedure KLiFunc.AddParam(const VName: string; VType: KLiType);
var
  X: integer;
  V: PLiVarb;
begin
  V := lse_mem_alloc_zero(sizeof(RLiVarb));
  V^.va_name := VName;
  V^.va_type := VType;
  X := Length(FParams);
  SetLength(FParams, X + 1);
  FParams[X] := V;
end;

procedure KLiFunc.AddSuper;
var
  X: integer;
  S: PLiVarb;
begin
  X := Length(FParams);
  if (X = 0) or (FParams[0]^.va_name <> '$') then
  begin
    AddParam('$', KT_VARSNAP);
    S := FParams[X];
    if X > 0 then
    begin
      while X > 0 do
      begin
        FParams[X] := FParams[X - 1];
        Dec(X);
      end;
      FParams[0] := S;
    end;
  end;
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

  if Params <> nil then
    for A := 0 to Params.Count - 1 do
      AddParam(Params[A], KLiType(Params.Objects[A]));

  FProc := Proc;
  if FProc = nil then
  begin
    FCodes := KLiCodes.Create(Self);
    FCodes.IncRefcount;
  end;

  if FModule.FEngine <> nil then
    FModule.Engine.AddCompiled(Self);
end;

function KLiFunc.Curry(List: KLiVarList; Module: KLiModule): KLiFunc;
var
  func: KLiFunc;
  this, curr: KLiFunc_curry;
  base, index: integer;
  varb: PLiVarb;
begin
  Result := Self;
  
  if (ParamCount = 0) or IsEmptyFunc or
     (List = nil) or (List.Count = 0) then Exit;

  if IsCurryFunc then
  begin
    this := Self as KLiFunc_curry;
    func := this.FCurryFunc;
  end
  else
  begin
    this := nil;
    func := Self;
  end;

  curr := KLiFunc_curry.Create(Module, '', func);
  
  base := 0;
  if this <> nil then
    while base < this.CurryCount do
    begin
      curr.AddCurry(this.CurryData(base));
      Inc(base);
    end;

  for index := base to func.ParamCount - 1 do
    if (index - base) >= List.Count then
    begin
      varb := func.GetParam(index);
      curr.AddParam(varb^.va_name, varb^.va_type);
    end
    else curr.AddCurry(List[index - base]);

  Result := curr;
end;

function KLiFunc.Curry(Data: PLseValue; Module: KLiModule): KLiFunc;
var
  func: KLiFunc;
  this, curr: KLiFunc_curry;
  base, index: integer;
  varb: PLiVarb;
begin
  Result := Self;

  if (ParamCount = 0) or IsEmptyFunc then Exit;

  if IsCurryFunc then
  begin
    this := Self as KLiFunc_curry;
    func := this.FCurryFunc;
  end
  else
  begin
    this := nil;
    func := Self;
  end;

  curr := KLiFunc_curry.Create(Module, '', func);
  
  base := 0;
  if this <> nil then
    while base < this.CurryCount do
    begin
      curr.AddCurry(this.CurryData(base));
      Inc(base);
    end;

  curr.AddCurry(Data);

  for index := base + 1 to func.ParamCount - 1 do
  begin
    varb := func.GetParam(index);
    curr.AddParam(varb^.va_name, varb^.va_type);
  end;

  Result := curr;
end;

destructor KLiFunc.Destroy;
var
  X: integer;
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
    FCodes.DecRefcount;
    FCodes := nil;
  end;

  for X := 0 to Length(FParams) - 1 do
  begin
    FParams[X]^.va_name := '';
    lse_mem_free(FParams[X], sizeof(RLiVarb));
  end;
  SetLength(FParams, 0);

  inherited;
end;

function KLiFunc.Engine: KLiEngine;
begin
  Result := FModule.FEngine;
end;

function KLiFunc.FullName: string;
begin
  Result := FModule.Name + '::' + Name;
end;

function KLiFunc.GetParam(Index: integer): PLiVarb;
begin
  Result := FParams[Index];
end;

function KLiFunc.ParamCount: integer;
begin
  Result := Length(FParams);
end;

function KLiFunc.Prototype: string;
var
  X: integer;
begin
  Result := FResultType.Prototype(Name);
  if IsConstFunc then
    Result := '{const ' + Result else
    Result := '{def ' + Result;
  if not IsNameCall then
  begin
    Result := Result + ' |';
    if Self.ParamCount > 0 then
    begin
      Result := Result + GetParam(0)^.va_type.Prototype(GetParam(0)^.va_name);
      for X := 1 to Self.ParamCount - 1 do
        Result := Result + ', ' + GetParam(X)^.va_type.Prototype(GetParam(X)^.va_name);
    end;
    Result := Result + '|';
  end;
  Result := Result + '}';
end;

procedure KLiFunc.DumpCode(list: TStrings; const margin: string);

  function Add(const S: string): integer;
  begin
    Result := list.Add(margin + S);
  end;

var
  P: string;
begin
  if FCodes <> nil then
  begin
    P := Prototype;
    Add(Copy(P, 1, Length(P) - 1));
    FCodes.DumpCode(list, margin);
    Add('}');
  end
  else Add(Prototype);
end;

function KLiFunc.FindBy(const ID: string; rec: PLiFind; Range: KLiFindObjects): boolean;
var
  o_name, m_name: string;
begin
  Result := (ID <> '');
  if Result then
  begin
    o_name := extract_name_module(ID, m_name);
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
  if (Result = nil) or not Result.FResultType.Match(AType) then
  begin
    Result := AType.FModule.FindFunc(AType.Name + '_create');
    if (Result <> nil) and not Result.FResultType.Match(AType) then
      Result := nil;
  end;
end;

function KLiFunc.FindMethod(const AName: string; AType: KLiType): KLiFunc;
var
  N: string;
  M, T: KLiModule;
  F: KLiFunc;
  X: integer;
  
  function match_class(func: KLiFunc): boolean;
  begin
    Result := (func <> nil) and
              (func.ParamCount > 0) and
              func.GetParam(0)^.va_type.Match(AType);
  end;
  
begin
  N := AType.Name + '_' + AName;
  M := Module;

  F := M.FindFunc(N);
  if match_class(F) then
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
        if match_class(F) then
        begin
          Result := F;
          Exit;
        end;
      end;
    end;
    if M.FModules.Find('sys') = nil then
    begin
      F := sys_module.FindFunc(N);
      if match_class(F) then
      begin
        Result := F;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function KLiFunc.FindParam(const VName: string): PLiVarb;
var
  X: integer;
begin
  for X := 0 to Length(FParams) - 1 do
    if FParams[X]^.va_name = VName then
    begin
      Result := FParams[X];
      Exit;
    end;
  Result := nil;
end;

function KLiFunc.HasState(Index: KLiFuncState): boolean;
begin
  Result := (Index in FState);
end;

function KLiFunc.HasSuper: boolean;
begin
  Result := (Length(FParams) > 0) and (FParams[0]^.va_name = '$');
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

function KLiFunc.FindInside(const ID: string; rec: PLiFind): boolean;
var
  findrec: RLiFind;
begin
  if rec = nil then rec := @findrec;
  if ID = Name then
  begin
    rec^.fo_type := foFunc;
    rec^.VFunc := Self;
  end
  else
  begin
    rec^.VVarb := FindParam(ID);
    if rec^.VVarb <> nil then
      rec^.fo_type := foVarb else
      rec^.fo_type := foNone;
  end;
  Result := (rec^.fo_type <> foNone);
end;

{ KLiCodes }

procedure KLiCodes.Add(AExprRec: PLiToken);
begin
  FItems.Add(AExprRec);
end;

function KLiCodes.AddGoto(const Name: string; Pos: RLiPos): PLiToken;
begin
  Result := FFunc.FCodes.AddNew(syGoto, @Pos);
  Result^.tk_name := Name;
end;

function KLiCodes.AddLabel(const Name: string; const Pos: RLiPos): PLiToken;
begin
  Result := FFunc.FCodes.AddNew(syLabel, @Pos);
  Result^.tk_prmc := -1;
  Result^.tk_name := Name;
end;

procedure KLiCodes.Clear(Sender: TObject);
var
  X: integer;
begin
  try
    for X := 0 to FItems.Count - 1 do
      token_free(FItems[X]);
  finally
    FItems.Clear;
  end;
end;

function KLiCodes.AddClone(Expr: PLiToken): PLiToken;
var
  S: string;
begin
  Result := token_new;
  Add(Result);
  S := Expr^.tk_name;
  Expr^.tk_name := '';
  Result^ := Expr^;
  Expr^.tk_name := S;
  Result^.tk_name := S;
end;

constructor KLiCodes.Create(AFunc: KLiFunc);
begin
  FFunc := AFunc;
  FItems := TList.Create;
end;

destructor KLiCodes.Destroy;
begin
  Clear(nil);
  FreeAndNil(FItems);
  inherited;
end;

procedure KLiCodes.DumpCode(List: TStrings; const Margin: string);
var
  A: integer;
  R: PLiToken;
  L: TList;
  H: string;

  function Log(const S: string): integer;
  begin
    Result := List.Add(Margin + S);
  end;

begin
  L := TList.Create;
  try
    for A := 0 to GetCount - 1 do
    begin
      R := GetItem(A);
      if R^.tk_sym in [syJump, syJmpF, syJmpT, syJmpFP, syJmpTP] then
        L.Add(pointer(A + R^.tk_prmc));
    end;

    for A := 0 to GetCount - 1 do
    begin
      R := GetItem(A);
      case R^.tk_sym of
        syReturn    : if R^.tk_prmc > 0 then
                        H := 'RETL' else // return last
                        H := 'EXIT';
        syJump      : H := Format('JUMP &%.4d:', [A + R^.tk_prmc]);
        syJmpF      : H := Format('JMPF &%.4d:', [A + R^.tk_prmc]);
        syJmpT      : H := Format('JMPT &%.4d:', [A + R^.tk_prmc]);
        syJmpFP     : H := Format('JMPF &%.4d: POP', [A + R^.tk_prmc]);
        syJmpTP     : H := Format('JMPT &%.4d: POP', [A + R^.tk_prmc]);
        syAsk       : H := Format('CASK %d', [R^.tk_prmc]);
        syReferAsk  : H := Format('RASK %d', [R^.tk_prmc]);
        syIdle      : H := 'IDLE';
        syID        : H := Format('PUSH %s', [R^.tk_name]);
        syBecome    : H := Format('SAVE %s', [R^.tk_name]);
        syFloat     : H := Format('PSHF %f', [R^.VFloat]);
        syInt       : H := Format('PSHI %d', [R^.VInteger]);
        syStr       : H := Format('PSHS %s', [str_to_comma(R^.tk_name)]);
        syTry       : if R^.tk_prmc > 0 then
                        H := Format('TRYF %s', [R^.tk_name]) else
                        H := Format('TRYC %s', [R^.tk_name]);
        syEcho      : H := Format('ECHO %d', [R^.tk_prmc]);
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
        syVarList   : H := Format('LIST %d', [R^.tk_prmc]);
        syNil       : H := 'PUSH NIL';
        syGetEnv    : H := Format('PUSH ${%s}', [R^.tk_name]);
        syGetSV     : H := Format('PUSH $%s', [R^.tk_name]);
        sySetSV     : H := Format('SAVE $%s', [R^.tk_name]);
        syFormat    : H := 'FRMT';
        syLabel     : H := R^.tk_name;
        syGoto      : H := Format('GOTO %s', [R^.tk_name]);
        syGoTP      : H := Format('GOTP %s', [R^.tk_name]);
        syGoFP    : H := Format('GOFP %s', [R^.tk_name]);
        syIs        : H := 'CALC IS';
        syAs        : H := 'CALC AS';
        syVarGen    : H := 'VGEN';
        syHashed    : H := Format('HASH %d', [R^.tk_prmc]);
        syRINR      : H := Format('RINR %s', [R^.tk_name]);
        syGETV      : H := 'GETV';
        sySETV      : H := 'SETV';
        syLike      : H := 'LIKE';
        syGetIV     : H := 'GETI';
        sySetIV     : H := 'SETI';
        syDupLast   : H := 'DUPL';
        sySend      : H := Format('SEND %s', [R^.tk_name]);
        syCase      : H := 'CASE';
        sySTMT      : H := 'STMT';
        else          H := 'WRNG: ' + Symbols[R^.tk_sym].SM;
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

function KLiCodes.FindLabel(const Name: string): PLiToken;
var
  X: integer;
begin
  for X := 0 to GetCount - 1 do
  begin
    Result := GetItem(X);
    if Result^.tk_sym = syLabel then
      if Result^.tk_name = Name then
        Exit;
  end;
  Result := nil;
end;

function KLiCodes.GetCount: integer;
begin
  if Self <> nil then
    Result := FItems.Count else
    Result := 0;
end;

function KLiCodes.GetItem(Index: integer): PLiToken;
begin
  Result := PLiToken(FItems[Index]);
end;

function KLiCodes.GetLast: PLiToken;
begin
  Result := PLiToken(FItems.Last);
end;

function KLiCodes.AddToken(token: PLiToken): PLiToken;
begin
  Result := token_new;
  Add(Result);
  token_copy(token, Result);
end;

function KLiCodes.AddNew(sym: KLiSymbol; SymPos: PLiPos): PLiToken;
begin
  Result := token_new;
  Result^.tk_sym := sym;
  if SymPos <> nil then
    Result^.tk_pos := SymPos^;
  Add(Result);
end;

function KLiCodes.AddRinr(const Name: string; Pos: RLiPos): PLiToken;
begin
  Result := AddGoto(Name, Pos);
  Result^.tk_sym := syRINR;
end;

{ KLiFunc_curry }

function KLiFunc_curry.AddCurry(value: PLseValue): integer;
begin
  Result := Length(FCurry);
  SetLength(FCurry, Result + 1);
  FCurry[Result] := value_new;
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

function KLiFunc_curry.CurryCount: integer;
begin
  Result := Length(FCurry);
end;

function KLiFunc_curry.CurryData(Index: integer): PLseValue;
begin
  Result := FCurry[Index];
end;

destructor KLiFunc_curry.Destroy;
begin
  GarbageCollect;
  Engine.OrLeave(@FObjRec);
  inherited;
end;

procedure KLiFunc_curry.GarbageCollect;
var
  index: integer;
begin
  if FCurryFunc <> nil then
  begin
    FCurryFunc.DecRefcount;
    FCurryFunc := nil;
  end;

  for index := Length(FCurry) - 1 downto 0 do
  begin
    value_free(FCurry[index]);
    FCurry[index] := nil;
  end;
  
  SetLength(FCurry, 0);
end;

function KLiFunc_curry.ObjRec: PLiObjRec;
begin
  Result := @FObjRec;
end;

{ KLiFunc_oper }

constructor KLiFunc_oper.Create(AOper: KLiSymbol);
begin
  inherited Create(sys_module, KT_VARIANT,
    Symbols[AOper].ID, nil, @udc_oper);
  AddParam('V1', KT_VARIANT);
  AddParam('V2', KT_VARIANT);
  FOper := AOper;
end;

{ KLiType }

procedure KLiType.Cast(V: PLseValue; E: KLiEngine);
var
  T: KLiType;
  G: PLseVargen;
begin
  if Self = KT_VARIANT then Exit;
  
  if Self = KT_VOID then
  begin
    lse_clear_value(V);
    Exit;
  end;
  
  if Self = KT_VARGEN then
  begin
    lse_set_vargen(V, lse_vargen_ensure(value_vargen(V, E)));
    Exit;
  end;

  T := value_type(V);
  
  if Self = KT_CLASS then
  begin
    if T <> KT_CLASS then
      T.SaveTo(V);
    Exit;
  end;

  if T = KT_VARGEN then
  begin
    G := PLseVargen(V^.VObject);
    if G <> nil then
      lse_vargen_send(G, V) else
      lse_clear_value(V);
    T := value_type(V);
  end;

  if T = Self then Exit;

  if Self = KT_STRING then lse_set_string(V, value_str(V)) else
  if Self = KT_INT    then lse_set_int64(V, value_int(V)) else
  if Self = KT_FLOAT  then lse_set_float(V, value_float(V)) else
  if T = KT_STRING then
    lse_set_object(V, TypeRec, StrecToObject(V^.VObject, E)) else
    lse_set_object(V, TypeRec, nil);
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

procedure KLiType.Default(V: PLseValue);
begin
  case FTypeRec^.cr_type of
    LSV_STRING : lse_set_string(V, '');
    LSV_INT    : lse_set_int64(V, 0);
    LSV_FLOAT  : lse_set_float(V, 0);
    LSV_OBJECT : lse_set_object(V, FTypeRec, nil);
    else         lse_clear_value(V);
  end;
end;

destructor KLiType.Destroy;
begin
  sys_cik_entries.cik_types^[TLseKernelType(Ord(DataType))] := nil;
  remove_from(Module.FTypeList, Self);
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

function KLiType.Match(AType: KLiType): boolean;
begin
  Result := (Self = AType) or (Self = KT_VARIANT);
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
  if Self <> KT_VARIANT then
    Result := ID + ':' + Name else
    Result := ID;
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
  FArgs[X] := token_clone(Arg);
end;

procedure KLiSyntax.AddToken(Token: PLiToken);
var
  lastsym: KLiSymbol;
begin
  Token := token_clone(Token);
  FBody.Add(Token);
  if token_pure_ID(Token) and ('v_' = Copy(Token^.tk_name, 1, 2)) then
  begin
    if FBody.Count > 1 then
      lastsym := PLiToken(FBody[FBody.Count - 2])^.tk_sym else
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
    token_free(FArgs[X]);
  SetLength(FArgs, 0);

  for X := FBody.Count - 1 downto 0 do
    token_free(PLiToken(FBody[X]));
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
      T^.tk_next := nil;
    end;

    Inc(FRenameCount);

    for X := FLocals.Count - 1 downto 0 do
    begin
      T := PLiToken(FLocals[X]);
      if T^.tk_next = nil then
      begin
        T^.tk_next := T;
        org_name := T^.tk_name;
        new_name := Format('#%p_%X', [T, FRenameCount]);
        T^.tk_name := new_name;
        for I := X - 1 downto 0 do
        begin
          T := PLiToken(FLocals[I]);
          if (T^.tk_next = nil) and (T^.tk_name = org_name) then
          begin
            T^.tk_next := T;
            T^.tk_name := new_name;
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
  Result := value_new;
  FItems.Add(Result);
end;

function KLiVarList.AddDefault(Klass: PLseType): PLseValue;
begin
  Result := value_new;
  Result^.vtype := Klass;
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: double): PLseValue;
begin
  Result := value_new(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: int64): PLseValue;
begin
  Result := value_new(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: pointer; Klass: PLseType): PLseValue;
begin
  Result := value_new(Value, Klass);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: PLseString): PLseValue;
begin
  Result := value_new(Value);
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
  Result := value_new(Value);
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
    lse_stream_write(S, value_str(V));
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
  value_free(Pop(Index));
end;

procedure KLiVarList.DeleteLast;
begin
  value_free(Pop);
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
  Result := value_new;
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
  Result := value_new(Value);
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

constructor KLiModule.Create(const MName: string; MEngine: KLiEngine; MType: KLiModuleType);
begin
  inherited Create(MName);
  IncRefcount;
  FFileName := MName;
  FModuleType := MType;
  if FModuleType = moyKernel then
  begin
    FFileName    := sys_kernel;
    FVersion     := sys_version;
    FDescription := Format('builtin %s module', [Name]);
  end;
  FTypeList := new_named_list(true);
  if MName = 'sys' then
    FFuncList := KLiNameHashed.Create(16) else
    FFuncList := KLiNameHashed.Create(4);
  if FModuleType = moyScript then
  begin
    FEngine := MEngine;
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
      remove_from(P.FModules.FModules, Self);
    end;
    remove_from(FEngine.FModules.FModules, Self);
    if Self = FEngine.FMainModule then
      FEngine.FMainModule := nil;
    FreeAndNil(FModules);
    FreeAndNil(FImporters);
  end;
  DeleteFunctions;
  FreeAndNil(FFuncList);
  release_string_object_list(FTypeList);
  remove_from(sys_libraries, Self);
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
    WriteText('{import');
    lse_stream_write(stream, ' ' + FModules[2].Name);
    for A := 3 to FModules.Count - 1 do
      lse_stream_write(stream, ' ' + FModules[A].Name);
    lse_stream_write(stream, '}');
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
        Result := KLiModule(find_named(sys_libraries, ID));
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
  Result := KLiType(find_named(FTypeList, ID));
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
        if is_ID(pchar(ID)) then
        begin
          VT := FindTypeBy(Trim(Copy(S, X + 1, Length(S))), '');
          if VT <> nil then Exit;
        end;
      end
      else
      begin
        ID := Trim(S);
        if is_ID(pchar(ID)) then
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
          (Result.FindParam(p_name) <> nil) or
          (Result.ParamCount = LSE_MAX_PARAMS) then
        begin
          FreeAndNil(Result);
          Exit;
        end;
        Result.AddParam(p_name, p_type);
      until endc = '|';
      if Result.ParamCount = 0 then
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
    and is_ID(TR^.cr_name)
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

function KLiModule.Find(const ID: string; rec: PLiFind): boolean;
var
  FR: RLiFind;
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

function KLiModule.FindBy(const ID, module_name: string; rec: PLiFind): boolean;
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
  Result := KLiVarList.Create(Engine);
  for index := 0 to GetCount - 1 do
    Result.Add(GetModule(index), KR_MODULE);
end;

{ KLiError }

procedure KLiError.Clear;
begin
  Write('', 0, 0, 0, '', '', '');
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

function KLiError.ErrorModule(func: KLiFunc; expr: PLiToken): KLiModule;
begin
  Result := KLiModule(expr^.tk_pos.module);
  if Result = nil then
    Result := func.FModule;
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
  FErrID := Name;
  FMsg := Msg;
  FModule := Module;
  FModuleFile := FileName;
  FRow := Row;
  FCol := Col;
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

procedure KLiError.LabelNotExists(func: KLiFunc; expr: PLiToken);
var
  M: KLiModule;
begin
  M := ErrorModule(func, expr);
  with expr^ do
    SyntaxErr(EvLabelNotExists, tk_pos.row, tk_pos.col, M.Name,
      EsLabelNotExists, M.Name, [tk_name]);
end;

{ KLiEngine }

procedure KLiEngine.Clear;
begin
  engine_lock(Self);
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
    engine_unlock(Self);
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
      if not same_fname(new_fname, old_fname) then
        if not same_fname(new_fname, sys_kernel) then
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

  FExitResult := value_new;
  
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

  FTempValues := KLiVarList.Create(Self);
  FTempValues.IncRefcount;
end;

destructor KLiEngine.Destroy;
begin
  Clear;

  free_and_nil(FMainValues);
  free_and_nil(FTempValues);
  free_and_nil(FMainSnap);

  SetInputStream(nil);
  SetOutputStream(nil);
  SetErrputStream(nil);

  value_free(FExitResult);
  FExitResult := nil;

  free_and_nil(FMainModule);
  free_and_nil(FModules);
  free_and_nil(FError);
  free_and_nil(FArguments);
  inherited;
end;

function KLiEngine.DoCompile(const Code: string): KLiFunc;
var
  module: KLiModule;

  procedure roll_back;
  var
    X: integer;
    A: TLseObject;
  begin
    try
      X := FCompiledObjects.Count - 1;
      while X >= 0 do
      begin
        A := TLseObject(FCompiledObjects[X]);
        FCompiledObjects.Delete(X);
        A.Free;
        X := FCompiledObjects.Count - 1;
      end;
    finally
      FreeAndNil(FCompiledObjects);
    end;
  end;

begin
  check(FCompiledObjects = nil, 'invalid embeded compiling');
  try
    FError.Clear;
    if FMainRunner <> nil then
      module := FMainRunner.CurrentFunc.FModule else
      module := FMainModule;
    FCompiledObjects := TList.Create;
    Result := KLiParser.Create(module).ParseAndFree(Code);
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
    Result := TryCompileCode(file_text(fname));
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
  Result := FMainSnap;
end;

function KLiEngine.GetResultText: string;
begin
  Result := value_str(FExitResult);
end;

function KLiEngine.GetResultType: KLiType;
begin
  Result := value_type(FExitResult);
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
      lse_set_string(Value, kernel_read_config(Name));
  end;
end;

procedure KLiEngine.Go;
begin
  try
    check(FReady, 'The engine is not ready to run');
    PrepareCompile;
    Reset(false);
    FError.Clear;
    FExited := false;
    BeginExecute;
    try
      FMainRunner := KLiRunner.Create(Self);
      try
        FMainRunner.Goon(FMainFunc, 0, FExitResult);
        if not FMainRunner.FExcepted then
          FError.Clear;
      finally
        FreeAndNil(FMainRunner);
      end;
    finally
      engine_lock(Self);
      try
        EndExecute;
        FMainFunc.FCodes.Clear(FMainSnap);
        Reset(false);
      finally
        engine_unlock(Self);
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
          KLiFunc_curry(or_object).GarbageCollect;
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
    clss := value_type(VD);
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
      FMainSnap.Reset;
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
  check(not Running, 'the engine is running');
end;

function KLiEngine.ReadValue(const Name: string): string;
var
  data: PLseValue;
begin
  try
    data := FMainValues.FindValue(Name);
    if data <> nil then
      Result := value_str(data) else
    if Name = 'search' then
      Result := GetSearchPath else
    if Name = 'mainfile' then
      Result := FMainFile else
      Result := kernel_read_config(Name);
  except
    Result := '';
  end;
end;

procedure KLiEngine.AddCompiled(AObject: TLseObject);
begin
  if FCompiledObjects <> nil then
    FCompiledObjects.Add(AObject);
end;

{ KLiVarSnap }

constructor KLiVarSnap.Create(Func: KLiFunc);
var
  X: integer;
begin
  inherited Create(Func.Engine);
  IncRefcount;
  SetLength(FNames, Func.ParamCount);
  for X := 0 to Func.ParamCount - 1 do
  begin
    FNames[X] := Func.GetParam(X)^.va_name;
    Add;
  end;
end;

destructor KLiVarSnap.Destroy;
var
  X: integer;
begin
  for X := 0 to Length(FNames) - 1 do
    FNames[X] := '';
  SetLength(FNames, 0);
  inherited;
end;

function KLiVarSnap.GetValue(const Name: string): PLseValue;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X >= 0 then
    Result := GetData(X) else
    Result := nil;
end;

function KLiVarSnap.IndexOf(const Name: string): integer;
var
  X: integer;
begin
  for X := Length(FNames) - 1 downto 0 do
    if Name = FNames[X] then
    begin
      Result := X;
      Exit;
    end;
  Result := -1;
end;

function KLiVarSnap.NewValue(const Name: string): PLseValue;
var
  X: integer;
begin
  X := IndexOf(Name);
  if X < 0 then
  begin
    X := Length(FNames);
    SetLength(FNames, X + 1);
    FNames[X] := Name;
    Result := Add;
  end
  else Result := GetData(X);
end;

procedure KLiVarSnap.Reset;
var
  X: integer;
begin
  for X := GetCount - 1 downto 0 do
    lse_clear_value(GetData(X));
end;

procedure KLiVarSnap.SetValue(const Name: string; V: PLseValue);
var
  X: integer;
begin
  X := IndexOf(Name);
  if X < 0 then
  begin
    X := Length(FNames);
    SetLength(FNames, X + 1);
    FNames[X] := Name;
    Add(V);
  end
  else lse_set_value(GetData(X), V);
end;

function KLiVarSnap.Super: KLiVarSnap;
begin
  if (Length(FNames) > 0) and (FNames[0] = '$') then
    Result := KLiVarSnap(GetData(0)^.VObject) else
    Result := nil;
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
    for X := 0 to cs^.snap^.func.ParamCount - 1 do //.FActualParamCount - 1 do
      Result.Add(cs^.snap^.values[X]);
  end;
end;

function KLiCallStack.GetItem(Index: integer): PLiCallSnap;
begin
  if (Index < 0) or (Index >= FCount) then
    lse_error('call stack index (%d) out of bounds (%d)', [Index, FCount]);
  Result := PLiCallSnap(FStack[Index]);
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
                         FExprrec^.tk_pos.Row,
                         FExprrec^.tk_pos.Col,
                         CurrentModule.Name,
                         ErrorStr,
                         CurrentModule.FileName);
    FExcepted := true;
  end;
end;

constructor KLiRunner.Create(Engine: KLiEngine);
begin
  FEngine := Engine;
  engine_lock(FEngine);
  try
    FStackBase := 0;
    FStack := KLiVarList.Create(FEngine);
    FCallStack := KLiCallStack.Create(Self);
    FEngine.OrLeave(@(FStack.FObjRec));
  finally
    engine_unlock(FEngine);
  end;
end;

function KLiRunner.CurrentFunc: KLiFunc;
begin
  if FCurrent <> nil then
    Result := FCurrent^.func else
    Result := nil;
end;

function KLiRunner.Regexpr: PLiRegexpr;
begin
  Result := @FRegexpr;
end;

function KLiRunner.CurrentModule: KLiModule;
begin
  if FCurrent <> nil then
    Result := FCurrent^.func.FModule else
    Result := nil;
end;

destructor KLiRunner.Destroy;
begin
  engine_lock(FEngine);
  try
    FCallStack.Free;
    FStack.Free;
  finally
    engine_unlock(FEngine);
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
  
  function invoke_func: boolean;
  begin
    if Assigned(func.FModule.FInvokeProc) then
      func.FModule.FInvokeProc(TLseFuncInvoke(func.FProc), @call) else
      TLseFuncCall(func.FProc)(@call);
    Result := not (Terminated or FExcepted);
    if Result then
      func.FResultType.Cast(call.p_result, FEngine) else
      FExcepted := true;
  end;

begin
  try
    index := ParamCount - Func.ParamCount;
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
        lse_set_value(FStack.Insert(base + index), curry.CurryData(index));
      Inc(ParamCount, count);
      func := curry.FCurryFunc;
    end
    else count := 0;

    for index := count to func.ParamCount - 1 do
    begin
      clss := func.GetParam(index)^.va_type;
      if index < ParamCount then
        clss.Cast(FStack[base + index], FEngine) else
        clss.Default(FStack.Add);
    end;

    if Func.FProc <> nil then
    begin
      call.p_count := 0;
      call.p_result := nil;
      call.p_func := func;
      call.p_runner := Self;
      call.p_exprec := FExprrec;
      for index := 0 to Func.ParamCount - 1 do
        call.p_param[index] := FStack[index + base];
      call.p_count := ParamCount;
      FCallStack.Push(PLseParam(@call));
      try
        if Output <> nil then
        begin
          lse_clear_value(Output);
          call.p_result := Output;
          Result := invoke_func;
          if Result then
            FStack.SetCount(base);
        end
        else
        begin
          lse_init_value(@data);
          call.p_result := @data;
          Result := invoke_func;
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
        snap.values.IncRefcount;
      end
      else
      begin
        snap.values := KLiVarSnap.Create(func);
        for index := 0 to Func.ParamCount - 1 do
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
            if FStack.Count > base then
            begin
              lse_set_value(snap.output, FStack[-1]);
              snap.outype.Cast(snap.output, FEngine);
            end
            else snap.outype.Default(snap.output);
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
  with FRegexpr do
    if mp_result.mr_str <> nil then
    begin
      Result := KLiVarList.Create(FEngine);
      Result.Add(mp_result.mr_str - mp_source);
      Result.Add(mp_result.mr_len);
      for index := 0 to mp_level - 1 do
      begin
        Result.Add(mp_captures[index].mr_str - mp_source);
        Result.Add(mp_captures[index].mr_len);
      end;
    end;
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
    next := seek_ch(base, ['%', #0]);
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
      Result := Result + value_str(ValueAt(StrToInt(temp)));
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
        'd': Result := Result + SysUtils.Format(temp, [value_int(data)]);
        'u': Result := Result + SysUtils.Format(temp, [value_int(data)]);
        'e': Result := Result + SysUtils.Format(temp, [value_float(data)]);
        'f': Result := Result + SysUtils.Format(temp, [value_float(data)]);
        'g': Result := Result + SysUtils.Format(temp, [value_float(data)]);
        'n': Result := Result + SysUtils.Format(temp, [value_float(data)]);
        'm': Result := Result + SysUtils.Format(temp, [value_float(data)]);
        'p': Result := Result + SysUtils.Format(temp, [value_int(data)]);
        'c',
        's': Result := Result + SysUtils.Format(temp, [value_str(data)]);
        'x': Result := Result + SysUtils.Format(temp, [value_int(data)]);
        else ErrorFmt;
      end;
      Inc(index);
    end;
    base := next + 1;
  until (base^ = #0);
end;

function KLiRunner.GetValue(varb: PLiVarb): PLseValue;
begin
  Result := FCurrent^.values.GetValue(varb^.va_name);
end;

procedure KLiRunner.Terminate;
begin
  FTerminated := true;
end;

function KLiRunner.GetString(const ID: string): string;
var
  vname, mname: string; // variant & module name
  R: RLiFind;
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
      else Result := value_str(@V);
    finally
      lse_set_nil(@V);
    end;
  end;
  
begin
  Result := '';
  curr := CurrentFunc;
  vname := extract_name_module(ID, mname);
  found := ((mname = '') and (System.Pos('::', ID) < 1) and
           curr.FindInside(vname, @R)) or
           curr.FModule.FindBy(vname, mname, @R);
  if not found then Exit;

  // 1. variable
  if R.fo_type = foVarb then
  begin
    Result := value_str(GetValue(R.VVarb));
    Exit;
  end;

  // 2. function
  if R.fo_type = foFunc then
  begin
    if R.VFunc.IsNameCall then
      Result := smart_call_string(R.VFunc) else
      Result := R.VFunc.Prototype;
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
    sys_runner_procs[FExprrec^.tk_sym](Self);
  except
    ErrorRT(lse_exception_str);
  end;
  Result := not FExcepted and not FTerminated and HasNext;
end;

{ KLiHashTable }

procedure KLiHashTable.Clear;
var
  index: integer;
  base, next: PLiHashItem;
begin
  for index := 0 to FBucketSize - 1 do
  begin
    base := FBucketList[index].hp_head;
    while base <> nil do
    begin
      next := base^.hi_next;
      FreeItem(base);
      base := next;
      Dec(FCount);
    end;
    FBucketList[index].hp_head := nil;
    FBucketList[index].hp_tail := nil;
  end;
  FCount := 0;
end;

constructor KLiHashTable.Create(Size: integer);
begin
  inherited Create;
  FCount := 0;
  FBucketSize := Max(1, Size);
  SetLength(FBucketList, FBucketSize);
  FillChar(FBucketList[0], FBucketSize * sizeof(RLiHashPair), 0);
end;

destructor KLiHashTable.Destroy;
begin
  Clear;
  SetLength(FBucketList, 0);
  inherited;
end;

function KLiHashTable.DoGet(const Key: string): pointer;
var
  hash: PLiHashItem;
begin
  hash := Find(Key);
  if hash <> nil then
    Result := hash^.hi_data else
    Result := nil;
end;

procedure KLiHashTable.DoPut(const Key: string; Value: pointer);
var
  item: PLiHashItem;
begin
  item := NewItem;
  string(item^.hi_key) := Key;
  item^.hi_data := Value;
  with FBucketList[HashOf(Key)] do
  begin
    if hp_tail <> nil then
      hp_tail^.hi_next := item else
      hp_head := item;
    hp_tail := item;
  end;
  Inc(FCount);
end;

function KLiHashTable.EnumKeyData(Proc: KLiEnumKeyData; Param: pointer): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  Result := 0;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      Proc(string(item^.hi_key), item^.hi_data, Param);
      item := item^.hi_next;
      Inc(Result);
    end;
  end;
end;

function KLiHashTable.Find(const Key: string): PLiHashItem;
begin
  Result := FBucketList[HashOf(Key)].hp_head;
  while Result <> nil do
  begin
    if MatchKey(Key, string(Result^.hi_key)) then Exit;
    Result := Result^.hi_next;
  end;
end;

procedure KLiHashTable.FreeItem(Item: PLiHashItem);
begin
  string(Item^.hi_key) := '';
  lse_mem_free(Item, sizeof(RLiHashItem));
end;

function KLiHashTable.HashOf(const Key: string): cardinal;
begin
  if FBucketSize = 1 then Result := 0 else
  if FIgnoreCase then
    Result := hash_of(LowerCase(Key)) mod cardinal(FBucketSize) else
    Result := hash_of(Key) mod cardinal(FBucketSize);
end;

function KLiHashTable.IsSet(const Key: string): boolean;
begin
  Result := (Find(Key) <> nil);
end;

function KLiHashTable.ListKey(List: TStrings): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  List.Clear;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      List.Add(string(item^.hi_key));
      item := item^.hi_next;
    end;
  end;
  Result := List.Count;
end;

function KLiHashTable.ListData(List: TList): integer;
var
  next: integer;
  item: PLiHashItem;
begin
  List.Clear;
  for next := 0 to FBucketSize - 1 do
  begin
    item := FBucketList[next].hp_head;
    while item <> nil do
    begin
      List.Add(item^.hi_data);
      item := item^.hi_next;
    end;
  end;
  Result := List.Count;
end;

function KLiHashTable.MatchKey(const Key, ID: string): boolean;
begin
  if FIgnoreCase then
    Result := AnsiSameText(Key, ID) else
    Result := AnsiSameStr(Key, ID);
end;

function KLiHashTable.NewItem: PLiHashItem;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiHashItem)); 
end;

procedure KLiHashTable.Remove(const Key: string);
var
  item, root: PLiHashItem;
begin
  root := nil;
  with FBucketList[HashOf(Key)] do
  begin
    item := hp_head;
    while item <> nil do
    begin
      if MatchKey(Key, string(item^.hi_key)) then
      begin
        if root <> nil then
          root^.hi_next := item^.hi_next else
          hp_head := item^.hi_next;
        if hp_tail = item then
          hp_tail := root;
        lse_mem_free(item, sizeof(RLiHashItem));
        Dec(FCount);
        Exit;
      end;
      root := item;
      item := item^.hi_next;
    end;
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
    Result := value_new;
    DoPut(Key, Result);
  end
  else Result := hash^.hi_data;
end;

procedure KLiHashed.FreeItem(Item: PLiHashItem);
begin
  if Item^.hi_data <> nil then
  begin
    value_free(Item^.hi_data);
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

{ KLiSpinLock }

constructor KLiSpinLock.Create;
begin
  FCriticalSection := SyncObjs.TCriticalSection.Create;
end;

destructor KLiSpinLock.Destroy;
begin
  FreeAndNil(FCriticalSection);
end;

procedure KLiSpinLock.Enter;
begin
  FCriticalSection.Enter;
end;

procedure KLiSpinLock.Leave;
begin
  FCriticalSection.Leave;
end;

function KLiSpinLock.TryEnter: boolean;
begin
  {$IFDEF FPC}
  FCriticalSection.Enter;
  Result := true;
  {$ELSE}
  Result := FCriticalSection.TryEnter;
  {$ENDIF}
end;

{ KLiMD5 }

procedure KLiMD5.FF(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + ((b^ and c^) or ((not b^) and d^)) + x^ + ac, s) + b^;
end;

function KLiMD5.GetDigest: string;

  function VS(V: cardinal): string;
  var
    B: array[0..3] of byte;
  begin
    Move(V, B, 4);
    Result := Format('%.2x%.2x%.2x%.2x', [B[0], B[1], B[2], B[3]]);
  end;

begin
  Result := Format('%s%s%s%s', [VS(PA^), VS(PB^), VS(PC^), VS(PD^)]);
end;

procedure KLiMD5.GG(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + ((b^ and d^) or (c^ and (not d^))) + x^ + ac, s) + b^;
end;

procedure KLiMD5.HH(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + (b^ xor c^ xor d^) + x^ + ac, s) + b^;
end;

procedure KLiMD5.II(a, b, c, d, x: pardinal; s: byte; ac: cardinal);
begin
  a^ := ROL(a^ + (c^ xor (b^ or (not d^))) + x^ + ac, s) + b^;
end;

procedure KLiMD5.Init;
begin
  FA := cardinal($67452301); PA := @FA;
  FB := cardinal($efcdab89); PB := @FB;
  FC := cardinal($98badcfe); PC := @FC;
  FD := cardinal($10325476); PD := @FD;
end;

function KLiMD5.ROL(A: cardinal; Amount: byte): cardinal;
const
  CARMASK = $80000000;
var
  X: byte;
begin
  for X := 1 to Amount do
    if (A and CARMASK) = CARMASK then
      A := (A shl 1) or $01 else  
      A := (A shl 1);  
   Result := A;  
end;

function KLiMD5.sumBuf(Abuf: pchar; count: integer): string;
var
  buf: array[0..4159] of byte;
  len: int64;
  eob: boolean;
  bytes, index: integer;
begin
  Init;
  eob := False;
  len := 0;
  repeat
    bytes := Min(4096, count);
    Move(Abuf^, buf, bytes);
    Inc(Abuf, bytes);
    Dec(count, bytes);
    len := len + bytes;
    if bytes <> 4096 then
    begin
      buf[bytes] := $80;
      Inc(bytes);
      while (bytes mod 64) <> 56 do
      begin
        buf[bytes] := 0;
        Inc(bytes);
      end;
      len := len * 8;
      Move(len, buf[bytes], 8);
      Inc(bytes, 8);
      eob := True;
    end;
    index := 0;
    repeat
      Move(buf[index], FBuffer, 64);
      Transform;
      Inc(index, 64);
    until index = bytes;
  until eob;
  Result := GetDigest;
end;

function KLiMD5.sumFile(const fname: string): string;
var
  F: TFileStream;
begin
  F := TFileStream.Create(lse_veryPD(fname), fmShareDenyWrite);
  try
    Result := sumStream(F);
  finally
    F.Free;
  end;
end;

function KLiMD5.sumStr(const S: string): string;
begin
  Result := sumBuf(pchar(S), Length(S));
end;

function KLiMD5.sumStream(Stream: TStream): string;
var
  buf: array[0..4159] of byte;
  len: int64;
  eof: Boolean;
  bytes, index: integer;
begin
  Init;
  eof := False;
  len := 0;
  repeat
    bytes := Stream.Read(buf, 4096);
    len := len + bytes;
    if bytes <> 4096 then
    begin
      buf[bytes] := $80;
      Inc(bytes);
      while (bytes mod 64) <> 56 do
      begin
        buf[bytes] := 0;
        Inc(bytes);
      end;
      len := len * 8;
      Move(len, buf[bytes], 8);
      Inc(bytes, 8);
      eof := True;
    end;
    index := 0;
    repeat
      Move(buf[index], FBuffer, 64);
      Transform;
      Inc(index, 64);
    until index = bytes;
  until eof;
  Result := GetDigest;
end;

procedure KLiMD5.Transform;
const
  S11 = 7;  S12 = 12;  S13 = 17;  S14 = 22;
  S21 = 5;  S22 = 9;   S23 = 14;  S24 = 20;
  S31 = 4;  S32 = 11;  S33 = 16;  S34 = 23;
  S41 = 6;  S42 = 10;  S43 = 15;  S44 = 21;
var
  FAA, FBB, FCC, FDD: cardinal;
begin
  FAA := FA;
  FBB := FB;
  FCC := FC;
  FDD := FD;

  { Round 1 }
  
  FF (PA, PB, PC, PD, @FBuffer[ 0], S11, cardinal($d76aa478)); {  1 }
  FF (PD, PA, PB, PC, @FBuffer[ 1], S12, cardinal($e8c7b756)); {  2 }
  FF (PC, PD, PA, PB, @FBuffer[ 2], S13, cardinal($242070db)); {  3 }
  FF (PB, PC, PD, PA, @FBuffer[ 3], S14, cardinal($c1bdceee)); {  4 }
  FF (PA, PB, PC, PD, @FBuffer[ 4], S11, cardinal($f57c0faf)); {  5 }
  FF (PD, PA, PB, PC, @FBuffer[ 5], S12, cardinal($4787c62a)); {  6 }
  FF (PC, PD, PA, PB, @FBuffer[ 6], S13, cardinal($a8304613)); {  7 }
  FF (PB, PC, PD, PA, @FBuffer[ 7], S14, cardinal($fd469501)); {  8 }
  FF (PA, PB, PC, PD, @FBuffer[ 8], S11, cardinal($698098d8)); {  9 }
  FF (PD, PA, PB, PC, @FBuffer[ 9], S12, cardinal($8b44f7af)); { 10 }
  FF (PC, PD, PA, PB, @FBuffer[10], S13, cardinal($ffff5bb1)); { 11 }
  FF (PB, PC, PD, PA, @FBuffer[11], S14, cardinal($895cd7be)); { 12 }
  FF (PA, PB, PC, PD, @FBuffer[12], S11, cardinal($6b901122)); { 13 }
  FF (PD, PA, PB, PC, @FBuffer[13], S12, cardinal($fd987193)); { 14 }
  FF (PC, PD, PA, PB, @FBuffer[14], S13, cardinal($a679438e)); { 15 }
  FF (PB, PC, PD, PA, @FBuffer[15], S14, cardinal($49b40821)); { 16 }

  { Round 2 }

  GG (PA, PB, PC, PD, @FBuffer[ 1], S21, cardinal($f61e2562)); { 17 }
  GG (PD, PA, PB, PC, @FBuffer[ 6], S22, cardinal($c040b340)); { 18 }
  GG (PC, PD, PA, PB, @FBuffer[11], S23, cardinal($265e5a51)); { 19 }
  GG (PB, PC, PD, PA, @FBuffer[ 0], S24, cardinal($e9b6c7aa)); { 20 }
  GG (PA, PB, PC, PD, @FBuffer[ 5], S21, cardinal($d62f105d)); { 21 }
  GG (PD, PA, PB, PC, @FBuffer[10], S22,  cardinal($2441453)); { 22 }
  GG (PC, PD, PA, PB, @FBuffer[15], S23, cardinal($d8a1e681)); { 23 }
  GG (PB, PC, PD, PA, @FBuffer[ 4], S24, cardinal($e7d3fbc8)); { 24 }
  GG (PA, PB, PC, PD, @FBuffer[ 9], S21, cardinal($21e1cde6)); { 25 }
  GG (PD, PA, PB, PC, @FBuffer[14], S22, cardinal($c33707d6)); { 26 }
  GG (PC, PD, PA, PB, @FBuffer[ 3], S23, cardinal($f4d50d87)); { 27 }
  GG (PB, PC, PD, PA, @FBuffer[ 8], S24, cardinal($455a14ed)); { 28 }
  GG (PA, PB, PC, PD, @FBuffer[13], S21, cardinal($a9e3e905)); { 29 }
  GG (PD, PA, PB, PC, @FBuffer[ 2], S22, cardinal($fcefa3f8)); { 30 }
  GG (PC, PD, PA, PB, @FBuffer[ 7], S23, cardinal($676f02d9)); { 31 }
  GG (PB, PC, PD, PA, @FBuffer[12], S24, cardinal($8d2a4c8a)); { 32 }

  { Round 3 }
  
  HH (PA, PB, PC, PD, @FBuffer[ 5], S31, cardinal($fffa3942)); { 33 }
  HH (PD, PA, PB, PC, @FBuffer[ 8], S32, cardinal($8771f681)); { 34 }
  HH (PC, PD, PA, PB, @FBuffer[11], S33, cardinal($6d9d6122)); { 35 }
  HH (PB, PC, PD, PA, @FBuffer[14], S34, cardinal($fde5380c)); { 36 }
  HH (PA, PB, PC, PD, @FBuffer[ 1], S31, cardinal($a4beea44)); { 37 }
  HH (PD, PA, PB, PC, @FBuffer[ 4], S32, cardinal($4bdecfa9)); { 38 }
  HH (PC, PD, PA, PB, @FBuffer[ 7], S33, cardinal($f6bb4b60)); { 39 }
  HH (PB, PC, PD, PA, @FBuffer[10], S34, cardinal($bebfbc70)); { 40 }
  HH (PA, PB, PC, PD, @FBuffer[13], S31, cardinal($289b7ec6)); { 41 }
  HH (PD, PA, PB, PC, @FBuffer[ 0], S32, cardinal($eaa127fa)); { 42 }
  HH (PC, PD, PA, PB, @FBuffer[ 3], S33, cardinal($d4ef3085)); { 43 }
  HH (PB, PC, PD, PA, @FBuffer[ 6], S34,  cardinal($4881d05)); { 44 }
  HH (PA, PB, PC, PD, @FBuffer[ 9], S31, cardinal($d9d4d039)); { 45 }
  HH (PD, PA, PB, PC, @FBuffer[12], S32, cardinal($e6db99e5)); { 46 }
  HH (PC, PD, PA, PB, @FBuffer[15], S33, cardinal($1fa27cf8)); { 47 }
  HH (PB, PC, PD, PA, @FBuffer[ 2], S34, cardinal($c4ac5665)); { 48 }

  { Round 4 }
  
  II (PA, PB, PC, PD, @FBuffer[ 0], S41, cardinal($f4292244)); { 49 }
  II (PD, PA, PB, PC, @FBuffer[ 7], S42, cardinal($432aff97)); { 50 }
  II (PC, PD, PA, PB, @FBuffer[14], S43, cardinal($ab9423a7)); { 51 }
  II (PB, PC, PD, PA, @FBuffer[ 5], S44, cardinal($fc93a039)); { 52 }
  II (PA, PB, PC, PD, @FBuffer[12], S41, cardinal($655b59c3)); { 53 }
  II (PD, PA, PB, PC, @FBuffer[ 3], S42, cardinal($8f0ccc92)); { 54 }
  II (PC, PD, PA, PB, @FBuffer[10], S43, cardinal($ffeff47d)); { 55 }
  II (PB, PC, PD, PA, @FBuffer[ 1], S44, cardinal($85845dd1)); { 56 }
  II (PA, PB, PC, PD, @FBuffer[ 8], S41, cardinal($6fa87e4f)); { 57 }
  II (PD, PA, PB, PC, @FBuffer[15], S42, cardinal($fe2ce6e0)); { 58 }
  II (PC, PD, PA, PB, @FBuffer[ 6], S43, cardinal($a3014314)); { 59 }
  II (PB, PC, PD, PA, @FBuffer[13], S44, cardinal($4e0811a1)); { 60 }
  II (PA, PB, PC, PD, @FBuffer[ 4], S41, cardinal($f7537e82)); { 61 }
  II (PD, PA, PB, PC, @FBuffer[11], S42, cardinal($bd3af235)); { 62 }
  II (PC, PD, PA, PB, @FBuffer[ 2], S43, cardinal($2ad7d2bb)); { 63 }
  II (PB, PC, PD, PA, @FBuffer[ 9], S44, cardinal($eb86d391)); { 64 }

  FA := FA + FAA;
  FB := FB + FBB;
  FC := FC + FCC;
  FD := FD + FDD;

  FillChar(FBuffer, SizeOf(FBuffer), #0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure kernel_startup;
var
  X: KLiSymbol;
begin
  if not sys_init_lysee then
  begin
    sys_init_lysee := true;
    sys_libraries := new_named_list(false);
    sys_spinlock := KLiSpinLock.Create;
    sys_version := LSE_VERSION;
    kernel_load_config_file('');
    kernel_setup_builtin_types;

    for X := Low(KLiSymbol) to High(KLiSymbol) do
    begin
      sys_runner_procs[X] := {$IFDEF FPC}@{$ENDIF}runner_error;
      if X in OperIDSyms then
        KLiFunc_oper.Create(X);
    end;
    sys_oper_inc := sys_module.FindFunc('+');

    sys_runner_procs[syID]       := {$IFDEF FPC}@{$ENDIF}runner_ID;
    sys_runner_procs[syBecome]   := {$IFDEF FPC}@{$ENDIF}runner_become;
    sys_runner_procs[syEcho]     := {$IFDEF FPC}@{$ENDIF}runner_echo;
    sys_runner_procs[syFloat]    := {$IFDEF FPC}@{$ENDIF}runner_float;
    sys_runner_procs[syInt]      := {$IFDEF FPC}@{$ENDIF}runner_int;
    sys_runner_procs[syStr]      := {$IFDEF FPC}@{$ENDIF}runner_str;
    sys_runner_procs[syAdd]      := {$IFDEF FPC}@{$ENDIF}runner_add;
    sys_runner_procs[syDec]      := {$IFDEF FPC}@{$ENDIF}runner_dec;
    sys_runner_procs[syMul]      := {$IFDEF FPC}@{$ENDIF}runner_mul;
    sys_runner_procs[syDiv]      := {$IFDEF FPC}@{$ENDIF}runner_div;
    sys_runner_procs[syMod]      := {$IFDEF FPC}@{$ENDIF}runner_mod;
    sys_runner_procs[syBNot]     := {$IFDEF FPC}@{$ENDIF}runner_bnot;
    sys_runner_procs[syBXor]     := {$IFDEF FPC}@{$ENDIF}runner_bxor;
    sys_runner_procs[syBOr]      := {$IFDEF FPC}@{$ENDIF}runner_bor;
    sys_runner_procs[syBAnd]     := {$IFDEF FPC}@{$ENDIF}runner_band;
    sys_runner_procs[syBShl]     := {$IFDEF FPC}@{$ENDIF}runner_bshl;
    sys_runner_procs[syBShr]     := {$IFDEF FPC}@{$ENDIF}runner_bshr;
    sys_runner_procs[syNot]      := {$IFDEF FPC}@{$ENDIF}runner_not;
    sys_runner_procs[syNeg]      := {$IFDEF FPC}@{$ENDIF}runner_neg;
    sys_runner_procs[syEQ]       := {$IFDEF FPC}@{$ENDIF}runner_eq;
    sys_runner_procs[syNE]       := {$IFDEF FPC}@{$ENDIF}runner_ne;
    sys_runner_procs[syLess]     := {$IFDEF FPC}@{$ENDIF}runner_less;
    sys_runner_procs[syLE]       := {$IFDEF FPC}@{$ENDIF}runner_le;
    sys_runner_procs[syMore]     := {$IFDEF FPC}@{$ENDIF}runner_more;
    sys_runner_procs[syME]       := {$IFDEF FPC}@{$ENDIF}runner_me;
    sys_runner_procs[syIn]       := {$IFDEF FPC}@{$ENDIF}runner_in;
    sys_runner_procs[syAnd]      := {$IFDEF FPC}@{$ENDIF}runner_and;
    sys_runner_procs[syOr]       := {$IFDEF FPC}@{$ENDIF}runner_or;
    sys_runner_procs[syVarList]  := {$IFDEF FPC}@{$ENDIF}runner_varlist;
    sys_runner_procs[syNil]      := {$IFDEF FPC}@{$ENDIF}runner_nil;
    sys_runner_procs[syGetEnv]   := {$IFDEF FPC}@{$ENDIF}runner_getenv;
    sys_runner_procs[syGetSV]    := {$IFDEF FPC}@{$ENDIF}runner_getsv;
    sys_runner_procs[sySetSV]    := {$IFDEF FPC}@{$ENDIF}runner_setsv;
    sys_runner_procs[syFormat]   := {$IFDEF FPC}@{$ENDIF}runner_format;
    sys_runner_procs[syIs]       := {$IFDEF FPC}@{$ENDIF}runner_is;
    sys_runner_procs[syAs]       := {$IFDEF FPC}@{$ENDIF}runner_as;
    sys_runner_procs[syVarGen]   := {$IFDEF FPC}@{$ENDIF}runner_vargen;
    sys_runner_procs[syAsk]      := {$IFDEF FPC}@{$ENDIF}runner_ask;
    sys_runner_procs[syReferAsk] := {$IFDEF FPC}@{$ENDIF}runner_refer_ask;
    sys_runner_procs[syLabel]    := {$IFDEF FPC}@{$ENDIF}runner_next;
    sys_runner_procs[syIdle]     := {$IFDEF FPC}@{$ENDIF}runner_next;
    sys_runner_procs[syTry]      := {$IFDEF FPC}@{$ENDIF}runner_try;
    sys_runner_procs[syReturn]   := {$IFDEF FPC}@{$ENDIF}runner_return;
    sys_runner_procs[syJump]     := {$IFDEF FPC}@{$ENDIF}runner_jump;
    sys_runner_procs[syJmpF]     := {$IFDEF FPC}@{$ENDIF}runner_jmpf;
    sys_runner_procs[syJmpT]     := {$IFDEF FPC}@{$ENDIF}runner_jmpt;
    sys_runner_procs[syJmpFP]    := {$IFDEF FPC}@{$ENDIF}runner_jmpfpop;
    sys_runner_procs[syJmpTP]    := {$IFDEF FPC}@{$ENDIF}runner_jmptpop;
    sys_runner_procs[syGoto]     := {$IFDEF FPC}@{$ENDIF}runner_goto;
    sys_runner_procs[syGoTP]     := {$IFDEF FPC}@{$ENDIF}runner_gototp;
    sys_runner_procs[syGoFP]     := {$IFDEF FPC}@{$ENDIF}runner_gotofp;
    sys_runner_procs[syHashed]   := {$IFDEF FPC}@{$ENDIF}runner_hashed;
    sys_runner_procs[syRINR]     := {$IFDEF FPC}@{$ENDIF}runner_RINR;
    sys_runner_procs[syGETV]     := {$IFDEF FPC}@{$ENDIF}runner_GETV;
    sys_runner_procs[sySETV]     := {$IFDEF FPC}@{$ENDIF}runner_SETV;
    sys_runner_procs[syLike]     := {$IFDEF FPC}@{$ENDIF}runner_Like;
    sys_runner_procs[syGetIV]    := {$IFDEF FPC}@{$ENDIF}runner_getiv;
    sys_runner_procs[sySetIV]    := {$IFDEF FPC}@{$ENDIF}runner_setiv;
    sys_runner_procs[syDupLast]  := {$IFDEF FPC}@{$ENDIF}runner_duplast;
    sys_runner_procs[syFill]     := {$IFDEF FPC}@{$ENDIF}runner_fill;
    sys_runner_procs[sySend]     := {$IFDEF FPC}@{$ENDIF}runner_send;
    sys_runner_procs[syCase]     := {$IFDEF FPC}@{$ENDIF}runner_case;
    sys_runner_procs[sySTMT]     := {$IFDEF FPC}@{$ENDIF}runner_STMT;
  end;
end;

initialization
begin
  lse_entries := @sys_cik_entries;
end;

finalization
try
  if not sys_exit_lysee then
  begin
    sys_exit_lysee := true;
    release_string_object_list(sys_libraries);
    free_and_nil(sys_configures);
    free_and_nil(sys_mimes);
    free_and_nil(sys_spinlock);
  end;
except
  { safe & quiet }
end;

end.

