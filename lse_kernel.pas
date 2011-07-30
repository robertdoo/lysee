{==============================================================================}
{        UNIT: lse_kernel                                                      }
{ DESCRIPTION: kernel of lysee                                                 }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/29                                                      }
{    MODIFIED: 2011/07/30                                                      }
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

type
  KLiFunc       = class; {forward}
  KLiMacro      = class;
  KLiModule     = class;
  KLiModuleList = class;
  KLiError      = class;
  KLiEngine     = class;
  KLiVarList    = class;
  KLiVarSnap    = class;
  KLiRunner     = class;
  KLiHashed     = class;

  RLiPos = packed record
    row, col: word;
    module: KLiModule;
  end;
  PLiPos = ^RLiPos;

  KLiSymbol = (
    syError, syBegin, syImport, syConst, syClass, syObject, sySet, syGet, syDef,
    syReturn, syIf, syThen, syElse, syWhile, syRepeat, syUntil, syFor, syDo,
    syBreak, syContinue, sySwitch, syCase, syTry, syExcept, syFinally, syIn,
    syIs, syAs, syLike, syAnd, syOr, syNot, syFrom, syTo, syEnd, syBecome,
    syAdd, syDec, syMul, syDiv, syMod, syBitNot, syBitXor, syBitAnd, syBitOr,
    syBitShr, syBitShl, syFill, syLParen, syRParen, syLBlock, syRBlock,
    syLArray, syRArray, syDot, syUpto, syAsk, syDot2, syComma, sySemicolon,
    syEQ, syNE, syLess, syLE, syMore, syME, syFormat, syID, syFloat, syInt,
    syStr, syNil, syNeg, syJmpT, syJmpF, syJmpTP, syJmpFP, syJump, syIdle,
    syGetEnv, syLabel, syGoto, syGoTP, syGoFP, syVarGen, syRINR, syGETV,
    sySETV, syGetIV, sySetIV, syDupLast, syEOF, syEcho, syGetSV, sySetSV,
    sySend, syVarList, syHashed, sySTMT
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
    FEOF: boolean;                       {<--end of file}
    FSkip0A: boolean;                    {<--GetChar skip #10}
    FNextToken: PLiToken;                {<--tokens put back}
    function GetChar: boolean;
    function PeekChar: char;
    function GotoChar(Chars: TLseCharSet): boolean;
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
    FFunc: KLiFunc;
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): PLiToken;
    function GetLast: PLiToken;
    procedure DoAdd(token: PLiToken);
  public
    constructor Create(AFunc: KLiFunc);
    destructor Destroy;override;
    procedure DumpCode(List: TStrings; const Margin: string);
    procedure Clear;
    function Add: PLiToken;
    function AddNew(sym: KLiSymbol; const Pos: PLiPos): PLiToken;
    function AddToken(token: PLiToken): PLiToken;
    function AddGoto(const Name: string; const Pos: PLiPos): PLiToken;
    function AddRinr(const Name: string; const Pos: PLiPos): PLiToken;
    function AddLabel(const Name: string; const Pos: PLiPos): PLiToken;
    function FindLabel(const Name: string): PLiToken;
    property Count: integer read GetCount;
    property Last: PLiToken read GetLast;
    property Items[Index: integer]: PLiToken read GetItem;default;
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
    function CurCodes: KLiTokens;
    function AddToken(token: PLiToken): PLiToken;
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
    procedure ExpandMacro(AMacro: KLiMacro);
    function FindMacro(const ID: string): KLiMacro;
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

  KLiFindObject = (foNone, foVarb, foFunc, foType, foMacro, foModule);
  KLiFindObjects = set of KLiFindObject;

  RLiFind = packed record
    case fo_type: KLiFindObject of
      foVarb  : (VVarb  : PLseVarb);
      foFunc  : (VFunc  : KLiFunc);
      foType  : (VType  : PLseType);
      foMacro : (VMacro : KLiMacro);
      foModule: (VModule: KLiModule);
  end;
  PLiFind = ^RLiFind;

  KLiFuncState = (fusMainFunc, fusInitFunc, fusCurry, fusNameCall, fusLambda,
                  fusConst, fusEmpty);
  KLiFuncStates = set of KLiFuncState;

  KLiFunc = class(TLseNamed)
  private
    FModule: KLiModule;
    FParams: array of PLseVarb;
    FResultType: PLseType;
    FDescription: string;
    FState: KLiFuncStates;
    FCodes: KLiTokens;
    FProc: pointer;
    FNext: KLiFunc;
    FPrev: KLiFunc;
    function HasState(Index: KLiFuncState): boolean;
    procedure SetState(Index: KLiFuncState; Value: boolean);
  public
    constructor Create(Parent: KLiModule; AResultType: PLseType;
      const AName: string; Params: TStringList; Proc: pointer);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    procedure AddParam(const VName: string; VType: PLseType);
    procedure AddSuper;
    function HasSuper: boolean;
    function FindParam(const VName: string): PLseVarb;
    function ParamCount: integer;
    function GetParam(Index: integer): PLseVarb;
    function Curry(List: KLiVarList; Module: KLiModule): KLiFunc;overload;
    function Curry(Data: PLseValue; Module: KLiModule): KLiFunc;overload;
    procedure DumpCode(list: TStrings; const margin: string);
    function Prototype: string;
    function FullName: string;
    function FindInside(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID: string; rec: PLiFind; Range: KLiFindObjects = []): boolean;
    function FindMethod(const AName: string; AType: PLseType): KLiFunc;
    function ListMethod(AType: PLseType; OnlyName: boolean): KLiVarList;
    function FindCreate(AType: PLseType): KLiFunc;
    function Engine: KLiEngine;
    property Module: KLiModule read FModule;
    property ResultType: PLseType read FResultType write FResultType;
    property IsMainFunc: boolean index fusMainFunc read HasState;
    property IsInitFunc: boolean index fusInitFunc read HasState write SetState;
    property IsCurryFunc: boolean index fusCurry read HasState write SetState;
    property IsNameCall: boolean index fusNameCall read HasState write SetState;
    property IsConstFunc: boolean index fusConst read HasState write SetState;
    property IsLambdaFunc: boolean index fusLambda read HasState write SetState;
    property IsEmptyFunc: boolean index fusEmpty read HasState;
    property Description: string read FDescription write FDescription;
    property Proc: pointer read FProc write FProc;
    property Codes: KLiTokens read FCodes;
    property Next: KLiFunc read FNext;
  end;

  { KLiFunc_curry }

  KLiObjRecState  = (orsInChain, orsMarked);
  KLiObjRecStates = set of KLiObjRecState;

  PLiObjRec = ^RLiObjRec;
  RLiObjRec = packed record
    or_object: pointer;
    or_type  : PLseType;
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
  
  { KLiMacro }

  KLiMacro = class(TLseNamed)
  private
    FModule: KLiModule;
    FPrev: KLiMacro;
    FNext: KLiMacro;
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

  KLiModule = class(TLseNamed)
  private
    FAliasName: string;         {<--KRLS: alias name}
    FFileName: string;          {<--KRLS: module file name}
    FModuleType: KLiModuleType; {<--KRLS: module type}
    FVersion: string;           {<--KRLS: module version}
    FDescription: string;       {<--KRLS: description}
    FTypeList: TList;           {<--KRLS: type list}
    FFuncList: TLseHashNamed;   {<--KRLS: function list}
    FFirstFunc: KLiFunc;        {<--KRLS: first function}
    FLastFunc: KLiFunc;         {<--KRLS: last function}
    FInvokeProc: TLseOnInvoke;  {<--*RL*: call gate function}
    FHandle: THandle;           {<--**L*: library (DLL) handle}
    FEngine: KLiEngine;         {<--***S: owner script engine}
    FModules: KLiModuleList;    {<--***S: modules imported by this module}
    FImporters: TList;          {<--***S: modules importing this module}
    FMainFunc: KLiFunc;         {<--***S: modules entry function}
    FParsing: boolean;          {<--***S: parsing}
    FFirstMacro: KLiMacro;      {<--***S: first macro}
    FLastMacro: KLiMacro;       {<--***S: last macro}
  public
    constructor Create(const MName: string; MEngine: KLiEngine; MType: KLiModuleType);
    destructor Destroy;override;
    procedure SaveTo(V: PLseValue);
    procedure DeleteFunctions;
    procedure DumpCodeToStream(stream: TStream; const margin: string);
    function Import(AModule: KLiModule): KLiModule;
    function NewTempID(const Prefix: string): string;
    function NewFuncName: string;
    function NewLabelName: string;
    function NewFunc(const AName: string): KLiFunc;
    function SetupFunc(Func: PLseFunc): KLiFunc;
    function SetupModuleFuncs(Rec: PLseFuncListRec): integer;
    function FindModule(const ID: string; FindPossible: boolean): KLiModule;
    function IsMainModule: boolean;
    function SetupType(const TR: PLseType): boolean;
    function SetupModuleTypes(const TLR: PLseTypeListRec): integer;
    function TypeCount: integer;
    function GetType(Index: integer): PLseType;
    function FindType(const ID: string): PLseType;
    function FindTypeBy(const ID, module_name: string): PLseType;
    function FindFunc(const ID: string): KLiFunc;
    function FindMacro(const ID: string): KLiMacro;
    function Find(const ID: string; rec: PLiFind = nil): boolean;
    function FindBy(const ID, module_name: string; rec: PLiFind): boolean;
    property ModuleType: KLiModuleType read FModuleType;
    property Modules: KLiModuleList read FModules;
    property FileName: string read FFileName write FFileName;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Engine: KLiEngine read FEngine;
    property MainFunc: KLiFunc read FMainFunc;
    property FirstFunc: KLiFunc read FFirstFunc;
  end;

  { KLiModuleList }

  KLiModuleList = class(TLseObject)
  private
    FEngine: KLiEngine;
    FImporter: KLiModule;
    FModules: TList;
    function GetModule(Index: integer): KLiModule;
    function GetCount: integer;
  public
    constructor Create(Engine: KLiEngine);
    destructor Destroy;override;
    function IndexOf(AModule: KLiModule): integer;overload;
    function IndexOf(const Name: string): integer;overload;
    function Has(AModule: KLiModule): boolean;overload;
    function Has(const Name: string): boolean;overload;
    function Find(const Name: string): KLiModule;
    function Add(AModule: KLiModule): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure DeleteFunctions;
    function ToVarlist(Engine: KLiEngine): KLiVarList;
    property Count: integer read GetCount;
    property Modules[Index: integer]: KLiModule read GetModule;default;
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
    FMainModule: KLiModule;
    FMainFunc: KLiFunc;
    FMainSnap: KLiVarSnap;
    FMainRunner: KLiRunner;
    FMainFile: string;           {<--main script file}
    FMainSearchPath: string;     {<--primary search path}
    FMainValues: KLiHashed;
    FTempValues: KLiVarList;
    FError: KLiError;            {<--error object}
    FExitResult: PLseValue;      {<--exit result}
    FExitResultType: string;
    FExitResultText: string;
    FExited: boolean;
    FArguments: TStringList;     {<--argument list}
    FModules: KLiModuleList;     {<--module list}
    FCompiledObjects: TList;     {<--compiled objects}
    FReady: boolean;
    FOnReadBuf: KLiReadBuf;
    FOrChain: PLiObjRec;
    FNameSeed: cardinal;         {<--label seed}
    FInput: PLseStream;
    FOutput: PLseStream;
    function GetResultText: string;
    function GetResultType: PLseType;
    procedure SetMainFile(const AValue: string);
    procedure SetMainSearchPath(const AValue: string);
    function GetMainFunc: KLiFunc;
    function GetMainSnap: KLiVarSnap;
    function GetInputStream: PLseStream;
    procedure SetInputStream(const Value: PLseStream);
    function GetOutputStream: PLseStream;
    procedure SetOutputStream(const Value: PLseStream);
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
    procedure RollbackCompiled;
    function GetSearchPath: string;
    procedure SetResultTypeText(const RType, RText: string);
    function FindModuleByFileName(const FileName: string): KLiModule;
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
    property ResultType: PLseType read GetResultType;
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
    exprec: PLiToken;   // prior FExprrec
    output: PLseValue;  // result
  end;

  PLiCallSnap = ^RLiCallSnap;
  RLiCallSnap = packed record
    prev: PLiCallSnap;
    call: PLseParam;
    snap: PLiSnap;
  end;

  { KLiRunner }

  KLiRunner = class(TLseObject)
  private
    FEngine: KLiEngine;
    FStack: KLiVarList;
    FStackBase: integer;
    FCurrent: PLiSnap;
    FLastCall: PLiCallSnap;
    FExprrec: PLiToken;
    FTerminated: boolean;
    FExcepted: boolean;
    function ExecGoonNext: boolean;
  public
    constructor Create(Engine: KLiEngine);
    destructor Destroy;override;
    procedure ErrorRT(const ErrorStr: string);
    procedure Terminate;
    function Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
    function GetString(const ID: string): string;
    function FormatFor(const Fmt: string; Values: KLiVarList): string;
    function HasNext: boolean;
    function CurrentModule: KLiModule;
    function CurrentFunc: KLiFunc;
    property Current: PLiSnap read FCurrent;
    property Engine: KLiEngine read FEngine;
    property Stack: KLiVarList read FStack;
    property Exprrec: PLiToken read FExprrec;
    property Excepted: boolean read FExcepted write FExcepted;
    property Terminated: boolean read FTerminated;
  end;

  { KLiHashed }

  KLiHashed = class(TLseHashTable)
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
    procedure ListKeys(List: KLiVarList);
  end;

{======================================================================)
(======== kernel ======================================================)
(======================================================================}
procedure kernel_startup;
procedure kernel_load_confile(const ConfigFile: string);
function  kernel_config(const ID: string): string;
function  kernel_expand_value(const S: string; E: KLiEngine): string;
procedure kernel_lock;
procedure kernel_unlock;
function  kernel_query(const ID: pchar): pointer;cdecl;
function  kernel_type(Index: TLseKernelType): PLseType;cdecl;

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
(======== type ========================================================)
(======================================================================}
function type_module(T: PLseType): KLiModule;
function type_full_name(T: PLseType): string;

{======================================================================)
(======== value =======================================================)
(======================================================================}
function value_func(V: PLseValue): KLiFunc;
function value_varlist(V: PLseValue): KLiVarList;
function value_in(V: PLseValue; Host: KLiVarList; FindItemVG: boolean): boolean;overload;
function value_in(V: PLseValue; Host: PLseString): boolean;overload;
function value_in(V: PLseValue; Host: int64): boolean;overload;
function value_in(V: PLseValue; Host: KLiHashed): boolean;overload;

{======================================================================)
(======== PLseParam ===================================================)
(======================================================================}
function  get_runner(Param: PLseParam): KLiRunner;
function  get_engine(Param: PLseParam): KLiEngine;
function  get_this(Param: PLseParam; var This): boolean;
procedure set_error(Param: PLseParam; const Msg: string);overload;
procedure set_error(Param: PLseParam; const Msg: string; const Args: array of const);overload;
procedure set_error(Param: PLseParam; const EID: string; Errno: integer; const Msg: string);overload;
procedure set_error_this(Param: PLseParam);

{======================================================================)
(======== tokens ======================================================)
(======================================================================}
function  token_new: PLiToken;
procedure token_free(Token: PLiToken);
procedure token_copy(SrcToken, DstToken: PLiToken);
function  token_clone(Token: PLiToken): PLiToken;
function  token_pure_ID(token: PLiToken): boolean;
procedure token_reset(Token: PLiToken);

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
function  genid: string;
function  hex_value(ch: char): integer;
function  same_fname(const F1, F2: string): boolean;
function  file_text(const FileName: string): string;
function  encode_UTF8(const S: string): string;
function  decode_UTF8(const S: string): string;
function  get_open_file_mode(const openMode: string; var fileMode: word; var R, W: boolean): boolean;
function  new_string(Source: pchar; Count: integer): string;

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
  sys_kernel            : string;        {<--kernel file name}
  sys_version           : string;        {<--kernel version}
  sys_knpath            : string;        {<--kernel file path}
  sys_kndir             : string;        {<--kernel file directory}
  sys_home_path         : string;        {<--home path}
  sys_tmpath            : string;        {<--temporary path}
  sys_search_path       : string;        {<--module search path}
  sys_confile           : string;        {<--config file name}
  sys_mimefile          : string;        {<--MIME file name}
  sys_mimes             : TStringList;   {<--MIME list}
  sys_configures        : TStringList;   {<--configure value list}
  sys_program           : string;        {<--program file name}
  sys_process_ID        : string;        {<--process ID}
  sys_libraries         : KLiModuleList; {<--kernel & library module list}
  sys_seed              : int64;         {<--system name seed}
  sys_module            : KLiModule;     {<--builtin [sys] module}
  sys_spinlock          : Syncobjs.TCriticalSection; {<--kernel's spinlock}
  sys_oper_inc          : KLiFunc;       {<--sys::+}
  sys_nothing           : KLiFunc;       {<--sys::nothing}
  sys_nil               : RLseValue;     {<--default empty data}
  sys_LB                : string = sLineBreak;
  sys_runner_procs      : array[KLiSymbol] of procedure(Runner: KLiRunner);
  sys_reserved_words    : string = '';
  sys_randomized        : boolean = false;
  sys_exit_lysee        : boolean = false;
  sys_init_lysee        : boolean = false;
  sys_kernel_types      : array[TLseKernelType] of RLseType;

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
    (SY:syClass;     ID:'class';        SM:'class'),
    (SY:syObject;    ID:'object';       SM:'object'),
    (SY:sySet;       ID:'set';          SM:'set'),
    (SY:syGet;       ID:'get';          SM:'get'),
    (sy:syDef;       ID:'def';          SM:'define functions'),
    (SY:syReturn;    ID:'return';       SM:'return'),
    (SY:syIf;        ID:'if';           SM:'if'),
    (sy:syThen;      ID:'then';         SM:'then clause'),
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
    (SY:syBitNot;    ID:'~';            SM:'bit xor -1'),
    (SY:syBitXor;    ID:'^';            SM:'bit xor'),
    (SY:syBitAnd;    ID:'&';            SM:'bit and'),
    (SY:syBitOr;     ID:'|';            SM:'bit or'),
    (SY:syBitShr;    ID:'>>';           SM:'bit shift right'),
    (SY:syBitShl;    ID:'<<';           SM:'bit shift left'),
    (sy:syFill;      ID:'<<<';          SM:'add all'),
    (SY:syLParen;    ID:'(';            SM:'left paren'),
    (SY:syRParen;    ID:')';            SM:'right paren'),
    (SY:syLBlock;    ID:'{';            SM:'left block'),
    (SY:syRBlock;    ID:'}';            SM:'right block'),
    (SY:syLArray;    ID:'[';            SM:'left array'),
    (SY:syRArray;    ID:']';            SM:'right array'),
    (SY:syDot;       ID:'.';            SM:'dot'),
    (SY:syUpto;      ID:'..';           SM:'upto'),
    (SY:syAsk;       ID:'?';            SM:'ask'),
    (SY:syDot2;      ID:':';            SM:'dot 2'),
    (SY:syComma;     ID:',';            SM:'comma'),
    (SY:sySemicolon; ID:';';            SM:'semicolon'),
    (SY:syEQ;        ID:'==';           SM:'equal'),
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
    (SY:sySTMT;      ID:'<STMT>';       SM:'end statement')
  );

  FirstKeyword = syBegin;
  LastKeyword = syEnd;
  FirstOper = Succ(syEnd);
  LastOper = syFormat;
  
  ConstantSyms = [syFloat, syInt, syStr, syNil, syGetEnv, syGetSV];

  ExprOperSyms: array[0..4] of KLiSymbols = (
   {[syNeg, syNot, syBitNot, syFormat]}                                         // 1
    [syMul, syDiv, syMod],                                                      // 2
    [syAdd, syDec],                                                             // 3
    [syBitXor, syBitAnd, syBitOr, syBitShl, syBitShr, syFill],                  // 4
    [syEQ, syNE, syLess, syLE, syMore, syME, syIn, syLike, syAs, syIs, syUpto], // 5
    [syAnd, syOr]                                                               // 6
  );

  OperIDSyms = [syMul, syDiv, syMod, syAdd, syDec, syBitXor, syBitAnd,
    syBitOr, syBitShl, syBitShr, syFill, syEQ, syNE, syLess, syLE, syMore, syME,
    syLike, syAs, syIs, syUpto, syAnd, syOr];

  ExprHeadSyms = ConstantSyms + OperIDSyms + [syID, syNot, syDec, syBitNot,
    syLParen, syLArray, syLBlock, syBitOr, syFormat];

  ExprEndSyms = [syIf, syElse, syExcept, syFinally, syBecome, syRParen,
    syRBlock, syRArray, syDot, syAsk, syDot2, syComma,
    syIn, syThen, syEnd, syUntil, syEOF];

  GotoSyms = [syTry, syGoto, syGoTP, syGoFP, syRINR];

// <UDC> ///////////////////////////////////////////////////////////////////////

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
  L, R: PLseValue;
begin
  L := Param^.p_result;
  lse_set_value(L, Param^.p_param[0]);
  R := Param^.p_param[1];
  func := KLiFunc_oper(Param^.p_func);
  case func.FOper of
    syMul   : lse_mul(L, R);
    syDiv   : lse_div(L, R);
    syMod   : lse_mod(L, R);
    syAdd   : lse_add(L, R);
    syDec   : lse_dec(L, R);
    syBitXor: lse_bit_xor(L, R);
    syBitAnd: lse_bit_and(L, R);
    syBitOr : lse_bit_or(L, R);
    syBitShl: lse_bit_shl(L, R);
    syBitShr: lse_bit_shr(L, R);
    syFill  : lse_fill(L, R);
    syEQ    : lse_equal(L, R);
    syNE    : lse_diff(L, R);
    syLess  : lse_less(L, R);
    syLE    : lse_eqless(L, R);
    syMore  : lse_more(L, R);
    syUpto  : lse_upto(L, R);
    syME    : lse_eqmore(L, R);
    syIs    : lse_is(L, R);
    syAs    : lse_as(L, R);
    syLike  : lse_like(L, R);
    syAnd   : lse_logic_and(L, R);
    syOr    : lse_logic_or(L, R);
  end;
end;

procedure udc_empty(const Param: PLseParam);cdecl;
begin
  { do nothing }
end;

// <RUNNER> ////////////////////////////////////////////////////////////////////

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
          data.vtype := KT_VARSNAP;
          data.VObject := FCurrent^.values;
          func := srec.VFunc.Curry(@data, CurrentModule);
          func.SaveTo(FStack.Add);
          if func <> srec.VFunc then
            func.DecRefcount;
        end
        else FStack.Add(srec.VFunc, KT_FUNC);
      end
      else
      if srec.fo_type = foType then
        lse_set_type(FStack.Add, srec.VType) else
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
        lse_stream_write(FEngine.Output, lse_get_str(data));
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
  lse_add(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_dec(Sender: KLiRunner);
begin
  lse_dec(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_mul(Sender: KLiRunner);
begin
  lse_mul(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_div(Sender: KLiRunner);
begin
  lse_div(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_mod(Sender: KLiRunner);
begin
  lse_mod(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bit_not(Sender: KLiRunner);
begin
  lse_bit_not(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_bit_xor(Sender: KLiRunner);
begin
  lse_bit_xor(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bit_or(Sender: KLiRunner);
begin
  lse_bit_or(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bit_and(Sender: KLiRunner);
begin
  lse_bit_and(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bit_shl(Sender: KLiRunner);
begin
  lse_bit_shl(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_bit_shr(Sender: KLiRunner);
begin
  lse_bit_shr(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_not(Sender: KLiRunner);
begin
  lse_logic_not(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_neg(Sender: KLiRunner);
begin
  lse_neg(Sender.FStack[-1]);
  runner_next(Sender);
end;

procedure runner_eq(Sender: KLiRunner);
begin
  lse_equal(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_ne(Sender: KLiRunner);
begin
  lse_diff(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_less(Sender: KLiRunner);
begin
  lse_less(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_le(Sender: KLiRunner);
begin
  lse_eqless(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_more(Sender: KLiRunner);
begin
  lse_more(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_me(Sender: KLiRunner);
begin
  lse_eqmore(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_in(Sender: KLiRunner);
var
  V1, V2: PLseValue;
  T2: PLseType;
  done: boolean;
begin
  done := false;
  with Sender do
  begin
    V1 := FStack[-2];
    V2 := FStack[-1];
    T2 := lse_type(V2);
    case T2^.cr_type of
      LSV_STRING: done := value_in(V1, V2^.VObject);
      LSV_INT   : done := value_in(V1, V2^.VInteger);
      LSV_OBJECT: if V2^.VObject <> nil then
                    if T2 = KT_VARLIST then
                      done := value_in(V1, KLiVarList(V2^.VObject), true) else
                    if T2 = KT_HASHED then
                      done := value_in(V1, KLiHashed(V2^.VObject)) else
                      done := lse_vargen_contains(lse_get_vargen(V2), V1);
    end;
    FStack.DeleteLast;
  end;
  lse_set_bool(V1, done);
  runner_next(Sender);
end;

procedure runner_and(Sender: KLiRunner);
begin
  lse_logic_and(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_or(Sender: KLiRunner);
begin
  lse_logic_or(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_upto(Sender: KLiRunner);
begin
  lse_upto(Sender.FStack[-2], Sender.FStack[-1]);
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
    FStack.Add(L, KT_VARLIST);
  end;
  runner_next(Sender);
end;

procedure runner_nil(Sender: KLiRunner);
begin
  Sender.FStack.AddDefault(KT_VOID);
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
  T: PLseType;
  M: KLiModule;
  F, new_func: KLiFunc;
  N: string;
  X: integer;
begin
  V := Sender.FStack[-1];
  T := lse_type(V);
  if T = KT_STRING then
    lse_set_string(V, Sender.FormatFor(lse_get_str(V), nil)) else
  if T = KT_FUNC then
  begin
    F := KLiFunc(V^.VObject);
    if (F <> nil) and (F.FModule.FEngine = nil) then
    begin
      M := Sender.CurrentModule;
      N := '#' + F.FullName;
      new_func := M.FindFunc(N);
      if new_func = nil then
      begin
        new_func := KLiFunc.Create(M, F.FResultType, N, nil, F.FProc);
        for X := 0 to F.ParamCount - 1 do
          new_func.AddParam(F.GetParam(X)^.va_name, F.GetParam(X)^.va_type);
      end;
      new_func.SaveTo(V);
    end;
  end;
  runner_next(Sender);
end;

procedure runner_is(Sender: KLiRunner);
begin
  lse_is(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_as(Sender: KLiRunner);
begin
  lse_as(Sender.FStack[-2], Sender.FStack[-1]);
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
    lse_set_vargen(last, lse_get_vargen(last));
  end;
  runner_next(Sender);
end;

procedure runner_ask(Sender: KLiRunner);
var
  base, prmc: integer;
  data: PLseValue;
  clss: PLseType;
  func: KLiFunc;
begin
  with Sender do
  begin
    prmc := FExprrec^.tk_prmc;
    base := FStack.Count - prmc;
    data := FStack[base];
    clss := lse_type(data);
    if clss = KT_FUNC then
    begin
      func := value_func(data);
      lse_check(func <> nil, EsFuncNotSpecify);
      Goon(func, prmc - 1, data);
    end
    else
    if clss = KT_TYPE then
    begin
      clss := PLseType(data^.VObject);
      lse_check(clss <> nil, EsClassNotSpecify);
      lse_set_nil(data, clss);
      func := CurrentFunc.FindCreate(clss);
      if func <> nil then
      begin
        FStack.Delete(base);
        Goon(func, prmc - 1, nil);
      end
      else FStack.Press(prmc - 1);
    end
    else lse_error('invalid call to %s', [type_full_name(clss)]);
  end;
  runner_next(Sender);
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
    if not lse_get_bool(FStack[-1]) then
      Inc(FCurrent^.next, FExprrec^.tk_prmc) else
      Inc(FCurrent^.next);
end;

procedure runner_jmpt(Sender: KLiRunner);
begin
  with Sender do
    if lse_get_bool(FStack[-1]) then
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
    if lse_get_bool(FStack[-1]) then
      FCurrent^.next := FExprrec^.VLabel^.tk_prmc else
      Inc(FCurrent^.next);
    FStack.DeleteLast;
  end;
end;

procedure runner_gotofp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if not lse_get_bool(FStack[-1]) then
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
      H.SetValue(lse_get_str(FStack[B]), FStack[B + 1]);
      Inc(B, 2);
      Inc(X, 2);
    end;
    FStack.Press(N);
    FStack.Add(H, KT_HASHED);
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
  lse_like(Sender.FStack[-2], Sender.FStack[-1]);
  Sender.FStack.DeleteLast;
  runner_next(Sender);
end;

procedure runner_getiv(Sender: KLiRunner);
var
  data, keyr: PLseValue;
  clss: PLseType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-2];
  clss := lse_type(data);
  keyr := Sender.FStack[-1];
  case lse_vtype(keyr) of
    LSV_INT:
      if Assigned(clss^.cr_getiv) then
      begin
        index := keyr^.VInteger;
        if clss^.cr_getiv(data^.VObject, index, keyr) > 0 then
        begin
          Sender.FStack.ExchangeLastTwo;
          Sender.FStack.DeleteLast;
        end
        else lse_error('failed get item by index of %d', [index]);
      end
      else lse_error('%s.getiv not supplied', [clss^.cr_name]);
    LSV_STRING:
      begin
        name := lse_strec_data(keyr^.VObject);
        if Assigned(clss^.cr_getpv) and
          (clss^.cr_getpv(data^.VObject, pchar(name), keyr) > 0) then
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
  clss: PLseType;
  name: string;
  index: integer;
  func, curr: KLiFunc;
begin
  data := Sender.FStack[-3];
  clss := lse_type(data);
  keyr := Sender.FStack[-2];
  case lse_vtype(keyr) of
    LSV_INT:
      if Assigned(clss^.cr_setiv) then
      begin
        index := keyr^.VInteger; 
        if clss^.cr_setiv(data^.VObject, index, Sender.FStack[-1]) > 0 then
          Sender.FStack.Press(2) else
          lse_error('failed set item by index of %d', [index]);
      end
      else lse_error('%s.setiv not supplied', [lse_type(data)^.cr_name]);
    LSV_STRING:
      begin
        name := lse_strec_data(keyr^.VObject);
        if Assigned(clss^.cr_setpv) and
           (clss^.cr_setpv(data^.VObject, pchar(name), Sender.FStack[-1]) > 0) then
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
    lse_fill(FStack[-2], FStack[-1]);
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
  lse_abseq(L, R);
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

// <TYPE> //////////////////////////////////////////////////////////////////////

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
  clss: PLseType;
begin
  clss := PLseType(obj);
  if clss <> nil then
    Result := lse_strec_alloc(type_full_name(clss)) else
    Result := nil;
end;

function type_varlist_otos(obj: pointer): PLseString;cdecl;
begin
  if obj <> nil then
    Result := lse_strec_alloc(KLiVarList(obj).AsString) else
    Result := nil;
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

function type_varlist_vargen(obj: pointer): PLseVargen;cdecl;
var
  list: KLiVarList;
  cvgr: PLiVG_varlist;
begin
  list := KLiVarList(obj);
  if list <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_varlist));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_type := KT_VARLIST;
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

function type_vargen_func_hasNext(vrec: PLseVargen): integer;cdecl;
var
  func: KLiFunc;
begin
  func := PLiVG_func(vrec^.vg_data)^.func;
  Result := Ord((func <> nil) and
                (func.FModule.FEngine <> nil) and
                (func.FModule.FEngine.FMainRunner <>nil));
end;

function type_vargen_func_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  func: KLiFunc;
begin
  if type_vargen_func_hasNext(vrec) <> 0 then
  begin
    func := PLiVG_func(vrec^.vg_data)^.func;
    Result := Ord(func.FModule.FEngine.MainRunner.Goon(func, 0, value));
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

function type_function_vargen(obj: pointer): PLseVargen;cdecl;
var
  func: KLiFunc;
  cvgr: PLiVG_func;
begin
  Result := nil;
  func := KLiFunc(obj);
  if (func <> nil) and (func.FModule.FEngine <> nil) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_func));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_type := KT_FUNC;
    cvgr^.vgrec.vg_rewind := @type_vargen_func_hasNext;
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

function type_stream_vargen(obj: pointer): PLseVargen;cdecl;
var
  stream: PLseStream;
  cvgr: PLiVG_stream;
begin
  stream := PLseStream(obj);
  if stream <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_stream));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_type := KT_STREAM;
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
  Result := type_stream_vargen(obj);
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

function type_string_vargen(obj: pointer): PLseVargen;cdecl;
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
    cvgr^.vgrec.vg_type := KT_STRING;
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

function type_int_vargen(obj: pointer): PLseVargen;cdecl;
var
  R: int64;
begin
  Move(obj^, R, sizeof(int64));
  Result := lse_vargen_upto(0, R - 1, 1);
end;

{ ADDI}

function type_varlist_add(obj: pointer; Value: PLseValue): integer;cdecl;
begin
  Result := KLiVarList(obj).Count;
  KLiVarList(obj).Add(Value);
end;

function type_stream_add(obj: pointer; Value: PLseValue): integer;cdecl;
var
  S: PLseStream;
begin
  S := PLseStream(obj);
  if lse_vtype(Value) = LSV_STRING then
    Result := lse_stream_write(S, Value^.VObject) else
    Result := lse_stream_write(S, lse_get_str(Value));
end;

{ GETIV: get item value }

function type_string_getiv(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
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

function type_varlist_getiv(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
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

{ SETIV: set item value }

function type_varlist_setiv(obj: pointer; index: integer; value: PLseValue): integer;cdecl;
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

{ GETPV: get property value }

function type_hashed_getpv(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
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

function type_module_getpv(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
var
  this: KLiModule;
  func: KLiFunc;
  clss: PLseType;
begin
  Result := 0;
  if obj <> nil then
  begin
    this := KLiModule(obj);
    clss := this.FindType(name);
    if clss <> nil then
    begin
      Result := 1;
      lse_set_type(value, clss);
    end
    else
    begin
      func := this.FindFunc(name);
      if func <> nil then
      begin
        Result := 1;
        func.SaveTo(value);
      end;
    end;
  end;
end;

function type_varsnap_getpv(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
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

function type_hashed_setpv(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    KLiHashed(obj).SetValue(name, value);
    Result := 1;
  end;
end;

function type_varsnap_setpv(obj: pointer; const name: pchar; value: PLseValue): integer;cdecl;
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

// <SYSTEM> ////////////////////////////////////////////////////////////////////

procedure pp_system_dir(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, GetCurrentDir);
end;

procedure pp_system_modules(const Param: PLseParam);cdecl;
var
  E: KLiEngine;
begin
  E := get_engine(Param);
  E.Modules.ToVarlist(E).SaveTo(Param^.p_result);
end;

procedure pp_system_libs(const Param: PLseParam);cdecl;
begin
  sys_libraries.ToVarlist(get_engine(Param)).SaveTo(Param^.p_result);
end;

procedure pp_system_exit(const Param: PLseParam);cdecl;
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

procedure pp_system_print(const Param: PLseParam);cdecl;
begin
  lse_stream_write(get_engine(Param).Output, Param^.p_param[0]^.VObject);
end;

procedure pp_system_printf(const Param: PLseParam);cdecl;
var
  inf: TFileStream;
  buf: array[0..1023] of char;
  len: integer;
  eng: KLiEngine;
begin
  if Param^.p_count > 0 then
  begin
    inf := TFileStream.Create(lse_get_fname(Param^.p_param[0]), fmShareDenyWrite);
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

procedure pp_system_println(const Param: PLseParam);cdecl;
var
  stdout: PLseStream;
begin
  stdout := get_engine(Param).Output;
  lse_stream_write(stdout, Param^.p_param[0]^.VObject);
  lse_stream_writeln(stdout);
end;

procedure pp_system_readln(const Param: PLseParam);cdecl;
var
  E: KLiEngine;
  S: PLseString;
  T: PLseType;
begin
  E := get_engine(Param); 
  S := lse_stream_readln(E.Input);
  lse_set_string(Param^.p_result, S);
  if Param^.p_count > 0 then
  begin
    T := PLseType(Param^.p_param[0]^.VObject);
    if (T <> nil) and not (T^.cr_type in [LSV_VARIANT, LSV_STRING]) then
      lse_type_cast(T, Param^.p_result);
  end;
end;

procedure pp_system_random(const Param: PLseParam);cdecl;
var
  F, T: integer;
begin
  if not sys_randomized then
  begin
    sys_randomized := true;
    Randomize;
    for F := Random(20) downto 0 do Random(100);
  end;
  F := lse_get_int(Param^.p_param[0]);
  T := lse_get_int(Param^.p_param[1]);
  if F <> T then
    F := Random(Abs(F - T)) + Min(F, T) else
  if F = 0 then
    F := Random(MaxInt);
  lse_set_int(Param^.p_result, F);
end;

procedure pp_system_sleep(const Param: PLseParam);cdecl;
var
  timeout: integer;
begin
  timeout := lse_get_int(Param^.p_param[0]);
  if timeout > 0 then
    Sleep(timeout);
end;

procedure pp_system_getenv(const Param: PLseParam);cdecl;
var
  ID: string;
begin
  ID := lse_get_str(Param^.p_param[0]);
  if ID <> '' then
    lse_set_string(Param^.p_result, lse_getenv(ID)) else
    lse_set_string(Param^.p_result, '', 0);
end;

procedure pp_system_dumpc(const Param: PLseParam);cdecl;
var
  stream: TStringStream;
  clss: PLseType;
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
    clss := lse_type(Param^.p_param[0]);
    if clss = KT_FUNC then
    begin
      func := KLiFunc(lse_get_obj(Param^.p_param[0]));
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
      module := KLiModule(lse_get_obj(Param^.p_param[0]));
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

procedure pp_system_length(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, lse_length(Param^.p_param[0]));
end;

procedure pp_system_genid(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, genid);
end;

procedure pp_system_parse(const Param: PLseParam);cdecl;
var
  code: string;
  func: KLiFunc;
begin
  code := lse_get_str(Param^.p_param[0]);
  if code <> '' then
  begin
    func := get_engine(Param).DoCompile(code);
    func.SaveTo(Param^.p_result);
    func.DecRefcount;
  end
  else sys_nothing.SaveTo(Param^.p_result);
end;

procedure pp_system_eval(const Param: PLseParam);cdecl;
var
  code: string;
  rnnr: KLiRunner;
  func: KLiFunc;
begin
  code := lse_get_str(Param^.p_param[0]);
  if code <> '' then
  begin
    rnnr := get_runner(Param);
    func := rnnr.Engine.DoCompile(code);
    try
      rnnr.Goon(func, 0, Param^.p_result);
    finally
      func.DecRefcount;
    end;
  end;
end;

procedure pp_system_load(const Param: PLseParam);cdecl;
var
  E: KLiEngine;
  M: KLiModule;
  F: string;
  N: string;
begin
  N := Trim(lse_get_str(Param^.p_param[0]));
  if N = '' then Exit;
  if lse_is_ident(pchar(N)) then
  begin
    lse_set_string(Param^.p_param[0], Format('{import %s}', [N]));
    pp_system_eval(Param);
    M := get_runner(Param).CurrentModule.FindModule(N, false);
    if M <> nil then
      M.SaveTo(Param^.p_result);
  end
  else
  begin
    kernel_lock;
    try
      E := get_engine(Param);
      F := lse_expand_fname(N);
      M := E.FindModuleByFileName(F);
      if M = nil then
      begin
        Inc(sys_seed);
        N := Format('#%.4d', [sys_seed]);
        if AnsiSameText(ExtractFileExt(F), LSE_DLLEXT) then
        begin
          M := module_load(N, F);
          if M = nil then
            lse_error('failed loading "%s"', [F]);
        end
        else
        try
          E.FCompiledObjects := TList.Create;
          M := KLiModule.Create(N, E, moyScript);
          M.FileName := F;
          M.FParsing := true;
          KLiParser.Create(M).ParseAndFree(file_Text(F));
          M.FParsing := false;
          FreeAndNil(E.FCompiledObjects);
        except
          E.RollbackCompiled;
          raise;
        end;
      end;
      M.SaveTo(Param^.p_result);
      get_runner(Param).CurrentModule.Import(M);
    finally
      kernel_unlock;
    end;
  end;
end;

procedure pp_system_uses(const Param: PLseParam);cdecl;
var
  M: KLiModule;
  T: PLseType;
  V: PLseValue;
  F: KLiFunc;
begin
  V := Param^.p_param[0];
  T := lse_type(V);
  if (T^.cr_type in [LSV_STRING, LSV_OBJECT]) and (V^.VObject <> nil) then
    if T^.cr_type = LSV_OBJECT then
    begin
      M := get_runner(Param).CurrentModule;
      if T = KT_MODULE then
      begin
        M := M.Import(KLiModule(V^.VObject));
        M.SaveTo(Param^.p_result);
      end
      else
      if T = KT_FUNC then
      begin
        F := KLiFunc(V^.VObject);
        F.SaveTo(Param^.p_result);
        M.Import(F.FModule);
      end
      else
      if T = KT_TYPE then
      begin
        T := PLseType(V^.VObject);
        lse_set_type(Param^.p_result, T);
        M.Import(type_module(T));
      end;
    end
    else pp_system_load(Param);
end;

procedure pp_system_format(const Param: PLseParam);cdecl;
var
  F: string;
  L: KLiVarList;
begin
  F := lse_get_str(Param^.p_param[0]);
  L := KLiVarList(Param^.p_param[1]^.VObject);
  lse_set_string(Param^.p_result, get_runner(Param).FormatFor(F, L));
end;

procedure pp_system_max(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or lse_match(V1, V2, [crEqual, crMore]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure pp_system_min(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or lse_match(V1, V2, [crEqual, crLess]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure pp_system_leap(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.p_result, IsLeapYear(lse_get_int(Param^.p_param[0])));
end;

procedure pp_system_which(const Param: PLseParam);cdecl;
var
  N: string;
  R: RLiFind;
begin
  N := Trim(lse_get_str(Param^.p_param[0]));
  if get_runner(Param).CurrentFunc.FindBy(N, @R) then
    case R.fo_type of
      foFunc  : R.VFunc.SaveTo(Param^.p_result);
      foType  : lse_set_type(Param^.p_result, R.VType);
      foModule: R.VModule.SaveTo(Param^.p_result)
    end;
end;

procedure pp_system_curry(const Param: PLseParam);cdecl;
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

procedure pp_system_curryone(const Param: PLseParam);cdecl;
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

procedure pp_system_gc(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, get_engine(Param).GarbageCollect);
end;

procedure pp_system_apply(const Param: PLseParam);cdecl;
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

procedure pp_system_tmpfname(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := sys_tmpath + genid + lse_get_fname(Param^.p_param[0]);
  lse_set_string(Param^.p_result, fname);
end;

procedure pp_system_encodeUTF8(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, encode_UTF8(lse_get_str(Param^.p_param[0])));
end;

procedure pp_system_decodeUTF8(const Param: PLseParam);cdecl;
begin
  if Param^.p_count > 0 then
    lse_set_string(Param^.p_result, decode_UTF8(lse_get_str(Param^.p_param[0])));
end;

procedure pp_system_openfs(const Param: PLseParam);cdecl;
var
  fname, fmode: string;
  open_mode: word;
  read, write: boolean;
  stream: PLseStream;
begin
  fname := lse_get_fname(Param^.p_param[0]);
  fmode := lse_get_str(Param^.p_param[1]);
  if fmode = '' then fmode := 'r';
  if get_open_file_mode(fmode, open_mode, read, write) then
  begin
    stream := lse_file_stream(fname, open_mode);
    lse_set_stream(Param^.p_result, stream);
  end
  else set_error(Param, 'Unknown file open mode "%s"', [fmode]);
end;

procedure pp_system_memory(const Param: PLseParam);cdecl;
var
  stream: PLseStream;
begin
  stream := lse_memory_stream;
  lse_set_stream(Param^.p_result, stream);
  if Param^.p_count > 1 then
    lse_stream_resize(stream, Max(0, Param^.p_param[0]^.VInteger));
end;

procedure pp_system_incPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, IncludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_excPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, ExcludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_veryPD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryPD(lse_get_str(Param^.p_param[0])));
end;

procedure pp_system_veryUD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryUD(lse_get_str(Param^.p_param[0])));
end;

procedure pp_system_msecs(const Param: PLseParam);cdecl;
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
    list := KLiVarList(lse_get_obj(Param^.p_param[1]));
    if list <> nil then
      prms := Min(func.ParamCount, list.Count) else
      prms := 0;
    rnnr.Stack.AddFrom(list, prms);
    rnnr.Goon(func, prms, Param^.p_result);
    lse_set_int(Param^.p_result, MilliSecondsBetween(Now, beg_time));
  end
  else lse_set_int(Param^.p_result, 0);
end;

procedure pp_system_current_module(const Param: PLseParam);cdecl;
begin
  get_runner(Param).CurrentFunc.Module.SaveTo(Param^.p_result);
end;

procedure pp_system_current_func(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  rnnr.Current^.func.SaveTo(Param^.p_result);
end;

procedure pp_system_current_error(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.p_result, KT_ERROR, get_engine(Param).Error);
end;

procedure pp_system_current_args(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  E: KLiEngine;
begin
  E := get_engine(Param);
  L := KLiVarList.Create(E);
  L.SaveTo(Param^.p_result);
  L.AddStrings(E.Arguments);
end;

procedure pp_system_current_line(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, int64(get_runner(Param).Exprrec^.tk_pos.row) + 1);
end;

procedure pp_system_current_envs(const Param: PLseParam);cdecl;
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

procedure pp_system_current_file(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, get_runner(Param).CurrentFunc.Module.FileName);
end;

procedure pp_system_current_pd(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, LSE_PATH_DELIMITER);
end;

procedure pp_system_eol(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, sys_LB);
end;

procedure pp_system_each(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  func := value_func(Param^.p_param[1]);
  if func = nil then Exit;
  rnnr := get_runner(Param);
  this := lse_vargen_this(Param);
  while rnnr.Stack.AddSend(this) do
    if not rnnr.Goon(func, 1, Param^.p_result) then
      Break;
end;

procedure pp_system_map(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  list := KLiVarList.Create(rnnr.Engine);
  lse_set_object(Param^.p_result, KT_VARLIST, list);
  this := lse_vargen_this(Param);
  func := value_func(Param^.p_param[1]);
  if func <> nil then
  begin
    lse_init_value(@data);
    try
      while rnnr.Stack.AddSend(this) do
        if rnnr.Goon(func, 1, @data) then
          list.Add(PLseValue(@data)) else
          Break;
    finally
      lse_set_nil(@data);
    end;
  end
  else list.AddAll(this);
end;

procedure pp_system_reduce(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  lse_set_value(Param^.p_result, Param^.p_param[1]);
  func := value_func(Param^.p_param[2]);
  if func = nil then Exit;
  this := lse_vargen_this(Param);
  rnnr := get_runner(Param);
  while true do
  begin
    rnnr.Stack.Add(Param^.p_result);
    if not rnnr.Stack.AddSend(this)
    or not rnnr.Goon(func, 2, Param^.p_result) then Break;
  end;
end;

procedure pp_system_filter(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  test, data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := get_runner(Param);
  list := KLiVarList.Create(rnnr.Engine);
  lse_set_object(Param^.p_result, KT_VARLIST, list);
  func := value_func(Param^.p_param[1]);
  if func = nil then Exit;
  lse_init_value(@test);
  lse_init_value(@data);
  try
    this := lse_vargen_this(Param);
    while lse_vargen_send(this, @data) do
    begin
      rnnr.Stack.Add(PLseValue(@data));
      if not rnnr.Goon(func, 1, @test) then Break else
      if lse_get_bool(@test) then
        list.Add(PLseValue(@data));
    end;
  finally
    lse_clear_value(@data);
    lse_clear_value(@test);
  end;
end;

procedure pp_system_maxint(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, high(int64));
end;

procedure pp_system_minint(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, low(int64));
end;

procedure pp_system_abs(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: PLseType;
begin
  data := Param^.p_param[0];
  clss := lse_type(data);
  case clss^.cr_type of
    LSV_INT  : lse_set_int(Param^.p_result, Abs(data^.VInteger));
    LSV_FLOAT: lse_set_float(Param^.p_result, Abs(data^.VFloat));
          else lse_set_value(Param^.p_result, data);
  end;
end;

procedure pp_system_gsub(const Param: PLseParam);cdecl;
var
  P: TLsePatten;
  N: integer;
begin
  P := TLsePatten.Create(lse_get_pchar(Param^.p_param[1]));
  try
    if Param^.p_count > 3 then
      N := lse_get_int(Param^.p_param[3]) else
      N := MaxInt;
    lse_set_string(Param^.p_result, P.Replace(
      lse_get_strec(Param^.p_param[0]),
      lse_get_strec(Param^.p_param[2]), N));
  finally
    P.Free;
  end;
end;

procedure pp_system_split(const Param: PLseParam);cdecl;
var
  P: TLsePatten;
  L: KLiVarList;
  S: pchar;
begin
  L := KLiVarList.Create(get_engine(Param));
  L.SaveTo(Param^.p_result);
  P := TLsePatten.Create(lse_get_pchar(Param^.p_param[1]));
  try
    if P.ExecStrec(lse_get_strec(Param^.p_param[0])) then
    begin
      S := P.Source;
      repeat
        L.Add(lse_strec_alloc(S, P.MatchStr(0) - S));
        S := P.MatchStr(0) + P.MatchLen(0);
      until not P.Next;
      L.Add(lse_strec_alloc(S, P.SourceLength - (S - P.Source)));
    end
    else L.Add(lse_get_strec(Param^.p_param[0]));
  finally
    P.Free;
  end;
end;

procedure pp_system_throw(const Param: PLseParam);cdecl;
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
      eid := Trim(lse_get_str(Param^.p_param[1]));
      if eid = '' then
        eid := RuntimeError;
    end
    else eid := RuntimeError;

    msg := Trim(lse_get_str(Param^.p_param[0]));
    if msg = '' then
      msg := EsRuntimeError;

    set_error(Param, eid, 0, msg);
  end;
end;

procedure pp_system_hex(const Param: PLseParam);cdecl;
var
  this: int64;
  size, digits: integer;
  text: string;
begin
  this := lse_get_int(Param^.p_param[0]);
  size := Max(0, lse_get_int(Param^.p_param[1]));
  digits := Min(16, size);
  if digits > 1 then
    text := Format('%.' + IntToStr(digits) + 'x', [this]) else
    text := Format('%x', [this]);
  digits := Length(text);
  if digits < size then
    text := StringOfChar('0', size - digits) + text;
  lse_set_string(Param^.p_result, text);
end;

procedure pp_system_typeof(const Param: PLseParam);cdecl;
begin
  lse_set_type(Param^.p_result, lse_get_type(Param^.p_param[0]));
end;

{ error }

procedure pp_error_text(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_string(Param^.p_result, E.ErrorText);
end;

procedure pp_error_module(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_string(Param^.p_result, E.module);
end;

procedure pp_error_name(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_string(Param^.p_result, E.Name);
end;

procedure pp_error_message(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_string(Param^.p_result, E.msg);
end;

procedure pp_error_row(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_int(Param^.p_result, E.row);
end;

procedure pp_error_col(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_int(Param^.p_result, E.col);
end;

procedure pp_error_errno(const Param: PLseParam);cdecl;
var
  E: KLiError;
begin
  if get_this(Param, E) then
    lse_set_int(Param^.p_result, E.errno);
end;

{ function }

procedure pp_func_name(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
begin
  if get_this(Param, F) then
    lse_set_string(Param^.p_result, F.Name);
end;

procedure pp_func_desc(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
begin
  if get_this(Param, F) then
    lse_set_string(Param^.p_result, F.Description);
end;

procedure pp_func_type(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
begin
  if get_this(Param, F) then
    lse_set_type(Param^.p_result, F.FResultType);
end;

procedure pp_func_prototype(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
begin
  if get_this(Param, F) then
    lse_set_string(Param^.p_result, F.Prototype);
end;

procedure pp_func_module(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
begin
  if get_this(Param, F) then
    F.Module.SaveTo(Param^.p_result);
end;

procedure pp_func_paramNames(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
  L: KLiVarList;
  X: integer;
begin
  if get_this(Param, F) then
  begin
    L := KLiVarList.Create(get_engine(Param));
    L.SaveTo(Param^.p_result);
    for X := 0 to F.ParamCount - 1 do
      L.Add(F.GetParam(X)^.va_name);
  end;
end;

procedure pp_func_paramTypes(const Param: PLseParam);cdecl;
var
  F: KLiFunc;
  L: KLiVarList;
  X: integer;
begin
  if get_this(Param, F) then
  begin
    L := KLiVarList.Create(get_engine(Param));
    L.SaveTo(Param^.p_result);
    for X := 0 to F.ParamCount - 1 do
      lse_set_type(L.Add, F.GetParam(X)^.va_type);
  end;
end;

{ hashed }

procedure pp_hashed_create(const Param: PLseParam);cdecl;
var
  buckets: integer;
begin
  if Param^.p_count > 0 then
    buckets := Max(1, lse_get_int(Param^.p_param[0])) else
    buckets := 1;
  KLiHashed.Create(get_engine(Param), buckets).SaveTo(Param^.p_result);
end;

procedure pp_hashed_keys(const Param: PLseParam);cdecl;
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

procedure pp_hashed_values(const Param: PLseParam);cdecl;
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

procedure pp_hashed_remove(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if get_this(Param, this) and (Param^.p_count > 1) then
    this.Remove(lse_get_str(Param^.p_param[1]));
end;

procedure pp_hashed_clear(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if get_this(Param, this) then
    this.Clear;
end;

procedure pp_hashed_read(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if get_this(Param, this) then
  begin
    key := lse_get_str(Param^.p_param[1]);
    data := this.FindValue(key);
    if data = nil then
      lse_set_value(Param^.p_result, Param^.p_param[2]) else
      lse_set_value(Param^.p_result, data);
  end;
end;

procedure pp_hashed_isset(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
begin
  if get_this(Param, this) and (Param^.p_count > 1) then
  begin
    key := lse_get_str(Param^.p_param[1]);
    lse_set_bool(Param^.p_result, this.IsSet(key));
  end;
end;

{ module }

procedure pp_module_name(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_module_desc(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure pp_module_file(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.FileName);
end;

procedure pp_module_modules(const Param: PLseParam);cdecl;
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

procedure pp_module_funcs(const Param: PLseParam);cdecl;
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
      L.Add(F, KT_FUNC);
      F := F.Next;
    end;
  end;
end;

procedure pp_module_types(const Param: PLseParam);cdecl;
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
      list.Add(this.GetType(index), KT_TYPE);
  end;
end;

procedure pp_module_version(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if get_this(Param, this) then
    lse_set_string(Param^.p_result, this.Version);
end;

procedure pp_module_main(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  func: KLiFunc;
begin
  if get_this(Param, this) then
  begin
    func := this.FMainFunc;
    if func <> nil then
    begin
      this.FMainFunc := nil;
      get_runner(Param).Goon(func, 0, Param^.p_result);
    end;
  end;
end;

{ vargen }

procedure pp_vargen_create(const Param: PLseParam);cdecl;
begin
  lse_set_vargen(Param^.p_result, lse_get_vargen(Param^.p_param[0]));
end;

procedure pp_vargen_eof(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.p_result, lse_vargen_eof(lse_vargen_this(Param)));
end;

procedure pp_vargen_next(const Param: PLseParam);cdecl;
begin
  lse_vargen_send(lse_vargen_this(Param), Param^.p_result);
end;

procedure pp_vargen_rewind(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.p_result, lse_vargen_rewind(lse_vargen_this(Param)));
end;

{ stream }

procedure pp_stream_close(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    S^.s_close(S);
end;

procedure pp_stream_eof(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_bool(Param^.p_result, S^.s_eof(S) <> 0);
end;

procedure pp_stream_get_position(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_int(Param^.p_result, S^.s_seek(S, 0, SSF_CURRENT));
end;

procedure pp_stream_set_position(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    S^.s_seek(S, Param^.p_param[1]^.VInteger, SSF_BEGINNING);
end;

procedure pp_stream_set_length(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    S^.s_set_size(S, Param^.p_param[1]^.VInteger);
end;

procedure pp_stream_read(const Param: PLseParam);cdecl;
var
  S: PLseStream;
  N: integer;
  L: string;
begin
  if get_this(Param, S) then
  begin
    if Assigned(S^.s_read) then
    begin
      N := Param^.p_param[1]^.VInteger;
      if N > 0 then
      begin
        SetLength(L, N);
        N := S^.s_read(S, pointer(L), N);
        if N > 0 then
        begin
          lse_set_string(Param^.p_result, pchar(L), N);
          Exit;
        end;
      end;
    end;
    lse_set_string(Param^.p_result, '');
  end;
end;

procedure pp_stream_readln(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_string(Param^.p_result, S^.s_readln(S));
end;

procedure pp_stream_write(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_int(Param^.p_result,
      lse_stream_write(S, Param^.p_param[1]^.VObject));
end;

procedure pp_stream_writeln(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_int(Param^.p_result,
      lse_stream_writeln(S, Param^.p_param[1]^.VObject));
end;

procedure pp_stream_writeTo(const Param: PLseParam);cdecl;
var
  S, T: PLseStream;
  N: integer;
begin
  if get_this(Param, S) then
  begin
    T := PLseStream(Param^.p_param[1]^.VObject);
    if (T <> nil) and (T <> S) then
      N := lse_stream_fill(T, S, Param^.p_param[2]^.VInteger) else
      N := 0;
    lse_set_int(Param^.p_result, N);
  end;
end;

procedure pp_stream_flush(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    S^.s_flush(S);
end;

procedure pp_stream_lines(const Param: PLseParam);cdecl;
var
  S: PLseStream;
begin
  if get_this(Param, S) then
    lse_set_vargen(Param^.p_result,
      type_stream_vargen_lines(S, get_engine(Param)));
end;

{ string }

procedure pp_string_setAt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_set(
    Param^.p_param[0]^.VObject,
    lse_get_int(Param^.p_param[1]),
    lse_get_char(Param^.p_param[2])));
end;

procedure pp_string_name(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_name(Param^.p_param[0]^.VObject));
end;

procedure pp_string_value(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_value(Param^.p_param[0]^.VObject));
end;

procedure pp_string_lower(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_lower(Param^.p_param[0]^.VObject));
end;

procedure pp_string_upper(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_upper(Param^.p_param[0]^.VObject));
end;

procedure pp_string_compare(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, lse_strec_comp(
    Param^.p_param[0]^.VObject,
    Param^.p_param[1]^.VObject,
    lse_get_bool(Param^.p_param[2])));
end;

procedure pp_string_replace(const Param: PLseParam);cdecl;
var
  this, patten, newStr: pchar;
  flags: TReplaceFlags;
begin
  this := lse_strec_data(Param^.p_param[0]^.VObject);
  patten := lse_strec_data(Param^.p_param[1]^.VObject);
  newStr := lse_strec_data(Param^.p_param[2]^.VObject);
  flags := [rfReplaceAll];
  if lse_get_bool(Param^.p_param[3]) then // IgnoreCase
    flags := flags + [rfIgnoreCase];
  if lse_get_bool(Param^.p_param[4]) then // FirstOnly
    flags := flags - [rfReplaceAll];
  lse_set_string(Param^.p_result, StringReplace(this, patten, newStr, flags));
end;

procedure pp_string_pos(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, lse_strec_pos(
    Param^.p_param[0]^.VObject,
    Param^.p_param[1]^.VObject));
end;

procedure pp_string_lastPos(const Param: PLseParam);cdecl;
begin
  lse_set_int(Param^.p_result, lse_strec_last_pos(
    Param^.p_param[0]^.VObject,
    Param^.p_param[1]^.VObject));
end;

procedure pp_string_left(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_left(
    Param^.p_param[0]^.VObject,
    lse_get_int(Param^.p_param[1])));
end;

procedure pp_string_right(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_right(
    Param^.p_param[0]^.VObject,
    lse_get_int(Param^.p_param[1])));
end;

procedure pp_string_trim(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_trim(Param^.p_param[0]^.VObject));
end;

procedure pp_string_trimLeft(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_trimL(Param^.p_param[0]^.VObject));
end;

procedure pp_string_trimRight(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_trimR(Param^.p_param[0]^.VObject));
end;

procedure pp_string_trimAll(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_trim_all(Param^.p_param[0]^.VObject));
end;

procedure pp_string_copy(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_copy(
    Param^.p_param[0]^.VObject,
    lse_get_int(Param^.p_param[1]),
    lse_get_int(Param^.p_param[2])));
end;

procedure pp_string_delete(const Param: PLseParam);cdecl;
var
  N: int64;
begin
  if Param^.p_count > 2 then
    N := lse_get_int(Param^.p_param[2]) else
    N := 1;
  lse_set_string(Param^.p_result, lse_strec_delete(
    Param^.p_param[0]^.VObject,
    lse_get_int(Param^.p_param[1]), N));
end;

procedure pp_string_insert(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_insert(
    Param^.p_param[0]^.VObject,
    Param^.p_param[1]^.VObject,
    lse_get_int(Param^.p_param[2])));
end;

procedure pp_string_isAlpha(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_ALPHA));
end;

procedure pp_string_isAlnum(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_ALNUM));
end;

procedure pp_string_isCntrl(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_CNTRL));
end;

procedure pp_string_isDigit(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_DIGIT));
end;

procedure pp_string_isSpace(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_SPACE));
end;

procedure pp_string_isHex(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this), LCS_HEX));
end;

procedure pp_string_saveToFile(const Param: PLseParam);cdecl;
begin
  lse_strec_save(Param^.p_param[0]^.VObject, lse_get_fname(Param^.p_param[1]));
end;

procedure pp_string_fileText(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_strec_load(
    lse_get_fname(Param^.p_param[0])));
end;

procedure pp_string_isLower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this),
    lse_strec_length(this), ['a'..'z']));
end;

procedure pp_string_isUpper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, lse_in_charset(lse_strec_data(this),
    lse_strec_length(this), ['A'..'Z']));
end;

procedure pp_string_filePath(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFilePath(lse_get_fname(Param^.p_param[0])));
end;

procedure pp_string_fullFileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    lse_expand_fname(lse_get_fname(Param^.p_param[0])));
end;

procedure pp_string_fileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileName(lse_get_fname(Param^.p_param[0])));
end;

procedure pp_string_fileExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileExt(lse_get_fname(Param^.p_param[0])));
end;

procedure pp_string_changeExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ChangeFileExt(lse_get_fname(Param^.p_param[0]),
                  lse_get_fname(Param^.p_param[1])));
end;

procedure pp_string_hexToInt(const Param: PLseParam);cdecl;
var
  S: string;
begin
  S := lse_get_str(Param^.p_param[0]^.VObject);
  lse_set_int(Param^.p_result, StrToInt64Def('$' + S, 0));
end;

procedure pp_string_lines(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  S, N: pchar;
begin
  L := KLiVarList.Create(get_engine(Param));
  L.SaveTo(Param^.p_result);
  S := lse_get_pchar(Param^.p_param[0]);
  if (S <> nil) and (S^ <> #0) then
  repeat
    N := S;
    while not (N^ in [#0, #13, #10]) do Inc(N);
    L.Add(lse_strec_alloc(S, N - S));
    if N^ = #13 then Inc(N);
    if N^ = #10 then Inc(N);
    S := N;
  until S^ = #0;
end;

{ type }

procedure pp_type_name(const Param: PLseParam);cdecl;
var
  T: PLseType;
begin
  if get_this(Param, T) then
    lse_set_string(Param^.p_result, T^.cr_name);
end;

procedure pp_type_description(const Param: PLseParam);cdecl;
var
  T: PLseType;
begin
  if get_this(Param, T) then
    lse_set_string(Param^.p_result, lse_type_desc(T));
end;

procedure pp_type_module(const Param: PLseParam);cdecl;
var
  T: PLseType;
begin
  if get_this(Param, T) then
    type_module(T).SaveTo(Param^.p_result);
end;

procedure pp_type_methods(const Param: PLseParam);cdecl;
var
  T: PLseType;
  L: KLiVarList;
  F: boolean;
begin
  if get_this(Param, T) then
  begin
    F := lse_get_bool(Param^.p_param[1]); 
    L := get_runner(Param).CurrentFunc.ListMethod(T, F);
    L.SaveTo(Param^.p_result);
  end;
end;

{ varlist }

procedure pp_varlist_create(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  L := KLiVarList.Create(get_engine(Param)); 
  L.SaveTo(Param^.p_result);
  if Param^.p_count > 0 then
    L.Count := Max(0, Param^.p_param[0]^.VInteger);
end;

procedure pp_varlist_get_length(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    lse_set_int(Param^.p_result, L.Count);
end;

procedure pp_varlist_set_length(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
      L.Count := Max(0, Param^.p_param[1]^.VInteger);
end;

procedure pp_varlist_exchange(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X1, X2: int64;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      X1 := lse_vary_index(lse_get_int(Param^.p_param[1]), L.Count);
      X2 := lse_vary_index(lse_get_int(Param^.p_param[2]), L.Count);
      if X1 <> X2 then
        L.Exchange(X1, X2);
    end;
end;

procedure pp_varlist_move(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X1, X2: int64;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      X1 := lse_vary_index(lse_get_int(Param^.p_param[1]), L.Count);
      X2 := lse_vary_index(lse_get_int(Param^.p_param[2]), L.Count);
      if X1 <> X2 then
        L.Move(X1, X2);
    end;
end;

procedure pp_varlist_add(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      lse_set_int(Param^.p_result, L.Count);
      L.Add(Param^.p_param[1]);
    end;
end;

procedure pp_varlist_addFrom(const Param: PLseParam);cdecl;
var
  L, F: KLiVarList;
  X, N: integer;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      F := KLiVarList(Param^.p_param[1]^.VObject);
      if (F <> nil) and (F.Count > 0) then
      begin
        N := F.Count;
        for X := 0 to N - 1 do L.Add(F[X]);
      end;
    end;
end;

procedure pp_varlist_insert(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X: int64;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      X := lse_vary_index(lse_get_int(Param^.p_param[1]), L.Count);
      lse_check_index(X, int64(L.Count) + 1);
      lse_set_value(L.Insert(X), Param^.p_param[2]);
    end;
end;

procedure pp_varlist_delete(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X: int64;
begin
  if get_this(Param, L) then
    if not (L is KLiVarSnap) then
    begin
      X := lse_vary_index(lse_get_int(Param^.p_param[1]), L.Count);
      lse_check_index(X, L.Count);
      L.Delete(X);
    end;
end;

procedure pp_varlist_clear(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    if L is KLiVarSnap then
      KLiVarSnap(L).Reset else
      L.Clear;
end;

procedure pp_varlist_copy(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X, N: int64;
begin
  if get_this(Param, L) then
  begin
    X := lse_get_int(Param^.p_param[1]);
    N := lse_get_int(Param^.p_param[2]);
    X := lse_vary_range(X, L.Count, N);
    L.Copy(X, N).SaveTo(Param^.p_result);
  end;
end;

procedure pp_varlist_left(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    L.Left(lse_get_int(Param^.p_param[1])).SaveTo(Param^.p_result);
end;

procedure pp_varlist_right(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    L.Right(lse_get_int(Param^.p_param[1])).SaveTo(Param^.p_result);
end;

procedure pp_varlist_first(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    lse_set_value(Param^.p_result, L[0]);
end;

procedure pp_varlist_last(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
    lse_set_value(Param^.p_result, L[-1]);
end;

procedure pp_varlist_shift(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
begin
  if get_this(Param, L) then
  begin
    lse_set_value(Param^.p_result, L[0]);
    if not (L is KLiVarSnap) then L.Delete(0);
  end;
end;

procedure pp_varlist_pop(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  X: integer;
begin
  if get_this(Param, L) then
  begin
    X := L.Count - 1;
    lse_set_value(Param^.p_result, L[X]);
    if not (L is KLiVarSnap) then L.Delete(X);
  end;
end;

const
  sys_func_count = 160;
  sys_func_array: array[0..sys_func_count - 1] of RLseFunc = (
    (fr_prot:'dir:string ||';
     fr_addr:@pp_system_dir;
     fr_desc:'get current directory';
    ),
    (fr_prot:'modules:varlist ||';
     fr_addr:@pp_system_modules;
     fr_desc:'return loaded script module list';
    ),
    (fr_prot:'libs:varlist ||';
     fr_addr:@pp_system_libs;
     fr_desc:'return loaded libraries';
    ),
    (fr_prot:'print |text:string|';
     fr_addr:@pp_system_print;
     fr_desc:'print text into standard output';
    ),
    (fr_prot:'println |text:string|';
     fr_addr:@pp_system_println;
     fr_desc:'print text and a line break into standard output';
    ),
    (fr_prot:'printf |fileName:string|';
     fr_addr:@pp_system_printf;
     fr_desc:'print file content into standard output';
    ),
    (fr_prot:'readln |T:type|';
     fr_addr:@pp_system_readln;
     fr_desc:'read from standard input';
    ),
    (fr_prot:'exit |status:int|';
     fr_addr:@pp_system_exit;
     fr_desc:'exit with status code';
    ),
    (fr_prot:'random:int |low:int, high:int|';
     fr_addr:@pp_system_random;
     fr_desc:'generate random number';
    ),
    (fr_prot:'sleep |milliSeconds:int|';
     fr_addr:@pp_system_sleep;
     fr_desc:'sleep a number of milliseconds';
    ),
    (fr_prot:'getenv:string |name:string|';
     fr_addr:@pp_system_getenv;
     fr_desc:'get environment value';
    ),
    (fr_prot:'dumpc:string |any|';
     fr_addr:@pp_system_dumpc;
     fr_desc:'dump object p-codes';
    ),
    (fr_prot:'length:int |any|';
     fr_addr:@pp_system_length;
     fr_desc:'get string length or item count';
    ),
    (fr_prot:'genid:string ||';
     fr_addr:@pp_system_genid;
     fr_desc:'generate a global unique string';
    ),
    (fr_prot:'nothing ||';
     fr_addr:@udc_empty;
     fr_desc:'do nothing';
    ),
    (fr_prot:'parse:function |script:string|';
     fr_addr:@pp_system_parse;
     fr_desc:'parse and compile script';
    ),
    (fr_prot:'eval |script:string|';
     fr_addr:@pp_system_eval;
     fr_desc:'evaluate a block of lysee script';
    ),
    (fr_prot:'load:module |fileName:string|';
     fr_addr:@pp_system_load;
     fr_desc:'load module dynamically';
    ),
    (fr_prot:'uses |any|';
     fr_addr:@pp_system_uses;
     fr_desc:'uses module or functions';
    ),
    (fr_prot:'format:string |fmt:string, args:varlist|';
     fr_addr:@pp_system_format;
     fr_desc:'format string';
    ),
    (fr_prot:'max |v1, v2|';
     fr_addr:@pp_system_max;
     fr_desc:'get max value';
    ),
    (fr_prot:'min |v1, v2|';
     fr_addr:@pp_system_min;
     fr_desc:'get min value';
    ),
    (fr_prot:'leap:int |year:int|';
     fr_addr:@pp_system_leap;
     fr_desc:'test leap year';
    ),
    (fr_prot:'which |name:string|';
     fr_addr:@pp_system_which;
     fr_desc:'find function, module or anything else by name';
    ),
    (fr_prot:'curry:function |func:function, paramList:varlist|';
     fr_addr:@pp_system_curry;
     fr_desc:'curry function with parametre list';
    ),
    (fr_prot:'curryOne:function |func:function, value|';
     fr_addr:@pp_system_curryone;
     fr_desc:'curry function with one parametre';
    ),
    (fr_prot:'gc ||';
     fr_addr:@pp_system_gc;
     fr_desc:'execute garbage collection immediately';
    ),
    (fr_prot:'apply |func:function, params:varlist|';
     fr_addr:@pp_system_apply;
     fr_desc:'call function with supplied parametres';
    ),
    (fr_prot:'tempFileName:string |fileExt:string|';
     fr_addr:@pp_system_tmpfname;
     fr_desc:'generate temp file name at temp path';
    ),
    (fr_prot:'encodeUTF8:string |ANSI:string|';
     fr_addr:@pp_system_encodeUTF8;
     fr_desc:'encode ANSI string to UTF8 format';
    ),
    (fr_prot:'decodeUTF8:string |UTF8:string|';
     fr_addr:@pp_system_decodeUTF8;
     fr_desc:'decode UTF8 string to ANSI format';
    ),
    (fr_prot:'openfs:stream |fileName:string, mode:string|';
     fr_addr:@pp_system_openfs;
     fr_desc:'open file stream by specified mode: CRWE (default R)';
    ),
    (fr_prot:'memory:stream |size:int|';
     fr_addr:@pp_system_memory;
     fr_desc:'create memory stream';
    ),
    (fr_prot:'incPD:string |dir:string|';
     fr_addr:@pp_system_incPD;
     fr_desc:'include trailing path delimiter';
    ),
    (fr_prot:'excPD:string |path:string|';
     fr_addr:@pp_system_excPD;
     fr_desc:'exclude trailing path delimiter';
    ),
    (fr_prot:'veryPD:string |path:string|';
     fr_addr:@pp_system_veryPD;
     fr_desc:'correct path delimiter';
    ),
    (fr_prot:'veryUD:string |RUL:string|';
     fr_addr:@pp_system_veryUD;
     fr_desc:'correct URL delimiter';
    ),
    (fr_prot:'msecs:int |func:function, params:varlist|';
     fr_addr:@pp_system_msecs;
     fr_desc:'count milliseconds used to call a function';
    ),
    (fr_prot:'__module__:module ||';
     fr_addr:@pp_system_current_module;
     fr_desc:'get current module';
    ),
    (fr_prot:'__func__:function ||';
     fr_addr:@pp_system_current_func;
     fr_desc:'get current function';
    ),
    (fr_prot:'__error__:error ||';
     fr_addr:@pp_system_current_error;
     fr_desc:'get current error';
    ),
    (fr_prot:'__args__:varlist ||';
     fr_addr:@pp_system_current_args;
     fr_desc:'get argument list';
    ),
    (fr_prot:'__line__:int ||';
     fr_addr:@pp_system_current_line;
     fr_desc:'get current line number';
    ),
    (fr_prot:'__envs__:varlist ||';
     fr_addr:@pp_system_current_envs;
     fr_desc:'get environment value list';
    ),
    (fr_prot:'__file__:string ||';
     fr_addr:@pp_system_current_file;
     fr_desc:'get current file';
    ),
    (fr_prot:'__pd__:string ||';
     fr_addr:@pp_system_current_pd;
     fr_desc:'get system path delimiter';
    ),
    (fr_prot:'eol:string ||';
     fr_addr:@pp_system_eol;
     fr_desc:'get eol(line break)';
    ),
    (fr_prot:'each |any:vargen, func:function|';
     fr_addr:@pp_system_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'map:varlist |any:vargen, func:function|';
     fr_addr:@pp_system_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'reduce |any:vargen, initValue, func:function|';
     fr_addr:@pp_system_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'filter:varlist |any:vargen, func:function|';
     fr_addr:@pp_system_filter;
     fr_desc:'filter item: |item| ... end';
    ),
    (fr_prot:'__maxint__:int ||';
     fr_addr:@pp_system_maxint;
     fr_desc:'get max integer value';
    ),
    (fr_prot:'__minint__:int ||';
     fr_addr:@pp_system_minint;
     fr_desc:'get min integer value';
    ),
    (fr_prot:'abs |value|';
     fr_addr:@pp_system_abs;
     fr_desc:'get absolute value';
    ),
    (fr_prot:'gsub:string |S:string, patten:string, newStr:string, count:int|';
     fr_addr:@pp_system_gsub;
     fr_desc:'replace patten with new string';
    ),
    (fr_prot:'split:varlist |S:string, patten:string|';
     fr_addr:@pp_system_split;
     fr_desc:'split string to varlist';
    ),
    (fr_prot:'throw:void |exceptionMsg:string, exceptionID:string|';
     fr_addr:@pp_system_throw;
     fr_desc:'throw exception';
    ),
    (fr_prot:'hex:string |value:int, size:int|';
     fr_addr:@pp_system_hex;
     fr_desc:'convert to hex string';
    ),
    (fr_prot:'typeof:type |any|';
     fr_addr:@pp_system_typeof;
     fr_desc:'get value type';
    ),
  { error }
    (fr_prot:'error_get_text:string |e:error|';
     fr_addr:@pp_error_text;
     fr_desc:'error text';
    ),
    (fr_prot:'error_get_module:string |e:error|';
     fr_addr:@pp_error_module;
     fr_desc:'error module';
    ),
    (fr_prot:'error_get_name:string |e:error|';
     fr_addr:@pp_error_name;
     fr_desc:'error name';
    ),
    (fr_prot:'error_get_message:string |e:error|';
     fr_addr:@pp_error_message;
     fr_desc:'error message';
    ),
    (fr_prot:'error_get_row:int |e:error|';
     fr_addr:@pp_error_row;
     fr_desc:'error row';
    ),
    (fr_prot:'error_get_col:int |e:error|';
     fr_addr:@pp_error_col;
     fr_desc:'error column';
    ),
    (fr_prot:'error_get_errno:int |e:error|';
     fr_addr:@pp_error_errno;
     fr_desc:'error code';
    ),
  { function }
    (fr_prot:'function_get_name:string |f:function|';
     fr_addr:@pp_func_name;
     fr_desc:'function name';
    ),
    (fr_prot:'function_get_description:string |f:function|';
     fr_addr:@pp_func_desc;
     fr_desc:'function description';
    ),
    (fr_prot:'function_get_type:string |f:function|';
     fr_addr:@pp_func_type;
     fr_desc:'get function result type';
    ),
    (fr_prot:'function_get_prototype:string |f:function|';
     fr_addr:@pp_func_prototype;
     fr_desc:'function prototype';
    ),
    (fr_prot:'function_get_module:module |f:function|';
     fr_addr:@pp_func_module;
     fr_desc:'function module';
    ),
    (fr_prot:'function_get_pnames:varlist |f:function|';
     fr_addr:@pp_func_paramNames;
     fr_desc:'function parametre name list';
    ),
    (fr_prot:'function_get_ptypes:varlist |f:function|';
     fr_addr:@pp_func_paramTypes;
     fr_desc:'function parametre type list';
    ),
  { hashed }
    (fr_prot:'hashed_create:hashed |buckets:int|';
     fr_addr:@pp_hashed_create;
     fr_desc:'create a hashed key-value list';
    ),
    (fr_prot:'hashed_clear:void |h:hashed|';
     fr_addr:@pp_hashed_clear;
     fr_desc:'clear list item';
    ),
    (fr_prot:'hashed_remove:void |h:hashed, key:string|';
     fr_addr:@pp_hashed_remove;
     fr_desc:'remove specifed key value';
    ),
    (fr_prot:'hashed_isset:int |h:hashed, key:string|';
     fr_addr:@pp_hashed_isset;
     fr_desc:'check if the key exists';
    ),
    (fr_prot:'hashed_read |h:hashed, key:string, defaultValue|';
     fr_addr:@pp_hashed_read;
     fr_desc:'read key value';
    ),
    (fr_prot:'hashed_get_keys:varlist |h:hashed|';
     fr_addr:@pp_hashed_keys;
     fr_desc:'get hashed key list';
    ),
    (fr_prot:'hashed_get_values:varlist |h:hashed|';
     fr_addr:@pp_hashed_values;
     fr_desc:'get hashed value list';
    ),
  { module }
    (fr_prot:'module_get_name:string |m:module|';
     fr_addr:@pp_module_name;
     fr_desc:'module name';
    ),
    (fr_prot:'module_get_description:string |m:module|';
     fr_addr:@pp_module_desc;
     fr_desc:'module description';
    ),
    (fr_prot:'module_get_file:string |m:module|';
     fr_addr:@pp_module_file;
     fr_desc:'module file';
    ),
    (fr_prot:'module_get_modules:varlist |m:module|';
     fr_addr:@pp_module_modules;
     fr_desc:'imported module list';
    ),
    (fr_prot:'module_get_funcs:varlist |m:module|';
     fr_addr:@pp_module_funcs;
     fr_desc:'global function list';
    ),
    (fr_prot:'module_get_classes:varlist |m:module|';
     fr_addr:@pp_module_types;
     fr_desc:'get type list';
    ),
    (fr_prot:'module_get_version:string |m:module|';
     fr_addr:@pp_module_version;
     fr_desc:'type version';
    ),
    (fr_prot:'module_main |m:module|';
     fr_addr:@pp_module_main;
     fr_desc:'execute module''s main function';
    ),
  { vargen }
    (fr_prot:'vargen_create:vargen |any|';
     fr_addr:@pp_vargen_create;
     fr_desc:'create variant generator';
    ),
    (fr_prot:'vargen_get_eof:int |v:vargen|';
     fr_addr:@pp_vargen_eof;
     fr_desc:'test if finished';
    ),
    (fr_prot:'vargen_next |v:vargen|';
     fr_addr:@pp_vargen_next;
     fr_desc:'generate next value';
    ),
    (fr_prot:'vargen_rewind:int |v:vargen|';
     fr_addr:@pp_vargen_rewind;
     fr_desc:'restart from first element';
    ),
  { stream }
    (fr_prot:'stream_close:void |s:stream|';
     fr_addr:@pp_stream_close;
     fr_desc:'close stream';
    ),
    (fr_prot:'stream_resize:void |s:stream, length:int|';
     fr_addr:@pp_stream_set_length;
     fr_desc:'set stream size';
    ),
    (fr_prot:'stream_get_eof:int |s:stream|';
     fr_addr:@pp_stream_eof;
     fr_desc:'test if is at end of the stream';
    ),
    (fr_prot:'stream_position:string |s:stream|';
     fr_addr:@pp_stream_get_position;
     fr_desc:'get current position';
    ),
    (fr_prot:'stream_seekTo:void |s:stream, newPosition:int|';
     fr_addr:@pp_stream_set_position;
     fr_desc:'set current position';
    ),
    (fr_prot:'stream_read:string |s:stream, count:int|';
     fr_addr:@pp_stream_read;
     fr_desc:'read value';
    ),
    (fr_prot:'stream_readln:string |s:stream|';
     fr_addr:@pp_stream_readln;
     fr_desc:'read a line';
    ),
    (fr_prot:'stream_write:int |s:stream, text:string|';
     fr_addr:@pp_stream_write;
     fr_desc:'write into stream';
    ),
    (fr_prot:'stream_writeln:int |s:stream, text:string|';
     fr_addr:@pp_stream_writeln;
     fr_desc:'write text and a line break into stream';
    ),
    (fr_prot:'stream_writeTo:int |s:stream, stream:stream, count:int|';
     fr_addr:@pp_stream_writeTo;
     fr_desc:'write part data into another stream';
    ),
    (fr_prot:'stream_flush:void |s:stream|';
     fr_addr:@pp_stream_flush;
     fr_desc:'flush stream';
    ),
    (fr_prot:'stream_lines:vargen |s:stream|';
     fr_addr:@pp_stream_lines;
     fr_desc:'wrap stram as a line generator';
    ),
  { string }
    (fr_prot:'string_setAt:string |s:string, index:int, value:string|';
     fr_addr:@pp_string_setAt;
     fr_desc:'set char by index';
    ),
    (fr_prot:'string_get_name:string |s:string|';
     fr_addr:@pp_string_name;
     fr_desc:'extract name';
    ),
    (fr_prot:'string_get_value:string |s:string|';
     fr_addr:@pp_string_value;
     fr_desc:'extract value';
    ),
    (fr_prot:'string_compare:int |s:string, S2:string, ignoreCase|';
     fr_addr:@pp_string_compare;
     fr_desc:'compare two string';
     ),
    (fr_prot:'string_replace:string |s:string, patten:string, newStr:string, ignoreCase, replaceFirstOnly|';
     fr_addr:@pp_string_replace;
     fr_desc:'replace patten to new string';
    ),
    (fr_prot:'string_pos:int |s:string, SubStr:string|';
     fr_addr:@pp_string_pos;
     fr_desc:'get first sub-string position';
    ),
    (fr_prot:'string_lastPos:int |s:string, SubStr:string|';
     fr_addr:@pp_string_lastPos;
     fr_desc:'get last sub-string position';
    ),
    (fr_prot:'string_left:string |s:string, count:int|';
     fr_addr:@pp_string_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'string_right:string |s:string, count:int|';
     fr_addr:@pp_string_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'string_trim:string |s:string|';
     fr_addr:@pp_string_trim;
     fr_desc:'trim left and right';
    ),
    (fr_prot:'string_trimLeft:string |s:string|';
     fr_addr:@pp_string_trimLeft;
     fr_desc:'trim left';
    ),
    (fr_prot:'string_trimRight:string |s:string|';
     fr_addr:@pp_string_trimRight;
     fr_desc:'trim right';
    ),
    (fr_prot:'string_trimAll:string |s:string|';
     fr_addr:@pp_string_trimAll;
     fr_desc:'trim all spaces';
    ),
    (fr_prot:'string_copy:string |s:string, index:int, count:int|';
     fr_addr:@pp_string_copy;
     fr_desc:'copy sub-string';
    ),
    (fr_prot:'string_delete:string |s:string, index:int, count:int|';
     fr_addr:@pp_string_delete;
     fr_desc:'delete by range';
    ),
    (fr_prot:'string_insert:string |s:string, substr:string, index:int|';
     fr_addr:@pp_string_insert;
     fr_desc:'insert sub-string';
    ),
    (fr_prot:'string_lower:string |s:string|';
     fr_addr:@pp_string_lower;
     fr_desc:'convert to lower case string';
    ),
    (fr_prot:'string_upper:string |s:string|';
     fr_addr:@pp_string_upper;
     fr_desc:'convert to upper case string';
    ),
    (fr_prot:'string_isAlpha:int |s:string|';
     fr_addr:@pp_string_isAlpha;
     fr_desc:'test if the string contains only alpha charactors';
    ),
    (fr_prot:'string_isAlnum:int |s:string|';
     fr_addr:@pp_string_isAlnum;
     fr_desc:'test if the string contains only alpha and digit charactors';
    ),
    (fr_prot:'string_isCntrl:int |s:string|';
     fr_addr:@pp_string_isCntrl;
     fr_desc:'test if the string contains only control charactors';
    ),
    (fr_prot:'string_isSpace:int |s:string|';
     fr_addr:@pp_string_isSpace;
     fr_desc:'test if the string contains only space charactors';
    ),
    (fr_prot:'string_isDigit:int |s:string|';
     fr_addr:@pp_string_isDigit;
     fr_desc:'test if the string contains only digit charactors';
    ),
    (fr_prot:'string_isHex:int |s:string|';
     fr_addr:@pp_string_isHex;
     fr_desc:'test if the string contains only HEX charactors';
    ),
    (fr_prot:'string_fileText:string |s:string|';
     fr_addr:@pp_string_fileText;
     fr_desc:'get file text';
    ),
    (fr_prot:'string_saveToFile:void |s:string, fileName:string|';
     fr_addr:@pp_string_saveToFile;
     fr_desc:'save string to file';
    ),
    (fr_prot:'string_fullFileName:string |s:string|';
     fr_addr:@pp_string_fullFileName;
     fr_desc:'expand to full file name';
    ),
    (fr_prot:'string_filePath:string |s:string|';
     fr_addr:@pp_string_filePath;
     fr_desc:'extract file path';
    ),
    (fr_prot:'string_fileName:string |s:string|';
     fr_addr:@pp_string_fileName;
     fr_desc:'extract file name';
    ),
    (fr_prot:'string_fileExt:string |s:string|';
     fr_addr:@pp_string_fileExt;
     fr_desc:'extract file extension';
    ),
    (fr_prot:'string_changeExt:string |s:string, newFileExt:string|';
     fr_addr:@pp_string_changeExt;
     fr_desc:'change file extension';
    ),
    (fr_prot:'string_hexToInt:int |s:string, defaultValue:int|';
     fr_addr:@pp_string_hexToInt;
     fr_desc:'convert HEX string to integer value';
    ),
    (fr_prot:'string_lines:varlist |s:string|';
     fr_addr:@pp_string_lines;
     fr_desc:'split string by linebreak';
    ),
    (fr_prot:'string_isLower:int |s:string|';
     fr_addr:@pp_string_isLower;
     fr_desc:'test if is lower case string';
    ),
    (fr_prot:'string_isUpper:int |s:string|';
     fr_addr:@pp_string_isUpper;
     fr_desc:'test if is upper case string';
    ),
  { type }
    (fr_prot:'type_get_name:string |t:type|';
     fr_addr:@pp_type_name;
     fr_desc:'type name';
    ),
    (fr_prot:'type_get_module:module |t:type|';
     fr_addr:@pp_type_module;
     fr_desc:'owner module';
    ),
    (fr_prot:'type_get_description:string |t:type|';
     fr_addr:@pp_type_description;
     fr_desc:'type description';
    ),
    (fr_prot:'type_methods:varlist |t:type, listName:int|';
     fr_addr:@pp_type_methods;
     fr_desc:'list possible method';
    ),
  { varlist }
    (fr_prot:'varlist_create:varlist |count:int|';
     fr_addr:@pp_varlist_create;
     fr_desc:'create variant list';
    ),
    (fr_prot:'varlist_get_length:int |l:varlist|';
     fr_addr:@pp_varlist_get_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_set_length:void |l:varlist, count:int|';
     fr_addr:@pp_varlist_set_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_exchange:void |l:varlist, X1:int, X2:int|';
     fr_addr:@pp_varlist_exchange;
     fr_desc:'exchange by index';
    ),
    (fr_prot:'varlist_move:void |l:varlist, curIndex:int, newIndex:int|';
     fr_addr:@pp_varlist_move;
     fr_desc:'move variant to new position';
    ),
    (fr_prot:'varlist_add:int |l:varlist, value|';
     fr_addr:@pp_varlist_add;
     fr_desc:'add variant';
    ),
    (fr_prot:'varlist_addFrom:int |l:varlist, variants:varlist|';
     fr_addr:@pp_varlist_addFrom;
     fr_desc:'add variants from';
    ),
    (fr_prot:'varlist_insert:void |l:varlist, index:int, value|';
     fr_addr:@pp_varlist_insert;
     fr_desc:'insert variant at specified position';
    ),
    (fr_prot:'varlist_delete:void |l:varlist, index:int|';
     fr_addr:@pp_varlist_delete;
     fr_desc:'delete variant by index';
    ),
    (fr_prot:'varlist_clear:void |l:varlist|';
     fr_addr:@pp_varlist_clear;
     fr_desc:'clear variant list';
    ),
    (fr_prot:'varlist_copy:varlist |l:varlist, index:int, count:int|';
     fr_addr:@pp_varlist_copy;
     fr_desc:'copy to another variant list';
    ),
    (fr_prot:'varlist_left:varlist |l:varlist, count:int|';
     fr_addr:@pp_varlist_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'varlist_right:varlist |l:varlist, count:int|';
     fr_addr:@pp_varlist_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'varlist_get_first |l:varlist|';
     fr_addr:@pp_varlist_first;
     fr_desc:'get first variant item';
    ),
    (fr_prot:'varlist_get_last |l:varlist|';
     fr_addr:@pp_varlist_last;
     fr_desc:'get last variant item';
    ),
    (fr_prot:'varlist_shift |l:varlist|';
     fr_addr:@pp_varlist_shift;
     fr_desc:'get and delete first item';
    ),
    (fr_prot:'varlist_pop |l:varlist|';
     fr_addr:@pp_varlist_pop;
     fr_desc:'get and delete last item';
    )
  );

  sys_module_funcs: RLseFuncListRec = (
    fl_count: sys_func_count;
    fl_entry:@sys_func_array;
  );
  
// <QUERY> /////////////////////////////////////////////////////////////////////

function qe_param_engine(const Param: PLseParam): PLseEngine;cdecl;
begin
  try
    if Param <> nil then
      Result := get_engine(Param).EngineRec else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure qe_engine_destroy(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Free;
  except
    { do nothing }
  end;
end;

function qe_engine_compile(const Engine: pointer; const code: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileCode(code));
  except
    Result := 0;
  end;
end;

function qe_engine_compile_file(const Engine: pointer; const fname: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileFile(fname));
  except
    Result := 0;
  end;
end;

function qe_engine_execute(const Engine: pointer; const code: pchar): integer;cdecl;
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

function qe_engine_execute_file(const Engine: pointer; const fname: pchar): integer;cdecl;
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

procedure qe_engine_terminate(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Terminate;
  except
    { do nothing }
  end;
end;

procedure qe_engine_clear(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Clear;
  except
    { do nothing }
  end;
end;

function qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_strec_alloc(KLiEngine(Engine).Arguments.Text);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Arguments.Text := Args;
  except
    { do nothing }
  end;
end;

function qe_engine_errno(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.errno;
  except
    Result := -1;
  end;
end;

function qe_engine_error_row(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Row;
  except
    Result := -1;
  end;
end;

function qe_engine_error_col(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Col;
  except
    Result := -1;
  end;
end;

function qe_engine_error_name(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Name);
  except
    Result := nil;
  end;
end;

function qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Msg);
  except
    Result := nil;
  end;
end;

function qe_engine_error_module(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Module);
  except
    Result := nil;
  end;
end;

function qe_engine_error_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.ModuleFile);
  except
    Result := nil;
  end;
end;

function qe_engine_result_type(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultType);
  except
    Result := nil;
  end;
end;

function qe_engine_result_text(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultText);
  except
    Result := nil;
  end;
end;

function qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainSearchPath);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainSearchPath := Trim(Path);
  except
    { do nothing }
  end;
end;

function qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainFile);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainFile := fname;
  except
    { do nothing }
  end;
end;

function qe_engine_ready(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Ready);
  except
    Result := 0;
  end;
end;

function qe_engine_running(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Running);
  except
    Result := 0;
  end;
end;

function qe_engine_terminated(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Terminated);
  except
    Result := 0;
  end;
end;

function qe_engine_exited(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Exited);
  except
    Result := 0;
  end;
end;

procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
begin
  try
    lse_stream_write(KLiEngine(Engine).Output, Text, Count);
  except
    { do nothing }
  end;
end;

function qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
begin
  try
    Result := lse_stream_read(KLiEngine(Engine).Input, Buf, Count);
  except
    Result := 0;
  end;
end;

function qe_engine_readln(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_stream_readln(KLiEngine(Engine).Input);
  except
    Result := nil;
  end;
end;

function qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
begin
  try
    Result := KLiEngine.Create(EngineRec);
  except
    Result := nil;
  end;
end;

function qe_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
begin
  try
    if Param <> nil then
      Result := lse_strec_alloc(get_runner(Param).FormatFor(Fmt, nil)) else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    set_error(Param, ID, Errno, Msg);
  except
    { do nothing }
  end;
end;

function qe_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
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

function qe_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
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

function qe_register_module(const Name: pchar; const initrec: PLseModule): pointer;cdecl;
begin
  try
    Result := module_register(Name, initrec);
  except
    Result := nil;
  end;
end;

function qe_register_type(const CR: PLseType): integer;cdecl;
begin
  Result := Ord(sys_module.SetupType(CR));
end;

function qe_register_func(const FR: PLseFunc): pointer;cdecl;
begin
  try
    Result := sys_module.SetupFunc(FR);
  except
    Result := nil;
  end;
end;

function qe_simple_test(const Script: pchar): integer;cdecl;
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

function qe_startup: integer;cdecl;
begin
  try
    kernel_startup;
    Result := 1;
  except
    Result := 0;
  end;
end;

procedure qe_cleanup;cdecl;
begin
  { do nothing }
end;

function qe_keywords: pchar;cdecl;
begin
  reserved_words;
  Result := pchar(sys_reserved_words);
end;

function qe_get_kernel_file: pchar;cdecl;
begin
  Result := pchar(sys_kernel);
end;

procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
begin
  try
    sys_kernel := lse_expand_fname(KernelFile);
    kernel_load_confile('');
  except
    { do nothing }
  end;
end;

function qe_get_program_file: pchar;cdecl;
begin
  Result := pchar(sys_program);
end;

procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
begin
  sys_program := lse_expand_fname(ProgramFile);
end;

procedure qe_load_config(const ConfigFile: pchar);cdecl;
begin
  try
    kernel_load_confile(lse_expand_fname(ConfigFile));
  except
    { do nothing }
  end;
end;

function qe_production: pchar;cdecl;
begin
  Result := LSE_ID;
end;

function qe_version: pchar;cdecl;
begin
  Result := pchar(sys_version);
end;

function qe_copyright: pchar;cdecl;
begin
  Result := LSE_COPYRIGHT;
end;

function qe_tmpath: pchar;cdecl;
begin
  Result := pchar(sys_tmpath);
end;

const
  sys_cik_entries       : RLseEntry = (
    cik_create          : {$IFDEF FPC}@{$ENDIF}qe_engine_create;
    cik_destroy         : {$IFDEF FPC}@{$ENDIF}qe_engine_destroy;
    cik_compile         : {$IFDEF FPC}@{$ENDIF}qe_engine_compile;
    cik_fcompile        : {$IFDEF FPC}@{$ENDIF}qe_engine_compile_file;
    cik_execute         : {$IFDEF FPC}@{$ENDIF}qe_engine_execute;
    cik_fexecute        : {$IFDEF FPC}@{$ENDIF}qe_engine_execute_file;
    cik_terminate       : {$IFDEF FPC}@{$ENDIF}qe_engine_terminate;
    cik_clear           : {$IFDEF FPC}@{$ENDIF}qe_engine_clear;
    cik_get_args        : {$IFDEF FPC}@{$ENDIF}qe_engine_get_args;
    cik_set_args        : {$IFDEF FPC}@{$ENDIF}qe_engine_set_args;
    cik_errno           : {$IFDEF FPC}@{$ENDIF}qe_engine_errno;
    cik_error_row       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_row;
    cik_error_col       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_col;
    cik_error_name      : {$IFDEF FPC}@{$ENDIF}qe_engine_error_name;
    cik_error_msg       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_msg;
    cik_error_module    : {$IFDEF FPC}@{$ENDIF}qe_engine_error_module;
    cik_error_file      : {$IFDEF FPC}@{$ENDIF}qe_engine_error_file;
    cik_result_type     : {$IFDEF FPC}@{$ENDIF}qe_engine_result_type;
    cik_result_text     : {$IFDEF FPC}@{$ENDIF}qe_engine_result_text;
    cik_get_search_path : {$IFDEF FPC}@{$ENDIF}qe_engine_get_search_path;
    cik_set_search_path : {$IFDEF FPC}@{$ENDIF}qe_engine_set_search_path;
    cik_get_main_file   : {$IFDEF FPC}@{$ENDIF}qe_engine_get_main_file;
    cik_set_main_file   : {$IFDEF FPC}@{$ENDIF}qe_engine_set_main_file;
    cik_ready           : {$IFDEF FPC}@{$ENDIF}qe_engine_ready;
    cik_running         : {$IFDEF FPC}@{$ENDIF}qe_engine_running;
    cik_terminated      : {$IFDEF FPC}@{$ENDIF}qe_engine_terminated;
    cik_exited          : {$IFDEF FPC}@{$ENDIF}qe_engine_exited;
    cik_write           : {$IFDEF FPC}@{$ENDIF}qe_engine_write;
    cik_read            : {$IFDEF FPC}@{$ENDIF}qe_engine_read;
    cik_readln          : {$IFDEF FPC}@{$ENDIF}qe_engine_readln;
    cik_register_module : {$IFDEF FPC}@{$ENDIF}qe_register_module;
    cik_register_type   : {$IFDEF FPC}@{$ENDIF}qe_register_type;
    cik_register_func   : {$IFDEF FPC}@{$ENDIF}qe_register_func;
    cik_param_engine    : {$IFDEF FPC}@{$ENDIF}qe_param_engine;
    cik_param_format    : {$IFDEF FPC}@{$ENDIF}qe_param_format;
    cik_param_error     : {$IFDEF FPC}@{$ENDIF}qe_param_error;
    cik_param_push      : {$IFDEF FPC}@{$ENDIF}qe_param_push;
    cik_param_goon      : {$IFDEF FPC}@{$ENDIF}qe_param_goon;
    cik_production      : {$IFDEF FPC}@{$ENDIF}qe_production;
    cik_version         : {$IFDEF FPC}@{$ENDIF}qe_version;
    cik_copyright       : {$IFDEF FPC}@{$ENDIF}qe_copyright;
    cik_tmpath          : {$IFDEF FPC}@{$ENDIF}qe_tmpath;
    cik_query           : {$IFDEF FPC}@{$ENDIF}kernel_query;
    cik_kernel_type     : {$IFDEF FPC}@{$ENDIF}kernel_type;
    cik_simple_test     : {$IFDEF FPC}@{$ENDIF}qe_simple_test;
    cik_startup         : {$IFDEF FPC}@{$ENDIF}qe_startup;
    cik_cleanup         : {$IFDEF FPC}@{$ENDIF}qe_cleanup;
    cik_keywords        : {$IFDEF FPC}@{$ENDIF}qe_keywords;
    cik_get_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_get_kernel_file;
    cik_set_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_set_kernel_file;
    cik_get_program_file: {$IFDEF FPC}@{$ENDIF}qe_get_program_file;
    cik_set_program_file: {$IFDEF FPC}@{$ENDIF}qe_set_program_file;
    cik_load_config     : {$IFDEF FPC}@{$ENDIF}qe_load_config
  );

////////////////////////////////////////////////////////////////////////////////

procedure kernel_lock;
begin
  sys_spinlock.Acquire;
end;

procedure kernel_unlock;
begin
  sys_spinlock.Release;
end;

function kernel_query(const ID: pchar): pointer;cdecl;
var
  name: string;
begin
  name := LowerCase(ID);
  if name = 'cik_entries' then
    Result := @sys_cik_entries else
    Result := nil;
end;

function kernel_type(Index: TLseKernelType): PLseType;cdecl;
begin
  Result := @sys_kernel_types[Index];
end;

procedure engine_lock(engine: KLiEngine);
begin

end;

procedure engine_unlock(engine: KLiEngine);
begin

end;

procedure kernel_load_confile(const ConfigFile: string);

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
      sys_program := lse_exe_name;
      sys_kernel := lse_lib_name;
      {$IFDEF WINDOWS}
      sys_home_path := lse_full_path(lse_getenv('HOMEDRIVER') + lse_getenv('HOMEPATH'));
      {$ELSE}
      sys_home_path := lse_full_path(lse_getenv('HOME'));
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

    sys_tmpath := lse_full_path(ReadCex('lse_tmpath', LSE_TEMP_PATH));
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

function kernel_config(const ID: string): string;
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
            Result := Result + kernel_config(name);
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
  Result := lse_is_ident(pchar(ID));
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

function genid: string;
var
  guid: TGuid;
  index: integer;
begin
  CreateGuid(guid);
  Result := UpperCase(GuidToString(guid));
  for index := Length(Result) downto 1 do
    if not (Result[index] in LCS_HEX) then
      System.Delete(Result, index, 1);
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

function new_string(Source: pchar; Count: integer): string;
begin
  if (Source <> nil) and (Count > 0) then
    SetString(Result, Source, Count) else
    Result := '';
end;

function get_runner(Param: PLseParam): KLiRunner;
begin
  Result := KLiRunner(Param^.p_runner);
end;

function get_engine(Param: PLseParam): KLiEngine;
begin
  Result := get_runner(Param).FEngine;
end;

function value_func(V: PLseValue): KLiFunc;
begin
  Result := KLiFunc(lse_get_obj(V, KT_FUNC));
end;

function value_varlist(V: PLseValue): KLiVarList;
begin
  Result := KLiVarList(lse_get_obj(V, KT_VARLIST));
end;

function get_this(Param: PLseParam; var This): boolean;
var
  this_obj: pointer;
begin
  this_obj := lse_get_obj(Param^.p_param[0]);
  Result := (this_obj <> nil);
  if Result then
    pointer(This) := this_obj else
    set_error_this(Param);
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
    if not lse_is_ident(pchar(errid)) then
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

function value_in(V: PLseValue; Host: KLiVarList; FindItemVG: boolean): boolean;
var
  clss, tmpc: PLseType;
  next, size: integer;
  data: PLseValue;
begin
  Result := false;
  clss := lse_type(V);
  size := Host.Count;
  next := 0;
  while not Result and (next < size) do
  begin
    data := Host[next];
    tmpc := lse_type(data);
    if clss = tmpc then
      case clss^.cr_type of
        LSV_VOID   : Result := true;
        LSV_STRING : Result := lse_strec_same(V^.VObject, data^.VObject);
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
  H: pchar;
begin
  Result := false;
  if Host <> nil then
  begin
    H := lse_strec_data(Host);
    case lse_type(V)^.cr_type of
      LSV_STRING: Result := (nil <> StrPos(H, lse_strec_data(V^.VObject)));
      LSV_INT   : Result := (V^.VInteger in [0..255]) and
                            (nil <> StrScan(H, char(V^.VInteger)));
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
            Host.IsSet(lse_get_str(V));
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
label UNLOCK;
var
  H: THandle;
  M: RLseModule;
  X: TLseInitExchange;
begin
  Result := nil;
  if not lse_is_ident(pchar(name)) then Exit;
  kernel_lock;
  try
    Result := sys_libraries.Find(name);
    if Result <> nil then
      goto UNLOCK;

    H := 0;
    if not lse_load_library(FileName, H) then
      goto UNLOCK;

    X := TLseInitExchange(lse_get_proc(H, 'InitExchange'));
    if not Assigned(X) then
    begin
      lse_free_library(H);
      goto UNLOCK;
    end;

    lse_mem_zero(@M, sizeof(RLseModule));
    X(@M, @sys_cik_entries);

    Result := KLiModule.Create(name, nil, moyLibrary);
    try
      Result.FFileName := FileName;
      Result.FVersion := M.iw_version;
      Result.FDescription := M.iw_desc;
      Result.FInvokeProc := M.iw_invoke;
      Result.FHandle := H;
      Result.SetupModuleTypes(Addr(M.iw_types));
      Result.SetupModuleFuncs(Addr(M.iw_funcs));
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

    if not lse_is_ident(pchar(ID)) or is_reserved(ID, true) or
      sys_libraries.Has(ID) or (MR = nil) then
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
      path := lse_full_path(path);

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

function type_module(T: PLseType): KLiModule;
begin
  Result := KLiModule(T^.cr_module);
end;

function type_full_name(T: PLseType): string;
begin
  Result := type_module(T).Name + '::' + T^.cr_name;
end;

function token_new: PLiToken;
begin
  Result := token_clone(nil);
end;

procedure token_free(Token: PLiToken);
begin
  if Token <> nil then
  begin
    Token^.tk_name := '';
    lse_mem_free(Token, sizeof(RLiToken));
  end;
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
  Token^.tk_name := '';
  lse_mem_zero(Token, sizeof(RLiToken));
  Token^.tk_next := next;
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
    list := TStringList.Create;
    try
      list.CommaText := 'void,int,float,string,object,variant,sys,main';
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
end;

destructor KLiTokenizer.Destroy;
var
  X: integer;
  T: PLiToken;
begin
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
type
  KLiExtInt = (eiNone, eiInt, eiExt);
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
    fs: string;
  begin
    ev := 0;
    iv := 0;
    ei := eiInt;
    next := FBase + FPosition - 1;
    base := next;
    if (next^ = '0') and ((next + 1)^ = 'x') then
    begin
      Inc(next, 2);
      if next^ in LCS_HEX then
      begin
        repeat
          iv := (iv * 16) + hex_value(next^);
          Inc(next);
        until not (next^ in LCS_HEX);
      end
      else ei := eiNone;
    end
    else
    begin
      while next^ in LCS_DIGIT do
      begin
        iv := (iv * 10) + Ord(next^) - Ord('0');
        Inc(next);
      end;
      if (next^ = '.') and ((next + 1)^ in LCS_DIGIT) then
      begin
        ei := eiExt;
        ev := iv;
        fs := '0.';
        Inc(next);
        while next^ in LCS_DIGIT do
        begin
          fs := fs + next^;
          Inc(next);
        end;
        ev := ev + StrToFloat(fs);
      end;
    end;
    trace_to_next;
    if ei = eiInt then
    begin
      token^.VInteger := iv;
      token^.tk_sym := syInt;
    end
    else
    if ei = eiExt then
    begin
      token^.VFloat := ev;
      token^.tk_sym := syFloat;
    end
    else token^.tk_name := new_string(base, next - base);
  end;

  procedure get_string;
  var
    quotc, ch: char;
  begin
    quotc := FChar;
    while GetChar and ((FChar <> quotc) or (PeekChar = quotc)) do
    begin
      ch := FChar;
      if ch = quotc then GetChar else
      if (ch = '\') and (quotc = '"') then
      begin
        if not GetChar then Exit;
        case FChar of
          '0': ch := #0;   {<--NULL}
          'a': ch := #7;   {<--BELL}
          'b': ch := #8;   {<--BACKSPACE}
          'e': ch := #27;  {<--ESCAPE}
          'f': ch := #12;  {<--FORMFEED}
          'n': ch := #10;  {<--NEWLINE}
          'r': ch := #13;  {<--CARRIGE RETURN}
          't': ch := #9;   {<--TAB}
          'v': ch := #11;  {<--VERTICAL TAB}
          'x': begin       {<--HEX}
                 if not GetChar or not (FChar in LCS_HEX) then Exit;
                 ch := FChar;
                 if not GetChar or not (FChar in LCS_HEX) then Exit;
                 ch := char(StrToInt('$' + ch + FChar));
               end;
          else ch := FChar;
        end;
      end;
      token^.tk_name := token^.tk_name + ch;
    end;
    if FChar = quotc then
    begin
      token^.tk_sym := syStr;
      GetChar;
    end;
  end;

  procedure get_pure_identity(pure_id: boolean);
  var
    idstr: string;
  begin
    if SkipSpaces and lse_is_idhead(FChar) then
    begin
      idstr := FChar;
      while GetChar and (FChar in LCS_ID) do
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
      if FChar in LCS_HEAD then
      begin
        vs := FChar;
        while GetChar and (FChar in LCS_ID) do
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
  if Result then
  begin
    case FChar of
    '0'..'9': get_number;
    '"','''': get_string;
    '+'     : get_operator(syAdd, [], []);
    '-'     : get_operator(syDec, [], []);
    '*'     : get_operator(syMul, [], []);
    '/'     : get_operator(syDiv, [], []);
    '%'     : get_operator(syMod, [], []);
    '^'     : get_operator(syBitXor, [], []);
    '~'     : get_operator(syBitNot, [], []);
    '('     : get_operator(syLParen, [], []);
    ')'     : get_operator(syRParen, [], []);
    '{'     : get_operator(syLBlock, [], []);
    '}'     : get_operator(syRBlock, [], []);
    '['     : get_operator(syLArray, [], []);
    ']'     : get_operator(syRArray, [], []);
    '.'     : get_operator(syDot, ['.'], [syUpto]);
    '?'     : get_operator(syAsk, [], []);
    ':'     : get_operator(syDot2, [], []);
    ','     : get_operator(syComma,  [], []);
    ';'     : get_operator(sySemicolon,  [], []);
    '='     : get_operator(syBecome, ['='], [syEQ]);
    '!'     : get_operator(syError, ['=', '<', '>'], [syNE, syME, syLE]);
    '<'     : begin
                get_operator(syLess, ['=', '<'], [syLE, syBitShl]);
                if (token^.tk_sym = syBitShl) and (FChar = '<') then
                begin
                  token^.tk_sym := syFill;
                  token^.tk_name := '<<<';
                  GetChar;
                end;
              end;
    '>'     : get_operator(syMore, ['=', '>'], [syME, syBitShr]);
    '&'     : get_operator(syBitAnd, [], []);
    '|'     : get_operator(syBitOr, [], []);
    '$'     : get_value;
    '@'     : get_operator(syFormat, [], []);
    else
      if lse_is_idhead(FChar) then
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
  IsStr := (token^.tk_sym = syStr);
end;

function KLiTokenizer.GotoChar(Chars: TLseCharSet): boolean;
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
  while not Result and GotoChar([#1..#255] - LCS_SPACE) do
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

{ KLiParser }

constructor KLiParser.Create(AModule: KLiModule);
begin
  FModule := AModule;
  FRunner := FModule.FEngine.FMainRunner;
  FSymbols := KLiTokens.Create(nil);
end;

function KLiParser.CurCodes: KLiTokens;
begin
  Result := FCurrent.FCodes;
end;

destructor KLiParser.Destroy;
begin
  if not FIsShadow then
    FreeAndNil(FTokenizer);
  FreeAndNil(FSymbols);
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
    FCurrent.FCodes.Clear;
  end
  else
  begin
    FCurrent := FModule.NewFunc('');
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
  Result := GetSym(FSymbols.Add);
end;

procedure KLiParser.ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
var
  L: string;
begin
  if not OnHead then SymGotoNext;
  L := FModule.NewLabelName;
  CurCodes.AddRinr(L, @FLast^.tk_pos);
  while not (FLast^.tk_sym in EndSyms) do
  begin
    ParseStatement(false);
    SymGotoNext;
  end;
  CurCodes.AddLabel(L, @FLast^.tk_pos);
end;

procedure KLiParser.ParseBody(EndSym: KLiSymbol; OnHead: boolean);
var
  list: KLiTokens;
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
      cx := CurCodes.AddGoto(cl, @FLast^.tk_pos);
      cx^.tk_sym := syGoFP;
      ParseBlock([syRBlock, syElse], false);
      if FLast^.tk_sym <> syRBlock then
      begin
        CurCodes.AddGoto(bl, @FLast^.tk_pos);
        CurCodes.AddLabel(cl, @FLast^.tk_pos);
        if FLast^.tk_sym = syElse then
        begin
          SymGotoNext;
          if FLast^.tk_sym <> syIf then
            ParseBlock([syRBlock], true);
        end;
      end
      else cx^.tk_name := bl;
    until FLast^.tk_sym = syRBlock;
    CurCodes.AddLabel(bl, @FLast^.tk_pos);
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

      CurCodes.AddRinr(FBreakLabel, @FLast^.tk_pos);

      ParseExpr([syIf, syDo], false);
      CurCodes.AddNew(syVarGen, @(FLast^.tk_pos));
      CurCodes.AddNew(sySETV, @(FLast^.tk_pos));

      CurCodes.AddLabel(FContinueLabel, @FLast^.tk_pos);

      expr := AddToken(vrec);
      expr^.tk_sym := sySend;
      expr := CurCodes.AddGoto(FBreakLabel, @FLast^.tk_pos);
      expr^.tk_sym := syGoFP;

      if FLast^.tk_sym = syIf then
      begin
        ParseExpr([syDo], false);
        expr := CurCodes.AddGoto(FContinueLabel, @FLast^.tk_pos);
        expr^.tk_sym := syGoFP;
      end;

      ParseBlock([syRBlock], false);
      CurCodes.AddGoto(FContinueLabel, @FLast^.tk_pos);
      CurCodes.AddLabel(FBreakLabel, @FLast^.tk_pos);
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
      CurCodes.AddLabel(FContinueLabel, @FLast^.tk_pos);
      ParseExpr([syDo], false);
      CurCodes.AddGoto(FBreakLabel, @FLast^.tk_pos)^.tk_sym := syGoFP;
      ParseBlock([syRBlock], false);
      CurCodes.AddGoto(FContinueLabel, @FLast^.tk_pos);
      CurCodes.AddLabel(FBreakLabel, @FLast^.tk_pos);
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
      CurCodes.AddLabel(body, @FLast^.tk_pos);
      ParseBlock([syUntil], false);
      CurCodes.AddLabel(FContinueLabel, @FLast^.tk_pos);
      ParseExpr([syRBlock], false);
      CurCodes.AddGoto(body, @FLast^.tk_pos)^.tk_sym := syGoFP;
      CurCodes.AddLabel(FBreakLabel, @FLast^.tk_pos);
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
    CurCodes.AddRinr(bl, @FLast^.tk_pos);
    ParseExpr([syCase], false);
    CurCodes.AddNew(sySETV, @FLast^.tk_pos);
    repeat
      ParseExpr([syDot2], false);
      CurCodes.AddNew(syCase, @(FLast^.tk_pos));
      cl := FModule.NewLabelName;
      cx := CurCodes.AddGoto(cl, @FLast^.tk_pos);
      cx^.tk_sym := syGoFP;
      ParseBlock([syCase, syElse, syRBlock], false);
      if FLast^.tk_sym in [syCase, syElse] then
      begin
        CurCodes.AddGoto(bl, @FLast^.tk_pos);
        CurCodes.AddLabel(cl, @FLast^.tk_pos);
        if FLast^.tk_sym = syElse then
          ParseBlock([syRBlock], false);
      end
      else cx^.tk_name := bl;
    until FLast^.tk_sym  in [syRBlock];
    CurCodes.AddLabel(bl, @FLast^.tk_pos);
  end;

  procedure parse_break;
  begin
    if FBreakLabel = '' then
      Error.SyntaxErr(EvBreakNoLoop, LastRow, LastCol, LastModule.Name,
        EsBreakNoLoop, LastModule.FileName, []);
    SymTestNext([syRBlock]);
    CurCodes.AddGoto(FBreakLabel, @FLast^.tk_pos);
  end;

  procedure parse_continue;
  begin
    if FContinueLabel = '' then
      Error.SyntaxErr(EvContinueNoLoop, LastRow, LastCol, LastModule.Name,
        EsContinueNoLoop, LastModule.FileName, []);
    SymTestNext([syRBlock]);
    CurCodes.AddGoto(FContinueLabel, @FLast^.tk_pos);
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

    CurCodes.AddRinr(LL, @FLast^.tk_pos);
    TX := CurCodes.AddGoto(CL, @FLast^.tk_pos);
    TX^.tk_sym := syTry;

    Inc(FTryCount);
    try
      ParseBlock([syFinally, syExcept], false);
      if FLast^.tk_sym = syFinally then
      begin
        TX^.tk_prmc := 1;
        CurCodes.AddGoto(CL, @FLast^.tk_pos);
      end
      else CurCodes.AddGoto(LL, @FLast^.tk_pos);
    finally
      Dec(FTryCount);
    end;

    Inc(FCatchCount);
    try
      CurCodes.AddLabel(CL, @FLast^.tk_pos);
      ParseBlock([syRBlock], false);
      CurCodes.AddLabel(LL, @FLast^.tk_pos);
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
    OPRS = [syMul, syDiv, syMod, syAdd, syDec, syBitXor,
            syBitAnd, syBitOr, syBitShl, syBitShr];

  function is_become(S1, S2: KLiSymbol): boolean;
  begin
    Result := (S1 = syBecome) or ((S2 = syBecome) and (S1 in OPRS));
  end;
  
  procedure parse_default;
  var
    R, M: PLiToken;
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
          AddToken(expr);
          expr^.tk_sym := syDupLast;
          ParseExpr([syRBlock], false);
          AddToken(M);
          expr := AddToken(expr);
        end
        else
        begin
          CurCodes.FItems.Delete(CurCodes.Count - 1);
          try
            ParseExpr([syRBlock], false);
          finally
            CurCodes.DoAdd(expr);
          end;
        end;
        expr^.tk_sym := sySetIV;
        expr^.tk_prmc := 3;
      end;
    end;
  end;

  procedure parse_macro;
  const
    SR = [FirstKeyword..LastOper] - [syLBlock, syRBlock];
  var
    synx: KLiMacro;
    onid: boolean;
    pair: integer;
  begin
  // {def {name ... } {...}}
    SymTestLast([syLBlock]);
    SymTestNextPureID;
    if FModule.Find(FLast^.tk_name) then
      Error.Redeclared(Self);

    synx := KLiMacro.Create(FModule, FLast^.tk_name);
    if not FModule.FParsing then
      FModule.FEngine.AddCompiled(synx);
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
    if FLast^.tk_sym = syLBlock then parse_macro else
    begin
      SymTestLastPureID;
      if FModule.Find(FLast^.tk_name) then
        Error.Redeclared(Self);
      FCurrent := FModule.NewFunc(FLast^.tk_name);
      SymGotoNext;
      if FLast^.tk_sym = syBitOr then
      begin
        ParseArguments(FCurrent, [syBitOr], false);
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

        module := FModule.FEngine.FModules.Find(m_name);
        if module <> nil then
        begin
          if module.FParsing then
            Error.SyntaxErr(EvImportEachOther, LastRow, LastCol,
              LastModule.Name, EsImportEachOther, LastModule.FileName,
              [FModule.Name, module.Name]);
          FModule.Import(module);
          goto NEXT;
        end;

        module := sys_libraries.Find(m_name);
        if module <> nil then
        begin
          FModule.Import(module);
          goto NEXT;
        end;

        is_lib := false;
        f_name := m_name;
        if not module_search(f_name, FModule.FEngine.GetSearchPath, is_lib) then
          Error.ImportErr(EvModuleNotFound, LastRow, LastCol, LastModule.Name,
            EsModuleNotFound, LastModule.FileName, [LastVal]);

        module := FModule.FEngine.FindModuleByFileName(f_name);
        if module <> nil then // loaded
        begin
          module.FAliasName := module.Name;
          module.Name := m_name;
        end
        else
        if is_lib then
        begin
          module := module_load(m_name, f_name);
          if module = nil then
            Error.ImportErr(EvWrongLibrary, LastRow, LastCol, LastModule.Name,
              EsWrongLibrary, LastModule.FileName, [LastVal]);
        end
        else
        begin
          module := KLiModule.Create(m_name, FModule.FEngine, moyScript);
          module.FFileName := f_name;
          module.FParsing := true;
          FModule.FEngine.AddCompiled(module);
          KLiParser.Create(module).ParseAndFree(file_text(f_name));
          module.FParsing := false;
        end;
        FModule.Import(Module);

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
    FCurrent := FModule.NewFunc(data^.tk_name);
    FCurrent.IsConstFunc := true;
    FCurrent.IsNameCall := true;
    ParseExpr([syRBlock], false);
    FCurrent.FCodes.AddNew(syReturn, @FLast^.tk_pos);
  end;

label MACRO_BACK;
var
  curr: KLiFunc;
  macro: KLiMacro;
begin
  curr := FCurrent;
  try
    if FLast^.tk_sym = syEOF then
      Error.SymNotFound(Self);

    while not OnHead and ((FLast^.tk_sym <> syLBlock) or (PeekNextSym = syBitOr)) do
    begin
      ParseExpr([], true);
      if FLast^.tk_sym in [syRBlock, syElse, syCase, syEOF] then
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
      syDef     : parse_define;
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
          macro := FindMacro(FLast^.tk_name);
          if macro <> nil then
          begin
            ExpandMacro(macro);
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
  if (Syms <> []) and not (FLast^.tk_sym in Syms) then
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
    CurCodes.AddRinr(L, @FLast^.tk_pos);
    Inc(FInTermCount);
    ParseStatement(true);
    Dec(FInTermCount);
    CurCodes.AddLabel(L, @FLast^.tk_pos);
  end;
  
  procedure parse_term;
  var
    L: PLiToken;
    hashed: boolean;
    macro: KLiMacro;
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
        macro := FindMacro(FLast^.tk_name);
        if macro <> nil then
        begin
          ExpandMacro(macro);
          SymGotoNext;
        end
        else Break;
      end;
      if FLast^.tk_sym = syBitOr then // lambda
      begin
        with Shadow do
        try
          lambda := FModule.NewFunc('');
          lambda.IsLambdaFunc := true;
          FCurrent := lambda;
          ParseArguments(lambda, [syBitOr], false);
          ParseBody(syRBlock, false);
          if lambda.IsEmptyFunc then
          begin
            FModule.FEngine.FCompiledObjects.Remove(lambda);
            lambda.DecRefcount;
            lambda := sys_nothing;
          end;
        finally
          Free;
        end;
        FLast^.tk_sym := syID;
        FLast^.tk_name := lambda.FullName;
        AddToken(FLast);
      end
      else
      if FLast^.tk_sym in [syImport, syDef, syConst] then
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
    if L^.tk_sym in [syNot, syDec, syBitNot, syFormat] then
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

begin
  if not OnHead then SymGotoNext;
  parse_fact(High(ExprOperSyms));
  SymTestLast(EndSyms);
end;

function KLiParser.AddToken(token: PLiToken): PLiToken;
begin
  Result := CurCodes.AddToken(token);
end;

function KLiParser.CloneSym(sym: PLiToken): PLiToken;
begin
  Result := FSymbols.Add;
  token_copy(sym, Result);
end;

function KLiParser.Error: KLiError;
begin
  Result := FModule.FEngine.FError;
end;

procedure KLiParser.ExpandMacro(AMacro: KLiMacro);
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
    if AMacro.ArgCount > 0 then
    begin
      index := 0;
      while index < AMacro.ArgCount do
      begin
        token := AMacro.ArgToken(index);
        if token^.tk_sym <> syID then
        begin
          SymTestNext([token^.tk_sym]);
          if index = AMacro.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else
        if index < AMacro.ArgCount - 1 then
        begin
          read_to(token^.tk_name, [AMacro.ArgToken(index + 1)^.tk_sym]);
          Inc(index);
          if index = AMacro.ArgCount - 1 then
            SymTestNext([syRBlock]);
        end
        else read_to(token^.tk_name, [syRBlock]);
        Inc(index);
      end;
    end
    else SymTestNext([syRBlock]);

    // 2.put back to FTokenizer
    FTokenizer.PutBack(FLast); // syRBlock
    AMacro.RenameLocals;
    lastsym := FLast^.tk_sym;
    for index := AMacro.FBody.Count - 1 downto 0 do
    begin
      token := PLiToken(AMacro.FBody[index]);
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

function KLiParser.FindMacro(const ID: string): KLiMacro;
var
  R: RLiFind;
  S, M: string;
begin
  M := '';
  S := extract_name_module(ID, M);
  if FModule.FindBy(S, M, @R) and (R.fo_type = foMacro) then
    Result := R.VMacro else
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

{ KLiFunc }

procedure KLiFunc.AddParam(const VName: string; VType: PLseType);
var
  X: integer;
  V: PLseVarb;
begin
  V := lse_mem_alloc_zero(sizeof(RLseVarb));
  V^.va_name := VName;
  V^.va_type := VType;
  X := Length(FParams);
  SetLength(FParams, X + 1);
  FParams[X] := V;
end;

procedure KLiFunc.AddSuper;
var
  X: integer;
  S: PLseVarb;
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

constructor KLiFunc.Create(Parent: KLiModule; AResultType: PLseType;
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
      AddParam(Params[A], PLseType(Params.Objects[A]));

  FProc := Proc;
  if FProc = nil then
  begin
    FCodes := KLiTokens.Create(Self);
    FCodes.IncRefcount;
  end;
end;

function KLiFunc.Curry(List: KLiVarList; Module: KLiModule): KLiFunc;
var
  func: KLiFunc;
  this, curr: KLiFunc_curry;
  base, index: integer;
  varb: PLseVarb;
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
  varb: PLseVarb;
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
  if FModule.FMainFunc = Self then
    FModule.FMainFunc := nil;
    
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
    lse_mem_free(FParams[X], sizeof(RLseVarb));
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

function KLiFunc.GetParam(Index: integer): PLseVarb;
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
  Result := lse_type_prot(FResultType, Name);
  if IsConstFunc then
    Result := '{const ' + Result else
    Result := '{def ' + Result;
  if not IsNameCall then
  begin
    Result := Result + ' |';
    if Self.ParamCount > 0 then
    begin
      Result := Result + lse_type_prot(GetParam(0));
      for X := 1 to Self.ParamCount - 1 do
        Result := Result + ', ' + lse_type_prot(GetParam(X));
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

function KLiFunc.FindCreate(AType: PLseType): KLiFunc;
begin
  Result := FindMethod('create', AType);
  if (Result = nil) or not lse_type_match(Result.FResultType, AType) then
  begin
    Result := type_module(AType).FindFunc(AType^.cr_name + '_create');
    if (Result <> nil) and not lse_type_match(Result.FResultType, AType) then
      Result := nil;
  end;
end;

function KLiFunc.FindMethod(const AName: string; AType: PLseType): KLiFunc;
var
  N: string;
  M, T: KLiModule;
  F: KLiFunc;
  X: integer;
  
  function match_class(func: KLiFunc): boolean;
  begin
    Result := (func <> nil) and
              (func.ParamCount > 0) and
              lse_type_match(func.GetParam(0)^.va_type, AType);
  end;
  
begin
  N := AType^.cr_name + '_' + AName;
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

function KLiFunc.FindParam(const VName: string): PLseVarb;
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

function KLiFunc.ListMethod(AType: PLseType; OnlyName: boolean): KLiVarList;
var
  N: string;
  M: KLiModule;
  X: integer;
  L: KLiVarList;
  
  procedure enum_func(func: KLiFunc);
  begin
    while func <> nil do
    begin
      if N = Copy(func.Name, 1, Length(N)) then
        if func.ParamCount > 0 then
          if lse_type_match(func.GetParam(0)^.va_type, AType) then
            if OnlyName then
              L.Add(func.Name) else
              L.Add(func, KT_FUNC);
      func := func.FNext;
    end;
  end;
  
begin
  L := KLiVarList.Create(FModule.FEngine);
  N := AType^.cr_name + '_';
  M := FModule;
  enum_func(M.FFirstFunc);
  if M.FModules <> nil then
  begin
    for X := 0 to M.FModules.Count - 1 do
      if M <> M.FModules[X] then
        enum_func(M.FModules[X].FFirstFunc);
    if M.FModules.Find('sys') = nil then
        enum_func(sys_module.FFirstFunc);
  end;
  Result := L;
end;

procedure KLiFunc.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KT_FUNC, Self);
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

{ KLiTokens }

procedure KLiTokens.DoAdd(token: PLiToken);
begin
  FItems.Add(token);
end;

function KLiTokens.Add: PLiToken;
begin
  Result := token_new;
  DoAdd(Result);
end;

function KLiTokens.AddGoto(const Name: string; const Pos: PLiPos): PLiToken;
begin
  Result := AddNew(syGoto, Pos);
  Result^.tk_name := Name;
end;

function KLiTokens.AddLabel(const Name: string; const Pos: PLiPos): PLiToken;
begin
  Result := AddNew(syLabel, Pos);
  Result^.tk_prmc := -1;
  Result^.tk_name := Name;
end;

procedure KLiTokens.Clear;
var
  X: integer;
  T: PLiToken;
begin
  for X := GetCount - 1 downto 0 do
  begin
    T := PLiToken(FItems[X]);
    FItems.Delete(X);
    token_free(T);
  end;
end;

constructor KLiTokens.Create(AFunc: KLiFunc);
begin
  FFunc := AFunc;
  FItems := TList.Create;
end;

destructor KLiTokens.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure KLiTokens.DumpCode(List: TStrings; const Margin: string);
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
        syIdle      : H := 'IDLE';
        syID        : H := Format('PUSH %s', [R^.tk_name]);
        syBecome    : H := Format('SAVE %s', [R^.tk_name]);
        syFloat     : H := Format('PSHF %f', [R^.VFloat]);
        syInt       : H := Format('PSHI %d', [R^.VInteger]);
        syStr       : H := Format('PSHS %s', [R^.tk_name]);
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
        syBitNot    : H := 'CALC ~';
        syBitXor    : H := 'CALC ^';
        syBitOr     : H := 'CALC |';
        syBitAnd    : H := 'CALC &';
        syBitShl    : H := 'CALC <<';
        syBitShr    : H := 'CALC >>';
        syEQ        : H := 'CALC ==';
        syIn        : H := 'CALC IN';
        syNE        : H := 'CALC !=';
        syLess      : H := 'CALC <';
        syLE        : H := 'CALC <=';
        syMore      : H := 'CALC >';
        syUpto      : H := 'CALC ..';
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

function KLiTokens.FindLabel(const Name: string): PLiToken;
var
  X: integer;
begin
  for X := GetCount - 1 downto 0 do
  begin
    Result := GetItem(X);
    if Result^.tk_sym = syLabel then
      if Result^.tk_name = Name then
        Exit;
  end;
  Result := nil;
end;

function KLiTokens.GetCount: integer;
begin
  Result := FItems.Count;
end;

function KLiTokens.GetItem(Index: integer): PLiToken;
begin
  Result := PLiToken(FItems[Index]);
end;

function KLiTokens.GetLast: PLiToken;
begin
  Result := PLiToken(FItems.Last);
end;

function KLiTokens.AddToken(token: PLiToken): PLiToken;
begin
  Result := Add;
  token_copy(token, Result);
end;

function KLiTokens.AddNew(sym: KLiSymbol; const Pos: PLiPos): PLiToken;
begin
  Result := Add;
  Result^.tk_sym := sym;
  if Pos <> nil then
    Result^.tk_pos := Pos^;
end;

function KLiTokens.AddRinr(const Name: string; const Pos: PLiPos): PLiToken;
begin
  Result := AddGoto(Name, Pos);
  Result^.tk_sym := syRINR;
end;

{ KLiFunc_curry }

function KLiFunc_curry.AddCurry(value: PLseValue): integer;
begin
  Result := Length(FCurry);
  SetLength(FCurry, Result + 1);
  FCurry[Result] := lse_new_value;
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
    lse_free_value(FCurry[index]);
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

{ KLiMacro }

procedure KLiMacro.AddArgument(Arg: PLiToken);
var
  X: integer;
begin
  X := Length(FArgs);
  SetLength(FArgs, X + 1);
  FArgs[X] := token_clone(Arg);
end;

procedure KLiMacro.AddToken(Token: PLiToken);
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

function KLiMacro.ArgCount: integer;
begin
  Result := Length(FArgs);
end;

function KLiMacro.ArgToken(Index: integer): PLiToken;
begin
  Result := FArgs[Index];
end;

constructor KLiMacro.Create(AModule: KLiModule; const AName: string);
begin
  inherited Create(AName);
  FRenameCount := 0;
  FModule := AModule;
  if FModule.FFirstMacro = nil then
  begin
    FModule.FFirstMacro := Self;
    FModule.FLastMacro := Self;
  end
  else
  begin
    FPrev := FModule.FLastMacro;
    FPrev.FNext := Self;
    FModule.FLastMacro := Self;
  end;
  FBody := TList.Create;
end;

destructor KLiMacro.Destroy;
var
  X: integer;
begin
  if FPrev = nil then
    FModule.FFirstMacro := FNext else
    FPrev.FNext := FNext;
  if FNext = nil then
    FModule.FLastMacro := FPrev else
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

procedure KLiMacro.RenameLocals;
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
  Result := lse_new_value;
  FItems.Add(Result);
end;

function KLiVarList.AddDefault(Klass: PLseType): PLseValue;
begin
  Result := lse_new_value;
  Result^.vtype := Klass;
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: double): PLseValue;
begin
  Result := lse_new_value(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(Value: int64): PLseValue;
begin
  Result := lse_new_value(Value);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: pointer; Klass: PLseType): PLseValue;
begin
  Result := lse_new_value(Value, Klass);
  FItems.Add(Result);
end;

function KLiVarList.Add(const Value: PLseString): PLseValue;
begin
  Result := lse_new_value(Value);
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
  Result := lse_new_value(Value);
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
    lse_stream_write(S, lse_get_str(V));
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
  lse_free_value(Pop(Index));
end;

procedure KLiVarList.DeleteLast;
begin
  lse_free_value(Pop());
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
  Result := lse_new_value;
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
  Result := lse_new_value(Value);
  FItems.Add(Result);
end;

procedure KLiVarList.SaveTo(V: PLseValue);
begin
  lse_set_object(V, KT_VARLIST, Self);
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
  FAliasName := '';
  FFileName := MName;
  FModuleType := MType;
  if FModuleType = moyKernel then
  begin
    FFileName    := sys_kernel;
    FVersion     := sys_version;
    FDescription := Format('builtin %s module', [Name]);
  end;
  FTypeList := TList.Create;
  if MName = 'sys' then
    FFuncList := TLseHashNamed.Create(16) else
    FFuncList := TLseHashNamed.Create(4);
  if FModuleType = moyScript then
  begin
    FEngine := MEngine;
    FEngine.FModules.Add(Self);
    FModules := KLiModuleList.Create(FEngine);
    FModules.FImporter := Self;
    FImporters := TList.Create;
  end
  else sys_libraries.Add(Self);
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
      P.FModules.FModules.Remove(Self);
    end;
    FEngine.FModules.FModules.Remove(Self);
    if Self = FEngine.FMainModule then
      FEngine.FMainModule := nil;
    FreeAndNil(FModules);
    FreeAndNil(FImporters);
  end;
  DeleteFunctions;
  FreeAndNil(FFuncList);
  FreeAndNil(FTypeList);
  sys_libraries.FModules.Remove(Self);
  while FFirstMacro <> nil do
    FFirstMacro.Free;
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
  lse_set_object(V, KT_MODULE, Self);
end;

function KLiModule.GetType(Index: integer): PLseType;
begin
  Result := PLseType(FTypeList[Index]);
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
        Result := sys_libraries.Find(ID);
    end;
  end;
end;

function KLiModule.FindMacro(const ID: string): KLiMacro;
begin
  Result := FFirstMacro;
  while Result <> nil do
  begin
    if Result.Name = ID then Exit;
    Result := Result.FNext;
  end;
  Result := nil;
end;

function KLiModule.Import(AModule: KLiModule): KLiModule;
begin
  if (AModule <> nil) and (AModule <> Self) then
  begin
    if (FModules <> nil) and not FModules.Has(AModule) then
      FModules.Add(AModule);
    if AModule.FImporters <> nil then
      if AModule.FImporters.IndexOf(Self) < 0 then
        AModule.FImporters.Add(Self);
  end;
  Result := AModule;
end;

function KLiModule.IsMainModule: boolean;
begin
  Result := (FEngine <> nil) and (FEngine.FMainModule = Self);
end;

function KLiModule.NewFunc(const AName: string): KLiFunc;
begin
  Result := KLiFunc.Create(Self, KT_VARIANT, AName, nil, nil);
  FEngine.AddCompiled(Result);
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

function KLiModule.FindType(const ID: string): PLseType;
var
  X: integer;
begin
  for X := 0 to FTypeList.Count - 1 do
  begin
    Result := PLseType(FTypeList[X]);
    if StrComp(pchar(ID), Result^.cr_name) = 0 then Exit;
  end;
  Result := nil;
end;

function KLiModule.FindTypeBy(const ID, module_name: string): PLseType;
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
  p_type, f_type: PLseType;
  endc: char;
  base, curr: pchar;

  function parse_next(var ID: string; var VT: PLseType): char;
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
        if lse_is_ident(pchar(ID)) then
        begin
          VT := FindTypeBy(Trim(Copy(S, X + 1, Length(S))), '');
          if VT <> nil then Exit;
        end;
      end
      else
      begin
        ID := Trim(S);
        if lse_is_ident(pchar(ID)) then
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

function KLiModule.SetupType(const TR: PLseType): boolean;
begin
  Result := (TR <> nil) and Assigned(TR^.cr_addref) and
            Assigned(TR^.cr_release) and lse_is_ident(TR^.cr_name) and
            not Find(TR^.cr_name);
  if Result then
  begin
    TR^.cr_type := LSV_OBJECT;
    TR^.cr_module := Self;
    FTypeList.Add(TR);
  end
  else
  if TR <> nil then
  begin
    TR^.cr_type := LSV_VOID;
    TR^.cr_module := nil;
  end;
end;

function KLiModule.SetupModuleTypes(const TLR: PLseTypeListRec): integer;
var
  A: integer;
begin
  Result := 0;
  if (TLR <> nil) and (TLR^.cl_entry <> nil) and (TLR^.cl_count > 0) then
    for A := 0 to TLR^.cl_count - 1 do
      if SetupType(@TLR^.cl_entry^[A]) then
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
    rec^.VMacro := FindMacro(ID);
    if rec^.VMacro = nil then
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
    else rec^.fo_type := foMacro;
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
  Result := IndexOf(AModule);
  if Result < 0 then
    Result := FModules.Add(AModule);
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
  FModules := TList.Create;
end;

procedure KLiModuleList.Delete(Index: integer);
var
  M: KLiModule;
begin
  M := GetModule(Index);
  FModules.Delete(Index);
  if FImporter = nil then M.Free; {<--KLiEngine.FModules/sys_libraries}
end;

destructor KLiModuleList.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited;
end;

function KLiModuleList.Find(const Name: string): KLiModule;
var
  X: integer;
begin
  X := IndexOf(Name);
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
  Result := KLiModule(FModules[Index]);
end;

function KLiModuleList.Has(const Name: string): boolean;
begin
  Result := (IndexOf(Name) >= 0);
end;

function KLiModuleList.Has(AModule: KLiModule): boolean;
begin
  Result := (IndexOf(AModule) >= 0);
end;

function KLiModuleList.IndexOf(const Name: string): integer;
var
  X: integer;
  M: KLiModule;
begin
  if Name <> '' then
    for X := 0 to FModules.Count - 1 do
    begin
      M := KLiModule(FModules[X]);
      if (M.Name = Name) or (M.FAliasName = Name) then
      begin
        Result := X;
        Exit;
      end;
    end;
  Result := -1;
end;

function KLiModuleList.IndexOf(AModule: KLiModule): integer;
begin
  Result := FModules.IndexOf(AModule);
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
    Result.Add(GetModule(index), KT_MODULE);
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

  FExitResult := lse_new_value;
  
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

  FreeAndNil(FMainValues);
  FreeAndNil(FTempValues);
  FreeAndNil(FMainSnap);

  SetInputStream(nil);
  SetOutputStream(nil);

  lse_free_value(FExitResult);
  FExitResult := nil;

  FreeAndNil(FMainModule);
  FreeAndNil(FModules);
  FreeAndNil(FError);
  FreeAndNil(FArguments);
  inherited;
end;

function KLiEngine.DoCompile(const Code: string): KLiFunc;
var
  module: KLiModule;
begin
  lse_check(FCompiledObjects = nil, 'invalid embeded compiling');
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
    RollbackCompiled;
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
  SetResultTypeText(type_full_name(GetResultType), GetResultText);
  FEngineRec^.er_executed(FEngineRec);
end;

function KLiEngine.FindModuleByFileName(const FileName: string): KLiModule;
var
  X: integer;
begin
  for X := 0 to FModules.Count - 1 do
  begin
    Result := FModules[X];
    if same_fname(FileName, Result.FFileName) then Exit;
  end;

  for X := 0 to sys_libraries.Count - 1 do
  begin
    Result := sys_libraries[X];
    if same_fname(FileName, Result.FFileName) then Exit;
  end;
  
  Result := nil;
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
  Result := lse_get_str(FExitResult);
end;

function KLiEngine.GetResultType: PLseType;
begin
  Result := lse_type(FExitResult);
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
      lse_set_string(Value, kernel_config(Name));
  end;
end;

procedure KLiEngine.Go;
begin
  try
    lse_check(FReady, 'The engine is not ready to run');
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
        FMainFunc.FCodes.Clear;
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
     or_type^.cr_release(or_object);
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
     or_type^.cr_addref(or_object);
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
  clss: PLseType;
  vobj: pointer;
begin
  Result := (VD <> nil) and (lse_vtype(VD) = LSV_OBJECT) and (VD^.VObject <> nil);
  if Result then
  begin
    vobj := VD^.VObject;
    clss := lse_type(VD);
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

procedure KLiEngine.RollbackCompiled;
var
  X: integer;
begin
  try
    for X := FCompiledObjects.Count - 1 downto 0 do
      TLseObject(FCompiledObjects[X]).Free;
  finally
    FreeAndNil(FCompiledObjects);
  end;
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
  lse_check(not Running, 'the engine is running');
end;

function KLiEngine.ReadValue(const Name: string): string;
var
  data: PLseValue;
begin
  try
    data := FMainValues.FindValue(Name);
    if data <> nil then
      Result := lse_get_str(data) else
    if Name = 'search' then
      Result := GetSearchPath else
    if Name = 'mainfile' then
      Result := FMainFile else
      Result := kernel_config(Name);
  except
    Result := '';
  end;
end;

procedure KLiEngine.AddCompiled(AObject: TLseObject);
begin
  if FCompiledObjects <> nil then
  begin
    if (AObject is KLiFunc) and KLiFunc(AObject).FModule.FParsing then Exit;
    FCompiledObjects.Add(AObject);
  end;
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
    FLastCall := nil;
    FStackBase := 0;
    FStack := KLiVarList.Create(FEngine);
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
    FStack.Free;
  finally
    engine_unlock(FEngine);
  end;
  inherited;
end;

function KLiRunner.HasNext: boolean;
begin
  Result := (FCurrent <> nil) and
            (FCurrent^.next < FCurrent^.func.FCodes.Count);
end;

function KLiRunner.Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
var
  base, index, count: integer;
  data: RLseValue;
  last: RLiCallSnap;
  call: RLseParam;
  snap: RLiSnap;
  clss: PLseType;
  curr: KLiFunc_curry;

  function invoke_func: boolean;
  begin
    if Assigned(func.FModule.FInvokeProc) then
      func.FModule.FInvokeProc(TLseFuncInvoke(func.FProc), @call) else
      TLseFuncCall(func.FProc)(@call);
    Result := not (Terminated or FExcepted);
    if Result then
      lse_type_cast(func.FResultType, call.p_result) else
      FExcepted := true;
  end;

begin
  try
    last.prev := FLastCall;
    last.call := nil;
    last.snap := nil;
    FLastCall := @last;
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
        curr := KLiFunc_curry(func);
        count := curr.CurryCount;
        for index := 0 to count - 1 do
          lse_set_value(FStack.Insert(base + index), curr.CurryData(index));
        Inc(ParamCount, count);
        func := curr.FCurryFunc;
      end
      else count := 0;

      for index := count to func.ParamCount - 1 do
      begin
        clss := func.GetParam(index)^.va_type;
        if index < ParamCount then
          lse_type_cast(clss, FStack[base + index]) else
          lse_set_nil(FStack.Add, clss);
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
        last.call := @call;
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
      end
      else
      begin
        snap.base := base;
        snap.next := 0;
        snap.values := nil;
        snap.output := nil;
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
          if func.FModule.FMainFunc = func then
            func.FModule.FMainFunc := nil;
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
          last.snap := FCurrent;
          while ExecGoonNext do
          begin
            { nothing }
          end;
          Result := not FExcepted;
          if Result then
          begin
            if FStack.Count > base then
              lse_set_value(snap.output, FStack[-1]) else
              lse_clear_value(snap.output);
            if func.IsConstFunc then
            begin
              FEngine.FMainValues.SetValue(Format('@%p', [pointer(func)]), snap.output);
              func.FProc := @udc_const;
            end;
          end;
        finally
          FCurrent := snap.prior;
          FExprrec := snap.exprec;
          snap.values.DecRefcount;
          FStack.SetCount(base);
        end;
      end;
    finally
      FLastCall := last.prev;
    end;
  except
    Result := false;
    if not FExcepted then
      ErrorRT(lse_exception_str);
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
    next := base;
    while not (next^ in ['%', #0]) do Inc(next);
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
      if not (next^ in LCS_DIGIT) then ErrorFmt;
      repeat Inc(next) until not (next^ in LCS_DIGIT);
      if next^ <> ']' then ErrorFmt;
      SetString(temp, base, next - base);
      Result := Result + lse_get_str(ValueAt(StrToInt(temp)));
    end
    else
    if next^ = '(' then
    begin
      Inc(next);
      base := next;
      if not (next^ in LCS_HEAD + [':']) then ErrorFmt;
      repeat Inc(next) until not (next^ in LCS_ID + [':']);
      if next^ <> ')' then ErrorFmt;
      SetString(temp, base, next - base);
      Result := Result + GetString(temp);
    end
    else
    if next^ = '{' then
    begin
      Inc(next);
      base := next;
      if not (next^ in LCS_HEAD) then ErrorFmt;
      while next^ in (['.'] + LCS_ID) do Inc(next);
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
        'd': Result := Result + SysUtils.Format(temp, [lse_get_int(data)]);
        'u': Result := Result + SysUtils.Format(temp, [lse_get_int(data)]);
        'e': Result := Result + SysUtils.Format(temp, [lse_get_float(data)]);
        'f': Result := Result + SysUtils.Format(temp, [lse_get_float(data)]);
        'g': Result := Result + SysUtils.Format(temp, [lse_get_float(data)]);
        'n': Result := Result + SysUtils.Format(temp, [lse_get_float(data)]);
        'm': Result := Result + SysUtils.Format(temp, [lse_get_float(data)]);
        'p': Result := Result + SysUtils.Format(temp, [lse_get_int(data)]);
        'c',
        's': Result := Result + SysUtils.Format(temp, [lse_get_str(data)]);
        'x': Result := Result + SysUtils.Format(temp, [lse_get_int(data)]);
        else ErrorFmt;
      end;
      Inc(index);
    end;
    base := next + 1;
  until (base^ = #0);
end;

procedure KLiRunner.Terminate;
begin
  FTerminated := true;
end;

function KLiRunner.GetString(const ID: string): string;
var
  R: RLiFind;
  data: PLseValue;

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
      else Result := lse_get_str(@V);
    finally
      lse_set_nil(@V);
    end;
  end;
  
begin
  Result := '';
  data := FCurrent^.values.GetValue(ID);
  if data <> nil then
    Result := lse_get_str(data) else
  if CurrentFunc.FindBy(ID, @R) then
    if R.fo_type = foFunc then
    begin
      if R.VFunc.IsNameCall then
        Result := smart_call_string(R.VFunc) else
        Result := R.VFunc.Prototype;
    end
    else
    if R.fo_type = foType then
      Result := type_full_name(R.VType);
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
    Result := lse_new_value;
    DoPut(Key, Result);
  end
  else Result := hash^.hi_data;
end;

procedure KLiHashed.FreeItem(Item: PLiHashItem);
begin
  if Item^.hi_data <> nil then
  begin
    lse_free_value(Item^.hi_data);
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
  lse_set_object(V, KT_HASHED, Self);
end;

function KLiHashed.SetValue(const Key: string; Value: PLseValue): PLseValue;
begin
  Result := ForceValue(Key);
  lse_set_value(Result, Value);
end;

////////////////////////////////////////////////////////////////////////////////

procedure kernel_startup;
var
  X: KLiSymbol;
  R: RLseType;

  function setup_type(M: KLiModule; T: TLseKernelType): PLseType;
  begin
    Result := @sys_kernel_types[T];
    Move(R, Result^, sizeof(RLseType));
    Result^.cr_module := M;
    M.FTypeList.Add(Result);
  end;

begin
  if not sys_init_lysee then
  begin
    sys_init_lysee := true;
    sys_libraries := KLiModuleList.Create(nil);
    sys_spinlock := Syncobjs.TCriticalSection.Create;
    sys_version := LSE_VERSION;
    kernel_load_confile('');

    { setup sys module}
    
    sys_module := KLiModule.Create('sys', nil, moyKernel);

    lse_type_init(@R, LSV_VOID);
    KT_VOID := setup_type(sys_module, kcVoid);

    lse_type_init(@R, LSV_STRING);
    R.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_strec_addref;
    R.cr_release := {$IFDEF FPC}@{$ENDIF}lse_strec_release;
    R.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_string_vargen;
    R.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_string_getiv;
    R.cr_length := {$IFDEF FPC}@{$ENDIF}type_string_length;
    KT_STRING := setup_type(sys_module, kcString);

    lse_type_init(@R, LSV_INT);
    R.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_int_vargen;
    KT_INT := setup_type(sys_module, kcInteger);

    lse_type_init(@R, LSV_FLOAT);
    KT_FLOAT := setup_type(sys_module, kcFloat);

    lse_type_init(@R, LSV_VARIANT);
    KT_VARIANT := setup_type(sys_module, kcVariant);

    lse_type_init(@R, 'type', 'type', LSV_OBJECT);
    R.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_long_life;
    R.cr_release := {$IFDEF FPC}@{$ENDIF}lse_long_life;
    R.cr_otos := {$IFDEF FPC}@{$ENDIF}type_type_otos;
    KT_TYPE := setup_type(sys_module, kcType);

    lse_type_init(@R, 'module', 'module', LSV_OBJECT);
    R.cr_otos := {$IFDEF FPC}@{$ENDIF}type_module_otos;
    R.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_module_getpv;
    KT_MODULE := setup_type(sys_module, kcModule);

    lse_type_init(@R, 'function', 'function', LSV_OBJECT);
    R.cr_otos := {$IFDEF FPC}@{$ENDIF}type_function_otos;
    R.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_function_vargen;
    KT_FUNC := setup_type(sys_module, kcFunc);

    lse_type_init(@R, 'error', 'error', LSV_OBJECT);
    R.cr_otos := {$IFDEF FPC}@{$ENDIF}type_error_otos;
    KT_ERROR := setup_type(sys_module, kcError);

    lse_type_init(@R, 'stream', 'stream', LSV_OBJECT);
    R.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_stream_addref;
    R.cr_release := {$IFDEF FPC}@{$ENDIF}lse_stream_release;
    R.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_stream_vargen;
    R.cr_add := {$IFDEF FPC}@{$ENDIF}type_stream_add;
    R.cr_length := {$IFDEF FPC}@{$ENDIF}type_stream_length;
    KT_STREAM := setup_type(sys_module, kcStream);

    lse_type_init(@R, 'varlist', 'varlist', LSV_OBJECT);
    R.cr_otos := {$IFDEF FPC}@{$ENDIF}type_varlist_otos;
    R.cr_vargen := {$IFDEF FPC}@{$ENDIF}type_varlist_vargen;
    R.cr_add := {$IFDEF FPC}@{$ENDIF}type_varlist_add;
    R.cr_length := {$IFDEF FPC}@{$ENDIF}type_varlist_length;
    R.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_varlist_getiv;
    R.cr_setiv := {$IFDEF FPC}@{$ENDIF}type_varlist_setiv;
    KT_VARLIST := setup_type(sys_module, kcVarlist);

    lse_type_init(@R, 'varsnap', 'varsnap', LSV_OBJECT);
    R.cr_getiv := {$IFDEF FPC}@{$ENDIF}type_varlist_getiv;
    R.cr_setiv := {$IFDEF FPC}@{$ENDIF}type_varlist_setiv;
    R.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_varsnap_getpv;
    R.cr_setpv := {$IFDEF FPC}@{$ENDIF}type_varsnap_setpv;
    R.cr_length := {$IFDEF FPC}@{$ENDIF}type_varsnap_length;
    KT_VARSNAP := setup_type(sys_module, kcVarsnap);

    lse_type_init(@R, 'hashed', 'hashed', LSV_OBJECT);
    R.cr_length := {$IFDEF FPC}@{$ENDIF}type_hashed_length;
    R.cr_getpv := {$IFDEF FPC}@{$ENDIF}type_hashed_getpv;
    R.cr_setpv := {$IFDEF FPC}@{$ENDIF}type_hashed_setpv;
    KT_HASHED := setup_type(sys_module, kcHashed);

    lse_type_init(@R, 'vargen', 'vargen', LSV_OBJECT);
    R.cr_addref := {$IFDEF FPC}@{$ENDIF}lse_vargen_addref;
    R.cr_release := {$IFDEF FPC}@{$ENDIF}lse_vargen_release;
    KT_VARGEN := setup_type(sys_module, kcVargen);

    sys_module.SetupModuleFuncs(@sys_module_funcs);
    sys_module.FindFunc('eol').IsNameCall := true;
    sys_nothing := sys_module.FindFunc('nothing');

    { setup operator functions }
    
    for X := Low(KLiSymbol) to High(KLiSymbol) do
    begin
      sys_runner_procs[X] := {$IFDEF FPC}@{$ENDIF}runner_error;
      if X in OperIDSyms then
        KLiFunc_oper.Create(X);
    end;
    sys_oper_inc := sys_module.FindFunc('+');

    { setup runner procedures }

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
    sys_runner_procs[syBitNot]   := {$IFDEF FPC}@{$ENDIF}runner_bit_not;
    sys_runner_procs[syBitXor]   := {$IFDEF FPC}@{$ENDIF}runner_bit_xor;
    sys_runner_procs[syBitOr]    := {$IFDEF FPC}@{$ENDIF}runner_bit_or;
    sys_runner_procs[syBitAnd]   := {$IFDEF FPC}@{$ENDIF}runner_bit_and;
    sys_runner_procs[syBitShl]   := {$IFDEF FPC}@{$ENDIF}runner_bit_shl;
    sys_runner_procs[syBitShr]   := {$IFDEF FPC}@{$ENDIF}runner_bit_shr;
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
    sys_runner_procs[syUpto]     := {$IFDEF FPC}@{$ENDIF}runner_upto;
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
    sys_libraries.Clear;
    FreeAndNil(sys_libraries);
    FreeAndNil(sys_configures);
    FreeAndNil(sys_mimes);
    FreeAndNil(sys_spinlock);
  end;
except
  { safe & quiet }
end;

end.
