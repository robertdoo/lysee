{==============================================================================}
{        UNIT: lse_kernel                                                      }
{ DESCRIPTION: kernel of lysee                                                 }
{     CREATED: 2003/02/29                                                      }
{    MODIFIED: 2010/09/25                                                      }
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
  KLiFunc_curry   = class;
  KLiClass        = class;
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
  KLiInvoke       = class;

  KLiObjRecState  = (orsInChain, orsMarked);
  KLiObjRecStates = set of KLiObjRecState;
  
  PLiObjRec = ^RLiObjRec;
  RLiObjRec = packed record
    or_object: pointer;
    or_class : KLiClass;
    or_prev  : PLiObjRec;
    or_next  : PLiObjRec;
    or_state : KLiObjRecStates;
  end;

  KLiFindObject = (foNone, foVarb, foFunc, foClass);
  KLiFindObjects = set of KLiFindObject;

  KLiFindRec = packed record
    case fo_type: KLiFindObject of
      foVarb : (VVarb : KLiVarb);
      foFunc : (VFunc : KLiFunc);
      foClass: (VClass: KLiClass);
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
    Name: string;        {<--syCall, syFloat, syTime, syMoney, syInt, syStr}
    ParamCount: integer; {<--syCall, syReturn}
    Next: PLiExprRec;
    flags: KLiExprFlags; {<--express flags}
    case KLiSymbol of
      syID   : (VVarb   : KLiVarb);
      syFunc : (VFunc   : KLiFunc);    // syCall
      syFloat: (VFLoat  : double);
      syMoney: (VMoney  : currency);
      syTime : (VTime   : TDateTime);
      syInt  : (VInteger: int64);
      syStr  : (VStr    : PLseString);
      syChar : (VChar   : char);
      syType : (VType   : KLiClass);
      syJump : (VOffset : integer);
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
    FIsLsp: boolean;          {<--is LSP?}
    FIsShadow: boolean;       {<--is a shadow parser}
    FBreakLabel: string;      {<--break label name}
    FContinueLabel: string;   {<--continue label name}
    FEndBlockSyms: KLiSymbols;{<--symbols of block end}
    FToDotComma: boolean;     {<--dup block end symbol to syDotComma}
    function GetLastRow: integer;
    function GetLastCol: integer;
    function GetLastVal: string;
    function Shadow: KLiParser;
    function GetIncludedFile: string;
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
    { labels }
    procedure SaveLabels(var BreakLabel, ContinueLabel: string; CreateNewLabels: boolean);
    procedure RestoreLabels(const BreakLabel, ContinueLabel: string);
    { parsing }
    procedure ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
    function  ParseLambda(ExpandThis: boolean): KLiFunc;
    function  ParseLambdaFunc(ExpandThis: boolean): KLiFunc;
    procedure ParseVarType(var VarType: KLiClass; Token: PLiToken);overload;
    procedure ParseVarType(var VarType: KLiClass; OnHead: boolean);overload;
    procedure ParseVarb(var varb: KLiVarb; var vrec: PLiToken;
                        EndSyms: KLiSymbols; OnHead: boolean);

    procedure ParseDefine;
    procedure ParseConst;
    procedure ParseClass;
    procedure ParseImport;
    procedure ParseOption;
    procedure ParseInclude;
    procedure ExecInclude(EndSym: KLiSymbol);
    procedure ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
    procedure ParseStatement;
    procedure ParseAny;
    procedure ParseIf;
    procedure ParseAddOne;
    procedure ParseFor;
    procedure ParseWhile;
    procedure ParseRepeatUntil;
    procedure ParseSwitch;
    procedure ParseBreak;
    procedure ParseContinue;
    procedure ParseReturn;
    procedure ParseTry;
    procedure ParseBecome;

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
    function Parse(const Code: string; IsLsp: boolean): KLiFunc;
    function ParseAndFree(const Code: string; IsLsp: boolean): KLiFunc;
    property Module: KLiModule read FModule;
    property Last: PLiToken read FLast;
    property LastRow: integer read GetLastRow;
    property LastCol: integer read GetLastCol;
    property LastVal: string read GetLastVal;
    property IncludedFile: string read GetIncludedFile;
  end;

  { KLiVarb }

  KLiVarb = class(KLiObject)
  private
    FName: string;
    FType: KLiClass;
    FList: KLiVarbList;
    FPos: KLiSymPos;
    FIndex: integer;
  public
    constructor Create(AList: KLiVarbList; const Name: string;
                       ValueType: KLiClass);virtual;
    destructor Destroy;override;
    function Prototype(HideType: boolean): string;
    function IsParam: boolean;
    function IsThis: boolean;
    function Func: KLiFunc;
    property Name: string read FName;
    property ValueType: KLiClass read FType;
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
    function Add(const Name: string; ValueType: KLiClass): KLiVarb;
    function IndexOf(const Name: string): integer;
    function Find(const Name: string): KLiVarb;
    function Exists(const Name: string): boolean;
    function ToVarlist(Engine: KLiEngine): KLiVarList;
    function ToString(HideType, HideThis: boolean): string;
    function IsParam: boolean;
    property Func: KLiFunc read FFunc;
    property Count: integer read GetCount;
    property Varbs[Index: integer]: KLiVarb read GetVarb;default;
  end;

  KLiMethodType = (cmCreator, cmCount, cmGetAt, cmSetAt, cmGetPV, cmSetPV,
                   cmMethod, cmNormal);

  KLiFuncState = (fusIsMainFunc, fusIsInitFunc, fusIsCurry, fusIsNameCall,
                  fusIsLambda, fusIsConst, fusHasConstValue, fusExpandThis);
  KLiFuncStates = set of KLiFuncState;

  { KLiFunc }

  KLiFunc = class(KLiObject)
  private
    FName: string;         {<--function name}
    FKind: KLiMethodType;  {<--function type}
    FModule: KLiModule;    {<--owner module}
    FOwnerClass: KLiClass; {<--owner class type}
    FParams: KLiVarbList;  {<--parametres list}
    FResultType: KLiClass; {<--function result type}
    FDescription: string;  {<--description}
    FState: KLiFuncStates; {<--function state}
    FCodes: KLiExprList;   {<--instruction buffer}
    FProc: pointer;        {<--call back function}
    FComponent: TComponent;{<--lazarus or delphi component}
    function HasState(Index: KLiFuncState): boolean;
    procedure SetState(Index: KLiFuncState; Value: boolean);
  public
    constructor Create(AOwnerClass, AResultType: KLiClass;
      const Name, Desc: string; Params: TStringList; Proc: pointer;
      Kind: KLiMethodType);
    destructor Destroy;override;
    procedure Garbaged;virtual;
    function Execute(Param: PLseParam): boolean;
    procedure DumpCode(list: TStrings; const margin: string);
    function ParamCount: integer;
    function ParamList(HideType, HideThis: boolean): string;
    function AddParam(const Name: string; varType: KLiClass): KLiVarb;
    function Prototype(ShowFullName: boolean): string;
    function FullName: string;
    function CanInvoke(func: KLiFunc): boolean;
    function FindInside(const ID: string; rec: PLiFindRec = nil): boolean;
    function FindDeclared(const ID: string; rec: PLiFindRec = nil): boolean;
    function CanDeclare(const ID: string): boolean;
    function FindBy(const ID: string; rec: PLiFindRec; Range: KLiFindObjects = []): boolean;
    function Engine: KLiEngine;
    function IsScript: boolean;
    function IsClassMethod: boolean;
    function IsConstructor: boolean;
    function GetVarbData(const VarbName: string;
      var Varb: KLiVarb; var Data: PLseValue): boolean;virtual;
    property Name: string read FName;
    property Module: KLiModule read FModule;
    property Params: KLiVarbList read FParams;
    property ResultType: KLiClass read FResultType write FResultType;
    property IsMainFunc: boolean index fusIsMainFunc read HasState;
    property IsInitFunc: boolean index fusIsInitFunc read HasState write SetState;
    property IsCurryFunc: boolean index fusIsCurry read HasState write SetState;
    property IsNameCall: boolean index fusIsNameCall read HasState write SetState;
    property IsConstFunc: boolean index fusIsConst read HasState write SetState;
    property ExpandThis: boolean index fusExpandThis read HasState write SetState;
    property IsLambdaFunc: boolean index fusIsLambda read HasState write SetState;
    property Description: string read FDescription write FDescription;
    property Proc: pointer read FProc write FProc;
    property OwnerClass: KLiClass read FOwnerClass;
    property Codes: KLiExprList read FCodes;
    property Component: TComponent read FComponent write FComponent;
  end;

  KLiExprList = class(KLiObject)
  private
    FOwner: KLiFunc;
    FLocals: KLiVarbList;
    FItems: KLiList;
    FSatisfyIndex: integer;
    FGotos: array of PLiExprRec;
    FLabels: array of PLiExprRec;
    FPos: KLiSymPos;
    function GetCount: integer;
    function GetItem(Index: integer): PLiExprRec;
    function GetLast: PLiExprRec;
    function GetLabelCount: integer;
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
    function BeginStatement(Pos: KLiSymPos): PLiExprRec;
    function IsEmpty: boolean;
    { locals }
    function AddLocal(const Name: string; varType: KLiClass): KLiVarb;
    procedure PushVarb(AVarb: KLiVarb; Pos: KLiSymPos);
    property Locals: KLiVarbList read FLocals;
    { label }
    function AddGoto(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function AddTry(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function AddLabel(const Name: string; Pos: KLiSymPos): PLiExprRec;
    function FindLabel(const Name: string): PLiExprRec;
    function RebuildLabelList: integer;
    property LabelCount: integer read GetLabelCount;
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
    constructor CreateConst(AModule: KLiModule;
      const AName: string; AType: KLiClass);
    destructor Destroy;override;
    procedure Garbaged;override;
    function AddCurry(value: PLseValue): integer;
    function GetVarbData(const VarbName: string;
      var Varb: KLiVarb; var Data: PLseValue): boolean;override;
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
  
  { KLiClass }

  KLiClassState = (clsModule, clsSimple, clsBuiltin, clsReady, clsSatisfied,
                   clsHashed, clsUDC);
  KLiClassStates = set of KLiClassState;

  KLiClass = class(KLiObject)
  private
    FName: string;            {<--class name}
    FModule: KLiModule;       {<--owner module}
    FClassRec: RLseClassRec;  {<--class information}
    FInitFunc: KLiFunc;
    FCmCreator: KLiFunc;
    FCmCount: KLiFunc;
    FCmGetAt: KLiFunc;
    FCmSetAt: KLiFunc;
    FCmGetPV: KLiFunc;
    FCmSetPV: KLiFunc;
    fCmFuncs: TStringList;    {<--cmMethod, cmNormal}
    FState: KLiClassStates;
    FUID: string;             {<--unique identiry}
    function GetClassRec: PLseClassRec;
    function GetDataType: TLseValue;
    function GetDescription: string;
    function GetInfomation: string;
    function GetBuiltin: boolean;
    function GetModuleFile: string;
    function GetFullName: string;
    function GetHasCreator: boolean;
    function GetMethodList: TStrings;
    procedure SetState(Index: KLiClassState; Value: boolean);
    function HasState(Index: KLiClassState): boolean;
  public
    constructor Create(Module: KLiModule; const Name: string; ADataType: TLseValue);
    destructor Destroy;override;
    procedure DumpCodeToStream(stream: TStream; const margin: string);
    procedure Satisfy;
    function Prototype(const ID: string): string;
    procedure DeleteFunctions;
    function ObjectToStrec(obj: pointer): PLseString;
    function ObjectToString(obj: pointer): string;
    function StrecToObject(const S: PLseString; Engine: KLiEngine): pointer;
    function SetupMethod(Func: PLseFuncRec): KLiFunc;
    function FindMethod(Kind: KLiMethodType; const AName: string): KLiFunc;
    function FindGetMethod(const prop_name: string): KLiFunc;
    function FindSetMethod(const prop_name: string): KLiFunc;
    function SingleMethod(cate: KLiMethodType): KLiFunc;
    function SetSingleMethod(cate: KLiMethodType; func: KLiFunc): boolean;
    function DispSingleMethod(func: KLiFunc): boolean;
    function ListFuncTo(List: TList): TList;
    function FindInside(const ID: string; rec: PLiFindRec = nil): boolean;
    function FindDeclared(const ID: string; rec: PLiFindRec = nil): boolean;
    function Cast(Param: PLseParam; Index: integer): PLseValue;
    property Name: string read FName;
    property FullName: string read GetFullName;
    property Module: KLiModule read FModule;
    property ClassRec: PLseClassRec read GetClassRec;
    property DataType: TLseValue read GetDataType;
    property Description: string read GetDescription;
    property Infomation: string read GetInfomation;
    property Builtin: boolean read GetBuiltin;
    property FileName: string read GetModuleFile;
    property IsModuleClass: boolean index clsModule read HasState;
    property IsSimpleType: boolean index clsSimple read HasState;
    property IsBuiltinClass: boolean index clsBuiltin read HasState;
    property IsHashed: boolean index clsHashed read HasState;
    property IsUDC: boolean index clsUDC read HasState;
    property Creator: KLiFunc read FCmCreator;
    property HasCreator: boolean read GetHasCreator;
    property MethodList: TStrings read GetMethodList;
    property UID: string read FUID;
  end;

  KLiModuleType = (
    moyKernel,        {<--K: builtin sys module}
    moyRegistered,    {<--R: by lse_setup_module()}
    moyLibrary,       {<--L: extending library}
    moyScript         {<--S: Lysee script moudle}
  );

  KLiModuleState = (mosParsing);
  KLiModuleStates = set of KLiModuleState;

  { KLiModule }

  KLiModule = class(KLiObject)
  private
    FName: string;              {<--KRLS: module name}
    FFileName: string;          {<--KRLS: module file name}
    FModuleType: KLiModuleType; {<--KRLS: module type}
    FVersion: string;           {<--KRLS: module version}
    FDescription: string;       {<--KRLS: description}
    FClassList: TStringList;    {<--KRLS: class list}
    FModuleClass: KLiClass;     {<--KRLS: module class}
    FImportProc: TLseOnImport;  {<--KRL*: called when importing this module}
    FInvokeProc: TLseOnInvoke;  {<--*RL*: call gate function}
    FHandle: THandle;           {<--**L*: library (DLL) handle}
    FEngine: KLiEngine;         {<--***S: owner script engine}
    FModules: KLiModuleList;    {<--***S: modules imported by this module}
    FImporters: TList;          {<--***S: modules importing this module}
    FMainFunc: KLiFunc;         {<--***S: modules entry function}
    FState: KLiModuleStates;    {<--***S: module state}
    function GetIsLibrary: boolean;
    function GetIsScript: boolean;
    function GetIsBuiltin: boolean;
    function GetState(Index: KLiModuleState): boolean;
    procedure SetState(Index: KLiModuleState; Value: boolean);
  public
    constructor Create(const Name: string; Engine: KLiEngine; ModuleType: KLiModuleType);
    destructor Destroy;override;
    procedure Satisfy;
    procedure DeleteFunctions;
    procedure ImportNotification;
    function NewTempID(const Prefix: string): string;
    function NewFuncName: string;
    function NewLabelName: string;
    function NewFunc: KLiFunc;

    { module }

    function FindModule(const ID: string; FindPossible: boolean): KLiModule;
    function FindModuleClass(const ID: string): KLiClass;
    function SetupModuleClass(Rec: PLseFuncListRec): KLiClass;
    function ModuleClass: KLiClass;
    function AddImporter(module: KLiModule): KLiModule;
    function ImportedBy(module: KLiModule): boolean;
    function IsMainModule: boolean;
    property ModuleType: KLiModuleType read FModuleType;
    property Modules: KLiModuleList read FModules;

    { class }

    function ClassCount: integer;
    function GetClass(Index: integer): KLiClass;
    function FindClass(const ID: string): KLiClass;
    function FindClassBy(const ID, module_name: string): KLiClass;

    { function }

    function FuncList: TStringList;
    function FuncCount: integer;
    function GetFunc(Index: integer): KLiFunc;
    function FindFunc(const ID: string): KLiFunc;

    { find }

    function Find(const ID: string; rec: PLiFindRec): boolean;
    function FindBy(const ID, module_name: string; rec: PLiFindRec): boolean;
    function Declared(const ID: string): boolean;
    function CanDeclare(const ID: string): boolean;

    property FileName: string read FFileName write FFileName;
    property Name: string read FName;
    property IsBuiltin: boolean read GetIsBuiltin;
    property IsLibrary: boolean read GetIsLibrary;
    property IsScript: boolean read GetIsScript;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Engine: KLiEngine read FEngine;
    property Parsing: boolean index mosParsing read GetState write SetState;
    property MainFunc: KLiFunc read FMainFunc;
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

  RLiError = packed record
    errno : integer;               {<--error number}
    error : array[0..63] of char;  {<--error name}
    errmsg: array[0..259] of char; {<--error message}
    module: array[0..63] of char;  {<--error module name}
    ifname: array[0..259] of char; {<--included file name}
    row   : integer;               {<--error row}
    col   : integer;               {<--error column}
  end;
  PLiError = ^RLiError;

  { KLiError }

  KLiError = class(KLiObject)
  private
    FErrec: RLiError;
    FEngine: KLiEngine;
    function GetErrorText: string;
    function GetErrno: integer;
    procedure SetErrno(const Value: integer);
    function GetRow: integer;
    procedure SetRow(const Value: integer);
    function GetCol: integer;
    procedure SetCol(const Value: integer);
    function GetMsg: string;
    procedure SetMsg(const Value: string);
    function GetModule: string;
    procedure SetModule(const Value: string);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetErrorRec: PLiError;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetIncludedFile(FileID: integer): string;
  protected
    { SyntaxError }
    procedure SymNotFound(Parser: KLiParser);
    procedure ClassNotExists(Parser: KLiParser);
    procedure WrongIDName(Parser: KLiParser);
    procedure SymExpected(Parser: KLiParser; const syms: string);
    procedure SymUnexpected(Parser: KLiParser);
    procedure Redeclared(Parser: KLiParser);
    procedure TooManyParam(Parser: KLiParser; func: KLiFunc);
    procedure BreakNoLoop(Parser: KLiParser);
    procedure ContinueNoLoop(Parser: KLiParser);
    procedure ThrowNothing(Parser: KLiParser);
    procedure WrongException(Parser: KLiParser);
    procedure CatchNotFound(Parser: KLiParser);
    procedure NeedPureID(Parser: KLiParser);
    procedure WrongModuleName(Parser: KLiParser);
    procedure ModuleReimport(Parser: KLiParser);
    procedure ModuleNotFound(Parser: KLiParser);
    procedure FileNotFound(Parser: KLiParser);
    procedure WrongLibrary(Parser: KLiParser);
    procedure ImportEachOther(Parser: KLiParser; module: KLiModule);
    procedure LocalNotExists(Parser: KLiParser);
    { SatisfyError }
    procedure ObjectNotExists(func: KLiFunc; expr: PLiExprRec);
    procedure LabelNotExists(func: KLiFunc; expr: PLiExprRec);
    procedure CanNotAsk(func: KLiFunc; expr: PLiExprRec; clss: KLiClass);
    procedure FuncNotFound(func: KLiFunc; expr: PLiExprRec);
    procedure ResetMethod(func: KLiFunc; expr: PLiExprRec; const Method: string);
  public
    constructor Create(AEngine: KLiEngine);
    procedure Clear;
    procedure Write(const name: string; errno, row, col: integer;
      const module, msg, fname: string);
    procedure Error(const name: string; errno, row, col: integer;
      const module, msg, fname: string);
    procedure SyntaxErr(errno, row, col: integer;
      const module, fmt, fname: string; const args: array of const);
    procedure ImportErr(errno, row, col: integer;
      const module, fmt, fname: string; const args: array of const);
    property errno: integer read GetErrno write SetErrno;
    property msg: string read GetMsg write SetMsg;
    property row: integer read GetRow write SetRow;
    property col: integer read GetCol write SetCol;
    property module: string read GetModule write SetModule;
    property name: string read GetName write SetName;
    property ErrorIncludedFile: string read GetFileName write SetFileName;
    property ErrorText: string read GetErrorText;
    property ErrorRec: PLiError read GetErrorRec;
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
    FArguments: KLiStrlist;      {<--argument list}
    FModules: KLiModuleList;     {<--module list}
    FCompiledObjects: KLiList;   {<--compiled objects}
    FReady: boolean;
    FOnReadBuf: KLiReadBuf;
    FOrChain: PLiObjRec;
    FNameSeed: cardinal;         {<--label seed}
    FIncludedFiles: TStringList; {<--file ID list}
    FCGI: TLseObject;            {<--used by KLiCGI}
    FStdinStream: PLseStream;
    FStdoutStream: PLseStream;
    FStderrStream: PLseStream;
    FInvoker: KLiInvoke;
    function GetResultText: string;
    function GetResultType: KLiClass;
    procedure SetMainFile(const AValue: string);
    procedure SetMainSearchPath(const AValue: string);
    function GetMainFunc: KLiFunc;
    function GetMainSnap: KLiVarSnap;
    function GetStdinStream: PLseStream;
    procedure SetStdinStream(const Value: PLseStream);
    function GetStdoutStream: PLseStream;
    procedure SetStdoutStream(const Value: PLseStream);
    function GetStderrStream: PLseStream;
    procedure SetStderrStream(const Value: PLseStream);
  public
    constructor Create(const AEngineRec: PLseEngine);
    destructor Destroy;override;
    procedure Reset(IncludeVar: boolean);
    procedure Clear;
    procedure PrepareCompile;
    function DoCompile(const Code: string; IsLsp: boolean): KLiFunc;
    function Compile(const Code: string; IsLsp: boolean): KLiFunc;
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
    function AddIncludedFile(const FileName: string): integer;
    function GetIncludedFile(FileID: integer): string;
    procedure SetResultTypeText(const RType, RText: string);

    { events }

    procedure EventNotify(ID: integer);virtual;

    { tryings }

    function TryCompileCode(const code: string; IsLsp: boolean): boolean;
    function TryExecuteCode(const code: string; IsLsp: boolean): boolean;
    function TryCompileFile(const fname: string; IsLsp: boolean): boolean;
    function TryExecuteFile(const fname: string; IsLsp: boolean): boolean;
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
    property ResultType: KLiClass read GetResultType;
    property ResultText: string read GetResultText;
    property ExitResultType: string read FExitResultType;
    property ExitResultText: string read FExitResultText;
    property Arguments: KLiStrlist read FArguments;
    property Ready: boolean read FReady;
    property Exited: boolean read FExited write FExited;
    property CGI: TLseObject read FCGI write FCGI;
    property Invoker: KLiInvoke read FInvoker write FInvoker;
    property OnReadBuf: KLiReadBuf read FOnReadBuf write FOnReadBuf;
    property OrChain: PLiObjRec read FOrChain write FOrChain;
    property StdinStream: PLseStream read GetStdinStream write SetStdinStream;
    property StdoutStream: PLseStream read GetStdoutStream write SetStdoutStream;
    property StderrStream: PLseStream read GetStderrStream write SetStderrStream;
    property CompiledObjects: KLiList read FCompiledObjects write FCompiledObjects;
  end;

  { KLiVarList }

  KLiVarList = class(KLiObject)
  private
    FEngine: KLiEngine;
    FObjRec: RLiObjRec;
    FItems: TList;
    FNames: TStringList;
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    procedure ExpandAt(Index, ItemCount: integer);
    procedure CheckIndex(Index: integer);
    procedure Error(const Msg: string; Data: integer);
    function SaveName(const Name: string; Value: PLseValue): boolean;
    function NameIndex(const Name: string): integer;overload;
    function NameIndex(Value: PLseValue): integer;overload;
    function GetName(Index: integer): string;
    function GetData(Index: integer): PLseValue;
  public
    constructor Create(AEngine: KLiEngine);
    destructor Destroy;override;
    procedure Clear;
    procedure ClearTo(NewCount: integer);
    procedure ClearToIndex(Index: integer);
    procedure Press(Times: integer = 1);
    procedure Delete(Index: integer);
    procedure Exchange(Index1, Index2: integer);
    procedure ExchangeLastTwo;
    procedure Move(CurIndex, NewIndex: integer);
    function Insert(Index: integer): PLseValue;
    function Add: PLseValue;
    function AddSend(VG: PLseVargen): boolean;
    function AddSendAll(VG: PLseVargen): integer;
    function Push(Value: PLseValue): PLseValue;
    function PushInt64(Value: int64): PLseValue;
    function PushBoolean(Value: boolean): PLseValue;
    function PushFloat(Value: double): PLseValue;
    function PushTime(Value: TDateTime): PLseValue;
    function PushMoney(Value: currency): PLseValue;
    function PushString(const Value: string): PLseValue;
    function PushChar(Value: char): PLseValue;
    function PushObject(Value: pointer; AClass: KLiClass): PLseValue;
    function PushDefaultValue(AClass: KLiClass): PLseValue;
    function PushValues(List: KLiVarList; ItemCount: integer = 0): integer;
    function MinValue: PLseValue;
    function MaxValue: PLseValue;
    function First: PLseValue;
    function Last: PLseValue;
    function SecondLast: PLseValue;
    function AsString: string;
    function Copy(Index, ItemCount: integer): KLiVarList;
    function Left(ItemCount: integer): KLiVarList;
    function Right(ItemCount: integer): KLiVarList;
    function ValueIndex(const Name: string): integer;overload;
    function ValueIndex(Value: PLseValue): integer;overload;
    function ValueName(Value: PLseValue): string;
    function AddNamed(const Name: string): PLseValue;
    function InsertNamed(Index: integer; const Name: string): PLseValue;
    function RemoveNamed(const Name: string): boolean;
    function GetNamed(const Name: string): PLseValue;
    function GetNameList(Sorted: boolean): KLiStrList;
    function IsSnap: boolean;virtual;
    property Engine: KLiEngine read FEngine;
    property Count: integer read GetCount write SetCount;
    property Datas[Index: integer]: PLseValue read GetData;default;
    property Names[Index: integer]: string read GetName;
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
    function FindVarb(const Name: string): KLiVarb;
    procedure Prepare;
    procedure ClearValues;
    function GetParamValues(Values: KLiVarList): integer;
    function ParamCount: integer;
    function IsSnap: boolean;override;
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
    prior : PLiSnap;       // prior FCurrent
    exprec: PLiExprRec;    // prior FExprrec
    output: PLseValue;     // result
    outype: KLiClass;      // result type
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
    FShellExitCode: integer;
    FMatchPatten: RLiMatchPatten;
    function ExecGoonNext: boolean;
  public
    constructor Create(Engine: KLiEngine);
    destructor Destroy;override;
    procedure ErrorRT(const ErrorStr: string);
    procedure Terminate;
    procedure Eval(const Code: string; Output: PLseValue; IsLsp: boolean);
    procedure Return(HasResult: boolean);
    function Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
    function GoonConst(Param: PLseParam): boolean;
    function ToString(const ID: string): string;
    function FormatFor(const Fmt: string; Values: KLiVarList): string;
    function GetValue(varb: KLiVarb): PLseValue;
    function ListMatchResult: KLiVarList;
    function HasNext: boolean;
    function IncludedFile: string;
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
    property ShellExitCode: integer read FShellExitCode write FShellExitCode;
  end;

  { KLiSatisfy }
  
  RLiSatisfy = packed record
    s_type: KLiClass;
    s_expr: PLiExprRec;
    s_func: KLiFunc;
  end;
  PLiSatisfy = ^RLiSatisfy;
  
  KLiSatisfy = class(KLiObject)
  private
    FItems: TList;
    function GetCount: integer;
    function GetType(Index: integer): KLiClass;
    function GetExpr(Index: integer): PLiExprRec;
    function GetFunc(Index: integer): KLiFunc;
    function GetLastType: KLiClass;
    function GetLastExpr: PLiExprRec;
    function GetLastFunc: KLiFunc;
  protected
    function NewData(T: KLiClass; X: PLiExprRec; F: KLiFunc): PLiSatisfy;
    function GetData(Index: integer): PLiSatisfy;
    function GetLastData: PLiSatisfy;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Press(Count: integer = 1);
    procedure DupLast(N: integer);
    function Add(T: KLiClass; X: PLiExprRec; F: KLiFunc): integer;
    property Count: integer read GetCount;
    property Types[Index: integer]: KLiClass read GetType;
    property Exprs[Index: integer]: PLiExprRec read GetExpr;
    property Funcs[Index: integer]: KLiFunc read GetFunc;
    property LastType: KLiClass read GetLastType;
    property LastExpr: PLiExprRec read GetLastExpr;
    property LastFunc: KLiFunc read GetLastFunc;
  end;

  { KLiHashed }

  KLiHashed = class(KLiHashTable)
  private
    FEngine: KLiEngine;
    FObjRec: RLiObjRec;
    procedure EnumAdd(const Key: string; Value, Param: pointer);
    procedure EnumListKV(const Key: string; Value, Param: pointer);
  protected
    procedure FreeItem(Item: PLiHashItem);override;
  public
    constructor Create(AEngine: KLiEngine; Size: cardinal = 1);
    destructor Destroy;override;
    function ForceValue(const Key: string): PLseValue;
    function ForceValueByChain(const Key: string): PLseValue;
    function FindValue(const Key: string): PLseValue;
    function FindValueByChain(const Key: string): PLseValue;
    function SetValueByChain(const Key: string; Value: PLseValue): PLseValue;
    function SetValue(const Key: string; Value: PLseValue): PLseValue;
    function SetStr(const Key, Value: string): PLseValue;
    function SetStrByChain(const Key, Value: string): PLseValue;
    function SetInt64(const Key: string; Value: int64): PLseValue;
    function SetObject(const Key: string; Obj: pointer; Clss: PLseClassRec): PLseValue;
    function AddFrom(Hash: KLiHashed): integer;
    function ListKeyValue(List: KLiVarList): integer;
  end;

  KLiExportAPI = class
  private
    FStream: TStream;
    FModuleCount: integer;
    FFuncCount: integer;
    FClassCount: integer;
    FMethodCount: integer;
    procedure Writeln(const Text: string);
    function GetDesc(const Desc: string): string;
    function GetProt(func: KLiFunc): string;
    function GetTotal: string;
    procedure BeginPage;
    procedure WriteMethod(func: KLiFunc);
    procedure WriteConstructor(func: KLiFunc);
    procedure WriteClass(cls: KLiClass);
    procedure WriteFunc(func: KLiFunc);
    procedure WriteModule(module: KLiModule);
    procedure EndPage;
  public
    procedure Execute(const FileName: string);
  end;

  { KLiInvoke }

  KLiInvoke = class(TLseObject)
  private
    FEngine: KLiEngine;
    FParam: PLseParam;
    procedure SetNil(Value: PLseValue);
  public
    constructor Create(const AEngine: KLiEngine);
    procedure ReturnInt(const Value: integer);
    procedure ReturnInt64(const Value: int64);
    procedure ReturnFloat(const Value: double);
    procedure ReturnMoney(const Value: currency);
    procedure ReturnTime(const Value: TDateTime);
    procedure ReturnStr(const Value: string);
    procedure ReturnChar(const Value: char);
    procedure ReturnBool(const Value: boolean);
    procedure ReturnObject(const Value: pointer; AClass: KLiClass);
    procedure ReturnObj(const Value: pointer; AClass: PLseClassRec);
    procedure ReturnStream(const Value: TStream);overload;
    procedure ReturnStream(const Value: PLseStream);overload;
    procedure ReturnStrlist(const Value: KLiStrList);overload;
    procedure ReturnError(const ID: string; Errno: integer; const Msg: string);
    procedure Print(const Str: string);
    function Read(const Buf: pchar; Count: integer): integer;
    function Readln: string;
    function FormatStr(const Str: string): string;
    function GetThis(var This): boolean;
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
    function ParamClass(Index: integer): KLiClass;
    function ParamClassRec(Index: integer): PLseClassRec;
    function ParamTime(Index: integer): TDateTime;
    function ParamStream(Index: integer): PLseStream;
    property Param: PLseParam read FParam write FParam;
    property Engine: KLiEngine read FEngine;
  end;

  TInitProc = procedure;
  TExitProc = TInitProc;

procedure InitLyseeKernel;
procedure ExitLyseeKernel;

{-----------------------------------------------------------------------
(  F_NAME: __ExportAPI
(
(  F_DESC: export API reference to HTML file
(
(  F_ARGS: const HTMLFileName: string - result file name
(
(  F_TYPE:
(
(  EXCEPT:
(----------------------------------------------------------------------}
procedure __ExportAPI(const HTMLFileName: string);

{-----------------------------------------------------------------------
( F_NAME: __LoadConfig
( 
( F_DESC: load configuration, called once by __SetupLseClasses
( 
( F_ARGS:
( 
( F_TYPE:
(----------------------------------------------------------------------}
procedure __LoadConfig;

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
function __NewValue: PLseValue;
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
{ set value                                                            }
{----------------------------------------------------------------------}
function __PutInt64(V: PLseValue; Value: int64): PLseValue;
function __PutFloat(V: PLseValue; Value: double): PLseValue;
function __PutMoney(V: PLseValue; Value: currency): PLseValue;
function __PutTime(V: PLseValue; Value: TDateTime): PLseValue;
function __PutBool(V: PLseValue; Value: boolean): PLseValue;
function __PutChar(V: PLseValue; Value: char): PLseValue;
function __PutString(V: PLseValue; Value: PLseString): PLseValue;overload;
function __PutString(V: PLseValue; const Value: string): PLseValue;overload;
function __PutString(V: PLseValue; const Value: pchar): PLseValue;overload;
function __PutString(V: PLseValue; const Value: pchar; Count: integer): PLseValue;overload;
function __PutObject(V: PLseValue; Clss: PLseClassRec; Obj: pointer): PLseValue;
function __PutDB(V: PLseValue; ADB: PLseDB): PLseValue;
function __PutDS(V: PLseValue; ADS: PLseDS): PLseValue;
function __PutClass(V: PLseValue; Clss: PLseClassRec): PLseValue;overload;
function __PutClass(V: PLseValue; Clss: KLiClass): PLseValue;overload;
function __PutValue(V, Value: PLseValue): PLseValue;

function __SetStrlist(V: PLseValue; Value: KLiStrlist): PLseValue;
function __SetVarlist(V: PLseValue; Value: KLiVarList): PLseValue;
function __SetHashed(V: PLseValue; Value: KLiHashed): PLseValue;
function __SetStream(V: PLseValue; Value: PLseStream): PLseValue;
function __SetFunc(V: PLseValue; Value: KLiFunc): PLseValue;
function __SetVarb(V: PLseValue; Value: KLiVarb): PLseValue;
function __SetModule(V: PLseValue; Value: KLiModule): PLseValue;
function __SetVarGen(V: PLseValue; Value: PLseVargen): PLseValue;
function __SetObject(V: PLseValue; AClass: KLiClass; AInstance: pointer): PLseValue;
function __SetClass(V: PLseValue; Value: KLiClass): PLseValue;

{----------------------------------------------------------------------}
{ read value                                                           }
{----------------------------------------------------------------------}
function __AsInt64(V: PLseValue): int64;
function __AsFloat(V: PLseValue): double;
function __AsMoney(V: PLseValue): currency;
function __AsTime(V: PLseValue): TDateTime;
function __AsString(V: PLseValue): string;
function __AsFileName(V: PLseValue): string;
function __AsStrec(V: PLseValue): PLseString;
function __AsPChar(V: PLseValue): pchar;
function __AsChar(V: PLseValue): char;
function __AsBool(V: PLseValue): boolean;
function __AsObject(V: PLseValue): pointer;overload;
function __AsObject(V: PLseValue; T: KLiClass): pointer;overload;
function __AsRunner(Param: PLseParam): KLiRunner;
function __AsEngine(Param: PLseParam): KLiEngine;
function __AsFunc(V: PLseValue): KLiFunc;
function __AsStrlist(V: PLseValue): KLiStrList;
function __AsVarlist(V: PLseValue): KLiVarList;
function __AsVargen(Engine: KLiEngine; data: PLseValue): PLseVargen;
function __AsClass(V: PLseValue): KLiClass;

function __NewVarlist(Engine: KLiEngine): KLiVarList;
function __GetThis(Param: PLseParam; var This): boolean;

{-----------------------------------------------------------------------
( F_NAME: __IsClassRec
(
( F_DESC: check class record
(
( F_ARGS: R: PLseClassRec
(
( F_TYPE: boolean - true if is ok
(
( EXCEPT:
(----------------------------------------------------------------------}
function __IsClassRec(R: PLseClassRec): boolean;

{-----------------------------------------------------------------------
( F_NAME: __FormatParam
( 
( F_DESC: format param string
( 
( F_ARGS: param: PLseParam
(         index: integer
(
( F_TYPE: string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __FormatParam(Param: PLseParam; Index: integer): string;

{----------------------------------------------------------------------)
(                                                                      )
(                   USER DEFINED FUNCTIONS                             )
(                                                                      )
(----------------------------------------------------------------------}

procedure udc_curry(const Param: PLseParam);cdecl;
procedure udc_const(const Param: PLseParam);cdecl;
procedure udc_create(const Param: PLseParam);cdecl;
procedure udc_getpv(const Param: PLseParam);cdecl;
procedure udc_setpv(const Param: PLseParam);cdecl;
procedure udc_oper(const Param: PLseParam);cdecl;

{-----------------------------------------------------------------------
( F_NAME: __SetClassValue
( 
( F_DESC: convert to specified data type
(
( F_ARGS: E: KLiEngine
(         V: PLseValue
(         AClass: KLiClass - new data type
( 
( F_TYPE: PLseValue
(
( EXCEPT: 
(----------------------------------------------------------------------}
function __SetClassValue(E: KLiEngine; V: PLseValue; AClass: KLiClass): PLseValue;

{-----------------------------------------------------------------------
( F_NAME: __SetDefaultValue
( 
( F_DESC: clear and set default value
(
( F_ARGS: V: PLseValue
(         AClass: KLiClass - new data type
( 
( F_TYPE: PLseValue
( 
( EXCEPT: 
(----------------------------------------------------------------------}
function __SetDefaultValue(V: PLseValue; AClass: KLiClass): PLseValue;

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
function __inc(V, Value: PLseValue): PLseValue;      // V  :=     V  +   Value
function __dec(V, Value: PLseValue): PLseValue;      // V  :=     V  -   Value
function __mul(V, Value: PLseValue): PLseValue;      // V  :=     V  *   Value
function __div(V, Value: PLseValue): PLseValue;      // V  :=     V  /   Value
function __mod(V, Value: PLseValue): PLseValue;      // V  :=     V  %   Value
function __neg(V: PLseValue): PLseValue;             // V  :=   - V

function __xor(V, Value: PLseValue): PLseValue;      // V  :=     V  ^   Value
function __and(V, Value: PLseValue): PLseValue;      // V  :=     V  &   Value
function __or(V, Value: PLseValue): PLseValue;       // V  :=     V  |   Value
function __shl(V, Value: PLseValue; E: KLiEngine): PLseValue;
                                                     // V  :=     V  <<  Value
function __shr(V, Value: PLseValue): PLseValue;      // V  :=     V  >>  Value
function __not(V: PLseValue): PLseValue;             // V  :=   ~ V

function __logicAnd(V, Value: PLseValue): PLseValue; // V  :=     V  and Value
function __logicOr(V, Value: PLseValue): PLseValue;  // V  :=     V  or  Value
function __logicNot(V: PLseValue): PLseValue;        // V  := not V

function __equal(V1, V2: PLseValue): PLseValue;      // V1 :=     V1 ==   V2
function __diff(V1, V2: PLseValue): PLseValue;       // V1 :=     V1 !=   V2
function __less(V1, V2: PLseValue): PLseValue;       // V1 :=     V1 <    V2
function __eqless(V1, V2: PLseValue): PLseValue;     // V1 :=     V1 <=   V2
function __more(V1, V2: PLseValue): PLseValue;       // V1 :=     V1 >    V2
function __eqmore(V1, V2: PLseValue): PLseValue;     // V1 :=     V1 >=   V2
function __abseq(V1, V2: PLseValue): PLseValue;      // V1 :=     V1 ===  V2
procedure __like(V1, V2: PLseValue; R: KLiRunner);   // V1 :=     V1 like V2
procedure __addAll(V1, V2: PLseValue; R: KLiRunner); // V1 :=     V1 +<   V2
procedure __is(V1, V2: PLseValue);                   // V1 :=     V1 is   V2
procedure __as(V1, V2: PLseValue; E: KLiEngine);     // V1 :=     V1 as   V2

{----------------------------------------------------------------------)
(                                                                      )
(                      mathmetics type test                            )
(                                                                      )
(----------------------------------------------------------------------}
function __type_inc(L, R: KLiClass): KLiClass;
function __type_dec(L, R: KLiClass): KLiClass;
function __type_mul(L, R: KLiClass): KLiClass;
function __type_div(L, R: KLiClass): KLiClass;
function __type_mod(L, R: KLiClass): KLiClass;
function __type_neg(L: KLiClass): KLiClass;

function __type_xor(L, R: KLiClass): KLiClass;
function __type_and(L, R: KLiClass): KLiClass;
function __type_or(L, R: KLiClass): KLiClass;
function __type_shl(L, R: KLiClass): KLiClass;
function __type_shr(L, R: KLiClass): KLiClass;
function __type_not(V: KLiClass): KLiClass;

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
function __contains(Host: KLiStrlist; Value: PLseValue): boolean;overload;
function __contains(Host: KLiVarList; Value: PLseValue; FindItemVG: boolean): boolean;overload;
function __contains(Host: PLseString; Value: PLseValue): boolean;overload;
function __contains(Host: int64; Value: PLseValue): boolean;overload;
function __contains(Host: KLiHashed; Value: PLseValue): boolean;overload;

{----------------------------------------------------------------------)
(                                                                      )
(             以下函数用于对系统内嵌函数、类型和库进行管理             )
(                                                                      )
(----------------------------------------------------------------------)
( F_NAME: __decodeClassName
( 
( F_DESC: 解析类型名称
( 
( F_ARGS: const className: string - 目标类名称
(             var libName: string - 所在库名
( 
( F_TYPE: string - 类型名称
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __decodeClassName(const className: string; var libName: string): string;

{-----------------------------------------------------------------------
( F_NAME: __SetupClasses
( 
( F_DESC: 注册LSE类和类内嵌函数
( 
( F_ARGS:  List: PLseClassList - 类定义结构数组
(         Count: integer         - 数组长度
(         Owner: KLiLibrary    - 属主类列表
( 
( F_TYPE: 注册成功的类和函数总数
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __SetupClasses(List: PLseClassList; Count: integer;
  Owner: KLiModule): integer;

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
(            initrec: PLseModuleRe - 初始化结构
(
( F_TYPE: 注册成功后返回库中类的列表
(
( EXCEPT:
(----------------------------------------------------------------------}
function __SetupModule(const name: string; initrec: PLseModuleRec): KLiModule;

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
procedure __runner_money(Sender: KLiRunner);
procedure __runner_call(Sender: KLiRunner);
procedure __runner_out(Sender: KLiRunner);
procedure __runner_time(Sender: KLiRunner);
procedure __runner_int(Sender: KLiRunner);
procedure __runner_str(Sender: KLiRunner);
procedure __runner_char(Sender: KLiRunner);
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
procedure __runner_abseq(Sender: KLiRunner);
procedure __runner_ne(Sender: KLiRunner);
procedure __runner_lt(Sender: KLiRunner);
procedure __runner_le(Sender: KLiRunner);
procedure __runner_gt(Sender: KLiRunner);
procedure __runner_ge(Sender: KLiRunner);
procedure __runner_in(Sender: KLiRunner);
procedure __runner_and(Sender: KLiRunner);
procedure __runner_or(Sender: KLiRunner);
procedure __runner_true(Sender: KLiRunner);
procedure __runner_false(Sender: KLiRunner);
procedure __runner_type(Sender: KLiRunner);
procedure __runner_func(Sender: KLiRunner);
procedure __runner_varlist(Sender: KLiRunner);
procedure __runner_nil(Sender: KLiRunner);
procedure __runner_shell(Sender: KLiRunner);
procedure __runner_getvalue(Sender: KLiRunner);
procedure __runner_setvalue(Sender: KLiRunner);
procedure __runner_format(Sender: KLiRunner);
procedure __runner_bool(Sender: KLiRunner);
procedure __runner_method(Sender: KLiRunner);
procedure __runner_puts(Sender: KLiRunner);
procedure __runner_module(Sender: KLiRunner);
procedure __runner_is(Sender: KLiRunner);
procedure __runner_as(Sender: KLiRunner);
procedure __runner_statement(Sender: KLiRunner);
procedure __runner_vargen(Sender: KLiRunner);
procedure __runner_pushvarb(Sender: KLiRunner);
procedure __runner_ask(Sender: KLiRunner);
procedure __runner_label(Sender: KLiRunner);
procedure __runner_idle(Sender: KLiRunner);
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
procedure __runner_callask(Sender: KLiRunner);
procedure __runner_upto(Sender: KLiRunner);
procedure __runner_RINR(Sender: KLiRunner);
procedure __runner_SETV(Sender: KLiRunner);
procedure __runner_GETV(Sender: KLiRunner);
procedure __runner_like(Sender: KLiRunner);
procedure __runner_getpv(Sender: KLiRunner);
procedure __runner_setpv(Sender: KLiRunner);
procedure __runner_getiv(Sender: KLiRunner);
procedure __runner_setiv(Sender: KLiRunner);
procedure __runner_clen(Sender: KLiRunner);
procedure __runner_this(Sender: KLiRunner);
procedure __runner_duplast(Sender: KLiRunner);
procedure __runner_addAll(Sender: KLiRunner);

{-----------------------------------------------------------------------
( F_NAME: __log
(
( F_DESC: save log message
(
( F_ARGS: const Msg: string
(
( F_TYPE: string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function __log(const Msg: pchar; Count: integer): pchar;overload;
function __log(const Msg: string): string;overload;
function __log(const ID, Msg: string): string;overload;

const
  ParamRecSize  = sizeof(RLseParam);

  FloatTypes = [LSV_FLOAT, LSV_MONEY];

  SingleMethods = [cmCount, cmGetAt, cmSetAt, cmGetPV, cmSetPV, cmCreator];

  SyntaxError  = 'SyntaxError';
  ImportError  = 'ImportError';
  RuntimeError = 'RuntimeError';
  FileNotFound = 'FileNotFound';

  ESYNTAX               = 1000;
    EsSymNotFound       = 'symbol expected but file end';
    EvSymNotFound       = ESYNTAX + 1;
    EsClassNotExists    = 'data type "%s" not exists';
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
    EsCanNotCast        = 'invalide type casting from %s to %s';
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
  sys_confile      : string;      {<--config file name}
  sys_mimefile     : string;      {<--MIME file name}
  sys_mimes        : KLiStrlist;  {<--MIME list}
  sys_configures   : KLiStrlist;  {<--configure value list}
  sys_tmpath       : string;      {<--temporary path}
  sys_search_path  : string;      {<--module search path}
  sys_program      : string;      {<--program file name}
  sys_process_ID   : string;      {<--process ID}
  sys_libraries    : TStringList; {<--kernel & library module list}
  sys_create_log   : boolean;     {<--create log file}
  sys_log_file     : string;      {<--log file name}
  sys_log_stream   : TStream;     {<--log stream}
  sys_class_list   : RLseKernelClassList;
  sys_module       : KLiModule;   {<--builtin [sys] module}
  cgi_module       : KLiModule;   {<--builtin [cgi] module}
  sys_spinlock     : TLseLock;    {<--kernel's spinlock}
  sys_getpv_func   : KLiFunc;     {<--sys::getpv()}
  sys_setpv_func   : KLiFunc;     {<--sys::setpv()}
  sys_getiv_func   : KLiFunc;     {<--sys::getiv()}
  sys_setiv_func   : KLiFunc;     {<--sys::setiv()}
  sys_getfv_func   : KLiFunc;     {<--sys::getfv()}
  sys_setfv_func   : KLiFunc;     {<--sys::setfv()}
  sys_encodeUTF8   : KLiFunc;     {<--sys::encodeUTF8()}
  sys_decodeUTF8   : KLiFunc;     {<--sys::decodeUTF8()}
  sys_encodeS      : KLiFunc;     {<--sys::encodeS()}
  sys_decodeS      : KLiFunc;     {<--sys::decodeS()}
  sys_vargen_eof   : KLiFunc;     {<--sys::vargen.eof}
  sys_vargen_map   : KLiFunc;     {<--sys::vargen.map}
  sys_vargen_reduce: KLiFunc;     {<--sys::vargen.reduce}
  sys_vargen_filter: KLiFunc;     {<--sys::vargen.filter}
  sys_vargen_each  : KLiFunc;     {<--sys::vargen.each}
  sys_vargen_send  : KLiFunc;     {<--sys::vargen.send}
  sys_vargen_next  : KLiFunc;     {<--sys::vargen.next}
  sys_curryone_func: KLiFunc;     {<--sys::curryone()}
  sys_varlist_getpv: KLiFunc;     {<--sys::varsnap.getpv}
  sys_varlist_setpv: KLiFunc;     {<--sys::varsnap.setpv}
  sys_oper_inc     : KLiFunc;     {<--sys::+}
  sys_void_data    : RLseValue;   {<--default empty data}
  sys_LB           : string = LB;
  sys_pos          : KLiSymPos;
  sys_runner_procs : array[KLiSymbol] of KLiRunnerProc;

  { KT: KERNEL TYPE }

  KT_VOID, KT_STRING, KT_INT, KT_FLOAT, KT_MONEY, KT_TIME, KT_BOOL,
  KT_CHAR, KT_VARIANT, KT_STRLIST, KT_CLASS, KT_MODULE, KT_FUNC,
  KT_VARIABLE, KT_ERROR, KT_STREAM, KT_VARLIST, KT_DB,
  KT_DS, KT_HASHED, KT_VARGEN: KLiClass;

  { KR: KERNEL TYPE RECORD }

  KR_VOID, KR_STRING, KR_INT, KR_FLOAT, KR_MONEY, KR_TIME, KR_BOOL,
  KR_CHAR, KR_VARIANT, KR_STRLIST, KR_CLASS, KR_MODULE, KR_FUNC,
  KR_VARIABLE, KR_ERROR, KR_STREAM, KR_VARLIST, KR_DB,
  KR_DS, KR_HASHED, KR_VARGEN: PLseClassRec;

procedure lock_kernel;
procedure unlock_kernel;

procedure lock_engine(engine: KLiEngine);
procedure unlock_engine(engine: KLiEngine);

implementation

uses
  Math, DateUtils, lse_export, lse_api, lse_spawn, lse_cgi;

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

procedure __ExportAPI(const HTMLFileName: string);
var
  source: KLiExportAPI;
begin
  source := KLiExportAPI.Create;
  try
    source.Execute(HTMLFileName);
  finally
    source.Free;
  end;
end;

procedure __LoadConfig;

  function Read(const Key, Value: string): string;
  begin
    if sys_configures <> nil then
      Result := sys_configures.ReadValue(Key, Value) else
      Result := Value;
  end;

  function Load(const FileName: string): boolean;
  var
    list: KLiStrlist;
    index: integer;
    source, ID, value: string;
  begin
    source := lse_expand_fname(FileName);
    Result := FileExists(source) and not __sameFileName(source, sys_confile);
    if Result then
    begin
      sys_confile := source;
      list := KLiStrlist.Create;
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

begin
  try
    if sys_process_ID = '' then
    begin
      sys_process_ID := __genid;
      sys_program := __programFile;
      sys_kernel := __kernelFile;
      {$IFDEF WINDOWS}
      sys_home_path := __fullPath(lse_getenv('HOMEDRIVER') + lse_getenv('HOMEPATH'));
      {$ELSE}
      sys_home_path := __fullPath(lse_getenv('HOME'));
      {$ENDIF}
      sys_configures := KLiStrlist.Create;
      sys_configures.CaseSensitive := true;
      sys_configures.IncRefcount;
    end;

    sys_knpath := ExtractFilePath(sys_kernel);
    sys_kndir := ExcludeTrailingPathDelimiter(sys_knpath);

    sys_configures.Clear;
    Load(sys_knpath + LSE_CONFILE);

    sys_mimefile := sys_knpath + LSE_MIMEFILE;
    if sys_mimes <> nil then
    begin
      sys_mimes.DecRefcount;
      sys_mimes := nil;
    end;

    sys_tmpath := __fullPath(__ExpandValue(Read('lse_tmpath', LSE_TEMP_PATH), nil));
    sys_search_path := __ExpandValue(Read('lse_search', LSE_SEARCH_PATH), nil);
    sys_create_log := __parseInt(pchar(Read('lse_create_log', '0'))) <> 0;
    
    {$IFDEF WINDOWS}
    ForceDirectories(sys_tmpath);
    {$ENDIF}
  except
    { ignore exceptions }
  end;
end;

function __ReadConfig(const ID: string): string;
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
  if (sys_configures = nil)
    or not sys_configures.Read(ID, Result) then
      Result := lse_getenv(ID);
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

function __NewValue: PLseValue;
begin
  Result := lse_mem_alloc_zero(sizeof(RLseValue));
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
  Param^.count := 0;
  Param^.result := nil;
  Param^.func := Func;
  Param^.runner := Runner;
  Param^.row := Runner.FExprrec^.Pos.row;
  Param^.col := Runner.FExprrec^.Pos.col;
end;

procedure __ExecParam(Param: PLseParam; Func: KLiFunc);
begin
  TLseFuncCall(Func.FProc)(Param);
end;

function __PutInt64(V: PLseValue; Value: int64): PLseValue;
begin
  V^.value_class := KR_INT;
  V^.VInteger := Value;
  Result := V;
end;

function __PutFloat(V: PLseValue; Value: double): PLseValue;
begin
  V^.value_class := KR_FLOAT;
  V^.VFloat := Value;
  Result := V;
end;

function __PutMoney(V: PLseValue; Value: currency): PLseValue;
begin
  V^.value_class := KR_MONEY;
  V^.VMoney := Value;
  Result := V;
end;

function __PutTime(V: PLseValue; Value: TDateTime): PLseValue;
begin
  V^.value_class := KR_TIME;
  V^.VTime := Value;
  Result := V;
end;

function __PutBool(V: PLseValue; Value: boolean): PLseValue;
begin
  V^.value_class := KR_BOOL;
  V^.VBool := Value;
  Result := V;
end;

function __PutChar(V: PLseValue; Value: char): PLseValue;
begin
  V^.value_class := KR_CHAR;
  V^.VChar := Value;
  Result := V;
end;

function __PutString(V: PLseValue; Value: PLseString): PLseValue;
begin
  lse_strec_inclife(Value);
  V^.value_class := KR_STRING;
  V^.VString := Value;
  Result := V;
end;

function __PutString(V: PLseValue; const Value: string): PLseValue;
var
  sr: PLseString;
begin
  sr := lse_strec_alloc(Value); 
  lse_strec_inclife(sr);
  V^.value_class := KR_STRING;
  V^.VString := sr;
  Result := V;
end;

function __PutString(V: PLseValue; const Value: pchar): PLseValue;
var
  sr: PLseString;
begin
  sr := lse_strec_alloc(Value, StrLen(Value));
  lse_strec_inclife(sr);
  V^.value_class := KR_STRING;
  V^.VString := sr;
  Result := V;
end;

function __PutString(V: PLseValue; const Value: pchar; Count: integer): PLseValue;overload;
var
  sr: PLseString;
begin
  sr := lse_strec_alloc(Value, Count);
  lse_strec_inclife(sr);
  V^.value_class := KR_STRING;
  V^.VString := sr;
  Result := V;
end;

function __PutObject(V: PLseValue; Clss: PLseClassRec; Obj: pointer): PLseValue;
begin
  Clss^.incRefcount(Obj);
  V^.value_class := Clss;
  V^.VObject := Obj;
  Result := V;
end;

function __PutDB(V: PLseValue; ADB: PLseDB): PLseValue;
begin
  Result := __PutObject(V, KR_DB, ADB);
end;

function __PutDS(V: PLseValue; ADS: PLseDS): PLseValue;
begin
  Result := __PutObject(V, KR_DS, ADS);
end;

function __PutClass(V: PLseValue; Clss: PLseClassRec): PLseValue;
begin
  Result := __PutObject(V, KR_CLASS, Clss^.lysee_class);
end;

function __PutClass(V: PLseValue; Clss: KLiClass): PLseValue;overload;
begin
  Result := __PutObject(V, KR_CLASS, Clss);
end;

function __PutValue(V, Value: PLseValue): PLseValue;
var
  crec: PLseClassRec;
begin
  if V <> Value then
  begin
    crec := lse_class_rec(Value);
    case crec^.vtype of
      LSV_STRING: lse_strec_inclife(Value^.VString);
      LSV_OBJECT: crec^.incRefcount(Value^.VObject);
    end;
    V^ := Value^;
  end;
  Result := V;
end;

function __SetStrlist(V: PLseValue; Value: KLiStrlist): PLseValue;
begin
  Result := lse_set_object(V, KR_STRLIST, Value);
end;

function __SetVarlist(V: PLseValue; Value: KLiVarList): PLseValue;
begin
  Result := lse_set_object(V, KR_VARLIST, Value);
end;

function __SetHashed(V: PLseValue; Value: KLiHashed): PLseValue;
begin
  Result := lse_set_object(V, KR_HASHED, Value);
end;

function __SetStream(V: PLseValue; Value: PLseStream): PLseValue;
begin
  Result := lse_set_object(V, KR_STREAM, Value);
end;

function __SetFunc(V: PLseValue; Value: KLiFunc): PLseValue;
begin
  Result := lse_set_object(V, KR_FUNC, Value);
end;

function __SetVarb(V: PLseValue; Value: KLiVarb): PLseValue;
begin
  Result := lse_set_object(V, KR_VARIABLE, Value);
end;

function __SetModule(V: PLseValue; Value: KLiModule): PLseValue;
begin
  Result := lse_set_object(V, KR_MODULE, Value);
end;

function __SetVarGen(V: PLseValue; Value: PLseVargen): PLseValue;
begin
  Result := lse_set_object(V, KR_VARGEN, Value);
end;

function __SetObject(V: PLseValue; AClass: KLiClass; AInstance: pointer): PLseValue;
begin
  Result := lse_set_object(V, @AClass.FClassRec, AInstance);
end;

function __SetClass(V: PLseValue; Value: KLiClass): PLseValue;
begin
  Result := lse_set_object(V, KR_CLASS, Value);
end;

function __int64_void(V: PLseValue): int64;
begin
  Result := 0;
end;

function __int64_string(V: PLseValue): int64;
begin
  Result := __parseInt(lse_strec_data(V^.VString));
end;

function __int64_int64(V: PLseValue): int64;
begin
  Result := V^.VInteger;
end;

function __int64_float(V: PLseValue): int64;
begin
  Result := Trunc(V^.VFloat);
end;

function __int64_money(V: PLseValue): int64;
begin
  Result := Trunc(V^.VMoney);
end;

function __int64_time(V: PLseValue): int64;
begin
  Result := DateTimeToUnix(V^.VTime);
end;

function __int64_bool(V: PLseValue): int64;
begin
  Result := Ord(V^.VBool);
end;

function __int64_char(V: PLseValue): int64;
begin
  Result := Ord(V^.VChar);
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
    {$IFDEF FPC}@{$ENDIF}__int64_money,
    {$IFDEF FPC}@{$ENDIF}__int64_time,
    {$IFDEF FPC}@{$ENDIF}__int64_bool,
    {$IFDEF FPC}@{$ENDIF}__int64_char,
    {$IFDEF FPC}@{$ENDIF}__int64_variant,
    {$IFDEF FPC}@{$ENDIF}__int64_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __float_void(V: PLseValue): double;
begin
  Result := 0;
end;

function __float_string(V: PLseValue): double;
begin
  Result := __parseExt(lse_strec_data(V^.VString));
end;

function __float_int64(V: PLseValue): double;
begin
  Result := V^.VInteger;
end;

function __float_float(V: PLseValue): double;
begin
  Result := V^.VFloat;
end;

function __float_money(V: PLseValue): double;
begin
  Result := V^.VMoney;
end;

function __float_time(V: PLseValue): double;
begin
  Result := V^.VTime;
end;

function __float_bool(V: PLseValue): double;
begin
  Result := Ord(V^.VBool);
end;

function __float_char(V: PLseValue): double;
begin
  Result := Ord(V^.VChar);
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
    {$IFDEF FPC}@{$ENDIF}__float_money,
    {$IFDEF FPC}@{$ENDIF}__float_time,
    {$IFDEF FPC}@{$ENDIF}__float_bool,
    {$IFDEF FPC}@{$ENDIF}__float_char,
    {$IFDEF FPC}@{$ENDIF}__float_variant,
    {$IFDEF FPC}@{$ENDIF}__float_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __money_void(V: PLseValue): currency;
begin
  Result := 0;
end;

function __money_string(V: PLseValue): currency;
begin
  Result := __parseExt(lse_strec_data(V^.VString));
end;

function __money_int64(V: PLseValue): currency;
begin
  Result := V^.VInteger;
end;

function __money_float(V: PLseValue): currency;
begin
  Result := V^.VFloat;
end;

function __money_money(V: PLseValue): currency;
begin
  Result := V^.VMoney;
end;

function __money_time(V: PLseValue): currency;
begin
  Result := 0;
end;

function __money_bool(V: PLseValue): currency;
begin
  Result := Ord(V^.VBool);
end;

function __money_char(V: PLseValue): currency;
begin
  Result := 0;
end;

function __money_variant(V: PLseValue): currency;
begin
  Result := 0;
end;

function __money_object(V: PLseValue): currency;
begin
  Result := 0;
end;

function __AsMoney(V: PLseValue): currency;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): currency = (
    {$IFDEF FPC}@{$ENDIF}__money_void,
    {$IFDEF FPC}@{$ENDIF}__money_string,
    {$IFDEF FPC}@{$ENDIF}__money_int64,
    {$IFDEF FPC}@{$ENDIF}__money_float,
    {$IFDEF FPC}@{$ENDIF}__money_money,
    {$IFDEF FPC}@{$ENDIF}__money_time,
    {$IFDEF FPC}@{$ENDIF}__money_bool,
    {$IFDEF FPC}@{$ENDIF}__money_char,
    {$IFDEF FPC}@{$ENDIF}__money_variant,
    {$IFDEF FPC}@{$ENDIF}__money_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsTime(V: PLseValue): TDateTime;
begin
  case lse_vtype(V) of
    LSV_STRING: Result := lse_decode_GMT(__AsString(V));
    LSV_FLOAT : Result := V^.VFloat;
    LSV_TIME  : Result := V^.VTime;
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
  Result := lse_strec_string(V^.VString);
end;

function __string_int64(V: PLseValue): string;
begin
  Result := IntToStr(V^.VInteger);
end;

function __string_float(V: PLseValue): string;
begin
  Result := FloatToStr(V^.VFloat);
end;

function __string_money(V: PLseValue): string;
begin
  Result := CurrToStr(V^.VMoney);
end;

function __string_time(V: PLseValue): string;
begin
  Result := lse_encode_GMT(V^.VTime);
end;

function __string_bool(V: PLseValue): string;
begin
  Result := IntToStr(Ord(V^.VBool));
end;

function __string_char(V: PLseValue): string;
begin
  Result := V^.VChar;
end;

function __string_variant(V: PLseValue): string;
begin
  Result := '';
end;

function __string_object(V: PLseValue): string;
begin
  Result := __AsClass(V).ObjectToString(V^.VObject);
end;

function __AsString(V: PLseValue): string;
const
  OPRS : array[LSV_VOID..LSV_OBJECT] of function(V: PLseValue): string = (
    {$IFDEF FPC}@{$ENDIF}__string_void,
    {$IFDEF FPC}@{$ENDIF}__string_string,
    {$IFDEF FPC}@{$ENDIF}__string_int64,
    {$IFDEF FPC}@{$ENDIF}__string_float,
    {$IFDEF FPC}@{$ENDIF}__string_money,
    {$IFDEF FPC}@{$ENDIF}__string_time,
    {$IFDEF FPC}@{$ENDIF}__string_bool,
    {$IFDEF FPC}@{$ENDIF}__string_char,
    {$IFDEF FPC}@{$ENDIF}__string_variant,
    {$IFDEF FPC}@{$ENDIF}__string_object
  );
begin
  Result := OPRS[lse_vtype(V)](V);
end;

function __AsFileName(V: PLseValue): string;
var
  VT: TLseValue;
begin
  VT := lse_vtype(V);
  if VT = LSV_STRING then
    Result := lse_veryPD(Trim(lse_strec_data(V^.VString))) else
  if VT = LSV_CHAR then
    Result := lse_veryPD(Trim(string(V^.VChar))) else
    Result := '';
end;

function __AsStrec(V: PLseValue): PLseString;
begin
  if lse_vtype(V) = LSV_STRING then
    Result := V^.VString else
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
  data := lse_strec_data(V^.VString);
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

function __char_money(V: PLseValue): char;
begin
  Result := char(Trunc(V^.VMoney));
end;

function __char_time(V: PLseValue): char;
begin
  Result := #0;
end;

function __char_bool(V: PLseValue): char;
begin
  Result := char(Ord(V^.VBool));
end;

function __char_char(V: PLseValue): char;
begin
  Result := V^.VChar;
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
    {$IFDEF FPC}@{$ENDIF}__char_money,
    {$IFDEF FPC}@{$ENDIF}__char_time,
    {$IFDEF FPC}@{$ENDIF}__char_bool,
    {$IFDEF FPC}@{$ENDIF}__char_char,
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
  Result := (lse_strec_length(V^.VString) > 0);
end;

function __bool_int64(V: PLseValue): boolean;
begin
  Result := (V^.VInteger <> 0);
end;

function __bool_float(V: PLseValue): boolean;
begin
  Result := not IsZero(V^.VFloat);
end;

function __bool_money(V: PLseValue): boolean;
begin
  Result := (V^.VMoney <> 0);
end;

function __bool_time(V: PLseValue): boolean;
begin
  Result := not IsZero(V^.VTime);
end;

function __bool_bool(V: PLseValue): boolean;
begin
  Result := V^.VBool;
end;

function __bool_char(V: PLseValue): boolean;
begin
  Result := (V^.VChar <> #0);
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
    {$IFDEF FPC}@{$ENDIF}__bool_money,
    {$IFDEF FPC}@{$ENDIF}__bool_time,
    {$IFDEF FPC}@{$ENDIF}__bool_bool,
    {$IFDEF FPC}@{$ENDIF}__bool_char,
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

function __AsObject(V: PLseValue; T: KLiClass): pointer;
var
  R: PLseClassRec;
begin
  R := lse_class_rec(V);
  if (R^.vtype = LSV_OBJECT) and (KLiClass(R^.lysee_class) = T) then
    Result := V^.VObject else
    Result := nil;
end;

function __AsRunner(Param: PLseParam): KLiRunner;
begin
  Result := KLiRunner(Param^.runner);
end;

function __AsEngine(Param: PLseParam): KLiEngine;
begin
  Result := KLiRunner(Param^.runner).FEngine;
end;

function __AsFunc(V: PLseValue): KLiFunc;
begin
  Result := KLiFunc(__AsObject(V));
end;

function __AsStrlist(V: PLseValue): KLiStrList;
begin
  Result := KLiStrList(__AsObject(V));
end;

function __AsVarlist(V: PLseValue): KLiVarList;
begin
  Result := KLiVarList(__AsObject(V));
end;

function __AsVargen(Engine: KLiEngine; data: PLseValue): PLseVargen;
var
  crec: PLseClassRec;
  clss: KLiClass;
  varg: PLseVargen;
  list: pointer;
begin
  varg := nil;
  crec := lse_class_rec(data);
  clss := KLiClass(crec^.lysee_class);
  if clss = KT_VARGEN then
    varg := PLseVargen(data^.VObject) else
  if clss = KT_STRING then
    varg := cvgr_string(data^.VString, Engine) else
  if clss = KT_INT then
    varg := cvgr_upto(0, data^.VInteger - 1, 1, Engine) else
  if clss = KT_FLOAT then
    varg := cvgr_upto(0, Trunc(data^.VFloat) - 1, 1, Engine) else
  if clss = KT_MONEY then
    varg := cvgr_upto(0, Trunc(data^.VMoney) - 1, 1, Engine) else
  if crec^.vtype = LSV_OBJECT then
  begin
    list := data^.VObject;
    if list <> nil then
      if Assigned(crec^.toVargen) then
        varg := crec^.toVargen(list, Engine) else
        varg := cvgr_anylist(list, crec, Engine);
  end;
  if varg = nil then
    Result := lse_vargen_none else
    Result := varg;
end;

function __AsClass(V: PLseValue): KLiClass;
begin
  Result := KLiClass(lse_class(V));
end;

function __NewVarlist(Engine: KLiEngine): KLiVarList;
begin
  Result := KLiVarList.Create(Engine);
end;

function __GetThis(Param: PLseParam; var This): boolean;
var
  this_obj: pointer;
begin
  this_obj := __AsObject(Param^.param[0]);
  Result := (this_obj <> nil);
  if Result then
    pointer(This) := this_obj else
    __SetErrorThis(Param);
end;

function __IsClassRec(R: PLseClassRec): boolean;
begin
  Result := (R^.vtype = LSV_OBJECT) and
            Assigned(R^.incRefcount) and
            Assigned(R^.decRefcount) and
            __IsIDStr(R^.name);
end;

function __FormatParam(Param: PLseParam; Index: integer): string;
begin
  Result := KLiRunner(Param^.runner).FormatFor(__AsString(Param^.param[Index]), nil);
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
  rnnr := KLiRunner(Param^.runner);
  func := KLiFunc_curry(Param^.func);
  N := func.GetCurryCount;
  for X := 0 to N - 1 do
    rnnr.FStack.Push(func.GetCurryData(X));
  for X := 0 to Param^.count - 1 do
    rnnr.FStack.Push(Param^.param[X]);
  rnnr.Goon(func.FCurryFunc, N + Param^.count, Param^.result);
end;
         
procedure udc_const(const Param: PLseParam);cdecl;
var
  func: KLiFunc_curry;
  data: PLseValue;
begin
  func := KLiFunc_curry(Param^.func);
  data := func.FCurry[0];
  if func.HasState(fusHasConstValue) then
    lse_set_value(Param^.result, data) else
    begin
      __SetDefaultValue(data, func.FResultType);
      func.SetState(fusHasConstValue, true);
      KLiRunner(Param^.runner).GoonConst(Param);
      lse_set_value(data, Param^.result);
    end;
end;

procedure udc_create(const Param: PLseParam);cdecl;
var
  clss: KLiClass;
  hash: KLiHashed;
begin
  clss := __AsClass(Param^.param[0]);
  hash := KLiHashed.Create(__AsEngine(Param), 1);
  __SetObject(Param^.result, clss, hash);
  if clss.FInitFunc <> nil then
  begin
    __SetObject(Param^.param[0], clss, hash);
    __AsRunner(Param).Goon(clss.FInitFunc, clss.FInitFunc.ParamCount, nil);
  end;
end;

procedure udc_getpv(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  name: string;
  data: PLseValue;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    data := this.FindValue(name);
    if data = nil then
      __SetError(Param, 'property "%s" not found', [name]) else
      __PutValue(Param^.result, data);
  end;
end;

procedure udc_setpv(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  name: string;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    this.SetValue(name, Param^.param[2]);
  end;
end;

procedure udc_oper(const Param: PLseParam);cdecl;
var
  func: KLiFunc_operator;
  rnnr: KLiRunner;
  L, R: PLseValue;
begin
  L := __PutValue(Param^.result, Param^.param[0]);
  R := Param^.param[1];
  rnnr := KLiRunner(Param^.runner);
  func := KLiFunc_operator(Param^.func);
  case func.FOper of
    syMul   : __mul(L, R);
    syDiv   : __div(L, R);
    syMod   : __mod(L, R);
    syAdd   : __inc(L, R);
    syDec   : __dec(L, R);
    syBXor  : __xor(L, R);
    syBAnd  : __and(L, R);
    syBOr   : __or(L, R);
    syBShl  : __shl(L, R, rnnr.FEngine);
    syBShr  : __shr(L, R);
    syAddAll: __addAll(L, R, rnnr);
    syEQ    : __equal(L, R);
    syNE    : __diff(L, R);
    syLT    : __less(L, R);
    syLE    : __eqless(L, R);
    syGT    : __more(L, R);
    syGE    : __eqmore(L, R);
    syAbsEQ : __abseq(L, R);
    syIs    : __is(L, R);
    syAs    : __as(L, R, rnnr.FEngine);
    syLike  : __like(L, R, rnnr);
    syAnd   : __logicAnd(L, R);
    syOr    : __logicOr(L, R);
  end;
end;

function __SetClassValue(E: KLiEngine; V: PLseValue; AClass: KLiClass): PLseValue;
var
  vtype: KLiClass;
  vgrec: PLseVargen;
begin
  Result := V;
  if AClass = KT_VARIANT then Exit;
  
  if AClass = KT_VOID then
  begin
    lse_clear_value(V);
    Exit;
  end;
  
  if AClass = KT_VARGEN then
  begin
    __SetVargen(V, lse_vargen_ensure(__AsVargen(E, V)));
    Exit;
  end;

  vtype := __AsClass(V);
  
  if AClass = KT_CLASS then
  begin
    if vtype <> KT_CLASS then
      __SetClass(V, vtype);
    Exit;
  end;

  if vtype = KT_VARGEN then
  begin
    vgrec := PLseVargen(V^.VObject);
    if vgrec <> nil then
      lse_vargen_send(vgrec, V) else
      lse_clear_value(V);
    vtype := __AsClass(V);
  end;

  if vtype = AClass then Exit;

  if AClass = KT_STRING then lse_set_string(V, __AsString(V)) else
  if AClass = KT_INT    then lse_set_int64(V, __AsInt64(V)) else
  if AClass = KT_FLOAT  then lse_set_float(V, __AsFloat(V)) else
  if AClass = KT_MONEY  then lse_set_money(V, __AsMoney(V)) else
  if AClass = KT_TIME   then lse_set_time(V, __AsTime(V)) else
  if AClass = KT_BOOL   then lse_set_bool(V, __AsBool(V)) else
  if AClass = KT_CHAR   then lse_set_char(V, __AsChar(V)) else
  if vtype = KT_STRING then
    __SetObject(V, AClass, AClass.StrecToObject(V^.VString, E)) else
  if (AClass = KT_HASHED) and vtype.IsHashed then
    V^.value_class := KR_HASHED else
    __SetObject(V, AClass, nil);
end;

function __SetDefaultValue(V: PLseValue; AClass: KLiClass): PLseValue;
begin
  case AClass.DataType of
    LSV_VOID   : lse_clear_value(V);
    LSV_STRING : lse_set_string(V, '');
    LSV_INT    : lse_set_int64(V, 0);
    LSV_FLOAT  : lse_set_float(V, 0);
    LSV_MONEY  : lse_set_money(V, 0);
    LSV_TIME   : lse_set_time(V, 0);
    LSV_BOOL   : lse_set_bool(V, false);
    LSV_CHAR   : lse_set_char(V, #0);
    LSV_VARIANT: lse_clear_value(V);
    LSV_OBJECT : __SetObject(V, AClass, nil);
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
  runner := KLiRunner(Param^.runner);
  if not runner.FExcepted then
  begin
    func := KLiFunc(Param^.func);
    errid := Trim(ErrorName);
    if not __IsIDStr(pchar(errid)) then
      errid := func.FOwnerClass.FName + 'Error';
    if Errno = 0 then
      Errno := ERUNTIME;
    error := Trim(ErrorMsg);
    if error = '' then
      error := lse_exception_str;
    if error = '' then
      error := func.Name + '() - ' + errid else
      error := func.Name + '() - ' + error;
    module := runner.CurrentFunc.Module;
    runner.Engine.Error.write(errid, Errno, Param^.row, Param^.col,
      module.Name, error, runner.IncludedFile);
    runner.FExcepted := true;
//  __PutString(Param^.result, runner.Engine.Error.ErrorText);
  end;
end;

procedure __SetErrorThis(Param: PLseParam);
begin
  __SetError(Param, 'this is nil');
end;

// __inc() ---------------------------------------------------------------------

procedure __inc_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __inc_string(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R <> LSV_VOID then
  begin
    if R = LSV_STRING then
      lse_set_string(V, lse_strec_cat(V^.VString, Value^.VString)) else
      lse_set_string(V, lse_strec_cat(V^.VString, __AsString(Value)));
  end
  else lse_clear_value(V);
end;

procedure __inc_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);

  if R in [LSV_FLOAT, LSV_MONEY] then
  begin
    lse_set_float(V, __AsFloat(V) + __AsFloat(Value));
    Exit;
  end;

  if R in [LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_int64(V, V^.VInteger + __AsInt64(Value));
    Exit;
  end;

  if R = LSV_STRING then
  begin
    lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VString));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __inc_float(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);

  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_float(V, V^.VFloat + __AsFloat(Value));
    Exit;
  end;

  if R = LSV_STRING then
  begin
    lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VString));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __inc_money(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_money(V, V^.VMoney + __AsMoney(Value));
    Exit;
  end;

  if R = LSV_STRING then
  begin
    lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VString));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __inc_time(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  
  if R = LSV_INT then
  begin
    lse_set_time(V, V^.VTime + Value^.VInteger);
    Exit;
  end;

  if R = LSV_FLOAT then
  begin
    lse_set_time(V, V^.VTime + Value^.VFloat);
    Exit;
  end;

  if R = LSV_STRING then
  begin
    lse_set_string(V, lse_strec_cat(__AsString(V), Value^.VString));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __inc_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __inc_int(V, Value);
end;

procedure __inc_char(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);

  if R = LSV_STRING then
  begin
    lse_set_string(V, lse_strec_cat(V^.VChar, Value^.VString));
    Exit;
  end;

  if R = LSV_CHAR then
  begin
    lse_set_string(V, V^.VChar + Value^.VChar);
    Exit;
  end;

  if R in [LSV_FLOAT, LSV_MONEY] then
  begin
    lse_set_float(V, Ord(V^.VChar) + __AsFloat(Value));
    Exit;
  end;

  if R in [LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_int64(V, Ord(V^.VChar) + __AsInt64(Value));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __inc_variant(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __inc_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __inc(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__inc_void,
    {$IFDEF FPC}@{$ENDIF}__inc_string,
    {$IFDEF FPC}@{$ENDIF}__inc_int,
    {$IFDEF FPC}@{$ENDIF}__inc_float,
    {$IFDEF FPC}@{$ENDIF}__inc_money,
    {$IFDEF FPC}@{$ENDIF}__inc_time,
    {$IFDEF FPC}@{$ENDIF}__inc_bool,
    {$IFDEF FPC}@{$ENDIF}__inc_char,
    {$IFDEF FPC}@{$ENDIF}__inc_variant,
    {$IFDEF FPC}@{$ENDIF}__inc_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __dec() ---------------------------------------------------------------------

procedure __dec_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __dec_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __dec_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);

  if R in [LSV_FLOAT, LSV_MONEY] then
  begin
    lse_set_float(V, V^.VInteger - __AsFloat(Value));
    Exit;
  end;

  if R in [LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_int64(V, V^.VInteger - __AsInt64(Value));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __dec_float(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_float(V, V^.VFloat - __AsFloat(Value)) else
    lse_init_value(V);
end;

procedure __dec_money(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_money(V, V^.VMoney - __AsMoney(Value)) else
    lse_init_value(V);
end;

procedure __dec_time(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  
  if R = LSV_INT then
  begin
    lse_set_time(V, V^.VTime - Value^.VInteger);
    Exit;
  end;

  if R = LSV_FLOAT then
  begin
    lse_set_time(V, V^.VTime - Value^.VFloat);
    Exit;
  end;

  lse_init_value(V);
end;

procedure __dec_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __dec_int(V, Value);
end;

procedure __dec_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __dec_int(V, Value);
end;

procedure __dec_variant(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __dec_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __dec(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__dec_void,
    {$IFDEF FPC}@{$ENDIF}__dec_string,
    {$IFDEF FPC}@{$ENDIF}__dec_int,
    {$IFDEF FPC}@{$ENDIF}__dec_float,
    {$IFDEF FPC}@{$ENDIF}__dec_money,
    {$IFDEF FPC}@{$ENDIF}__dec_time,
    {$IFDEF FPC}@{$ENDIF}__dec_bool,
    {$IFDEF FPC}@{$ENDIF}__dec_char,
    {$IFDEF FPC}@{$ENDIF}__dec_variant,
    {$IFDEF FPC}@{$ENDIF}__dec_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __mul() ---------------------------------------------------------------------

procedure __mul_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mul_string(V, Value: PLseValue);
var
  R: TLseValue;
  S, T: PLseString;
  B, F: pchar;
  L, N: integer;
begin
  R := lse_vtype(Value);

  if R = LSV_INT then
  begin
    S := V^.VString;
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
    Exit;
  end;

  lse_clear_value(V);
end;

procedure __mul_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);

  if R in [LSV_FLOAT, LSV_MONEY] then
  begin
    lse_set_float(V, V^.VInteger * __AsFloat(Value));
    Exit;
  end;

  if R in [LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    lse_set_int64(V, V^.VInteger * __AsInt64(Value));
    Exit;
  end;

  lse_init_value(V);
end;

procedure __mul_float(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_float(V, V^.VFloat * __AsFloat(Value)) else
    lse_init_value(V);
end;

procedure __mul_money(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_money(V, V^.VMoney * __AsMoney(Value)) else
    lse_init_value(V);
end;

procedure __mul_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mul_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __mul_int(V, Value);
end;

procedure __mul_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __mul_int(V, Value);
end;

procedure __mul_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mul_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __mul(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__mul_void,
    {$IFDEF FPC}@{$ENDIF}__mul_string,
    {$IFDEF FPC}@{$ENDIF}__mul_int,
    {$IFDEF FPC}@{$ENDIF}__mul_float,
    {$IFDEF FPC}@{$ENDIF}__mul_money,
    {$IFDEF FPC}@{$ENDIF}__mul_time,
    {$IFDEF FPC}@{$ENDIF}__mul_bool,
    {$IFDEF FPC}@{$ENDIF}__mul_char,
    {$IFDEF FPC}@{$ENDIF}__mul_variant,
    {$IFDEF FPC}@{$ENDIF}__mul_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __div() ---------------------------------------------------------------------

procedure __div_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __div_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __div_int(V, Value: PLseValue);
var
  R: TLseValue;
  F: double;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    F := __AsFloat(Value);
    if IsZero(F) then
      lse_set_float(V, 0) else
      lse_set_float(V, V^.VInteger / F);
  end
  else lse_init_value(V);
end;

procedure __div_float(V, Value: PLseValue);
var
  R: TLseValue;
  F: double;
begin
  R := lse_vtype(Value);

  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    F := __AsFloat(Value);
    if IsZero(F) then
      lse_set_float(V, 0) else
      lse_set_float(V, V^.VFloat / F);
  end
  else lse_init_value(V);
end;

procedure __div_money(V, Value: PLseValue);
var
  R: TLseValue;
  F: currency;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    F := __AsMoney(Value);
    if F = 0 then
      lse_set_money(V, 0) else
      lse_set_money(V, V^.VMoney / F);
  end
  else lse_init_value(V);
end;

procedure __div_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __div_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __div_int(V, Value);
end;

procedure __div_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __div_int(V, Value);
end;

procedure __div_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __div_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __div(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__div_void,
    {$IFDEF FPC}@{$ENDIF}__div_string,
    {$IFDEF FPC}@{$ENDIF}__div_int,
    {$IFDEF FPC}@{$ENDIF}__div_float,
    {$IFDEF FPC}@{$ENDIF}__div_money,
    {$IFDEF FPC}@{$ENDIF}__div_time,
    {$IFDEF FPC}@{$ENDIF}__div_bool,
    {$IFDEF FPC}@{$ENDIF}__div_char,
    {$IFDEF FPC}@{$ENDIF}__div_variant,
    {$IFDEF FPC}@{$ENDIF}__div_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __mod() ---------------------------------------------------------------------

procedure __mod_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mod_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __mod_int(V, Value: PLseValue);
var
  R: TLseValue;
  F: int64;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
  begin
    F := __AsInt64(Value);
    if F = 0 then
      lse_set_int64(V, 0) else
      lse_set_int64(V, V^.VInteger mod F);
  end
  else lse_init_value(V);
end;

procedure __mod_float(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __mod_int(V, Value);
end;

procedure __mod_money(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __mod_int(V, Value);
end;

procedure __mod_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mod_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __mod_int(V, Value);
end;

procedure __mod_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __mod_int(V, Value);
end;

procedure __mod_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __mod_object(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

function __mod(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__mod_void,
    {$IFDEF FPC}@{$ENDIF}__mod_string,
    {$IFDEF FPC}@{$ENDIF}__mod_int,
    {$IFDEF FPC}@{$ENDIF}__mod_float,
    {$IFDEF FPC}@{$ENDIF}__mod_money,
    {$IFDEF FPC}@{$ENDIF}__mod_time,
    {$IFDEF FPC}@{$ENDIF}__mod_bool,
    {$IFDEF FPC}@{$ENDIF}__mod_char,
    {$IFDEF FPC}@{$ENDIF}__mod_variant,
    {$IFDEF FPC}@{$ENDIF}__mod_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __neg() ---------------------------------------------------------------------

function __neg(V: PLseValue): PLseValue;
begin
  Result := V;
  case lse_vtype(V) of
    LSV_INT  : V^.VInteger := - V^.VInteger;
    LSV_FLOAT: V^.VFloat := - V^.VFloat;
    LSV_MONEY: V^.VMoney := - V^.VMoney;
    LSV_BOOL : begin
                 V^.value_class := KR_INT;
                 V^.VInteger := - Ord(V^.VBool);
               end;
    LSV_CHAR : begin
                 V^.value_class := KR_INT;
                 V^.VInteger := - Ord(V^.VChar);
               end;
    else lse_init_value(V);
  end;
end;

// __xor() ---------------------------------------------------------------------

procedure __xor_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __xor_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __xor_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_int64(V, V^.VInteger xor __AsInt64(Value)) else
    lse_init_value(V);
end;

procedure __xor_float(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __xor_int(V, Value);
end;

procedure __xor_money(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __xor_int(V, Value);
end;

procedure __xor_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __xor_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __xor_int(V, Value);
end;

procedure __xor_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __xor_int(V, Value);
end;

procedure __xor_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __xor_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __xor(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__xor_void,
    {$IFDEF FPC}@{$ENDIF}__xor_string,
    {$IFDEF FPC}@{$ENDIF}__xor_int,
    {$IFDEF FPC}@{$ENDIF}__xor_float,
    {$IFDEF FPC}@{$ENDIF}__xor_money,
    {$IFDEF FPC}@{$ENDIF}__xor_time,
    {$IFDEF FPC}@{$ENDIF}__xor_bool,
    {$IFDEF FPC}@{$ENDIF}__xor_char,
    {$IFDEF FPC}@{$ENDIF}__xor_variant,
    {$IFDEF FPC}@{$ENDIF}__xor_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __and() ---------------------------------------------------------------------

procedure __and_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __and_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __and_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT,LSV_MONEY,LSV_INT,LSV_CHAR,LSV_BOOL] then
    lse_set_int64(V, V^.VInteger and __AsInt64(Value)) else
    lse_init_value(V);
end;

procedure __and_float(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __and_int(V, Value);
end;

procedure __and_money(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __and_int(V, Value);
end;

procedure __and_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __and_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __and_int(V, Value);
end;

procedure __and_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __and_int(V, Value);
end;

procedure __and_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __and_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __and(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__and_void,
    {$IFDEF FPC}@{$ENDIF}__and_string,
    {$IFDEF FPC}@{$ENDIF}__and_int,
    {$IFDEF FPC}@{$ENDIF}__and_float,
    {$IFDEF FPC}@{$ENDIF}__and_money,
    {$IFDEF FPC}@{$ENDIF}__and_time,
    {$IFDEF FPC}@{$ENDIF}__and_bool,
    {$IFDEF FPC}@{$ENDIF}__and_char,
    {$IFDEF FPC}@{$ENDIF}__and_variant,
    {$IFDEF FPC}@{$ENDIF}__and_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __or() ---------------------------------------------------------------------

procedure __or_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __or_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __or_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT,LSV_MONEY,LSV_INT,LSV_CHAR,LSV_BOOL] then
    lse_set_int64(V, V^.VInteger or __AsInt64(Value)) else
    lse_init_value(V);
end;

procedure __or_float(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __or_int(V, Value);
end;

procedure __or_money(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __or_int(V, Value);
end;

procedure __or_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __or_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __or_int(V, Value);
end;

procedure __or_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __or_int(V, Value);
end;

procedure __or_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __or_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __or(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__or_void,
    {$IFDEF FPC}@{$ENDIF}__or_string,
    {$IFDEF FPC}@{$ENDIF}__or_int,
    {$IFDEF FPC}@{$ENDIF}__or_float,
    {$IFDEF FPC}@{$ENDIF}__or_money,
    {$IFDEF FPC}@{$ENDIF}__or_time,
    {$IFDEF FPC}@{$ENDIF}__or_bool,
    {$IFDEF FPC}@{$ENDIF}__or_char,
    {$IFDEF FPC}@{$ENDIF}__or_variant,
    {$IFDEF FPC}@{$ENDIF}__or_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __shl() ---------------------------------------------------------------------

procedure __shl_void(V, Value: PLseValue; E: KLiEngine);
begin
  lse_init_value(V);
end;

procedure __shl_string(V, Value: PLseValue; E: KLiEngine);
begin
  lse_clear_value(V);
end;

procedure __shl_int(V, Value: PLseValue; E: KLiEngine);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_int64(V, V^.VInteger shl __AsInt64(Value)) else
    lse_init_value(V);
end;

procedure __shl_float(V, Value: PLseValue; E: KLiEngine);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __shl_int(V, Value, E);
end;

procedure __shl_money(V, Value: PLseValue; E: KLiEngine);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __shl_int(V, Value, E);
end;

procedure __shl_time(V, Value: PLseValue; E: KLiEngine);
begin
  lse_init_value(V);
end;

procedure __shl_bool(V, Value: PLseValue; E: KLiEngine);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __shl_int(V, Value, E);
end;

procedure __shl_char(V, Value: PLseValue; E: KLiEngine);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __shl_int(V, Value, E);
end;

procedure __shl_variant(V, Value: PLseValue; E: KLiEngine);
begin
  lse_init_value(V);
end;

procedure __shl_object(V, Value: PLseValue; E: KLiEngine);
begin
  if (V^.VObject <> nil) and Assigned(V^.value_class^.addItem) then
    V^.value_class^.addItem(V^.VObject, Value, E);
end;

function __shl(V, Value: PLseValue; E: KLiEngine): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue; E: KLiEngine) = (
    {$IFDEF FPC}@{$ENDIF}__shl_void,
    {$IFDEF FPC}@{$ENDIF}__shl_string,
    {$IFDEF FPC}@{$ENDIF}__shl_int,
    {$IFDEF FPC}@{$ENDIF}__shl_float,
    {$IFDEF FPC}@{$ENDIF}__shl_money,
    {$IFDEF FPC}@{$ENDIF}__shl_time,
    {$IFDEF FPC}@{$ENDIF}__shl_bool,
    {$IFDEF FPC}@{$ENDIF}__shl_char,
    {$IFDEF FPC}@{$ENDIF}__shl_variant,
    {$IFDEF FPC}@{$ENDIF}__shl_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value, E);
end;

// __shr() ---------------------------------------------------------------------

procedure __shr_void(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __shr_string(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

procedure __shr_int(V, Value: PLseValue);
var
  R: TLseValue;
begin
  R := lse_vtype(Value);
  if R in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_int64(V, V^.VInteger shr __AsInt64(Value)) else
    lse_clear_value(V);
end;

procedure __shr_float(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VFloat);
  __shr_int(V, Value);
end;

procedure __shr_money(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Trunc(V^.VMoney);
  __shr_int(V, Value);
end;

procedure __shr_time(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __shr_bool(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VBool);
  __shr_int(V, Value);
end;

procedure __shr_char(V, Value: PLseValue);
begin
  V^.value_class := KR_INT;
  V^.VInteger := Ord(V^.VChar);
  __shr_int(V, Value);
end;

procedure __shr_variant(V, Value: PLseValue);
begin
  lse_init_value(V);
end;

procedure __shr_object(V, Value: PLseValue);
begin
  lse_clear_value(V);
end;

function __shr(V, Value: PLseValue): PLseValue;
const
  OPRS : array[TLseValue] of procedure(V, Value: PLseValue) = (
    {$IFDEF FPC}@{$ENDIF}__shr_void,
    {$IFDEF FPC}@{$ENDIF}__shr_string,
    {$IFDEF FPC}@{$ENDIF}__shr_int,
    {$IFDEF FPC}@{$ENDIF}__shr_float,
    {$IFDEF FPC}@{$ENDIF}__shr_money,
    {$IFDEF FPC}@{$ENDIF}__shr_time,
    {$IFDEF FPC}@{$ENDIF}__shr_bool,
    {$IFDEF FPC}@{$ENDIF}__shr_char,
    {$IFDEF FPC}@{$ENDIF}__shr_variant,
    {$IFDEF FPC}@{$ENDIF}__shr_object
  );
begin
  Result := V;
  OPRS[lse_vtype(V)](V, Value);
end;

// __not() ---------------------------------------------------------------------

function __not(V: PLseValue): PLseValue;
begin
  Result := V;
  if lse_vtype(V) in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    lse_set_int64(V, __AsInt64(V) xor $FFFFFFFFFFFFFFFF) else
    lse_clear_value(V);
end;

// __logicAnd() ----------------------------------------------------------------

function __logicAnd(V, Value: PLseValue): PLseValue;
begin
  Result := V;
  if __AsBool(V) and not __AsBool(Value) then
    lse_set_value(V, Value);
end;

// __logicOr() -----------------------------------------------------------------

function __logicOr(V, Value: PLseValue): PLseValue;
begin
  Result := V;
  if not __AsBool(V) and __AsBool(Value) then
    lse_set_value(V, Value);
end;

// __logicNot() ----------------------------------------------------------------

function __logicNot(V: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V, not __AsBool(V));
end;

// __equal() -------------------------------------------------------------------

function __equal(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crEqual]));
end;

function __diff(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crLess, crMore, crDiff]));
end;

function __less(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crLess]));
end;

function __eqless(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crLess, crEqual]));
end;

function __more(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crMore]));
end;

function __eqmore(V1, V2: PLseValue): PLseValue;
begin
  Result := lse_set_bool(V1, __compare(V1, V2, [crMore, crEqual]));
end;

function __abseq(V1, V2: PLseValue): PLseValue;
var
  clss: KLiClass;
  same: boolean;
begin
  same := false;
  clss := __AsClass(V1);
  if clss = __AsClass(V2) then
    case clss.ClassRec^.vtype of
      LSV_VOID   : same := true;
      LSV_STRING : same := (V1^.VString = V2^.VString)
                        or __bufSame(lse_strec_data(V1^.VString),
                                     lse_strec_length(V1^.VString),
                                     lse_strec_data(V2^.VString),
                                     lse_strec_length(V2^.VString),
                                     false);
      LSV_INT    : same := (V1^.VInteger = V2^.VInteger);
      LSV_FLOAT  : same := IsZero(V1^.VFloat - V2^.VFloat);
      LSV_MONEY  : same := (V1^.VMoney = V2^.VMoney);
      LSV_TIME   : same := IsZero(V1^.VTime - V2^.VTime);
      LSV_BOOL   : same := (V1^.VBool = V2^.VBool);
      LSV_CHAR   : same := (V1^.VChar = V2^.VChar);
      LSV_VARIANT: same := true;
      LSV_OBJECT : same := (V1^.VObject = V2^.VObject);
    end;
  Result := lse_set_bool(V1, same);
end;

procedure __like(V1, V2: PLseValue; R: KLiRunner);
begin
  lse_set_bool(V1, init_patten(@R.FMatchPatten, V2) and
                   exec_patten(@R.FMatchPatten, V1));
end;

procedure __addAll(V1, V2: PLseValue; R: KLiRunner);
var
  K: PLseClassRec;
  G: PLseVargen;
begin
  K := lse_class_rec(V1);
  if Assigned(K^.addItem) and (V1^.VObject <> nil) then
  begin
    G := __AsVargen(R.FEngine, V2);
    lse_vargen_addref(G);
    try
      while lse_vargen_send(G, V2) do
        K^.addItem(V1^.VObject, V2, R.FEngine);
    finally
      lse_vargen_release(G);
    end;
  end;
end;

procedure __is(V1, V2: PLseValue);
var
  T1, T2: KLiClass;
begin
  T2 := __AsClass(V2);
  if T2 = KT_CLASS then
  begin
    T2 := KLiClass(__AsObject(V2));
    T1 := __AsClass(V1);
    if T1 = KT_CLASS then
      T1 := KLiClass(__AsObject(V1));
    lse_set_bool(V1, (T1 = T2) or ((T1 <> nil) and T1.IsHashed) and (T2 <> nil) and (T2 = KT_HASHED));
  end
  else __abseq(V1, V2);
end;

procedure __as(V1, V2: PLseValue; E: KLiEngine);
var
  T2: KLiClass;
begin
  T2 := __AsClass(V2);
  if T2 = KT_CLASS then
    T2 := KLiClass(__AsObject(V2));
  __SetClassValue(E, V1, T2);
end;

// __type_inc() ----------------------------------------------------------------

function __type_inc_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_inc_string(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
    Result := KT_STRING;
end;

function __type_inc_int(R: KLiClass): KLiClass;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
  if R.DataType in [LSV_FLOAT, LSV_MONEY] then
    Result := KT_FLOAT else
    Result := KT_INT;
end;

function __type_inc_float(R: KLiClass): KLiClass;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
    Result := KT_FLOAT;
end;

function __type_inc_money(R: KLiClass): KLiClass;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
    Result := KT_MONEY;
end;

function __type_inc_time(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
  if R.DataType in [LSV_INT, LSV_FLOAT] then
    Result := KT_TIME else
    Result := KT_VOID;
end;

function __type_inc_bool(R: KLiClass): KLiClass;
begin
  Result := __type_inc_int(R);
end;

function __type_inc_char(R: KLiClass): KLiClass;
begin
  if (R = KT_VOID) or not R.IsSimpleType then
    Result := KT_VOID else
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R = KT_STRING then
    Result := KT_STRING else
  if R = KT_CHAR then
    Result := KT_STRING else
  if R.DataType in [LSV_FLOAT,LSV_MONEY,LSV_TIME] then
    Result := KT_FLOAT else
    Result := KT_INT;
end;

function __type_inc_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_inc_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_inc(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_inc_void,
    {$IFDEF FPC}@{$ENDIF}__type_inc_string,
    {$IFDEF FPC}@{$ENDIF}__type_inc_int,
    {$IFDEF FPC}@{$ENDIF}__type_inc_float,
    {$IFDEF FPC}@{$ENDIF}__type_inc_money,
    {$IFDEF FPC}@{$ENDIF}__type_inc_time,
    {$IFDEF FPC}@{$ENDIF}__type_inc_bool,
    {$IFDEF FPC}@{$ENDIF}__type_inc_char,
    {$IFDEF FPC}@{$ENDIF}__type_inc_variant,
    {$IFDEF FPC}@{$ENDIF}__type_inc_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_dec() ----------------------------------------------------------------

function __type_dec_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_dec_string(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_dec_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY] then
    Result := KT_FLOAT else
  if R.DataType in [LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_dec_float(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_dec_money(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_MONEY else
    Result := KT_VOID;
end;

function __type_dec_time(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_INT, LSV_FLOAT] then
    Result := KT_TIME else
    Result := KT_VOID;
end;

function __type_dec_bool(R: KLiClass): KLiClass;
begin
  Result := __type_dec_int(R);
end;

function __type_dec_char(R: KLiClass): KLiClass;
begin
  Result := __type_dec_int(R);
end;

function __type_dec_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_dec_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_dec(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_dec_void,
    {$IFDEF FPC}@{$ENDIF}__type_dec_string,
    {$IFDEF FPC}@{$ENDIF}__type_dec_int,
    {$IFDEF FPC}@{$ENDIF}__type_dec_float,
    {$IFDEF FPC}@{$ENDIF}__type_dec_money,
    {$IFDEF FPC}@{$ENDIF}__type_dec_time,
    {$IFDEF FPC}@{$ENDIF}__type_dec_bool,
    {$IFDEF FPC}@{$ENDIF}__type_dec_char,
    {$IFDEF FPC}@{$ENDIF}__type_dec_variant,
    {$IFDEF FPC}@{$ENDIF}__type_dec_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_mul() ----------------------------------------------------------------

function __type_mul_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mul_string(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_INT] then
    Result := KT_STRING else
    Result := KT_VOID;
end;

function __type_mul_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY] then
    Result := KT_FLOAT else
  if R.DataType in [LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_mul_float(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_mul_money(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_MONEY else
    Result := KT_VOID;
end;

function __type_mul_time(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mul_bool(R: KLiClass): KLiClass;
begin
  Result := __type_mul_int(R);
end;

function __type_mul_char(R: KLiClass): KLiClass;
begin
  Result := __type_mul_int(R);
end;

function __type_mul_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_mul_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mul(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_mul_void,
    {$IFDEF FPC}@{$ENDIF}__type_mul_string,
    {$IFDEF FPC}@{$ENDIF}__type_mul_int,
    {$IFDEF FPC}@{$ENDIF}__type_mul_float,
    {$IFDEF FPC}@{$ENDIF}__type_mul_money,
    {$IFDEF FPC}@{$ENDIF}__type_mul_time,
    {$IFDEF FPC}@{$ENDIF}__type_mul_bool,
    {$IFDEF FPC}@{$ENDIF}__type_mul_char,
    {$IFDEF FPC}@{$ENDIF}__type_mul_variant,
    {$IFDEF FPC}@{$ENDIF}__type_mul_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_div() ----------------------------------------------------------------

function __type_div_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_div_string(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_div_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_div_float(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_FLOAT else
    Result := KT_VOID;
end;

function __type_div_money(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_MONEY else
    Result := KT_VOID;
end;

function __type_div_time(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_div_bool(R: KLiClass): KLiClass;
begin
  Result := __type_div_int(R);
end;

function __type_div_char(R: KLiClass): KLiClass;
begin
  Result := __type_div_int(R);
end;

function __type_div_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_div_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_div(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_div_void,
    {$IFDEF FPC}@{$ENDIF}__type_div_string,
    {$IFDEF FPC}@{$ENDIF}__type_div_int,
    {$IFDEF FPC}@{$ENDIF}__type_div_float,
    {$IFDEF FPC}@{$ENDIF}__type_div_money,
    {$IFDEF FPC}@{$ENDIF}__type_div_time,
    {$IFDEF FPC}@{$ENDIF}__type_div_bool,
    {$IFDEF FPC}@{$ENDIF}__type_div_char,
    {$IFDEF FPC}@{$ENDIF}__type_div_variant,
    {$IFDEF FPC}@{$ENDIF}__type_div_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_mod() ----------------------------------------------------------------

function __type_mod_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mod_string(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mod_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_mod_float(R: KLiClass): KLiClass;
begin
  Result := __type_mod_int(R);
end;

function __type_mod_money(R: KLiClass): KLiClass;
begin
  Result := __type_mod_int(R);
end;

function __type_mod_time(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mod_bool(R: KLiClass): KLiClass;
begin
  Result := __type_mod_int(R);
end;

function __type_mod_char(R: KLiClass): KLiClass;
begin
  Result := __type_mod_int(R);
end;

function __type_mod_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_mod_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_mod(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_mod_void,
    {$IFDEF FPC}@{$ENDIF}__type_mod_string,
    {$IFDEF FPC}@{$ENDIF}__type_mod_int,
    {$IFDEF FPC}@{$ENDIF}__type_mod_float,
    {$IFDEF FPC}@{$ENDIF}__type_mod_money,
    {$IFDEF FPC}@{$ENDIF}__type_mod_time,
    {$IFDEF FPC}@{$ENDIF}__type_mod_bool,
    {$IFDEF FPC}@{$ENDIF}__type_mod_char,
    {$IFDEF FPC}@{$ENDIF}__type_mod_variant,
    {$IFDEF FPC}@{$ENDIF}__type_mod_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_neg() ----------------------------------------------------------------

function __type_neg(L: KLiClass): KLiClass;
begin
  if L = KT_VARIANT then
    Result := KT_VARIANT else
  if L.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT] then
    Result := L else
  if L.DataType in [LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

// __type_xor() ----------------------------------------------------------------

function __type_xor_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_xor_string(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_xor_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_xor_float(R: KLiClass): KLiClass;
begin
  Result := __type_xor_int(R);
end;

function __type_xor_money(R: KLiClass): KLiClass;
begin
  Result := __type_xor_int(R);
end;

function __type_xor_time(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_xor_bool(R: KLiClass): KLiClass;
begin
  Result := __type_xor_int(R);
end;

function __type_xor_char(R: KLiClass): KLiClass;
begin
  Result := __type_xor_int(R);
end;

function __type_xor_variant(R: KLiClass): KLiClass;
begin
  if R = KT_VOID then
    Result := KT_VOID else
    Result := KT_VARIANT;
end;

function __type_xor_object(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_xor(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_xor_void,
    {$IFDEF FPC}@{$ENDIF}__type_xor_string,
    {$IFDEF FPC}@{$ENDIF}__type_xor_int,
    {$IFDEF FPC}@{$ENDIF}__type_xor_float,
    {$IFDEF FPC}@{$ENDIF}__type_xor_money,
    {$IFDEF FPC}@{$ENDIF}__type_xor_time,
    {$IFDEF FPC}@{$ENDIF}__type_xor_bool,
    {$IFDEF FPC}@{$ENDIF}__type_xor_char,
    {$IFDEF FPC}@{$ENDIF}__type_xor_variant,
    {$IFDEF FPC}@{$ENDIF}__type_xor_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_and() ----------------------------------------------------------------

function __type_and(L, R: KLiClass): KLiClass;
begin
  Result := __type_xor(L, R);
end;

// __type_or() -----------------------------------------------------------------

function __type_or(L, R: KLiClass): KLiClass;
begin
  Result := __type_xor(L, R);
end;

// __type_shl() ----------------------------------------------------------------

function __type_shl_void(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_shl_string(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_shl_int(R: KLiClass): KLiClass;
begin
  if R = KT_VARIANT then
    Result := KT_VARIANT else
  if R.DataType in [LSV_FLOAT, LSV_MONEY, LSV_INT, LSV_CHAR, LSV_BOOL] then
    Result := KT_INT else
    Result := KT_VOID;
end;

function __type_shl_float(R: KLiClass): KLiClass;
begin
  Result := __type_shl_int(R);
end;

function __type_shl_money(R: KLiClass): KLiClass;
begin
  Result := __type_shl_int(R);
end;

function __type_shl_time(R: KLiClass): KLiClass;
begin
  Result := KT_VOID;
end;

function __type_shl_bool(R: KLiClass): KLiClass;
begin
  Result := __type_shl_int(R);
end;

function __type_shl_char(R: KLiClass): KLiClass;
begin
  Result := __type_shl_int(R);
end;

function __type_shl_variant(R: KLiClass): KLiClass;
begin
  Result := KT_VARIANT;
end;

function __type_shl_object(R: KLiClass): KLiClass;
begin
  Result := R;
end;

function __type_shl(L, R: KLiClass): KLiClass;
const
  OPRS : array[TLseValue] of function(R: KLiClass): KLiClass = (
    {$IFDEF FPC}@{$ENDIF}__type_shl_void,
    {$IFDEF FPC}@{$ENDIF}__type_shl_string,
    {$IFDEF FPC}@{$ENDIF}__type_shl_int,
    {$IFDEF FPC}@{$ENDIF}__type_shl_float,
    {$IFDEF FPC}@{$ENDIF}__type_shl_money,
    {$IFDEF FPC}@{$ENDIF}__type_shl_time,
    {$IFDEF FPC}@{$ENDIF}__type_shl_bool,
    {$IFDEF FPC}@{$ENDIF}__type_shl_char,
    {$IFDEF FPC}@{$ENDIF}__type_shl_variant,
    {$IFDEF FPC}@{$ENDIF}__type_shl_object
  );
begin
  Result := OPRS[L.DataType](R);
end;

// __type_shr() ----------------------------------------------------------------

function __type_shr(L, R: KLiClass): KLiClass;
begin
  Result := __type_xor(L, R);
end;

// __type_not() ----------------------------------------------------------------

function __type_not(V: KLiClass): KLiClass;
begin
  if V.DataType in [LSV_FLOAT,LSV_MONEY,LSV_INT,LSV_CHAR,LSV_BOOL] then
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
  T1, T2: PLseClassRec;
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

  function compareM(A, B: currency): KLiCompResult;
  begin
    A := A - B;
    if A = 0 then
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
  T1 := lse_class_rec(V1);
  R1 := T1^.vtype;
  
  if R1 = LSV_VOID then
  begin
    if lse_is_defv(V2) then 
      Result := crEqual else
      Result := crLess;
    Exit;
  end;
  
  T2 := lse_class_rec(V2);
  R2 := T2^.vtype;
  
  if R2 = LSV_VOID then
  begin
    if lse_is_defv(V1) then 
      Result := crEqual else
      Result := crMore;
    Exit;
  end;

  if R1 = LSV_STRING then
  begin
    if R2 in [LSV_STRING, LSV_CHAR] then
      Result := compareS(__AsString(V1), __AsString(V2)) else
      Result := crDiff;
    Exit;
  end;

  if R1 = LSV_OBJECT then
  begin
    if (T1^.lysee_class = T2^.lysee_class) and (V1^.VObject = V2^.VObject) then
      Result := crEqual else
      Result := crDiff;
    Exit;
  end;

  if R2 in [LSV_STRING, LSV_OBJECT] then
  begin
    Result := crDiff;
    Exit;
  end;

  if (R1 = LSV_MONEY) and (R2 in [LSV_INT, LSV_FLOAT, LSV_MONEY]) then
  begin
    Result := compareM(__AsMoney(V1), __AsMoney(V2));
    Exit;
  end;
  
  if (R2 = LSV_MONEY) and (R1 in [LSV_INT, LSV_FLOAT, LSV_MONEY]) then
  begin
    Result := compareM(__AsMoney(V1), __AsMoney(V2));
    Exit;
  end;

  if (R1 in [LSV_FLOAT, LSV_MONEY, LSV_TIME])
    or (R2 in [LSV_FLOAT, LSV_MONEY, LSV_TIME]) then
      Result := compareF(__AsFloat(V1), __AsFloat(V2)) else
      Result := compareI(__AsInt64(V1), __AsInt64(V2));
end;

function __compare(V1, V2: PLseValue; Test: KLiCompResults): boolean;
begin
  Result := (__compare(V1, V2) in Test);
end;

function __contains(Host: KLiStrlist; Value: PLseValue): boolean;
begin
  Result := false;
  case lse_class_rec(Value)^.vtype of
    LSV_STRING: Result := (Host.IndexOf(lse_strec_string(Value^.VString)) >= 0);
    LSV_CHAR  : Result := (Host.IndexOf(Value^.VChar) >= 0);
  end;
end;

function __contains(Host: KLiVarList; Value: PLseValue; FindItemVG: boolean): boolean;
var
  clss, tmpc: KLiClass;
  next, size: integer;
  data: PLseValue;
begin
  Result := false;
  clss := __AsClass(Value);
  size := Host.Count;
  next := 0;
  while not Result and (next < size) do
  begin
    data := Host[next];
    tmpc := __AsClass(data);
    if clss = tmpc then
      case clss.ClassRec^.vtype of
        LSV_VOID   : Result := true;
        LSV_STRING : Result := (Value^.VString = data^.VString)
                            or __bufSame(lse_strec_data(Value^.VString),
                                         lse_strec_length(Value^.VString),
                                         lse_strec_data(data^.VString),
                                         lse_strec_length(data^.VString),
                                         false);
        LSV_INT    : Result := (Value^.VInteger = data^.VInteger);
        LSV_FLOAT  : Result := IsZero(Value^.VFloat - data^.VFloat);
        LSV_MONEY  : Result := (Value^.VMoney = data^.VMoney);
        LSV_TIME   : Result := IsZero(Value^.VTime - data^.VTime);
        LSV_BOOL   : Result := (Value^.VBool = data^.VBool);
        LSV_CHAR   : Result := (Value^.VChar = data^.VChar);
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
    case lse_class_rec(Value)^.vtype of
      LSV_CHAR  : Result := hasc(Value^.VChar);
      LSV_STRING: Result := hass(Value^.VString);
      LSV_INT   : Result := (Value^.VInteger in [0..255]) and
                             hasc(char(Value^.VInteger));
    end;
  end;
end;

function __contains(Host: int64; Value: PLseValue): boolean;
begin
  Result := false;
  if Host > 0 then
    case lse_class_rec(Value)^.vtype of
      LSV_INT : Result := (Host > Value^.VInteger) and (Value^.VInteger >= 0);
      LSV_CHAR: Result := (Host > Ord(Value^.VChar));
      LSV_BOOL: Result := (Host > Ord(Value^.VBool));
    end;
end;

function __contains(Host: KLiHashed; Value: PLseValue): boolean;
begin
  Result := (Value^.value_class <> nil) and
            (Value^.value_class^.vtype in [LSV_STRING, LSV_CHAR]) and
            (Host.FindValueByChain(__AsString(Value)) <> nil);
end;

function __decodeClassName(const className: string; var libName: string): string;
var
  X: integer;
begin
  X := Pos('::', className);
  if X > 0 then
  begin
    Result := Copy(className, X + 2, MaxInt);
    libName := Copy(className, 1, X - 1);
  end
  else
  begin
    Result := className;
    LibName := '';
  end;
end;

function __SetupClasses(List: PLseClassList; Count: integer; Owner: KLiModule): integer;
var
  K: KLiClass;
  A: integer;
  L: TList;
  rec: PLseClassRec;
begin
  L := TList.Create;
  try
    for A := 0 to Count - 1 do
    begin
      rec := Addr(List^[A]);
      if __IsClassRec(rec) and Owner.CanDeclare(rec^.name) then
      begin
        K := KLiClass.Create(Owner, rec^.name, LSV_OBJECT);
        rec^.lysee_class := K;
        Move(rec^, K.FClassRec, sizeof(RLseClassRec));
        L.Add(K);
      end;
    end;
    Result := L.Count;
  finally
    L.Free;
  end;
end;

function __IsClassListRec(R: PLseClassListRec): boolean;
begin
  Result := (R <> nil) and ((R^.count = 0) or ((R^.count > 0) and (R^.entry <> nil)));
end;

function __loadLibrary(const name: string; const libfile: string): KLiModule;
label
  UNLOCK;
var
  handle: THandle;
  initex: TLseInitExchange;
  initrec: RLseModuleRec;
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

    initex := TLseInitExchange(lse_get_proc(handle, 'InitExchange'));
    if not Assigned(initex) then
    begin
      lse_free_library(handle);
      goto UNLOCK;
    end;

    // 4. initialize the library

    __zero(@initrec, sizeof(RLseModuleRec));
    initex(@initrec, @QueryEntry);

    // 5. setup classes list

    Result := KLiModule.Create(name, nil, moyLibrary);
    try
      Result.FFileName := libfile;
      Result.FImportProc := initrec.iw_import;
      Result.FInvokeProc := initrec.iw_invoke;
      Result.FVersion := initrec.iw_version;
      Result.FDescription := initrec.iw_desc;
      Result.FHandle := handle;
      if __IsClassListRec(Addr(initrec.iw_classes)) then
        __SetupClasses(initrec.iw_classes.entry, initrec.iw_classes.count, Result);
      Result.SetupModuleClass(Addr(initrec.iw_libfuncs));
    except
      FreeAndNil(Result);
    end;
    UNLOCK:
  finally
    unlock_kernel;
  end;
end;

function __SetupModule(const name: string; initrec: PLseModuleRec): KLiModule;
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
    or __namedExists(sys_libraries, ID) or (initrec = nil)
    or not __IsClassListRec(Addr(initrec^.iw_classes)) then goto UNLOCK;

    // 2. setup classes list
    Result := KLiModule.Create(ID, nil, moyRegistered);
    Result.FFileName := fname;
    Result.FImportProc := initrec^.iw_import;
    Result.FInvokeProc := initrec^.iw_invoke;
    Result.FVersion := initrec^.iw_version;
    Result.FDescription := initrec^.iw_desc;
    __SetupClasses(initrec^.iw_classes.entry, initrec^.iw_classes.count, Result);
    Result.SetupModuleClass(Addr(initrec^.iw_libfuncs));

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

      temp := path + Name + '.lsp';
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
  class_rec: RLseClassRec;

  function classup(M: KLiModule; KC: TLseKernelClass; var KR: PLseClassRec): KLiClass;
  begin
    Result := KLiClass.Create(M, class_rec.name, TLseValue(class_rec.vtype));
    KR := @Result.FClassRec;
    class_rec.lysee_class := Result;
    Move(class_rec, KR^, sizeof(RLseClassRec));
    Result.SetState(clsBuiltin, true);
    sys_class_list[KC] := KR;
  end;

begin
  if sys_module = nil then
  begin
    sys_module := KLiModule.Create('sys', nil, moyKernel);
    cgi_module := KLiModule.Create('cgi', nil, moyKernel);

    lse_fill_class(@class_rec, LSV_VOID);
    KT_VOID := classup(sys_module, kcVoid, KR_VOID);

    lse_fill_class(@class_rec, LSV_STRING);
    class_rec.incRefcount := @lse_strec_addref;
    class_rec.decRefcount := @lse_strec_release;
    class_rec.toVargen := @cvgr_string;
    class_rec.funcs.count := string_func_count;
    class_rec.funcs.entry :=@string_func_array;
    KT_STRING := classup(sys_module, kcString, KR_STRING);

    lse_fill_class(@class_rec, LSV_INT);
    class_rec.funcs.count := int_func_count;
    class_rec.funcs.entry :=@int_func_array;
    KT_INT := classup(sys_module, kcInteger, KR_INT);

    lse_fill_class(@class_rec, LSV_FLOAT);
    KT_FLOAT := classup(sys_module, kcFloat, KR_FLOAT);

    lse_fill_class(@class_rec, LSV_MONEY);
    KT_MONEY := classup(sys_module, kcMoney, KR_MONEY);

    lse_fill_class(@class_rec, LSV_TIME);
    class_rec.funcs.count := time_func_count;
    class_rec.funcs.entry :=@time_func_array;
    KT_TIME := classup(sys_module, kcTime, KR_TIME);

    lse_fill_class(@class_rec, LSV_BOOL);
    KT_BOOL := classup(sys_module, kcBool, KR_BOOL);

    lse_fill_class(@class_rec, LSV_CHAR);
    class_rec.funcs.count := char_func_count;
    class_rec.funcs.entry :=@char_func_array;
    KT_CHAR := classup(sys_module, kcChar, KR_CHAR);

    lse_fill_class(@class_rec, LSV_VARIANT);
    KT_VARIANT := classup(sys_module, kcVariant, KR_VARIANT);

    lse_fill_class(@class_rec, KTN_VARLIST, KTD_VARLIST, LSV_OBJECT);
    class_rec.toString := @otos_varlist;
    class_rec.stringTo := @stoo_varlist;
    class_rec.toVargen := @cvgr_varlist;
    class_rec.addItem := @addi_varlist;
    class_rec.funcs.count := varlist_func_count;
    class_rec.funcs.entry :=@varlist_func_array;
    KT_VARLIST := classup(sys_module, kcVarlist, KR_VARLIST);

    lse_fill_class(@class_rec, KTN_STRLIST, KTD_STRLIST, LSV_OBJECT);
    class_rec.incRefcount := @LsIncRefcount;
    class_rec.decRefcount := @LsDecRefcount;
    class_rec.toString := @otos_strlist;
    class_rec.stringTo := @stoo_strlist;
    class_rec.writeTo := @wtos_strlist;
    class_rec.toVargen := @cvgr_strlist;
    class_rec.addItem := @addi_strlist;
    class_rec.funcs.count := strlist_func_count;
    class_rec.funcs.entry :=@strlist_func_array;
    KT_STRLIST := classup(sys_module, kcStrlist, KR_STRLIST);

    lse_fill_class(@class_rec, KTN_TYPE, KTD_TYPE, LSV_OBJECT);
    class_rec.toString := @otos_type;
    class_rec.stringTo := @stoo_type;
    class_rec.funcs.count := type_func_count;
    class_rec.funcs.entry :=@type_func_array;
    KT_CLASS := classup(sys_module, kcClass, KR_CLASS);

    lse_fill_class(@class_rec, KTN_MODULE, KTD_MODULE, LSV_OBJECT);
    class_rec.toString := @otos_module;
    class_rec.stringTo := @stoo_module;
    class_rec.funcs.count := module_func_count;
    class_rec.funcs.entry :=@module_func_array;
    KT_MODULE := classup(sys_module, kcModule, KR_MODULE);

    lse_fill_class(@class_rec, KTN_FUNC, KTD_FUNC, LSV_OBJECT);
    class_rec.toString := @otos_function;
    class_rec.toVargen := @cvgr_func;
    class_rec.addItem := @addi_function;
    class_rec.funcs.count := func_func_count;
    class_rec.funcs.entry :=@func_func_array;
    KT_FUNC := classup(sys_module, kcFunc, KR_FUNC);

    lse_fill_class(@class_rec, KTN_VARIABLE, KTD_VARIABLE, LSV_OBJECT);
    class_rec.toString := @otos_variable;
    class_rec.stringTo := @stoo_variable;
    class_rec.funcs.count := varb_func_count;
    class_rec.funcs.entry :=@varb_func_array;
    KT_VARIABLE := classup(sys_module, kcVariable, KR_VARIABLE);

    lse_fill_class(@class_rec, KTN_ERROR, KTD_ERROR, LSV_OBJECT);
    class_rec.toString := @otos_error;
    class_rec.funcs.count := error_func_count;
    class_rec.funcs.entry :=@error_func_array;
    KT_ERROR := classup(sys_module, kcError, KR_ERROR);

    lse_fill_class(@class_rec, KTN_STREAM, KTD_STREAM, LSV_OBJECT);
    class_rec.incRefcount := @lse_stream_addref;
    class_rec.decRefcount := @lse_stream_release;
    class_rec.toVargen := @cvgr_stream;
    class_rec.writeTo := @wtos_stream;
    class_rec.addItem := @addi_stream;
    class_rec.funcs.count := stream_func_count;
    class_rec.funcs.entry :=@stream_func_array;
    KT_STREAM := classup(sys_module, kcStream, KR_STREAM);

    lse_fill_class(@class_rec, KTN_DB, KTD_DB, LSV_OBJECT);
    class_rec.funcs.count := db_execute_count;
    class_rec.funcs.entry :=@db_execute_array;
    class_rec.incRefcount :=@lse_db_addref;
    class_rec.decRefcount :=@lse_db_release;
    KT_DB := classup(sys_module, kcDB, KR_DB);

    lse_fill_class(@class_rec, KTN_DS, KTD_DS, LSV_OBJECT);
    class_rec.funcs.count := ds_execute_count;
    class_rec.funcs.entry :=@ds_execute_array;
    class_rec.toString := @otos_dataset;
    class_rec.toVargen := @cvgr_dataset;
    class_rec.incRefcount :=@lse_ds_addref;
    class_rec.decRefcount :=@lse_ds_release;
    KT_DS := classup(sys_module, kcDS, KR_DS);

    lse_fill_class(@class_rec, KTN_HASHED, KTD_HASHED, LSV_OBJECT);
    class_rec.funcs.count := hashed_func_count;
    class_rec.funcs.entry :=@hashed_func_array;
    KT_HASHED := classup(sys_module, kcHashed, KR_HASHED);
    KT_HASHED.SetState(clsHashed, true);

    lse_fill_class(@class_rec, KTN_VARGEN, KTD_VARGEN, LSV_OBJECT);
    class_rec.incRefcount := @lse_vargen_addref;
    class_rec.decRefcount := @lse_vargen_release;
    class_rec.funcs.count := vargen_func_count;
    class_rec.funcs.entry :=@vargen_func_array;
    KT_VARGEN := classup(sys_module, kcVargen, KR_VARGEN);

    { setup class methods }

    sys_module.SetupModuleClass(@sys_module_funcs);
    cgi_module.SetupModuleClass(@cgi_module_funcs);

    { prepare }

    lse_init_value(@sys_void_data);

    sys_getpv_func    := sys_module.FindFunc('getpv');
    sys_setpv_func    := sys_module.FindFunc('setpv');
    sys_getiv_func    := sys_module.FindFunc('getiv');
    sys_setiv_func    := sys_module.FindFunc('setiv');
    sys_getfv_func    := sys_module.FindFunc('getfv');
    sys_setfv_func    := sys_module.FindFunc('setfv');
    sys_encodeUTF8    := sys_module.FindFunc('encodeUTF8');
    sys_decodeUTF8    := sys_module.FindFunc('decodeUTF8');
    sys_encodeS       := sys_module.FindFunc('encodeS');
    sys_decodeS       := sys_module.FindFunc('decodeS');
    sys_curryone_func := sys_module.FindFunc('curryOne');
    sys_module.FindFunc('eol').IsNameCall := true;
    sys_vargen_eof    := KT_VARGEN.FindMethod(cmMethod, 'get_eof');
    sys_vargen_map    := KT_VARGEN.FindMethod(cmMethod, 'map');
    sys_vargen_reduce := KT_VARGEN.FindMethod(cmMethod, 'reduce');
    sys_vargen_filter := KT_VARGEN.FindMethod(cmMethod, 'filter');
    sys_vargen_each   := KT_VARGEN.FindMethod(cmMethod, 'each');
    sys_vargen_send   := KT_VARGEN.FindMethod(cmMethod, 'send');
    sys_vargen_next   := KT_VARGEN.FindMethod(cmMethod, 'next');
    sys_varlist_getpv := KT_VARLIST.FindMethod(cmMethod, 'getpv');
    sys_varlist_setpv := KT_VARLIST.FindMethod(cmMethod, 'setpv');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure runner_seek_next(Sender: KLiRunner);
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
  Sender.FStack.Press;
  runner_seek_next(Sender);
end;

procedure __runner_ID(Sender: KLiRunner);
begin
  with Sender do
    FStack.Push(GetValue(FExprrec^.VVarb));
  runner_seek_next(Sender);
end;

procedure __runner_become(Sender: KLiRunner);
var
  varb: KLiVarb;
  data: PLseValue;
begin
  with Sender do
  begin
    data := FStack.Last;
    varb := FExprrec^.VVarb;
    __SetClassValue(FEngine, data, varb.ValueType);
    if not FExcepted then
      lse_set_value(GetValue(varb), data);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_float(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushFloat(FExprrec^.VFLoat);
  runner_seek_next(Sender);
end;

procedure __runner_money(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushMoney(FExprrec^.VMoney);
  runner_seek_next(Sender);
end;

procedure __runner_call(Sender: KLiRunner);
begin
  with Sender do with FExprrec^ do
    Goon(VFunc, ParamCount, nil);
  runner_seek_next(Sender);
end;

procedure __runner_out(Sender: KLiRunner);
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
        lse_stream_write(FEngine.StdoutStream, ' ', 1);
      data := FStack[index + A];
      if lse_vtype(data) = LSV_STRING then
        lse_stream_write(FEngine.StdoutStream, data^.VString) else
        lse_stream_write(FEngine.StdoutStream, __AsString(data));
    end;
    FStack.Press(FExprrec^.ParamCount);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_time(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushTime(FExprrec^.VTime);
  runner_seek_next(Sender);
end;

procedure __runner_int(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushInt64(FExprrec^.VInteger);
  runner_seek_next(Sender);
end;

procedure __runner_str(Sender: KLiRunner);
begin
  with Sender do
    lse_set_string(FStack.Add, FExprrec^.VStr);
  runner_seek_next(Sender);
end;

procedure __runner_char(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushChar(FExprrec^.VChar);
  runner_seek_next(Sender);
end;

procedure __runner_add(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __inc(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_dec(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __dec(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_mul(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __mul(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_div(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __div(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_mod(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __mod(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_bnot(Sender: KLiRunner);
begin
  __not(Sender.FStack.Last);
  runner_seek_next(Sender);
end;

procedure __runner_bxor(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __xor(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_bor(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __or(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_band(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __and(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_bshl(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __shl(SecondLast, Last, FEngine);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_bshr(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __shr(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_not(Sender: KLiRunner);
begin
  __logicNot(Sender.FStack.Last);
  runner_seek_next(Sender);
end;

procedure __runner_neg(Sender: KLiRunner);
begin
  __neg(Sender.FStack.Last);
  runner_seek_next(Sender);
end;

procedure __runner_eq(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __equal(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_abseq(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __abseq(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_ne(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __diff(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_lt(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __less(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_le(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __eqless(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_gt(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __more(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_ge(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __eqmore(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_in(Sender: KLiRunner);
var
  V1, V2: PLseValue;
  T2: KLiClass;
  done: boolean;
begin
  done := false;
  with Sender do
  begin
    V1 := FStack.SecondLast;
    V2 := FStack.Last;
    T2 := __AsClass(V2);
    case T2.FClassRec.vtype of
      LSV_STRING: done := __contains(V2^.VString, V1);
      LSV_INT   : done := __contains(V2^.VInteger, V1);
      LSV_OBJECT: if V2^.VObject <> nil then
                    if T2 = KT_VARLIST then
                      done := __contains(KLiVarList(V2^.VObject), V1, true) else
                    if T2 = KT_STRLIST then
                      done := __contains(KLiStrlist(V2^.VObject), V1) else
                    if T2 = KT_HASHED then
                      done := __contains(KLiHashed(V2^.VObject), V1) else
                      done := lse_vargen_contains(__AsVargen(FEngine, V2), V1);
    end;
    FStack.Press;
  end;
  lse_set_bool(V1, done);
  runner_seek_next(Sender);
end;

procedure __runner_and(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __logicAnd(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_or(Sender: KLiRunner);
begin
  with Sender.FStack do
  begin
    __logicOr(SecondLast, Last);
    Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_true(Sender: KLiRunner);
begin
  Sender.FStack.PushBoolean(true);
  runner_seek_next(Sender);
end;

procedure __runner_false(Sender: KLiRunner);
begin
  Sender.FStack.PushBoolean(false);
  runner_seek_next(Sender);
end;

procedure __runner_type(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushObject(FExprrec^.VType, KT_CLASS);
  runner_seek_next(Sender);
end;

procedure __runner_func(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushObject(FExprrec^.VFunc, KT_FUNC);
  runner_seek_next(Sender);
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
      __PutValue(L.AddNamed(__AsString(FStack[X])), FStack[X + 1]);
      Inc(X, 2);
    end;
    FStack.Press(FExprrec^.ParamCount);
    FStack.PushObject(L, KT_VARLIST);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_nil(Sender: KLiRunner);
begin
  Sender.FStack.PushDefaultValue(KT_VOID);
  runner_seek_next(Sender);
end;

procedure __runner_shell(Sender: KLiRunner);
var
  status: integer;
begin
  with Sender do
  begin
    FStack.PushString(spawn_shouts(FormatFor(FExprrec^.Name, nil), '', status));
    FShellExitCode := status;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_getvalue(Sender: KLiRunner);
begin
  with Sender do
    FEngine.GetValue(FExprrec^.Name, FStack.Add);
  runner_seek_next(Sender);
end;

procedure __runner_setvalue(Sender: KLiRunner);
begin
  with Sender do
    FEngine.FMainValues.SetValueByChain(FExprrec^.Name, FStack.Last);
  runner_seek_next(Sender);
end;

procedure __runner_format(Sender: KLiRunner);
var
  last: PLseValue;
begin
  with Sender do
  begin
    last := FStack.Last;
    lse_set_string(last, FormatFor(__AsString(last), nil));
  end;
  runner_seek_next(Sender);
end;

procedure __runner_bool(Sender: KLiRunner);
var
  last: PLseValue;
begin
  with Sender do
  begin
    last := FStack.Last;
    lse_set_bool(last, __AsBool(last));
  end;
  runner_seek_next(Sender);
end;

procedure __runner_method(Sender: KLiRunner);
var
  last: PLseValue;
  func, curry: KLiFunc;
begin
  with Sender do
  begin
    last := FStack.Last;
    func := FExprrec^.VFunc;
    curry := curry_one(func, last, CurrentFunc.Module);
    __SetObject(last, KT_FUNC, curry);
    if curry <> func then
      curry.DecRefcount; // adjust refcount
  end;
  runner_seek_next(Sender);
end;

procedure __runner_puts(Sender: KLiRunner);
begin
  with Sender do
    lse_stream_write(FEngine.StdoutStream, FExprrec^.Name);
  runner_seek_next(Sender);
end;

procedure __runner_module(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushObject(FExprrec^.VType.Module, KT_MODULE);
  runner_seek_next(Sender);
end;

procedure __runner_is(Sender: KLiRunner);
begin
  with Sender do
  begin
    __is(FStack.SecondLast, FStack.Last);
    FStack.Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_as(Sender: KLiRunner);
begin
  with Sender do
  begin
    __as(FStack.SecondLast, FStack.Last, FEngine);
    FStack.Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_statement(Sender: KLiRunner);
begin
  with Sender do
    FStack.ClearTo(FCurrent^.base);
  runner_seek_next(Sender);
end;

procedure __runner_vargen(Sender: KLiRunner);
var
  varg: PLseVargen;
  last: PLseValue;
begin
  with Sender do
    if FExprrec^.ParamCount = 1 then
    begin
      last := FStack.Last;
      __SetVarGen(last, __AsVargen(FEngine, last));
    end
    else
    begin
      varg := cvgr_combine(FStack, FExprrec^.ParamCount);
      FStack.Press(FExprrec^.ParamCount - 1);
      __SetVarGen(FStack.Last, varg);
    end;
  runner_seek_next(Sender);
end;

procedure __runner_pushvarb(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushObject(FExprrec^.VVarb, KT_VARIABLE);
  runner_seek_next(Sender);
end;

procedure __runner_ask(Sender: KLiRunner);
var
  base, prmc: integer;
  data: PLseValue;
  clss: KLiClass;
  func: KLiFunc;
begin
  with Sender do
  begin
    prmc := FExprrec^.ParamCount;
    base := FStack.Count - prmc;
    data := FStack[base];
    clss := __AsClass(data);
    if clss = KT_FUNC then
    begin
      func := __AsFunc(data);
      __check(func <> nil, EsFuncNotSpecify);
      Goon(func, prmc - 1, data);
    end
    else
    if clss = KT_CLASS then
    begin
      clss := KLiClass(data^.VObject);
      __check(clss <> nil, EsClassNotSpecify);
      __SetDefaultValue(data, clss);
      func := clss.SingleMethod(cmCreator);
      if func = nil then
        FStack.Press(prmc - 1) else
        Goon(func, prmc, nil);
    end
    else lse_error('invalid call to %s', [clss.FullName]);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_label(Sender: KLiRunner);
begin
  with Sender do
    FStack.ClearTo(FCurrent^.base);
  runner_seek_next(Sender);
end;

procedure __runner_idle(Sender: KLiRunner);
begin
  runner_seek_next(Sender);
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
    endx := FExprrec^.Next^.VOffset;
    in_finally := xrInFinally in FExprrec^.flags;
    while ExecGoonNext do
      if (FCurrent^.next < begx) or (FCurrent^.next > endx) then
        Break;
    if FExcepted and not FTerminated and HasNext then
    begin
      FExcepted := false;
      FCurrent^.next := endx + 1;
      FStack.ClearTo(FCurrent^.base);
      ExecGoonNext; // syRINR
      if in_finally then
        FExcepted := true;
    end;
  end;
end;

procedure __runner_return(Sender: KLiRunner);
begin
  Sender.Return(Sender.FExprrec^.ParamCount > 0);
end;

procedure __runner_jump(Sender: KLiRunner);
begin
  with Sender do
    Inc(FCurrent^.next, FExprrec^.VOffset);
end;

procedure __runner_jmpf(Sender: KLiRunner);
begin
  with Sender do
    if not __AsBool(FStack.Last) then
      Inc(FCurrent^.next, FExprrec^.VOffset) else
      Inc(FCurrent^.next);
end;

procedure __runner_jmpt(Sender: KLiRunner);
begin
  with Sender do
    if __AsBool(FStack.Last) then
      Inc(FCurrent^.next, FExprrec^.VOffset) else
      Inc(FCurrent^.next);
end;

procedure __runner_jmpfpop(Sender: KLiRunner);
begin
  __runner_jmpf(Sender);
  Sender.FStack.Press;
end;

procedure __runner_jmptpop(Sender: KLiRunner);
begin
  __runner_jmpt(Sender);
  Sender.FStack.Press;
end;

procedure __runner_goto(Sender: KLiRunner);
begin
  with Sender do
    FCurrent^.next := FExprrec^.Next^.VOffset;
end;

procedure __runner_gototp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if __AsBool(FStack.Last) then
      FCurrent^.next := FExprrec^.Next^.VOffset else
      Inc(FCurrent^.next);
    FStack.Press;
  end;
end;

procedure __runner_gotofp(Sender: KLiRunner);
begin
  with Sender do
  begin
    if not __AsBool(FStack.Last) then
      FCurrent^.next := FExprrec^.Next^.VOffset else
      Inc(FCurrent^.next);
    FStack.Press;
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
    FStack.PushObject(H, KT_HASHED);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_callask(Sender: KLiRunner);
var
  base, prmc: integer;
  data: PLseValue;
  func: KLiFunc;
  clss: KLiClass;
  name: string;
begin
  with Sender do
  begin
    base := FStack.Count - FExprrec^.ParamCount;
    data := FStack[base];
    func := FExprrec^.VFunc;
    if func = nil then
    begin
      clss := __AsClass(data);
      name := FExprrec^.Name;
      func := clss.FindMethod(cmMethod, name);
      if func <> nil then
      begin
        Goon(func, FExprrec^.ParamCount, nil);
        runner_seek_next(Sender);
        Exit;
      end;
      func := clss.FindGetMethod(name);
      if func = nil then
      begin
        func := clss.SingleMethod(cmGetPv);
        if func = nil then
        begin
          ErrorRT(Format('class %s has no "%s" property', [clss.Name, name]));
          Exit;
        end;
      end;
    end;
    FStack.Push(data);
    if func.ParamCount > 1 then
    begin
      FStack.PushString(FExprrec^.Name);
      prmc := 2;
    end
    else prmc := 1;
    if Goon(func, prmc, data) then
      __runner_ask(Sender);
  end;
end;

procedure __runner_upto(Sender: KLiRunner);
var
  begv, endv: int64;
  data: PLseValue;
begin
  with Sender do
  begin
    data := FStack.SecondLast; 
    begv := __AsInt64(data);
    endv := __AsInt64(FStack.Last);
    lse_set_object(data, KR_VARGEN,
      lse_vargen_ensure(cvgr_upto(begv, endv, 1, FEngine)));
    FStack.Press;
  end;
  runner_seek_next(Sender);
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
      endx := FExprrec^.Next^.VOffset;
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
  Sender.FStack.Push(Sender.FEngine.FTempValues.Last);
  runner_seek_next(Sender);
end;

procedure __runner_SETV(Sender: KLiRunner);
begin
  lse_set_value(Sender.FEngine.FTempValues.Last, Sender.FStack.Last);
  Sender.FStack.Press;
  runner_seek_next(Sender);
end;

procedure __runner_like(Sender: KLiRunner);
begin
  with Sender do
  begin
    __like(FStack.SecondLast, FStack.Last, Sender);
    FStack.Press;
  end;
  runner_seek_next(Sender);
end;

procedure __runner_getpv(Sender: KLiRunner);
begin
  with Sender do
    if FExprrec^.ParamCount = 0 then
    begin
      FStack.Push(FCurrent^.values[0]);
      FStack.PushString(FExprrec^.Name);
      Goon(sys_varlist_getpv, 2, nil);
    end
    else
    begin
      FStack.PushString(FExprrec^.Name);
      Goon(FExprrec^.VFunc, 2, nil);
    end;
  runner_seek_next(Sender);
end;

procedure __runner_setpv(Sender: KLiRunner);
begin
  with Sender do
    if FExprrec^.ParamCount = 1 then
    begin
      FStack.Push(FCurrent^.values[0]);
      FStack.ExchangeLastTwo;
      FStack.PushString(FExprrec^.Name);
      FStack.ExchangeLastTwo;
      Goon(sys_varlist_setpv, 3, nil);
    end
    else
    begin
      FStack.PushString(FExprrec^.Name);
      FStack.ExchangeLastTwo;
      Goon(FExprrec^.VFunc, 3, nil);
    end;
  runner_seek_next(Sender);
end;

procedure __runner_getiv(Sender: KLiRunner);
begin
  with Sender do
    Goon(FExprrec^.VFunc, 2, nil);
  runner_seek_next(Sender);
end;

procedure __runner_setiv(Sender: KLiRunner);
begin
  with Sender do
    Goon(FExprrec^.VFunc, 3, nil);
  runner_seek_next(Sender);
end;

procedure __runner_clen(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushInt64(FCallStack.FCount);
  runner_seek_next(Sender);
end;

procedure __runner_this(Sender: KLiRunner);
begin
  with Sender do
    FStack.PushObject(FCurrent^.values, KT_VARLIST);
  runner_seek_next(Sender);
end;

procedure __runner_duplast(Sender: KLiRunner);
var
  X, N: integer;
begin
  N := Sender.FExprrec^.ParamCount;
  X := Sender.FStack.Count - N;
  while N > 0 do
  begin
    Sender.FStack.Push(Sender.FStack[X]);
    Inc(X);
    Dec(N);
  end;
  runner_seek_next(Sender);
end;

procedure __runner_addAll(Sender: KLiRunner);
begin
  with Sender do
  begin
    __addAll(FStack.SecondLast, FStack.Last, Sender);
    Sender.FStack.Press;
  end;
  runner_seek_next(Sender);
end;

function __log(const Msg: pchar; Count: integer): pchar;overload;
begin
  if sys_create_log then
  begin
    if sys_log_stream = nil then
    begin
      sys_log_file := sys_tmpath + 'lysee-' +
        FormatDateTime('yyyymmddhhnnsszzz', Now) + '.log'; 
      sys_log_stream := TFileStream.Create(lse_veryPD(sys_log_file), fmCreate);
    end;
    lse_stream_write(sys_log_stream, FormatDateTime('hh:nn:ss zzz> ', Now));
    if Msg <> nil then
      lse_stream_write(sys_log_stream, Msg, Count);
    lse_stream_writeln(sys_log_stream);
  end;
  Result := Msg;
end;

function __log(const Msg: string): string;
begin
  __log(pchar(Msg), Length(Msg));
  Result := Msg;
end;

function __log(const ID, Msg: string): string;
begin
  Result := __log(ID + ': ' + Msg);
end;

////////////////////////////////////////////////////////////////////////////////

{ KLiParser }

constructor KLiParser.Create(AModule: KLiModule);
begin
  FModule := AModule;
  FRunner := FModule.FEngine.FMainRunner;
  FExpr := TList.Create;
  FSymbols := KLiTokens.Create;
  FEndBlockSyms := [syEOF];
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

function KLiParser.Parse(const Code: string; IsLsp: boolean): KLiFunc;
begin
  FExpr.Clear;
  FSymbols.Clear;
  FIsLsp := IsLsp;
  FCurrent := nil;
  FreeAndNil(FTokenizer);
  FTokenizer := KLiTokenizer.Create(Code,
    FModule.FEngine.AddIncludedFile(FModule.FFileName), FIsLsp);

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

  if GetNextSym then
  repeat
    if FLast^.Sym = syDefine then ParseDefine else
    if FLast^.Sym = syImport then ParseImport else
    if FLast^.Sym = syConst then ParseConst else
    if FLast^.Sym = syClass then ParseClass else
    if FLast^.Sym = syOption then ParseOption else
    if FLast^.Sym = syInclude then ParseInclude else
    if Result <> nil then
    begin
      FCurrent := Result;
      ParseStatement;
    end;
  until not GetNextSym;
end;

function KLiParser.GetSym(sym: PLiToken): boolean;
var
  token: PLiToken;
  isstr: boolean;
begin
  token := FTokenizer.GetNextToken(isstr);
  Result := token <> nil;
  if Result then
  begin
    sym^.Pos := token^.Pos;
    sym^.VInteger := token^.VInteger;
    if (token^.Sym in FEndBlockSyms) and not FToDotComma then
    begin
      sym^.Sym := syDotComma;
      sym^.Val := ';';
      FToDotComma := true;
      FTokenizer.DupCurrentToken;
    end
    else
    begin
      sym^.Sym := token^.Sym;
      sym^.Val := token^.Val;
      FToDotComma := false;
    end;
    FLast := sym;
  end;
end;

function KLiParser.GetNextSym: boolean;
begin
  Result := GetSym(FSymbols.Next);
end;

procedure KLiParser.ParseVarb(var varb: KLiVarb; var vrec: PLiToken;
  EndSyms: KLiSymbols; OnHead: boolean);
var
  clss: KLiClass;
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
    if not FCurrent.CanDeclare(data^.Val) then
    begin
      FLast := data;
      Error.Redeclared(Self);
    end;
    SymGotoNext;
    ParseVarType(clss, false);
    varb := CurCodes.AddLocal(data^.Val, clss);
    varb.FPos := data^.Pos;
  end
  else
  if not FCurrent.FindBy(data^.Val, @rec, [foVarb]) then
  begin
    if not FCurrent.CanDeclare(data^.Val) then
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

procedure KLiParser.ParseVarType(var VarType: KLiClass; Token: PLiToken);
var
  m_name, c_name: string;
  bklast: PLiToken;
begin
  bklast := FLast;
  try
    FLast := Token;
    if IsPureID(FLast) then
    begin
      m_name := '';
      c_name := FLast^.Val;
    end
    else c_name := __decodeClassName(FLast^.Val, m_name);
    VarType := FModule.FindClassBy(c_name, m_name);
    if VarType = nil then
      Error.ClassNotExists(Self);
  finally
    FLast := bklast;
  end;
end;

procedure KLiParser.ParseVarType(var VarType: KLiClass; OnHead: boolean);
begin
  if not OnHead then SymGotoNext;
  ParseVarType(VarType, FLast);
end;

procedure KLiParser.ParseBlock(EndSyms: KLiSymbols; OnHead: boolean);
var
  ends: KLiSymbols;
begin
  ends := FEndBlockSyms;
  FEndBlockSyms := EndSyms + [syEOF];
  try
    if not OnHead then
    begin
      FSymbols.Clear;
      SymGotoNext;
    end;
    while not (FLast^.Sym in EndSyms) do
    begin
      ParseStatement;
      SymGotoNext;
    end;
  finally
    FEndBlockSyms := ends;
    FToDotComma := false;
  end;
end;

procedure KLiParser.ParseStatement;
begin
  try
    if FLast^.Sym = syDot2 then SymGotoNext;
    if not (FLast^.Sym in [syDotComma, syEOF]) then
    begin
      CurCodes.BeginStatement(FLast^.Pos);
      case FLast^.Sym of
        syIf      : ParseIf;
        syWhile   : ParseWhile;
        sySwitch  : ParseSwitch;
        syBreak   : ParseBreak;
        syContinue: ParseContinue;
        syReturn  : ParseReturn;
        syTry     : ParseTry;
        syRepeat  : ParseRepeatUntil;
        syFor     : ParseFor;
        syAdd1    : ParseAddOne;
        syDec1    : ParseAddOne;
        syBecome  : ParseBecome;
        syPuts    : CurCodes.LoadToken(FLast);
        syImport  : ParseImport;
        syOption  : ParseOption;
        syInclude : ParseInclude;
        else        ParseAny;
      end;
    end;
  finally
    FExpr.Clear;
    FSymbols.Clear;
  end;
end;

procedure KLiParser.ParseWhile;
var
  bl, cl: string;
begin
// --------------------------------------------
// BLOCK: while condition do
//          ...
//        end
// --------------------------------------------
// CONTINUE_LABEL:
//      <condition>
//      IF TRUE GOTO BODY
//      GOTO BREAK_LABEL
// BODY_LABEL:
//      <statements>
//      GOTO CONTINUE_LABEL
// BREAK_LABEL:
// --------------------------------------------
  SaveLabels(bl, cl, true);
  try
    CurCodes.AddLabel(FContinueLabel, FLast^.Pos);
    FExpr.Clear;
    ParseExpr(FExpr, [syDo], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddGoto(FBreakLabel, FLast^.Pos)^.Sym := syGotoFP;
    SymGotoNext;
    if FLast^.Sym = syDot2 then
      ParseStatement else
      ParseBlock([syEnd], true);
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
    CurCodes.AddLabel(FBreakLabel, FLast^.Pos);
  finally
    RestoreLabels(bl, cl);
  end;
end;

procedure KLiParser.ParseIf;
var
  or_else, end_if: string;
  on_if, on_dot2: boolean;
begin
  on_if := true;
  on_dot2 := false;
  end_if := FModule.NewLabelName;
  while FLast^.Sym <> syEnd do
  begin
    or_else := FModule.NewLabelName; // or else label
    FExpr.Clear;
    ParseExpr(FExpr, [syThen], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddGoto(or_else, FLast^.Pos)^.Sym := syGotoFP;
    if on_if then
    begin
      on_if := false;
      SymGotoNext;
      if FLast^.Sym = syDot2 then
      begin
        on_dot2 := true;
        ParseStatement;
      end
      else ParseBlock([syEnd, syElse, syElseIf], true);
    end
    else ParseBlock([syEnd, syElse, syElseIf], false);
    CurCodes.AddGoto(end_if, FLast^.Pos);
    CurCodes.AddLabel(or_else, FLast^.Pos);
    if on_dot2 then Break else
    if FLast^.Sym = syElse then
      ParseBlock([syEnd], false);
  end;
  CurCodes.AddLabel(end_if, FLast^.Pos);
end;

procedure KLiParser.ParseBreak;
var
  end_if: string;
begin
  if FBreakLabel = '' then
    Error.BreakNoLoop(Self);
  SymTestNext([syIf, syDotComma]);
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syDotComma);
    CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
    EndTailIf(end_if);
  end
  else CurCodes.AddGoto(FBreakLabel, FLast^.Pos);
end;

procedure KLiParser.ParseClass;
var
  curr: KLiClass;
  newf: KLiFunc;
  func: RLseFuncRec;
  S, T: string;
  varb: KLiVarb;
  index: integer;

  procedure parse_method;
  var
    clss: KLiClass;
    data: PLiToken;
  begin
    SymTestNextPureID;
    if curr.FindInside(FLast^.Val) then
      Error.Redeclared(Self);
    data := FLast;

    SymTestNext([syDot2, syBOr]);
    if FLast^.Sym = syDot2 then
    begin
      SymTestNext([syID]);
      ParseVarType(clss, true);
      SymTestNext([syBOr]);
    end
    else clss := KT_VARIANT;

    FCurrent := KLiFunc.Create(curr, clss, data^.Val, '', nil, nil, cmMethod);
    FCurrent.AddParam('my', curr);

    ParseArguments(FCurrent, [syBOr], false);
    ParseBlock([syEnd], false);

    curr.DispSingleMethod(FCurrent);
  end;

begin
  SymTestNextPureID;
  if not FModule.CanDeclare(FLast^.Val) then
    Error.Redeclared(Self);
    
  curr := KLiClass.Create(FModule, FLast^.Val, LSV_OBJECT);
  curr.FClassRec.lysee_class := curr;
  curr.FClassRec.name := pchar(curr.FName);
  curr.FClassRec.incRefcount := @lse_incRefcount;
  curr.FClassRec.decRefcount := @lse_decRefcount;
  curr.SetState(clsHashed, true);
  curr.SetState(clsUDC, true);

  S := Format('%s %s()', [FLast^.Val, FLast^.Val]);
  T := Format('create %s object', [FLast^.Val]);
  func.fr_prot := pchar(S);
  func.fr_addr := @udc_create;
  func.fr_desc := pchar(T);
  newf := curr.SetupMethod(@func);

  SymTestNext([syBOr]);
  curr.FInitFunc := KLiFunc.Create(curr, KT_VARIANT, '@', '', nil, nil, cmMethod);
  curr.FInitFunc.AddParam('my', curr);
  ParseArguments(curr.FInitFunc, [syBOr], false);
  for index := 1 to curr.FInitFunc.ParamCount - 1 do
  begin
    varb := curr.FInitFunc.Params[index];
    newf.AddParam(varb.FName, varb.FType);
  end;
  SymGotoNext;

  while FLast^.Sym <> syEnd do
  begin
    if FLast^.Sym <> syDefine then
    begin
      FCurrent := curr.FInitFunc;
      ParseStatement;
    end
    else parse_method;
    SymGotoNext;
  end;

  if curr.FCmGetPV = nil then
  begin
    func.fr_prot := 'variant getpv(string name)';
    func.fr_addr := @udc_getpv;
    func.fr_desc := 'get property by name';
    curr.SetupMethod(@func);
  end;

  if curr.FCmSetPV = nil then
  begin
    func.fr_prot := 'void setpv(string name, variant value)';
    func.fr_addr := @udc_setpv;
    func.fr_desc := 'set property by name';
    curr.SetupMethod(@func);
  end;
end;

procedure KLiParser.ParseConst;
var
  clss: KLiClass;
  data: PLiToken;
begin
  SymTestNextPureID;
  if not FModule.CanDeclare(FLast^.Val) then
    Error.Redeclared(Self);
  data := FLast;

  SymGotoNext;
  if FLast^.Sym = syDot2 then
  begin
    SymTestNext([syID]);
    ParseVarType(clss, true);
    SymGotoNext;
  end
  else clss := KT_VARIANT;

  SymTestLast([syBecome]);
  SymGotoNext;
  
  FCurrent := KLiFunc_curry.CreateConst(FModule, data^.Val, clss);
  FCurrent.FCodes.FPos := data^.Pos;
  
  if FLast^.Sym = syDo then
  begin
    ParseBlock([syEnd, syDefine, syConst, syImport, syEOF], false);
    if FLast^.Sym <> syEnd then
      FTokenizer.DupCurrentToken;
  end
  else
  begin
    FExpr.Clear;
    ParseExpr(FExpr, [syDotComma], true);
    FLast^.Sym := syReturn;
    FLast^.VParamCount := 1;
    FExpr.Add(FLast);
    FCurrent.FCodes.LoadExpr(FExpr);
  end;
end;

procedure KLiParser.ParseContinue;
var
  end_if: string;
begin
  if FContinueLabel = '' then
    Error.ContinueNoLoop(Self);
  SymTestNext([syIf, syDotComma]);
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syDotComma);
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
    EndTailIf(end_if);
  end
  else CurCodes.AddGoto(FContinueLabel, FLast^.Pos);
end;

procedure KLiParser.ParseRepeatUntil;
var
  bl, cl, body: string;
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
//      IF TRUE GOTO BREAK_LABEL
//      GOTO BODY_LABEL
// BREAK_LABEL:
// --------------------------------------------
  SaveLabels(bl, cl, true);
  try
    body := FModule.NewLabelName;
    CurCodes.AddLabel(body, FLast^.Pos);
    ParseBlock([syUntil], false);
    CurCodes.AddLabel(FContinueLabel, FLast^.Pos);
    FExpr.Clear;
    ParseExpr(FExpr, [syDotComma], false);
    CurCodes.LoadExpr(FExpr);
    CurCodes.AddGoto(body, FLast^.Pos)^.Sym := syGotoFP;
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
  
  if FLast^.Sym = syDotComma then
  begin
    setup_return(false);
    Exit;
  end;

  FExpr.Clear;
  
  if FLast^.Sym = syIf then
  begin
    BeginTailIf(end_if, syDotComma);
    setup_return(false);
    EndTailIf(end_if);
    Exit;
  end;
  
  ParseExpr(FExpr, [syDotComma, syIf], true);
  if FLast^.Sym = syDotComma then
  begin
    CurCodes.LoadExpr(FExpr);
    setup_return(true);
    Exit;
  end;
  
  BeginTailIf(end_if, syDotComma);
  CurCodes.LoadExpr(FExpr);
  setup_return(true);
  EndTailIf(end_if);
end;

procedure KLiParser.ParseFor;
var
  index, vgcount: integer;
  list: array of record
          varb: KLiVarb;
          vrec: PLiToken
        end;
  varb: KLiVarb;
  vrec, in_rec: PLiToken;
  bl, cl: string;
  expr: PLiExprRec;
begin
// --------------------------------------------
// BLOCK: for v1,v2,..,v? in vargen if condition do
//          ...
//        end
// --------------------------------------------
//      SAVE vargen TO TV
// CONTINUE_LABEL:
//      PUSH TV.vargen
//      CALL vargen.eof
//      IF TRUE GOTO BREAK_LABEL
//      CALL condition
//      IF FALSE GOTO CONTINUE_LABEL
//      SET variable = vargen.next()
//      <statements>
//      GOTO CONTINUE_LABEL
// BREAK_LABEL:
// --------------------------------------------

  SaveLabels(bl, cl, true);
  try
    // 1. parse variable list
    index := 0;
    repeat
      ParseVarb(varb, vrec, [syComma, syIn, syIf, syDo], false);
      SetLength(list, index + 1);
      list[index].varb := varb;
      list[index].vrec := vrec;
      Inc(index);
    until FLast^.Sym in [syIn, syIf, syDo];

    // 2. save in keyword
    in_rec := FLast;

    // 3. setup run in range
    expr := CurCodes.AddGoto(FBreakLabel, in_rec^.Pos);
    expr^.Sym := syRINR;

    // 4. change next expression into temp vargen
    CurCodes.BeginStatement(in_rec^.Pos);
    if in_rec^.Sym in [syIf, syDo] then
    begin
      for index := 0 to Length(list) - 1 do
        CurCodes.PushVarb(list[index].varb, list[index].vrec^.Pos);
      vgcount := Length(list);
    end
    else
    begin
      vgcount := 0;
      repeat
        FExpr.Clear;
        ParseExpr(FExpr, [syComma, syIf, syDo], false);
        CurCodes.LoadExpr(FExpr);
        Inc(vgcount);
      until FLast^.Sym in [syIf, syDo];
    end;
    expr := CurCodes.AddNew(syVarGen, @(in_rec^.Pos));
    expr^.ParamCount := vgcount;

    // 5. save to runner.FTempValue
    CurCodes.AddNew(sySETV, @(in_rec^.Pos));

    // 6. setup continue label
    CurCodes.AddLabel(FContinueLabel, FLast^.Pos);

    // 7. send values to variables
    for index := 0 to Length(list) - 1 do
    begin
      // a. push vargen
      CurCodes.AddNew(syGETV, @(FLast^.Pos));

      // b. push variable
      expr := CurCodes.AddNew(syPushVarb, @(list[index].vrec^.Pos));
      expr^.VVarb := list[index].varb;
      expr^.flags := expr^.flags + [xrSatisfied];

      // c. send value to varible
      expr := CurCodes.AddNew(syCall, @(in_rec^.Pos));
      expr^.Name := sys_vargen_send.FName;
      expr^.VFunc := sys_vargen_send;
      expr^.ParamCount := 2;
      expr^.flags := expr^.flags + [xrSatisfied];

      // d. break if false
      expr := CurCodes.AddGoto(FBreakLabel, in_rec^.Pos);
      expr^.Sym := syGotoFP;
    end;

    // 7. check condition
    if FLast^.Sym = syIf then
    begin
      FExpr.Clear;
      ParseExpr(FExpr, [syDo], false);
      CurCodes.LoadExpr(FExpr);
      expr := CurCodes.AddGoto(FContinueLabel, in_rec^.Pos);
      expr^.Sym := syGotoFP;
    end;
    
    // 8. parse body
    SymGotoNext;
    if FLast^.Sym = syDot2 then
      ParseStatement else
      ParseBlock([syEnd], true);

    // 9. continue
    CurCodes.AddGoto(FContinueLabel, FLast^.Pos);

    // A. setup break label
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
  is_lib, is_str, is_lsp: boolean;
  curr: KLiModule;

  function check_conflict(module: KLiModule): boolean;
  begin
    Result := (module <> nil);
    if Result and is_str and not __sameFileName(f_name, module.FFileName) then
      Error.ModuleReimport(Self);
  end;

begin
  lock_kernel;
  try
    repeat
      // 1. parse module name
      FLast := FSymbols.Next;
      if not FTokenizer.GetFileNameToken(FLast) then
        SymTestLast([syID, syStr]);

      is_str := (FLast^.Sym = syStr);
      if is_str then
      begin
        f_name := lse_veryPD(Trim(__ExpandValue(FLast^.Val, FModule.Engine)));
        f_name := __fullFileName(f_name, ExtractFilePath(FModule.FFileName));
        if not FileExists(f_name) then
          Error.ModuleNotFound(Self);
        m_name := ExtractFileExt(f_name);
        is_lib := AnsiSameText(m_name, LSE_DLLEXT);
        is_lsp := not is_lib and not AnsiSameText(m_name, '.ls');
        m_name := ChangeFileExt(ExtractFileName(f_name), '');
      end
      else
      begin
        m_name := FLast^.Val;
        is_lib := false;
        is_lsp := false;
      end;

      // 2. check conflict
      if check_conflict(FModule.FindModule(m_name, false)) then
        goto NEXT;

      if FModule.Declared(m_name) then
        Error.ModuleReimport(Self);

      curr := KLiModule(__findNamed(sys_libraries, m_name));
      if check_conflict(curr) then
      begin
        FModule.FModules.Add(curr);
        curr.ImportNotification;
        goto NEXT;
      end;

      curr := FModule.FEngine.FModules.Find(m_name);
      if check_conflict(curr) then
      begin
        if curr.Parsing then
          Error.ImportEachOther(Self, curr);
        curr.AddImporter(FModule);
        FModule.FModules.Add(curr);
        curr.ImportNotification;
        goto NEXT;
      end;


      // 3. search module file
      if not is_str then
      begin
        f_name := m_name;
        if not __searchModule(f_name, FModule.FEngine.GetSearchPath, is_lib) then
          Error.ModuleNotFound(Self);
        is_lsp := not is_lib and not AnsiSameText(ExtractFileExt(f_name), '.ls');
      end;

      // 4. load module
      if is_lib then
      begin
        curr := __loadLibrary(m_name, f_name);
        if curr = nil then
          Error.WrongLibrary(Self);
        FModule.FModules.Add(curr);
        curr.ImportNotification;
      end
      else
      begin
        curr := KLiModule.Create(m_name, FModule.FEngine, moyScript);
        curr.FFileName := f_name;
        curr.Parsing := true;
        KLiParser.Create(curr).ParseAndFree(__fileText(f_name), is_lsp);
        curr.Parsing := false;
        curr.AddImporter(FModule);
        FModule.FModules.Add(curr);
        curr.ImportNotification;
      end;

      NEXT:
      SymTestNext([syComma, syDotComma]);
    until FLast^.Sym = syDotComma;
  finally
    unlock_kernel;
  end;
end;

procedure KLiParser.ParseOption;
var
  cmd: string;

  procedure option_load;
  var
    ID, value, section, line: string;
    index, slen: integer;
    list: KLiStrlist;
  begin
    FLast := FSymbols.Next;
    if not FTokenizer.GetFileNameToken(FLast) then
      SymTestLast([syID, syStr]);
    value := lse_veryPD(Trim(__ExpandValue(FLast^.Val, FModule.Engine)));
    if FileExists(value) then
    begin
      list := KLiStrlist.Create;
      try
        list.LoadFromFile(value);
        section := '';
        for index := 0 to list.Count - 1 do
        begin
          line := __deleteConfigComment(list[index]);
          slen := Length(line);
          if slen > 2 then
            if (line[1] = '[') and (line[slen] = ']') then
              section := Trim(Copy(line, 2, slen - 2)) else
            if __parseConfig(line, ID, value) then
            begin
              value := __ExpandValue(value, FModule.Engine);
              if section <> '' then
                ID := section + '.' + ID;
              FModule.FEngine.FMainValues.SetStrByChain(ID, value);
            end
        end;
      finally
        list.Free;
      end;
      SymTestNext([syRBlock]);
    end
    else Error.FileNotFound(Self);
  end;

  procedure option_set;
  var
    ID, value: string;
  begin
    FLast := FSymbols.Next;
    if not FTokenizer.GetValueNameToken(FLast) then
      SymTestLast([syStr]);
    ID := FLast^.Val;
    FLast := FSymbols.Next;
    if not FTokenizer.GetOptionValueToken(FLast) then
      SymTestLast([syStr]);
    value := Trim(__ExpandValue(FLast^.Val, FModule.Engine));
    if (value <> '') and (cmd <> 'set') then
    begin    
      value := lse_expand_fname(value);
      if cmd = 'set::path' then
        value := IncludeTrailingPathDelimiter(value) else
      if Length(value) > 1 then
        value := ExcludeTrailingPathDelimiter(value);
    end;
    FModule.FEngine.FMainValues.SetStrByChain(ID, value);
    SymTestNext([syRBlock]);
  end;

  procedure option_search;
  var
    path: string;
  begin
    FLast := FSymbols.Next;
    if not FTokenizer.GetFileNameToken(FLast) then
      SymTestLast([syID, syStr]);
    path := lse_expand_fname(lse_veryPD(Trim(__ExpandValue(
            FLast^.Val, FModule.Engine)))); 
    with FModule.FEngine do
      if FMainSearchPath <> '' then
        FMainSearchPath := FMainSearchPath + ';' + path else
        FMainSearchPath := path;
    SymTestNext([syRBlock]);
  end;

  procedure option_include;
  begin
    FLast := FSymbols.Next;
    if not FTokenizer.GetFileNameToken(FLast) then
      SymTestLast([syID, syStr]);
    ExecInclude(syRBlock);
  end;
    
begin
  SymTestNext([syID]);
  cmd := LowerCase(FLast^.Val);
  if cmd = 'include' then option_include else
  if cmd = 'load' then option_load else
  if cmd = 'search' then option_search else
  if cmd = 'set' then option_set else
  if cmd = 'set::file' then option_set else
  if cmd = 'set::path' then option_set else
    Error.SymUnexpected(Self);
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
    BeginTailIf(F, syDotComma);
    CurCodes.LoadExpr(ExprList);
    EndTailIf(F);
  end
  else CurCodes.LoadExpr(ExprList);
end;

procedure KLiParser.SymGotoNext;
begin
  if not GetNextSym then
    Error.SymNotFound(Self);
end;

procedure KLiParser.ParseTry;
var
  catch_label, leave_label: string;
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
      CurCodes.AddLabel(leave_label, FLast^.Pos);
      leave_label := FModule.NewLabelName;
    end;
    CurCodes.AddLabel(catch_label, FLast^.Pos);
    expr := CurCodes.AddGoto(leave_label, FLast^.Pos);
    expr^.Sym := syRINR;
    ParseBlock([syEnd], false);
    CurCodes.AddLabel(leave_label, FLast^.Pos);
  finally
    Dec(FCatchCount);
  end;
end;

procedure KLiParser.ParseAddOne;
var
  R, M, L: PLiToken;
  F: string;
  S: KLiSymbol;
begin
  M := FLast;
  if M^.Sym = syAdd1 then
    M^.Sym := syAdd else
    M^.Sym := syDec;

  FExpr.Clear;
  SymGotoNext;
  S := PeekNextSym;

  if IsPureID(FLast) and (S in [syDotComma, syIf]) then
  begin
    // ++ a ==> set a += 1
    R := FLast;
    FExpr.Add(R);

    L := CloneSym(M);
    L^.Sym := syInt;
    L^.VInteger := 1;
    FExpr.Add(L);

    FExpr.Add(M);

    L := CloneSym(R);
    L^.Sym := syBecome;
    FExpr.Add(L);

    SymTestNext([syIf, syDotComma]);
  end
  else
  if (FLast^.Sym = syGetValue) and (S in [syDotComma, syIf]) then
  begin
    // ++ ${a} ==> set ${a} += 1
    R := FLast;
    FExpr.Add(R);

    L := CloneSym(M);
    L^.Sym := syInt;
    L^.VInteger := 1;
    FExpr.Add(L);

    FExpr.Add(M);

    L := CloneSym(R);
    L^.Sym := sySetValue;
    FExpr.Add(L);

    SymTestNext([syIf, syDotComma]);
  end
  else
  begin
    ParseExpr(FExpr, [syDotComma, syIf], true);
    R := FExpr.Last;

    if R^.Sym <> syCall then
    begin
      FLast := R;
      Error.SymExpected(Self, SymsToStrList([syCall]));
    end;

    F := Copy(R^.Val, 1, 7);
    if (F <> '___get:') and (F <> '___giv:') then
    begin
      FLast := R;
      Error.SymExpected(Self, SymsToStrList([syCall]) + ': ___get or ___giv');
    end;

    // ++ a.b ==> set a.b += 1

    L := CloneSym(R);
    FExpr.Add(L);

    R^.Sym := syDupLast;
    R := CloneSym(L);

    L := CloneSym(M);
    L^.Sym := syInt;
    L^.VInteger := 1;
    FExpr.Add(L);

    FExpr.Add(M);

    FExpr.Add(R);

    Inc(R^.VParamCount);
    if F = '___get:' then
      R^.Val := '___Set:' + Copy(R^.Val, 8, MaxInt) else
    if F = '___giv:' then
      R^.Val := '___siv:' + Copy(R^.Val, 8, MaxInt);
  end;

  TailIf(FExpr);
end;

function KLiParser.ParseAndFree(const Code: string; IsLsp: boolean): KLiFunc;
begin
  try
    Result := Parse(Code, IsLsp);
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
  F: string;
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
    ParseExpr(FExpr, [syIf, syDotComma], false);
    if S in OPRS then
      FExpr.Add(M);
    FExpr.Add(R);
    R^.Sym := syBecome;
  end
  else
  if (FLast^.Sym = syGetValue) and ((S in [syBecome]) or ((S in OPRS) and (X = syBecome))) then
  begin
    R := FLast;
    SymGotoNext;
    S := FLast^.Sym;
    if S in OPRS then
    begin
      M := FLast;
      SymTestNext([syBecome]);
      FExpr.Add(CloneSym(R));
    end;
    ParseExpr(FExpr, [syIf, syDotComma], false);
    if S in OPRS then
      FExpr.Add(M);
    FExpr.Add(R);
    R^.Sym := sySetValue;
  end
  else
  begin
    ParseExpr(FExpr, [syBecome, syIf, syDotComma] + OPRS, true);
    S := FLast^.Sym;
    if S in [syBecome] + OPRS then
    begin
      R := FExpr.Last;

      if R^.Sym <> syCall then
      begin
        FLast := R;
        Error.SymExpected(Self, SymsToStrList([syCall]));
      end;

      F := Copy(R^.Val, 1, 7);
      if (F <> '___get:') and (F <> '___giv:') then
      begin
        FLast := R;
        Error.SymExpected(Self, SymsToStrList([syCall]) + ': ___get or ___giv');
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

      ParseExpr(FExpr, [syDotComma, syIf], false);

      if S in OPRS then
        FExpr.Add(M);

      FExpr.Add(R);

      Inc(R^.VParamCount);
      if F = '___get:' then
        R^.Val := '___Set:' + Copy(R^.Val, 8, MaxInt) else
      if F = '___giv:' then
        R^.Val := '___siv:' + Copy(R^.Val, 8, MaxInt);
    end;
  end;

  TailIf(FExpr);
end;

procedure KLiParser.ParseArguments(Func: KLiFunc; EndSym: KLiSymbols; OnHead: boolean);
var
  L: PLiToken;
  V: KLiVarb;
  T: KLiClass;
begin
  if not OnHead then SymGotoNext;
  
  if (FLast^.Sym = syMul) and (Func.FParams.Count = 0) then
  begin
    V := Func.AddParam('my', KT_VARLIST);
    V.FPos := Flast^.Pos;
    Func.ExpandThis := true;
    SymTestNext([syComma] + EndSym);
    if FLast^.Sym in EndSym then Exit else SymTestNext([syID]);
  end;

  if not (FLast^.Sym in EndSym) then
  begin
    SymTestLastPureID;
    while true do
    begin
      if not Func.CanDeclare(FLast^.Val) then
        Error.Redeclared(Self);
      L := FLast;
      T := KT_VARIANT;
      SymTestNext(EndSym + [syComma, syDot2]);
      if FLast^.Sym = syDot2 then
      begin
        SymTestNext([syID]);
        ParseVarType(T, true);
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
var
  JT, JF, JP: PLiToken;
  TX, FX: integer;

  function IsOperID(Sym: KLiSymbol): boolean;
  var
    M: KLiSymbol;
  begin
    Result := (Sym in OperIDSyms);
    if Result then
    begin
      M := PeekNextSym;
      if Sym = syBOr then
        Result := not (M in [syMul, syAsk, syID, syBOr]) else
      if not (M in OperIDSyms + EndSyms) then
        if Sym = syDec then
          Result := M in ExprEndSyms else
          Result := M in ExprEndSyms + [syLParen];
    end;
  end;
  
  procedure parse_term;
  var
    L, V: PLiToken;
    X: integer;
  begin
    SymTestLast(ExprHeadSyms);
    if IsOperID(FLast^.Sym) then
    begin
      FLast^.Val := lse_symbol.Symbols[FLast^.Sym].ID;
      FLast^.Sym := syID;
    end;
    L := FLast;
      
    if L^.Sym = syBOr then
    begin
      if PeekNextSym = syAsk then
      begin
        SymGotoNext;
        SymTestNext([syComma, syBOr]);
        if FLast^.Sym = syBOr then
          FTokenizer.DupCurrentToken else
          FLast^.Sym := syBOr;
        ParseLambda(true);
      end
      else ParseLambda(false);
      SymGotoNext;
      ParseAsk(Expr);
    end
    else
    begin
      SymGotoNext;
      if (L^.Sym = syDec) and (FLast^.Sym in [syFloat, syMoney, syInt]) then // 拼负数
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
        if L^.Sym = syGetValue then
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
      if L^.Sym = syLArray then // varlist
      begin
        L^.Sym := syVarlist;
        L^.VParamCount := 0;
        while FLast^.Sym <> syRArray do
        begin
          X := Expr.Count;
          ParseExpr(Expr, [syDot2, syComma, syRArray], L^.VParamCount = 0);
          if FLast^.Sym <> syDot2 then
          begin
            V := CloneSym(FLast);
            V^.Sym := syNil;
            V^.Val := 'nil';
            Expr.Insert(X, V);
          end
          else ParseExpr(Expr, [syComma, syRArray], false);
          Inc(L^.VParamCount, 2);
        end;
        SymGotoNext;
        Expr.Add(L);
      end
      else
      if L^.Sym = syLBlock then // hashed
      begin
        L^.Sym := syHashed;
        L^.VParamCount := 0;
        while FLast^.Sym <> syRBlock do
        begin
          ParseExpr(Expr, [syDot2, syRBlock], L^.VParamCount = 0);
          ParseExpr(Expr, [syComma, syRBlock], false);
          Inc(L^.VParamCount, 2);
        end;
        SymGotoNext;
        Expr.Add(L);
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
  if not (FLast^.Sym in EndSyms) then
    if FLast^.Sym = syAsk then
    begin
      JF := FLast;
      JF^.Sym := syJmpFPop;
      FX := Expr.Add(JF);
      ParseExpr(Expr, [syDot2], false);
      JT := FLast;
      JT^.Sym := syJump;
      TX := Expr.Add(JT);
      JP := CloneSym(JT);
      JP^.Sym := syPress;
      Expr.Add(JP);
      JF^.VParamCount := Expr.Count - FX;
      ParseExpr(Expr, EndSyms, false);
      JT^.VParamCount := Expr.Count - TX;
    end
    else SymTestLast(EndSyms);
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
    CurCodes.AddGoto(end_if_label, FLast^.Pos)^.Sym := syGotoFP;
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
  clss: KLiClass;
  data: PLiToken;
begin
  SymTestNextPureID;
  if not FModule.CanDeclare(FLast^.Val) then
    Error.Redeclared(Self);
  data := FLast;

  SymGotoNext;
  if FLast^.Sym = syDot2 then
  begin
    SymTestNext([syID]);
    ParseVarType(clss, true);
    SymGotoNext;                               
  end
  else clss := KT_VARIANT;

  FCurrent := KLiFunc.Create(FModule.ModuleClass, clss,
    data^.Val, '', nil, nil, cmNormal);
  CurCodes.FPos := data^.Pos;
  
  if FLast^.Sym = syBOr then
  begin
    ParseArguments(FCurrent, [syBOr], false);
    SymGotoNext;
  end
  else FCurrent.IsNameCall := true;
  
  ParseBlock([syEnd, syDefine, syConst, syImport, syEOF], true);
  if FLast^.Sym <> syEnd then
    FTokenizer.DupCurrentToken;
end;

procedure KLiParser.ParseDotItem(Expr: TList);
var
  L: PLiToken;
begin
  while FLast^.Sym in [syDot, syLArray] do
  begin
    L := FLast;
    if FLast^.Sym = syLArray then
    begin
      ParseExpr(Expr, [syRArray], false);
      Expr.Add(L);
      L^.Sym := syCall;
      L^.Val := '___giv:';
      L^.VParamCount := 2;
    end
    else
    begin
      SymTestNextPureID;
      L := FLast;
      L^.Sym := syCall;
      L^.VParamCount := 1;
      if PeekNextSym = syLParen then
      begin
        L^.Val := '___run:' + L^.Val;
        SymTestNext([syLParen]);
        SymGotoNext;
        if FLast^.Sym <> syRParen then
        begin
          ParseExpr(Expr, [syRParen, syComma], true);
          Inc(L^.VParamCount);
          while FLast^.Sym = syComma do
          begin
            ParseExpr(Expr, [syRParen, syComma], false);
            Inc(L^.VParamCount);
          end;
        end;
      end
      else L^.Val := '___get:' + L^.Val;
      Expr.Add(L);
    end;
    SymGotoNext;
    ParseAsk(Expr);
  end;
end;

procedure KLiParser.ParseBecome;
var
  L: PLiToken;
begin
  FExpr.Clear;
  L := FLast;
  L^.VParamCount := 0;
  repeat
    SymGotoNext;
    ParseExpr(FExpr, [syIf, syDotComma, syComma], true);
    Inc(L^.VParamCount);
  until FLast^.Sym in [syIf, syDotComma];
  FExpr.Add(L);
  L^.Sym := syOut;
  TailIf(FExpr);
end;

procedure KLiParser.EndTailIf(const end_if_label: string);
begin
  CurCodes.AddLabel(end_if_label, FLast^.Pos);
end;

function KLiParser.Error: KLiError;
begin
  Result := FModule.FEngine.FError;
end;

procedure KLiParser.ExecInclude(EndSym: KLiSymbol);
var
  fname, path, TAG: string;
  index: integer;
begin
  fname := lse_veryPD(Trim(__ExpandValue(FLast^.Val, FModule.Engine)));
  SymTestNext([EndSym]);

  index := Pos('@', fname);
  if index > 0 then
  begin
    TAG := Trim(Copy(fname, 1, index - 1));
    fname := Trim(Copy(fname, index + 1, MaxInt));
  end
  else TAG := '';

  if TAG = '' then if FIsLsp then
    TAG := 'LSP' else
    TAG := 'LS';

  path := ExtractFilePath(FModule.FEngine.GetIncludedFile(FLast^.Pos.fid));
  if path = '' then
  begin
    path := ExtractFilePath(FModule.FFileName);
    if path = '' then
      path := GetCurrentDir;
  end;
  
  fname := __fullFileName(fname, path);
  try
    FTokenizer.Include(fname, TAG, FModule.FEngine.AddIncludedFile(fname));
  except
    Error.error(SyntaxError, ESYNTAX, FLast^.Pos.row,
      FLast^.Pos.col, FModule.Name, lse_exception_str,
      IncludedFile);
  end;
end;

procedure KLiParser.ParseSwitch;
var
  end_switch: string;
  next_case: string;
  data: PLiToken;
begin
// --------------------------------------------
// BLOCK: switch expr
//          case expr1: ...
//          case ...  : ...
//          case exprN: ...
//          else        ...
//        end
// --------------------------------------------
  end_switch := FModule.NewLabelName;
  CurCodes.AddGoto(end_switch, FLast^.Pos)^.Sym := syRINR;
  CurCodes.BeginStatement(FLast^.Pos);
  FExpr.Clear;
  ParseExpr(FExpr, [syCase], false);
  CurCodes.LoadExpr(FExpr);
  CurCodes.AddNew(sySETV, @(FLast^.Pos));
  repeat
    data := FLast;
    data^.Sym := syAbsEQ;
    CurCodes.AddNew(syGETV, @(data^.Pos));
    FExpr.Clear;
    ParseExpr(FExpr, [syDot2], false);
    FExpr.Add(data);
    CurCodes.LoadExpr(FExpr);
    next_case := FModule.NewLabelName;
    CurCodes.AddGoto(next_case, FLast^.Pos)^.Sym := syGotoFP;
    ParseBlock([syCase, syElse, syEnd], false);
    CurCodes.AddGoto(end_switch, FLast^.Pos);
    CurCodes.AddLabel(next_case, FLast^.Pos);
    if FLast^.Sym = syElse then
      ParseBlock([syEnd], false);
  until FLast^.Sym = syEnd;
  CurCodes.AddLabel(end_switch, FLast^.Pos);
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
  begin
    Result := token^.Sym;
    if (Result in FEndBlockSyms) and not FToDotComma then
      Result := syDotComma;
  end
  else Result := syError;
end;

function KLiParser.PeekNextTwoSym(var one, two: KLiSymbol): integer;
var
  A, B, C: PLiToken;
begin
  Result := FTokenizer.PeekNextThreeTokens(A, B, C);

  if Result > 0 then
  begin
    one := A^.Sym;
    if (one in FEndBlockSyms) and not FToDotComma then
    begin
      two := one;
      one := syDotComma; // EndSym ==> syDotComma, EndSym
      Result := 2;
      Exit;
    end;
  end
  else one := syError;

  if Result > 1 then
  begin
    two := B^.Sym;
    if (two in FEndBlockSyms) and not FToDotComma then
      two := syDotComma;
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

function KLiParser.GetIncludedFile: string;
begin
  if (FTokenizer <> nil) and (FModule <> nil) then
    Result := FModule.FEngine.GetIncludedFile(FTokenizer.FileID) else
    Result := '';
end;

function KLiParser.GetLastCol: integer;
begin
  Result := FLast^.Pos.col;
end;

function KLiParser.GetLastVal: string;
begin
  Result := FLast^.Val;
end;

function KLiParser.ParseLambdaFunc(ExpandThis: boolean): KLiFunc;
begin
//SymTestLast([syBOr]);
  Result := FModule.NewFunc;
  Result.FCodes.FPos := FLast^.Pos;
  Result.IsLambdaFunc := true;
  FCurrent := Result;
  if ExpandThis then
  begin
    Result.AddParam('my', KT_VARLIST).FPos := Flast^.Pos;
    Result.ExpandThis := true;
  end;
  ParseArguments(Result, [syBOr], false);
  ParseBlock([syEnd], false);
end;

function KLiParser.Shadow: KLiParser;
begin
  Result := KLiParser.Create(FModule);
  Result.FTokenizer := FTokenizer;
  Result.FModule := FModule;
  Result.FCurrent := FCurrent;
  Result.FIsLsp := FIsLsp;
  Result.FIsShadow := true;
  Result.FLast := FLast;
end;

// ----------------------------------------------------
// interface interface_name {
//   [public] methods
// }
// ----------------------------------------------------
procedure KLiParser.ParseInclude;
begin
  FLast := FSymbols.Next;
  if not FTokenizer.GetFileNameToken(FLast) then
    SymTestLast([syID, syStr]);
  ExecInclude(syDotComma);
end;

function KLiParser.ParseLambda(ExpandThis: boolean): KLiFunc;
var
  parser: KLiParser;
  expr: PLiToken;
begin
  if ExpandThis then
  begin
    expr := CloneSym(FLast);
    expr^.Sym := syID;
    expr^.Val := sys_curryone_func.FullName;
    expr^.Pos := FLast^.Pos;
    FExpr.Add(expr); // 1
  end;

  parser := Shadow;
  try
    Result := parser.ParseLambdaFunc(ExpandThis);
    FLast := CloneSym(parser.FLast);
    FLast^.Sym := syID;
    FLast^.Val := Result.FullName;
    FExpr.Add(FLast); // 2
  finally
    parser.Free;
  end;

  if ExpandThis then
  begin
    expr := CloneSym(FLast);
    expr^.Sym := syThis;
    FExpr.Add(expr); // 3
    expr := CloneSym(FLast);
    expr^.Sym := syAsk;
    expr^.VParamCount := 3; // curryOne(function, this)
    FExpr.Add(expr);
  end;
end;

{ KLiVarb }

constructor KLiVarb.Create(AList: KLiVarbList; const Name: string;
  ValueType: KLiClass);
begin
  IncRefcount;
  FName := Name;
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

function KLiVarb.IsThis: boolean;
begin
  Result := (FIndex = 0) and IsParam and FList.FFunc.IsClassMethod;
end;

function KLiVarb.IsParam: boolean;
begin
  Result := (FList <> nil) and FList.IsParam;
end;

function KLiVarb.Prototype(HideType: boolean): string;
begin
  if HideType then
    Result := FName else
    Result := ValueType.Prototype(FName);
end;

{ KLiVarbList }

function KLiVarbList.Add(const Name: string; ValueType: KLiClass): KLiVarb;
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
    if GetVarb(Result).FName = Name then Exit;
    Dec(Result);
  end;
end;

function KLiVarbList.IsParam: boolean;
begin
  Result := (FFunc <> nil) and (Self = FFunc.FParams);
end;

function KLiVarbList.ToString(HideType, HideThis: boolean): string;
var
  A, index: integer;
begin
  Result := '';
  if GetCount > 0 then
  begin
    if HideThis and GetVarb(0).IsThis then
      index := 1 else
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
    Result.PushObject(GetVarb(index), KT_VARIABLE);
end;

{ KLiFunc }

function KLiFunc.AddParam(const Name: string; varType: KLiClass): KLiVarb;
begin
  Result := FParams.Add(Name, varType);
end;

function KLiFunc.CanInvoke(func: KLiFunc): boolean;
begin
  Result := (func <> nil);
end;

constructor KLiFunc.Create(AOwnerClass, AResultType: KLiClass;
  const Name, Desc: string;
  Params: TStringList; Proc: pointer; Kind: KLiMethodType);
var
  A: integer;
begin
  IncRefCount;
  FOwnerClass := AOwnerClass;
  FModule := FOwnerClass.Module;
  FResultType := AResultType;
  FKind := Kind;
  if Name = '' then
    FName := FModule.NewFuncName else
    FName := Name;
  case FKind of
    cmCreator: FOwnerClass.FCmCreator := Self;
    cmCount  : FOwnerClass.FCmCount := Self;
    cmGetAt  : FOwnerClass.FCmGetAt := Self;
    cmSetAt  : FOwnerClass.FCmSetAt := Self;
    cmGetPV  : FOwnerClass.FCmGetPV := Self;
    cmSetPV  : FOwnerClass.FCmSetPV := Self;
  end;
  FOwnerClass.FCmFuncs.AddObject(FName, Self);
  FDescription := Desc;
  FParams := KLiVarbList.Create(Self);
  if Params <> nil then
    for A := 0 to Params.Count - 1 do
      AddParam(Params[A], KLiClass(Params.Objects[A]));

  FProc := Proc;
  if (FProc = nil) and ((FKind = cmNormal) or AOwnerClass.FModule.IsScript) then
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
  __nilWhenSame(FOwnerClass.FCmCreator, Self);
  __nilWhenSame(FOwnerClass.FCmCount,   Self);
  __nilWhenSame(FOwnerClass.FCmGetAt,   Self);
  __nilWhenSame(FOwnerClass.FCmSetAt,   Self);
  __nilWhenSame(FOwnerClass.FCmGetPV,   Self);
  __nilWhenSame(FOwnerClass.FCmSetPV,   Self);
  __removeAllFrom(FOwnerClass.fCmFuncs, Self);

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
    rnnr := KLiRunner(Param^.runner);
    func := KLiFunc(Param^.func);
    if Self <> func then
    begin
      Param^.func := Self;
      invoke_proc;
      Param^.func := func;
    end
    else invoke_proc;
    Result := not (rnnr.Terminated or rnnr.FExcepted);
    if Result then
      __SetClassValue(rnnr.FEngine, Param^.result, FResultType) else
      rnnr.FExcepted := true;
  except
    Result := false;
    __SetError(Param);
  end;
end;

function KLiFunc.FullName: string;
begin
  if FKind = cmNormal then Result := FName else
  if IsConstructor then
    Result := FOwnerClass.FName + '.' + FOwnerClass.FName else
    Result := FOwnerClass.FName + '.' + FName;
  Result := Module.Name + '::' + Result;
end;

procedure KLiFunc.Garbaged;
begin

end;

function KLiFunc.ParamCount: integer;
begin
  Result := FParams.Count;
end;

function KLiFunc.ParamList(HideType, HideThis: boolean): string;
begin
  Result := Params.ToString(HideType, HideThis);
end;

function KLiFunc.Prototype(ShowFullName: boolean): string;
var
  A, X: integer;
  T: KLiClass;
  N: string;
begin
  if ShowFullName then
  begin
    if IsConstructor then
      Result := FOwnerClass.FullName else
    if FResultType = KT_VARIANT then
      Result := FullName else
      Result := FResultType.Prototype(FullName);
  end
  else
  if IsConstructor then
    Result := FOwnerClass.Name else
  if FResultType = KT_VARIANT then
    Result := FName else
    Result := FResultType.Prototype(FName);

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
        Result := Result + '*, ' else
        Result := Result + '*';
    end
    else X := Ord(IsClassMethod);
    if FParams.Count > X then
    begin
      T := FParams[X].FType;
      if T <> KT_VARIANT then
        N := T.Prototype(FParams[X].FName) else
        N := FParams[X].FName;
      Result := Result + N;
      for A := X + 1 to FParams.Count - 1 do
      begin
        T := FParams[A].FType;
        if T <> KT_VARIANT then
          N := T.Prototype(FParams[A].FName) else
          N := FParams[A].FName;
        Result := Format('%s, %s', [Result, N]);
      end;
    end;
    Result := Result + '|';
  end;
end;

function KLiFunc.IsClassMethod: boolean;
begin
  Result := (FKind <> cmNormal);
end;

function KLiFunc.IsConstructor: boolean;
begin
  Result := (FOwnerClass.FCmCreator = Self);
end;

function KLiFunc.IsScript: boolean;
begin
  Result := (FProc = nil);
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
    o_name := __decodeClassName(ID, m_name);
    if (m_name <> '') or (System.Pos('::', ID) > 0) then
      Result := Module.FindBy(o_name, m_name, rec) else
      Result := FindDeclared(o_name, rec) or
                Module.FindBy(o_name, '', rec);
    if Result then
      Result := (Range = []) or (rec^.fo_type in Range);
  end;
end;

function KLiFunc.HasState(Index: KLiFuncState): boolean;
begin
  Result := (Index in FState);
end;

procedure KLiFunc.SetState(Index: KLiFuncState; Value: boolean);
begin
  if Value then
  begin
    Include(FState, Index);
    case Index of
      fusIsMainFunc: Exclude(FState, fusIsInitFunc);
      fusIsInitFunc: Exclude(FState, fusIsMainFunc);
    end;
  end
  else Exclude(FState, Index);
end;

function KLiFunc.GetVarbData(const VarbName: string;
  var Varb: KLiVarb; var Data: PLseValue): boolean;
begin
  Varb := nil;
  Data := nil;
  Result := false;
end;

function KLiFunc.FindInside(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if rec = nil then rec := @findrec;
  rec^.fo_type := foNone;
  
  Result := (ID = FName);
  if Result then
  begin
    rec^.VFunc := Self;
    rec^.fo_type := foFunc;
    Exit;
  end;
  
  rec^.VVarb := FParams.Find(ID);
  if (rec^.VVarb = nil) and (FCodes <> nil) then
    rec^.VVarb := FCodes.FLocals.Find(ID);
  if rec^.VVarb <> nil then
  begin
    rec^.fo_type := foVarb;
    Result := true;
    Exit;
  end;

  Result := false;
end;

function KLiFunc.FindDeclared(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if rec = nil then rec := @findrec;
  Result := FindInside(ID, rec);
end;

function KLiFunc.CanDeclare(const ID: string): boolean;
var
  findrec: KLiFindRec;
begin
  Result := not FindDeclared(ID, @findrec) and
            not __IsReserved(ID, false);
end;

{ KLiExprList }

function KLiExprList.Add(AExprRec: PLiExprRec): integer;
begin
  Result := FItems.Add(AExprRec);
end;

function KLiExprList.AddGoto(const Name: string; Pos: KLiSymPos): PLiExprRec;
var
  index: integer;
begin
  Result := FOwner.FCodes.AddNew(syGoto, @Pos);
  Result^.Name := Name;
  index := Length(FGotos);
  SetLength(FGotos, index + 1);
  FGotos[index] := Result;
end;

function KLiExprList.AddLabel(const Name: string; Pos: KLiSymPos): PLiExprRec;
var
  index: integer;
begin
  Result := FOwner.FCodes.AddNew(syLabel, @Pos);
  Result^.VOffset := GetCount - 1;
  Result^.Name := Name;
  Result^.flags := Result^.flags + [xrSatisfied];
  index := Length(FLabels);
  SetLength(FLabels, index + 1);
  FLabels[index] := Result;
end;

function KLiExprList.AddLocal(const Name: string; varType: KLiClass): KLiVarb;
begin
  Result := FLocals.Add(Name, varType);
  Inc(Result.FIndex, FOwner.FParams.Count);
end;

function KLiExprList.AddTry(const Name: string; Pos: KLiSymPos): PLiExprRec;
begin
  Result := AddGoto(Name, Pos);
  Result^.Sym := syTry;
end;

function KLiExprList.BeginStatement(Pos: KLiSymPos): PLiExprRec;
begin
  if GetCount > 0 then
  begin
    Result := GetLast;
    if Result^.Sym in [syPress] then
    begin
      Result^.Sym := syStatement;
      Exit;
    end;
    if Result^.Sym = syStatement then Exit;
  end;
  Result := AddNew(syStatement, @Pos);
end;

procedure KLiExprList.Clear(Sender: TObject);
var
  index: integer;
begin
  FSatisfyIndex := 0;
  SetLength(FGotos, 0);
  SetLength(FLabels, 0);
  try
    for index := 0 to FItems.Count - 1 do
      __FreeExprec(FItems[index]);
  finally
    FItems.Clear;
  end;
end;

constructor KLiExprList.Create(AFunc: KLiFunc);
begin
  FOwner := AFunc;
  FItems := KLiList.Create;
  FItems.IncRefcount;
  FLocals := KLiVarbList.Create(FOwner);
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
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFPop, syJmpTPop] then
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
  Clear;
  FItems.DecRefcount;
  FItems := nil;
  FLocals.FFunc := nil;
  FLocals.DecRefcount;
  FLocals := nil;
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
  Satisfy;

  for A := 0 to FLocals.Count - 1 do
    Log(Format('     VARB %s: %s',
      [FLocals[A].Name, FLocals[A].ValueType.Name]));
      
  L := TList.Create;
  try
    for A := 0 to GetCount - 1 do
    begin
      R := GetItem(A);
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFPop, syJmpTPop] then
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
        syJmpFPop   : H := Format('JMPF &%.4d: POP', [A + R^.VOffset]);
        syJmpTPop   : H := Format('JMPT &%.4d: POP', [A + R^.VOffset]);
        syCall      : H := Format('CALL %s: %d', [R^.VFunc.FullName, R^.ParamCount]);
        syAsk       : H := Format('CASK [%d]', [R^.ParamCount]);
        syIdle      : H := 'IDLE';
        syPress     : if R^.ParamCount > 1 then
                        H := Format('POP  [%d]', [R^.ParamCount]) else
                        H := 'POP';
        syID        : H := Format('PUSH %s', [R^.VVarb.Name]);
        syBecome    : H := Format('SAVE %s', [R^.VVarb.Name]);
        syFloat     : H := Format('PUSH FLOAT: %f', [R^.VFloat]);
        syMoney     : H := Format('PUSH CURRENCY: %f', [R^.VMoney]);
        syTime      : H := Format('PUSH TIME: %s', [lse_encode_GMT(R^.VTime)]);
        syInt       : H := Format('PUSH %d', [R^.VInteger]);
        syStr       : H := Format('PUSH STRING: %s', [__strToComma(lse_strec_data(R^.VStr))]);
        syChar      : if R^.VChar in [' '..'~'] then
                        H := 'PUSH CHAR ''' + R^.VChar + '''' else
                        H := Format('PUSH CHAR \x%.2x', [integer(R^.VChar)]);
        syTry       : if xrInFinally in R^.flags then
                        H := Format('TRY  FINALLY: %s', [R^.Name]) else
                        H := Format('TRY  CATCH: %s', [R^.Name]);
        syOut       : H := Format('PRNT [%d]', [R^.ParamCount]);
        syNeg       : H := 'CALC NEG';
        syAdd       : H := 'CALC +';
        syAddAll    : H := 'CALC +<';
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
        syLT        : H := 'CALC <';
        syLE        : H := 'CALC <=';
        syGT        : H := 'CALC >';
        syGE        : H := 'CALC >=';
        syNot       : H := 'CALC NOT';
        syAnd       : H := 'CALC AND';
        syOr        : H := 'CALC OR';
        syTrue      : H := 'PUSH TRUE';
        syFalse     : H := 'PUSH FALSE';
        syType      : H := Format('PUSH CLASS: %s', [R^.VType.FullName]);
        syFunc      : H := Format('PUSH FUNC: %s', [R^.VFunc.FullName]);
        syVarlist   : H := Format('LIST [%d]', [R^.ParamCount]);
        syNil       : H := 'PUSH NIL';
        syShell     : H := 'SHLL ' + R^.Name;
        syGetValue  : H := Format('PUSH ${%s}', [R^.Name]);
        sySetValue  : H := Format('SAVE TO ${%s}', [R^.Name]);
        syFormat    : H := 'FRMT';
        syBool      : H := 'TEST'; // TEST IF IS TRUE
        syMethod    : H := Format('CAST %s METHOD [%d]', [R^.VFunc.FullName, R^.ParamCount]);
        syPuts      : H := Format('PRNT STRING: %s', [__strToComma(R^.Name)]);
        syLabel     : H := '[' + R^.Name + ']';
        syGoto      : H := Format('GOTO %s', [R^.Name]);
        syGotoTP    : H := Format('GOTP %s', [R^.Name]);
        syGotoFP    : H := Format('GOFP %s', [R^.Name]);
        syModule    : H := Format('PUSH MODULE: %s', [R^.VType.Module.Name]);
        syIs        : H := 'CALC IS';
        syAs        : H := 'CALC AS';
        syStatement : H := 'STMT';
        syVarGen    : H := Format('VGEN [%d]', [R^.ParamCount]);
        syPushVarb  : H := Format('PUSH VARB %s', [R^.VVarb.Name]);
        syHashed    : H := Format('HASH [%d]', [R^.ParamCount]);
        syCallAsk   : H := Format('CASK [%s][%d]', [R^.Name, R^.ParamCount]);
        syUpto      : H := 'UPTO';
        syAbsEQ     : H := 'CALC ===';
        syRINR      : H := Format('RINR %s', [R^.Name]);
        syGETV      : H := 'GETV';
        sySETV      : H := 'SETV';
        syLike      : H := 'LIKE';
        syGetPV     : H := Format('GETP %s', [R^.Name]);
        sySetPV     : H := Format('SETP %s', [R^.Name]);
        syGetIV     : H := 'GETI';
        syClen      : H := 'PUSH CALLSTACK LENGTH';
        syThis      : H := 'PUSH VARIABLE SNAP';
        syDupLast   : H := 'DUPL';
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
  index: integer;
begin
  for index := 0 to Length(FLabels) - 1 do
  begin
    Result := FLabels[index];
    if Result^.Name = Name then Exit;
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
  Result := GetModule.FEngine;
end;

function KLiExprList.GetError: KLiError;
begin
  Result := GetEngine.FError;
end;

function KLiExprList.GetItem(Index: integer): PLiExprRec;
begin
  Result := PLiExprRec(FItems[Index]);
end;

function KLiExprList.GetLabelCount: integer;
begin
  Result := Length(FLabels);
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
  Result := FOwner.FModule;
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
      if R^.Sym in [syJump, syJmpF, syJmpT, syJmpFPop, syJmpTPop] then
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
  if exprec^.Sym = syChar  then exprec^.VChar  := token^.VChar  else
  if exprec^.Sym = syStr   then
  begin
    exprec^.VStr := lse_strec_alloc(token^.Val);
    lse_strec_inclife(exprec^.VStr);
  end
  else
  if exprec^.Sym in [syJump, syJmpT, syJmpF, syJmpTPop, syJmpFPop] then
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

function KLiExprList.RebuildLabelList: integer;
var
  index, V: integer;
  expr, lrec: PLiExprRec;

  function compare_label(cur_label, new_label: PLiExprRec): integer;
  var
    L, R: integer;
  begin
    // Alpha - Digit =  1  ==> 1 - 0 =  1
    // Alpha - Alpha =  0  ==> 1 - 1 =  0
    // Digit - Alpha = -1  ==> 0 - 1 = -1
    // Digit - Digit =  1  ==> 0 - 0 =  1
    L := Ord(cur_label^.Name[1] in IDHeadChar);
    R := Ord(new_label^.Name[1] in IDHeadChar);
    if L = R then
      Result := 1 - R else
      Result := L - R;
  end;

  procedure change_label(cur_label, new_label: PLiExprRec);
  var
    X: integer;
    G: PLiExprRec;
  begin
    for X := 0 to Length(FGotos) - 1 do
    begin
      G := FGotos[X];
      if G^.Next = cur_label then
      begin
        G^.Next := new_label;
        G^.Name := new_label^.Name;
      end;
    end;
  end;
  
begin
  Result := 0;
  SetLength(FLabels, 0);
  lrec := nil;
  index := 0;
  while index < GetCount do
  begin
    expr := GetItem(index);
    if expr^.Sym in [syLabel, syTry, syGoto, syGotoTP, syGotoFP, syRINR] then
    begin
      expr^.VOffset := index;
      if expr^.Sym = syLabel then
      begin
        if (lrec <> nil) and (lrec^.Sym = syLabel) then
        begin
          V := compare_label(lrec, expr);
          if V > 0 then
          begin
            change_label(expr, lrec);
            expr := lrec;
            FOwner.FCodes.Delete(index);
            Dec(index);
          end
          else
          if V < 0 then
          begin
            change_label(lrec, expr);
            FLabels[Result - 1] := expr;
            FOwner.FCodes.Delete(index - 1);
            Dec(index);
            expr^.VOffset := index - 1;
          end
          else
          begin
            Inc(Result);
            SetLength(FLabels, Result);
            FLabels[Result - 1] := expr;
          end;
        end
        else
        begin
          Inc(Result);
          SetLength(FLabels, Result);
          FLabels[Result - 1] := expr;
        end;
      end;
    end;
    lrec := expr;
    Inc(index);
  end;
end;

procedure KLiExprList.Satisfy;
var
  stack: KLiSatisfy;
  exprec: PLiExprRec;
  exprec_func: KLiFunc;

  procedure exec_push(K: KLiClass);
  begin
    stack.Add(K, exprec, exprec_func);
  end;
  
  procedure exec_last(K: KLiClass; backward: integer);
  begin
    stack.Press(backward);
    exec_push(K);
  end;

  function find_varb: KLiClass;
  label FAILURE;
  var
    vname: string; // variant name
    mname: string; // module name
    R: KLiFindRec;
    found: boolean;
    clss: KLiClass;
    pure: boolean;
  begin
    // 1. find by name
    vname := __decodeClassName(exprec^.Name, mname);
    pure := (mname = '') and (System.Pos('::', exprec^.Name) < 1);
    found := (pure and FOwner.FindDeclared(vname, @R)) or
             FOwner.FModule.FindBy(vname, mname, @R);

    if not found then goto FAILURE;

    // 2. firstly become
    if exprec^.Sym = syBecome then
      if R.fo_type = foVarb then
      begin
        exprec^.VVarb := R.VVarb;
        exprec^.Name := exprec^.VVarb.FName;
        Result := exprec^.VVarb.ValueType;
        Exit;
      end
      else goto FAILURE;

    // 3. push variable value
    if R.fo_type = foVarb then
    begin
      exprec^.VVarb := R.VVarb;
      exprec^.Name := exprec^.VVarb.FName;
      Result := exprec^.VVarb.ValueType;
      Exit;
    end;

    // 2. push class or module
    if R.fo_type = foClass then
    begin
      clss := R.VClass;
      exprec^.VType := clss;
      exprec^.Name := clss.Name;
      if clss.IsModuleClass then
      begin
        exprec^.Sym := syModule;
        Result := KT_MODULE;
      end
      else
      begin
        exprec^.Sym := syType;
        Result := KT_CLASS;
      end;
      Exit;
    end;

    // 3. push function
    if R.fo_type = foFunc then
    begin
      exprec^.VFunc := R.VFunc;
      exprec^.Name := exprec^.VFunc.FName;
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

  FAILURE:
    Result := nil;
    Error.ObjectNotExists(FOwner, exprec);
  end;

  procedure exec_get(otype: KLiClass; const name: string);
  var
    func: KLiFunc;
  begin
    func := otype.FindMethod(cmMethod, name);
    if func <> nil then
    begin
      exprec^.Sym := syMethod;
      exprec^.ParamCount := 1;
      exprec^.VFunc := func;
      exprec^.Name := func.Name;
      exprec_func := func;
      exec_last(KT_FUNC, 1);
    end
    else
    begin
      func := otype.FindGetMethod(name);
      if func = nil then
      begin
        func := otype.SingleMethod(cmGetPv);
        if (func = nil) and (otype = KT_VARIANT) then
          func := sys_getpv_func;
      end;
      if func <> nil then
      begin
        exprec^.Sym := syGetPV;
        exprec^.Name := name;
        exprec^.VFunc := func;
        exprec_func := func;
        exec_last(func.FResultType, 1);
      end
      else
      begin
        exprec^.Name := name;
        Error.FuncNotFound(FOwner, exprec);
      end;
    end;
  end;

  procedure exec_set(otype: KLiClass; const name: string);
  var
    func: KLiFunc;
  begin
    func := otype.FindSetMethod(name);
    if func = nil then
    begin
      func := otype.SingleMethod(cmSetPv);
      if (func = nil) and (otype = KT_VARIANT) then
        func := sys_setpv_func;
    end;
    if func <> nil then
    begin
      exprec^.Sym := sySetPV;
      exprec^.Name := name;
      exprec^.VFunc := func;
      exprec_func := func;
      exec_last(func.FResultType, 2);
    end
    else
    begin
      exprec^.Name := name;
      Error.FuncNotFound(FOwner, exprec);
    end;
  end;

  procedure exec_giv(otype: KLiClass; const name: string);
  var
    func: KLiFunc;
  begin
    exprec^.Name := 'getiv';
    func := otype.SingleMethod(cmGetAt);
    if (func = nil) and (otype = KT_VARIANT) then
      func := sys_getiv_func;
    if func <> nil then
    begin
      exprec^.Sym := syGetIV;
      exprec^.VFunc := func;
      exprec_func := func;
      exec_last(func.FResultType, 2);
    end
    else Error.FuncNotFound(FOwner, exprec);
  end;

  procedure exec_siv(otype: KLiClass; const name: string);
  var
    func: KLiFunc;
  begin
    exprec^.Name := 'setiv';
    func := otype.SingleMethod(cmSetAt);
    if (func = nil) and (otype = KT_VARIANT) then
      func := sys_setiv_func;
    if func <> nil then
    begin
      exprec^.Sym := sySetIV;
      exprec^.VFunc := func;
      exprec_func := exprec^.VFunc;
      exec_last(func.FResultType, 3);
    end
    else Error.FuncNotFound(FOwner, exprec);
  end;

  procedure exec_run(otype: KLiClass; const name: string);
  var
    func: KLiFunc;
  begin
    func := otype.FindMethod(cmMethod, name);
    if func <> nil then
    begin
      exprec_func := func;
      exprec^.VFunc := func;
      exprec^.Name := func.Name;
      exec_last(func.FResultType, exprec^.ParamCount);
      Exit;
    end;
    
    exprec^.Name := name;

    func := otype.FindGetMethod(name);
    if func = nil then
    begin
      func := otype.SingleMethod(cmGetPv);
      if (func = nil) and (otype <> KT_VARIANT) then
        Error.FuncNotFound(FOwner, exprec);
    end;

    exprec_func := func;
    exprec^.VFunc := func;
    exprec^.Sym := syCallAsk;
    exec_last(KT_VARIANT, exprec^.ParamCount);
  end;

  procedure exec_call;
  var
    index: integer;
    clss: KLiClass;
    mode, name: string;
  begin
    if not (xrSatisfied in exprec^.flags) then
    begin
      index := stack.Count - exprec^.ParamCount;
      clss := stack.Types[index];
      mode := Copy(exprec^.Name, 1, 7);
      name := Copy(exprec^.Name, 8, MaxInt);
      if mode = '___get:' then exec_get(clss, name) else
      if mode = '___Set:' then exec_set(clss, name) else
      if mode = '___giv:' then exec_giv(clss, name) else
      if mode = '___siv:' then exec_siv(clss, name) else
      if mode = '___run:' then exec_run(clss, name);
    end
    else
    begin
      exprec_func := exprec^.VFunc;
      exec_last(exprec^.VFunc.FResultType, exprec^.ParamCount);
    end;
  end;

  procedure exec_inc;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_inc(L, R), 2);
  end;

  procedure exec_dec;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_dec(L, R), 2);
  end;

  procedure exec_mul;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_mul(L, R), 2);
  end;

  procedure exec_div;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_div(L, R), 2);
  end;
  
  procedure exec_mod;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_mod(L, R), 2);
  end;

  procedure exec_bit_xor;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_xor(L, R), 2);
  end;

  procedure exec_bit_or;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_or(L, R), 2);
  end;

  procedure exec_bit_and;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_and(L, R), 2);
  end;

  procedure exec_bit_shl;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    exec_last(__type_shl(L, R), 2);
  end;

  procedure exec_bit_shr;
  var
    L, R: KLiClass;
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
    exec_last(KT_BOOL, 2);
  end;

  procedure exec_and_or;
  var
    L, R: KLiClass;
  begin
    L := stack.Types[stack.Count - 2];
    R := stack.Types[stack.Count - 1];
    if L = R then
      exec_last(L, 2) else
      exec_last(KT_VARIANT, 2);
  end;
  
  procedure exec_push_varb;
  var
    R: KLiFindRec;
  begin
    if not (xrSatisfied in exprec^.flags) then
    begin
      if FOwner.FindInside(exprec^.Name, @R) then
        if R.fo_type = foVarb then
        begin
          exprec^.VVarb := R.VVarb;
          exec_last(KT_VARIABLE, 1);
          Exit;
        end;
        Error.ObjectNotExists(FOwner, exprec);
    end
    else exec_last(KT_VARIABLE, 1);
  end;

  procedure exec_goto;
  var
    expr: PLiExprRec;
  begin
    if exprec^.Sym in [syGotoTP, syGotoFP] then
      stack.Press(1);
    expr := FindLabel(exprec^.Name);
    if expr = nil then
      Error.LabelNotExists(FOwner, exprec);
    exprec^.Next := expr;
    exprec^.VOffset := FSatisfyIndex;
  end;

  procedure exec_label;
  begin
    exprec^.VOffset := FSatisfyIndex;
    stack.Clear;
  end;
  
  procedure exec_ask;
  var
    base: integer;
    clss: KLiClass;
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
          exprec^.Name := func.FName;
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
      Error.CanNotAsk(FOwner, stack.Exprs[base], clss);
    exec_last(clss, exprec^.ParamCount);
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
          syMoney    : exec_push(KT_MONEY);
          syTime     : exec_push(KT_TIME);
          syInt      : exec_push(KT_INT);
          syStr      : exec_push(KT_STRING);
          syChar     : exec_push(KT_CHAR);
          syBool     : exec_last(KT_BOOL, 1);
          syTrue     : exec_push(KT_BOOL);
          syFalse    : exec_push(KT_BOOL);
          syBecome   : find_varb;
          syCall     : exec_call;
          syNil      : exec_push(KT_VOID);
          syAdd      : exec_inc;
          syAddAll   : stack.Press;
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
          syUpto     : exec_last(KT_VARGEN, 2);
          syEQ       : exec_compare;
          syNE       : exec_compare;
          syLT       : exec_compare;
          syLE       : exec_compare;
          syGT       : exec_compare;
          syGE       : exec_compare;
          syIn       : exec_compare;
          syAbsEQ    : exec_compare;
          syLike     : exec_compare;
          syAnd      : exec_and_or;
          syOr       : exec_and_or;
          syIs       : exec_last(KT_BOOL, 2);
          syAs       : exec_last(stack.LastType, 2);
          syNot      : exec_last(KT_BOOL, 1);
          syReturn   : stack.Clear;
          syJmpTPop  : stack.Press;
          syJmpFPop  : stack.Press;
          syPress    : stack.Press;
          syOut      : stack.Press(exprec^.ParamCount);
          syVarlist  : exec_last(KT_VARLIST, exprec^.ParamCount);
          syHashed   : exec_last(KT_HASHED, exprec^.ParamCount);
          syVarGen   : exec_last(KT_VARGEN, exprec^.ParamCount);
          syPushVarb : exec_push_varb;
          syFunc     : stack.Add(KT_FUNC, exprec, exprec^.VFunc);
          syShell    : exec_push(KT_STRING);
          syGetValue : exec_push(KT_VARIANT);
          syFormat   : exec_last(KT_STRING, 1);
          syTry      : exec_goto;
          syGoto     : exec_goto;
          syGotoTP   : exec_goto;
          syGotoFP   : exec_goto;
          syRINR     : exec_goto;
          syLabel    : exec_label;
          syAsk      : exec_ask;
          syStatement: stack.Clear;
          syGETV     : exec_push(KT_VARIANT);
          sySETV     : stack.Press;
          syClen     : exec_push(KT_INT);
          syThis     : exec_push(KT_VARLIST);
          syDupLast  : stack.DupLast(exprec^.ParamCount);
        end;
        exprec^.flags := exprec^.flags + [xrSatisfied];
        Inc(FSatisfyIndex);
      until FSatisfyIndex >= GetCount;
    finally
      stack.Free;
    end;
    RebuildLabelList;
  end;
end;

{ KLiFunc_curry }

function KLiFunc_curry.AddCurry(value: PLseValue): integer;
begin
  Result := Length(FCurry);
  SetLength(FCurry, Result + 1);
  FCurry[Result] := __NewValue;
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
  inherited Create(AModule.ModuleClass, AFunc.FResultType, f_name, '', nil,
                   @udc_curry, cmNormal);
  FCurryFunc := AFunc;
  FCurryFunc.IncRefcount;
  IsCurryFunc := true;
  FObjRec.or_object := Self;
  FObjRec.or_class := KT_FUNC;
  Engine.OrEnter(@FObjRec);
end;

constructor KLiFunc_curry.CreateConst(AModule: KLiModule;
  const AName: string; AType: KLiClass);
begin
  inherited Create(AModule.ModuleClass, AType, AName, '',
                   nil, nil, cmNormal);
  FProc := @udc_const;
  IsNameCall := true;
  IsCurryFunc := false;
  IsConstFunc := true;
  AddCurry(nil);
  FObjRec.or_object := Self;
  FObjRec.or_class := KT_FUNC;
  Engine.OrEnter(@FObjRec);
end;

destructor KLiFunc_curry.Destroy;
begin
  Engine.OrLeave(@FObjRec);
//  if Assigned(FCurryFunc) then
//    FCurryFunc.DecRefcount;
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

function KLiFunc_curry.GetVarbData(const VarbName: string;
  var Varb: KLiVarb; var Data: PLseValue): boolean;
begin
  Varb := FCurryFunc.FParams.Find(VarbName);
  if (Varb <> nil) and (Varb.FIndex < CurryCount) then
    Data := GetCurryData(Varb.FIndex) else
    Data := nil;
  Result := (Data <> nil);
end;

{ KLiFunc_operator }

constructor KLiFunc_operator.Create(AOper: KLiSymbol);
begin
  inherited Create(sys_module.FModuleClass, KT_VARIANT,
    Symbols[AOper].ID, '', nil, @udc_oper, cmNormal);
  AddParam('V1', KT_VARIANT);
  AddParam('V2', KT_VARIANT);
  FOper := AOper;
end;

{ KLiClass }

function KLiClass.Cast(Param: PLseParam; Index: integer): PLseValue;
begin
  Result := __SetClassValue(__AsEngine(Param), Param^.param[Index], Self)
end;

constructor KLiClass.Create(Module: KLiModule; const Name: string; ADataType: TLseValue);
begin
  IncRefcount;
  FModule := Module;
  FModule.FClassList.AddObject(Name, Self);
  FName := Name;
  FClassRec.vtype := Ord(ADataType);
  if ADataType <> LSV_OBJECT then
    SetState(clsSimple, true);
  fCmFuncs := __newNamedList(true);
  if FModule.FEngine <> nil then
    FModule.FEngine.AddCompiled(Self);
  FUID := Format('%P', [pointer(Self)]);
end;

destructor KLiClass.Destroy;
begin
  DeleteFunctions;
  FreeAndNil(fCmFuncs);
  qe_entries.cik_classes^[TLseKernelClass(Ord(DataType))] := nil;
  __removeFrom(Module.FClassList, Self);
  if FModule.FModuleClass = Self then
    FModule.FModuleClass := nil;
  inherited;
end;

function KLiClass.DispSingleMethod(func: KLiFunc): boolean;
var
  ID: string;
begin
  Result := false;
  if func.FKind = cmMethod then
  begin
    ID := func.FName;
    if (FCmCount = nil) and (ID = 'get_length') then
      Result := SetSingleMethod(cmCount, func) else
    if (FCmGetAt = nil) and (ID = 'getiv') then
      Result := SetSingleMethod(cmGetAt, func) else
    if (FCmSetAt = nil) and (ID = 'setiv') then
      Result := SetSingleMethod(cmSetAt, func) else
    if (FCmGetPV = nil) and (ID = 'getpv') then
      Result := SetSingleMethod(cmGetPV, func) else
    if (FCmSetPV = nil) and (ID = 'setpv') then
      Result := SetSingleMethod(cmSetPV, func);
  end;
end;

procedure KLiClass.DumpCodeToStream(stream: TStream; const margin: string);
var
  A: integer;
  F, f_clss: KLiFunc;
  C: KLiClass;
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
  f_clss := nil;

  if IsModuleClass and (FModule.FFileName <> '') then
    WriteLine('# source file: ' + FModule.FFileName);

  if Description <> '' then
    WriteLine('# description: ' + Description);

  if IsModuleClass then
  begin
    WriteLine('# module: ' + FModule.Name);
    lse_stream_writeln(stream);
  end
  else
  begin
    f_clss := FCmCreator;
    if f_clss <> nil then
      WriteLine('class' + Copy(f_clss.Prototype(false), 4, MaxInt)) else
      WriteLine('class ' + FName);
  end;

  L := TStringList.Create;
  try
    if IsModuleClass then
    begin
      if Assigned(FModule.FModules) and (FModule.FModules.Count > 2) then
      begin
        WriteText('import');
        lse_stream_write(stream, ' ' + FModule.FModules[2].Name);
        for A := 3 to FModule.FModules.Count - 1 do
          lse_stream_write(stream, ', ' + FModule.FModules[A].Name);
        lse_stream_write(stream, ';');
        lse_stream_writeln(stream);
        NeedNewLine := true;
      end;
      
      for A := 0 to FModule.ClassCount - 1 do
      begin
        C := FModule.GetClass(A);
        if C <> Self then
        begin
          GiveNewLine;
          C.DumpCodeToStream(stream, margin);
          NeedNewLine := true;
        end;
      end;
    end;

    if FInitFunc <> nil then
    begin
      L.Clear;
      FInitFunc.DumpCode(L, margin);
      if L.Count > 2 then
      begin
        for A := 1 to L.Count - 2 do
          lse_stream_writeln(stream, L[A]);
        NeedNewLine := true;
      end;
    end;

    for A := 0 to FCmFuncs.Count - 1 do
    begin
      F := KLiFunc(FCmFuncs.Objects[A]);
      if (F <> f_clss) and (F <> FInitFunc) then
      begin
        GiveNewLine;
        L.Clear;
        if IsModuleClass then
          F.DumpCode(L, margin) else
          F.DumpCode(L, margin + '     ');
        lse_stream_write(stream, L.Text);
        NeedNewLine := true;
      end;
    end;
  finally
    L.Free;
  end;

  if IsModuleClass then
    lse_stream_writeln(stream) else
    WriteLine('end');
end;

function KLiClass.FindDeclared(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if rec = nil then rec := @findrec;
  rec^.fo_type := foNone;
  Result := FindInside(ID, rec);
  if not Result then
  begin
    rec^.VClass := Module.FindModuleClass(ID);
    Result := (rec^.VClass <> nil);
    if Result then
      rec^.fo_type := foClass;
  end;
end;

function KLiClass.FindGetMethod(const prop_name: string): KLiFunc;
begin
  Result := FindMethod(cmMethod, 'get_' + prop_name);
  if Result <> nil then
    if Result.Params.Count <> 1 then
      Result := nil;
end;

function KLiClass.FindInside(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if rec = nil then rec := @findrec;
  rec^.VFunc := FindMethod(cmMethod, ID);
  if rec^.VFunc <> nil then
  begin
    rec^.fo_type := foFunc;
    Result := true;
  end
  else
  begin
    rec^.fo_type := foNone;
    Result := false;
  end;
end;

function KLiClass.FindMethod(Kind: KLiMethodType; const AName: string): KLiFunc;
begin
  if Kind in (SingleMethods - [cmCreator]) then
    Result := SingleMethod(Kind) else
  if Kind = cmMethod then
    Result := KLiFunc(__findNamed(fCmFuncs, AName)) else
    Result := nil;
end;

function KLiClass.FindSetMethod(const prop_name: string): KLiFunc;
begin
  Result := FindMethod(cmMethod, 'set_' + prop_name);
  if Result <> nil then
    if Result.Params.Count < 2 then
      Result := nil;
end;

function KLiClass.GetBuiltin: boolean;
begin
  Result := FModule.IsBuiltin;
end;

function KLiClass.GetClassRec: PLseClassRec;
begin
  Result := @FClassRec;
end;

function KLiClass.GetDataType: TLseValue;
begin
  Result := TLseValue(FClassRec.vtype);
end;

function KLiClass.GetDescription: string;
begin
  Result := FClassRec.desc;
end;

function KLiClass.GetFullName: string;
begin
  Result := FModule.Name + '::' + FName;
end;

function KLiClass.GetHasCreator: boolean;
begin
  Result := (FCmCreator <> nil);
end;

function KLiClass.GetInfomation: string;
var
  temp: string;
  list: TStringList;
  func: KLiFunc;
  A: integer;
  clss: KLiClass;

  procedure add(const title, text: string);
  var
    value: string;
  begin
    value := Trim(text);
    if value <> '' then
      list.Add(Trim(title) + '=' + value);
  end;

begin
  list := TStringList.Create;
  try
    list.Add('[BASIC]');
    Add('B_NAME', FName);
    Add('B_DESC', GetDescription);
    Add('B_LIBRARY', GetModuleFile);
    Add('B_BUILTIN', BoolText[Builtin]);
    list.Add('');

    if FCmCreator <> nil then
    begin
      list.Add('[CONSTRUCTOR]');
      Add('C_FUNC', FCmCreator.Prototype(false));
      Add('C_DESC', FCmCreator.Description);
      list.Add('');
    end;

    if Assigned(FCmGetAt) or Assigned(FCmSetAt) then
    begin
      list.Add('[LIST]');
      if Assigned(FCmGetAt) then
        temp := FCmGetAt.FResultType.FName else
        temp := FCmSetAt.FResultType.FName;
      Add('L_ITEMTYPE', temp);
      if Assigned(FCmSetAt) then
        temp := 'read/write' else
        temp := 'readonly';
      Add('L_ACCESS', temp);
      if Assigned(FCmGetAt) then
        temp := FCmGetAt.Description else
        temp := FCmSetAt.Description;
      Add('L_DESC', temp);
      list.Add('');
    end;

    if fCmFuncs.Count > 0 then
    begin
      list.Add('[METHODS]');
      Add('M_COUNT', IntToStr(fCmFuncs.Count));
      for A := 0 to fCmFuncs.Count - 1 do
      begin
        func := KLiFunc(fCmFuncs.Objects[A]);
        temp := Format('%s "%s"', [func.Prototype(false), func.Description]);
        Add(Format('M%.2x', [A + 1]), temp);
      end;
      list.Add('');
    end;

    if IsModuleClass then
    begin
      if Module.ClassCount > 1 then
      begin
        list.Add('[CLASSES]');
        Add('C_COUNT', IntToStr(Module.ClassCount));
        for A := 0 to Module.ClassCount - 1 do
        begin
          clss := Module.GetClass(A);
          temp := Format('%s: %s', [clss.FName, clss.Description]);
          Add(Format('C%.2x', [A + 1]), temp);
        end;
        list.Add('');
      end;
      if Module.FEngine <> nil then
        if Module.FuncCount > 0 then
        begin
          list.Add('[FUNCTIONS]');
          Add('F_COUNT', IntToStr(Module.FuncCount));
          for A := 0 to Module.FuncCount - 1 do
            Add(Format('F%.2x', [A + 1]), Module.GetFunc(A).Prototype(false));
          list.Add('');
        end;
    end;

    Result := list.Text;
  finally
    list.Free;
  end;
end;

function KLiClass.GetMethodList: TStrings;
begin
  Result := FCmFuncs;
end;

function KLiClass.GetModuleFile: string;
begin
  Result := FModule.FileName;
end;

function KLiClass.HasState(Index: KLiClassState): boolean;
begin
  Result := (Index in FState);
end;

function KLiClass.ListFuncTo(List: TList): TList;

  procedure add_to_list(func: TObject);
  begin
    if (func <> nil) and (List.IndexOf(func) < 0) then
      List.Add(func);
  end;

var
  index: integer;
begin
  if List = nil then
    List := TList.Create else
    List.Clear;
    
  Result := List;

  add_to_list(FCmCreator);
  add_to_list(FCmCount);
  add_to_list(FCmGetAt);
  add_to_list(FCmSetAt);
  add_to_list(FCmGetPV);
  add_to_list(FCmSetPV);
  for index := 0 to fCmFuncs.Count - 1 do
    add_to_list(fCmFuncs.Objects[index]);
end;

procedure KLiClass.DeleteFunctions;
var
  funcs: TList;
  func: KLiFunc;
  index: integer;
begin
  Include(FState, clsReady);
  funcs := ListFuncTo(nil);
  try
    FCmCreator := nil;
    FCmCount := nil;
    FCmGetAt := nil;
    FCmSetAt := nil;
    FCmGetPV := nil;
    FCmSetPV := nil;
    fCmFuncs.Clear;
    for index := funcs.Count - 1 downto 0 do
    begin
      func := KLiFunc(funcs[index]);
      func.Free;
    end;
  finally
    funcs.Free;
  end;
end;

function KLiClass.ObjectToStrec(obj: pointer): PLseString;
begin
  if Assigned(FClassRec.toString) then
    Result := FClassRec.toString(obj) else
    Result := nil;
end;

function KLiClass.ObjectToString(obj: pointer): string;
var
  sr: PLseString;
begin
  sr := ObjectToStrec(obj);
  Result := lse_strec_string(sr);
  lse_strec_declife(sr);
end;

function KLiClass.Prototype(const ID: string): string;
begin
  Result := ID + ':' + Name;
//Result := Name + ' ' + ID;
end;

procedure KLiClass.Satisfy;
var
  index: integer;
begin
  if not IsModuleClass and not (clsSatisfied in FState) then
  begin
    FState := FState + [clsSatisfied]; 
    if FCmCreator <> nil then FCmCreator.FCodes.Satisfy;
    if FCmCount <> nil then FCmCount.FCodes.Satisfy;
    if FCmGetAt <> nil then FCmGetAt.FCodes.Satisfy;
    if FCmSetAt <> nil then FCmSetAt.FCodes.Satisfy;
    if FCmGetPV <> nil then FCmGetPV.FCodes.Satisfy;
    if FCmSetPV <> nil then FCmSetPV.FCodes.Satisfy;
    for index := 0 to fCmFuncs.Count - 1 do
      KLiFunc(fCmFuncs.Objects[index]).FCodes.Satisfy;
  end;
end;

function KLiClass.SetSingleMethod(cate: KLiMethodType; func: KLiFunc): boolean;
var
  clss: KLiClass;
  argc: integer;
begin
  Result := (cate in (SingleMethods - [cmCreator])) and
            (func <> nil) and
            (SingleMethod(cate) = nil) and
            (Self = func.FOwnerClass);
  if Result then
  begin
    clss := func.ResultType;
    argc := func.ParamCount;
    case cate of
      cmCount: Result := (argc = 1) and (clss =  KT_INT );
      cmGetAt: Result := (argc = 2) and{(clss <> KT_VOID) and}(func.Params[1].ValueType = KT_INT);
      cmSetAt: Result := (argc = 3) and{(clss =  KT_VOID) and}(func.Params[1].ValueType = KT_INT);
      cmGetPV: Result := (argc = 2) and{(clss <> KT_VOID) and}(func.Params[1].ValueType = KT_STRING);
      cmSetPV: Result := (argc = 3) and{(clss =  KT_VOID) and}(func.Params[1].ValueType = KT_STRING);
    end;
    if Result then
    begin
      case cate of
        cmCount: FCmCount := func;
        cmGetAt: FCmGetAt := func;
        cmSetAt: FCmSetAt := func;
        cmGetPV: FCmGetPV := func;
        cmSetPV: FCmSetPV := func;
      end;
      func.FKind := cate;
    end;
  end;
end;

procedure KLiClass.SetState(Index: KLiClassState; Value: boolean);
begin
  if Value then
    Include(FState, Index) else
    Include(FState, Index);
end;

function KLiClass.SetupMethod(Func: PLseFuncRec): KLiFunc;
var
  line, f_name, p_name: string;
  f_type, p_type: KLiClass;
  f_cate: KLiMethodType;
  cfmt: boolean;
  endc: char;

  function parse_next(var ID: string; var VT: KLiClass): char;
  var
    slen, index: integer;
    clss: string;
  begin
    slen := Length(line);
    if slen > 0 then
    begin
      index := 1;
      while (index <= slen) and
        not (line[index] in ['(', ',', ')', '|']) do
          Inc(index);

      Result := line[index];

      clss := Trim(Copy(line, 1, index - 1));
      line := Trim(Copy(line, index + 1, slen));

      if cfmt then
      begin
        index := Pos(' ', clss);
        if index > 0 then
        begin
          ID := Trim(Copy(clss, index + 1, MaxInt));
          clss := Trim(Copy(clss, 1, index - 1));
        end
        else
        begin
          ID := clss;
          clss := 'variant';
        end;
      end
      else
      begin
        index := Pos(':', clss);
        if index > 0 then
        begin
          ID := Trim(Copy(clss, 1, index - 1));
          clss := Trim(Copy(clss, index + 1, MaxInt));
        end
        else
        begin
          ID := clss;
          clss := 'variant';
        end;
      end;

      if __IsIDStr(pchar(ID)) then
      begin
        VT := FModule.FindClass(clss);
        if VT = nil then
          if FModule <> sys_module then
            VT := sys_module.FindClass(clss);
        if VT <> nil then Exit;
      end;
    end;

    Result := #0;
  end;

begin
  Result := nil;

  line := Trim(__replaceAll(Func^.fr_prot, '{c}', FName));
  if line = '' then Exit;

  endc := line[Length(line)];
  if not (endc in [')', '|']) then Exit;
  cfmt := (endc = ')');

  endc := parse_next(f_name, f_type);
  if endc in ['(', '|'] then
  begin
    // 1. decide function type
    if IsModuleClass then
    begin
      if FModule.Declared(f_name) then Exit;
      f_cate := cmNormal;
    end
    else
    if f_name = FName then {<--constructor}
    begin
      if f_type = KT_VARIANT then f_type := Self;
      if (f_type <> Self) or HasCreator then Exit;
      f_cate := cmCreator;
    end
    else
    begin
      if FindDeclared(f_name, nil) then Exit;
      f_cate := cmMethod;
    end;

    // 2. setup result function
    Result := KLiFunc.Create(Self, f_type, f_name, Func^.fr_desc,
                             nil, Func^.fr_addr, f_cate);

    if f_cate <> cmNormal then
      Result.AddParam('this', Self);

    // 3. parse parametres
    if not (line[1] in [')', '|']) then
    repeat
      endc := parse_next(p_name, p_type);
      if not (endc in [',', ')', '|'])
      or (p_type = KT_VOID)
      or (p_name = f_name)
      or Result.Params.Exists(p_name)
      or (Result.ParamCount = LSE_MAX_PARAMS) then
      begin
        FreeAndNil(Result);
        Exit;
      end;
      Result.AddParam(p_name, p_type);
    until endc in [')', '|'];

    // 4. try to set single method
    if (Result.ParamCount = 0) and ('__' = Copy(f_name, 1, 2)) then
      Result.IsNameCall := true else
      DispSingleMethod(Result);
  end;
end;

function KLiClass.SingleMethod(cate: KLiMethodType): KLiFunc;
begin
  Result := nil;
  case cate of
    cmCreator: Result := FCmCreator;
    cmCount  : Result := FCmCount;
    cmGetAt  : Result := FCmGetAt;
    cmSetAt  : Result := FCmSetAt;
    cmGetPV  : Result := FCmGetPV;
    cmSetPV  : Result := FCmSetPV;
//  cmMethod : Result := nil;
//  cmNormal : Result := nil;
  end;
end;

function KLiClass.StrecToObject(const S: PLseString; Engine: KLiEngine): pointer;
begin
  if Assigned(FClassRec.stringTo) then
    Result := FClassRec.stringTo(S, Engine) else
    Result := nil;
end;

{ KLiVarList }

destructor KLiVarList.Destroy;
begin
  FEngine.OrLeave(@FObjRec);
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FNames);
end;

function KLiVarList.Add: PLseValue;
begin
  Result := __NewValue;
  FItems.Add(Result);
end;

function KLiVarList.PushBoolean(Value: boolean): PLseValue;
begin
  Result := __PutBool(Add, Value);
end;

function KLiVarList.PushChar(Value: char): PLseValue;
begin
  Result := __PutChar(Add, Value);
end;

function KLiVarList.PushDefaultValue(AClass: KLiClass): PLseValue;
begin
  Result := __SetDefaultValue(Add, AClass);
end;

function KLiVarList.PushFloat(Value: double): PLseValue;
begin
  Result := __PutFloat(Add, Value);
end;

function KLiVarList.PushInt64(Value: int64): PLseValue;
begin
  Result := __PutInt64(Add, Value);
end;

function KLiVarList.PushMoney(Value: currency): PLseValue;
begin
  Result := __PutMoney(Add, Value);
end;

function KLiVarList.PushObject(Value: pointer; AClass: KLiClass): PLseValue;
begin
  Result := __PutObject(Add, @AClass.FClassRec, Value);
end;

function KLiVarList.AddNamed(const Name: string): PLseValue;
begin
  Result := Add;
  SaveName(Name, Result);
end;

function KLiVarList.AddSend(VG: PLseVargen): boolean;
begin
  Result := lse_vargen_send(VG, Add);
end;

function KLiVarList.AddSendAll(VG: PLseVargen): integer;
begin
  Result := 0;
  while not lse_vargen_eof(VG) do
  begin
    Inc(Result);
    lse_vargen_send(VG, Add);
  end;
end;

function KLiVarList.PushString(const Value: string): PLseValue;
begin
  Result := __PutString(Add, Value);
end;

function KLiVarList.PushTime(Value: TDateTime): PLseValue;
begin
  Result := __PutTime(Add, Value);
end;

function KLiVarList.PushValues(List: KLiVarList; ItemCount: integer): integer;
var
  index: integer;
begin
  if (List <> nil) and (List <> Self) then
  begin
    if ItemCount = 0 then
      Result := List.Count else
      Result := Min(ItemCount, List.Count);
    for index := 0 to Result - 1 do
      Push(List.GetData(index));
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
  var
    N: string;
  begin
    N := ValueName(V);
    if N <> '' then
    begin
      lse_stream_write(S, N);
      lse_stream_write(S, ':');
    end;
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

procedure KLiVarList.CheckIndex(Index: integer);
begin
  if (Index < 0) or (Index >= GetCount) then
    Error('List index out of bounds (%d)', Index);
end;

procedure KLiVarList.Clear;
var
  X: integer;
  V: PLseValue;
begin
  if FNames <> nil then
    if FNames.CaseSensitive then
      FreeAndNil(FNames) else
      FNames.Clear;
  for X := GetCount - 1 downto 0 do
  begin
    V := PLseValue(FItems[X]);
    FItems.Delete(X);
    __FreeValue(V);
  end;
end;

procedure KLiVarList.ClearTo(NewCount: integer);
begin
  if NewCount < GetCount then
    SetCount(NewCount);
end;

procedure KLiVarList.ClearToIndex(Index: integer);
begin
  ClearTo(Index + 1);
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
        Result.SaveName(ValueName(V), __PutValue(Result.Add, V));
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
  FObjRec.or_class := KT_VARLIST;
  FEngine.OrEnter(@FObjRec);
end;

procedure KLiVarList.Delete(Index: integer);
var
  V: PLseValue;
  X: integer;
begin
  V := GetData(Index);
  FItems.Delete(Index);
  __FreeValue(V);
  X := NameIndex(V);
  if X >= 0 then
    FNames.Delete(X);
end;

procedure KLiVarList.Error(const Msg: string; Data: integer);
begin
  raise Exception.CreateFmt(Msg, [Data]);
end;

procedure KLiVarList.Exchange(Index1, Index2: integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure KLiVarList.ExchangeLastTwo;
var
  L: integer;
begin
  L := GetCount;
  Exchange(L - 1, L - 2);
end;

procedure KLiVarList.ExpandAt(Index, ItemCount: integer);
var
  L: integer;
begin
  L := GetCount;
  if Index <> L then CheckIndex(Index);
  while ItemCount > 0 do
  begin
    Insert(Index);
    Inc(Index);
    Dec(ItemCount);
  end;
end;

function KLiVarList.First: PLseValue;
begin
  Result := GetData(0);
end;

function KLiVarList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function KLiVarList.GetData(Index: integer): PLseValue;
begin
  Result := PLseValue(FItems[Index]);
end;

function KLiVarList.GetName(Index: integer): string;
var
  X: integer;
begin
  X := NameIndex(GetData(Index));
  if X >= 0 then
    Result := FNames[X] else
    Result := '';
end;

function KLiVarList.GetNamed(const Name: string): PLseValue;
var
  index: integer;
begin
  index := NameIndex(Name);
  if Index >= 0 then
    Result := PLseValue(FNames.Objects[Index]) else
    Result := nil;
end;

function KLiVarList.GetNameList(Sorted: boolean): KLiStrList;
var
  X: integer;
  N: string;
begin
  Result := KLiStrList.Create;

  if Sorted then
  begin
    if FNames <> nil then
      Result.Assign(FNames);
    Exit;
  end;

  for X := 0 to GetCount - 1 do
  begin
    N := GetName(X);
    if N <> '' then
      Result.Add(N);
  end;
end;

function KLiVarList.Insert(Index: integer): PLseValue;
begin
  Result := __NewValue;
  FItems.Insert(Index, Result);
end;

function KLiVarList.InsertNamed(Index: integer; const Name: string): PLseValue;
begin
  Result := Insert(Index);
  SaveName(Name, Result);
end;

function KLiVarList.IsSnap: boolean;
begin
  Result := false;
end;

function KLiVarList.Last: PLseValue;
begin
  Result := GetData(GetCount - 1);
end;

function KLiVarList.Left(ItemCount: integer): KLiVarList;
begin
  Result := Copy(0, ItemCount);
end;

function KLiVarList.MaxValue: PLseValue;
var
  index: integer;
  data: PLseValue;
begin
  Result := First;
  for index := GetCount - 1 downto 1 do
  begin
    data := Datas[index];
    if __compare(data, Result) = crMore then
      Result := data;
  end;
end;

function KLiVarList.MinValue: PLseValue;
var
  index: integer;
  data: PLseValue;
begin
  Result := First;
  for index := GetCount - 1 downto 1 do
  begin
    data := Datas[index];
    if __compare(data, Result) = crLess then
      Result := data;
  end;
end;

procedure KLiVarList.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function KLiVarList.NameIndex(Value: PLseValue): integer;
begin
  if FNames <> nil then
    Result := FNames.IndexOfObject(TObject(Value)) else
    Result := -1;
end;

function KLiVarList.NameIndex(const Name: string): integer;
begin
  if FNames <> nil then
    Result := FNames.IndexOf(Name) else
    Result := -1;
end;

procedure KLiVarList.Press(Times: integer);
var
  X: integer;
begin
  X := GetCount - 1;
  if Times > X then Clear else
  while Times > 0 do
  begin
    Delete(X);
    Dec(X);
    Dec(Times);
  end;
end;

function KLiVarList.Push(Value: PLseValue): PLseValue;
begin
  if Value <> nil then
    Result := lse_set_value(Add, Value) else
    Result := Add;
end;

function KLiVarList.RemoveNamed(const Name: string): boolean;
var
  X: integer;
  V: PLseValue;
begin
  X := NameIndex(Name);
  if X >= 0 then
  begin
    V := PLseValue(FNames.Objects[X]);
    FNames.Delete(X);
    FItems.Remove(V);
    __FreeValue(V);
    Result := true;
  end
  else Result := false;
end;

function KLiVarList.SecondLast: PLseValue;
begin
  Result := GetData(GetCount - 2);
end;

procedure KLiVarList.SetCount(NewCount: integer);
var
  L, X: integer;
begin
  L := GetCount;
  if NewCount > L then
    for X := L + 1 to NewCount do Add else
    Press(L - NewCount);
end;

function KLiVarList.ValueIndex(const Name: string): integer;
begin
  Result := NameIndex(Name);
  if Result >= 0 then
    Result := ValueIndex(PLseValue(FNames.Objects[Result]));
end;

function KLiVarList.ValueIndex(Value: PLseValue): integer;
begin
  Result := FItems.IndexOf(Value);
end;

function KLiVarList.ValueName(Value: PLseValue): string;
var
  X: integer;
begin
  X := NameIndex(Value);
  if X >= 0 then
    Result := FNames[X] else
    Result := ''; 
end;

function KLiVarList.SaveName(const Name: string; Value: PLseValue): boolean;
begin
  Result := (Name <> '');
  if Result then
  begin
    if FNames = nil then
      FNames := __newNamedList(true);
    FNames.AddObject(Name, TObject(Value));
  end;
end;

{ KLiModule }

constructor KLiModule.Create(const Name: string; Engine: KLiEngine; ModuleType: KLiModuleType);
begin
  IncRefcount;
  FName := Name;
  FFileName := Name;
  FModuleType := ModuleType;
  if IsBuiltin then
  begin
    FFileName    := sys_kernel;
    FVersion     := sys_version;
    FDescription := 'builtin sys module';
  end;
  FClassList := __newNamedList(true);
  if IsScript then
  begin
    FEngine := Engine;
    FEngine.AddCompiled(Self);
    FEngine.FModules.Add(Self);
    FModules := KLiModuleList.Create(FEngine);
    FModules.FImporter := Self;
    FModules.Add(Self);
    FModules.Add(sys_module);
    FImporters := TList.Create;
  end
  else sys_libraries.AddObject(FName, Self);
end;

destructor KLiModule.Destroy;
var
  A: integer;
  P: KLiModule;
begin
  if IsScript then
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
  __releaseSOList(FClassList);
  if IsLibrary and (FHandle <> 0) then
    lse_free_library(FHandle);
  __removeFrom(sys_libraries, Self);
  inherited;
end;

procedure KLiModule.DeleteFunctions;
var
  index: integer;
begin
  for index := FClassList.Count - 1 downto 0 do
    KLiClass(FClassList.Objects[index]).DeleteFunctions;
end;

function KLiModule.GetFunc(Index: integer): KLiFunc;
begin
  Result := KLiFunc(FuncList.Objects[Index]);
end;

function KLiModule.Declared(const ID: string): boolean;
begin
  Result := Find(ID, nil);
end;

procedure KLiModule.Satisfy;
var
  list: TStringList;
  func: KLiFunc;
  index: integer;
begin
  list := FuncList;
  if list <> nil then
    for index := 0 to list.Count - 1 do
    begin
      func := KLiFunc(list.Objects[index]);
      if func.FCodes <> nil then
        func.FCodes.Satisfy;
    end;
  for index := 0 to ClassCount - 1 do
    GetClass(index).Satisfy;
end;

function KLiModule.AddImporter(module: KLiModule): KLiModule;
begin
  Result := module;
  if (module <> nil) and (module <> Self) then
    if (FImporters <> nil) and (FImporters.IndexOf(module) < 0) then
      FImporters.Add(module);
end;

function KLiModule.FindModuleClass(const ID: string): KLiClass;
var
  module: KLiModule;
begin
  module := FindModule(ID, false);
  if module <> nil then
    Result := module.ModuleClass else
    Result := nil;
end;

function KLiModule.GetClass(Index: integer): KLiClass;
begin
  Result := KLiClass(FClassList.Objects[Index]);
end;

function KLiModule.FindModule(const ID: string; FindPossible: boolean): KLiModule;
begin
  if FModules <> nil then
    Result := FModules.Find(ID) else
    Result := nil;
  if (Result = nil) and FindPossible then
  begin
    if FEngine <> nil then
      Result := FEngine.FModules.Find(ID);
    if Result = nil then
      Result := KLiModule(__findNamed(sys_libraries, ID));
  end;
end;

procedure KLiModule.ImportNotification;
var
  on_import: TLseOnImport;
begin
  if Assigned(FImportProc) then
  begin
    on_import := FImportProc;
    FImportProc := nil;
    on_import(Self);
  end;
end;

function KLiModule.IsMainModule: boolean;
begin
  Result := (FEngine.FMainModule = Self);
end;

function KLiModule.ModuleClass: KLiClass;
begin
  Result := SetupModuleClass(nil);
end;

function KLiModule.NewFunc: KLiFunc;
begin
  Result := KLiFunc.Create(ModuleClass, KT_VARIANT, '', '', nil, nil, cmNormal);
end;

function KLiModule.NewLabelName: string;
begin
  Inc(FEngine.FNameSeed);
  Result := Format('%.4d', [FEngine.FNameSeed]);
end;

function KLiModule.NewFuncName: string;
begin
  Result := NewTempID('func');
end;

function KLiModule.NewTempID(const Prefix: string): string;
begin
  Inc(FEngine.FNameSeed);
  Result := Format('%s_%.4d', [Prefix, FEngine.FNameSeed]);
end;

function KLiModule.GetIsLibrary: boolean;
begin
  Result := (FModuleType = moyLibrary);
end;

function KLiModule.GetIsScript: boolean;
begin
  Result := (FModuleType = moyScript);
end;

function KLiModule.GetIsBuiltin: boolean;
begin
  Result := (FModuleType = moyKernel);
end;

function KLiModule.FindFunc(const ID: string): KLiFunc;
begin
  if (Self <> nil) and (FuncList <> nil) then
    Result := KLiFunc(__findNamed(FuncList, ID)) else
    Result := nil;
end;

function KLiModule.FindClass(const ID: string): KLiClass;
begin
  if Self <> nil then
    Result := KLiClass(__findNamed(FClassList, ID)) else
    Result := nil;
end;

function KLiModule.FindClassBy(const ID, module_name: string): KLiClass;
var
  index: integer;
  module: KLiModule;
begin
  if module_name = '' then
  begin
    Result := FindClass(ID);
    if (Result = nil) and (FModules <> nil) then
      for index := 0 to FModules.Count - 1 do
      begin
        module := FModules.Modules[index];
        if module <> Self then
        begin
          Result := FModules[index].FindClass(ID);
          if Result <> nil then Exit;
        end;
      end;
  end
  else
  begin
    module := FindModule(module_name, true);
    Result := module.FindClass(ID);
  end;
end;

function KLiModule.SetupModuleClass(Rec: PLseFuncListRec): KLiClass;

  procedure setup_methods(clss: KLiClass);
  var
    X: integer;
  begin
    if not (clsReady in clss.FState) then
    begin
      Include(clss.FState, clsReady);
      for X := 0 to clss.FClassRec.funcs.count - 1 do
        clss.SetupMethod(@(clss.FClassRec.funcs.entry^[X]));
    end;
  end;
  
var
  index: integer;
begin
  if FModuleClass = nil then
  begin
    FModuleClass := KLiClass.Create(Self, FName, LSV_OBJECT);
    FModuleClass.SetState(clsModule, true);
    FModuleClass.SetState(clsBuiltin, IsBuiltin);
    lse_fill_class(Addr(FModuleClass.FClassRec),
                   PChar(FModuleClass.FName),
                   PChar(FDescription),
                   LSV_OBJECT);
    FModuleClass.FClassRec.lysee_class := FModuleClass;
    if Rec <> nil then
      FModuleClass.FClassRec.funcs := rec^;
    for index := 0 to ClassCount - 1 do
      setup_methods(GetClass(index));
  end;
  Result := FModuleClass;
end;

function KLiModule.Find(const ID: string; rec: PLiFindRec): boolean;
var
  findrec: KLiFindRec;
begin
  if Self <> nil then
  begin
    if rec = nil then rec := @findrec;

    // 1. imported modules
    rec^.VClass := FindModuleClass(ID);
    Result := rec^.VClass <> nil;
    if Result then
    begin
      rec^.fo_type := foClass;
      Exit;
    end;

    // 2. classes
    rec^.VClass := FindClass(ID);
    Result := (rec^.VClass <> nil);
    if Result then
    begin
      rec^.fo_type := foClass;
      Exit;
    end;

    // 3. public functions
    rec^.VFunc := FindFunc(ID);
    Result := (rec^.VFunc <> nil);
    if Result then
    begin
      rec^.fo_type := foFunc;
      Exit;
    end;

    // 4. find none
    rec^.fo_type := foNone;
  end
  else Result := false;
end;

function KLiModule.FindBy(const ID, module_name: string; rec: PLiFindRec): boolean;
var
  index: integer;
  module: KLiModule;
begin
  if module_name <> '' then
  begin
    module := FindModule(module_name, true);
    Result := module.Find(ID, rec);
  end
  else
  begin
    Result := Find(ID, rec);
    if not Result and (FModules <> nil) then
      for index := 0 to FModules.Count - 1 do
      begin
        module := FModules.Modules[index];
        if module <> Self then
        begin
          Result := module.Find(ID, rec);
          if Result then Exit;
        end;
      end;
  end;
end;

function KLiModule.FuncCount: integer;
begin
  if FuncList <> nil then
    Result := FuncList.Count else
    Result := 0;
end;

function KLiModule.FuncList: TStringList;
begin
  if FModuleClass <> nil then
    Result := FModuleClass.fCmFuncs else
    Result := nil;
end;

function KLiModule.CanDeclare(const ID: string): boolean;
begin
  Result := not Declared(ID);
end;

function KLiModule.ClassCount: integer;
begin
  if FClassList <> nil then
    Result := FClassList.Count else
    Result := 0;
end;

function KLiModule.GetState(Index: KLiModuleState): boolean;
begin
  Result := Index in FState;
end;

procedure KLiModule.SetState(Index: KLiModuleState; Value: boolean);
begin
  if Value then
    FState := FState + [Index] else
    FState := FState - [Index];
end;

function KLiModule.ImportedBy(module: KLiModule): boolean;
var
  index: integer;
begin
  if module <> nil then
  begin
    Result := (FImporters.IndexOf(module) >= 0);
    if not Result then
      for index := 0 to FImporters.Count - 1 do
      begin
        Result := KLiModule(FImporters[index]).ImportedBy(module);
        if Result then Exit;
      end;
  end
  else Result := false;
end;

{ KLiModuleList }

function KLiModuleList.Add(AModule: KLiModule): integer;
begin
  Result := IndexOfModule(AModule);
  if Result < 0 then
    Result := FModules.AddObject(AModule.FName, AModule);
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
    Result.PushObject(GetModule(index), KT_MODULE);
end;

{ KLiError }

procedure KLiError.BreakNoLoop(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvBreakNoLoop, LastRow, LastCol, Module.Name,
      EsBreakNoLoop, IncludedFile, []);
end;

procedure KLiError.CanNotAsk(func: KLiFunc; expr: PLiExprRec; clss: KLiClass);
begin
  with expr^ do
    SyntaxErr(EvCanNotAsk, Pos.row, Pos.col, func.Module.Name,
      EsCanNotAsk, GetIncludedFile(Pos.fid), [clss.Name]);
end;

procedure KLiError.ClassNotExists(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvClassNotExists, LastRow, LastCol, Module.Name,
      EsClassNotExists, IncludedFile, [LastVal]);
end;

procedure KLiError.Clear;
begin
  Write('', 0, 0, 0, '', '', '');
end;

procedure KLiError.ContinueNoLoop(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvContinueNoLoop, LastRow, LastCol, Module.Name,
      EsContinueNoLoop, IncludedFile, []);
end;

constructor KLiError.Create(AEngine: KLiEngine);
begin
  IncRefcount;
  FEngine := AEngine;
end;

procedure KLiError.Error(const name: string; errno, row, col: integer; const module, msg, fname: string);
begin
  Write(name, errno, row, col, module, msg, fname);
  if FEngine.FMainRunner <> nil then
  begin
    FEngine.FMainRunner.Excepted := true;
    lse_error(ErrorText);
  end
  else Abort;
end;

procedure KLiError.FuncNotFound(func: KLiFunc; expr: PLiExprRec);
begin
  with expr^ do
    SyntaxErr(EvFuncNotFound, Pos.row, Pos.col, func.Module.Name,
      EsFuncNotFound, GetIncludedFile(Pos.fid), [Name]);
end;

function KLiError.GetCol: integer;
begin
  Result := FErrec.col;
end;

function KLiError.GetErrno: integer;
begin
  Result := FErrec.errno;
end;

function KLiError.GetErrorRec: PLiError;
begin
  Result := @FErrec;
end;

function KLiError.GetErrorText: string;
const
  E = '[%s]: (module=%s%s row=%d col=%d errno=%d) %s';
begin
  if (Errno <> 0) and (Msg <> '') then
  begin
    Result := GetFileName;
    if Result <> '' then
      Result := ' file=' + Result;
    Result := Format(E, [Name, module, Result, Row + 1, Col + 1, Errno, Msg]);
  end
  else Result := '';
end;

function KLiError.GetFileName: string;
begin
  Result := FErrec.ifname;
end;

function KLiError.GetIncludedFile(FileID: integer): string;
begin
  Result := FEngine.GetIncludedFile(FileID);
end;

function KLiError.GetMsg: string;
begin
  Result := FErrec.errmsg;
end;

function KLiError.GetName: string;
begin
  Result := FErrec.error;
end;

function KLiError.GetModule: string;
begin
  Result := FErrec.module;
end;

function KLiError.GetRow: integer;
begin
  Result := FErrec.row;
end;

procedure KLiError.SetCol(const Value: integer);
begin
  FErrec.col := Value;
end;

procedure KLiError.SetErrno(const Value: integer);
begin
  FErrec.errno := Value;
end;

procedure KLiError.SetFileName(const Value: string);
begin
  StrPLCopy(FErrec.ifname, Value, 259);
end;

procedure KLiError.SetMsg(const Value: string);
begin
  StrPLCopy(FErrec.errmsg, Value, 259);
end;

procedure KLiError.SetName(const Value: string);
begin
  StrPLCopy(FErrec.error, Value, 63);
end;

procedure KLiError.SetModule(const Value: string);
begin
  StrPLCopy(FErrec.module, Value, 63);
end;

procedure KLiError.SetRow(const Value: integer);
begin
  FErrec.row := Value;
end;

procedure KLiError.SymExpected(Parser: KLiParser; const syms: string);
begin
  with Parser do
    SyntaxErr(EvSymExpected, LastRow, LastCol, Module.Name,
      EsSymExpected, IncludedFile, [syms, LastVal]);
end;

procedure KLiError.SymNotFound(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvSymNotFound, LastRow, LastCol, Module.Name,
      EsSymNotFound, IncludedFile, []);
end;

procedure KLiError.SymUnexpected(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvSymUnexpected, LastRow, LastCol, Module.Name,
      EsSymUnexpected, IncludedFile, [LastVal]);
end;

procedure KLiError.ThrowNothing(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvThrowNothing, LastRow, LastCol, Module.Name,
      EsThrowNothing, IncludedFile, []);
end;

procedure KLiError.TooManyParam(Parser: KLiParser; func: KLiFunc);
begin
  with Parser do
    SyntaxErr(EvTooManyParam, func.FCodes.FPos.row, func.FCodes.FPos.col, Module.Name,
      EsTooManyParam, IncludedFile, [func.Name]);
end;

procedure KLiError.Redeclared(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvRedeclared, LastRow, LastCol, Module.Name,
      EsRedeclared, IncludedFile, [LastVal]);
end;

procedure KLiError.ResetMethod(func: KLiFunc; expr: PLiExprRec; const Method: string);
begin
  with expr^ do
    SyntaxErr(EvResetMethod, Pos.row, Pos.col, func.Module.Name,
      EsResetMethod, GetIncludedFile(Pos.fid), [Method]);
end;

procedure KLiError.Write(const name: string; errno, row, col: integer; const module, msg, fname: string);
begin
  SetName(name);
  SetErrno(errno);
  SetRow(row);
  SetCol(col);
  SetMsg(msg);
  SetModule(module);
  SetFileName(fname);
end;

procedure KLiError.WrongException(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvWrongException, LastRow, LastCol, Module.Name,
      EsWrongException, IncludedFile, [LastVal]);
end;

procedure KLiError.ObjectNotExists(func: KLiFunc; expr: PLiExprRec);
begin
  with expr^ do
    SyntaxErr(EvObjectNotExists, Pos.row, Pos.col, func.Module.Name,
      EsObjectNotExists, GetIncludedFile(Pos.fid), [Name]);
end;

procedure KLiError.SyntaxErr(errno, row, col: integer; const module, fmt, fname: string;
  const args: array of const);
begin
  Error(SyntaxError, errno, row, col, module, Format(fmt, args), fname);
end;

procedure KLiError.ImportErr(errno, row, col: integer; const module, fmt, fname: string;
  const args: array of const);
begin
  Error(ImportError, errno, row, col, module, Format(fmt, args), fname);
end;

procedure KLiError.WrongModuleName(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvWrongModuleName, LastRow, LastCol, Module.Name,
      EsWrongModuleName, IncludedFile, [LastVal]);
end;

procedure KLiError.ModuleReimport(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvModuleReimport, LastRow, LastCol, Module.Name,
      EsModuleReimport, IncludedFile, [LastVal]);
end;

procedure KLiError.ModuleNotFound(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvModuleNotFound, LastRow, LastCol, Module.Name,
      EsModuleNotFound, IncludedFile, [LastVal]);
end;

procedure KLiError.FileNotFound(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvFileNotFound, LastRow, LastCol, Module.Name,
      EsFileNotFound, IncludedFile, [LastVal]);
end;

procedure KLiError.WrongLibrary(Parser: KLiParser);
begin
  with Parser do
    ImportErr(EvWrongLibrary, LastRow, LastCol, Module.Name,
      EsWrongLibrary, IncludedFile, [LastVal]);
end;

procedure KLiError.WrongIDName(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvWrongIDName, LastRow, LastCol, Module.Name,
      EsWrongIDName, IncludedFile, [LastVal]);
end;

procedure KLiError.CatchNotFound(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvCatchNotFound, LastRow, LastCol, Module.Name,
      EsCatchNotFound, IncludedFile, []);
end;

procedure KLiError.NeedPureID(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvNeedPureID, LastRow, LastCol, Module.Name,
      EsNeedPureID, IncludedFile, [LastVal]);
end;

procedure KLiError.LabelNotExists(func: KLiFunc; expr: PLiExprRec);
begin
  with expr^ do
    SyntaxErr(EvLabelNotExists, Pos.row, Pos.col, func.Module.Name,
      EsLabelNotExists, GetIncludedFile(Pos.fid), [Name]);
end;

procedure KLiError.LocalNotExists(Parser: KLiParser);
begin
  with Parser do
    SyntaxErr(EvLocalNotFound, LastRow, LastCol, Module.Name,
      EsLocalNotFound, IncludedFile, [LastVal]);
end;

procedure KLiError.ImportEachOther(Parser: KLiParser; module: KLiModule);
begin
  with Parser do
    SyntaxErr(EvImportEachOther, LastRow, LastCol, Module.Name,
      EsImportEachOther, IncludedFile, [Module.Name, module.Name]);
end;

{ KLiEngine }

procedure KLiEngine.Clear;
begin
  lock_engine(Self);
  try
    Reset(true);
    FArguments.Clear;
    FIncludedFiles.Clear;
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

function KLiEngine.AddIncludedFile(const FileName: string): integer;
begin
  Result := FIncludedFiles.IndexOf(FileName);
  if Result < 0 then
    Result := FIncludedFiles.Add(FileName);
  Inc(Result);
end;

function KLiEngine.Compile(const Code: string; IsLsp: boolean): KLiFunc;
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
    Result := DoCompile(Code, IsLsp);
  except
    FMainModule.FileName := old_fname;
    raise;
  end;
end;

constructor KLiEngine.Create(const AEngineRec: PLseEngine);
begin
  FEngineRec := AEngineRec;
  if FEngineRec <> nil then
    FEngineRec^.kernel_engine := Self;

  IncRefcount;

  FCompiledObjects := nil;
  FModules := KLiModuleList.Create(Self);

  FExitResult := __NewValue;
  
  FError := KLiError.Create(Self);

  FArguments := KLiStrlist.Create;
  FArguments.CaseSensitive := true;
  FArguments.IncRefcount;

  FIncludedFiles := TStringList.Create;
  {$IFNDEF WINDOWS}
  FIncludedFiles.CaseSensitive := true;
  {$ENDIF}
  AddIncludedFile(sys_kernel);

  FMainFile := sys_kernel;

  FMainModule := KLiModule.Create(RPN_MAIN, Self, moyScript);
  if FMainFile = '' then
    FMainModule.FFileName := sys_kernel else
    FMainModule.FFileName := FMainFile;

  FMainValues := KLiHashed.Create(Self, 64);
  FMainValues.IncRefcount;

  FTempValues := __NewVarlist(Self);
  FTempValues.IncRefcount;

  FInvoker := KLiInvoke.Create(Self);
end;

destructor KLiEngine.Destroy;
begin
  Clear;

  __freeAndNil(FInvoker);
  __freeAndNil(FMainValues);
  __freeAndNil(FTempValues);
  __freeAndNil(FMainSnap);

  SetStdinStream(nil);
  SetStdoutStream(nil);
  SetStderrStream(nil);

  __FreeValue(FExitResult);
  FExitResult := nil;

  __freeAndNil(FMainModule);
  __freeAndNil(FModules);
  __freeAndNil(FError);
  __freeAndNil(FArguments);
  __freeAndNil(FIncludedFiles);
  inherited;
end;

function KLiEngine.DoCompile(const Code: string; IsLsp: boolean): KLiFunc;
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
      module := FMainRunner.CurrentFunc.Module else
      module := FMainModule;
    count := FModules.Count;
    FCompiledObjects := KLiList.Create;
    Result := KLiParser.Create(module).ParseAndFree(Code, IsLsp);
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
    FModules[A].ModuleClass.DumpCodeToStream(stream, margin);
    lse_stream_writeln(stream);
  end;
end;

procedure KLiEngine.EventNotify(ID: integer);
begin
  if ID = KEE_EXECUTED then
    SetResultTypeText(GetResultType.FullName, GetResultText) else
    SetResultTypeText('', '');
  FEngineRec^.lseu_engine_event(FEngineRec, ID, FEngineRec^.lseu_data);
end;

function KLiEngine.TryCompileCode(const code: string; IsLsp: boolean): boolean;
begin
  try
    SetResultTypeText('', '');
    Compile(code, IsLsp);
    Result := (Error.errno = 0);
  except
    Result := false;
  end;
end;

function KLiEngine.TryExecuteCode(const code: string; IsLsp: boolean): boolean;
begin
  Result := TryCompileCode(code, IsLsp) and TryGo(false);
end;

function KLiEngine.TryCompileFile(const fname: string; IsLsp: boolean): boolean;
begin
  try
    SetMainFile(fname);
    Result := TryCompileCode(__fileText(fname), IsLsp);
  except
    Result := false;
    Error.write(RuntimeError, ERUNTIME, 0, 0, MainModule.Name,
                lse_exception_str, fname);
  end;
end;

function KLiEngine.TryExecuteFile(const fname: string; IsLsp: boolean): boolean;
begin
  Result := TryCompileFile(fname, IsLsp) and TryGo(false);
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

function KLiEngine.GetIncludedFile(FileID: integer): string;
begin
  if (FileID > 0) and (FileID <= FIncludedFiles.Count) then
    Result := FIncludedFiles[FileID - 1] else
    Result := '';
end;

procedure KLiEngine.SetResultTypeText(const RType, RText: string);
begin
  FExitResultType := RType;
  FExitResultText := RText;
end;

procedure KLiEngine.SetStderrStream(const Value: PLseStream);
begin
  if Value <> FStderrStream then
  begin
    if FStderrStream <> nil then
    try
      FStderrStream^.release(FStderrStream);
    finally
      FStderrStream := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.lseu_stderr then
      begin
        Value^.addref(Value);
        FStderrStream := Value;
      end;
  end;
end;

procedure KLiEngine.SetStdinStream(const Value: PLseStream);
begin
  if Value <> FStdinStream then
  begin
    if FStdinStream <> nil then
    try
      FStdinStream^.release(FStdinStream);
    finally
      FStdinStream := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.lseu_stdin then
      begin
        Value^.addref(Value);
        FStdinStream := Value;
      end;
  end;
end;

procedure KLiEngine.SetStdoutStream(const Value: PLseStream);
begin
  if Value <> FStdoutStream then
  begin
    if FStdoutStream <> nil then
    try
      FStdoutStream^.release(FStdoutStream);
    finally
      FStdoutStream := nil;
    end;
    if Value <> nil then
      if Value <> FEngineRec^.lseu_stdout then
      begin
        Value^.addref(Value);
        FStdoutStream := Value;
      end;
  end;
end;

function KLiEngine.GetMainFunc: KLiFunc;
begin
  if FMainFunc = nil then
  begin
    FMainFunc := KLiFunc.Create(FMainModule.ModuleClass, KT_VARIANT,
      RPN_MAIN, '', nil, nil, cmNormal);
    FMainFunc.SetState(fusIsMainFunc, true);
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

function KLiEngine.GetResultType: KLiClass;
begin
  Result := __AsClass(FExitResult);
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

function KLiEngine.GetStderrStream: PLseStream;
begin
  Result := FStderrStream;
  if Result = nil then
    Result := FEngineRec^.lseu_stderr;
end;

function KLiEngine.GetStdinStream: PLseStream;
begin
  Result := FStdinStream;
  if Result = nil then
    Result := FEngineRec^.lseu_stdin;
end;

function KLiEngine.GetStdoutStream: PLseStream;
begin
  Result := FStdoutStream;
  if Result = nil then
    Result := FEngineRec^.lseu_stdout;
end;

procedure KLiEngine.GetValue(const Name: string; Value: PLseValue);
var
  data: PLseValue;
begin
  data := FMainValues.FindValueByChain(Name);
  if data <> nil then
    lse_set_value(Value, data) else
  if Name = 'search' then
    lse_set_string(Value, GetSearchPath) else
  if Name = 'mainfile' then
    lse_set_string(Value, FMainFile) else
    lse_set_string(Value, __ReadConfig(Name));
end;

procedure KLiEngine.Go;
begin
  try
    __check(FReady, 'The engine is not ready to run');
    PrepareCompile;
    Reset(false);
    FError.Clear;
    FExited := false;
    EventNotify(KEE_EXECUTING);
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
        EventNotify(KEE_EXECUTED);
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
        if or_class = KT_VARLIST then
          KLiVarList(or_object).Clear else
        if or_class = KT_HASHED then
          KLiHashed(or_object).Clear else
        if or_class = KT_FUNC then
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
     or_class.FClassRec.decRefcount(or_object);
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
     or_class.FClassRec.incRefcount(or_object);
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
  clss: KLiClass;
  vobj: pointer;
begin
  Result := (VD <> nil) and (lse_vtype(VD) = LSV_OBJECT) and (VD^.VObject <> nil);
  if Result then
  begin
    vobj := VD^.VObject;
    clss := __AsClass(VD);
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
    data := FMainValues.FindValueByChain(Name);
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
    Values.Push(GetData(index));
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

function KLiVarSnap.IsSnap: boolean;
begin
  Result := true;
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
    PushDefaultValue(GetVarb(index).ValueType);
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
    __SetFunc(Result.Add, KLiFunc(cs^.call^.func));
    for X := 0 to cs^.call^.count - 1 do
      Result.Push(cs^.call^.param[X]);
  end
  else
  begin
    __SetFunc(Result.Add, cs^.snap^.func);
    for X := 0 to cs^.snap^.values.FActualParamCount - 1 do
      Result.Push(cs^.snap^.values[X]);
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
                         IncludedFile);
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

procedure KLiRunner.Eval(const Code: string; Output: PLseValue; IsLsp: boolean);
var
  func: KLiFunc;
begin
  try
    FEngine.FError.Clear;
    func := FEngine.DoCompile(Code, IsLsp);
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

function KLiRunner.IncludedFile: string;
begin
  if FExprrec <> nil then
    Result := FEngine.GetIncludedFile(FExprrec^.Pos.fid) else
    Result := '';
end;

function KLiRunner.Goon(func: KLiFunc; ParamCount: integer; Output: PLseValue): boolean;
var
  base, index, count: integer;
  call: RLseParam;
  data: RLseValue;
  snap: RLiSnap;
  clss: KLiClass;
  curry: KLiFunc_curry;
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
      FStack.ExpandAt(base, count);
      for index := 0 to count - 1 do
        __PutValue(FStack[base + index], curry.CurryData[index]);
      Inc(ParamCount, count);
      func := curry.FCurryFunc;
    end
    else count := 0;

    for index := count to func.ParamCount - 1 do
    begin
      clss := func.FParams[index].ValueType;
      if index < ParamCount then
        __SetClassValue(FEngine, FStack[base + index], clss) else
        __SetDefaultValue(FStack.Add, clss);
    end;

    if Func.FProc <> nil then
    begin
      __InitParam(@call, Self, Func);
      for index := 0 to Func.ParamCount - 1 do
        call.param[index] := FStack[index + base];
      call.count := ParamCount;
      FCallStack.Push(PLseParam(@call));
      try
        if Output <> nil then
        begin
          call.result := lse_clear_value(Output);
          Result := Func.Execute(@call);
          if Result then
            FStack.ClearTo(base);
        end
        else
        begin
          lse_init_value(@data);
          call.result := @data;
          Result := Func.Execute(@call);
          if Result then
          begin
            FStack.ClearTo(base);
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
        for index := 0 to Func.ParamCount - 1 do
          lse_set_value(snap.values[index], FStack[base + index]);
      end;
      try
        FStack.ClearTo(base);
        if Output = nil then
        begin
          snap.output := FStack.Add;
          Inc(snap.base);
          Inc(base);
        end
        else snap.output := lse_clear_value(Output);
        FCurrent := @snap;
        FCallStack.Push(FCurrent);
        try
          while ExecGoonNext do
          begin
            { nothing }
          end;
          Result := not FExcepted;
          if Result and (FCurrent <> nil) then
            Return(false);
        finally
          FCallStack.Pop;
        end;
      finally
        FCurrent := snap.prior;
        FExprrec := snap.exprec;
        snap.values.DecRefcount;
        FStack.ClearTo(base);
      end;
    end;
  except
    Result := false;
    if not FExcepted then
      ErrorRT(lse_exception_str);
  end;
end;

function KLiRunner.GoonConst(Param: PLseParam): boolean;
var
  func: KLiFunc_curry;
  base: integer;
  snap: RLiSnap;
begin
  try
    func := KLiFunc_curry(Param^.func);
    base := FStack.Count;
    snap.base := base;
    snap.next := 0;
    snap.values := KLiVarSnap.Create(func);
    snap.output := Param^.result;
    snap.outype := func.FResultType;
    snap.prior := FCurrent;
    snap.exprec := FExprrec;
    snap.func := func;
    try
      FCurrent := @snap;
      FCallStack.Push(FCurrent);
      try
        while ExecGoonNext do
        begin
          { nothing }
        end;
        Result := not FExcepted;
        if Result and (FCurrent <> nil) then
          Return(false);
      finally
        FCallStack.Pop;
      end;
    finally
      FCurrent := snap.prior;
      FExprrec := snap.exprec;
      snap.values.DecRefcount;
      FStack.ClearTo(base);
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
      Result.PushInt64(mp_result.mr_str - mp_source);
      Result.PushInt64(mp_result.mr_len);
      for index := 0 to mp_level - 1 do
      begin
        Result.PushInt64(mp_captures[index].mr_str - mp_source);
        Result.PushInt64(mp_captures[index].mr_len);
      end;
    end;
end;

function KLiRunner.MatchPatten: PLiMatchPatten;
begin
  Result := @FMatchPatten;
end;

procedure KLiRunner.Return(HasResult: boolean);
begin
  with FCurrent^ do
  begin
    if HasResult then
      __SetClassValue(FEngine, lse_set_value(output, FStack.Last), outype) else
      __SetDefaultValue(output, outype);
    FStack.ClearTo(base);
    next := LSE_MAX_CODES;
  end;
  FCurrent := nil; {<--returned}
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
      Result := @sys_void_data;
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
      Result := Result + ToString(temp);
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

function KLiRunner.ToString(const ID: string): string;
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
  vname := __decodeClassName(ID, mname);
  found := ((mname = '') and (System.Pos('::', ID) < 1) and
           curr.FindDeclared(vname, @R)) or
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
  if R.fo_type = foClass then
    Result := R.VClass.FullName;
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

function KLiSatisfy.Add(T: KLiClass; X: PLiExprRec; F: KLiFunc): integer;
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

function KLiSatisfy.GetLastType: KLiClass;
begin
  Result := GetLastData^.s_type;
end;

function KLiSatisfy.GetType(Index: integer): KLiClass;
begin
  Result := GetData(Index)^.s_type;
end;

function KLiSatisfy.NewData(T: KLiClass; X: PLiExprRec; F: KLiFunc): PLiSatisfy;
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

function KLiHashed.AddFrom(Hash: KLiHashed): integer;
begin
  if (Hash <> nil) and (Hash <> Self) then
    Result := Hash.EnumKeyData({$IFDEF FPC}@{$ENDIF}EnumAdd, Self) else
    Result := 0;
end;

constructor KLiHashed.Create(AEngine: KLiEngine; Size: cardinal);
begin
  inherited Create(Size);
  FObjRec.or_object := Self;
  FObjRec.or_class := KT_HASHED;
  FEngine := AEngine;
  FEngine.OrEnter(@FObjRec);
end;

destructor KLiHashed.Destroy;
begin
  FEngine.OrLeave(@FObjRec);
  inherited;
end;

procedure KLiHashed.EnumAdd(const Key: string; Value, Param: pointer);
begin
  SetValue(Key, PLseValue(Value));
end;

procedure KLiHashed.EnumListKV(const Key: string; Value, Param: pointer);
begin
  KLiVarList(Param).PushString(Key);
  KLiVarList(Param).Push(Value);
end;

function KLiHashed.FindValue(const Key: string): PLseValue;
begin
  Result := DoGet(Key);
end;

function KLiHashed.FindValueByChain(const Key: string): PLseValue;
var
  hash: KLiHashed;
  curr: string;
  data: PLseValue;
  next, base: PChar;
begin
  Result := nil;
  next := pchar(Key);
  if (next <> nil) and (next^ <> #0) then
  begin
    hash := Self;
    repeat
      base := next;
      while not (next^ in [#0, '.']) do Inc(next);

      SetString(curr, base, next - base);
      if next^ = #0 then
      begin
        Result := hash.FindValue(curr);
        Exit;
      end;

      data := hash.FindValue(curr);
      if (data <> nil) and (__AsClass(data) = KT_HASHED) then
      begin
        hash := KLiHashed(__AsObject(data));
        Inc(next);
      end
      else hash := nil;
    until hash = nil;
  end;
end;

function KLiHashed.ForceValue(const Key: string): PLseValue;
var
  hash: PLiHashItem;
begin
  hash := Find(Key);
  if hash = nil then
  begin
    Result := __NewValue;
    DoPut(Key, Result);
  end
  else Result := hash^.hi_data;
end;

function KLiHashed.ForceValueByChain(const Key: string): PLseValue;
var
  hash, item: KLiHashed;
  curr: string;
  data: PLseValue;
  next, base: PChar;
begin
  Result := nil;
  hash := Self;
  next := pchar(Key);
  if (next <> nil) then while next^ <> #0 do
  begin
    base := next;
    while not (next^ in [#0, '.']) do Inc(next);
    SetString(curr, base, next - base);
    data := hash.FindValue(curr);
    if next^ = #0 then
    begin
      if data = nil then
      begin
        data := __NewValue;
        hash.DoPut(curr, data);
      end;
      Result := data;
    end
    else
    begin
      if data = nil then
      begin
        item := KLiHashed.Create(FEngine);
        data := __NewValue;
        hash.DoPut(curr, data);
        __SetObject(data, KT_HASHED, item);
        hash := item;
      end
      else
      if __AsClass(data) = KT_HASHED then
      begin
        item := KLiHashed(__AsObject(data));
        if item = nil then
        begin
          item := KLiHashed.Create(FEngine);
          __SetObject(data, KT_HASHED, item);
        end;
        hash := item;
      end
      else
      begin
        item := KLiHashed.Create(FEngine);
        __SetObject(data, KT_HASHED, item);
        hash := item;
      end;
      Inc(next);
    end;
  end;
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

function KLiHashed.ListKeyValue(List: KLiVarList): integer;
begin
  if List <> nil then
    Result := EnumKeyData({$IFDEF FPC}@{$ENDIF}EnumListKV, List) else
    Result := 0;
end;

function KLiHashed.SetInt64(const Key: string; Value: int64): PLseValue;
begin
  Result := lse_set_int64(ForceValue(Key), Value);
end;

function KLiHashed.SetObject(const Key: string; Obj: pointer; Clss: PLseClassRec): PLseValue;
begin
  Result := lse_set_object(ForceValue(Key), Clss, Obj);
end;

function KLiHashed.SetStr(const Key, Value: string): PLseValue;
begin
  Result := lse_set_string(ForceValue(Key), Value);
end;

function KLiHashed.SetStrByChain(const Key, Value: string): PLseValue;
begin
  Result := ForceValueByChain(Key);
  if Result <> nil then
    __PutString(Result, Value);
end;

function KLiHashed.SetValue(const Key: string; Value: PLseValue): PLseValue;
begin
  Result := lse_set_value(ForceValue(Key), Value);
end;

function KLiHashed.SetValueByChain(const Key: string; Value: PLseValue): PLseValue;
begin
  Result := ForceValueByChain(Key);
  if Result <> nil then
    lse_set_value(Result, Value);
end;


{ KLiExportAPI }

procedure KLiExportAPI.BeginPage;
var
  title: string;
begin
  title := Format('Class Reference Of Lysee %s', [sys_version]);
  Writeln('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"');
  Writeln('"http://www.w3.org/TR/html4/loose.dtd">');
  Writeln('<html>');
  Writeln('<head>');
  Writeln('<meta http-equiv="Content-Type" content="text/html; charset=utf-8">');
  Writeln('<title>' + title + '</title>');
  Writeln('<style type="text/css">');
  Writeln('<!--');
  Writeln('.white {');
  Writeln('	color: #FFFFFF;');
  Writeln('	font-family: "Courier New", Courier, mono;');
  Writeln('	font-size: 10pt;');
  Writeln('}');
  Writeln('.black {');
  Writeln('	color: #000000F;');
  Writeln('	font-family: "Courier New", Courier, mono;');
  Writeln('	font-size: 10pt;');
  Writeln('}');
  Writeln('-->');
  Writeln('</style>');
  Writeln('</head>');
  Writeln('<body>');
  Writeln('<h2 align="center">' + title + '</h2>');
  Writeln('<table border="0" align="center" cellpadding="4" cellspacing="1" bgcolor="#CCCCCC">');
  Writeln('<tr bgcolor="#999999" class="white">');
  Writeln('<th>ITEM</th>');
  Writeln('<th>NAME</th>');
  Writeln('<th>DESCRIPTION</th>');
  Writeln('</tr>');
  Writeln('<!--BEGIN API LIST-->');
end;

procedure KLiExportAPI.EndPage;
begin
  Writeln('<tr>');
  Writeln('<td colspan=3 bgcolor="#000000" class="white" align="right">');
  Writeln(GetTotal);
  Writeln('</td>');
  Writeln('</tr>');
  Writeln('<!--END API LIST-->');
  Writeln('</table>');
  Writeln('</body>');
  Writeln('</html>');
end;

procedure KLiExportAPI.Execute(const FileName: string);
var
  module: KLiModule;
  index: integer;
begin
  FStream := TFileStream.Create(FileName, fmCreate);
  try
    FModuleCount := 0;
    FFuncCount := 0;
    FClassCount := 0;
    FMethodCount := 0;
    BeginPage;
    for index := 0 to sys_libraries.Count - 1 do
    begin
      module := KLiModule(sys_libraries.Objects[index]);
      WriteModule(module);
    end;
    EndPage;
  finally
    FreeAndNil(FStream);
  end;
end;

function KLiExportAPI.GetDesc(const Desc: string): string;
begin
  Result := Trim(Desc);
  if Result = '' then Result := '&nbsp;';
end;

function KLiExportAPI.GetProt(func: KLiFunc): string;
begin
  Result := func.Prototype(false);
  if Copy(Result, 1, 4) = 'def ' then
    Result := Copy(Result, 5, MaxInt);
end;

function KLiExportAPI.GetTotal: string;
begin
  Result := Format('TOTAL: %d modules | %d functions | %d classes | %d methods',
    [FModuleCount, FFuncCount, FClassCount, FMethodCount])
end;

procedure KLiExportAPI.WriteClass(cls: KLiClass);
var
  index: integer;
  func: KLiFunc;
  list: TStrings;
begin
  if (cls.Creator = nil) and (cls.MethodList.Count = 0) then Exit;
  
  Writeln('<tr>');
  Writeln('<td align="center" bgcolor="#FFFFFF" class="black" nowrap>class</td>');
  Writeln('<td bgcolor="#0000FF" class="white">' + cls.Name + '</td>');
  Writeln('<td bgcolor="#0000FF" class="white">' +  GetDesc(cls.Description) + '</td>');
  Writeln('</tr>');

  func := cls.Creator;
  if func <> nil then
    WriteConstructor(func);

  list := cls.MethodList;
  for index := 0 to list.Count - 1 do
  begin
    func := KLiFunc(list.Objects[index]);
    WriteMethod(func);
  end;

  Inc(FClassCount);
end;

procedure KLiExportAPI.WriteConstructor(func: KLiFunc);
begin
  Writeln('<tr class="black">');
  Writeln('<td bgcolor="#FFFFFF"><div align="center" nowrap>constructor</div></td>');
  Writeln('<td bgcolor="#97FFC0">&nbsp;&nbsp;' + func.OwnerClass.Name + '(' +
    func.ParamList(false, true) + ')</td>');
  Writeln('<td bgcolor="#97FFC0">' + GetDesc(func.Description) + '</td>');
  Writeln('</tr>');
  Inc(FMethodCount);
end;

procedure KLiExportAPI.WriteFunc(func: KLiFunc);
begin
  Writeln('<tr class="black">');
  Writeln('<td align="center" bgcolor="#FFFFFF" nowrap>function</td>');
  Writeln('<td bgcolor="#FFC6C6">&nbsp;&nbsp;' + GetProt(func) + '</td>');
  Writeln('<td bgcolor="#FFC6C6">' + GetDesc(func.Description) + '</td>');
  Writeln('</tr>');
  Inc(FFuncCount);
end;

procedure KLiExportAPI.Writeln(const Text: string);
begin
  lse_stream_writeln(FStream, Text);
end;

procedure KLiExportAPI.WriteMethod(func: KLiFunc);
begin
  Writeln('<tr class="black">');
  Writeln('<td bgcolor="#FFFFFF"><div align="center" nowrap>method</div></td>');
  Writeln('<td bgcolor="#C6C6FF">&nbsp;&nbsp;' + GetProt(func) + '</td>');
  Writeln('<td bgcolor="#C6C6FF">' + GetDesc(func.Description) + '</td>');
  Writeln('</tr>');
  Inc(FMethodCount);
end;

procedure KLiExportAPI.WriteModule(module: KLiModule);
var
  index: integer;
  func: KLiFunc;
  cls: KLiClass;
begin
  Writeln('<tr bgcolor="#FF0000" class="white">');
  Writeln('<td nowrap>module</td>');
  Writeln('<td>' + module.Name + '</td>');
  Writeln('<td>' + GetDesc(module.Description) + '</td>');
  Writeln('</tr>');

  for index := 0 to module.FuncCount - 1 do
  begin
    func := module.GetFunc(index);
    WriteFunc(func);
  end;
  
  for index := 0 to module.ClassCount - 1 do
  begin
    cls := module.GetClass(index);
    WriteClass(cls);
  end;

  Inc(FModuleCount);
end;

{ KLiInvoke }

function KLiInvoke.FormatStr(const Str: string): string;
begin
  Result := __AsRunner(Param).FormatFor(Str, nil);
end;

procedure KLiInvoke.ReturnBool(const Value: boolean);
begin
  SetNil(FParam^.result);
  __PutBool(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnChar(const Value: char);
begin
  SetNil(FParam^.result);
  __PutChar(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnError(const ID: string; Errno: integer; const Msg: string);
begin
  __SetError(FParam, ID, Errno, Msg);
end;

function KLiInvoke.Read(const Buf: pchar; Count: integer): integer;
begin
  Result := lse_stream_read(FEngine.StdinStream, Buf, Count);
end;

function KLiInvoke.Readln: string;
var
  sr: PLseString;
begin
  sr := lse_stream_readln(FEngine.StdinStream);
  lse_strec_inclife(sr);
  Result := lse_strec_string(sr);
  lse_strec_declife(sr);
end;

procedure KLiInvoke.ReturnFloat(const Value: double);
begin
  SetNil(FParam^.result);
  __PutFloat(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnInt64(const Value: int64);
begin
  SetNil(FParam^.result);
  __PutInt64(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnMoney(const Value: currency);
begin
  SetNil(FParam^.result);
  __PutMoney(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnObject(const Value: pointer; AClass: KLiClass);
begin
  SetNil(FParam^.result);
  __PutObject(FParam^.result, AClass.ClassRec, Value);
end;

procedure KLiInvoke.ReturnObj(const Value: pointer; AClass: PLseClassRec);
begin
  SetNil(FParam^.result);
  __PutObject(FParam^.result, AClass, Value);
end;

procedure KLiInvoke.ReturnStr(const Value: string);
begin
  SetNil(FParam^.result);
  __PutString(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnStream(const Value: TStream);
begin
  SetNil(FParam^.result);
  ReturnStream(lse_wrap_stream(Value, true));
end;

procedure KLiInvoke.ReturnStream(const Value: PLseStream);
begin
  SetNil(FParam^.result);
  __PutObject(Param^.result, KR_STREAM, Value);
end;

procedure KLiInvoke.ReturnStrlist(const Value: KLiStrList);
begin
  SetNil(FParam^.result);
  __PutObject(Param^.result, KR_STRLIST, Value);
end;

procedure KLiInvoke.ReturnTime(const Value: TDateTime);
begin
  SetNil(FParam^.result);
  __PutTime(FParam^.result, Value);
end;

procedure KLiInvoke.ReturnInt(const Value: integer);
begin
  SetNil(FParam^.result);
  __PutInt64(FParam^.result, Value);
end;

function KLiInvoke.GetThis(var This): boolean;
var
  lobj: pointer;
begin
  lobj := ParamObject(0);
  Result := (lobj <> nil);
  if Result then
    pointer(This) := lobj else
    __SetError(FParam, 'this was not supplied');
end;

procedure KLiInvoke.SetNil(Value: PLseValue);
var
  cr: PLseClassRec;
begin
  cr := Value^.value_class;
  if cr <> nil then
    if cr^.vtype = LSV_STRING then
      lse_strec_declife(Value^.VString) else
    if cr^.vtype = LSV_OBJECT then
      cr^.decRefcount(Value^.VObject);
end;

constructor KLiInvoke.Create(const AEngine: KLiEngine);
begin
  FEngine := AEngine;
  FParam := nil;
end;

function KLiInvoke.ParamBool(Index: integer): boolean;
begin
  Result := FParam^.param[Index]^.VBool;
end;

function KLiInvoke.ParamChar(Index: integer): char;
begin
  Result := FParam^.param[Index]^.VChar;
end;

function KLiInvoke.ParamClass(Index: integer): KLiClass;
begin
  Result:= KLiClass(ParamClassRec(Index)^.lysee_class);
end;

function KLiInvoke.ParamClassRec(Index: integer): PLseClassRec;
begin
  Result:= FParam^.param[Index]^.value_class;
  if Result = nil then Result := KR_VOID;
end;

function KLiInvoke.ParamCount: integer;
begin
  Result := FParam^.count;
end;

function KLiInvoke.ParamFloat(Index: integer): double;
begin
  Result := FParam^.param[Index]^.VFloat;
end;

function KLiInvoke.ParamFmt(Index: integer): string;
begin
  Result := FormatStr(ParamStr(Index));
end;

function KLiInvoke.ParamInt(Index: integer): integer;
begin
  Result := FParam^.param[Index]^.VInteger;
end;

function KLiInvoke.ParamInt64(Index: integer): int64;
begin
  Result := FParam^.param[Index]^.VInteger;
end;

function KLiInvoke.ParamMoney(Index: integer): currency;
begin
  Result := FParam^.param[Index]^.VMoney;
end;

function KLiInvoke.ParamObject(Index: integer): pointer;
begin
  Result := FParam^.param[Index]^.VObject;
end;

function KLiInvoke.ParamStr(Index: integer): string;
begin
  Result := lse_strec_string(FParam^.param[Index]^.VString);
end;

function KLiInvoke.ParamCStr(Index: integer; var Size: integer): pchar;
var
  sr: PLseString;
begin
  sr := FParam^.param[Index]^.VString;
  Result := lse_strec_data(sr);
  Size := lse_strec_length(sr);
end;

function KLiInvoke.ParamStrec(Index: integer): PLseString;
begin
  Result := FParam^.param[Index]^.VString;
end;

function KLiInvoke.ParamStream(Index: integer): PLseStream;
begin
  Result := PLseStream(FParam^.param[Index]^.VObject);
end;

function KLiInvoke.ParamTime(Index: integer): TDateTime;
begin
  Result := FParam^.param[Index]^.VTime;
end;

procedure KLiInvoke.Print(const Str: string);
begin
  lse_stream_write(FEngine.StdoutStream, Str);
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
      __LoadConfig;
      __SetupLseClasses;

      for X := Low(KLiSymbol) to High(KLiSymbol) do
      begin
        if X in OperIDSyms then
          KLiFunc_operator.Create(X);
        sys_runner_procs[X] := @__runner_error;
      end;
      sys_oper_inc := sys_module.FindFunc('+');

      sys_runner_procs[syPress]    := @__runner_press;
      sys_runner_procs[syID]       := @__runner_ID;
      sys_runner_procs[syBecome]   := @__runner_become;
      sys_runner_procs[syFloat]    := @__runner_float;
      sys_runner_procs[syMoney]    := @__runner_money;
      sys_runner_procs[syCall]     := @__runner_call;
      sys_runner_procs[syOut]      := @__runner_out;
      sys_runner_procs[syTime]     := @__runner_time;
      sys_runner_procs[syInt]      := @__runner_int;
      sys_runner_procs[syStr]      := @__runner_str;
      sys_runner_procs[syChar]     := @__runner_char;
      sys_runner_procs[syAdd]      := @__runner_add;
      sys_runner_procs[syDec]      := @__runner_dec;
      sys_runner_procs[syMul]      := @__runner_mul;
      sys_runner_procs[syDiv]      := @__runner_div;
      sys_runner_procs[syMod]      := @__runner_mod;
      sys_runner_procs[syBNot]     := @__runner_bnot;
      sys_runner_procs[syBXor]     := @__runner_bxor;
      sys_runner_procs[syBOr]      := @__runner_bor;
      sys_runner_procs[syBAnd]     := @__runner_band;
      sys_runner_procs[syBShl]     := @__runner_bshl;
      sys_runner_procs[syBShr]     := @__runner_bshr;
      sys_runner_procs[syNot]      := @__runner_not;
      sys_runner_procs[syNeg]      := @__runner_neg;
      sys_runner_procs[syEQ]       := @__runner_eq;
      sys_runner_procs[syAbsEQ]    := @__runner_abseq;
      sys_runner_procs[syNE]       := @__runner_ne;
      sys_runner_procs[syLT]       := @__runner_lt;
      sys_runner_procs[syLE]       := @__runner_le;
      sys_runner_procs[syGT]       := @__runner_gt;
      sys_runner_procs[syGE]       := @__runner_ge;
      sys_runner_procs[syIn]       := @__runner_in;
      sys_runner_procs[syAnd]      := @__runner_and;
      sys_runner_procs[syOr]       := @__runner_or;
      sys_runner_procs[syTrue]     := @__runner_true;
      sys_runner_procs[syFalse]    := @__runner_false;
      sys_runner_procs[syType]     := @__runner_type;
      sys_runner_procs[syFunc]     := @__runner_func;
      sys_runner_procs[syVarlist]  := @__runner_varlist;
      sys_runner_procs[syNil]      := @__runner_nil;
      sys_runner_procs[syShell]    := @__runner_shell;
      sys_runner_procs[syGetValue] := @__runner_getvalue;
      sys_runner_procs[sySetValue] := @__runner_setvalue;
      sys_runner_procs[syFormat]   := @__runner_format;
      sys_runner_procs[syBool]     := @__runner_bool;
      sys_runner_procs[syMethod]   := @__runner_method;
      sys_runner_procs[syPuts]     := @__runner_puts;
      sys_runner_procs[syModule]   := @__runner_module;
      sys_runner_procs[syIs]       := @__runner_is;
      sys_runner_procs[syAs]       := @__runner_as;
      sys_runner_procs[syStatement]:= @__runner_statement;
      sys_runner_procs[syVarGen]   := @__runner_vargen;
      sys_runner_procs[syPushVarb] := @__runner_pushvarb;
      sys_runner_procs[syAsk]      := @__runner_ask;
      sys_runner_procs[syLabel]    := @__runner_label;
      sys_runner_procs[syIdle]     := @__runner_idle;
      sys_runner_procs[syTry]      := @__runner_try;
      sys_runner_procs[syReturn]   := @__runner_return;
      sys_runner_procs[syJump]     := @__runner_jump;
      sys_runner_procs[syJmpF]     := @__runner_jmpf;
      sys_runner_procs[syJmpT]     := @__runner_jmpt;
      sys_runner_procs[syJmpFPop]  := @__runner_jmpfpop;
      sys_runner_procs[syJmpTPop]  := @__runner_jmptpop;
      sys_runner_procs[syGoto]     := @__runner_goto;
      sys_runner_procs[syGotoTP]   := @__runner_gototp;
      sys_runner_procs[syGotoFP]   := @__runner_gotofp;
      sys_runner_procs[syHashed]   := @__runner_hashed;
      sys_runner_procs[syCallAsk]  := @__runner_callask;
      sys_runner_procs[syUpto]     := @__runner_upto;
      sys_runner_procs[syRINR]     := @__runner_RINR;
      sys_runner_procs[syGETV]     := @__runner_GETV;
      sys_runner_procs[sySETV]     := @__runner_SETV;
      sys_runner_procs[syLike]     := @__runner_Like;
      sys_runner_procs[syGetPV]    := @__runner_getpv;
      sys_runner_procs[sySetPV]    := @__runner_setpv;
      sys_runner_procs[syGetIV]    := @__runner_getiv;
      sys_runner_procs[sySetIV]    := @__runner_setiv;
      sys_runner_procs[syClen]     := @__runner_clen;
      sys_runner_procs[syThis]     := @__runner_this;
      sys_runner_procs[syDupLast]  := @__runner_duplast;
      sys_runner_procs[syAddAll]   := @__runner_addAll;
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
      __freeAndNil(sys_log_stream);
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

