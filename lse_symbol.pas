{==============================================================================}
{        UNIT: lse_spawn                                                       }
{ DESCRIPTION: symbolization of lysee script parser                            }
{     CREATED: 2003/02/26                                                      }
{    MODIFIED: 2010/10/16                                                      }
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
unit lse_symbol;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lse_funcs;

type
  KLiSymbol = (
    syError, syID, syFloat, syInt, syStr, syChar, syImport,
    syReturn, syCall, syIf, syAnd, syOr, syNot, syElse, syFor,
    syWhile, syBreak, syContinue, syTry, syExcept, syFinally,
    syDo, syCase, syBecome, syAdd, syAdd1, syDec, syDec1,
    syMul, syDiv, syMod, syBNot, syBXor, syBAnd, syBOr, syBShl,
    syBShr, syLParen, syRParen, syLBlock, syRBlock, syLArray, syRArray,
    syDot, syAsk, syDot2, syDot4, syComma, syDotComma, syNeg,
    syEQ, syNE, syLT, syLE, syGT, syGE, syJmpT, syJmpF, syJmpTPop,
    syJmpFPop, syJump, syPress, syOut, syTrue, syFalse, syType, syFunc,
    syVarlist, syNil, syIdle,
    syShell,       // 2005-01-22: shell execute
    syGetValue,    // 2006-09-10: return a configuration value
    syFormat,      // 2007-02-08: add '@' for formating string 
    sySwitch,      // 2007-02-21: enable switch clause
    syBool,        // 2007-02-21: cast last to bool
    syMethod,      // 2007-06-02: wrap stack last two into method
    syIn,          // 2007-07-06: key word 'in'
    syPuts,        // 2007-07-14: print string
    syInclude,     // 2007-07-14: include LSP file
    syLabel,       // 2007-08-07: label
    syGoto,        // 2007-08-07: goto label
    syModule,      // 2007-08-12: push module
    syIs,          // 2007-09-01: is ? type checking
    syAs,          // 2007-09-02: type casting
    syMoney,       // 2007-09-07: push money
    syTime,        // 2007-09-07: push time
    syGotoTP,      // 2008-01-27: goto label when true
    syGotoFP,      // 2008-01-28: goto label when false
    syEndInc,      // 2008-03-03: end include file
    syStatement,   // 2008-03-25: begin statement
    sySaveRV,      // 2008-03-25: save result value
    syVarGen,      // 2008-09-07: cast to vargen object
    syPushVarb,    // 2008-09-07: push variable
    syHashed,      // 2009-02-28: create hashed object  
    sySetValue,    // 2009-03-07: set configuration value
    syCallAsk,     // 2009-09-07: ask after call
    syUpto,        // 2009-09-16: create integer upto range
    syAbsEQ,       // 2009-09-24: absolutely equal
    syRINR,        // 2009-09-25: run in range
    syGETV,        // 2009-09-26: get temp value
    sySETV,        // 2009-09-26: set temp value
    syLike,        // 2010-02-01: string like patten
    syGetPV,       // 2010-02-12: get property
    sySetPV,       // 2010-02-13: set property
    syGetIV,       // 2010-02-13: get item value
    sySetIV,       // 2010-02-13: set item value
    syClen,        // 2010-03-23: callstack length
    syThis,        // 2010-04-09: current variable snap
    syDefine,      // 2010-07-10: define function or constant
    syDupLast,     // 2010-07-17: duplicate last value
    syThen,        // 2010-07-18: then clause
    syEnd,         // 2010-07-18: end block
    syRepeat,      // 2010-07-18: begin do..while loop
    syUntil,       // 2010-07-18: end do..while loop
    syConst,       // 2010-07-20: define constant values
    syEOF,         // 2010-07-24: end of file
    syElseIf,      // 2010-08-01: else if
    syElif,        // 2010-08-01: else if
    syClass,       // 2010-08-01: define class
    syOption,      // 2010-08-18: kernel engine option
    syAddAll       // 2010-09-04: add all item from vargen
  );
  KLiSymbols = set of KLiSymbol;

const
  Symbols: array[KLiSymbol] of packed record
    SY: KLiSymbol; // symbol value
    ID: pchar;     // symbol spelling
    SM: pchar;     // symbol description
  end = (
    (SY:syError;      ID:'<ERROR>';      SM:'error'),
    (SY:syID;         ID:'<ID>';         SM:'identity name'),
    (SY:syFloat;      ID:'<FLOAT>';      SM:'push float'),
    (SY:syInt;        ID:'<INT>';        SM:'push int'),
    (SY:syStr;        ID:'<STR>';        SM:'push string'),
    (SY:syChar;       ID:'<CHAR>';       SM:'push char'),
    (SY:syImport;     ID:'import';       SM:'import module [in ...]'),
    (SY:syReturn;     ID:'return';       SM:'return'),
    (SY:syCall;       ID:'<CALL>';       SM:'call function'),
    (SY:syIf;         ID:'if';           SM:'if'),
    (SY:syAnd;        ID:'and';          SM:'logical and'),
    (SY:syOr;         ID:'or';           SM:'logical or'),
    (SY:syNot;        ID:'not';          SM:'logical not'),
    (SY:syElse;       ID:'else';         SM:'else'),
    (SY:syFor;        ID:'for';          SM:'for'),
    (SY:syWhile;      ID:'while';        SM:'while'),
    (SY:syBreak;      ID:'break';        SM:'break loop'),
    (SY:syContinue;   ID:'continue';     SM:'continue loop'),
    (SY:syTry;        ID:'try';          SM:'try'),
    (SY:syExcept;     ID:'except';       SM:'catch execption'),
    (SY:syFinally;    ID:'finally';      SM:'finally'),
    (SY:syDo;         ID:'do';           SM:'do'),
    (SY:syCase;       ID:'case';         SM:'case'),
    (SY:syBecome;     ID:'=';            SM:'become'),
    (SY:syAdd;        ID:'+';            SM:'add'),
    (SY:syAdd1;       ID:'++';           SM:'add one'),
    (SY:syDec;        ID:'-';            SM:'dec'),
    (SY:syDec1;       ID:'--';           SM:'dec one'),
    (SY:syMul;        ID:'*';            SM:'mul'),
    (SY:syDiv;        ID:'/';            SM:'div'),
    (SY:syMod;        ID:'%';            SM:'mod'),
    (SY:syBNot;       ID:'~';            SM:'bit xor -1'),
    (SY:syBXor;       ID:'^';            SM:'bit xor'),
    (SY:syBAnd;       ID:'&';            SM:'bit and'),
    (SY:syBor;        ID:'|';            SM:'bit or'),
    (SY:syBShl;       ID:'<<';           SM:'bit shift left'),
    (SY:syBShr;       ID:'>>';           SM:'bit shift right'),
    (SY:syLParen;     ID:'(';            SM:'left paren'),
    (SY:syRParen;     ID:')';            SM:'right paren'),
    (SY:syLBlock;     ID:'{';            SM:'left block'),
    (SY:syRBlock;     ID:'}';            SM:'right block'),
    (SY:syLArray;     ID:'[';            SM:'left array'),
    (SY:syRArray;     ID:']';            SM:'right array'),
    (SY:syDot;        ID:'.';            SM:'dot'),
    (SY:syAsk;        ID:'?';            SM:'ask'),
    (SY:syDot2;       ID:':';            SM:'dot 2'),
    (SY:syDot4;       ID:'::';           SM:'double dot 2'),
    (SY:syComma;      ID:',';            SM:'comma'),
    (SY:syDotComma;   ID:';';            SM:'dot comma'),
    (SY:syNeg;        ID:'-';            SM:'neg'),
    (SY:syEQ;         ID:'==';           SM:'equal'),
    (SY:syNE;         ID:'!=';           SM:'not equal'),
    (SY:syLT;         ID:'<';            SM:'less'),
    (SY:syLE;         ID:'<=';           SM:'less equal'),
    (SY:syGT;         ID:'>';            SM:'great'),
    (SY:syGE;         ID:'>=';           SM:'great equal'),
    (SY:syJmpT;       ID:'<JMPT>';       SM:'jump true'),
    (SY:syJmpF;       ID:'<JMPF>';       SM:'jump false'),
    (SY:syJmpTPop;    ID:'<JMPTPOP>';    SM:'jump true pop last'),
    (SY:syJmpFPop;    ID:'<JMPFPOP>';    SM:'jump flase pop last'),
    (SY:syJump;       ID:'<JUMP>';       SM:'jump'),
    (SY:syPress;      ID:'<PRESS>';      SM:'pop'),
    (SY:syOut;        ID:'<PRINT>';      SM:'print'),
    (SY:syTrue;       ID:'true';         SM:'true value'),
    (SY:syFalse;      ID:'false';        SM:'false value'),
    (SY:syType;       ID:'<TYPE>';       SM:'push class'),
    (SY:syFunc;       ID:'<FUNC>';       SM:'push function'),
    (SY:syVarlist;    ID:'<VARLIST>';    SM:'create varlist'),
    (SY:syNil;        ID:'nil';          SM:'nil value'),
    (SY:syIdle;       ID:'<IDLE>';       SM:'idle'),
    (SY:syShell;      ID:'`';            SM:'shell execute'),
    (SY:syGetValue;   ID:'<VALUE>';      SM:'push enviromnent value'),
    (SY:syFormat;     ID:'@';            SM:'format string'),
    (SY:sySwitch;     ID:'switch';       SM:'switch'),
    (SY:syBool;       ID:'<BOOL>';       SM:'cast to boolean'),
    (SY:syMethod;     ID:'<METHOD>';     SM:'wrap last two to method'),
    (SY:syIn;         ID:'in';           SM:'in'),
    (SY:syPuts;       ID:'<PUTS>';       SM:'print string'),
    (SY:syInclude;    ID:'include';      SM:'include file'),
    (SY:syLabel;      ID:'<LABEL>';      SM:'label'),
    (SY:syGoto;       ID:'<GOTO>';       SM:'goto label'),
    (SY:syModule;     ID:'<MODULE>';     SM:'push module'),
    (SY:syIs;         ID:'is';           SM:'type checking'),
    (SY:syAs;         ID:'as';           SM:'type casting'),
    (SY:syMoney;      ID:'<MONEY>';      SM:'push money'),
    (SY:syTime;       ID:'<TIME>';       SM:'push time'),
    (SY:syGotoTP;     ID:'<GOTOTP>';     SM:'goto label when true'),
    (SY:syGotoFP;     ID:'<GOTOFP>';     SM:'goto label when false'),
    (SY:syEndInc;     ID:'<ENDINC>';     SM:'end include file'),
    (SY:syStatement;  ID:'<STATEMENT>';  SM:'begin statement'),
    (SY:sySaveRV;     ID:'<SAVERV>';     SM:'save result value'),
    (SY:syVarGen;     ID:'<VARGEN>';     SM:'cast to vargen object'),
    (SY:syPushVarb;   ID:'<PUSHVARB>';   SM:'push variable'),
    (SY:syHashed;     ID:'<HASHED>';     SM:'create hashed object'),  
    (sy:sySetValue;   ID:'<SETVALUE>';   SM:'set configuration value'),
    (sy:syCallAsk;    ID:'<CALLASK>';    SM:'ask after call'),
    (sy:syUpto;       ID:'..';           SM:'create integer upto range'),
    (sy:syAbsEQ;      ID:'===';          SM:'absolutely equal'),
    (sy:syRINR;       ID:'<RINR>';       SM:'run in range'),
    (sy:syGETV;       ID:'<GETV>';       SM:'get temp value'),
    (sy:sySETV;       ID:'<SETV>';       SM:'set temp value'),
    (sy:syLike;       ID:'like';         SM:'string like patten'),
    (sy:syGetPV;      ID:'<GETPV>';      SM:'get property'),
    (sy:sySetPV;      ID:'<SETPV>';      SM:'set property'),
    (sy:syGetIV;      ID:'<GETIV>';      SM:'get item value'),
    (sy:sySetIV;      ID:'<SETIV>';      SM:'set item value'),
    (sy:syClen;       ID:'__clen__';     SM:'callstack length'),
    (sy:syThis;       ID:'this';         SM:'current variable snap'),
    (sy:syDefine;     ID:'def';          SM:'define function or constant'),
    (sy:syDupLast;    ID:'<DUPLAST>';    SM:'duplicate last value'),
    (sy:syThen;       ID:'then';         SM:'then clause'),
    (sy:syEnd;        ID:'end';          SM:'end block'),
    (sy:syRepeat;     ID:'repeat';       SM:'begin do..while loop'),
    (sy:syUntil;      ID:'until';        SM:'end do..while loop'),
    (sy:syConst;      ID:'const';        SM:'define constant values'),
    (sy:syEOF;        ID:'<EOF>';        SM:'end of file'),
    (sy:syElseIf;     ID:'elseif';       SM:'else if'),
    (sy:syElif;       ID:'elif';         SM:'else if'),
    (sy:syClass;      ID:'class';        SM:'define class'),
    (sy:syOption;     ID:'<OPTION>';     SM:'kernel engine option'),
    (sy:syAddAll;     ID:'+<';           SM:'add all item from vargen')
  );

  ConstantSyms = [
    syFloat, syMoney, syTime, syInt, syStr, syChar, syTrue, syFalse,
    syNil, syShell, syGetValue, syClen, syThis
  ];

  ExprOperSyms: array[0..4] of KLiSymbols = (
    {syNeg, syNot, syBNot, syFormat}                                         // 1
    [syMul, syDiv, syMod],                                                   // 2
    [syAdd, syDec],                                                          // 3
    [syBXor, syBAnd, syBOr, syBShl, syBShr, syUpto, syAddAll],               // 4
    [syEQ, syNE, syLT, syLE, syGT, syGE, syIn, syAbsEQ, syLike, syAs, syIs], // 6
    [syAnd, syOr]                                                            // 7
  );

  OperIDSyms = [syMul, syDiv, syMod, syAdd, syDec, syBXor, syBAnd,
    syBOr, syBShl, syBShr, syAddAll, syEQ, syNE, syLT, syLE, syGT, syGE,
    syAbsEQ, syLike, syAs, syIs, syAnd, syOr];

  ExprHeadSyms = ConstantSyms + OperIDSyms + [
    syID, syNot, syDec, syBNot, syLParen, syLArray, syLBlock, syFormat,
    syPuts, syBOr
  ];

  ExprEndSyms = [syIf, syElse, syExcept, syFinally, syBecome, syRParen,
    syRBlock, syRArray, syDot, syAsk, syDot2, syDot4, syComma, syDotComma,
    syIn, syEndInc, syUpto, syThen, syEnd, syUntil, syEOF, syElseIf];
    
type
  KLiSymPos = packed record
    fid: word; // file ID
    row: word; // row index
    col: word; // column index
  end;
  PLiSymPos = ^KLiSymPos;

  PLiToken = ^RLiToken;
  RLiToken = packed record
    next: PLiToken;            {<--next token}
    Sym: KLiSymbol;            {<--symbol}
    Pos: KLiSymPos;            {<--start position}
    Val: string;               {<--spelling}
    case integer of
    0: (VInteger: int64);      {<--syInt}
    1: (VFloat: double);       {<--syFloat}
    2: (VParamCount: integer); {<--parametre count}
    3: (VChar: char);          {<--syChar}
    4: (VPureID: boolean);     {<--syID has no '::'}
    5: (VType: pointer);       {<--syIs, syAs}
  end;

  KLiGetToken = procedure(Sender: TObject; Token: PLiToken;
    xpos, xbeg, xend: integer) of object;

  { KLiTokenizer }

  KLiTokenizer = class(KLiObject)
  private
    FTokenList: array[0..7] of RLiToken; {<--token buffer list}
    FUnused: PLiToken;                   {<--unused tokens}
    FCurrent: PLiToken;                  {<--current token}
    FCode: string;                       {<--script code}
    FCodeBase: pchar;                    {<--start address of FCode}
    FCodeSize: integer;                  {<--code size}
    FPosition: integer;                  {<--current position}
    FHeadPosition: integer;              {<--head char position}
    FChar: char;                         {<--current char}
    FRow: integer;                       {<--current row}
    FCol: integer;                       {<--current column}
    FFileID: integer;                    {<--current file ID}
    FBuffer: TMemoryStream;              {<--used by get_string}
    FModuleTemplate: KLiStrlist;         {<--module template names}
    FIsLsp: boolean;                     {<--if is parsing LSP}
    FInLspCode: boolean;                 {<--if within code}
    FEOF: boolean;                       {<--end of file}
    FOnGetToken: KLiGetToken;            {<--get token event}
    FIncludeCount: integer;              {<--rest include count}
    FEndInc: string;                     {<--end include keyword}
    FSkip10: boolean;                    {<--GetChar skip #10}
    function GetChar: boolean;
    function PeekChar: char;
    function GotoChar(Chars: KLiCharSet): boolean;
    function SkipChar(Count: integer): boolean;
    function SkipSpaces: boolean;
    function GetHeadChar: boolean;
    function GetToken(token: PLiToken; var IsStr: boolean): boolean;
    function GetCurrentToken: PLiToken;
    function BeginID(ch: char; CheckHead: boolean): boolean;
  public
    constructor Create(const Script: string; FID: integer; IsLsp: boolean);
    destructor Destroy;override;
    function GetNextToken(var IsStr: boolean): PLiToken;
    function GetFileNameToken(token: PLiToken): boolean;
    function GetValueNameToken(token: PLiToken): boolean;
    function GetOptionValueToken(token: PLiToken): boolean;
    function PeekNextToken: PLiToken;
    function PeekNextKeywordToken: PLiToken;
    function PeekNextThreeTokens(var one, two, three: PLiToken): integer;
    function PeekNextThreeSym(var one, two, three: KLiSymbol): integer;
    function PrepareTokens(Count: integer): integer;
    function DupCurrentToken: PLiToken;
    function Include(const FileName, TAG: string; FileID: integer): boolean;
    function ReadTo(DestiChar: KLiCharSet; var S: string): boolean;
    function HeadChar: char;
    property Current: PLiToken read GetCurrentToken;
    property FileID: integer read FFileID write FFileID;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Position: integer read FPosition;
    property Code: string read FCode;
    property CodeSize: integer read FCodeSize;
    property ModuleTemplate: KLiStrlist read FModuleTemplate write FModuleTemplate;
  end;

  KLiTokens = class(KLiObject)
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

{-----------------------------------------------------------------------
( F_NAME: IsLyseeID
( 
( F_DESC: 测试输入的字符串是否是合法的标识符
( 
( F_ARGS: const ID: string - 字符串
( 
( F_TYPE: boolean - 如果是则返回真值
( 
( EXCEPT:
(----------------------------------------------------------------------}
function IsLyseeID(const ID: string): boolean;

{-----------------------------------------------------------------------
( F_NAME: StrToSym
( 
( F_DESC: 测试输入的字符串属于哪种符号
( 
( F_ARGS: const S: string - 字符串
(         DefValue: KLiSymbol - 默认符号
( 
( F_TYPE: 如果未找到匹配的符号，函数返回DefValue
( 
( EXCEPT:
(----------------------------------------------------------------------}
function StrToSym(const S: string; DefValue: KLiSymbol): KLiSymbol;

{-----------------------------------------------------------------------
( F_NAME: SymToStr
( 
( F_DESC: 将符号转换为其程序文本
( 
( F_ARGS: Sym: KLiSymbol - 符号
( 
( F_TYPE: 符号文本
( 
( EXCEPT:
(----------------------------------------------------------------------}
function SymToStr(Sym: KLiSymbol): string;

{-----------------------------------------------------------------------
( F_NAME: SymsToStrList
( 
( F_DESC: 将符号集转换为其程序文本列表
( 
( F_ARGS: Syms: KLiSymbols - 符号集
( 
( F_TYPE: 符号文本列表
( 
( EXCEPT:
(----------------------------------------------------------------------}
function SymsToStrList(syms: KLiSymbols): string;

{-----------------------------------------------------------------------
( F_NAME: IsKeyword
( 
( F_DESC: 测试输入的字符串是否是Lysee关键字
( 
( F_ARGS: const ID: string - 对象名称
( 
( F_TYPE: 如果ID是系统预定义关键字则返回真，否则返回假
( 
( EXCEPT:
(----------------------------------------------------------------------}
function IsKeyword(const ID: string): boolean;
function IsKeywordSymbol(sym: KLiSymbol): boolean;

{-----------------------------------------------------------------------
( F_NAME: ReservedWords
( 
( F_DESC: 返回保留字列表
( 
( F_ARGS:
( 
( F_TYPE: string - 保留字列表
( 
( EXCEPT:
(----------------------------------------------------------------------}
function ReservedWords: string;

{-----------------------------------------------------------------------
( F_NAME: IsPureID
( 
( F_DESC: 测试指定符号是否是纯标识符(不含'::')
( 
( F_ARGS: token: PLiToken - 符号记录
( 
( F_TYPE: boolean - 如果是纯标识符则返回真，否则返回假
( 
( EXCEPT:
(----------------------------------------------------------------------}
function IsPureID(token: PLiToken): boolean;

{-----------------------------------------------------------------------
( F_NAME: ComparePos
( 
( F_DESC: 比较两个位置的前后
( 
( F_ARGS: P1 - PLiSymPos - 位置1
(         P2 - PLiSymPos - 位置2
( 
( F_TYPE: integer - -X:之前 0:同一位置 +X:之后
( 
( EXCEPT:
(----------------------------------------------------------------------}
function ComparePos(P1, P2: PLiSymPos): integer;

{-----------------------------------------------------------------------
( F_NAME: SimpleTest
( 
( F_DESC: 检测代码是否有可能执行
( 
( F_ARGS: const Script: string - 脚本
( 
( F_TYPE: integer - -1=绝无可能 0=可以 1=还未完
( 
( EXCEPT:
(----------------------------------------------------------------------}
function SimpleTest(const Script: string): integer;

function HeadIs__(const S: string): boolean;

var
  reserved_words: string = '';

implementation

uses
  math, lse_kernel, lseu;

function IsLyseeID(const ID: string): boolean;
var
  S: pchar;
begin
  Result := false;
  S := pchar(ID);
  if (S <> nil) and (S^ in IDHeadChar) then
  begin
    Inc(S);
    Result := (S^ = #0) or __inCharSet(S, IDChar);
  end;
end;

function StrToSym(const S: string; DefValue: KLiSymbol): KLiSymbol;
var
  X: KLiSymbol;
  L: integer;
begin
  L := Length(S);
  if (L > 0) and ((S[1] <> '<') or (S[L] <> '>')) then
  begin
    for X := Low(KLiSymbol) to High(KLiSymbol) do
      if Symbols[X].ID = S then
      begin
        Result := X;
        Exit;
      end;
  end;
  Result := DefValue;
end;

function SymToStr(Sym: KLiSymbol): string;
var
  L: integer;
begin
  Result := Symbols[Sym].ID;
  L := Length(Result);
  if (Result[1] <> '<') or (Result[L] <> '>') then
    Result := '"' + Result + '"';
end;

function SymsToStrList(syms: KLiSymbols): string;
var
  A: KLiSymbol;
begin
  Result := '';
  for A := Low(KLiSymbol) to High(KLiSymbol) do
    if A in Syms then if Result <> '' then
      Result := Format('%s, %s', [Result, SymToStr(A)]) else
      Result := Format('%s', [SymToStr(A)]);
end;

function IsKeyword(const ID: string): boolean;
begin
  Result := __isIDStr(pchar(ID)) and (StrToSym(ID, syError) <> syError);
end;

function IsKeywordSymbol(sym: KLiSymbol): boolean;
begin
  Result := Symbols[sym].ID[1] in IDHeadChar;
end;

function ReservedWords: string;
var
  sym: KLiSymbol;
  key: string;
  list: TStringList;
begin
  if reserved_words = '' then
  begin
    list := __newNamedList(false);
    try
      list.CommaText := 'this,void,int,float,string,bool,time,char,object,variant,sys,type,main,super,recall';
      for sym := Low(KLiSymbol) to High(KLiSymbol) do
      begin
        key := Symbols[sym].ID;
        if key[1] in IDHeadChar then
          if list.IndexOf(key) < 0 then
            list.Add(key);
      end;
      list.Sort;
      reserved_words := list.CommaText;
    finally
      list.Free;
    end;
  end;
  Result := reserved_words;
end;

function IsPureID(token: PLiToken): boolean;
begin
  Result := (token^.Sym = syID) and token^.VPureID;
end;

function ComparePos(P1, P2: PLiSymPos): integer;
begin
  Result := P1^.row - P2^.row;
  if Result = 0 then
    Result := P1^.col - P2^.col;
end;

{ KLiTokenizer }

function KLiTokenizer.BeginID(ch: char; CheckHead: boolean): boolean;
begin
  if CheckHead then
    Result := __isIDHead(ch) else
    Result := (ch in IDChar);
end;

constructor KLiTokenizer.Create(const Script: string; FID: integer; IsLsp: boolean);
var
  index: integer;
begin
  FCode := Script;
  FIsLsp := IsLsp;
  FInLspCode := false;
  FEOF := false;
  FCodeBase := pchar(FCode);
  FCodeSize := Length(FCode);
  FPosition := 1;
  FHeadPosition := FCodeSize + 1;
  FFileID := FID;  
  FRow := 0;
  FCol := 0;
  FIncludeCount := 0;
  FEndInc := Format('%p', [pointer(Self)]);
  FEndInc[1] := 'R';
  FSkip10 := false;

  // SKIP FIRST LINE WHICH LOOKS LIKE: #!......
  if Copy(FCode, 1, 2) = '#!' then
  begin
    index := 3;
    while (index <= FCodeSize) and not (FCode[index] in [#13, #10]) do
      Inc(index);
    if (index < FCodeSize) and (FCode[index] = #13) then
      if FCode[index + 1] = #10 then
        Inc(index);
    FPosition := index + 1;
    Inc(FRow);
  end;

  if FPosition <= FCodeSize then
    FChar := FCode[FPosition] else
    FChar := #0;
  FillChar(FTokenList, sizeof(FTokenList), 0);
  FUnused := @FTokenList[0];
  FCurrent := FUnused;
  for Index := 1 to Length(FTokenList) - 1 do
  begin
    FCurrent^.next := @FTokenList[Index];
    FCurrent := FCurrent^.next;
  end;
  FCurrent := nil;
  FBuffer := TMemoryStream.Create;
end;

destructor KLiTokenizer.Destroy;
var
  A: integer;
begin
  FreeAndNil(FBuffer);
  for A := 0 to Length(FTokenList) - 1 do
    FTokenList[A].Val := '';
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
    FUnused := FUnused^.next;
    Result^.next := token^.next;
    Result^.Sym := token^.Sym;
    Result^.Pos := token^.Pos;
    Result^.Val := token^.Val;
    Result^.VInteger := token^.VInteger;
    token^.next := Result;
  end;
  Result := token;
end;

function KLiTokenizer.GetChar: boolean;
var
  F: boolean;
begin
  FSkip10 := false;
  F := (FChar = #13);
  if F or (FChar = #10) then
  begin
    Inc(FRow);
    FCol := 0;
  end
  else Inc(FCol);
  Inc(FPosition);
  Result := (FPosition <= FCodeSize);
  if Result then
  begin
    FChar := FCode[FPosition];
    if F and (FChar = #10) then
    begin
      FSkip10 := true;
      Inc(FPosition);
      Result := (FPosition <= FCodeSize);
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

function KLiTokenizer.GetFileNameToken(token: PLiToken): boolean;
const
  FNCHS = IDChar + ['/', '\', ':', '.', '-', '$', '{', '}', '@', '>'];
var
  isstr: boolean;
  fname: string;
  block: integer;
begin
  Result := (FChar <> #0) and GetHeadChar and (FChar in FNCHS);
  token^.Sym := syError;
  token^.Pos.fid := FFileID;
  token^.Pos.Row := FRow;
  token^.Pos.Col := FCol;
  token^.Val := FChar;
  token^.VInteger := 0;
  if Result then
  begin
    block := 0;
    isstr := not (FChar in IDChar); // allow digit heading
    fname := FChar;
    while GetChar and (FChar in FNCHS) do
    begin
      case FChar of
        ':': if FInLspCode and (PeekChar = '}') then Break;
        '{': if block = 0 then Inc(block) else Break;
        '}': if block = 1 then Dec(block) else Break;
      end;
      if not (FChar in IDChar) then isstr := true;
      fname := fname + FChar;
    end;
    token^.Val := fname;
    Result := (block = 0);
    if Result then
      if isstr then
        token^.Sym := syStr else
        token^.Sym := syID;
  end;
end;

function KLiTokenizer.GetHeadChar: boolean;
begin
  Result := SkipSpaces;
end;

function KLiTokenizer.GetNextToken(var IsStr: boolean): PLiToken;
var
  token: PLiToken;
  done: boolean;
begin
  if FCurrent = nil then
  begin
    done := GetToken(FUnused, IsStr);
    if done then
    begin
      FCurrent := FUnused;
      FUnused := FUnused^.next;
      FCurrent^.next := nil;
    end;
  end
  else
  if FCurrent^.next <> nil then
  begin
    token := FCurrent;
    FCurrent := FCurrent^.next;
    token^.next := FUnused;
    FUnused := token;
    done := true;
  end
  else done := GetToken(FCurrent, IsStr);
  if not done and (FCurrent <> nil) then
  begin
    FCurrent^.next := FUnused;
    FUnused := FCurrent;
    FCurrent := nil;
  end;
  Result := FCurrent;
end;

function KLiTokenizer.GetOptionValueToken(token: PLiToken): boolean;
var
  value: string;
  block: integer;
begin
  Result := (FChar <> #0) and GetHeadChar and (FChar <> '}');
  token^.Sym := syError;
  token^.Pos.fid := FFileID;
  token^.Pos.Row := FRow;
  token^.Pos.Col := FCol;
  token^.Val := FChar;
  token^.VInteger := 0;
  if Result then
  begin
    block := 0;
    value := FChar;
    while GetChar and (FChar <> #0) do
    begin
      if FSkip10 then
        value := value + #10;
      case FChar of
        '{': Inc(block);
        '}': if block > 0 then Dec(block) else Break;
      end;
      value := value + FChar;
    end;
    token^.Val := value;
    Result := (block = 0) and (FChar <> #0);
    if Result then
      token^.Sym := syStr;
  end;
end;

function KLiTokenizer.GetToken(token: PLiToken; var IsStr: boolean): boolean;
var
  base, next: pchar;
  xpos, xbeg, org_fid, org_row, org_col: integer;
  tmps: string;

  procedure trace_to_next;
  var
    endp: integer;
  begin
    endp := (next - FCodeBase) + 1;
    while FPosition < endp do GetChar;
  end;

  procedure get_number;
  var
    iv: int64;
    ev: extended;
    ei: KLiExtInt;
  begin
    next := FCodeBase + FPosition - 1;
    base := next;
    ev := __parseExtInt(next, iv, ei);
    if ei <> eiNone then
    begin
      if ei = eiInt then
      begin
        token^.VInteger := iv;
        token^.Sym := syInt;
      end
      else
      begin
        token^.VFloat := ev;
        token^.Sym := syFloat;
      end;
      trace_to_next;
    end;
    token^.Val := __newString(base, next - base);
  end;

  procedure get_char;
  begin
    base := FCodeBase + FPosition - 1;
    next := base;
    if __parseChar(next, token^.VChar) then
    begin
      token^.Sym := syChar;
      trace_to_next;
    end;
    token^.Val := __newString(base, next - base);
  end;

  function next_is_str: boolean;
  begin
    Result := GetHeadChar and (FChar = '"')
           or ((FChar = 'R') and (PeekChar = '"'));
  end;

  procedure get_string(allow_esc_char: boolean);
  begin
    IsStr := true;
    while true do
    begin
      FBuffer.Clear;
      base := FCodeBase + FPosition - 1;
      next := base;
      if __parseStr(next, FBuffer, allow_esc_char) then
      begin
        token^.Val := token^.Val + __newString(pchar(FBuffer.Memory), FBuffer.Size);
        trace_to_next;
        if next_is_str then
        begin
          allow_esc_char := (FChar = '"');
          if not allow_esc_char then GetChar;
        end
        else
        begin
          token^.Sym := syStr;
          Exit;
        end;
      end
      else Break;
    end;
  end;

  procedure get_pure_identity(pure_id: boolean);
  var
    idstr: string;
  begin
    if SkipSpaces and BeginID(FChar, true) then
    begin
      idstr := FChar;
      while GetChar and (FChar in IDChar) do
        idstr := idstr + FChar;
      if pure_id then
      begin
        token^.Val := idstr;
        token^.Sym := StrToSym(idstr, syID);
        if (token^.Sym = syID) and (idstr = FEndInc) then
          token^.Sym := syEndInc;
        if token^.Sym in [syID] then
          if SkipSpaces and (FChar = ':') and (PeekChar = ':') then
          begin
            token^.Val := token^.Val + '::';
            GetChar;
            GetChar;
            get_pure_identity(false);
          end
          else token^.VPureID := true;
      end
      else
      begin
        token^.Sym := syID;
        token^.Val := token^.Val + idstr;
      end;
    end
    else token^.Sym := syError;
  end;

  procedure get_shell(symbol: KLiSymbol);
  begin
    if GetChar and ReadTo(['`'], tmps) then
    begin
      token^.Val := tmps;
      token^.Sym := symbol;
    end;
  end;

  procedure get_identity;
  begin
    if (FChar = 'R') and (PeekChar = '"') then
    begin
      GetChar;
      get_string(false);
    end
    else get_pure_identity(true);
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
          token^.Sym := syms[index];
          token^.Val := Symbols[token^.Sym].ID;
          GetChar;
          Exit;
        end;
    end;
    token^.Sym := defaultSymbol;
    token^.Val := Symbols[token^.Sym].ID;
  end;

  procedure get_value;
  label ATTACH;
  var
    idstr: string;
  begin
    if GetChar and (FChar = '{') and GetChar and (FChar in IDChar) then
    begin
      // syntax: ${I.D.E.N.T}
      idstr := FChar;
      ATTACH:
      while GetChar and (FChar in (IDChar + ['-', '>', ':'])) do
        idstr := idstr + FChar;
      if FChar = '.' then
        if GetChar and (FChar in IDChar) then
        begin
          idstr := idstr + '.' + FChar;
          goto ATTACH;
        end
        else Exit;
      if FChar = '}' then
      begin
        token^.Val := TrimRight(idstr);
        token^.Sym := syGetValue;
        GetChar;
      end;
    end;
  end;

  procedure seek_lsp_code;
  var
    S: string;
  begin
    base := FCodeBase + FPosition - 1;
    next := base;
    while next^ <> #0 do
    begin
      if (next^ = '{') and ((next + 1)^ = ':') then Break;
      Inc(next);
    end;
    S := __newString(base, next - base);
    if next^ <> #0 then
      if Copy(S, Length(S) - 3, 4) = '<!--' then
        SetLength(S, Length(S) - 4);
    if S <> '' then
    begin
      token^.Val := S;
      token^.Sym := syPuts;
    end
    else token^.Sym := syDotComma;
    FInLspCode := true;
    trace_to_next;
    SkipChar(2);
  end;

  function read_integer: integer;
  begin
    GetToken(token, IsStr);
    Result := token^.VInteger;
  end;

begin
  xpos := FPosition;

  if FIsLsp then
    Result := (FChar <> #0) and (not FInLspCode or GetHeadChar) else
    Result := (FChar <> #0) and GetHeadChar;

  token^.Sym := syError;
  token^.Pos.fid := FFileID;
  token^.Pos.Row := FRow;
  token^.Pos.Col := FCol;
  token^.Val := '';
  token^.VInteger := 0;
  IsStr := false;

  if Result then
  begin
    xbeg := FPosition;
    if FIsLsp and not FInLspCode then seek_lsp_code else
    case FChar of
    '0'..'9': get_number;
    ''''    : get_char;
    '"'     : get_string(true);
    '`'     : get_shell(syShell);
    '+'     : get_operator(syAdd, ['+', '<'], [syAdd1, syAddAll]);
    '-'     : get_operator(syDec, ['-'], [syDec1]);
    '*'     : get_operator(syMul, [], []);
    '/'     : get_operator(syDiv, [], []);
    '%'     : get_operator(syMod, [], []);
    '^'     : get_operator(syBXor, [], []);
    '~'     : get_operator(syBNot, [], []);
    '('     : get_operator(syLParen, [], []);
    ')'     : get_operator(syRParen, [], []);
    '{'     : get_operator(syLBlock, ['$'], [syOption]);
    '}'     : get_operator(syRBlock, [], []);
    '['     : get_operator(syLArray, [], []);
    ']'     : get_operator(syRArray, [], []);
    '.'     : get_operator(syDot, ['.'], [syUpto]);
    '?'     : get_operator(syAsk, [], []);
    ':'     : if FIsLsp and FInLspCode then
              begin
                get_operator(syDot2, ['}', ':'], [syDotComma, syDot4]);
                if token^.Sym = syDotComma then
                begin
                  FInLspCode := false;
                  if Copy(FCode, FPosition, 3) = '-->' then
                    SkipChar(3);
                end
                else
                if token^.Sym = syDot4 then
                  get_pure_identity(false);
              end
              else 
              begin
                get_operator(syDot2, [':'], [syDot4]);
                if token^.Sym = syDot4 then
                  get_pure_identity(false);
              end;
    ','     : get_operator(syComma,  [], []);
    ';'     : get_operator(syDotComma, [], []);
    '='     : begin
                get_operator(syBecome, ['='], [syEQ]);
                if (token^.Sym = syEQ) and (FChar = '=') then
                begin
                  token^.Sym := syAbsEQ;
                  token^.Val := '===';
                  GetChar;
                end;
              end;
    '!'     : get_operator(syError, ['=', '<', '>'], [syNE, syGE, syLE]);
    '<'     : get_operator(syLT, ['=', '<', '>'], [syLE, syBShl, syNE]);
    '>'     : get_operator(syGT, ['=', '>'], [syGE, syBShr]);
    '&'     : get_operator(syBAnd, [], []);
    '|'     : get_operator(syBOr, [], []);
    '$'     : get_value;
    '@'     : get_operator(syFormat, [], []);
    else
      if __isIDHead(FChar) then
        get_identity
      else
        token^.Val := FChar;
    end;
    
    if Assigned(FOnGetToken) then
    begin
      if xbeg = FPosition then GetChar;
      FOnGetToken(Self, token, xpos, xbeg, FPosition);
    end;
  end
  else
  if not FEOF then
  begin
    FEOF := true;
    token^.Sym := syEOF;
    Result := true;
  end;
  
  if token^.Sym = syEndInc then
  begin
    org_fid := read_integer;
    org_row := read_integer;
    org_col := read_integer;
    Result := GetToken(token, IsStr);
    FFileID := org_fid;
    FRow := org_row;
    FCol := org_col;
  end;
end;

function KLiTokenizer.GetValueNameToken(token: PLiToken): boolean;
label ATTACH;
var
  chain: string;
begin
  Result := (FChar <> #0) and GetHeadChar and (FChar in IDChar);
  token^.Sym := syError;
  token^.Pos.fid := FFileID;
  token^.Pos.Row := FRow;
  token^.Pos.Col := FCol;
  token^.Val := FChar;
  token^.VInteger := 0;
  if Result then
  begin
    chain := FChar;
    ATTACH:
    while GetChar and (FChar in (IDChar + ['-', '>', ':'])) do
      chain := chain + FChar;
    if FChar = '.' then
      if GetChar and (FChar in IDChar) then
      begin
        chain := chain + '.' + FChar;
        goto ATTACH;
      end;
    token^.Val := chain;
    token^.Sym := syStr;
  end;
end;

function KLiTokenizer.GotoChar(Chars: KLiCharSet): boolean;
begin
  repeat Result := (FChar in Chars);
  until Result or not GetChar;
end;

function KLiTokenizer.HeadChar: char;
begin
  SkipSpaces;
  Result := FChar;
end;

function KLiTokenizer.SkipChar(Count: integer): boolean;
begin
  Result := (Count > 0);
  while Result and (Count > 0) do
  begin
    Result := GetChar;
    Dec(Count);
  end;
end;

function KLiTokenizer.Include(const FileName, TAG: string; FileID: integer): boolean;
const
  EndIncFmt: array[boolean] of string = ('{:%s %d %d %d:}', '{:%s %d %d %d;');
var
  source, tags: string;
  index, ipos, irow, icol: integer;
  curr: char;

  procedure get_char;
  var
    F: boolean;
  begin
    F := (curr = #13);
    if F or (curr = #10) then
    begin
      Inc(irow);
      icol := 0;
    end
    else Inc(icol);
    Inc(ipos);
    curr := source[ipos];
    if F and (curr = #10) then
    begin
      Inc(ipos);
      curr := source[ipos];
    end;
  end;

  function get_pos(const S: string): integer;
  var
    curr: pchar;
  begin
    curr := __pos(pchar(source), Length(source), pchar(S), Length(S), true);
    if curr <> nil then
      Result := (curr - pchar(source)) + 1 else
      Result := 0;
  end;

begin
  Result := false;
  
  // 1. get file source
  source := TrimRight(__fileText(FileName));
  if source = '' then Exit;

  // 2. cut tail
  index := get_pos(Format('<!--END-%s-INC-->', [TAG]));
  if index > 0 then
  begin
    source := TrimRight(Copy(source, 1, index - 1));
    if source = '' then Exit;
  end;

  // 3. skip head
  irow := 0;
  icol := 0;
  tags := Format('<!--BEG-%s-INC-->', [TAG]);
  index := get_pos(tags);
  if index > 0 then
  begin
    ipos := 1;
    curr := source[ipos];
    while ipos < index do get_char;
    Inc(icol, Length(tags));
    source := Copy(source, index + Length(tags), Length(source));
    if source = '' then Exit;
  end;

  // 4. adjust current
  if FIsLsp then
    tags := Format(EndIncFmt[FInLspCode], [FEndInc, FFileID, FRow, FCol]) else
    tags := Format('%s %d %d %d;', [FEndInc, FFileID, FRow, FCol]);
  FInLspCode := false;
  FCode := source + tags + Copy(FCode, FPosition, MaxInt);
  FCodeBase := pchar(FCode);
  FCodeSize := Length(FCode);
  FRow := irow;
  FCol := icol;
  FFileID := FileID;
  FPosition := 1;
  FChar := FCode[FPosition];
  FHeadPosition := FCodeSize + 1;
  Inc(FIncludeCount);
  Result := true;
end;

function KLiTokenizer.PeekChar: char;
begin
  if FPosition < FCodeSize then
    Result := FCode[FPosition + 1] else
    Result := #0;
end;

function KLiTokenizer.PeekNextKeywordToken: PLiToken;
begin
  Result := PeekNextToken;
  if Result <> nil then
    if not IsKeywordSymbol(Result^.Sym) then
      Result := nil;
end;

function KLiTokenizer.PeekNextThreeSym(var one, two, three: KLiSymbol): integer;
var
  A, B, C: PLiToken;
begin
  Result := PeekNextThreeTokens(A, B, C);
  if Result > 0 then
  begin
    one := A^.Sym;
    if Result > 1 then
    begin
      two := B^.Sym;
      if Result > 2 then
        three := C^.Sym;
    end;
  end;
end;

function KLiTokenizer.PeekNextThreeTokens(var one, two, three: PLiToken): integer;
begin
  one := nil;
  two := nil;
  three := nil;
  Result := max(0, PrepareTokens(4) - 1);
  if Result > 0 then
  begin
    one := FCurrent^.next;
    if Result > 1 then
    begin
      two := one^.next;
      if Result > 2 then
        three := two^.next;
    end;
  end;
end;

function KLiTokenizer.PeekNextToken: PLiToken;
begin
  if PrepareTokens(2) > 1 then
    Result := FCurrent^.next else
    Result := nil;
end;

function KLiTokenizer.PrepareTokens(Count: integer): integer;
var
  token: PLiToken;
  isstr: boolean;
begin
  Result := 0;
  Count := max(min(Count, Length(FTokenList) - 1), 0);
  if (Count > 0) and (GetCurrentToken <> nil) then
  begin
    Inc(Result);
    token := FCurrent;
    while (Result < Count) and (token^.next <> nil) do
    begin
      token := token^.next;
      Inc(Result);
    end;
    while (Result < Count) and (FUnused <> nil) and GetToken(FUnused, isstr) do
    begin
      token^.next := FUnused;
      token := FUnused;
      FUnused := FUnused^.next;
      token^.next := nil;
      Inc(Result);
    end;
  end;
end;

function KLiTokenizer.ReadTo(DestiChar: KLiCharSet; var S: string): boolean;
var
  P: integer;
begin
  Result := false;
  P := FPosition;
  while (FChar <> #0) and not (FChar in DestiChar) do GetChar;
  if FChar in DestiChar then
  begin
    S := Copy(FCode, P, FPosition - P);
    Result := true;
    GetChar;
  end;
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
  FHeadPosition := FCodeSize + 1;
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
    else
    begin
      FHeadPosition := FPosition;
      Result := true;
    end;
end;

function SimpleTest(const Script: string): integer;
var
  parser: KLiTokenizer;
  pairs: array of KLiSymbol;
  count, mask: integer;
  last, prev: KLiSymbol;
  token: PLiToken;
  isstr: boolean;

  procedure try_close_pair(begin_sym: KLiSymbols);
  begin
    if (count > 0) and (pairs[count - 1] in begin_sym) then
      Dec(count);
  end;

  procedure close_pair(begin_sym: KLiSymbols);
  begin
    if (count > 0) and (pairs[count - 1] in begin_sym) then Dec(count) else
    if begin_sym <> [syDotComma] then
      mask := SCT_ERROR;
  end;

  function peek_next_sym: KLiSymbol;
  var
    T: PLiToken;
  begin
    T := parser.PeekNextToken;
    if T <> nil then
      Result := T^.Sym else
      Result := syError;
  end;

  procedure begin_pair(begin_sym: KLiSymbol);
  begin
    if begin_sym in [syDefine] then
      try_close_pair([syDefine]);

    if begin_sym = syIf then
      if not (prev in [syDotComma, syEnd, syDot2, syDo, syThen,
        syRepeat, syTry, syExcept, syFinally, syElse]) then
          Exit;
        
    if begin_sym = syDo then  // const V = do ... end
      if prev = syBecome then
        begin_sym := syDefine else
        Exit;

    if begin_sym = syBOr then
      if not (prev in [syID, syRParen, syRArray, syBOr, syUpto] + ConstantSyms)
        and (peek_next_sym in [syMul, syAsk, syID, syBOr]) then
          begin_sym := syDefine else  // lambda | closure
          Exit;

    if count = Length(pairs) then
      SetLength(pairs, count + 1);
    pairs[count] := begin_sym;
    Inc(count);
  end;

  function get_next_token: PLiToken;
  begin
    repeat Result := parser.GetNextToken(isstr);
    until (Result <> nil) or not parser.GetChar;
  end;
  
begin
  last := syDotComma;
  prev := last;
  mask := SCT_OK;
  parser := KLiTokenizer.Create(Script, 1, false);
  try
    parser.FEOF := true;
    count := 0;
    token := get_next_token;
    while (token <> nil) and (mask = SCT_OK) do
    begin
      last := token^.Sym;
      case last of
        syLBlock, syLParen, syLArray, syClass, syFor, syWhile, syRepeat, syIf,
        sySwitch, syTry, syBOr, syDo, syOption,
        syDefine: begin_pair(last);
        syRBlock: close_pair([syLBlock, syOption]);
        syRParen: close_pair([syLParen]);
        syRArray: close_pair([syLArray]);
        syEnd   : close_pair([syFor, syWhile, syIf, sySwitch, syTry, syDefine, syClass]);
        syUntil : close_pair([syRepeat]);
        syDot2  : if prev = syThen then
                    close_pair([syIf]) else
                  if prev = syDo then
                    close_pair([syFor, syWhile]);
        syConst : try_close_pair([syDefine]);
        syImport: try_close_pair([syDefine]);
        syEOF   : try_close_pair([syDefine]);
        syError : if isstr then
                    mask := SCT_UNFINISHED;
      end;
      prev := last;
      token := get_next_token;
    end;
    if (mask = SCT_OK) and (count > 0) then
      mask := SCT_UNFINISHED;
    case last of
      syEnd     : Inc(mask, SCT_ENDBLOCK);
      syDotComma: Inc(mask, SCT_DOTCOMMA);
    end;
    Result := mask;
  finally
    parser.Free;
  end;
end;

function HeadIs__(const S: string): boolean;
begin
  Result := ('__' = Copy(S, 1, 2));
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
      token^.Val := '';
      lse_mem_free(token, sizeof(RLiToken));
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
  if index = FItems.Count then
    Result := Next else
    Result := FItems[index];
end;

function KLiTokens.Next: PLiToken;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiToken));
  FItems.Add(Result);
end;

end.
