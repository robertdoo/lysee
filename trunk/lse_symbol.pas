{==============================================================================}
{        UNIT: lse_symbol                                                      }
{ DESCRIPTION: symbolization of lysee script parser                            }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/26                                                      }
{    MODIFIED: 2011/07/09                                                      }
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
    syError, syBegin, syImport, syConst, sySyntax, syLambda, syClass, syThis,
    syObject, syWith, syLet, sySet, syGet, syHas, syDefine, syReturn, syIf, syThen, syElif, syElse, syWhile,
    syRepeat, syUntil, syFor, syDo, syBreak, syContinue, sySwitch, syCase,
    syTry, syExcept, syFinally, syIn, syIs, syAs, syLike, syAnd, syOr, syNot,
    syFrom, syTo, syEnd, syBecome, syAdd, syDec, syMul, syDiv, syMod, syBNot,
    syBXor, syBAnd, syBOr, syBShr, syBShl, syFill, syLParen, syRParen, syLBlock,
    syRBlock, syLArray, syRArray, syDot, syAsk, syDot2, syComma, syEQ, syNE,
    syLess, syLE, syMore, syME, syFormat, syID, syFloat, syInt, syStr, syNil,
    syNeg, syCall, syJmpT, syJmpF, syJmpTP, syJmpFP, syJump, syPress, syType,
    syFunc, syIdle, syGetEnv, syLabel, syGoto, syGoTP, syGoFP, syModule, sySTMT,
    syVarGen, syRINR, syGETV, sySETV, syGetIV, sySetIV, syDupLast, syEOF,
    syEcho, syGetSV, sySetSV, sySend, syVarList, syHashed
  );
  KLiSymbols = set of KLiSymbol;

const
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
    (SY:syCall;      ID:'<CALL>';       SM:'call function'),
    (SY:syJmpT;      ID:'<JMPT>';       SM:'jump true'),
    (SY:syJmpF;      ID:'<JMPF>';       SM:'jump false'),
    (SY:syJmpTP;     ID:'<JMPTP>';      SM:'jump true pop last'),
    (SY:syJmpFP;     ID:'<JMPFP>';      SM:'jump flase pop last'),
    (SY:syJump;      ID:'<JUMP>';       SM:'jump'),
    (SY:syPress;     ID:'<PRESS>';      SM:'pop'),
    (SY:syType;      ID:'<TYPE>';       SM:'push class'),
    (SY:syFunc;      ID:'<FUNC>';       SM:'push function'),
    (SY:syIdle;      ID:'<IDLE>';       SM:'idle'),
    (SY:syGetEnv;    ID:'<GETENV>';     SM:'push enviromnent value'),
    (SY:syLabel;     ID:'<LABEL>';      SM:'label'),
    (SY:syGoto;      ID:'<GOTO>';       SM:'goto label'),
    (SY:syGoTP;      ID:'<GOTOTP>';     SM:'goto when true and pop'),
    (SY:syGoFP;      ID:'<GOTOFP>';     SM:'goto when false and pop'),
    (SY:syModule;    ID:'<MODULE>';     SM:'push module'),
    (SY:sySTMT;      ID:'<STMT>';       SM:'end statement'),
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
    (SY:syHashed;    ID:'<HASHED>';     SM:'create hashed')
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
  
type
  KLiSymPos = packed record
    row, col: word;
    module: pointer; {<--KLiModule}
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
    4: (VPureID: boolean);     {<--syID has no '::'}
    5: (VType: pointer);       {<--syIs, syAs}
  end;

  { KLiTokenizer }

  KLiTokenizer = class(KLiObject)
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
( F_NAME: StrToSym
( 
( F_DESC: convert string to symbol
( 
( F_ARGS: const S: string
(         DefSymbol: KLiSymbol
( 
( F_TYPE: KLiSymbol
( 
( EXCEPT:
(----------------------------------------------------------------------}
function StrToSym(const S: string; DefSymbol: KLiSymbol): KLiSymbol;

{-----------------------------------------------------------------------
( F_NAME: SymToStr
( 
( F_DESC: const symbol to string
( 
( F_ARGS: Sym: KLiSymbol
(
( F_TYPE: string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function SymToStr(Sym: KLiSymbol): string;

{-----------------------------------------------------------------------
( F_NAME: SymsToStrList
( 
( F_DESC: convert symbols to string
(
( F_ARGS: Syms: KLiSymbols
(
( F_TYPE: string
( 
( EXCEPT:
(----------------------------------------------------------------------}
function SymsToStrList(syms: KLiSymbols): string;

{-----------------------------------------------------------------------
( F_NAME: IsKeyword
( 
( F_DESC: is keyword string?
(
( F_ARGS: const ID: string
( 
( F_TYPE: boolean
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
( F_DESC: is pure identity?(not contains '::')
( 
( F_ARGS: token: PLiToken
( 
( F_TYPE: boolean
( 
( EXCEPT:
(----------------------------------------------------------------------}
function IsPureID(token: PLiToken): boolean;

{-----------------------------------------------------------------------
( F_NAME: SimpleTest
( 
( F_DESC: test script before run
(
( F_ARGS: const Script: string
(
( F_TYPE: integer - lseu.SCT_XXXX
(
( EXCEPT:
(----------------------------------------------------------------------}
function SimpleTest(const Script: string): integer;

{======================================================================)
(======== tokens ======================================================)
(======================================================================)
( clone_token: clone a token
( new_token  : create a empty token
( free_token : release a token
(----------------------------------------------------------------------}
function  clone_token(Token: PLiToken): PLiToken;
function  new_token: PLiToken;
procedure free_token(Token: PLiToken);
procedure copy_token(SrcToken, DstToken: PLiToken);

var
  reserved_words: string = '';

implementation

uses
  math, lse_kernel, lseu;

function StrToSym(const S: string; DefSymbol: KLiSymbol): KLiSymbol;
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
  Result := DefSymbol;
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
  Result := (sym in [FirstKeyword..LastKeyword]);
end;

function ReservedWords: string;
var
  sym: KLiSymbol;
  list: TStringList;
begin
  if reserved_words = '' then
  begin
    list := __newNamedList(false);
    try
      list.CommaText := 'this,void,int,float,string,object,variant,sys,class,main';
      for sym := FirstKeyword to LastKeyword do
        if list.IndexOf(Symbols[sym].ID) < 0 then
          list.Add(Symbols[sym].ID);
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
    FTokenList[X - 1].next := @FTokenList[X];
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
    FTokenList[X].Val := '';
  while FNextToken <> nil do
  begin
    T := FNextToken^.next;
    free_token(FNextToken);
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
      copy_token(token, FUnused);
      FNextToken := token^.next;
      free_token(token);
      done := true;
    end
    else done := GetToken(FUnused, IsStr);
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
  else
  if FNextToken <> nil then
  begin
    token := FNextToken;
    copy_token(token, FCurrent);
    FNextToken := token^.next;
    free_token(token);
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

  function next_is_str: boolean;
  begin
    Result := SkipSpaces and (FChar in QuoteChar);
  end;

  procedure get_string(allow_esc_char: boolean);
  begin
    IsStr := true;
    while true do
    begin
      FBuffer.Clear;
      base := FBase + FPosition - 1;
      next := base;
      if __parseStr(next, FBuffer, allow_esc_char) then
      begin
        token^.Val := token^.Val + __newString(FBuffer.Memory, FBuffer.Size);
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
    if SkipSpaces and __isIDHead(FChar) then
    begin
      idstr := FChar;
      while GetChar and (FChar in IDChar) do
        idstr := idstr + FChar;
      if pure_id then
      begin
        token^.Val := idstr;
        token^.Sym := StrToSym(idstr, syID);
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
  var
    vs: string;
  begin
    if GetChar then
      if FChar in IDHeadChar then
      begin
        vs := FChar;
        while GetChar and (FChar in IDChar) do
          vs := vs + FChar;
        token^.Val := vs;
        token^.Sym := syGetSV;
      end
      else
      if (FChar = '{') and GetChar and (FChar <> '}') then
      begin
        vs := FChar;
        while GetChar and (FChar <> '}') do
          vs := vs + FChar;
        if FChar = '}' then
        begin
          token^.Val := vs;
          token^.Sym := syGetEnv;
          GetChar;
        end;
      end;
  end;

begin
  Result := (FChar <> #0) and SkipSpaces;

  token^.Sym := syError;
  token^.Pos.Row := FRow;
  token^.Pos.Col := FCol;
  token^.Val := '';
  token^.VInteger := 0;
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
    '='     : get_operator(syBecome, ['='], [syEQ]);
    '!'     : get_operator(syError, ['=', '<', '>'], [syNE, syME, syLE]);
    '<'     : begin
                get_operator(syLess, ['=', '<'], [syLE, syBShl]);
                if (token^.Sym = syBShl) and (FChar = '<') then
                begin
                  token^.Sym := syFill;
                  token^.Val := '<<<';
                  GetChar;
                end;
              end;
    '>'     : get_operator(syMore, ['=', '>'], [syME, syBShr]);
    '&'     : get_operator(syBAnd, [], []);
    '|'     : get_operator(syBOr, [], []);
    '$'     : get_value;
    '@'     : get_operator(syFormat, [], []);
    else
      if __isIDHead(FChar) then
        get_pure_identity(true) else
        token^.Val := FChar;
    end;
  end
  else
  if not FEOF then
  begin
    FEOF := true;
    token^.Sym := syEOF;
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
  token, last: PLiToken;
  isstr: boolean;
begin
  Result := 0;
  Count := max(min(Count, Length(FTokenList) - 1), 0);
  if (Count > 0) and (GetCurrentToken <> nil) then
  begin
    Inc(Result);
    last := FCurrent;
    while (Result < Count) and (last^.next <> nil) do
    begin
      last := last^.next;
      Inc(Result);
    end;
    while (Result < Count) and (FUnused <> nil)  do
    begin
      if FNextToken <> nil then
      begin
        token := FNextToken;
        copy_token(token, FUnused);
        FNextToken := token^.next;
        free_token(token);
      end
      else if not GetToken(FUnused, IsStr) then Exit;
      last^.next := FUnused;
      last := FUnused;
      FUnused := FUnused^.next;
      last^.next := nil;
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
  Result := clone_token(Token);
  Result^.next := FNextToken;
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

function SimpleTest(const Script: string): integer;
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
  parser := KLiTokenizer.Create(Script);
  try
    parser.FEOF := true; // hide syEOF
    last := syError;
    mask := SCT_OK;
    count := 0;
    token := get_next_token;
    while (token <> nil) and (mask = SCT_OK) do
    begin
      last := token^.Sym;
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
end;

function clone_token(Token: PLiToken): PLiToken;
begin
  Result := lse_mem_alloc_zero(sizeof(RLiToken));
  if Token <> nil then
    copy_token(Token, Result);
end;

function new_token: PLiToken;
begin
  Result := clone_token(nil);
end;

procedure free_token(Token: PLiToken);
begin
  Token^.Val := '';
  lse_mem_free(Token, sizeof(RLiToken));
end;

procedure copy_token(SrcToken, DstToken: PLiToken);
var
  name: string;
  next: PLiToken;
begin
  next := DstToken^.next;
  name := SrcToken^.Val;
  SrcToken^.Val := '';
  DstToken^.Val := '';
  Move(SrcToken^, DstToken^, sizeof(RLiToken));
  SrcToken^.Val := name;
  DstToken^.Val := name;
  DstToken^.next := next;
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
      free_token(token);
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
  Result := clone_token(nil);
  FItems.Add(Result);
end;

end.
