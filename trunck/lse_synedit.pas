{==============================================================================}
{        UNIT: lse_synedit                                                     }
{ DESCRIPTION: syntax highlighter of lysee script                              }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2008-2010, Li Yun Jie                                          }
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
unit lse_synedit;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, SynEdit, SynMemo;

const
  SYNS_FILTERLANGUAGE = 'Lysee (*.ls,*.lsp)|*.ls;*.lsp';
  SYNS_LANGUAGE = 'Lysee Script';

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkString,
                  tkChar, tkNumber, tkHTML, tkOption, tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsRawStr, rsTriple,
                 rsHTML, rsOption);

  { TSynLysee }

  TSynLysee = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FLineNumber: Integer;
    FRange: TRangeState;
    FRun: LongInt;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FIsLsp: boolean;
    FIgnoreTagCase: boolean;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FOptionAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    FOutTagAttri: TSynHighlighterAttributes;
    FOptionCount: integer;
    procedure ParseCommentStart;
    procedure ParseComment;
    procedure ParseHTML;
    procedure ParseIdentity;
    procedure ParseStringStart(IsRawStr: boolean);
    procedure ParseString;
    procedure ParseOptionStart;
    procedure ParseOption;
    procedure ParseChar;
    procedure ParseNumber;
    procedure ParseSpace;
    procedure ParseLineFeed;
    procedure ParseEnter;
    procedure ParseNull;
    procedure ParseUnknown;
    procedure SetIsLsp(const Value: boolean);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
    function OnTag(const TagStr: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1}
    class
    {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
   {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar;
                         out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property OptionAttri: TSynHighlighterAttributes read FOptionAttri write FOptionAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri write FCharAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
    property IsLsp: boolean read FIsLsp write SetIsLsp;
    property IgnoreTagCase: boolean read FIgnoreTagCase write FIgnoreTagCase;
  end;

  TSynLyseeMemo = class(TSynMemo)
  private
    FSynLyseeHilighter: TSynLysee;
    function GetIsLsp: boolean;
    procedure SetIsLsp(const Value: boolean);
  protected
    procedure ResetHighlighter;
  public
    constructor Create(AOwner: TComponent);override;
    property Highlighter;
  published
    property IsLsp: boolean read GetIsLsp write SetIsLsp;
  end;

var
  Keywords: string =
    '__clen__,and,as,bool,break,case,char,class,const,continue,def,do,else,' +
    'elseif,end,except,false,finally,float,for,if,import,in,include,' +
    'int,is,like,main,nil,not,object,or,recall,repeat,return,string,' +
    'super,switch,sys,then,this,time,true,try,type,until,variant,void,while';

function DealAsKeyword(const S: string): boolean;

implementation

uses
  Clipbrd, SynEditStrConst, lse_funcs;

function DealAsKeyword(const S: string): boolean;
begin
  Result := (Copy(S, 1, 2) = '__') or
            (Pos(',' + S + ',', ',' + Keywords + ',') > 0);
end;

{ TSynLysee}

procedure TSynLysee.ParseSpace;
begin
  FTokenID := tkSpace;
  repeat inc(FRun);
  until not (FLine[FRun] in [#1..#32]);
end;

procedure TSynLysee.ParseNull;
begin
  FTokenID := tkNull;
end;

procedure TSynLysee.ParseEnter;
begin
  FTokenID := tkSpace;
  inc(FRun);
  if FLine[FRun] = #10 then
    inc(FRun);
end;

procedure TSynLysee.ParseLineFeed;
begin
  FTokenID := tkSpace;
  inc(FRun);
end;

procedure TSynLysee.ParseCommentStart;

  procedure skip_line_comment;
  begin
    FRange := rsUnknown;
    repeat Inc(FRun) until FLine[FRun] in [#0, #10, #13];
    FTokenID := tkComment;
  end;

begin
  { /* # // */ }
  if (FLine[FRun] = '/') and (FLine[FRun + 1] in ['/', '*']) then
  begin
    Inc(FRun);
    if (FLine[FRun] = '*') then
    begin
      Inc(FRun);
      FRange := rsComment;
      FTokenID := tkComment;
    end
    else skip_line_comment;
  end
  else
  if FLine[FRun] = '#' then
    skip_line_comment else
    ParseUnknown;
end;

procedure TSynLysee.ParseComment;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else repeat
           if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
           begin
             Inc(FRun, 2);
             FRange := rsUnKnown;
             Exit;
           end;
           if not (FLine[FRun] in [#0, #10, #13]) then
             Inc(FRun);
         until FLine[FRun] in [#0, #10, #13];
  end;
end;

procedure TSynLysee.ParseStringStart(IsRawStr: boolean);
var
  times: integer;
begin
  FTokenID := tkString;
  Inc(FRun);
  times := 1;
  while (FLine[FRun] = '"') and (times < 3) do
  begin
    Inc(FRun);
    Inc(times);
  end;
  if times <> 2 then
    if times = 3 then
      FRange := rsTriple else
    if IsRawStr then
      FRange := rsRawStr else
      FRange := rsString;
end;

procedure TSynLysee.ParseString;
var
  count, quotes, thick: integer;
begin
  FTokenID := tkString;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else repeat
           if (FRange = rsString) and (FLine[FRun] = '\') then Inc(FRun) else
           if (FLine[FRun] = '"') then
           begin
             count := Length(FLine);
             quotes := 0;
             if FRange = rsTriple then
               thick := 3 else
               thick := 1;
             while (quotes < thick) and (FRun <= count) and (FLine[FRun]= '"') do
             begin
               Inc(quotes);
               Inc(FRun);
             end;
             if quotes = thick then
             begin
               if FRange = rsTriple then
                 while (FRun <= count) and (FLine[FRun]= '"') do
                   Inc(FRun);
               FRange := rsUnKnown;
               Exit;
             end;
           end;
           if not (FLine[FRun] in [#0, #10, #13]) then
             Inc(FRun);
         until FLine[FRun] in [#0, #10, #13];
  end;
end;

constructor TSynLysee.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Foreground := clOlive;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  FStringAttri.Foreground := clNavy;
  AddAttribute(FStringAttri);

  FOptionAttri := FCommentAttri;
  FOptionAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirective);
  FOptionAttri.Foreground := clGreen;
  AddAttribute(FOptionAttri);

  FNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  FNumberAttri.Foreground := $004080FF;
  AddAttribute(FNumberAttri);

  FCharAttri := TSynHighLighterAttributes.Create(SYNS_AttrCharacter);
  FCharAttri.Foreground := clNavy;
  AddAttribute(FCharAttri);

  FUnknownAttri := TSynHighLighterAttributes.Create(SYNS_AttrUnknownWord);
  AddAttribute(FUnknownAttri);

  FOutTagAttri := TSynHighLighterAttributes.Create(SYNS_AttrText);
  FOutTagAttri.Foreground := clGray;
  AddAttribute(FOutTagAttri);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  fDefaultFilter := SYNS_FILTERLANGUAGE;
  FRange := rsUnknown;

  FIsLsp := false;
  FIgnoreTagCase := true;
end;

procedure TSynLysee.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer);
begin
  FLine := PChar(NewValue);
  FRun := 0;
  FLineNumber := LineNumber;
  if FLineNumber = 0 then
    if FIsLsp then
    begin
      FTokenID := tkHTML;
      FRange := rsHTML;
    end
    else
    begin
      FTokenID := tkUnknown;
      FRange := rsUnknown;
    end;
  Next;
end;

procedure TSynLysee.ParseIdentity;
var
  base: integer;
  temp: string;
begin
  if (FLine[FRun] = 'R') and (FLine[FRun + 1] = '"') then
  begin
    Inc(FRun);
    ParseStringStart(true);
  end
  else
  begin
    base := FRun;
    while FLine[FRun] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do Inc(FRun);
    SetString(temp, FLine + base, FRun - base);
    if DealAsKeyword(temp) then
      FTokenID := tkKey else
      FTokenID := tkIdentifier;
  end;
end;

procedure TSynLysee.ParseUnknown;
begin
  if (FLine[FRun] = '{') and (FLine[FRun + 1] = '$') then
  begin
    Inc(FRun);
    ParseOptionStart;
  end
  else
  begin
    {$IFDEF SYN_MBCSSUPPORT}
    if FLine[FRun] in LeadBytes then Inc(FRun,2) else
    {$ENDIF}
    inc(FRun);
    FTokenID := tkUnknown;
  end;
end;

procedure TSynLysee.Next;
begin
  FTokenPos := FRun;
  if FRange = rsHTML then ParseHTML else
  if FRange = rsComment then ParseComment else
  if FRange = rsOption then ParseOption else
  if FRange in [rsString, rsRawStr, rsTriple] then ParseString else
  if FIsLsp and OnTag(':}') then
  begin
    Inc(FRun, 2);
    FTokenID := tkHTML;
    FRange := rsHTML;
  end
  else
  begin
    FRange := rsUnknown;
    case FLine[FRun] of
      #0: ParseNull;
      #10: ParseLineFeed;
      #13: ParseEnter;
      '#', '/': ParseCommentStart;
      '"': ParseStringStart(false);
      '''': ParseChar;
      '0'..'9': ParseNumber;
      #1..#9, #11, #12, #14..' ': ParseSpace;
      'A'..'Z', 'a'..'z', '_': ParseIdentity;
      else ParseUnknown;
    end;
  end;
end;

function TSynLysee.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := FKeyAttri;
    SYN_ATTR_STRING     : Result := FStringAttri;
    SYN_ATTR_WHITESPACE : Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynLysee.GetEol: boolean;
begin
  Result := (FTokenID = tkNull);
end;

function TSynLysee.GetKeyWords: string;
begin
  Result := Keywords;
end;

function TSynLysee.GetToken: String;
var
  Len: LongInt;
begin
  Len := FRun - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynLysee.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := FRun - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;
{$ENDIF}

function TSynLysee.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynLysee.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkOption: Result := FOptionAttri;
    tkChar: Result := FCharAttri;
    tkNumber: Result := FNumberAttri;
    tkHTML: Result := FOutTagAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynLysee.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynLysee.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TSynLysee.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z', '0'..'9'];
end;

function TSynLysee.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FILTERLANGUAGE;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynLysee.GetLanguageName: string;
begin
  Result := SYNS_LANGUAGE;
end;

procedure TSynLysee.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynLysee.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynLysee.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynLysee.ParseChar;
begin
  FTokenID := tkChar;
  Inc(FRun);
  repeat
    if FLine[FRun] = '\' then
    begin
      Inc(FRun);
      if FLine[FRun] = 'x' then
      begin
        Inc(FRun);
        if FLine[FRun] in ['0'..'9', 'A'..'F', 'a'..'f'] then Inc(FRun);
        if FLine[FRun] in ['0'..'9', 'A'..'F', 'a'..'f'] then Inc(FRun);
      end;
    end
    else
    if FLine[FRun] = '''' then
    begin
      Inc(FRun, 1);
      Break;
    end;
    if not (FLine[FRun] in [#0, #10, #13]) then
      Inc(FRun);
  until FLine[FRun] in [#0, #10, #13];
  FRange := rsUnKnown;
end;

procedure TSynLysee.ParseNumber;
var
  cset: set of char;
  h: char;
begin
  FTokenID := tkNumber;
  h := FLine[FRun];
  Inc(FRun);
  if (h = '0') and (FLine[FRun] in ['x', 'X']) then
  begin
    cset := ['0'..'9', 'a'..'f', 'A'..'F'];
    Inc(FRun);
  end
  else cset := ['0'..'9'];
  while FLine[FRun] in cset do Inc(FRun);
  FRange := rsUnKnown;
end;

procedure TSynLysee.ParseOption;
begin
  FTokenID := tkOption;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else repeat
           case FLine[FRun] of
             '{': Inc(FOptionCount);
             '}': begin
                    Dec(FOptionCount);
                    if FOptionCount = 0 then
                    begin
                      FRange := rsUnKnown;
                      Inc(FRun);
                      Exit;
                    end;
                  end;
           end;
           if not (FLine[FRun] in [#0, #10, #13]) then Inc(FRun);
         until FLine[FRun] in [#0, #10, #13];
  end;
end;

procedure TSynLysee.ParseOptionStart;
begin
  Inc(FRun);
  FTokenID := tkOption;
  FRange := rsOption;
  FOptionCount := 1;
end;

procedure TSynLysee.ParseHTML;
begin
  FTokenID := tkHTML;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else repeat
           if OnTag('{:') then
           begin
             Inc(FRun, 2);
             FRange := rsUnKnown;
             Exit;
           end;
           if not (FLine[FRun] in [#0, #10, #13]) then
             Inc(FRun);
         until FLine[FRun] in [#0, #10, #13];
  end;
end;

function TSynLysee.OnTag(const TagStr: string): boolean;
var
  index, range: integer;

  function SameChar(c1, c2: char): boolean;
  begin
    if FIgnoreTagCase then
      Result := (__LoCase(c1) = __LoCase(c2)) else
      Result := (c1 = c2);
  end;

begin
  range := Length(TagStr) - 1;
  for index := 0 to range do
  begin
    Result := SameChar(FLine[FRun + index], TagStr[1 + index]);
    if not Result then Exit;
  end;
  Result := true;
end;

procedure TSynLysee.SetIsLsp(const Value: boolean);
begin
  FIsLsp := Value;
  if FIsLsp then
  begin
    FRange := rsHTML;
    FTokenID := tkHTML;
  end
  else
  begin
    if FRange = rsHTML then
      FRange := rsUnknown;
    if FTokenID = tkHTML then
      FTokenID := tkUnknown;
  end;
end;

{ TSynLyseeMemo }

constructor TSynLyseeMemo.Create(AOwner: TComponent);
begin
  inherited;
  FSynLyseeHilighter := TSynLysee.Create(Self);
  Highlighter := FSynLyseeHilighter;  
end;

function TSynLyseeMemo.GetIsLsp: boolean;
begin
  Result := FSynLyseeHilighter.IsLsp;
end;

procedure TSynLyseeMemo.ResetHighlighter;
var
  current: TSynCustomHighlighter;
begin
  current := Highlighter;
  Highlighter := nil;
  Highlighter := current;
end;

procedure TSynLyseeMemo.SetIsLsp(const Value: boolean);
begin
  if Value <> IsLsp then
  begin
    FSynLyseeHilighter.IsLsp := Value;
    ResetHighlighter;
  end;
end;

end.
