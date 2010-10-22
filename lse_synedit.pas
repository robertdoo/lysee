{==============================================================================}
{        UNIT: lse_synedit                                                     }
{ DESCRIPTION: syntax highlighter of lysee script                              }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2010/10/21                                                      }
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
  {$IFDEF LYSEE_LAZ}LResources,{$ENDIF}
  SynEditTypes, SynEditHighlighter;

const
  SYNS_FILTERLANGUAGE = 'Lysee (*.ls,*.lsp)|*.ls;*.lsp';
  SYNS_LYSEELANGUAGE = 'Lysee';

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkString,
                  tkChar, tkNumber, tkHTML, tkOption, tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsRawStr, rsTriple,
                 rsHTML, rsOption);

  { TLyseeSyn }

  TLyseeSyn = class(TSynCustomHighlighter)
  private
    FLineString: string;
    FLine: pchar;
    FLineNumber: integer;
    FRange: TRangeState;
    FRun: longint;
    FTokenPos: integer;
    FTokenID: TtkTokenKind;
    FIsLsp: boolean;
    FAttrComment: TSynHighlighterAttributes;
    FAttrID: TSynHighlighterAttributes;
    FAttrKeyword: TSynHighlighterAttributes;
    FAttrSpace: TSynHighlighterAttributes;
    FAttrString: TSynHighlighterAttributes;
    FAttrOption: TSynHighlighterAttributes;
    FAttrNumber: TSynHighlighterAttributes;
    FAttrChar: TSynHighlighterAttributes;
    FAttrHTML: TSynHighlighterAttributes;
    FAttrUnknown: TSynHighlighterAttributes;
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
    {$IFNDEF SYN_CPPB_1}class{$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: pchar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
  published
    property AttrComment: TSynHighlighterAttributes read FAttrComment write FAttrComment;
    property AttrID: TSynHighlighterAttributes read FAttrID write FAttrID;
    property AttrKeyword: TSynHighlighterAttributes read FAttrKeyword write FAttrKeyword;
    property AttrSpace: TSynHighlighterAttributes read FAttrSpace write FAttrSpace;
    property AttrString: TSynHighlighterAttributes read FAttrString write FAttrString;
    property AttrOption: TSynHighlighterAttributes read FAttrOption write FAttrOption;
    property AttrChar: TSynHighlighterAttributes read FAttrChar write FAttrChar;
    property AttrNumber: TSynHighlighterAttributes read FAttrNumber write FAttrNumber;
    property AttrHTML: TSynHighlighterAttributes read FAttrHTML write FAttrHTML;
    property AttrUnknown: TSynHighlighterAttributes read FAttrUnknown write FAttrUnknown;
    property IsLsp: boolean read FIsLsp write SetIsLsp default false;
  end;

function GetLyseeKeywords: string;
function SetLyseeKeywords(const Keywords: string): string;
function DealAsKeyword(const S: string): boolean;

procedure Register;

implementation

uses
  Clipbrd, SynEditStrConst, lse_funcs;

procedure Register;
begin
  RegisterComponents('Lysee', [TLyseeSyn]);
end;

var
  LyseeKeywords: string =
    'and,as,bool,break,case,char,class,const,continue,def,do,' +
    'elif,else,elseif,end,except,false,finally,float,for,if,' +
    'import,in,include,int,is,like,main,nil,not,object,or,' +
    'recall,repeat,return,string,super,switch,sys,then,this,' +
    'time,true,try,type,until,variant,void,while';

function GetLyseeKeywords: string;
begin
  Result := Copy(LyseeKeywords, 2, Length(LyseeKeywords) - 2);
end;

function SetLyseeKeywords(const Keywords: string): string;
begin
  Result := GetLyseeKeywords;
  LyseeKeywords := ',' + Trim(Keywords) + ',';
end;

function DealAsKeyword(const S: string): boolean;
begin
  Result := (Copy(S, 1, 2) = '__') or
            (Pos(',' + S + ',', LyseeKeywords) > 0);
end;

{ TLyseeSyn}

procedure TLyseeSyn.ParseSpace;
begin
  FTokenID := tkSpace;
  repeat inc(FRun);
  until not (FLine[FRun] in [#1..#32]);
end;

procedure TLyseeSyn.ParseNull;
begin
  FTokenID := tkNull;
end;

procedure TLyseeSyn.ParseEnter;
begin
  FTokenID := tkSpace;
  inc(FRun);
  if FLine[FRun] = #10 then
    inc(FRun);
end;

procedure TLyseeSyn.ParseLineFeed;
begin
  FTokenID := tkSpace;
  inc(FRun);
end;

procedure TLyseeSyn.ParseCommentStart;

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

procedure TLyseeSyn.ParseComment;
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

procedure TLyseeSyn.ParseStringStart(IsRawStr: boolean);
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

procedure TLyseeSyn.ParseString;
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

constructor TLyseeSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAttrComment := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  FAttrComment.Foreground := clOlive;
  AddAttribute(FAttrComment);

  FAttrID := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(FAttrID);

  FAttrKeyword := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  FAttrKeyword.Style := [fsBold];
  AddAttribute(FAttrKeyword);

  FAttrSpace := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FAttrSpace);

  FAttrString := TSynHighLighterAttributes.Create(SYNS_AttrString);
  FAttrString.Foreground := clNavy;
  AddAttribute(FAttrString);

  FAttrOption := FAttrComment;
  FAttrOption := TSynHighLighterAttributes.Create(SYNS_AttrDirective);
  FAttrOption.Foreground := clGreen;
  AddAttribute(FAttrOption);

  FAttrNumber := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  FAttrNumber.Foreground := $004080FF;
  AddAttribute(FAttrNumber);

  FAttrChar := TSynHighLighterAttributes.Create(SYNS_AttrCharacter);
  FAttrChar.Foreground := clNavy;
  AddAttribute(FAttrChar);

  FAttrUnknown := TSynHighLighterAttributes.Create(SYNS_AttrUnknownWord);
  AddAttribute(FAttrUnknown);

  FAttrHTML := TSynHighLighterAttributes.Create(SYNS_AttrText);
  FAttrHTML.Foreground := clGray;
  AddAttribute(FAttrHTML);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  fDefaultFilter := SYNS_FILTERLANGUAGE;
  FRange := rsUnknown;
  FIsLsp := false;
end;

procedure TLyseeSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: integer);
begin
  FLineString := NewValue;
  FLine := pchar(FLineString);
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

procedure TLyseeSyn.ParseIdentity;
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

procedure TLyseeSyn.ParseUnknown;
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

procedure TLyseeSyn.Next;
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

function TLyseeSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := FAttrComment;
    SYN_ATTR_IDENTIFIER : Result := FAttrID;
    SYN_ATTR_KEYWORD    : Result := FAttrKeyword;
    SYN_ATTR_STRING     : Result := FAttrString;
    SYN_ATTR_WHITESPACE : Result := FAttrSpace;
    else                  Result := FAttrSpace;
  end;
end;

function TLyseeSyn.GetEol: boolean;
begin
  Result := (FTokenID = tkNull)
end;

function TLyseeSyn.GetKeyWords: string;
begin
  Result := GetLyseeKeywords;
end;

function TLyseeSyn.GetToken: string;
begin
  SetString(Result, (FLine + FTokenPos), FRun - FTokenPos);
end;

procedure TLyseeSyn.GetTokenEx(out TokenStart: pchar; out TokenLength: integer);
begin
  TokenLength := FRun - FTokenPos;
  if TokenLength > 0 then
    TokenStart := FLine + FTokenPos else
    TokenStart := nil;
end;

function TLyseeSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TLyseeSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FAttrComment;
    tkIdentifier: Result := FAttrID;
    tkKey: Result := FAttrKeyword;
    tkSpace: Result := FAttrSpace;
    tkString: Result := FAttrString;
    tkOption: Result := FAttrOption;
    tkChar: Result := FAttrChar;
    tkNumber: Result := FAttrNumber;
    tkHTML: Result := FAttrHTML;
    tkUnknown: Result := FAttrUnknown;
    else Result := FAttrUnknown;
  end;
end;

function TLyseeSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TLyseeSyn.GetTokenPos: integer;
begin
  Result := FTokenPos;
end;

function TLyseeSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z', '0'..'9'];
end;

function TLyseeSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FILTERLANGUAGE;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TLyseeSyn.GetLanguageName: string;
begin
  Result := SYNS_LYSEELANGUAGE;
end;

procedure TLyseeSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TLyseeSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TLyseeSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TLyseeSyn.ParseChar;
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

procedure TLyseeSyn.ParseNumber;
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

procedure TLyseeSyn.ParseOption;
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

procedure TLyseeSyn.ParseOptionStart;
begin
  Inc(FRun);
  FTokenID := tkOption;
  FRange := rsOption;
  FOptionCount := 1;
end;

procedure TLyseeSyn.ParseHTML;
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

function TLyseeSyn.OnTag(const TagStr: string): boolean;
var
  index, range: integer;
begin
  Result := false;
  range := Length(TagStr) - 1;
  for index := 0 to range do
    if FLine[FRun + index] <> TagStr[1 + index] then
      Exit;
  Result := true;
end;

procedure TLyseeSyn.SetIsLsp(const Value: boolean);
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

initialization
{$IFDEF LYSEE_LAZ}
{$I lse_synedit.lrs}
{$ENDIF}

end.
