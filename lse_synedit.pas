{==============================================================================}
{        UNIT: lse_synedit                                                     }
{ DESCRIPTION: syntax highlighter for lysee language                           }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2012/01/14                                                      }
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
  SYNS_FILTERLANGUAGE = 'Lysee *.ls|*.ls';
  SYNS_LYSEELANGUAGE = 'Lysee';

type
  TTokenID = (tkUnknown, tkComment, tkID, tkCurr, tkKey, tkNull, tkSpace, tkParen,
              tkString, tkNumber);
  TRangeID = (rkUnknown, rkComment, rkString, rkRawStr);

  { TLyseeSyn }

  TLyseeSyn = class(TSynCustomHighlighter)
  private
    FLineString: string;
    FLine: pchar;
    FLineNumber: integer;
    FRun: longint;
    FTokenPos: integer;
    FTokenID: TTokenID;
    FRangeID: TRangeID;
    FAttrComment: TSynHighlighterAttributes;
    FAttrID: TSynHighlighterAttributes;
    FAttrCurr: TSynHighlighterAttributes;
    FAttrKeyword: TSynHighlighterAttributes;
    FAttrSpace: TSynHighlighterAttributes;
    FAttrString: TSynHighlighterAttributes;
    FAttrNumber: TSynHighlighterAttributes;
    FAttrParen: TSynHighlighterAttributes;
    FAttrUnknown: TSynHighlighterAttributes;
    procedure ParseCommentStart;
    procedure ParseComment;
    procedure ParseID;
    procedure ParseStringStart(IsRawStr: boolean);
    procedure ParseString;
    procedure ParseNumber;
    procedure ParseParen;
    procedure ParseSpace;
    procedure ParseLineFeed;
    procedure ParseEnter;
    procedure ParseNull;
    procedure ParseUnknown;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1}class{$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TTokenID;
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
    property AttrNumber: TSynHighlighterAttributes read FAttrNumber write FAttrNumber;
    property AttrParen: TSynHighlighterAttributes read FAttrParen write FAttrParen;
    property AttrUnknown: TSynHighlighterAttributes read FAttrUnknown write FAttrUnknown;
  end;

procedure SetKeywords(const Keywords: string);

procedure Register;

implementation

uses
  Clipbrd, SynEditStrConst, lse_funcs;

procedure Register;
begin
  RegisterComponents('Lysee', [TLyseeSyn]);
end;

var
  LyseeKeywords: TStringList;

procedure SetKeywords(const Keywords: string);
begin
  LyseeKeywords.Sorted := false;
  LyseeKeywords.CommaText := Keywords;
  LyseeKeywords.Sorted := true;
end;

{ TLyseeSyn}

procedure TLyseeSyn.ParseSpace;
begin
  FTokenID := tkSpace;
  while FLine[FRun] in [#1..#32] do Inc(FRun);
end;

procedure TLyseeSyn.ParseNull;
begin
  FTokenID := tkNull;
end;

procedure TLyseeSyn.ParseEnter;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TLyseeSyn.ParseLineFeed;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TLyseeSyn.ParseCommentStart;

  procedure skip_line_comment;
  begin
    FRangeID := rkUnknown;
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
      FRangeID := rkComment;
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
    else
    repeat
      if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
      begin
        Inc(FRun, 2);
        FRangeID := rkUnknown;
        Exit;
      end;
      Inc(FRun);
    until FLine[FRun] in [#0, #10, #13];
  end;
end;

procedure TLyseeSyn.ParseStringStart(IsRawStr: boolean);
begin
  FTokenID := tkString;
  Inc(FRun);
  if IsRawStr then
    FRangeID := rkRawStr else
    FRangeID := rkString;
end;

procedure TLyseeSyn.ParseString;
begin
  FTokenID := tkString;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else
    repeat
      if (FLine[FRun] = '"') and (FRangeID = rkString) then
      begin
        Inc(FRun);
        FRangeID := rkUnknown;
        Exit;
      end
      else
      if (FLine[FRun] = '''') and (FRangeID = rkRawStr) then
      begin
        Inc(FRun);
        FRangeID := rkUnknown;
        Exit;
      end
      else
      if FLine[FRun] = '\' then
      begin
        Inc(FRun);
        if FRangeID = rkString then
          if not (FLine[FRun] in [#0, #10, #13]) then
            Inc(FRun);
      end
      else Inc(FRun);
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

  FAttrCurr := TSynHighLighterAttributes.Create(SYNS_AttrUser);
  FAttrCurr.Foreground := clGray;
  AddAttribute(FAttrCurr);

  FAttrKeyword := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  FAttrKeyword.Style := [fsBold];
  AddAttribute(FAttrKeyword);

  FAttrSpace := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FAttrSpace);

  FAttrString := TSynHighLighterAttributes.Create(SYNS_AttrString);
  FAttrString.Foreground := clBlue;
  AddAttribute(FAttrString);

  FAttrNumber := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  FAttrNumber.Foreground := clRed;
  AddAttribute(FAttrNumber);

  FAttrParen := TSynHighLighterAttributes.Create(SYNS_AttrBrackets);
  FAttrParen.Foreground := clRed;//clFuchsia;
  AddAttribute(FAttrParen);

  FAttrUnknown := TSynHighLighterAttributes.Create(SYNS_AttrUnknownWord);
  AddAttribute(FAttrUnknown);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  fDefaultFilter := SYNS_FILTERLANGUAGE;
  FRangeID := rkUnknown;
end;

procedure TLyseeSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: integer);
begin
  FLineString := NewValue;
  FLine := pchar(FLineString);
  FRun := 0;
  FLineNumber := LineNumber;
  if FLineNumber = 0 then
  begin
    FTokenID := tkUnknown;
    FRangeID := rkUnknown;
  end;
  Next;
end;

procedure TLyseeSyn.ParseID;
var
  base: integer;
  temp: string;
begin
  base := FRun;
  while FLine[FRun] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '@'] do Inc(FRun);
  SetString(temp, FLine + base, FRun - base);
  if LyseeKeywords.IndexOf(temp) >= 0 then
    FTokenID := tkKey else
  if temp[1] = '@' then
    FTokenID := tkCurr else
    FTokenID := tkID;
end;

procedure TLyseeSyn.ParseUnknown;
begin
  FTokenID := tkUnknown;
  {$IFDEF SYN_MBCSSUPPORT}
  while FLine[FRun] in LeadBytes do Inc(FRun);
  {$ELSE}
  Inc(FRun);
  {$ENDIF}
end;

procedure TLyseeSyn.Next;
begin
  FTokenPos := FRun;
  if FRangeID = rkComment then ParseComment else
  if FRangeID in [rkString, rkRawStr] then ParseString else
  begin
    FRangeID := rkUnknown;
    case FLine[FRun] of
      #0: ParseNull;
      #10: ParseLineFeed;
      #13: ParseEnter;
      '#': ParseCommentStart;
      '/': ParseCommentStart;
      '"': ParseStringStart(false);
      '''': ParseStringStart(true);
      '0'..'9': ParseNumber;
      '(', ')': ParseParen;
      'A'..'Z', 'a'..'z', '_', '@': ParseID;
      #1..#9, #11, #12, #14..' ': ParseSpace;
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
  Result := LyseeKeywords.CommaText;
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

function TLyseeSyn.GetTokenID: TTokenID;
begin
  Result := FTokenID;
end;

function TLyseeSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  Result := FAttrUnknown;
  case GetTokenID of
    tkComment: Result := FAttrComment;
    tkID     : Result := FAttrID;
    tkCurr   : Result := FAttrCurr;
    tkKey    : Result := FAttrKeyword;
    tkSpace  : Result := FAttrSpace;
    tkString : Result := FAttrString;
    tkNumber : Result := FAttrNumber;
    tkParen  : Result := FAttrParen;
    tkUnknown: Result := FAttrUnknown;
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
  Result := (fDefaultFilter <> SYNS_FILTERLANGUAGE);
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TLyseeSyn.GetLanguageName: string;
begin
  Result := SYNS_LYSEELANGUAGE;
end;

procedure TLyseeSyn.ResetRange;
begin
  FRangeID := rkUnknown;
end;

procedure TLyseeSyn.SetRange(Value: pointer);
begin
  FRangeID := TRangeID(Value);
end;

function TLyseeSyn.GetRange: pointer;
begin
  Result := pointer(FRangeID);
end;

procedure TLyseeSyn.ParseNumber;
var
  S: set of char;
  H: char;
begin
  FTokenID := tkNumber;
  H := FLine[FRun];
  Inc(FRun);
  if (H = '0') and (FLine[FRun] in ['x', 'X']) then
  begin
    S := ['0'..'9', 'a'..'f', 'A'..'F'];
    Inc(FRun);
  end
  else S := ['0'..'9'];
  while FLine[FRun] in S do Inc(FRun);
  FRangeID := rkUnknown;
end;

procedure TLyseeSyn.ParseParen;
begin
  FRangeID := rkUnknown;
  FTokenID := tkParen;
  Inc(FRun);
end;

initialization
{$IFDEF LYSEE_LAZ}
{$I lse_synedit.lrs}
{$ENDIF}
  LyseeKeywords := TStringList.Create;
  LyseeKeywords.CaseSensitive := true;
  LyseeKeywords.CommaText := 'and,as,break,catch,continue,def,do,each,elif,' +
                             'else,end,float,if,in,int,is,like,loop,main,map,' +
                             'object,or,reduce,return,set,string,sys,then,' +
                             'throw,variant,void,while,yield';
  LyseeKeywords.Sorted := true;

finalization
  FreeAndNil(LyseeKeyWords);

end.
