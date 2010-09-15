{==============================================================================}
{        UNIT: lse_patten                                                      }
{ DESCRIPTION: match patten (regular expression) functions                     }
{     CREATED: 2010/02/28                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2010, Li Yun Jie                                               }
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
{ Portions created by Li Yun Jie are Copyright (C) 2010.                       }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_patten;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, lseu, lse_funcs;

const
  MAX_CAPUTRES = 32;

type
  RLiMatchRec = packed record
    mr_str: pchar;
    mr_len: integer;
  end;
  PLiMatchRec = ^RLiMatchRec;
  
  RLiMatchPatten = packed record
    mp_patten: pchar;              // patten string
    mp_anchor: boolean;            // patten start with '^'?
    mp_boscp: array[0..1] of char; // buffer of single char patten
    mp_source: pchar;              // source string
    mp_eos: pchar;                 // end of source string
    mp_escape: char;               // escape char, default '\'
    mp_captures: array[0..MAX_CAPUTRES - 1] of RLiMatchRec;
    mp_level: integer;             // total number of captures
    mp_result: RLiMatchRec;        // match result
  end;
  PLiMatchPatten = ^RLiMatchPatten;

function init_patten(mp: PLiMatchPatten; const Patten: pchar; EscapeCh: char): boolean;overload;
function init_patten(mp: PLiMatchPatten; const Patten: pchar): boolean;overload;
function init_patten(mp: PLiMatchPatten; Patten: char): boolean;overload;
function init_patten(mp: PLiMatchPatten; const Patten: PLseString): boolean;overload;
function init_patten(mp: PLiMatchPatten; const Patten: PLseValue): boolean;overload;

function exec_patten(mp: PLiMatchPatten; S: pchar; L: integer): boolean;overload;
function exec_patten(mp: PLiMatchPatten; const S: string): boolean;overload;
function exec_patten(mp: PLiMatchPatten; ch: char): boolean;overload;
function exec_patten(mp: PLiMatchPatten; const S: PLseString): boolean;overload;
function exec_patten(mp: PLiMatchPatten; const S: PLseValue): boolean;overload;

implementation

function match(mp: PLiMatchPatten; Source, Patten: pchar): pchar;
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
    if cl >= MAX_CAPUTRES then
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

function init_patten(mp: PLiMatchPatten; const Patten: pchar; EscapeCh: char): boolean;
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

function init_patten(mp: PLiMatchPatten; const Patten: pchar): boolean;
begin
  Result := init_patten(mp, Patten, '\');
end;

function init_patten(mp: PLiMatchPatten; Patten: char): boolean;
begin
  mp^.mp_boscp[0] := Patten;
  mp^.mp_boscp[1] := #0;
  Result := init_patten(mp, mp^.mp_boscp);
end;

function init_patten(mp: PLiMatchPatten; const Patten: PLseString): boolean;
begin
  Result := init_patten(mp, lse_strec_data(Patten));
end;

function init_patten(mp: PLiMatchPatten; const Patten: PLseValue): boolean;
begin
  if Patten^.value_class^.vtype = LSV_STRING then
    Result := init_patten(mp, lse_strec_data(Patten^.VString)) else
  if Patten^.value_class^.vtype = LSV_CHAR then
  begin
    mp^.mp_boscp[0] := Patten^.VChar;
    mp^.mp_boscp[1] := #0;
    Result := init_patten(mp, mp^.mp_boscp);
  end
  else Result := false;
end;

var
  zero: char = #0;
  
function exec_patten(mp: PLiMatchPatten; S: pchar; L: integer): boolean;
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

function exec_patten(mp: PLiMatchPatten; const S: string): boolean;
begin
  Result := exec_patten(mp, pchar(S), Length(S));
end;

function exec_patten(mp: PLiMatchPatten; ch: char): boolean;
begin
  Result := exec_patten(mp, @ch, 1);
end;

function exec_patten(mp: PLiMatchPatten; const S: PLseString): boolean;
begin
  Result := exec_patten(mp, lse_strec_data(S), lse_strec_length(S));
end;

function exec_patten(mp: PLiMatchPatten; const S: PLseValue): boolean;
begin
  if S^.value_class^.vtype = LSV_STRING then
    Result := exec_patten(mp, lse_strec_data(S^.VString),
      lse_strec_length(S^.VString)) else
  if S^.value_class^.vtype = LSV_CHAR then
    Result := exec_patten(mp, @(S^.VChar), 1) else
    Result := false;
end;

end.

