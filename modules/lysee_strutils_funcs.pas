{==============================================================================}
{     PROJECT: lysee_strutils_funcs                                            }
{ DESCRIPTION: string utility functions (FPC)                                  }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2010/09/01                                                      }
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
unit lysee_strutils_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu;

const
  AlphaChar = ['A'..'Z', 'a'..'z'];
  SpaceChar = [#$09, #$0A, #$0C, #$0D, #$20];

type

  KLiCharSet = set of Char;

  { TLiStrBuf }

  TLiStrBuf = class(TLseObject)
  private
    FStrBuf: string;
    function GetStrLen: integer;
    procedure SetStrLen(const Value: integer);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetChar(index: integer): char;
    procedure SetChar(index: integer; const Value: char);
  public
    constructor Create(const AStr: string);
    procedure TrimAll;
    property StrLen: integer read GetStrLen write SetStrLen;
    property StrBuf: string read FStrBuf;
    property Name: string read GetName write SetName;
    property Value: string read GetValue write SetValue;
    property Chars[index: integer]: char read GetChar write SetChar;
  end;

  { TLiStrCut }

  TLiStrCut = class(TLseObject)
  private
    FDelimiter: char;
    FMask: array of integer;
    FNameList: array of string;
    FValueList: array of string;
    function GetCount: integer;
    function GetName(Index: integer): string;
    procedure SetName(Index: integer; const Value: string);
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function ParseNext(var S: pchar): string;
    function DeQuote(var S: pchar): string;
    function GetMask: string;
  public
    constructor Create(const AHeader: string; Delimiter: char);
    procedure Clear;
    procedure ClearValueList;
    procedure Init(const AHeader: string; Delimiter: char);
    procedure Parse(const S: string);
    function IndexOf(const Name: string): integer;
    function ValueAt(Index: integer): string;
    function NameList: string;
    function ValueList: string;
    function Keys: string;
    function Vals: string;
    procedure SaveToStrlist(list: TStringList);
    function ReadMatched(Source: TLiStrCut): integer;
    procedure Exchange(X1, X2: integer);
    procedure Move(X1, X2: Integer);
    procedure Delete(X: integer);
    procedure Remove(const Name: string);
    procedure Rename(const CurName, NewName: string);
    property Delimiter: char read FDelimiter;
    property Count: integer read GetCount;
    property Names[Index: integer]: string read GetName write SetName;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Mask: string read GetMask;
  end;

{ strbuf }

procedure pp_strbuf_create(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_length(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_setlen(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_gets(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_sets(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_getiv(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_setiv(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_getValue(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_setValue(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_replace(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_pos(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_lastPos(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_trim(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_trimLeft(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_trimRight(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_trimAll(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_lower(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_upper(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_delete(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_insert(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_join(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_reverse(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_randomize(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_contains(const invoker: TLseInvoke);cdecl;
procedure pp_strbuf_copy(const invoker: TLseInvoke);cdecl;

const

  strbuf_func_count = 25;
  strbuf_func_array: array[0..strbuf_func_count - 1] of RLseFuncRec = (
    (fr_prot:'strbuf strbuf(string value)';
     fr_addr:@pp_strbuf_create;
     fr_desc:'create strbuf object';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_strbuf_length;
     fr_desc:'get strbuf size';
    ),
    (fr_prot:'void set_length(int length)';
     fr_addr:@pp_strbuf_setlen;
     fr_desc:'set strbuf size';
    ),
    (fr_prot:'string get_data()';
     fr_addr:@pp_strbuf_gets;
     fr_desc:'get data string';
    ),
    (fr_prot:'void set_data(string data)';
     fr_addr:@pp_strbuf_sets;
     fr_desc:'set data string';
    ),
    (fr_prot:'char getiv(int index)';
     fr_addr:@pp_strbuf_getiv;
     fr_desc:'get charactor item by index';
    ),
    (fr_prot:'void setiv(int index, char value)';
     fr_addr:@pp_strbuf_setiv;
     fr_desc:'get charactor item by index';
    ),
    (fr_prot:'string get_value()';
     fr_addr:@pp_strbuf_getValue;
     fr_desc:'get strbuf string';
    ),
    (fr_prot:'void set_value(string value)';
     fr_addr:@pp_strbuf_setValue;
     fr_desc:'set strbuf string';
    ),
    (fr_prot:'void replace(string patten, string newStr, bool ignoreCase, bool replaceFirstOnly)';
     fr_addr:@pp_strbuf_replace;
     fr_desc:'replace patten to new string';
    ),
    (fr_prot:'int pos(string SubStr, bool IgnoreCase, int start)';
     fr_addr:@pp_strbuf_pos;
     fr_desc:'get first sub-string position';
    ),
    (fr_prot:'int lastpos(string SubStr, bool IgnoreCase, int start)';
     fr_addr:@pp_strbuf_lastPos;
     fr_desc:'get last sub-string position';
    ),
    (fr_prot:'void trim()';
     fr_addr:@pp_strbuf_trim;
     fr_desc:'clear left and right spaces';
    ),
    (fr_prot:'void trimLeft()';
     fr_addr:@pp_strbuf_trimLeft;
     fr_desc:'clear left spaces';
    ),
    (fr_prot:'void trimRight()';
     fr_addr:@pp_strbuf_trimRight;
     fr_desc:'clear right spaces';
    ),
    (fr_prot:'void trimAll()';
     fr_addr:@pp_strbuf_trimAll;
     fr_desc:'clear all spaces';
    ),
    (fr_prot:'void lower()';
     fr_addr:@pp_strbuf_lower;
     fr_desc:'translate to lower case';
    ),
    (fr_prot:'void upper()';
     fr_addr:@pp_strbuf_upper;
     fr_desc:'translate to upper case';
    ),
    (fr_prot:'void delete(int index, int count)';
     fr_addr:@pp_strbuf_delete;
     fr_desc:'delete sub-string by range';
    ),
    (fr_prot:'void insert(string substr, int index)';
     fr_addr:@pp_strbuf_insert;
     fr_desc:'insert sub-string at specified position';
    ),
    (fr_prot:'void join(string str)';
     fr_addr:@pp_strbuf_join;
     fr_desc:'join (concatenate) string';
    ),
    (fr_prot:'void reverse()';
     fr_addr:@pp_strbuf_reverse;
     fr_desc:'reverse string buffer';
    ),
    (fr_prot:'void randomize()';
     fr_addr:@pp_strbuf_randomize;
     fr_desc:'randomize string buffer';
    ),
    (fr_prot:'bool contains(string subStr, bool ignoreCase)';
     fr_addr:@pp_strbuf_contains;
     fr_desc:'test if contains specified sub-string';
    ),
    (fr_prot:'string copy(int index, int count)';
     fr_addr:@pp_strbuf_copy;
     fr_desc:'copy sub-string by range';
    )
  );

{ strcut }

procedure pp_strcut_create(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_reinit(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_parse(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_length(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_getiv(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_setiv(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_getpv(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_setpv(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_getname(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_setname(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_rename(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_indexof(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_clear(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_keylist(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_valist(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_keys(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_values(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_strlist(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_delete(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_remove(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_exchange(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_exchangeByName(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_move(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_moveByName(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_mask(const invoker: TLseInvoke);cdecl;
procedure pp_strcut_delimiter(const invoker: TLseInvoke);cdecl;

const

  strcut_func_count = 26;
  strcut_func_array: array[0..strcut_func_count - 1] of RLseFuncRec = (
    (fr_prot:'strcut strcut(string keylist, char delimiter)';
     fr_addr:@pp_strcut_create;
     fr_desc:'create strcut object';
    ),
    (fr_prot:'void reinit(string keylist, char delimiter)';
     fr_addr:@pp_strcut_reinit;
     fr_desc:'reinitialize this string cutter';
    ),
    (fr_prot:'void parse(string text)';
     fr_addr:@pp_strcut_parse;
     fr_desc:'parse plain text';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_strcut_length;
     fr_desc:'get key count';
    ),
    (fr_prot:'string getiv(int index)';
     fr_addr:@pp_strcut_getiv;
     fr_desc:'get value by key index';
    ),
    (fr_prot:'void setiv(int index, string value)';
     fr_addr:@pp_strcut_setiv;
     fr_desc:'set value by key index';
    ),
    (fr_prot:'string getpv(string key)';
     fr_addr:@pp_strcut_getpv;
     fr_desc:'get value by key name';
    ),
    (fr_prot:'void setpv(string key, string value)';
     fr_addr:@pp_strcut_setpv;
     fr_desc:'set value by key name';
    ),
    (fr_prot:'string getKey(int index)';
     fr_addr:@pp_strcut_getname;
     fr_desc:'get key name by index';
    ),
    (fr_prot:'void setKey(int index, string newKey)';
     fr_addr:@pp_strcut_setname;
     fr_desc:'change key name by index';
    ),
    (fr_prot:'void rename(string key, string newKey)';
     fr_addr:@pp_strcut_rename;
     fr_desc:'rename key';
    ),
    (fr_prot:'int indexOf(string key)';
     fr_addr:@pp_strcut_indexof;
     fr_desc:'get key index';
    ),
    (fr_prot:'void clear()';
     fr_addr:@pp_strcut_clear;
     fr_desc:'clear values';
    ),
    (fr_prot:'string get_keyList()';
     fr_addr:@pp_strcut_keylist;
     fr_desc:'list key with delimiter char';
    ),
    (fr_prot:'string get_valueList()';
     fr_addr:@pp_strcut_valist;
     fr_desc:'list value with delimiter char';
    ),
    (fr_prot:'strlist get_keys()';
     fr_addr:@pp_strcut_keys;
     fr_desc:'list key into strlist';
    ),
    (fr_prot:'strlist get_values()';
     fr_addr:@pp_strcut_values;
     fr_desc:'list value into strlist';
    ),
    (fr_prot:'strlist get_strlist()';
     fr_addr:@pp_strcut_strlist;
     fr_desc:'convert to strlist';
    ),
    (fr_prot:'void delete(int index)';
     fr_addr:@pp_strcut_delete;
     fr_desc:'delete key by index';
    ),
    (fr_prot:'void remove(string key)';
     fr_addr:@pp_strcut_remove;
     fr_desc:'remove key by name';
    ),
    (fr_prot:'void exchange(int X1, int X2)';
     fr_addr:@pp_strcut_exchange;
     fr_desc:'exchange two keys by index';
    ),
    (fr_prot:'void exchangeByName(string name1, string name2)';
     fr_addr:@pp_strcut_exchangeByName;
     fr_desc:'exchange two keys by name';
    ),
    (fr_prot:'void move(int X1, int X2)';
     fr_addr:@pp_strcut_move;
     fr_desc:'move key to another place by index';
    ),
    (fr_prot:'void moveByName(string key, string another)';
     fr_addr:@pp_strcut_moveByName;
     fr_desc:'move key to another place by name';
    ),
    (fr_prot:'string get_mask()';
     fr_addr:@pp_strcut_mask;
     fr_desc:'get parsing mask';
    ),
    (fr_prot:'char get_delimiter()';
     fr_addr:@pp_strcut_delimiter;
     fr_desc:'get parsing delimiter';
    )
  );

  strutils_class: array[0..1] of RLseClassRec = (
   (vtype      : LSV_OBJECT;
    name       : 'strbuf';
    desc       : 'string buffer object';
    incRefcount:@lse_incRefcount;
    decRefcount:@lse_decRefcount;
    funcs      : (count:strbuf_func_count; entry:@strbuf_func_array)
   ),
   (vtype      : LSV_OBJECT;
    name       : 'strcut';
    desc       : 'string cutter object';
    incRefcount:@lse_incRefcount;
    decRefcount:@lse_decRefcount;
    funcs      : (count:strcut_func_count; entry:@strcut_func_array)
   )
  );

function strbuf_classrec: PLseClassRec;
function strcut_classrec: PLseClassRec;

implementation

uses
  math, strutils, lse_funcs;

function strbuf_classrec: PLseClassRec;
begin
  Result := @strutils_class[0];
end;

function strcut_classrec: PLseClassRec;
begin
  Result := @strutils_class[1];
end;

{ strbuf }

procedure pp_strbuf_create(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  this := TLiStrBuf.Create(invoker.paramStr(1));
  invoker.returnObj(strbuf_classrec, this);
end;

procedure pp_strbuf_length(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    invoker.returnInt(this.StrLen);
end;

procedure pp_strbuf_setlen(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.StrLen := invoker.paramInt64(1);
end;

procedure pp_strbuf_gets(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.FStrBuf);
end;

procedure pp_strbuf_sets(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := invoker.paramStr(1);
end;

procedure pp_strbuf_getiv(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  index: integer;
begin
  if invoker.GetThis(this) then
  begin
    index := lse_vary_index(invoker.paramInt64(1), this.StrLen);
    lse_check_index(index, this.StrLen);
    invoker.returnChar(this.Chars[index]);
  end;
end;

procedure pp_strbuf_setiv(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  index: integer;
begin
  if invoker.GetThis(this) then
  begin
    index := lse_vary_index(invoker.paramInt64(1), this.StrLen);
    lse_check_index(index, this.StrLen);
    this.Chars[index] := invoker.paramChar(2);
  end;
end;

procedure pp_strbuf_getValue(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.FStrBuf);
end;

procedure pp_strbuf_setValue(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := invoker.paramStr(1);
end;

procedure pp_strbuf_replace(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  patten, newStr: string;
  flags: TReplaceFlags;
begin
  if invoker.GetThis(this) then
  begin
    patten := invoker.paramStr(1);
    newStr := invoker.paramStr(2);
    flags := [rfReplaceAll];
    if invoker.paramBool(3) then // IgnoreCase
      flags := flags + [rfIgnoreCase];
    if invoker.paramBool(4) then // FirstOnly
      flags := flags - [rfReplaceAll];
    this.FStrBuf := StringReplace(this.FStrBuf, patten, newStr, flags);
  end;
end;

procedure pp_strbuf_pos(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  subs: string;
  base, str, pos: pchar;
  start: integer;
begin
  if invoker.GetThis(this) then
  begin
    start := lse_vary_index(invoker.paramInt64(3), this.StrLen);
    if (start >= 0) and (start < this.StrLen) then
    begin
      base := pchar(this.FStrBuf);
      str := base + start;
      subs := invoker.paramStr(1);
      pos := __pos(str, this.StrLen, pchar(subs), Length(subs),
                   invoker.paramBool(2));
      if pos <> nil then
        invoker.returnInt64(pos - base) else
        invoker.returnInt64(-1);
    end
    else invoker.returnInt64(-1);
  end;
end;

procedure pp_strbuf_lastPos(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  subs: string;
  base, str, pos: pchar;
  endx: integer;
begin
  if invoker.GetThis(this) then
  begin
    if invoker.ParamCount < 4 then
      endx := this.StrLen else
      endx := invoker.paramInt64(3);
    if (endx > 0) and (endx <= this.StrLen) then
    begin
      base := pchar(this.FStrBuf);
      str := base;
      subs := invoker.paramStr(1);
      pos := __lastPos(str, endx, pchar(subs), Length(subs),
                       invoker.paramBool(2));
      if pos <> nil then
        invoker.returnInt64(pos - base) else
        invoker.returnInt64(-1);
    end
    else invoker.returnInt64(-1);
  end;
end;

procedure pp_strbuf_trim(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := Trim(this.FStrBuf);
end;

procedure pp_strbuf_trimLeft(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := TrimLeft(this.FStrBuf);
end;

procedure pp_strbuf_trimRight(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := TrimRight(this.FStrBuf);
end;

procedure pp_strbuf_trimAll(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.TrimAll;
end;

procedure pp_strbuf_lower(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := LowerCase(this.FStrBuf);
end;

procedure pp_strbuf_upper(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := UpperCase(this.FStrBuf);
end;

procedure pp_strbuf_delete(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  index, count: integer;
begin
  if invoker.GetThis(this) then
    if invoker.ParamCount > 1 then
    begin
      if invoker.ParamCount > 2 then
        count := invoker.paramInt64(2) else
        count := 1;
      index := lse_vary_index(invoker.paramInt64(1), this.StrLen);
      if index < 0 then
      begin
        Inc(count, index);
        index := 0;
      end;
      if (count > 0) and (index < this.StrLen) then
        System.Delete(this.FStrBuf, index + 1, count);
    end
    else this.FStrBuf := '';
end;

procedure pp_strbuf_insert(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  subs: string;
  index: integer;
begin
  if invoker.GetThis(this) then
    if invoker.ParamCount = 3 then
    begin
      subs := invoker.paramStr(1);
      if subs <> '' then
      begin
        index := lse_vary_index(invoker.paramInt64(2), this.StrLen);
        if (index >= 0) and (index <= this.StrLen) then
          System.Insert(subs, this.FStrBuf, index + 1);
      end;
    end;
end;

procedure pp_strbuf_join(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := this.FStrBuf + invoker.paramStr(1);
end;

procedure pp_strbuf_reverse(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
begin
  if invoker.GetThis(this) then
    this.FStrBuf := ReverseString(this.FStrBuf);
end;

procedure pp_strbuf_randomize(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  index, count, X: integer;
  ch: char;
begin
  if invoker.GetThis(this) then
  begin
    count := this.StrLen;
    if count > 1 then
      for index := 1 to count do
      begin
        X := random(count) + 1;
        ch := this.FStrBuf[X];
        this.FStrBuf[X] := this.FStrBuf[index];
        this.FStrBuf[index] := ch;
      end;
  end;
end;

// bool strbuf.contains(string subStr, bool ignoreCase)
procedure pp_strbuf_contains(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  subs: string;
  str, pos: pchar;
begin
  if invoker.GetThis(this) then
  begin
    str := pchar(this.FStrBuf);
    subs := invoker.paramStr(1);
    if subs <> '' then
    begin
      pos := __pos(str, this.StrLen, pchar(subs), Length(subs),
        invoker.paramBool(2));
      invoker.returnBool(pos <> nil);
    end;
  end;
end;

procedure pp_strbuf_copy(const invoker: TLseInvoke);cdecl;
var
  this: TLiStrBuf;
  index, count, len: int64;
begin
  if invoker.GetThis(this) then
  begin
    len := this.StrLen;
    if len > 0 then
    begin
      count := invoker.paramInt64(2);
      index := lse_vary_range(invoker.paramInt64(1), len, count);
      invoker.returnStr(Copy(this.FStrBuf, index + 1, count));
    end;
  end;
end;

{ strcut }

procedure pp_strcut_create(const invoker: TLseInvoke);cdecl;
var
  header: string;
  delimiter: char;
begin
  header := invoker.paramStr(1);
  delimiter := invoker.paramChar(2);
  invoker.returnObj(strcut_classrec, TLiStrCut.Create(header, delimiter));
end;

procedure pp_strcut_reinit(const invoker: TLseInvoke);cdecl;
var
  header: string;
  delimiter: char;
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
  begin
    header := invoker.paramStr(1);
    if invoker.ParamCount > 2 then
      delimiter := invoker.paramChar(2) else
      delimiter := cutter.FDelimiter;
    cutter.Init(header, delimiter);
  end;
end;

procedure pp_strcut_parse(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Parse(invoker.paramStr(1));
end;

procedure pp_strcut_length(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnInt64(cutter.Count);
end;

procedure pp_strcut_getiv(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    index := lse_Vary_index(invoker.paramInt64(1), cutter.Count);
    lse_check_index(index, cutter.Count);
    invoker.returnStr(cutter.ValueAt(index));
  end;
end;

procedure pp_strcut_setiv(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    index := lse_vary_index(invoker.paramInt64(1), cutter.Count);
    lse_check_index(index, cutter.Count);
    cutter.FValueList[index] := invoker.paramStr(2);
  end;
end;

procedure pp_strcut_getpv(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  column: string;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    column := invoker.paramStr(1);
    index := cutter.IndexOf(column);
    if index < 0 then
      invoker.returnError('', 0, Format('key "%s" not found', [column])) else
      invoker.returnStr(cutter.ValueAt(index));
  end;
end;

procedure pp_strcut_setpv(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  column: string;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    column := invoker.paramStr(1);
    index := cutter.IndexOf(column);
    if index < 0 then
      invoker.returnError('', 0, Format('key "%s" not found', [column])) else
      cutter.FValueList[index] := invoker.paramStr(2);
  end;
end;

procedure pp_strcut_getname(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    index := lse_vary_index(invoker.paramInt64(1), cutter.Count);
    invoker.returnStr(cutter.Names[index]);
  end;
end;

procedure pp_strcut_setname(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  index: integer;
begin
  if invoker.GetThis(cutter) then
  begin
    index := lse_vary_index(invoker.paramInt64(1), cutter.Count);
    cutter.SetName(index, Trim(invoker.paramStr(2)));
  end;
end;

procedure pp_strcut_rename(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Rename(Trim(invoker.paramStr(1)),
                  Trim(invoker.paramStr(2)));
end;

procedure pp_strcut_indexof(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  column: string;
begin
  if invoker.GetThis(cutter) then
  begin
    column := invoker.paramStr(1);
    invoker.returnInt64(cutter.IndexOf(column));
  end;
end;

procedure pp_strcut_clear(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.ClearValueList;
end;

procedure pp_strcut_keylist(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnStr(cutter.NameList);
end;

procedure pp_strcut_valist(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnStr(cutter.ValueList);
end;

procedure pp_strcut_keys(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnStr(cutter.Keys);
end;

procedure pp_strcut_values(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnStr(cutter.Vals);
end;

procedure pp_strcut_strlist(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
  list: TStringList;
begin
  if invoker.GetThis(cutter) then
  begin
    list := TStringList.Create;
    try
      cutter.SaveToStrlist(list);
      invoker.returnStr(list.Text);
    finally
      list.Free;
    end;
  end;
end;

procedure pp_strcut_delete(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Delete(invoker.paramInt64(1));
end;

procedure pp_strcut_remove(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Remove(invoker.paramStr(1));
end;

procedure pp_strcut_exchange(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Exchange(invoker.paramInt64(1),
                    invoker.paramInt64(2));
end;

procedure pp_strcut_exchangeByName(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) and (invoker.ParamCount > 2) then
    cutter.Exchange(cutter.IndexOf(invoker.paramStr(1)),
                    cutter.IndexOf(invoker.paramStr(2)));
end;

procedure pp_strcut_move(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    cutter.Move(invoker.paramInt64(1),
                invoker.paramInt64(2));
end;

procedure pp_strcut_moveByName(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) and (invoker.ParamCount > 2) then
    cutter.Move(cutter.IndexOf(invoker.paramStr(1)),
                cutter.IndexOf(invoker.paramStr(2)));
end;

procedure pp_strcut_mask(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnStr(cutter.Mask);
end;

procedure pp_strcut_delimiter(const invoker: TLseInvoke);cdecl;
var
  cutter: TLiStrCut;
begin
  if invoker.GetThis(cutter) then
    invoker.returnChar(cutter.FDelimiter);
end;

{ TLiStrBuf }

constructor TLiStrBuf.Create(const AStr: string);
begin
  inherited Create;
  FStrBuf := AStr;
end;

procedure TLiStrBuf.TrimAll;
var
  slen, L, M, R: integer;
  base, next: pchar;
  temp: string;
begin
  base := pchar(FStrBuf);
  slen := Length(FStrBuf);
  if __countTab(base, slen, L, M, R) > 0 then
  begin
    SetLength(temp, slen - (L + M + R));
    next := pchar(temp);
    while base^ <> #0  do
    begin
      if not (base^ in SpaceChar) then
      begin
        next^ := base^;
        Inc(next);
      end;
      Inc(base);
    end;
    FStrBuf := temp;
  end;
end;

function TLiStrBuf.GetChar(index: integer): char;
begin
  Result := FStrBuf[index + 1];
end;

function TLiStrBuf.GetName: string;
begin
  Result := __extractName(FStrBuf, '=');
end;

function TLiStrBuf.GetStrLen: integer;
begin
  Result := Length(FStrBuf);
end;

function TLiStrBuf.GetValue: string;
begin
  Result := __extractValue(FStrBuf, '=');
end;

procedure TLiStrBuf.SetChar(index: integer; const Value: char);
begin
  if GetStrLen <= index then
    SetStrLen(index + 1);
  FStrBuf[index + 1] := Value;
end;

procedure TLiStrBuf.SetName(const Value: string);
begin
  FStrBuf := Value + '=' + GetValue;
end;

procedure TLiStrBuf.SetStrLen(const Value: integer);
begin
  if Value > 0 then
    SetLength(FStrBuf, Value) else
    FStrBuf := '';
end;

procedure TLiStrBuf.SetValue(const Value: string);
begin
  FStrBuf := GetName + '=' + Value;
end;

{ TLiStrCut }

procedure TLiStrCut.Clear;
var
  index: integer;
begin
  FDelimiter := ',';
  SetLength(FMask, 0);
  for index := 0 to Length(FNameList) - 1 do
    FNameList[index] := '';
  SetLength(FNameList, 0);
  ClearValueList;
  SetLength(FValueList, 0);
end;

procedure TLiStrCut.ClearValueList;
var
  index: integer;
begin
  for index := 0 to Length(FValueList) - 1 do
    FValueList[index] := '';
end;

constructor TLiStrCut.Create(const AHeader: string; Delimiter: char);
begin
  Init(AHeader, Delimiter);
end;

procedure TLiStrCut.Delete(X: integer);
var
  index, N: integer;
begin
  lse_check_index(X, GetCount);
  N := GetCount - 1;
  for index := X to N - 1 do
  begin
    FNameList[index] := FNameList[index + 1];
    FValueList[index] := FValueList[index + 1];
  end;
  SetLength(FNameList, N);
  SetLength(FValueList, N);
  for index := 0 to Length(FMask) - 1 do
    if FMask[index] = X then FMask[index] := -1 else
    if FMask[index] > X then Dec(FMask[index]);
end;

function TLiStrCut.DeQuote(var S: pchar): string;
var
  quotec: char;
begin
  Result := '';
  if (S <> nil) and (S^ <> #0) then
  begin
    quotec := S^;
    Inc(S);
    while S^ <> #0 do
    begin
      if S^ = quotec then // "" ''
      begin
        Inc(S);
        if S^ <> quotec then Exit;
      end;
      Result := Result + S^;
      Inc(S);
    end;
    if S^ = quotec then Inc(S);
  end;
end;

procedure TLiStrCut.Exchange(X1, X2: integer);
var
  index: integer;
  temp: string;
begin
  if X1 <> X2 then
  begin
    lse_check_index(X1, GetCount);
    lse_check_index(X2, GetCount);

    temp := FNameList[X1];
    FNameList[X1] := FNameList[X2];
    FNameList[X2] := temp;

    temp := FValueList[X1];
    FValueList[X1] := FValueList[X2];
    FValueList[X2] := temp;

    for index := 0 to Length(FMask) - 1 do
      if FMask[index] = X1 then FMask[index] := X2 else
      if FMask[index] = X2 then FMask[index] := X1;
  end;
end;

function TLiStrCut.GetCount: integer;
begin
  Result := Length(FNameList);
end;

function TLiStrCut.GetMask: string;
var
  index, X: integer;
begin
  Result := '';
  for index := 0 to Length(FMask) - 1 do
  begin
    X := FMask[index];
    if X >= 0 then
      Result := Result + FNameList[X] + FDelimiter else
      Result := Result + FDelimiter;
  end;
  X := Length(Result);
  if (X > 1) and (Result[X - 1] <> FDelimiter) then
    SetLength(Result, X - 1);
end;

function TLiStrCut.GetName(Index: integer): string;
begin
  Result := FNameList[Index];
end;

function TLiStrCut.GetValue(const Name: string): string;
begin
  Result := ValueAt(IndexOf(Name));
end;

function TLiStrCut.IndexOf(const Name: string): integer;
var
  index: integer;
begin
  for index := 0 to Length(FNameList) - 1 do
    if AnsiSameText(Name, FNameList[index]) then
    begin
      Result := index;
      Exit;
    end;
  Result := -1;
end;

procedure TLiStrCut.Init(const AHeader: string; Delimiter: char);
var
  source: pchar;
  column: string;
  total, index: integer;
begin
  Clear;
  if Delimiter = #0 then Delimiter := ',';
  FDelimiter := Delimiter;
  total := 0;
  index := 0;
  source := pchar(AHeader);
  while (source <> nil) and (source^ <> #0) do
  begin
    column := Trim(ParseNext(source));
    SetLength(FMask, total + 1);
    if column <> '' then
    begin
      FMask[total] := index;
      SetLength(FNameList, index + 1);
      FNameList[index] := column;
      SetLength(FValueList, index + 1);
      FValueList[index] := '';
      Inc(index);
    end
    else FMask[total] := -1;
    Inc(total);
  end;
end;

function TLiStrCut.Keys: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FNameList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + sLineBreak + FNameList[index];
  end
  else Result := '';
end;

procedure TLiStrCut.Move(X1, X2: Integer);
begin
  if X1 <> X2 then
  begin
    lse_check_index(X1, GetCount);
    lse_check_index(X2, GetCount);

    while X1 < X2 do
    begin
      Exchange(X1, X1 + 1);
      Inc(X1);
    end;

    while X1 > X2 do
    begin
      Exchange(X1, X1 - 1);
      Dec(X1);
    end;
  end;
end;

function TLiStrCut.NameList: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FNameList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + FDelimiter + FNameList[index];
  end
  else Result := '';
end;

procedure TLiStrCut.Parse(const S: string);
var
  source: pchar;
  value: string;
  index, N: integer;
begin
  ClearValueList;
  index := 0;
  N := GetCount;
  source := pchar(S);
  while (N > 0) and (source <> nil) and (source^ <> #0) do
  begin
    value := ParseNext(source);
    if FMask[index] >= 0 then
    begin
      FValueList[FMask[index]] := value;
      Dec(N);
    end;
    Inc(index);
  end;
end;

function TLiStrCut.ParseNext(var S: pchar): string;
var
  base: pchar;
begin
  Result := '';
  if S = nil then Exit;
  if S^ = #0 then
  begin
    S := nil;
    Exit;
  end;

  if FDelimiter = ' ' then
  begin
    S := __skipch(S, SpaceChar);
    if S^ = #0 then
    begin
      S := nil;
      Exit;
    end;
  end
  else
  if S^ = FDelimiter then
  begin
    Inc(S);
    Exit;
  end;

  if S^ in ['"', ''''] then
  begin
    Result := DeQuote(S);
    if (S^ <> #0) and ((FDelimiter <> ' ') or not (S^ in SpaceChar)) then
      Result := Result + ParseNext(S) else
      S := nil;
  end
  else
  begin
    base := S;
    while not (S^ in [FDelimiter, #0]) do Inc(S);
    SetString(Result, base, S - base);
    if S^ = #0 then
      S := nil else
      Inc(S);
  end;
end;

function TLiStrCut.ReadMatched(Source: TLiStrCut): integer;
var
  index, X: integer;
begin
  Result := 0;

  if (Source = nil) or (Source = Self) or (Source.GetCount = 0)
  or (GetCount = 0) then Exit;

  for index := 0 to GetCount - 1 do
  begin
    X := Source.IndexOf(FNameList[index]);
    if X >= 0 then
    begin
      FValueList[index] := Source.FValueList[X];
      Inc(Result);
    end;
  end;
end;

procedure TLiStrCut.Remove(const Name: string);
var
  index: integer;
begin
  index := IndexOf(Name);
  if index >= 0 then
    Delete(index);
end;

procedure TLiStrCut.Rename(const CurName, NewName: string);
begin
  FNameList[IndexOf(CurName)] := NewName;
end;

procedure TLiStrCut.SaveToStrlist(list: TStringList);
var
  index: integer;
begin
  if list <> nil then
  begin
    list.Clear;
    for index := 0 to GetCount - 1 do
      list.Add(FNameList[index] + '=' + FValueList[index]);
  end;
end;

procedure TLiStrCut.SetName(Index: integer; const Value: string);
begin
  FNameList[Index] := Value;
end;

procedure TLiStrCut.SetValue(const Name, Value: string);
begin
  FValueList[IndexOf(Name)] := Value;
end;

function TLiStrCut.Vals: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FValueList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + sLineBreak + FValueList[index];
  end
  else Result := '';
end;

function TLiStrCut.ValueAt(Index: integer): string;
begin
  Result := FValueList[Index];
end;

function TLiStrCut.ValueList: string;
var
  index: integer;
begin
  if GetCount > 0 then
  begin
    Result := FValueList[0];
    for index := 1 to GetCount - 1 do
      Result := Result + FDelimiter + FValueList[index];
  end
  else Result := '';
end;

end.

