{==============================================================================}
{        UNIT: lysee_ym                                                        }
{ DESCRIPTION: year-month interger functions (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_ym;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  DateUtils,
  lseu in '../lseu.pas',
  lse_funcs in '../lse_funcs.pas';

procedure ym_now(const invoker: TLseInvoke);cdecl;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  invoker.returnInt((y * 100) + m);
end;

procedure ym_check(const invoker: TLseInvoke);cdecl;
var
  y, m: integer;
begin
  invoker.returnBool(__decodeYM(invoker.paramInt(0), y, m));
end;

procedure ym_next(const invoker: TLseInvoke);cdecl;
var
  n, ym: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ym_now(invoker);
    Exit;
  end;

  ym := invoker.paramInt(0);
  if n = 1 then
    ym := __nextYM(ym, 1) else
    ym := __nextYM(ym, invoker.paramInt(1));

  invoker.returnInt(ym);
end;

procedure ym_prev(const invoker: TLseInvoke);cdecl;
var
  n, ym: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ym_now(invoker);
    Exit;
  end;

  ym := invoker.paramInt(0);
  if n = 1 then
    ym := __prevYM(ym, 1) else
    ym := __prevYM(ym, invoker.paramInt(1));

  invoker.returnInt(ym);
end;

const
  func_count = 4;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'now:int ||';
     fr_addr:@ym_now;
     fr_desc:'get current ym'
    ),
    (fr_prot:'check:int|ym:int|';
     fr_addr:@ym_check;
     fr_desc:'check ym'
    ),
    (fr_prot:'next:int |ym:int, months:int|';
     fr_addr:@ym_next;
     fr_desc:'get next ym'
    ),
    (fr_prot:'prev:int |ym:int, months:int|';
     fr_addr:@ym_prev;
     fr_desc:'get prev ym'
    )
  );

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lse_prepare(ER);
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'fpc ym module for lysee';
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_ym.rc}{$ENDIF}

begin

end.

