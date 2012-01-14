{==============================================================================}
{        UNIT: lysee_ymd                                                       }
{ DESCRIPTION: year-month-day interger functions (FPC)                         }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_ymd;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  DateUtils,
  lseu in '../lseu.pas',
  lse_funcs in '../lse_funcs.pas';

procedure ymd_now(const invoker: TLseInvoke);cdecl;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  invoker.returnInt((y * 10000) + (m * 100) + d);
end;

procedure ymd_check(const invoker: TLseInvoke);cdecl;
var
  y, m, d: integer;
begin
  invoker.returnBool(__decodeYMD(invoker.paramInt(0), y, m, d));
end;

procedure ymd_next(const invoker: TLseInvoke);cdecl;
var
  n, ymd: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ymd_now(invoker);
    Exit;
  end;

  ymd := invoker.paramInt(0);
  if n = 1 then
    ymd := __nextYMD(ymd, 1) else
    ymd := __nextYMD(ymd, invoker.paramInt(1));

  invoker.returnInt(ymd);
end;

procedure ymd_prev(const invoker: TLseInvoke);cdecl;
var
  n, ymd: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ymd_now(invoker);
    Exit;
  end;

  ymd := invoker.paramInt(0);
  if n = 1 then
    ymd := __prevYMD(ymd, 1) else
    ymd := __prevYMD(ymd, invoker.paramInt(1));

  invoker.returnInt(ymd);
end;

const
  func_count = 4;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'now:int()';
     fr_addr:@ymd_now;
     fr_desc:'get current ymd'
    ),
    (fr_prot:'check:int(ymd:int)';
     fr_addr:@ymd_check;
     fr_desc:'check ymd'
    ),
    (fr_prot:'next:int(ymd:int, days:int)';
     fr_addr:@ymd_next;
     fr_desc:'get next ymd'
    ),
    (fr_prot:'prev:int(ymd:int, days:int)';
     fr_addr:@ymd_prev;
     fr_desc:'get prev ymd'
    )
  );

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lse_prepare(ER);
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'fpc ymd module for lysee';
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_ymd.rc}{$ENDIF}

begin

end.

