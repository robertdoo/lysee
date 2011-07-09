{==============================================================================}
{     PROJECT: lysee_sh                                                        }
{ DESCRIPTION: shell functions of current OS (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_sh;

{$mode objfpc}{$H+}

uses
  lseu in '../lseu.pas',
  lse_spawn in '../lse_spawn.pas',
  lysee_sh_funcs;

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lseu.lse_entries      := ER;
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'shell utils module for lysee';
  MR^.iw_types.cl_count := 1;
  MR^.iw_types.cl_entry :=@searcher_type;
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;
  
{$IFDEF WINDOWS}{$R lysee_sh.rc}{$ENDIF}

begin
end.

