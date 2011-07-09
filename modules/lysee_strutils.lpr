{==============================================================================}
{     PROJECT: lysee_strutils                                                  }
{ DESCRIPTION: string utility functions (FPC)                                  }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_strutils;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pas',
  lse_funcs in '../lse_funcs.pas',
  lysee_strutils_funcs;

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lseu.lse_entries      := ER;
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'string utilities module for lysee';
  MR^.iw_types.cl_count := 2;
  MR^.iw_types.cl_entry :=@strutils_types;
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_strutils.rc}{$ENDIF}

begin

end.

