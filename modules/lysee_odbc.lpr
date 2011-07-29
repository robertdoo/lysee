{==============================================================================}
{     PROJECT: lysee_odbc                                                      }
{ DESCRIPTION: ODBC database verdor (FPC)                                      }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_odbc;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pas',
  lse_dbu in '../lse_dbu.pas',
  lysee_odbc_funcs;

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lse_prepare(ER);
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'ODBC vendor for lysee';
  MR^.iw_types.cl_count := 2;
  MR^.iw_types.cl_entry :=@odbc_types;
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

begin

end.

