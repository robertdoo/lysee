{==============================================================================}
{     PROJECT: lysee_math                                                      }
{ DESCRIPTION: mathmetics functions                                            }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_math;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pad',
  lysee_math_funcs in 'lysee_math_funcs.pas';

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lse_prepare(ER);
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'mathematic module for lysee';
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_math.rc}{$ENDIF}

begin
end.

