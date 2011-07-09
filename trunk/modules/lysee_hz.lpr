{==============================================================================}
{     PROJECT: lysee_hz                                                        }
{ DESCRIPTION: chinese word                                                    }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_hz;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pas',
  lysee_hz_funcs;

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lseu.lse_entries      := ER;
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'fpc chinese hz module for lysee';
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_hz.rc}{$ENDIF}

begin

end.

