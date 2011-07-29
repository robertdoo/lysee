{==============================================================================}
{     PROJECT: lysee_syncobj                                                   }
{ DESCRIPTION: syncronisizing objects and functions (FPC)                      }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee_syncobj;

{$mode objfpc}{$H+}

uses
  Classes,
  lseu in '../lseu.pas',
  lse_syncobj in '../lse_syncobj.pas',
  lysee_syncobj_funcs;

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
  lse_prepare(ER);
  MR^.iw_version        := LSE_VERSION;
  MR^.iw_desc           := 'syncronisizing object module for lysee';
  MR^.iw_types.cl_count := syncobj_class_count;
  MR^.iw_types.cl_entry :=@syncobj_class_array;
  MR^.iw_funcs.fl_count := func_count;
  MR^.iw_funcs.fl_entry :=@func_array;
  MR^.iw_invoke         :=@lse_call_gate;
end;

exports
  InitExchange;

{$IFDEF WINDOWS}{$R lysee_syncobj.rc}{$ENDIF}

begin

end.

