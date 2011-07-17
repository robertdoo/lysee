{==============================================================================}
{     PROJECT: lysee                                                           }
{ DESCRIPTION: library of lysee kernel (Delphi)                                }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/07/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
library lysee;

uses
  SysUtils,
  Classes,
  Windows,
  lseu in 'lseu.pas',
  lse_kernel in 'lse_kernel.pas';

{$R *.res}

function QueryEntry(const ID: pchar): pointer;cdecl;
begin
  Result := query_entry(ID);
end;

exports QueryEntry;

var
  saved_dll_proc: TDLLProc;

procedure lysee_dll_proc(event: integer);
begin
  try
    case event of
      DLL_PROCESS_ATTACH:;
      DLL_THREAD_ATTACH :;
      DLL_THREAD_DETACH :;
      DLL_PROCESS_DETACH:;
    end;
    if Assigned(saved_dll_proc) then
      saved_dll_proc(event);
  except
    { do nothing }
  end;
end;
 
begin
  saved_dll_proc := DLLProc;
  DLLProc := lysee_dll_proc;
end.
