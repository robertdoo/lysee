{==============================================================================}
{     PROJECT: lysee                                                           }
{ DESCRIPTION: library of lysee kernel (Delphi)                                }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2010/08/31                                                      }
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
library lysee;

uses
  SysUtils,
  Classes,
  Windows,
  lseu in 'lseu.pas',
  lse_kernel in 'lse_kernel.pas',
  lse_export in 'lse_export.pas',
  lse_symbol in 'lse_symbol.pas',
  lse_funcs in 'lse_funcs.pas',
  lse_api in 'lse_api.pas',
  lse_spawn in 'lse_spawn.pas',
  lse_cgi in 'lse_cgi.pas',
  lse_syncobj in 'lse_syncobj.pas';

{$R *.res}

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
