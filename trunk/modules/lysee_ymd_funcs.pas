{==============================================================================}
{        UNIT: lysee_ymd_funcs                                                 }
{ DESCRIPTION: year-month-day interger functions (FPC)                         }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2007-2010, Li Yun Jie                                          }
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
{ Portions created by Li Yun Jie are Copyright (C) 2007-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_ymd_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu;

procedure ymd_now(invoker: TLseInvoke);cdecl;
procedure ymd_check(invoker: TLseInvoke);cdecl;
procedure ymd_next(invoker: TLseInvoke);cdecl;
procedure ymd_prev(invoker: TLseInvoke);cdecl;

const
  ymd_func_count = 4;
  ymd_func_array: array[0..ymd_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int now()';
     fr_addr:@ymd_now;
     fr_desc:'get current ymd'
    ),
    (fr_prot:'bool check(int ymd)';
     fr_addr:@ymd_check;
     fr_desc:'check ymd'
    ),
    (fr_prot:'int next(int ymd, int days)';
     fr_addr:@ymd_next;
     fr_desc:'get next ymd'
    ),
    (fr_prot:'int prev(int ymd, int days)';
     fr_addr:@ymd_prev;
     fr_desc:'get prev ymd'
    )
  );

implementation

uses
  DateUtils, lse_funcs;

procedure ymd_now(invoker: TLseInvoke);cdecl;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  invoker.returnInt((y * 10000) + (m * 100) + d);
end;

procedure ymd_check(invoker: TLseInvoke);cdecl;
var
  y, m, d: integer;
begin
  invoker.returnBool(__decodeYMD(invoker.paramInt(0), y, m, d));
end;

procedure ymd_next(invoker: TLseInvoke);cdecl;
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

procedure ymd_prev(invoker: TLseInvoke);cdecl;
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

end.

