{==============================================================================}
{        UNIT: lysee_ym_funcs                                                  }
{ DESCRIPTION: year-month interger functions (FPC)                             }
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
unit lysee_ym_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu;

procedure ym_now(invoker: TLseInvoke);cdecl;
procedure ym_check(invoker: TLseInvoke);cdecl;
procedure ym_next(invoker: TLseInvoke);cdecl;
procedure ym_prev(invoker: TLseInvoke);cdecl;

const
  ym_func_count = 4;
  ym_func_array: array[0..ym_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int now()';
     fr_addr:@ym_now;
     fr_desc:'get current ym'
    ),
    (fr_prot:'bool check(int ym)';
     fr_addr:@ym_check;
     fr_desc:'check ym'
    ),
    (fr_prot:'int next(int ym, int months)';
     fr_addr:@ym_next;
     fr_desc:'get next ym'
    ),
    (fr_prot:'int prev(int ym, int months)';
     fr_addr:@ym_prev;
     fr_desc:'get prev ym'
    )
  );

implementation

uses
  DateUtils, lse_funcs;

procedure ym_now(invoker: TLseInvoke);cdecl;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  invoker.returnInt((y * 100) + m);
end;

procedure ym_check(invoker: TLseInvoke);cdecl;
var
  y, m: integer;
begin
  invoker.returnBool(__decodeYM(invoker.paramInt(0), y, m));
end;

procedure ym_next(invoker: TLseInvoke);cdecl;
var
  n, ym: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ym_now(invoker);
    Exit;
  end;

  ym := invoker.paramInt(0);
  if n = 1 then
    ym := __nextYM(ym, 1) else
    ym := __nextYM(ym, invoker.paramInt(1));

  invoker.returnInt(ym);
end;

procedure ym_prev(invoker: TLseInvoke);cdecl;
var
  n, ym: integer;
begin
  n := invoker.ParamCount;
  if n = 0 then
  begin
    ym_now(invoker);
    Exit;
  end;

  ym := invoker.paramInt(0);
  if n = 1 then
    ym := __prevYM(ym, 1) else
    ym := __prevYM(ym, invoker.paramInt(1));

  invoker.returnInt(ym);
end;

end.

