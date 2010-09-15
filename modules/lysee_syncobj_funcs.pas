{==============================================================================}
{     PROJECT: lysee_syncobj_funcs                                             }
{ DESCRIPTION: syncronisizing objects and functions (FPC)                      }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2010/09/01                                                      }
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
unit lysee_syncobj_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu, lse_syncobj;

{ lock }

procedure pp_lock_create(const invoker: TLseInvoke);cdecl;
procedure pp_lock_lock(const invoker: TLseInvoke);cdecl;
procedure pp_lock_trylock(const invoker: TLseInvoke);cdecl;
procedure pp_lock_unlock(const invoker: TLseInvoke);cdecl;

const
  lock_func_count = 4;
  lock_func_array: array[0..lock_func_count - 1] of RLseFuncRec = (
    (fr_prot:'lock lock()';
     fr_addr:@pp_lock_create;
     fr_desc:'create lock';
    ),
    (fr_prot:'void enter()';
     fr_addr:@pp_lock_lock;
     fr_desc:'lock';
    ),
    (fr_prot:'bool tryEnter()';
     fr_addr:@pp_lock_trylock;
     fr_desc:'try lock';
    ),
    (fr_prot:'void leave()';
     fr_addr:@pp_lock_unlock;
     fr_desc:'unlock';
    )
  );

  syncobj_class_count = 1;
  syncobj_class_array: array[0..syncobj_class_count - 1] of RLseClassRec = (
   (vtype      : LSV_OBJECT;
    name       : 'lock';
    desc       : 'lock';
    incRefcount:@lse_incRefcount;
    decRefcount:@lse_decRefcount;
    funcs      : (count:lock_func_count; entry:@lock_func_array)
    )
  );

function lock_classrec: PLseClassRec;

implementation

function lock_classrec: PLseClassRec;
begin
  Result := @syncobj_class_array[0];
end;

{ lock }

procedure pp_lock_create(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnObj(lock_classrec, TLseSpinLock.Create);
end;

procedure pp_lock_lock(const invoker: TLseInvoke);cdecl;
var
  this: TLseLock;
begin
  if invoker.GetThis(this) then
    this.Enter;
end;

procedure pp_lock_trylock(const invoker: TLseInvoke);cdecl;
var
  this: TLseLock;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.TryEnter);
end;

procedure pp_lock_unlock(const invoker: TLseInvoke);cdecl;
var
  this: TLseLock;
begin
  if invoker.GetThis(this) then
    this.Leave;
end;

end.

