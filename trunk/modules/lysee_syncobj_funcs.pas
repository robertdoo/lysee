{==============================================================================}
{     PROJECT: lysee_syncobj_funcs                                             }
{ DESCRIPTION: syncronisizing objects and functions (FPC)                      }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2011/08/07                                                      }
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
  func_count = 4;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'lock_create:lock ||';
     fr_addr:@pp_lock_create;
     fr_desc:'create lock';
    ),
    (fr_prot:'lock_enter |sl:lock|';
     fr_addr:@pp_lock_lock;
     fr_desc:'lock';
    ),
    (fr_prot:'lock_tryEnter:int |sl:lock|';
     fr_addr:@pp_lock_trylock;
     fr_desc:'try lock';
    ),
    (fr_prot:'lock_leave |sl:lock|';
     fr_addr:@pp_lock_unlock;
     fr_desc:'unlock';
    )
  );

  syncobj_class_count = 1;

var
  syncobj_class_array: array[0..syncobj_class_count - 1] of RLseType = (
   (cr_type    : LSV_OBJECT;
    cr_name    :'lock';
    cr_desc    :'spinlock';
    cr_module  : nil;
    cr_addref  :@lse_addref_obj;
    cr_release :@lse_release_obj
    )
  );

function lock_type: PLseType;

implementation

function lock_type: PLseType;
begin
  Result := @syncobj_class_array[0];
end;

{ lock }

procedure pp_lock_create(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnObj(lock_type, TLseSpinLock.Create);
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

