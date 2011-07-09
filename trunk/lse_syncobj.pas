{==============================================================================}
{        UNIT: lse_syncobj                                                     }
{ DESCRIPTION: syncronisizing objects                                          }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_syncobj;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs, lseu;

type

  { TLseSpinLock }

  TLseSpinLock = class(TLseLock)
  private
    FCriticalSection: SyncObjs.TCriticalSection;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Enter;override;
    procedure Leave;override;
    function TryEnter: boolean;override;
  end;

implementation

{ TLseSpinLock }

constructor TLseSpinLock.Create;
begin
  FCriticalSection := SyncObjs.TCriticalSection.Create;
end;

destructor TLseSpinLock.Destroy;
begin
  FreeAndNil(FCriticalSection);
end;

procedure TLseSpinLock.Enter;
begin
  FCriticalSection.Enter;
end;

procedure TLseSpinLock.Leave;
begin
  FCriticalSection.Leave;
end;

function TLseSpinLock.TryEnter: boolean;
begin
  {$IFDEF FPC}
  FCriticalSection.Enter;
  Result := true;
  {$ELSE}
  Result := FCriticalSection.TryEnter;
  {$ENDIF}
end;

end.

