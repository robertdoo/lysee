{==============================================================================}
{     PROJECT: lysee_pad_fpc                                                   }
{ DESCRIPTION: lysee script file editing PAD (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2011/07/30                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_pad_fpc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}cthreads,{$ENDIF}
  {$ENDIF}
  Interfaces,
  Forms,
  lseu,
  lse_synedit,
  lse_kernel,
  lse_msgbox,
  lse_pad_fpc,
  lse_about_fpc,
  lse_pad_open_fpc;

{$IFDEF WINDOWS}
{$R lysee_pad_fpc.rc}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TLspadForm, LspadForm);
  Application.Run;
end.

