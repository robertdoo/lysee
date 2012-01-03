{==============================================================================}
{     PROJECT: lysee_exe                                                       }
{ DESCRIPTION: lysee script interpreter (Delphi)                               }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/12/05                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_exe;

{$APPTYPE CONSOLE}

uses
  lseu in 'lseu.pas',
  lse_console in 'lse_console.pas',
  lse_kernel in 'lse_kernel.pas';

begin
  lse_console.Execute;
end.
