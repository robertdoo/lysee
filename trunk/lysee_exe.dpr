{==============================================================================}
{     PROJECT: lysee_exe                                                       }
{ DESCRIPTION: lysee script interpreter (Delphi)                               }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/07/07                                                      }
{==============================================================================}
program lysee_exe;

{$APPTYPE CONSOLE}

uses
  lseu in 'lseu.pas',
  lse_console in 'lse_console.pas',
  lse_kernel in 'lse_kernel.pas',
  lse_export in 'lse_export.pas',
  lse_symbol in 'lse_symbol.pas',
  lse_funcs in 'lse_funcs.pas',
  lse_api in 'lse_api.pas',
  lse_cgi in 'lse_cgi.pas',
  lse_syncobj in 'lse_syncobj.pas',
  lse_patten in 'lse_patten.pas';

begin
  lse_console.Execute;
end.
