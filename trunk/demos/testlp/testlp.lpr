program testlp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, LResources, frmTestlp, lse_syncobj, lse_api, lse_cgi,
  lse_export, lse_funcs, lse_kernel, lse_patten, lse_spawn, lse_symbol;

{$IFDEF WINDOWS}{$R testlp.rc}{$ENDIF}

begin
  {$I testlp.lrs}
  Application.Initialize;
  Application.CreateForm(TTestlpForm, TestlpForm);
  Application.Run;
end.

