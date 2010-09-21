program testlp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  LResources,
  frmTestlp;

{$IFDEF WINDOWS}{$R testlp.rc}{$ENDIF}

begin
  {$I testlp.lrs}
  Application.Initialize;
  Application.CreateForm(TTestlpForm, TestlpForm);
  Application.Run;
end.

