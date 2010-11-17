program lazefx;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  LResources,
  lseu,
  frmlazefx;

{$IFDEF WINDOWS}{$R lazefx.rc}{$ENDIF}

begin
  {$I lazefx.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

