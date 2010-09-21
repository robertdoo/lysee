program lysee_package_app;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, utest, lysee_package, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R lysee_package_app.rc}{$ENDIF}

begin
  {$I lysee_package_app.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

