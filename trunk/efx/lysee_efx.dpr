program lysee_efx;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  frmPreview in 'frmPreview.pas' {PreviewForm},
  frmRunning in 'frmRunning.pas' {RunningForm},
  efxPreview in 'efxPreview.pas' {PreviewFrame: TFrame},
  frmSize in 'frmSize.pas' {SizeForm},
  frmLsyn in 'frmLsyn.pas' {LsynFrame: TFrame},
  frmSetup in 'frmSetup.pas' {PageSetupForm},
  frmText in 'frmText.pas' {TextForm},
  frmInputBox in 'frmInputBox.pas' {InputBoxForm},
  frmError in 'frmError.pas' {ErrorForm},
  frmInsert in 'frmInsert.pas' {InsertForm},
  frmStyle in 'frmStyle.pas' {StyleForm},
  frmAbout in 'frmAbout.pas' {AboutBox},
  lseu in '..\lseu.pas',
  lse_efx in '..\lse_efx.pas',
  lse_api in '..\lse_api.pas',
  lse_cgi in '..\lse_cgi.pas',
  lse_export in '..\lse_export.pas',
  lse_funcs in '..\lse_funcs.pas',
  lse_kernel in '..\lse_kernel.pas',
  lse_patten in '..\lse_patten.pas',
  lse_spawn in '..\lse_spawn.pas',
  lse_symbol in '..\lse_symbol.pas',
  lse_syncobj in '..\lse_syncobj.pas',
  lse_devcaps in 'lse_devcaps.pas',
  lse_msgbox in '..\lse_msgbox.pas';

{$R *.RES}

begin
  lse_startup;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  lse_cleanup;
end.
