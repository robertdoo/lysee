program lysee_efx;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  lse_efx in 'lse_efx.pas',
  frmPreview in 'frmPreview.pas' {PreviewForm},
  frmRunning in 'frmRunning.pas' {RunningForm},
  efxPreview in 'efxPreview.pas' {PreviewFrame: TFrame},
  frmSize in 'frmSize.pas' {SizeForm},
  frmLsyn in 'frmLsyn.pas' {LsynFrame: TFrame},
  frmEfxMacro in 'frmEfxMacro.pas' {EfxMacroFrame: TFrame},
  frmSetup in 'frmSetup.pas' {PageSetupForm},
  frmText in 'frmText.pas' {TextForm},
  frmInputBox in 'frmInputBox.pas' {InputBoxForm},
  frmError in 'frmError.pas' {ErrorForm},
  frmInsert in 'frmInsert.pas' {InsertForm},
  frmStyle in 'frmStyle.pas' {StyleForm},
  frmAbout in 'frmAbout.pas' {AboutBox},
  lseu in '..\lysee\lysee\lseu.pas',
  lse_api in '..\lysee\lysee\lse_api.pas',
  lse_cgi in '..\lysee\lysee\lse_cgi.pas',
  lse_export in '..\lysee\lysee\lse_export.pas',
  lse_funcs in '..\lysee\lysee\lse_funcs.pas',
  lse_kernel in '..\lysee\lysee\lse_kernel.pas',
  lse_patten in '..\lysee\lysee\lse_patten.pas',
  lse_spawn in '..\lysee\lysee\lse_spawn.pas',
  lse_symbol in '..\lysee\lysee\lse_symbol.pas',
  lse_syncobj in '..\lysee\lysee\lse_syncobj.pas',
  lse_devcaps in 'lse_devcaps.pas',
  lse_msgbox in '..\lysee\lysee\lse_msgbox.pas';

{$R *.RES}

begin
  lse_startup;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  lse_cleanup;
end.
