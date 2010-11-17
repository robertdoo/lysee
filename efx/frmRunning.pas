unit frmRunning;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, lseu;

type
  TRunningForm = class(TForm)
    Panel1: TPanel;
    imgApp: TImage;
    lblStatus: TLabel;
    btnCancel: TButton;
    tmrWork: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tmrWorkTimer(Sender: TObject);
  private
    FEngine: TLseEngine;
    FScript: string;
    procedure OnBeginExec(Sender: TObject);
    procedure OnEndExec(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RunningForm: TRunningForm;

procedure ExecScript(const Title, Script: string);

implementation

uses
  lse_msgbox;

{$R *.dfm}

procedure ExecScript(const Title, Script: string);
begin
  if TrimRight(Script) <> '' then
    with TRunningForm.Create(Application) do
    try
      imgApp.Picture.Icon.Assign(Application.Icon);
      lblStatus.Caption := Format('正在进行%s ...', [Title]);
      FScript := Script;
      tmrWork.Enabled := true;
      ShowModal;
    finally
      Release;
    end;
end;

procedure TRunningForm.FormCreate(Sender: TObject);
begin
  FEngine := TLseEngine.Create(Self);
  FEngine.OnExecuting := OnBeginExec;
  FEngine.OnExecuted := OnEndExec;
end;

procedure TRunningForm.btnCancelClick(Sender: TObject);
begin
  if Assigned(FEngine) then
    FEngine.Terminate;
end;

procedure TRunningForm.tmrWorkTimer(Sender: TObject);
begin
  if Assigned(FEngine) then
  begin
    tmrWork.Enabled := false;
    tmrWork.OnTimer := nil;
    if not FEngine.ExecuteCode(FScript, false) then
      lse_msgbox.MsgErr(FEngine.Error);
  end;
  Close;
end;

procedure TRunningForm.OnBeginExec(Sender: TObject);
begin
  lse_msgbox.lock_cursor;
end;

procedure TRunningForm.OnEndExec(Sender: TObject);
begin
  lse_msgbox.unlock_cursor;
end;

end.
