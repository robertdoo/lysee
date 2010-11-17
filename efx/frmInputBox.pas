unit frmInputBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, lseu;

type
  TInputOption = (ioAutoTrim, ioAllowNullString, ioPassword);
  TInputOptions = set of TInputOption;

  TInputBoxForm = class(TForm)
    lbPrompt: TLabel;
    bOK: TButton;
    Button2: TButton;
    eText: TMaskEdit;
    cbFlag: TCheckBox;
    procedure eTextChange(Sender: TObject);
  private
    FOptions: TInputOptions;
    FResult: string;
  end;

var
  InputBoxForm: TInputBoxForm;
  InputFlag: boolean;

function GetInputTextEx(const Title, Prompt, Flag, Mask: string;
  var Value: string; Options: TInputOptions): boolean;

function InputText(const Title, Prompt: string; var Text: string): boolean;
function InputName(const Title, Prompt: string; var Name: string): boolean;
function InputPath(const Title, Prompt, Flag: string; var Path: string; MustExists: boolean = true): boolean;
function InputCopyPath(var Path: string): boolean;
function InputYear(const Title, Prompt: string; var Year: string): boolean;
function InputPassword(const Title, Prompt: string; var Value: string): boolean;
function InputIP(const Title, Prompt: string; var IP: string): boolean;

implementation

uses
  lse_msgbox, lse_efx;

function GetInputTextEx(const Title, Prompt, Flag, Mask: string;
  var Value: string; Options: TInputOptions): boolean;
begin
  with TInputBoxForm.Create(Application) do
  try
    FOptions := Options;
    eText.Text := '';
    if ioPassword in FOptions then
      eText.PasswordChar := '*';
    if Mask <> '' then
      eText.EditMask := Mask;
    eText.Text := Value;
    cbFlag.Checked := false;
    cbFlag.Caption := Flag;
    cbFlag.Visible := Trim(Flag) <> '';
    eTextChange(nil);
    lbPrompt.Caption := Prompt;
    Caption := Title;
    Result := (ShowModal = mrOK);
    if Result then
    begin
      Value := FResult;
      InputFlag := cbFlag.Checked;
    end;
  finally
    Release;
  end;
end;

function InputText(const Title, Prompt: string; var Text: string): boolean;
begin
  Result := GetInputTextEx(Title, Prompt, '', '', Text, []);
end;

function InputName(const Title, Prompt: string; var Name: string): boolean;
begin
  Result := GetInputTextEx(Title, Prompt, '', '', Name, [ioAutoTrim]);
end;

function InputPath(const Title, Prompt, Flag: string; var Path: string; MustExists: boolean): boolean;
begin
  Result := GetInputTextEx(Title, Prompt, Flag, '', Path, [ioAutoTrim]);
  if Result then
  begin
    Path := lse_expand_fname(Path);
    if MustExists then
    begin
      Result := DirectoryExists(Path);
      if not Result then
        MsgErrFmt('路径“%s”不存在！', [Path]);
    end;
  end;
end;

function InputCopyPath(var Path: string): boolean;
const
  H = '请输入拷盘目录 (“A:\”代表软驱)';
var
  P: string;
begin
  if Trim(Path) = '' then P := 'A:\' else P := Path;
  Result := InputPath('选择拷盘目录', H, '', P, false);
  if Result then Path := P;
end;

function InputYear(const Title, Prompt: string; var Year: string): boolean;
var
  S: string;
begin
  S := Format('%4d', [stoi(pchar(Year)) mod 10000]);
  Result := GetInputTextEx(Title, Prompt, '', '9999;0; ', S, []);
  if Result then Year := S;
end;

function InputPassword(const Title, Prompt: string; var Value: string): boolean;
begin
  Result := GetInputTextEx(Title, Prompt, '', '', Value, [ioPassword]);
end;

function InputIP(const Title, Prompt: string; var IP: string): boolean;
begin
  Result := GetInputTextEx(Title, Prompt, '', '', IP, [ioAutoTrim]);
end;

{$R *.dfm}

procedure TInputBoxForm.eTextChange(Sender: TObject);
begin
  FResult := eText.Text;
  if ioAutoTrim in FOptions then
    FResult := Trim(FResult);
  bOK.Enabled := (ioAllowNullString in FOptions) or
    (FResult <> '');
end;

end.
