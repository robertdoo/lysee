unit frmText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TTextForm = class(TForm)
    mText: TMemo;
    bOK: TButton;
    bCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TextForm: TTextForm;

function EditCommaText(const Title: string; var CommaText: string): boolean;
function EditText(const Title: string; var AText: string): boolean;

implementation

{$R *.dfm}

function EditCommaText(const Title: string; var CommaText: string): boolean;
begin
  if TextForm = nil then
    TextForm := TTextForm.Create(Application);
  with TextForm do
  begin
    Caption := Title;
    mText.Lines.CommaText := CommaText;
    Result := (ShowModal = mrOK);
    if Result then
      CommaText := mText.Lines.CommaText;
  end;
end;

function EditText(const Title: string; var AText: string): boolean;
begin
  if TextForm = nil then
    TextForm := TTextForm.Create(Application);
  with TextForm do
  begin
    Caption := Title;
    mText.Lines.Text := AText;
    Result := (ShowModal = mrOK);
  if Result then
    AText := mText.Lines.Text;
  end;
end;

end.
