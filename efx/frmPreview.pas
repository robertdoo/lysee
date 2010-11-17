unit frmPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, ExtCtrls, lse_efx, StdCtrls, ImgList,
  Buttons, Menus, efxPreview;

type
  TPreviewForm = class(TForm)
    Previewer: TPreviewFrame;
    dlgSave: TSaveDialog;
    procedure FormClose(Sender: TObject);
    procedure FormEdit(Sender: TObject; book: TEasyForm);
    procedure FormSave(Sender: TObject; var fname: string);
  private
  end;

var
  PreviewForm: TPreviewForm;
  PreviewEdit: TEfxEditBook;
  PreviewRatio: integer = 100;

procedure preview(book: TEasyForm; Index: integer);
procedure previewAndFree(var book: TEasyForm; Index: integer);

implementation

uses
	Printers, Math, frmSetup, lse_devcaps, WinSpool, CommDlg, lse_msgbox;

procedure preview(book: TEasyForm; Index: integer);
begin
  with TPreviewForm.Create(Application) do
  try
    Previewer.Source := book;
    Previewer.PageIndex := 0;
    Previewer.OnClose := FormClose;
    if Assigned(PreviewEdit) then
      Previewer.OnEdit := FormEdit;
    Previewer.OnSave := FormSave;
    Previewer.Ratio := PreviewRatio;
    ShowModal;
    PreviewRatio := Previewer.Ratio;
  finally
    Release;
  end;
end;

procedure previewAndFree(var book: TEasyForm; Index: integer);
begin
  if Assigned(book) then
  try
    preview(book, Index);
  finally
    FreeAndNil(book);
  end;
end;

{$R *.DFM}


{ TPreviewForm }

procedure TPreviewForm.FormClose(Sender: TObject);
begin
  Close;
end;

procedure TPreviewForm.FormEdit(Sender: TObject; book: TEasyForm);
begin
  PreviewEdit(Self, book);
end;

procedure TPreviewForm.FormSave(Sender: TObject; var fname: string);
begin
  if dlgSave.Execute then
    fname := dlgSave.FileName;
end;

end.
