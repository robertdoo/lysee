unit frmSize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, lse_efx, ComCtrls, ValEdit, Spin;

type
  TSizeForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    veHeight: TValueListEditor;
    gHeight: TGroupBox;
    gWidth: TGroupBox;
    veWidth: TValueListEditor;
    eFixedRows: TSpinEdit;
    lblFixedRows: TLabel;
    eFixedCols: TSpinEdit;
    lblFixedCols: TLabel;
    Label5: TLabel;
    edtName: TEdit;
    procedure btnApplyClick(Sender: TObject);
  private
  	FForm: TEasyForm;
    FUndoSaved: boolean;
  end;

var
  SizeForm: TSizeForm;

procedure ModalSize(AForm: TEasyForm);

implementation

uses
  Math;

{$R *.DFM}

procedure ModalSize(AForm: TEasyForm);
var
  A: integer;
  K: string;
begin
  with TSizeForm.Create(Application) do
  try
    FForm := AForm;
    edtName.Text := FForm.Name;

    if FForm.RowCount > 1 then
    begin
      eFixedRows.MaxValue := FForm.RowCount - 1;
      eFixedRows.Value := FForm.FixedRowCount;
    end
    else
    begin
      eFixedRows.Enabled := false;
      lblFixedRows.Enabled := false;
    end;
    
    for A := 0 to FForm.RowCount - 1 do
    begin
      K := IntToStr(A + 1);
      veHeight.Strings.Add(Format('%s=%d', [K, FForm.RowHeights[A]]));
    end;

    if FForm.ColCount > 1 then
    begin
      eFixedCols.MaxValue := FForm.ColCount - 1;
      eFixedCols.Value := FForm.FixedColCount;
    end
    else
    begin
      eFixedCols.Enabled := false;
      lblFixedCols.Enabled := false;
    end;

    for A := 0 to FForm.ColCount - 1 do
    begin
      K := IntToStr(A + 1);
      veWidth.Strings.Add(Format('%s=%d', [K, FForm.ColWidths[A]]));
      veWidth.ItemProps[K].KeyDesc := EncodeColumnID(A + 1);
    end;

    btnApply.Enabled := Assigned(AForm.View);
    FUndoSaved := false;
    ShowModal;
  finally
    Release;
  end;
end;

procedure TSizeForm.btnApplyClick(Sender: TObject);
var
  A: integer;
begin
  if not FUndoSaved and (FForm.View <> nil) then
  begin
    FForm.View.UndoSaveResize;
    FUndoSaved := true;
  end;

  FForm.BeginUpdate;
  try
    FForm.Name := Trim(edtName.Text);

    for A := 0 to FForm.RowCount - 1 do
    begin
      FForm.RowHeights[A] := Max(0, stoi(veHeight.Cells[1, A + 1]));
      veHeight.Cells[1, A + 1] := IntToStr(FForm.RowHeights[A]);
    end;
    
    if FForm.RowCount > 1 then
      FForm.FixedRowCount := eFixedRows.Value;

    for A := 0 to FForm.ColCount - 1 do
    begin
      FForm.ColWidths[A] := Max(0, stoi(veWidth.Cells[1, A + 1]));
      veWidth.Cells[1, A + 1] := IntToStr(FForm.ColWidths[A]);
    end;
    
    if FForm.ColCount > 1 then
      FForm.FixedColCount := eFixedCols.Value;
  finally
    FForm.EndUpdate;
  end;
end;

end.
