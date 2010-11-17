unit frmEasyValues;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LiEasyForm, StdCtrls, ExtCtrls, ComCtrls;

type
  TEasyValuesForm = class(TForm)
    lvValues: TListView;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtName: TEdit;
    btnRemove: TButton;
    btnSave: TButton;
    Label2: TLabel;
    edtValue: TEdit;
    btnClose: TButton;
    btnClear: TButton;
    procedure lvValuesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure edtNameChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FValues: TEasyValues;
    FKey: string;
    FModified: boolean;
    FReadonly: boolean;
    function FindItem(const Name: string): TListItem;
    procedure RefreshItem(Item: TListItem; const Name, Value: string);
    procedure AddItem(const Name, Value: string);
    procedure Reselect;
  end;

var
  EasyValuesForm: TEasyValuesForm;

function EditValues(Values: TEasyValues; Readonly: boolean): boolean;

implementation

uses LiString, lse_msgbox, LiLML;

{$R *.dfm}

function EditValues(Values: TEasyValues; Readonly: boolean): boolean;
var
  index: integer;
  key, value: string;
begin
  with TEasyValuesForm.Create(Application) do
  try
    FReadonly := Readonly;
    FValues := Values;
    for index := 0 to FValues.Count - 1 do
    begin
      FValues.GetNameValue(index, key, value);
      AddItem(key, value);
    end;
    Reselect;
    btnClear.Enabled := not FReadonly;
    ShowModal;
    Result := FModified;
  finally
    Release;
  end;
end;

{ TEasyValuesForm }

procedure TEasyValuesForm.AddItem(const Name, Value: string);
begin
  RefreshItem(lvValues.Items.Add, Name, Value);
end;

procedure TEasyValuesForm.btnSaveClick(Sender: TObject);
var
  item: TListItem;
  value: string;
begin
  lock_cursor;
  try
    value := Trim(edtValue.Text);
    FValues.Write(FKey, value);
    FModified := true;
    item := FindItem(FKey);
    if item = nil then
      item := lvValues.Items.Add;
    RefreshItem(item, FKey, value);
    lvValues.Selected := item;
    Reselect;
  finally
    unlock_cursor;
  end;
end;

procedure TEasyValuesForm.btnClearClick(Sender: TObject);
begin
  if AnsYes('删除所有的参数吗？') then
  begin
    lock_cursor;
    try
      FValues.Clear;
      FModified := true;
      lvValues.Items.Clear;
      Reselect;
    finally
      unlock_cursor;
    end;
  end;
end;

procedure TEasyValuesForm.btnRemoveClick(Sender: TObject);
var
  item: TListItem;
begin
  item := lvValues.Selected;
  if item <> nil then
    if AnsYesFmt('删除参数[%s]吗？', [item.Caption]) then
    begin
      lock_cursor;
      try
        FValues.Remove(item.Caption);
        FModified := true;
        item.Delete;
        Reselect;
      finally
        unlock_cursor;
      end;
    end;
end;

procedure TEasyValuesForm.edtNameChange(Sender: TObject);
begin
  FKey := Trim(edtName.Text);
  btnSave.Enabled := not FReadonly and lml_is_tag_name(FKey);
end;

function TEasyValuesForm.FindItem(const Name: string): TListItem;
var
  index: integer;
begin
  if Name <> '' then
    for index := 0 to lvValues.Items.Count - 1 do
    begin
      Result := lvValues.Items[index];
      if AnsiSameText(Name, Result.Caption) then Exit;
    end;
  Result := nil;
end;

procedure TEasyValuesForm.lvValuesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  key: string;
begin
  if Selected then
  begin
    key := Item.Caption;
    edtName.Text := key;
    edtValue.Text := FValues.Values[key];
  end
  else
  begin
    edtName.Text := '';
    edtValue.Text := '';
  end;
  btnRemove.Enabled := not FReadonly and Selected;
  btnSave.Enabled := not FReadonly and lilml.lml_is_tag_name(FKey);
end;

procedure TEasyValuesForm.RefreshItem(Item: TListItem; const Name, Value: string);
begin
  Item.Caption := Name;
  Item.SubItems.Clear;
  Item.SubItems.Add(Value);
end;

procedure TEasyValuesForm.Reselect;
var
  item: TListItem;
begin
  item := lvValues.Selected;
  lvValuesSelectItem(lvValues, item, item <> nil);
  if item <> nil then
    item.MakeVisible(false);
end;

end.
