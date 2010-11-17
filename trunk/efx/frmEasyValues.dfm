object EasyValuesForm: TEasyValuesForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = #34920#26684#38468#21152#21442#25968
  ClientHeight = 329
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object lvValues: TListView
    Left = 0
    Top = 0
    Width = 425
    Height = 230
    Columns = <
      item
        Caption = #21517#31216
        Width = 100
      end
      item
        Caption = #24403#21069#20540
        Width = 300
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = lvValuesSelectItem
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 236
    Width = 425
    Height = 93
    Caption = ' '#24403#21069#21442#25968'(&V) '
    TabOrder = 1
    object Label1: TLabel
      Left = 26
      Top = 28
      Width = 24
      Height = 12
      Caption = #21517#31216
    end
    object Label2: TLabel
      Left = 14
      Top = 54
      Width = 36
      Height = 12
      Caption = #24403#21069#20540
    end
    object edtName: TEdit
      Left = 56
      Top = 24
      Width = 348
      Height = 20
      TabOrder = 0
      OnChange = edtNameChange
    end
    object edtValue: TEdit
      Left = 56
      Top = 50
      Width = 348
      Height = 20
      TabOrder = 1
    end
  end
  object btnRemove: TButton
    Left = 431
    Top = 31
    Width = 75
    Height = 25
    Caption = #21024#38500
    Enabled = False
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnSave: TButton
    Left = 431
    Top = 241
    Width = 75
    Height = 25
    Caption = #20445#23384
    Enabled = False
    TabOrder = 5
    OnClick = btnSaveClick
  end
  object btnClose: TButton
    Left = 431
    Top = 0
    Width = 75
    Height = 25
    Cancel = True
    Caption = #20851#38381
    ModalResult = 2
    TabOrder = 2
  end
  object btnClear: TButton
    Left = 431
    Top = 62
    Width = 75
    Height = 25
    Caption = #28165#31354
    Enabled = False
    TabOrder = 4
    OnClick = btnClearClick
  end
end
