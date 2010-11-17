object SizeForm: TSizeForm
  Left = 193
  Top = 109
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = 'Form Settings'
  ClientHeight = 347
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Label5: TLabel
    Left = 0
    Top = 5
    Width = 54
    Height = 12
    Caption = 'Form Name'
  end
  object btnOK: TButton
    Left = 415
    Top = 0
    Width = 80
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 415
    Top = 64
    Width = 80
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnApply: TButton
    Left = 415
    Top = 32
    Width = 80
    Height = 23
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnApplyClick
  end
  object gHeight: TGroupBox
    Left = 2
    Top = 27
    Width = 200
    Height = 320
    Caption = ' &Rows '
    TabOrder = 1
    object lblFixedRows: TLabel
      Left = 14
      Top = 280
      Width = 60
      Height = 12
      Caption = 'Fixed Rows'
    end
    object veHeight: TValueListEditor
      Left = 13
      Top = 22
      Width = 172
      Height = 241
      DefaultColWidth = 112
      TabOrder = 0
      TitleCaptions.Strings = (
        'Row'
        'Height')
      ColWidths = (
        112
        54)
    end
    object eFixedRows: TSpinEdit
      Left = 80
      Top = 276
      Width = 105
      Height = 21
      MaxValue = 32
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
  end
  object gWidth: TGroupBox
    Left = 208
    Top = 27
    Width = 200
    Height = 320
    Caption = ' &Columns '
    TabOrder = 2
    object lblFixedCols: TLabel
      Left = 14
      Top = 280
      Width = 78
      Height = 12
      Caption = 'Fixed Columns'
    end
    object veWidth: TValueListEditor
      Left = 14
      Top = 22
      Width = 172
      Height = 241
      DefaultColWidth = 112
      TabOrder = 0
      TitleCaptions.Strings = (
        'Column'
        'Width')
      ColWidths = (
        112
        54)
    end
    object eFixedCols: TSpinEdit
      Left = 98
      Top = 276
      Width = 87
      Height = 21
      MaxValue = 32
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
  end
  object edtName: TEdit
    Left = 56
    Top = 1
    Width = 352
    Height = 20
    TabOrder = 0
  end
end
