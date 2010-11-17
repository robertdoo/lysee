object InputBoxForm: TInputBoxForm
  Left = 272
  Top = 130
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = 'InputBoxForm'
  ClientHeight = 97
  ClientWidth = 280
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
  object lbPrompt: TLabel
    Left = 0
    Top = 0
    Width = 48
    Height = 12
    Caption = 'lbPrompt'
  end
  object bOK: TButton
    Left = 62
    Top = 72
    Width = 72
    Height = 25
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 146
    Top = 72
    Width = 72
    Height = 25
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 2
  end
  object eText: TMaskEdit
    Left = 0
    Top = 16
    Width = 280
    Height = 20
    TabOrder = 0
    OnChange = eTextChange
  end
  object cbFlag: TCheckBox
    Left = 0
    Top = 38
    Width = 280
    Height = 17
    TabOrder = 3
  end
end
