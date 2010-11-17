object TextForm: TTextForm
  Left = 400
  Top = 198
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = #32534#36753#21333#20803#26684#25991#26412
  ClientHeight = 280
  ClientWidth = 443
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
  object mText: TMemo
    Left = 0
    Top = 0
    Width = 360
    Height = 280
    Lines.Strings = (
      'mText')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object bOK: TButton
    Left = 368
    Top = 0
    Width = 75
    Height = 25
    Caption = #30830#23450
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 368
    Top = 32
    Width = 75
    Height = 25
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 2
  end
end
