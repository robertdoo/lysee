object PageSetupForm: TPageSetupForm
  Left = 188
  Top = 184
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = #39029#38754#35774#32622
  ClientHeight = 210
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object GroupBox1: TGroupBox
    Left = 0
    Top = 118
    Width = 300
    Height = 92
    Caption = ' '#39029#36793#36317'('#27627#31859') '
    TabOrder = 1
    object eLeft: TLabeledEdit
      Left = 32
      Top = 22
      Width = 45
      Height = 20
      EditLabel.Width = 12
      EditLabel.Height = 12
      EditLabel.Caption = #24038
      LabelPosition = lpLeft
      TabOrder = 0
      Text = '0'
    end
    object eRight: TLabeledEdit
      Left = 130
      Top = 22
      Width = 45
      Height = 20
      EditLabel.Width = 12
      EditLabel.Height = 12
      EditLabel.Caption = #21491
      LabelPosition = lpLeft
      TabOrder = 1
      Text = '0'
    end
    object eTop: TLabeledEdit
      Left = 32
      Top = 54
      Width = 45
      Height = 20
      EditLabel.Width = 12
      EditLabel.Height = 12
      EditLabel.Caption = #19978
      LabelPosition = lpLeft
      TabOrder = 2
      Text = '0'
    end
    object eBottom: TLabeledEdit
      Left = 130
      Top = 54
      Width = 45
      Height = 20
      EditLabel.Width = 12
      EditLabel.Height = 12
      EditLabel.Caption = #19979
      LabelPosition = lpLeft
      TabOrder = 3
      Text = '0'
    end
    object eHorzCenter: TCheckBox
      Left = 213
      Top = 22
      Width = 72
      Height = 17
      Caption = #27700#24179#23621#20013
      TabOrder = 4
    end
    object eVertCenter: TCheckBox
      Left = 213
      Top = 54
      Width = 72
      Height = 17
      Caption = #22402#30452#23621#20013
      TabOrder = 5
    end
    object udLeft: TUpDown
      Left = 77
      Top = 22
      Width = 18
      Height = 20
      Associate = eLeft
      TabOrder = 6
    end
    object udRight: TUpDown
      Left = 175
      Top = 22
      Width = 18
      Height = 20
      Associate = eRight
      TabOrder = 7
    end
    object udTop: TUpDown
      Left = 77
      Top = 54
      Width = 18
      Height = 20
      Associate = eTop
      TabOrder = 8
    end
    object udBottom: TUpDown
      Left = 175
      Top = 54
      Width = 18
      Height = 20
      Associate = eBottom
      TabOrder = 9
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 300
    Height = 108
    Caption = ' '#25171#21360#26426#21644#25171#21360#32440' '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 26
      Width = 36
      Height = 12
      Caption = #25171#21360#26426
    end
    object Label2: TLabel
      Left = 16
      Top = 58
      Width = 36
      Height = 12
      Caption = #25171#21360#32440
    end
    object lsPrinter: TComboBox
      Left = 56
      Top = 22
      Width = 224
      Height = 20
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 0
      OnChange = lsPrinterChange
    end
    object lsPaper: TComboBox
      Left = 56
      Top = 54
      Width = 224
      Height = 20
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 1
    end
    object bPortrait: TRadioButton
      Left = 56
      Top = 78
      Width = 56
      Height = 17
      Caption = #32437#21521
      TabOrder = 2
    end
    object bLandscape: TRadioButton
      Left = 152
      Top = 78
      Width = 56
      Height = 17
      Caption = #27178#21521
      TabOrder = 3
    end
  end
  object bOK: TButton
    Left = 308
    Top = 0
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 308
    Top = 32
    Width = 75
    Height = 25
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 3
  end
end
