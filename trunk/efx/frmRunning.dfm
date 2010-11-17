object RunningForm: TRunningForm
  Left = 377
  Top = 321
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'RunningForm'
  ClientHeight = 65
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 336
    Height = 65
    TabOrder = 0
    object imgApp: TImage
      Left = 22
      Top = 16
      Width = 32
      Height = 32
    end
    object lblStatus: TLabel
      Left = 70
      Top = 24
      Width = 120
      Height = 12
      Caption = #27491#22312#36827#34892#25968#25454#26657#26680' ...'
    end
    object btnCancel: TButton
      Left = 246
      Top = 23
      Width = 75
      Height = 25
      Caption = #21462#28040
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
  object tmrWork: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrWorkTimer
    Left = 184
    Top = 16
  end
end
