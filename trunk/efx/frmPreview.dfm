object PreviewForm: TPreviewForm
  Left = 204
  Top = 196
  Caption = #25171#21360#39044#35272
  ClientHeight = 371
  ClientWidth = 632
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
  inline Previewer: TPreviewFrame
    Left = 0
    Top = 0
    Width = 632
    Height = 371
    Align = alClient
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 632
    ExplicitHeight = 371
    inherited ClientPanel: TPanel
      Width = 632
      Height = 328
      ExplicitWidth = 632
      ExplicitHeight = 328
      inherited ViewBox: TScrollBox
        Width = 632
        Height = 328
        ExplicitWidth = 632
        ExplicitHeight = 328
      end
    end
    inherited ToolBar: TToolBar
      Width = 632
      ExplicitWidth = 632
      inherited btnFirst: TToolButton
        ExplicitWidth = 37
      end
      inherited btnPrev: TToolButton
        Left = 37
        ExplicitLeft = 37
        ExplicitWidth = 37
      end
      inherited btnNext: TToolButton
        Left = 74
        ExplicitLeft = 74
        ExplicitWidth = 37
      end
      inherited btnLast: TToolButton
        Left = 111
        ExplicitLeft = 111
        ExplicitWidth = 37
      end
      inherited ToolButton1: TToolButton
        Left = 148
        ExplicitLeft = 148
      end
      inherited cbxRatio: TComboBox
        Left = 156
        Top = 1
        Height = 20
        ItemHeight = 12
        ExplicitLeft = 156
        ExplicitTop = 1
        ExplicitHeight = 20
      end
      inherited btnZoomIn: TToolButton
        Left = 212
        ExplicitLeft = 212
        ExplicitWidth = 37
      end
      inherited btnZoomOut: TToolButton
        Left = 249
        ExplicitLeft = 249
        ExplicitWidth = 37
      end
      inherited ToolButton2: TToolButton
        Left = 286
        ExplicitLeft = 286
      end
      inherited btnSetup: TToolButton
        Left = 294
        ExplicitLeft = 294
      end
      inherited btnPrint: TToolButton
        Left = 349
        ExplicitLeft = 349
      end
      inherited btnEdit: TToolButton
        Left = 417
        ExplicitLeft = 417
      end
      inherited btnSave: TToolButton
        Left = 472
        ExplicitLeft = 472
      end
      inherited btnClose: TToolButton
        Left = 527
        ExplicitLeft = 527
      end
    end
    inherited StatusBar: TStatusBar
      Top = 350
      Width = 632
      ExplicitTop = 350
      ExplicitWidth = 632
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'ef'
    Filter = 'LML'#34920#26684' (*.ef)|*.ef|XML'#34920#26684' (*.efx)|*.efx|XLS'#34920#26684' (*.xls)|*.xls'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 345
    Top = 32
  end
end
