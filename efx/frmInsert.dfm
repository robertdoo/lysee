object InsertForm: TInsertForm
  Left = 207
  Top = 116
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 12
  Caption = #25554#20837#20195#30721
  ClientHeight = 329
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 0
    Top = 308
    Width = 48
    Height = 12
    Caption = #26597#25214#20869#23481
  end
  object lvAppx: TListView
    Left = 0
    Top = 0
    Width = 562
    Height = 295
    Columns = <
      item
        Caption = #31867
        Width = 68
      end
      item
        Caption = #36820#22238#20540
        Width = 68
      end
      item
        Caption = #26041#27861#21644#23646#24615
        Width = 200
      end
      item
        Caption = #35828#26126
        Width = 200
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = lvAppxColumnClick
    OnCompare = lvAppxCompare
    OnDblClick = lvAppxDblClick
    OnSelectItem = lvAppxSelectItem
  end
  object btnNext: TButton
    Left = 232
    Top = 304
    Width = 75
    Height = 25
    Caption = #26597#25214#19979#19968#20010
    Enabled = False
    TabOrder = 2
    OnClick = eFindChange
  end
  object eFind: TEdit
    Left = 64
    Top = 304
    Width = 160
    Height = 20
    BevelInner = bvLowered
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    OnChange = eFindChange
  end
  object btnOK: TButton
    Left = 403
    Top = 304
    Width = 75
    Height = 25
    Caption = #30830#23450
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 487
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 4
  end
end
