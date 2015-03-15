object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 575
  ClientWidth = 987
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object xnGrid1: TxnGrid
    Left = 96
    Top = 8
    Width = 483
    Height = 273
    Columns = <
      item
        Alignment = taLeftJustify
        Width = 0
      end
      item
        Alignment = taLeftJustify
        Width = 0
      end
      item
        Alignment = taLeftJustify
        Width = 0
      end
      item
        Alignment = taLeftJustify
        Width = 0
      end>
    OptionsEditing = False
  end
  object bt_fill: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'bt_fill'
    TabOrder = 1
    OnClick = bt_fillClick
  end
  object bt_insert: TButton
    Left = 8
    Top = 63
    Width = 75
    Height = 25
    Caption = 'bt_insert'
    TabOrder = 2
    OnClick = bt_insertClick
  end
  object bt_delete: TButton
    Left = 8
    Top = 94
    Width = 75
    Height = 25
    Caption = 'bt_delete'
    TabOrder = 3
    OnClick = bt_deleteClick
  end
  object bt_append: TButton
    Left = 8
    Top = 125
    Width = 75
    Height = 25
    Caption = 'bt_append'
    TabOrder = 4
    OnClick = bt_appendClick
  end
  object bt_edit: TButton
    Left = 8
    Top = 156
    Width = 75
    Height = 25
    Caption = 'bt_edit'
    TabOrder = 5
    OnClick = bt_editClick
  end
end
