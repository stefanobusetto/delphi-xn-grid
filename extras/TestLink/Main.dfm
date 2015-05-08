object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 957
  ClientWidth = 1501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object xnGrid1: TxnGrid
    Left = 159
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
    ColCount = 5
    RowCount = 1
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
  object bt_append: TButton
    Left = 8
    Top = 143
    Width = 75
    Height = 25
    Caption = 'bt_append'
    TabOrder = 2
    OnClick = bt_appendClick
  end
  object Memo1: TMemo
    Left = 648
    Top = 8
    Width = 288
    Height = 440
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 39
    Width = 145
    Height = 98
    TabOrder = 4
    object bt_insert: TButton
      Left = 3
      Top = 3
      Width = 75
      Height = 25
      Caption = 'bt_insert'
      TabOrder = 0
      OnClick = bt_insertClick
    end
    object bt_delete: TButton
      Left = 3
      Top = 34
      Width = 75
      Height = 25
      Caption = 'bt_delete'
      TabOrder = 1
      OnClick = bt_deleteClick
    end
    object bt_edit: TButton
      Left = 3
      Top = 65
      Width = 75
      Height = 25
      Caption = 'bt_edit'
      TabOrder = 2
      OnClick = bt_editClick
    end
    object edIndex: TEdit
      Left = 84
      Top = 34
      Width = 53
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = '0'
    end
  end
  object Button1: TButton
    Left = 232
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 408
    Top = 480
    Width = 937
    Height = 473
    BorderWidth = 3
    Caption = 'Panel1'
    Color = clFuchsia
    ParentBackground = False
    TabOrder = 6
    StyleElements = [seFont, seBorder]
    object Image2: TImage
      Left = 2
      Top = 2
      Width = 545
      Height = 273
      AutoSize = True
    end
  end
end
