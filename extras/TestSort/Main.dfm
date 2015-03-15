object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 551
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object bt_QuickSort: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'QuickSort'
    TabOrder = 0
    OnClick = bt_QuickSortClick
  end
  object memo_2: TMemo
    Left = 89
    Top = 8
    Width = 425
    Height = 534
    Lines.Strings = (
      'Memo2')
    TabOrder = 1
  end
  object bt_seek1: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Caption = 'seek1'
    TabOrder = 2
    OnClick = bt_seek1Click
  end
  object bt_seek2: TButton
    Left = 8
    Top = 119
    Width = 75
    Height = 25
    Caption = 'seek2'
    TabOrder = 3
    OnClick = bt_seek2Click
  end
end
