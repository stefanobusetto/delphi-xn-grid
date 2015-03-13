object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 455
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object bt_create: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'create'
    TabOrder = 0
    OnClick = bt_createClick
  end
  object txt_pas: TMemo
    Left = 89
    Top = 8
    Width = 304
    Height = 441
    TabOrder = 1
  end
  object txt_csv: TMemo
    Left = 399
    Top = 8
    Width = 304
    Height = 441
    TabOrder = 2
  end
end
