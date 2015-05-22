object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 671
  ClientWidth = 1417
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
  object xnGridImage: TImage
    Left = 843
    Top = 24
    Width = 184
    Height = 513
  end
  object DbGridImage: TImage
    Left = 1033
    Top = 24
    Width = 184
    Height = 513
  end
  object DiffImage: TImage
    Left = 1223
    Top = 24
    Width = 184
    Height = 513
  end
  object Label1: TLabel
    Left = 843
    Top = 8
    Width = 31
    Height = 13
    Caption = 'xnGrid'
  end
  object Label2: TLabel
    Left = 1033
    Top = 5
    Width = 32
    Height = 13
    Caption = 'DbGrid'
  end
  object Label3: TLabel
    Left = 1223
    Top = 5
    Width = 55
    Height = 13
    Caption = 'Differences'
  end
  object DiffCount: TLabel
    Left = 843
    Top = 561
    Width = 46
    Height = 13
    Caption = 'DiffCount'
  end
  object xnGrid1_RecNo: TLabel
    Left = 159
    Top = 559
    Width = 142
    Height = 25
    Caption = 'xnGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DbGrid1_RecNo: TLabel
    Left = 349
    Top = 559
    Width = 146
    Height = 25
    Caption = 'DbGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object xnGrid1_RecCount: TLabel
    Left = 159
    Top = 590
    Width = 142
    Height = 25
    Caption = 'xnGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DbGrid1_RecCount: TLabel
    Left = 349
    Top = 590
    Width = 146
    Height = 25
    Caption = 'DbGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object bt_fill: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'bt_fill'
    TabOrder = 0
    OnClick = bt_fillClick
  end
  object bt_append: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'bt_append'
    TabOrder = 1
    OnClick = bt_appendClick
  end
  object Memo1: TMemo
    Left = 539
    Top = 8
    Width = 288
    Height = 440
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object btBitmapCompare: TButton
    Left = 843
    Top = 580
    Width = 141
    Height = 25
    Caption = 'btBitmapCompare'
    TabOrder = 3
    OnClick = btBitmapCompareClick
  end
  object DBGrid1: TDBGrid
    Left = 349
    Top = 8
    Width = 184
    Height = 529
    DataSource = d0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ParentFont = False
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'cod'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'des'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'grp'
        Width = 40
        Visible = True
      end>
  end
  object xnGrid1: TxnGrid
    Left = 159
    Top = 8
    Width = 184
    Height = 529
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
      end>
    OptionsEditing = False
    ColWidths = (
      11
      40
      40
      40)
    RowHeights = (
      17
      20)
  end
  object bt_insert: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'bt_insert'
    TabOrder = 6
    OnClick = bt_insertClick
  end
  object bt_delete: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'bt_delete'
    TabOrder = 7
    OnClick = bt_deleteClick
  end
  object bt_edit: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'bt_edit'
    TabOrder = 8
    OnClick = bt_editClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 163
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 24
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 10
    OnClick = Button1Click
  end
  object d0: TDataSource
    DataSet = cds0
    Left = 400
    Top = 288
  end
  object cds0: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 368
    Top = 288
    object cds0cod: TStringField
      FieldName = 'cod'
    end
    object cds0des: TStringField
      FieldName = 'des'
    end
    object cds0grp: TStringField
      FieldName = 'grp'
    end
  end
end
