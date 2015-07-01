object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 847
  ClientWidth = 1417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Left = 903
    Top = 563
    Width = 67
    Height = 19
    Caption = 'DiffCount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object xnGrid1_RecNo: TLabel
    Left = 159
    Top = 8
    Width = 110
    Height = 19
    Caption = 'xnGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DbGrid1_RecNo: TLabel
    Left = 349
    Top = 8
    Width = 113
    Height = 19
    Caption = 'DbGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object xnGrid1_RecCount: TLabel
    Left = 159
    Top = 30
    Width = 110
    Height = 19
    Caption = 'xnGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DbGrid1_RecCount: TLabel
    Left = 349
    Top = 30
    Width = 113
    Height = 19
    Caption = 'DbGrid1_RecNo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 19
    Caption = 'RecNo()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 30
    Width = 79
    Height = 19
    Caption = 'RecCount()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DiffMin: TLabel
    Left = 1032
    Top = 563
    Width = 48
    Height = 19
    Caption = '+9999'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object DiffMax: TLabel
    Left = 1036
    Top = 587
    Width = 42
    Height = 19
    Caption = '-9999'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 994
    Top = 563
    Width = 37
    Height = 19
    Caption = 'Min()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 994
    Top = 587
    Width = 40
    Height = 19
    Caption = 'Max()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Differenze: TLabel
    Left = 843
    Top = 543
    Width = 71
    Height = 19
    Caption = 'Differenze'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 843
    Top = 563
    Width = 54
    Height = 19
    Caption = 'Count()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object bt_fill: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'bt_fill'
    TabOrder = 0
    OnClick = bt_fillClick
  end
  object bt_append: TButton
    Left = 8
    Top = 204
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
    Height = 769
    TabOrder = 2
  end
  object btBitmapCompare: TButton
    Left = 843
    Top = 590
    Width = 141
    Height = 25
    Caption = 'btBitmapCompare'
    TabOrder = 3
    OnClick = btBitmapCompareClick
  end
  object DBGrid1: TDBGrid
    Left = 349
    Top = 55
    Width = 184
    Height = 129
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
  object bt_insert: TButton
    Left = 8
    Top = 111
    Width = 75
    Height = 25
    Caption = 'bt_insert'
    TabOrder = 5
    OnClick = bt_insertClick
  end
  object bt_delete: TButton
    Left = 8
    Top = 142
    Width = 75
    Height = 25
    Caption = 'bt_delete'
    TabOrder = 6
    OnClick = bt_deleteClick
  end
  object bt_edit: TButton
    Left = 8
    Top = 173
    Width = 75
    Height = 25
    Caption = 'bt_edit'
    TabOrder = 7
    OnClick = bt_editClick
  end
  object bt_loop: TButton
    Left = 8
    Top = 432
    Width = 75
    Height = 25
    Caption = 'bt_loop'
    TabOrder = 8
    OnClick = bt_loopClick
  end
  object bt_clear: TButton
    Left = 8
    Top = 235
    Width = 75
    Height = 25
    Caption = 'bt_clear'
    TabOrder = 9
    OnClick = bt_clearClick
  end
  object bt_first: TButton
    Left = 8
    Top = 266
    Width = 75
    Height = 25
    Caption = 'bt_first'
    TabOrder = 10
    OnClick = bt_firstClick
  end
  object bt_last: TButton
    Left = 8
    Top = 297
    Width = 75
    Height = 25
    Caption = 'bt_last'
    TabOrder = 11
    OnClick = bt_lastClick
  end
  object bt_prior: TButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'bt_prior'
    TabOrder = 12
    OnClick = bt_priorClick
  end
  object bt_next: TButton
    Left = 8
    Top = 359
    Width = 75
    Height = 25
    Caption = 'bt_next'
    TabOrder = 13
    OnClick = bt_nextClick
  end
  object Memo2: TMemo
    Left = 158
    Top = 190
    Width = 185
    Height = 324
    TabOrder = 14
  end
  object Button2: TButton
    Left = 64
    Top = 632
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 16
    OnClick = Button2Click
  end
  object log_clear: TButton
    Left = 539
    Top = 783
    Width = 75
    Height = 25
    Caption = 'log_clear'
    TabOrder = 15
    OnClick = log_clearClick
  end
  object xnGrid1: TxnGrid
    Left = 158
    Top = 55
    Width = 185
    Height = 129
    Columns = <
      item
        Alignment = taLeftJustify
        Width = 0
        Caption = 'cod'
      end
      item
        Alignment = taLeftJustify
        Width = 0
        Caption = 'des'
      end
      item
        Alignment = taLeftJustify
        Width = 0
        Caption = 'grp'
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
  object StringGrid1: TStringGrid
    Left = 168
    Top = 537
    Width = 320
    Height = 120
    TabOrder = 18
  end
  object d0: TDataSource
    DataSet = cds0
    Left = 384
    Top = 207
  end
  object cds0: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 352
    Top = 207
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
