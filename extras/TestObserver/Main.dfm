object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 657
  ClientWidth = 1117
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 969
    Height = 657
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object g2: TDBGrid
        Left = 11
        Top = 19
        Width = 150
        Height = 305
        DataSource = d2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -16
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Alignment = taCenter
            Expanded = False
            FieldName = 'nn'
            Title.Alignment = taCenter
            Width = 50
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = 'vv'
            Title.Alignment = taCenter
            Width = 50
            Visible = True
          end>
      end
      object q2_open: TButton
        Left = 3
        Top = 380
        Width = 96
        Height = 25
        Caption = 'open'
        TabOrder = 1
        OnClick = q2_openClick
      end
      object q2_close: TButton
        Left = 3
        Top = 411
        Width = 96
        Height = 25
        Caption = 'close'
        TabOrder = 2
        OnClick = q2_closeClick
      end
      object q2_apply_updates: TButton
        Left = 3
        Top = 442
        Width = 96
        Height = 25
        Caption = 'apply updates'
        TabOrder = 3
        OnClick = q2_apply_updatesClick
      end
      object q2_cancel_updates: TButton
        Left = 3
        Top = 473
        Width = 96
        Height = 25
        Caption = 'cancel updates'
        TabOrder = 4
      end
      object DBNavigator1: TDBNavigator
        Left = 3
        Top = 330
        Width = 270
        Height = 44
        DataSource = d2
        TabOrder = 5
      end
      object q2_create_filled: TButton
        Left = 3
        Top = 521
        Width = 96
        Height = 25
        Caption = 'create filled'
        TabOrder = 6
        OnClick = q2_create_filledClick
      end
      object sg2: TStringGrid
        Left = 167
        Top = 19
        Width = 150
        Height = 305
        ColCount = 3
        DefaultColWidth = 50
        DefaultRowHeight = 23
        RowCount = 10
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
      end
      object q2_create_empty: TButton
        Tag = 1
        Left = 3
        Top = 552
        Width = 96
        Height = 25
        Caption = 'create empty'
        TabOrder = 8
        OnClick = q2_create_filledClick
      end
      object q2_create_opt: TRadioGroup
        Left = 105
        Top = 521
        Width = 185
        Height = 73
        Caption = 'Create options'
        ItemIndex = 0
        Items.Strings = (
          '1 --> 9'
          '11 --> 19')
        TabOrder = 9
      end
      object l2: TMemo
        Left = 336
        Top = 19
        Width = 249
        Height = 575
        Lines.Strings = (
          'l2')
        TabOrder = 10
      end
      object Button3: TButton
        Left = 168
        Top = 432
        Width = 105
        Height = 83
        Caption = 'Button3'
        TabOrder = 11
        OnClick = Button3Click
      end
      object l2b: TMemo
        Left = 591
        Top = 19
        Width = 330
        Height = 575
        Lines.Strings = (
          'l2b')
        TabOrder = 12
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Button1: TButton
        Left = 3
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 0
        OnClick = Button1Click
      end
      object g0: TDBGrid
        Left = 3
        Top = 39
        Width = 320
        Height = 482
        DataSource = d0
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      object Memo1: TMemo
        Left = 377
        Top = 0
        Width = 292
        Height = 629
        Align = alRight
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
      object Memo2: TMemo
        Left = 669
        Top = 0
        Width = 292
        Height = 629
        Align = alRight
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
      end
      object Button2: TButton
        Left = 240
        Top = 3
        Width = 75
        Height = 33
        Caption = 'Button2'
        TabOrder = 2
        OnClick = Button2Click
      end
      object g1: TDBGrid
        Left = 3
        Top = 158
        Width = 320
        Height = 401
        DataSource = d1
        TabOrder = 3
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object clear_memo: TButton
        Left = 240
        Top = 42
        Width = 75
        Height = 25
        Caption = 'clear_memo'
        TabOrder = 4
        OnClick = clear_memoClick
      end
      object DBNavigator2: TDBNavigator
        Left = 3
        Top = 565
        Width = 310
        Height = 44
        DataSource = d1
        TabOrder = 5
      end
    end
  end
  object db: TUniConnection
    ProviderName = 'SQL Server'
    Database = 'DOCNetDb'
    SpecificOptions.Strings = (
      'SQL Server.Authentication=auWindows')
    Server = 'localhost'
    LoginPrompt = False
    AfterConnect = dbAfterConnect
    Left = 992
    Top = 8
  end
  object q0: TUniQuery
    Connection = db
    SQL.Strings = (
      'select * from #xnGrid')
    Left = 1032
    Top = 64
  end
  object mssql: TSQLServerUniProvider
    Left = 1032
    Top = 8
  end
  object qc0: TUniQuery
    Connection = db
    SQL.Strings = (
      'if object_id ( '#39'tempdb..#xnGrid'#39' ) is not null'
      '   drop table #xnGrid '
      ''
      'create table #xnGrid ( '
      'nn varchar(20) not null, '
      'cc varchar(20) null,'
      'ss varchar(20) null,'
      'ff varchar(20) null,'
      'ii varchar(20) null,'
      'bb varchar(20) null,'
      'primary key ( nn )'
      ') '
      ''
      'insert into #xnGrid'
      'select 20,'#39'Gray'#39','#39'PA'#39',1.53,25,'#39'F'#39' union all'
      'select 0,'#39'Aqua'#39','#39'MD'#39',2.01,71,'#39'T'#39' union all'
      'select 10,'#39'Black'#39','#39'OH'#39',2.06,6,'#39'T'#39' union all'
      'select 6,'#39'Teal'#39','#39'NE'#39',2.06,6,'#39'F'#39' union all'
      'select 38,'#39'Purple'#39','#39'HI'#39',2.56,68,'#39'F'#39' union all'
      'select 22,'#39'Red'#39','#39'WA'#39',2.82,34,'#39'F'#39' union all'
      'select 12,'#39'Lime'#39','#39'HI'#39',2.82,32,'#39'F'#39' union all'
      'select 8,'#39'Red'#39','#39'LA'#39',2.82,80,'#39'F'#39' union all'
      'select 14,'#39'White'#39','#39'UT'#39',2.9,75,'#39'T'#39' union all'
      'select 26,'#39'Purple'#39','#39'MS'#39',3.17,70,'#39'T'#39' union all'
      'select 18,'#39'Olive'#39','#39'VA'#39',3.17,88,'#39'F'#39' union all'
      'select 16,'#39'Lime'#39','#39'VT'#39',3.17,75,'#39'F'#39' union all'
      'select 28,'#39'Fuchsia'#39','#39'ND'#39',3.6,77,'#39'F'#39' union all'
      'select 36,'#39'Silver'#39','#39'NY'#39',3.76,8,'#39'F'#39' union all'
      'select 30,'#39'Silver'#39','#39'AR'#39',3.76,25,'#39'F'#39' union all'
      'select 2,'#39'Yellow'#39','#39'LA'#39',3.76,29,'#39'T'#39' union all'
      'select 34,'#39'LtGray'#39','#39'AZ'#39',3.94,10,'#39'F'#39' union all'
      'select 32,'#39'Maroon'#39','#39'MT'#39',3.94,88,'#39'T'#39' union all'
      'select 24,'#39'White'#39','#39'RI'#39',4.04,29,'#39'T'#39' union all'
      'select 4,'#39'DkGray'#39','#39'VT'#39',4.74,47,'#39'T'#39)
    Left = 992
    Top = 64
  end
  object d0: TDataSource
    DataSet = q0
    Left = 1072
    Top = 64
  end
  object m1: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    Left = 984
    Top = 256
    object m1nn: TIntegerField
      FieldName = 'nn'
    end
    object m1ss: TStringField
      FieldName = 'ss'
    end
  end
  object d1: TDataSource
    DataSet = m1
    Left = 1016
    Top = 256
  end
  object qc11: TUniSQL
    Connection = db
    SQL.Strings = (
      'if object_id ( '#39'DOCNetDb..xnTmp'#39' ) is not null'
      '   drop table DOCNetDb..xnTmp '
      ''
      'create table DOCNetDb..xnTmp ( '
      'nn int not null, '
      'vv varchar(20) null'
      'primary key ( nn )'
      ')')
    Left = 992
    Top = 128
  end
  object qc13: TUniSQL
    Connection = db
    SQL.Strings = (
      'create trigger xnTmpTr'
      'ON DOCNetDb..xnTmp'
      'after insert, update, delete '
      'as '
      'begin'
      'declare @n0 int'
      'declare @n1 int'
      'select @n0 = count(*) from inserted where nn in ( 1 , 3 , 5 )'
      'select @n1 = count(*) from deleted  where nn in ( 1 , 3 , 5 )'
      ''
      'if @n0 + @n1 > 0'
      '   begin'
      '   raiserror ( '#39'r/o record'#39' , 16 , 10 ) ;'
      '   rollback tran'
      '   end '
      'end')
    Left = 1072
    Top = 128
  end
  object q2: TxnUniQuery
    SQLInsert.Strings = (
      'INSERT INTO xnTmp'
      '  (nn, vv)'
      'VALUES'
      '  (:nn, :vv)')
    SQLDelete.Strings = (
      'DELETE FROM xnTmp'
      'WHERE'
      '  nn = :Old_nn')
    SQLUpdate.Strings = (
      'UPDATE xnTmp'
      'SET'
      '  nn = :nn, vv = :vv'
      'WHERE'
      '  nn = :Old_nn')
    SQLLock.Strings = (
      'SELECT * FROM xnTmp'
      'WITH (UPDLOCK, ROWLOCK, HOLDLOCK)'
      'WHERE'
      '  nn = :Old_nn')
    SQLRefresh.Strings = (
      'SELECT nn, vv FROM xnTmp'
      'WHERE'
      '  nn = :nn')
    SQLRecCount.Strings = (
      'SET :PCOUNT = (SELECT COUNT(*) FROM xnTmp'
      ')')
    Connection = db
    SQL.Strings = (
      'select * from xnTmp')
    BeforeInsert = q2BeforeInsert
    AfterInsert = q2AfterInsert
    BeforePost = q2BeforePost
    AfterPost = q2AfterPost
    AfterScroll = q2AfterScroll
    OnNewRecord = q2NewRecord
    Left = 984
    Top = 312
  end
  object d2: TDataSource
    DataSet = q2
    Left = 1016
    Top = 312
  end
  object qc12a: TUniSQL
    Connection = db
    SQL.Strings = (
      'insert into DOCNetDb..xnTmp'
      'select 1 , '#39'a'#39' union all'
      'select 2 , '#39'b'#39' union all'
      'select 3 , '#39'c'#39' union all'
      'select 4 , '#39'd'#39' union all'
      'select 5 , '#39'e'#39' union all'
      'select 6 , '#39'f'#39' union all'
      'select 7 , '#39'g'#39' union all'
      'select 8 , '#39'h'#39' union all'
      'select 9 , '#39'i'#39)
    Left = 1032
    Top = 128
  end
  object qc12b: TUniSQL
    Connection = db
    SQL.Strings = (
      'insert into DOCNetDb..xnTmp'
      'select 11 , '#39'a'#39' union all'
      'select 12 , '#39'b'#39' union all'
      'select 13 , '#39'c'#39' union all'
      'select 14 , '#39'd'#39' union all'
      'select 15 , '#39'e'#39' union all'
      'select 16 , '#39'f'#39' union all'
      'select 17 , '#39'g'#39' union all'
      'select 18 , '#39'h'#39' union all'
      'select 19 , '#39'i'#39)
    Left = 1032
    Top = 184
  end
end
