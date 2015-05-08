unit cDatasource;

interface

uses TypInfo, Uni, Generics.Collections, Data.Db, System.SysUtils, System.Classes;

type
  // TxnDataSource = class(TDataSource)
  // public
  // published
  // end;

  TmyDataLink = class(TDataLink)
  private
    function NullIndex: integer;
    function NullBookmark: TBookmark;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  public
    fState1: string;
    fState2: string;

    Strings: TStrings;
    Strings2: TStrings;
    fData: TList<string>;
    fIndex: integer;
    fBookmark: TBookmark;
    constructor Create;
    destructor Destroy; override;
    procedure Log(aString: string);
    procedure Log2(aString: string);

    procedure DataLoad;
    procedure DataClear;

    function AsDebug: string;

  end;

  TmyEdit = class(TComponent)
  public
    Strings: TStrings;
    Strings2: TStrings;
    Link: TmyDataLink;

    procedure Log(aString: string);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

function BookmarkToString(aBookmark: TBookmark): string;
var
  b: byte;
begin
  for b in aBookmark do
  begin
    if Result <> '' then
      Result := Result + ':';
    Result := Result + IntToHex(b, 2);
  end;

  Result := '[' + Result + ']';
end;

function BookmarkCompare(aBookmark1, aBookmark2: TBookmark): boolean;
begin
  Result := SameStr(BookmarkToString(aBookmark1), BookmarkToString(aBookmark2));
end;

function EventToString(aEvent: TDataEvent): string;
begin
  Result := GetEnumName(TypeInfo(TDataEvent), integer(aEvent));
end;

function StateToString(aState: TDataSetState): string;
begin
  Result := GetEnumName(TypeInfo(TDataSetState), integer(aState));
end;

{ TxnDataSource }

// procedure TxnDataSource.DataEvent(Event: TDataEvent; Info: NativeInt);
// begin
// inherited DataEvent(Event, Info);
// end;

{ TnQuery }

{ TnQuery }

// procedure TnQuery.DataEvent(Event: TDataEvent; Info: TObject);
// begin
//
// end;

{ TnQuery }

{ TxnDataSource }

{ TmyEdit }

constructor TmyEdit.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Strings := nil;

  Link := TmyDataLink.Create;
  // Link.Control := Self;
  // Link.OnDataChange := DataChange;
  // Link.OnEditingChange := EditingChange;
  // Link.OnUpdateData := UpdateData;
  // Link.OnActiveChange := ActiveChange;
end;

destructor TmyEdit.Destroy;
begin
  Link.Free;
  inherited;
end;

procedure TmyEdit.Log(aString: string);
begin
  if Strings <> nil then
    Strings.Add(aString);
end;

{ TmyDataLink }

function TmyDataLink.AsDebug: string;
var
  c: string;
  r: string;
  i: integer;
begin
  // if fIndex <= 0 then
  // exit('')
  // else
  exit(IntToStr(fIndex));

  if fData.Count = 0 then
    exit('');



  // c := fData[fIndex];

  r := '';
  for i := 0 to fData.Count - 1 do
    r := r + fData[i] + ',';

  r := '[' + c + ']' + r;
  Result := r;
end;

constructor TmyDataLink.Create;
begin
  inherited Create;
  fState1 := '-1';
  fState2 := '-2';
  fData := TList<string>.Create;
  fIndex := NullIndex();
  fBookmark := NullBookmark();
  Strings := nil;
end;

procedure TmyDataLink.DataClear;
begin
  fData.Clear;
  fIndex := -1;
end;

procedure TmyDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
var
  state_new: string;
  state_old_1: string;
  state_old_2: string;

  index_new: integer;
  index_old: integer;
  bookmark_new: TBookmark;
  bookmark_old: TBookmark;

  valid_old: boolean;

  procedure LLL(aAction: string; aIndex: integer; aBookmark: TBookmark);
  begin
    if (SameText(aAction, 'DELETE')) and (aIndex = 0) then
      exit;

    // if (SameText(aAction, 'INSERT')) and (SameText(BookmarkToString(aBookmark), '[]')) then
    // exit;

    Log('[' + aAction + ']: ' + IntToStr(aIndex) + ': ' + BookmarkToString(aBookmark))
  end;

begin
  inherited;

  state_old_1 := fState1;
  state_old_2 := fState2;
  state_new := StateToString(DataSet.State);
  fState1 := state_new;
  fState2 := state_old_1;

  index_old := fIndex;
  index_new := DataSet.RecNo;
  fIndex := index_new;

  valid_old := DataSet.BookmarkValid(fBookmark);

  bookmark_old := fBookmark;
  bookmark_new := DataSet.Bookmark;
  fBookmark := bookmark_new;

  Log2(StateToString(DataSet.State)
    // + ' : ' + IntToStr(Info)
    + ' : ' + IntToStr(fIndex)
    + ' : ' + BookmarkToString(fBookmark)
    + ' : ' + DataSet.FieldByName('nn').AsString
    + ' : ' + DataSet.FieldByName('ss').AsString
    );

  if SameStr('dsEdit', state_old_2)
    and (SameStr('dsBrowse', state_old_1))
    and (SameStr('dsBrowse', state_new)) then
  begin
    LLL('EDIT', index_new, bookmark_new);
  end
  else if SameStr('dsInsert', state_old_2)
    and SameStr('dsBrowse', state_old_1)
    and SameStr('dsBrowse', state_new) then
  begin
    LLL('INSERT', index_new, bookmark_new);
    LLL('REC_NO', index_new, bookmark_new)
  end

  // else if SameStr('dsInsert', state_old_1)
  // and SameStr('dsInsert', state_new) then
  // begin
  // LLL('REC_NO', index_new, bookmark_new)
  // end

  else if SameStr('dsBrowse', state_old_1)
    and SameStr('dsBrowse', state_new) then
  begin
    if valid_old then
      LLL('REC_NO', index_new, bookmark_new)
    else
    begin
      LLL('DELETE', index_old, bookmark_old);
      LLL('REC_NO', index_new, bookmark_new);
    end;
  end
  else
  begin
    //
    // Log(StateToString(DataSet.State) // + ' : ' + StateToString(DataSource.State)
    // + ' : ' + IntToStr(Info)
    // + ' : ' + IntToStr(fIndex)
    // + ' : ' + BookmarkToString(fBookmark)
    // + ' : ' + DataSet.FieldByName('nn').AsString
    // + ' : ' + DataSet.FieldByName('ss').AsString
    // );
  end;



  // ;
  // else if (SameStr('dsBrowse', state_new))
  // and (SameStr('dsInsert', state_old_2)) then
  // begin
  // Log('[' + state_old_2 + '] : ' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark))
  // end
  // else if (SameStr('dsBrowse', state_new))
  // and (SameStr('dsBrowse', state_old_1))
  // then
  // Log('[REST' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark) + ' : valid ')
  // // if valid_old then
  // // Log('[' + state_new + '] : ' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark) + ' : valid ')
  // // else
  // // Log('[' + state_new + '] : ' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark) + ' : not valid ')
  // else;
  // if valid_old then
  // Log('[valid] : ' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark))
  // else
  // Log('[invalid] : ' + IntToStr(fIndex) + ' : ' + BookmarkToString(fBookmark));
  // ;

  exit;
  // if BookmarkToString(bookmark_old) <> BookmarkToString(bookmark_new) then
  Log(StateToString(DataSet.State)
    + ' : ' + IntToStr(Info)
    + ' : ' + IntToStr(fIndex)
    + ' : ' + BookmarkToString(fBookmark)
    + ' : ' + DataSet.FieldByName('nn').AsString
    + ' : ' + DataSet.FieldByName('ss').AsString
    );
  exit;
  case Event of
    deFieldChange:
      ;
    deRecordChange:
      // log ( 'deRecordChange')
      ;
    deDataSetChange:
      begin
        // if DataSet.State = dsInsert then
        // if DataSet.Eof then
        // Log('append ' + IntToStr(index_new))
        // else
        // Log('insert ' + IntToStr(index_new))
        // else
        //
        // begin
        // // Log(StateToString(DataSet.State));
        // // Log('old : ' + BookmarkToString(bookmark_old));
        // // Log('new : ' + BookmarkToString(bookmark_new));
        // if BookmarkToString(bookmark_old) <> '' then
        // begin
        // // if BookmarkCompare(bookmark_new, bookmark_old) then
        // // Log('edit ' + IntToStr(index_new))
        // // else
        // // Log('delete ' + IntToStr(index_new) + ':' + IntToStr(index_old))
        //
        // end;
        //
        // // if DataSet.State = dsEdit then
        // // Log('update ' + IntToStr(index_new))
        // // else
        // //
        // end;
      end;
    deDataSetScroll:
      ;
    deLayoutChange:
      ;
    deUpdateRecord:
      ;
    deUpdateState:
      // if DataSet.State <> dsInsert then
      // Log('update ' + IntToStr(index_new))
      ;
    deCheckBrowseMode:
      ;
    dePropertyChange:
      ;
    deFieldListChange:
      ;
    deFocusControl:
      ;
    deParentScroll:
      ;
    deConnectChange:
      ;
    deReconcileError:
      ;
    deDisabledStateChange:
      ;
  end;

end;

destructor TmyDataLink.Destroy;
begin
  fData.Free;
  inherited;
end;

function TmyDataLink.NullBookmark: TBookmark;
begin
  SetLength(Result, 0);
end;

function TmyDataLink.NullIndex: integer;
begin
  Result := -1;
end;

procedure TmyDataLink.DataLoad;
begin
  DataClear;

  DataSet.First;
  while not DataSet.Eof do
  begin
    fData.Add(DataSet.FieldByName('nn').AsString);
    DataSet.Next;
  end;
end;

procedure TmyDataLink.Log(aString: string);
begin
  if Strings <> nil then
    Strings.Add(aString);
end;

procedure TmyDataLink.Log2(aString: string);
begin
  if Strings2 <> nil then
    Strings2.Add(aString);
end;

end.
