unit xn.Items.view;

interface

uses System.Generics.Collections, System.Generics.Defaults, System.SysUtils,
  xn.Items;

type
  IxnItemsView<T> = interface(IxnItems<T>)
    ['{8E9D2328-F658-4FF9-ABB5-8E669E81EA26}']
    procedure Fill;
  end;

  TxnItemsView<T> = class(TInterfacedObject, IxnItemsView<T>)
  strict protected
    fList: IxnItems<T>;
    fIndex: TList<integer>;

    procedure Init(aList: IxnItems<T>); virtual;
    procedure Fill; virtual;
  public
    constructor Create(aList: IxnItems<T>); virtual;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; virtual;

    function Count: integer; virtual;
    function ItemGet(aIndex: integer): T; virtual;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnItemsViewEnumerator<T> = class(TEnumerator<T>)
  strict protected
    fList: TxnItemsView<T>;
  protected
    fIndex: integer;

    function DoGetCurrent: T; override;
    function DoMoveNext: boolean; override;
  public
    constructor Create(aList: TxnItemsView<T>);
  end;

  // ***************************************************************************
  // ***************************************************************************
  TxnItemsViewReverse<T> = class(TxnItemsView<T>)
  protected
    procedure Fill; override;
  end;

  // ***************************************************************************
  // ***************************************************************************
  TxnItemsViewFirst<T> = class(TxnItemsView<T>)
  private
    fCount: integer;
  protected
    procedure Fill; override;
  public
    constructor Create(aList: IxnItems<T>; aCount: integer); reintroduce;
  end;

  TxnItemsViewLast<T> = class(TxnItemsViewFirst<T>)
  protected
    procedure Fill; override;
  end;

  // ***************************************************************************
  // ***************************************************************************
  IxnItemsViewFilter<T> = interface(IxnItemsView<T>)
    ['{FA4D7BC5-FEDB-45AC-A797-E248D9CFF0FE}']
    function Accept(aItem: T): boolean;
  end;

  TxnItemsViewFilter<T> = class(TxnItemsView<T>, IxnItemsViewFilter<T>)
  private
    fAccept: TFunc<T, boolean>;
  protected
    procedure Fill; override;
  public
    constructor Create(aList: IxnItems<T>; aAccept: TFunc<T, boolean>); reintroduce;
    function Accept(aItem: T): boolean; virtual;
  end;

  // ***************************************************************************
  // ***************************************************************************
  IxnItemsViewIndex<T> = interface(IxnItemsView<T>)
    ['{A9771804-6676-4F30-AE94-BFDCCF98E8F5}']
    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;
  end;

  TxnItemsViewIndex<T> = class(TxnItemsView<T>, IxnItemsViewIndex<T>)
  private
    fComparer: iComparer<T>;
  protected
    procedure Fill; override;
  public
    constructor Create(aList: IxnItems<T>; aComparison: TComparison<T>); reintroduce;
    function Seek1(aItem: T): integer; virtual;
    function Seek2(aItem: T): integer; virtual;
  end;

implementation

{ TxnItemsViewEnumerator<T> }

constructor TxnItemsViewEnumerator<T>.Create(aList: TxnItemsView<T>);
begin
  inherited Create;
  fIndex := -1;
  fList := aList;
end;

function TxnItemsViewEnumerator<T>.DoGetCurrent: T;
begin
  Result := fList.Items[fIndex]
end;

function TxnItemsViewEnumerator<T>.DoMoveNext: boolean;
begin
  Result := fIndex < fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

{ TxnItemsView<T> }

function TxnItemsView<T>.Count: integer;
begin
  Result := fIndex.Count
end;

constructor TxnItemsView<T>.Create(aList: IxnItems<T>);
begin
  Init(aList);
  Fill;
end;

destructor TxnItemsView<T>.Destroy;
begin
  fIndex.Free;
  inherited;
end;

procedure TxnItemsView<T>.Fill;
var
  i: integer;
begin
  fIndex.Clear;
  for i := 0 to fList.Count - 1 do
    fIndex.Add(i);
end;

function TxnItemsView<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TxnItemsViewEnumerator<T>.Create(Self);
end;

procedure TxnItemsView<T>.Init(aList: IxnItems<T>);
begin
  fList := aList;
  fIndex := TList<integer>.Create;
end;

function TxnItemsView<T>.ItemGet(aIndex: integer): T;
begin
  Result := fList[fIndex[aIndex]]
end;

{ TxnItemsViewReverse<T> }

procedure TxnItemsViewReverse<T>.Fill;
var
  i: integer;
begin
  fIndex.Clear;
  for i := fList.Count - 1 downto 0 do
    fIndex.Add(i)
end;

{ TxnItemsViewFirst<T> }

constructor TxnItemsViewFirst<T>.Create(aList: IxnItems<T>; aCount: integer);
begin
  Init(aList);
  fCount := aCount;
  Fill;
end;

procedure TxnItemsViewFirst<T>.Fill;
var
  i: integer;
  n: integer;
begin
  if fList.Count < fCount then
    n := fList.Count
  else
    n := fCount;

  fIndex.Clear;
  for i := 0 to n - 1 do
    fIndex.Add(i)
end;

{ TxnItemsViewLast<T> }

procedure TxnItemsViewLast<T>.Fill;
var
  i: integer;
  n: integer;
begin
  if fList.Count - fCount < 0 then
    n := 0
  else
    n := fList.Count - fCount;

  fIndex.Clear;
  for i := n to fList.Count - 1 do
    fIndex.Add(i)
end;

{ TxnItemsViewFilter<T> }

function TxnItemsViewFilter<T>.Accept(aItem: T): boolean;
begin
  Result := fAccept(aItem)
end;

constructor TxnItemsViewFilter<T>.Create(aList: IxnItems<T>; aAccept: TFunc<T, boolean>);
begin
  Init(aList);
  fAccept := aAccept;
  Fill;
end;

procedure TxnItemsViewFilter<T>.Fill;
var
  i: integer;
begin
  fIndex.Clear;
  for i := 0 to fList.Count - 1 do
    if Accept(fList[i]) then
      fIndex.Add(i)
end;

{ TxnItemsViewIndex<T> }

constructor TxnItemsViewIndex<T>.Create(aList: IxnItems<T>; aComparison: TComparison<T>);
begin
  Init(aList);
  fComparer := TComparer<T>.Construct(aComparison);
  Fill;
end;

procedure TxnItemsViewIndex<T>.Fill;
var
  s: integer;
  i: integer;
begin
  fIndex.Clear;
  for i := 0 to fList.Count - 1 do
  begin
    s := Seek2(fList[i]);

    if s = -1 then
      fIndex.Add(i)
    else
      fIndex.Insert(s, i)
  end;
end;

function TxnItemsViewIndex<T>.Seek1(aItem: T): integer;
var
  iStart: integer;
  iStop: integer;
  iPivot: integer;
  iComparer: integer;
begin
  // returns the actual index of the item in the list
  // -1 if the item is not found
  iStart := 0;
  iStop := fIndex.Count - 1;

  if fIndex.Count = 0 then
    exit(-1);

  Result := -1;
  while iStart <= iStop do
  begin
    iPivot := (iStart + iStop) div 2;
    iComparer := fComparer.Compare(Items[iPivot], aItem);

    if iComparer = 0 then
      exit(iPivot)
    else if iComparer > 0 then
      iStop := iPivot - 1
    else
      iStart := iPivot + 1;
  end;
end;

function TxnItemsViewIndex<T>.Seek2(aItem: T): integer;
var
  iStart: integer;
  iStop: integer;
  oStart: integer;
  oStop: integer;
  iPivot: integer;
  iComparer: integer;
  iOther: integer;
begin
  // returns the expected index of the item in the list
  // -1 if the item is after the last item of the list
  iStart := 0;
  oStart := 0;
  iStop := fIndex.Count - 1;
  oStop := fIndex.Count - 1;

  if fIndex.Count = 0 then
    exit(0);

  Result := -1;
  while iStart <= iStop do
  begin
    iPivot := (iStart + iStop) div 2;
    iComparer := fComparer.Compare(Items[iPivot], aItem);

    if iComparer = 0 then
      exit(iPivot)
    else if iComparer > 0 then
    begin
      if iPivot > oStart then
      begin
        iOther := fComparer.Compare(Items[iPivot - 1], aItem);
        if iOther = 0 then
          exit(iPivot - 1)
        else if iOther < 0 then
          exit(iPivot);
      end;
      iStop := iPivot - 1
    end
    else
    begin
      if iPivot < oStop then
      begin
        iOther := fComparer.Compare(Items[iPivot + 1], aItem);
        if iOther = 0 then
          exit(iPivot + 1)
        else if iOther > 0 then
          exit(iPivot + 1);
      end;
      iStart := iPivot + 1;
    end;
  end;

  if fComparer.Compare(Items[oStart], aItem) > 0 then
    exit(oStart);
end;

end.
