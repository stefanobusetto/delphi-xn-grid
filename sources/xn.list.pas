unit xn.list;

interface

uses System.Generics.Collections, System.Generics.Defaults, xn.Items;

type
  IxnListCustom<T> = interface(IxnItems<T>)
    ['{D08DE521-118D-4A58-B9D4-34E8211FED75}']
    function GetEnumerator: TEnumerator<T>;

    function Add(aItem: T): Integer; overload;
    function Remove(aItem: T): Integer; overload;
    procedure Delete(aIndex: Integer); overload;
    procedure Clear;

    function IndexOf(aItem: T): Integer; overload ;
    function Contains(aItem: T): boolean; overload ;
  end;

  IxnList<T> = interface(IxnListCustom<T>)
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    procedure Sort; overload;
    procedure Sort(const aComparer: IComparer<T>); overload;

    procedure Insert(aIndex: Integer; aItem: T);
  end;

  TxnList<T> = class(TxnItems<T>, IxnList<T>)
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; override;

    function Add(aItem: T): Integer; virtual;
    function Remove(aItem: T): Integer; virtual;
    procedure Delete(aIndex: Integer); virtual;
    procedure Clear; virtual;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;
    function IndexOf(aItem: T): Integer; overload ;virtual;
    function Contains(aItem: T): boolean; overload ;virtual;

    procedure Insert(aIndex: Integer; aItem: T);
  end;

implementation

{ TxnList<T> }

function TxnList<T>.Add(aItem: T): Integer;
begin
  Result := fItems.Add(aItem);
end;

function TxnList<T>.Remove(aItem: T): Integer;
begin
  Result := fItems.Remove(aItem);
end;

procedure TxnList<T>.Clear;
begin
  fItems.Clear;
end;

procedure TxnList<T>.Delete(aIndex: Integer);
begin
  fItems.Delete(aIndex);
end;

constructor TxnList<T>.Create;
begin
  inherited;
end;

destructor TxnList<T>.Destroy;
begin
  inherited;
end;

function TxnList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fItems.GetEnumerator;
end;

function TxnList<T>.Contains(aItem: T): boolean;
begin
  Result := fItems.Contains(aItem)
end;

function TxnList<T>.IndexOf(aItem: T): Integer;
begin
  Result := fItems.IndexOf(aItem);
end;

procedure TxnList<T>.Insert(aIndex: Integer; aItem: T);
begin
  fItems.Insert(aIndex, aItem);
end;

procedure TxnList<T>.Sort;
begin
  fItems.Sort;
end;

procedure TxnList<T>.Sort(const aComparer: IComparer<T>);
begin
  fItems.Sort(aComparer);
end;

end.
