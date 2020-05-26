unit SmartPointer;

interface

{$mode Delphi} {$warnings off}

uses
  SysUtils;

type
  IARC<T:class> = interface(IInterface)
     function Value: T;
  end;
                
  ARC<T: class, constructor> = class(TInterfacedObject, IARC<T>)
  private
    FValue: T;
    constructor Create; overload;
    constructor Create(AValue: T); overload; 
  public
    destructor Destroy; override;   
    class function Make: IARC<T>; overload;
    class function Make(AValue: T): IARC<T>; overload;
    function Value: T;
  end;

implementation

{ ARC<T> }

class function ARC<T>.Make: IARC<T>;
begin
  result:= ARC<T>.Create;
end;

class function ARC<T>.Make(AValue: T): IARC<T>;
begin
  result:= ARC<T>.Create(AValue);
end;

constructor ARC<T>.Create;
begin
  inherited;
  FValue := T.Create;
end;

constructor ARC<T>.Create(AValue: T);
begin
  inherited Create;
  if AValue = nil then
    FValue := T.Create
  else
    FValue := AValue;
end;

destructor ARC<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function ARC<T>.Value: T;
begin
  Result := FValue;
end;

end.
