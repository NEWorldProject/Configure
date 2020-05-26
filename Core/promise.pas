unit Promise;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  { ITaskResult }

  ITaskResult<T> = interface(IInterface)
    function GetResult: T;
    property Result: T read GetResult;
  end;

  TContinuation<T> = procedure (AValue:IInterface) of object;

  { ITask }

  ITask<T> = interface(ITaskResult<T>)
    function GetReadyFlag : boolean;
    property Ready: boolean read GetReadyFlag;
    procedure Wait;
    procedure SetContinutation(AValue: TContinuation<T>);
  end;

  { ITaskSource }

  ITaskSource<T> = interface(IInterface)
    function GetTask: ITask<T>;
    property Task: ITask<T> read GetTask;
    procedure SetValue(AValue:T);
    procedure SetException(AValue: Exception);
  end;

  { ITaskCompletionSource }

  ITaskCompletionSource<T> = class(TInterfacedObject, ITaskSource<T>, ITask<T>)
    function GetResult: T;
    function GetReadyFlag : boolean; 
    function GetTask: ITask<T>;
    procedure SetAcquire;
    procedure SetRelease;
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    destructor Destroy; override;
    property Ready: boolean read GetReadyFlag;
    property Result: T read GetResult;
    procedure Wait;
    procedure SetContinutation(AValue: TContinuation<T>);
    property Task: ITask<T> read GetTask;
    procedure SetValue(AValue:T);
    procedure SetException(AValue: Exception);
  private
    FEvent: PRTLEvent;   
    FReady: Longint;
    FContinuation: TContinuation<T>;
    FValue: T;
    FException: Exception;
  end;

implementation


{ ITaskCompletionSource }

function ITaskCompletionSource<T>.GetResult: T;
begin
  self.Wait;
  if self.FException<>nil then raise self.FException;
  result:=self.FValue;
end;

function ITaskCompletionSource<T>.GetReadyFlag: boolean;
begin
  result:=self.FReady=2;
end;

function ITaskCompletionSource<T>.GetTask: ITask<T>;
begin
  result:=self;
end;

procedure ITaskCompletionSource<T>.SetAcquire;
var
  flag: Longint;
begin
  flag:=System.InterLockedExchange(FReady, 1);
  if (flag <> 0) and (flag <> 255) then raise Exception.Create('Value already set');
  if flag = 255 then FReady:=255;
end;

procedure ITaskCompletionSource<T>.SetRelease;
begin
  System.RTLeventSetEvent(FEvent);
  if System.InterLockedExchange(FReady, 2) = 255 then begin
    if @self.FContinuation <> nil then begin
      self.FContinuation(self);
    end;
  end;
end;

constructor ITaskCompletionSource<T>.Create;
begin
  self.FEvent:=System.RTLEventCreate;
  self.FReady:=0;
  self.FContinuation:=nil;
  self.FException:=nil;
end;

constructor ITaskCompletionSource<T>.Create(AValue: T);
begin          
  self.FEvent:=nil;
  self.FReady:=2;
  self.FContinuation:=nil;
  self.FException:=nil;
  self.FValue:=AValue;
end;

destructor ITaskCompletionSource<T>.Destroy;
begin
  if self.FEvent <> nil then System.RTLEventDestroy(self.FEvent);
  inherited Destroy;
end;

procedure ITaskCompletionSource<T>.Wait;
begin
  System.RTLEventWaitFor(FEvent);
end;

procedure ITaskCompletionSource<T>.SetContinutation(AValue: TContinuation<T>);
var
  val: Longint;
begin
  if @AValue=nil then Exit;
  self.FContinuation:=AValue;
  val:=System.InterlockedCompareExchange(self.FReady, 255, 0);
  if val=0 then Exit;
  if val=1 then begin
    val:=System.InterlockedCompareExchange(self.FReady, 255, 1);
    if val=1 then Exit;
  end;
  if val=255 then raise Exception.Create('Continuation already set');
  AValue(self);
end;

procedure ITaskCompletionSource<T>.SetValue(AValue: T);
begin
  self.SetAcquire;
  self.FValue:=AValue;
  self.SetRelease;
end;

procedure ITaskCompletionSource<T>.SetException(AValue: Exception);
begin             
  self.SetAcquire;
  self.FException:=AValue;
  self.SetRelease;
end;

end.

