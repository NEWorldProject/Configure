unit Log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogType = (nwltDebug, nwltVerbose, nwltInfo, nwltWarning, nwltFatal);

  TLogFilter = set of TLogType;
                
  { ILogPoolUnit }

  ILogPoolUnit = interface(IInterface)
    function GetFilter: TLogFilter;
    procedure SetFilter(AValue: TLogFilter);
    property Filter: TLogFilter read GetFilter write SetFilter;
    procedure Write(const s:string);
    procedure WriteLn(const s:string);
    procedure Flush;
  end;

  TLogCallback = procedure (s:string) of object;

  { ILogEchoUnit }

  ILogEchoUnit = interface(ILogPoolUnit)
    procedure SetCallback(callback: TLogCallback);
    function GetCallback: TLogCallback;
    property Callback: TLogCallback read GetCallback write SetCallback;
  end;

  { ILog }

  ILog = interface(IInterface)
    procedure Debug(const s:string);
    procedure Verbose(const s:string);
    procedure Info(const s:string);
    procedure Warning(const s:string);
    procedure Fatal(const s:string);
  end;

  { ILogFormatterUnit }

  ILogFormatterUnit = interface(ILog)
    function GetFormatDebug: string;
    function GetFormatVerbose: string;
    function GetFormatInfo: string;
    function GetFormatWarning: string;
    function GetFormatFatal: string;
    function GetFormatTimestamp: string;

    procedure SetFormatDebug(AValue: string);
    procedure SetFormatVerbose(AValue: string);
    procedure SetFormatInfo(AValue: string);
    procedure SetFormatWarning(AValue: string);
    procedure SetFormatFatal(AValue: string);
    procedure SetFormatTimestamp(AValue: string);

    property FormatDebug: string read GetFormatDebug write SetFormatDebug;
    property FormatVerbose: string read GetFormatVerbose write SetFormatVerbose;
    property FormatInfo: string read GetFormatInfo write SetFormatInfo;
    property FormatWarning: string read GetFormatWarning write SetFormatWarning;
    property FormatFatal: string read GetFormatFatal write SetFormatFatal;
    property FormatTimestamp: string read GetFormatTimestamp write SetFormatTimestamp;
  end;

  { ILogModule }

  ILogModule = interface(ILogFormatterUnit)
    function GetPrefix: string;
    property Prefix: string read GetPrefix;
    function Filter(AFilter: TLogFilter): ILogModule;
    function Install(pool: ILogPoolUnit): ILogModule;
    function Uninstall(pool: ILogPoolUnit): ILogModule;
  end;

  function CreateConsolePool: ILogPoolUnit;
  function CreateSimpleFilePool(const fileName: string): ILogPoolUnit;
  function CreateRotatingFilePool(
    const directoryName: string;
    const fileNameFormat: string;
    const rotationMaxSize: Integer;
    const rotationMaxTime: Integer
  ): ILogPoolUnit;
  function CreateSyncLogModule(const moduleName: string): ILogModule;
  function CreateEchoPool: ILogEchoUnit;

const
  LogFilterDevelop = [nwltDebug, nwltVerbose, nwltInfo, nwltWarning, nwltFatal]; 
  LogFilterDiagnose = [nwltVerbose, nwltInfo, nwltWarning, nwltFatal];
  LogFilterOperation = [nwltInfo, nwltWarning, nwltFatal];

implementation

type

  { TConsolePool }

  TConsolePool = class (TInterfacedObject, ILogPoolUnit)
  private             
    FFilter: TLogFilter;
    function GetFilter: TLogFilter;
    procedure SetFilter(AValue: TLogFilter);
  public
    constructor Create;
    property Filter: TLogFilter read GetFilter write SetFilter;
    procedure Write(const s:string);
    procedure WriteLn(const s:string);
    procedure Flush;
  end;


  { TSyncCallbackPool }

  TSyncCallbackPool = class (TInterfacedObject, ILogEchoUnit)
  private
    FFilter: TLogFilter;
    FCallback: TLogCallback;
    function GetFilter: TLogFilter;
    procedure SetFilter(AValue: TLogFilter);     
    procedure SetCallback(callback: TLogCallback);
    function GetCallback: TLogCallback;
  public
    constructor Create;
    property Filter: TLogFilter read GetFilter write SetFilter;  
    property Callback: TLogCallback read GetCallback write SetCallback;
    procedure Write(const s:string);
    procedure WriteLn(const s:string);
    procedure Flush;
  end;

  { TSimpleFilePool }

  TSimpleFilePool = class (TInterfacedObject, ILogPoolUnit)
  private
    FFile: TextFile; 
    FFilter: TLogFilter;
    function GetFilter: TLogFilter;
    procedure SetFilter(AValue: TLogFilter);
  public
    constructor Create(fileName: string);
    destructor Destroy; override;
    property Filter: TLogFilter read GetFilter write SetFilter;
    procedure Write(const s:string);
    procedure WriteLn(const s:string);
    procedure Flush;
  end;

  { TRotatingFilePool }

  TRotatingFilePool = class (TInterfacedObject, ILogPoolUnit)
  private
    FFile: TextFile;
    FFilter: TLogFilter;
    FBase: string;
    FFileNameFormat: string;
    FRotationMaxSize: Integer;
    FRotationMaxTime: Integer;
    FCurrentFileSize: Integer;
    FLastRotationTime: TDateTime; 
    function GetFilter: TLogFilter;
    procedure SetFilter(AValue: TLogFilter);
    procedure Rotate(AClose: boolean);
    procedure TestForRotation(const increment: Integer);
  public
    constructor Create(
      const directoryName: string;
      const fileNameFormat: string;
      const rotationMaxSize: Integer;
      const rotationMaxTime: Integer
    );
    destructor Destroy; override;
    property Filter: TLogFilter read GetFilter write SetFilter;
    procedure Write(const s:string);
    procedure WriteLn(const s:string);
    procedure Flush;
  end;
               
  { TSyncLogModule }

  TSyncLogModule = class (TInterfacedObject, ILogModule)
  private           
    FFilter: TLogFilter;
    FFormatDebug: string;
    FFormatFatal: string;
    FFormatInfo: string;
    FFormatTimestamp: string;
    FFormatVerbose: string;
    FFormatWarning: string;
    FLogPools: array of ILogPoolUnit;
    FPrefix: string;
    procedure InitDefault;
    function GetFormatDebug: string;
    function GetFormatVerbose: string;
    function GetFormatInfo: string;
    function GetFormatWarning: string;
    function GetFormatFatal: string;
    function GetFormatTimestamp: string;
    procedure SetFormatDebug(AValue: string);
    procedure SetFormatVerbose(AValue: string);
    procedure SetFormatInfo(AValue: string);
    procedure SetFormatWarning(AValue: string);
    procedure SetFormatFatal(AValue: string);
    procedure SetFormatTimestamp(AValue: string);
    procedure InternalDoRecord(cat: TLogType; usr: string; flush: boolean);
    function InternalPrintLogA(fmt: string; usr: string): string;
    function InternalPrintLogB(fmt: string; usr: string; stack: string): string;
    function GetPrefix: string;
    function StackTraceString: string;
  public
    constructor Create(module: string);
    procedure Debug(const s:string);
    procedure Verbose(const s:string);
    procedure Info(const s:string);
    procedure Warning(const s:string);
    procedure Fatal(const s:string);
    property FormatDebug: string read GetFormatDebug write SetFormatDebug;
    property FormatVerbose: string read GetFormatVerbose write SetFormatVerbose;
    property FormatInfo: string read GetFormatInfo write SetFormatInfo;
    property FormatWarning: string read GetFormatWarning write SetFormatWarning;
    property FormatFatal: string read GetFormatFatal write SetFormatFatal;
    property FormatTimestamp: string read GetFormatTimestamp write SetFormatTimestamp;
    property Prefix: string read GetPrefix;    
    function Filter(AFilter: TLogFilter): ILogModule;
    function Install(pool: ILogPoolUnit): ILogModule;
    function Uninstall(pool: ILogPoolUnit): ILogModule;
  end;

function CreateConsolePool: ILogPoolUnit;
begin
  result:=nil;
  try
    result:=TConsolePool.Create;
  except
  end;
end;

function CreateSimpleFilePool(const fileName: string): ILogPoolUnit;
begin
  result:=nil;
  try
    result:=TSimpleFilePool.Create(fileName);
  except
  end;
end;

function CreateRotatingFilePool(const directoryName: string;
  const fileNameFormat: string; const rotationMaxSize: Integer;
  const rotationMaxTime: Integer): ILogPoolUnit;
begin         
  result:=nil;
  try
    result:=TRotatingFilePool.Create(
      directoryName, fileNameFormat, rotationMaxSize, rotationMaxTime
    );
  except
  end;
end;

function CreateSyncLogModule(const moduleName: string): ILogModule;
begin   
  result:=nil;
  try
    result:=TSyncLogModule.Create(moduleName);
  except
  end;
end;

function CreateEchoPool: ILogEchoUnit;
begin
  result:=nil;
  try
    result:=TSyncCallbackPool.Create;
  except
  end;
end;

function TSyncCallbackPool.GetFilter: TLogFilter;
begin
  result:=FFilter;
end;

procedure TSyncCallbackPool.SetFilter(AValue: TLogFilter);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

procedure TSyncCallbackPool.SetCallback(callback: TLogCallback);
begin        
  if FCallback=callback then Exit;
  FCallback:=callback;
end;

function TSyncCallbackPool.GetCallback: TLogCallback;
begin
  result:=FCallback;
end;

constructor TSyncCallbackPool.Create;
begin
  FCallback:=nil;
  FFilter:=LogFilterDiagnose;
end;

procedure TSyncCallbackPool.Write(const s: string);
begin
  if FCallback<>nil then FCallback(s);
end;

procedure TSyncCallbackPool.WriteLn(const s: string);
begin
  if FCallback<>nil then FCallback(s + LineEnding);
end;

procedure TSyncCallbackPool.Flush;
begin

end;

{ TSyncLogModule }

procedure TSyncLogModule.InitDefault;
begin
    FFormatDebug:=  '[  Debug][%0:s][%1:s]%2:s';
    FFormatVerbose:='[Verbose][%0:s][%1:s]%2:s';
    FFormatInfo:=   '[   Info][%0:s][%1:s]%2:s';
    FFormatWarning:='[Warning][%0:s][%1:s]%2:s';
    FFormatFatal:=  '[  Fatal][%0:s][%1:s]%2:s' + LineEnding + '%3:S';
    FFormatTimestamp:='YYMMDD-hhnnss';
    FPrefix:='';
    FFilter:=LogFilterDiagnose;
end;

function TSyncLogModule.GetFormatDebug: string;
begin
  result:=FFormatDebug;
end;

function TSyncLogModule.GetFormatVerbose: string;
begin
  result:=FFormatVerbose;
end;

function TSyncLogModule.GetFormatInfo: string;
begin
  result:=FFormatInfo;
end;

function TSyncLogModule.GetFormatWarning: string;
begin
  result:=FFormatWarning;
end;

function TSyncLogModule.GetFormatFatal: string;
begin
  result:=FFormatFatal;
end;

function TSyncLogModule.GetFormatTimestamp: string;
begin
  result:=FFormatTimestamp;
end;

procedure TSyncLogModule.SetFormatDebug(AValue: string);
begin
  if FFormatDebug=AValue then Exit;
  FFormatDebug:=AValue;
end;

procedure TSyncLogModule.SetFormatFatal(AValue: string);
begin
  if FFormatFatal=AValue then Exit;
  FFormatFatal:=AValue;
end;

procedure TSyncLogModule.SetFormatInfo(AValue: string);
begin
  if FFormatInfo=AValue then Exit;
  FFormatInfo:=AValue;
end;

procedure TSyncLogModule.SetFormatTimestamp(AValue: string);
begin
  if FFormatTimestamp=AValue then Exit;
  FFormatTimestamp:=AValue;
end;

procedure TSyncLogModule.SetFormatVerbose(AValue: string);
begin
  if FFormatVerbose=AValue then Exit;
  FFormatVerbose:=AValue;
end;

procedure TSyncLogModule.SetFormatWarning(AValue: string);
begin
  if FFormatWarning=AValue then Exit;
  FFormatWarning:=AValue;
end;

procedure TSyncLogModule.InternalDoRecord(cat: TLogType; usr: string; flush: boolean);
var
  pool: ILogPoolUnit;
  cats: TLogFilter;
begin
  for pool in FLogPools do begin
    cats:=pool.Filter;
    if cat in cats then begin
      pool.WriteLn(usr);
      if flush then pool.Flush;
    end;
  end;
end;

function TSyncLogModule.InternalPrintLogA(fmt: string; usr: string): string;
var
  time: string;
begin
  time:=FormatDateTime(FFormatTimestamp, Now);
  result:=Format(fmt, [self.FPrefix, time, usr]);
end;

function TSyncLogModule.InternalPrintLogB(fmt: string; usr: string;
  stack: string): string;
var
  time: string;
begin
  time:=FormatDateTime(FFormatTimestamp, Now);
  result:=Format(fmt, [self.FPrefix, time, usr, stack]);
end;

function TSyncLogModule.GetPrefix: string;
begin
  result:=FPrefix;
end;

function TSyncLogModule.StackTraceString: string;
var
  frameDepth: Longint;
  callerFrame, callerAddress, bp, bpOld: Pointer;
const
  MaxDepth = 2048;
begin    
  result := '';
  try
    bp:= get_caller_frame(get_frame);
    bpOld := bp - 1;
    frameDepth := 0;
    while bp > bpOld do begin
       callerAddress := get_caller_addr(bp);
       callerFrame := get_caller_frame(bp);
       if (callerAddress = nil) then break;
       result := result + BackTraceStrFunc(callerAddress) + LineEnding;
       Inc(frameDepth);
       if (frameDepth >= MaxDepth) or (callerFrame = nil) then break;
       bpOld := bp;
       bp := callerFrame;
     end;
   except
   end;
end;

constructor TSyncLogModule.Create(module: string);
begin
  inherited Create;
  InitDefault;
  FPrefix:=module;
end;

procedure TSyncLogModule.Debug(const s: string);
begin
  if not (nwltDebug in FFilter) then Exit;
  InternalDoRecord(nwltDebug, InternalPrintLogA(FFormatDebug, s), false);
end;

procedure TSyncLogModule.Verbose(const s: string);
begin            
  if not (nwltVerbose in FFilter) then Exit;
  InternalDoRecord(nwltVerbose, InternalPrintLogA(FFormatVerbose, s), false);
end;

procedure TSyncLogModule.Info(const s: string);
begin                                      
  if not (nwltInfo in FFilter) then Exit;
  InternalDoRecord(nwltInfo, InternalPrintLogA(FFormatInfo, s), false);
end;

procedure TSyncLogModule.Warning(const s: string);
begin                               
  if not (nwltWarning in FFilter) then Exit;
  InternalDoRecord(nwltWarning, InternalPrintLogA(FFormatWarning, s), false);
end;

procedure TSyncLogModule.Fatal(const s: string);
begin              
  if not (nwltFatal in FFilter) then Exit;
  InternalDoRecord(nwltFatal, InternalPrintLogB(FFormatFatal, s, StackTraceString), true);
end;

function TSyncLogModule.Filter(AFilter: TLogFilter): ILogModule;
begin
  FFilter:=AFilter;
  result:=self;
end;

function TSyncLogModule.Install(pool: ILogPoolUnit): ILogModule;
var
  len: integer;
begin
  if pool = nil then Exit;
  len:=length(FLogPools);
  setLength(FLogPools, len + 1);
  FLogPools[len]:=pool;
  result:=self;
end;

function TSyncLogModule.Uninstall(pool: ILogPoolUnit): ILogModule;
var
  i, j, len: integer;
begin     
  if pool = nil then Exit;
  len:=length(FLogPools);
  for i:=0 to len-1 do begin
    if FLogPools[i]=pool then begin
      for j:=i to len - 2 do begin
        FLogPools[i]:=FLogPools[i+1];
      end;
      Exit(self);
    end;
  end;
end;

{ TRotatingFilePool }

function TRotatingFilePool.GetFilter: TLogFilter;
begin
  result:=FFilter;
end;

procedure TRotatingFilePool.SetFilter(AValue: TLogFilter);
begin     
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

procedure TRotatingFilePool.Rotate(AClose: boolean);
var
  newName: string;
begin
  FLastRotationTime:=Now;
  FCurrentFileSize:=0;
  if AClose then CloseFile(FFile);
  newName:= Format(FFileNameFormat, [FormatDateTime('YYYY-MM-DD--hh-nn-ss-zzz', Now)]) + '.log';
  AssignFile(FFile, FBase + '/' + newName);
  Rewrite(FFile);
end;

procedure TRotatingFilePool.TestForRotation(const increment: Integer);
var
  test0, test1: boolean;
begin
  FCurrentFileSize:=FCurrentFileSize + increment;
  test0:=FCurrentFileSize >= FRotationMaxSize;
  test1:=(FLastRotationTime + FRotationMaxTime) <= Now;
  if test0 or test1 then Rotate(true);
end;

constructor TRotatingFilePool.Create(const directoryName: string;
  const fileNameFormat: string; const rotationMaxSize: Integer;
  const rotationMaxTime: Integer);
begin                                                                 
  FFilter:=LogFilterDiagnose;
  if not DirectoryExists(directoryName) then CreateDir(directoryName);
  FBase:=directoryName;
  FFileNameFormat:=fileNameFormat;
  FRotationMaxSize:=rotationMaxSize;
  FRotationMaxTime:=rotationMaxTime;
  Rotate(false);
end;

destructor TRotatingFilePool.Destroy;
begin            
  CloseFile(FFile);
  inherited Destroy;
end;

procedure TRotatingFilePool.Write(const s: string);
begin             
  System.Write(FFile, s);
  TestForRotation(length(s));
end;

procedure TRotatingFilePool.WriteLn(const s: string);
begin       
  System.WriteLn(FFile, s);  
  TestForRotation(length(s) + 1);
end;

procedure TRotatingFilePool.Flush;
begin
  System.Flush(FFile);
end;

{ TSimpleFilePool }

function TSimpleFilePool.GetFilter: TLogFilter;
begin
  result:=FFilter;
end;

procedure TSimpleFilePool.SetFilter(AValue: TLogFilter);
begin         
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

constructor TSimpleFilePool.Create(fileName: string);
begin
  FFilter:=LogFilterDiagnose;
  AssignFile(FFile, fileName);
  Rewrite(FFile);
end;

destructor TSimpleFilePool.Destroy;
begin
  CloseFile(FFile);
  inherited Destroy;
end;

procedure TSimpleFilePool.Write(const s: string);
begin
  System.Write(FFile, s);
end;

procedure TSimpleFilePool.WriteLn(const s: string);
begin
  System.WriteLn(FFile, s);
end;

procedure TSimpleFilePool.Flush;
begin
  System.Flush(FFile);
end;

{ TConsolePool }

function TConsolePool.GetFilter: TLogFilter;
begin
  result:=FFilter;
end;

procedure TConsolePool.SetFilter(AValue: TLogFilter);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

constructor TConsolePool.Create;
begin
  FFilter:=LogFilterDevelop;
end;

procedure TConsolePool.Write(const s: string);
begin
  System.Write(s);
end;

procedure TConsolePool.WriteLn(const s: string);
begin
  System.WriteLn(s);
end;

procedure TConsolePool.Flush;
begin
end;


end.

