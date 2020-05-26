unit Git;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGitInvokeCallback = procedure (success: boolean; log: string) of object;

  { TGitCloneArgs }

  TGitCloneArgs = class
  public
    WorkingDir: string;
    Uri: string;
    Path: string;
    Branch: string;
    Depth: integer;
    constructor Create(work: string; iUri: string; iPath: string);
  end;

  { TGitPullArgs }

  TGitPullArgs = class
  public
    WorkingDir: string;  
    constructor Create(work: string);
  end;

  procedure GitCloneAsync(args: TGitCloneArgs; completes: TGitInvokeCallback);

  procedure GitPullAsync(args: TGitPullArgs; completes: TGitInvokeCallback); 

  procedure SetGitPath(path: string);

  function GetGitPath(): string;

implementation

uses
  Process;

type

  { TGitCaller }

  TGitCaller = class(TThread)
    VWork: string;
    VArgs: TStringList;
    VCompletes: TGitInvokeCallback;
    Success: boolean;
    Output: string;
    procedure RunContinuation();
  protected
    procedure Execute; override;
  public
    constructor Create(work: string; args: TStringList; completes: TGitInvokeCallback);
  end;


var
  GitPath: string;

{ TGitPullArgs }

constructor TGitPullArgs.Create(work: string);
begin
  WorkingDir:=work;
end;

{ TGitCloneArgs }

constructor TGitCloneArgs.Create(work: string; iUri: string; iPath: string);
begin
  WorkingDir:=work;
  Uri:=iUri;
  Path:=iPath;
  Branch:='';
  Depth:=0;
end;

{ TGitCaller }

procedure TGitCaller.RunContinuation();
begin
  VCompletes(Success, Output);
end;

procedure TGitCaller.Execute;
var
  arr: array of string;
  i: integer;
begin
  setLength(arr, VArgs.Count);
  for i:=0 to VArgs.Count - 1 do arr[i]:=VArgs.Strings[i];
  VArgs.Free;
  Success:=RunCommandInDir(VWork, GitPath, arr, Output);
  Synchronize(@RunContinuation);
end;

constructor TGitCaller.Create(work:string; args: TStringList;
  completes: TGitInvokeCallback);
begin           
  inherited Create(true);
  self.VWork:=work;
  self.VArgs:=args;
  self.VCompletes:=completes;
  self.FreeOnTerminate:=true;
  self.Start;
end;

procedure GitCloneAsync(args: TGitCloneArgs; completes: TGitInvokeCallback);
var
  list: TStringList;
begin
  list:=TStringList.Create;
  list.Add('clone');
  list.Add(args.Uri);
  if args.Path <> '' then list.Add(args.Path);
  if args.Branch <> '' then begin
    list.Add('-b');
    list.Add(args.Branch);
  end;
  if args.Depth <> 0 then begin
    list.Add('--depth');
    list.Add(args.Depth.ToString);
  end;
  TGitCaller.Create(args.WorkingDir, list, completes);
  args.Free;
end;

procedure GitPullAsync(args: TGitPullArgs; completes: TGitInvokeCallback);
var
  list: TStringList;
begin
  list:=TStringList.Create;
  list.Add('pull');
  TGitCaller.Create(args.WorkingDir, list, completes);
  args.Free;
end;

procedure SetGitPath(path: string);
begin
  GitPath:=path;
end;

function GetGitPath(): string;
begin
  result:=GitPath;
end;

end.

