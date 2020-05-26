unit Projects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Module, Dialogs;

type
  TProjectAsyncCallback = procedure(sender: TObject);

  { IProject }

  IProject = interface(IInterface)
    function GetName: UTF8String;
    function GetUri: UTF8String;
    function GetState: boolean;
    function GetModule(Index: Integer): TModule;
    procedure SetUri(const AValue: UTF8String);
    property Name: UTF8String read GetName;
    property Uri: UTF8String read GetUri write SetUri;
    property Good: boolean read GetState;
    property Modules[Index: Integer]: TModule read GetModule;
    procedure UpdateAsync(callback: TProjectAsyncCallback);
    procedure ReloadAsync(callback: TProjectAsyncCallback);
  end;

  IProjcets = interface(IInterface)
  end;

  { TProject }

  TProject = class(TObject)
  public
    Name: UTF8String;
    Uri: UTF8String;
    Good: boolean;
    LastUpdate: TDateTime;
    Modules: array of TModule;
    procedure UpdateAsync();
    procedure ReloadJson();
  private
    procedure AsyncUpdateCompletion(success: boolean; log: string);
    procedure PushModule(module: TModule);
  end;

implementation

uses
  Utils, Git, fpjson, jsonparser, Forms;

var
  HomeDirectory: string;

{ TProject }

procedure TProject.UpdateAsync();
var
  dir: string;
begin
  HomeDirectory:=Application.Location; //TODO: Remove
  dir:= HomeDirectory + '/' + Name;
  if not DirectoryExists(dir) then begin
    GitCloneAsync(
      TGitCloneArgs.Create(HomeDirectory, Uri, Name),
      @AsyncUpdateCompletion
    );
  end
  else begin
    GitPullAsync(TGitPullArgs.Create(dir), @AsyncUpdateCompletion);
  end;
end;

procedure TProject.ReloadJson();
var
  jData : TJSONData;
  jContent : TJSONArray;
  jObject: TJSONObject;
  fName: string;
  field: TJSONEnum;
  module: TModule;
begin
  fName:=HomeDirectory + '/' + Name + '/modules.json';
  if not FileExists(fName) then begin
    Good:=false;
    //AppendLog('Project ' + Name + ' does not have modules.json list file');
  end;
  jData:=GetJSON(ReadAllText(fName));
  jContent:=TJSONArray(jData);
  for field in jContent do begin
    module:=TModule.Create;
    jObject:=TJSONObject(field.Value);
    module.Id:=jObject.Get('Id');
    module.Uri:=jObject.Get('Uri');
    module.FancyName:=jObject.Get('Fancy Name');
    PushModule(module);
  end;
  jData.Free;
end;

procedure TProject.AsyncUpdateCompletion(success: boolean; log: string);
begin
  Good:=success;
  if Good then begin
    ReloadJson;
  end;
end;

procedure TProject.PushModule(module: TModule);
begin
  setLength(Modules, length(Modules) + 1);
  Modules[length(Modules)-1]:=module;
end;

end.

