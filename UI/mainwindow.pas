unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Grids, ComCtrls, Module;

type

  { TMainForm }

  TMainForm = class(TForm)
    EGit: TFileNameEdit;
    ECMake: TFileNameEdit;
    LGit: TLabel;
    LCMake: TLabel;
    TProjects: TDrawGrid;
    EHomeDir: TDirectoryEdit;
    LHomeDir: TLabel;
    Tables: TPageControl;
    PProjects: TTabSheet;
    PRepo: TTabSheet;
    TRepos: TDrawGrid;
    procedure ECMakeEditingDone(Sender: TObject);
    procedure EGitEditingDone(Sender: TObject);
    procedure EHomeDirEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TProjectsClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;
  HomeDirectory, CMake: string;

implementation

uses
  Process, Utils, Git, Projects;

{$R *.lfm}

procedure CreateDirectoryIfDoesNotExist(dir: string);
begin
  if not DirectoryExists(dir) then
    CreateDir(dir);
end;

procedure AppendLog(s: string);
begin

end;

procedure Reload();
begin

end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  default: string;
  proj: TProject;
begin
  // Set all fields to default
  default:=Application.Location;
  HomeDirectory:=default;
  EHomeDir.Directory:=default;
  EHomeDir.RootDir:=default;
  SetGitPath(Where('git'));
  CMake:=Where('cmake');
  EGit.FileName:=GetGitPath();
  ECMake.FileName:=CMake;
  Reload();
end;

procedure TMainForm.TProjectsClick(Sender: TObject);
begin

end;

procedure TMainForm.EHomeDirEditingDone(Sender: TObject);
begin
  HomeDirectory:=EHomeDir.Directory;
  EHomeDir.RootDir:=HomeDirectory;
  Reload();
end;

procedure TMainForm.EGitEditingDone(Sender: TObject);
begin
  SetGitPath(EGit.FileName);
end;

procedure TMainForm.ECMakeEditingDone(Sender: TObject);
begin
  CMake:=ECMake.FileName;
end;



end.

