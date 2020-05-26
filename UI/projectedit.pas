unit ProjectEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Projects;

type

  { TProjectEditor }

  TProjectEditDone = procedure (edited: boolean; proj: TProject) of object;

  TProjectEditor = class(TForm)
    BCancel: TButton;
    BConfirm: TButton;
    EName: TEdit;
    EUri: TEdit;
    LName: TLabel;
    LUri: TLabel;
    Progression: TProgressBar;
    procedure BCancelClick(Sender: TObject);
    procedure BConfirmClick(Sender: TObject);
  private
    Project: TProject;
    Completion: TProjectEditDone;
    function Verify: boolean; 
    procedure EnableEditing;
    procedure DisableEditing;
    procedure UpdateAttemptUISetup;
  public
    procedure Setup(proj: TProject; completes: TProjectEditDone);
  end;

  procedure StartProjectEditAsync(proj: TProject; completes: TProjectEditDone);

implementation

procedure StartProjectEditAsync(proj: TProject; completes: TProjectEditDone);
var
  dialog: TProjectEditor;
begin
  Application.CreateForm(TProjectEditor, dialog);
  dialog.Setup(proj, complete);
  dialog.Show;
end;

{$R *.lfm}

{ TProjectEditor }

procedure TProjectEditor.BCancelClick(Sender: TObject);
begin
  // Canceled immediately
  if Completion <> nil then Completion(false, Project);
  Close;
end;

procedure TProjectEditor.BConfirmClick(Sender: TObject);
begin
  // Execute Verification
  if not Verify then exit;
  // Launches Async Profile Update
end;

function TProjectEditor.Verify: boolean;
begin
      
  Progression.Style:=pbstMarquee;
end;

procedure TProjectEditor.EnableEditing;
begin
  // Enable all interactive components
  EName.Enabled:=true;
  EName.ReadOnly:=false
  EUri.Enabled:=true;
  EUri.ReadOnly:=false;
  BCancel.Enabled:=true;
  BConfirm.Enabled:=true;
end;

procedure TProjectEditor.DisableEditing;
begin
  // Disable all interactive components
  EName.Enabled:=false;
  EName.ReadOnly:=true
  EUri.Enabled:=false;
  EUri.ReadOnly:=true;
  BCancel.Enabled:=false;
  BConfirm.Enabled:=false;
end;
                         
procedure TProjectEditor.UpdateAttemptUISetup;
begin
  DisableEditing;
  Progression.Style:=pbstMarquee;
  BConfirm.Caption:='Confirming';
end;

procedure TProjectEditor.Setup(proj: TProject; completes: TProjectEditDone);
begin
  Project:=proj;
  Completion:=completes;
end;

end.

