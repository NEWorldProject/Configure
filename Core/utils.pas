unit Utils;

{$mode objfpc}{$H+}

interface

function Where(prog: string): string;
function ReadAllText(name: string):string;

implementation

uses
  Classes, SysUtils, Forms;

function Where(prog: string): string;
var
  path: TStringArray;
  temp: string;
  i, len: integer;
begin
  path:=Application.EnvironmentVariable['PATH'].Split([';', ':']);
{$ifdef Darwin}
  // for some reason, if you use app bundle on macOS, the launchd will not
  // include /usr/local/bin in the path, while it is included from the shell
  // we will manually add it here. duplication will not matter as it will only
  // return the first match
  setLength(path, length(path) + 1);
  len:=length(path)-1;
  for i:=len downto 1 do path[i] := path[i-1];
  path[0]:='/usr/local/bin';
{$endif}
  len:=length(path)-1;
  for i:=0 to len do begin
    temp := path[i] + '/' + prog;
    if FileExists(temp) then exit(temp);
  end;
  result:='';
end;


function ReadAllText(name: string):string;
var
  list: TStringList;
begin
  list:=nil;
  result:='';
  try   
    list:= TStringList.Create;
    list.LoadFromFile(name);
    result:=list.Text;
  finally 
    list.Free;
  end;
end;


end.

